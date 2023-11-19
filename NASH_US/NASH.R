# Import required libraries ---------

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(zoo)
library(spatstat)
library(ggridges)
options(scipen = 999)


# ------
# How many of pats within each NASH groups also have Obesity ? ---------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")

sum(NASH_diagnosis$weight)
NASH_diagnosis %>% group_by(NASH_diganosis) %>% summarise(n=sum(weight)) # 25653131

# NASH_diganosis         n
# <chr>              <dbl>
# 1 NAFLD          14081980.
# 2 NASH-Cirrohsis   303005.
# 3 NASH-Fibrosis     92489.
# 4 NASH-Only        944489.
# 5 Other Liver    10231168.

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(grepl("Obesity", diagnosis))
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis)

NASH_diagnosis <- NASH_diagnosis %>% left_join(DANU_Demographics, by=c("patient"="patid", "weight"="weight"))

NASH_diagnosis <- NASH_diagnosis %>% 
  mutate(Obesity_status = ifelse(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity", "Obesity +/- Diabetes", "no OBE"))

NASH_diagnosis %>% group_by(NASH_diganosis, Obesity_status) %>% summarise(n=sum(weight))

NASH_diganosis Obesity_status               n
<chr>          <chr>                    <dbl>
1 NAFLD          Obesity +/- Diabetes 12312661.
2 NAFLD          NA                    1769319.
3 NASH-Cirrohsis Obesity +/- Diabetes   283257.
4 NASH-Cirrohsis NA                      19749.
5 NASH-Fibrosis  Obesity +/- Diabetes    85541.
6 NASH-Fibrosis  NA                       6947.
7 NASH-Only      Obesity +/- Diabetes   856868.
8 NASH-Only      NA                      87621.
9 Other Liver    Obesity +/- Diabetes  8683811.
10 Other Liver    NA                    1547357.



NASH_diagnosis <- NASH_diagnosis %>% 
  mutate(Obesity_only = ifelse(diagnosis=="Obesity", "Obesity only", 
                               ifelse(diagnosis=="Diabetes + Obesity", "Obesity + Diabetes", "other")))

NASH_diagnosis %>% group_by(NASH_diganosis, Obesity_only) %>% summarise(n=sum(weight))

NASH_diganosis Obesity_only              n
<chr>          <chr>                 <dbl>
1 NAFLD          Obesity + Diabetes 6001293.
2 NAFLD          Obesity only       6311368.
3 NAFLD          NA                 1769319.
4 NASH-Cirrohsis Obesity + Diabetes  216984.
5 NASH-Cirrohsis Obesity only         66272.
6 NASH-Cirrohsis NA                   19749.
7 NASH-Fibrosis  Obesity + Diabetes   52098.
8 NASH-Fibrosis  Obesity only         33443.
9 NASH-Fibrosis  NA                    6947.
10 NASH-Only      Obesity + Diabetes  477819.
11 NASH-Only      Obesity only        379049.
12 NASH-Only      NA                   87621.
13 Other Liver    Obesity + Diabetes 3489438.
14 Other Liver    Obesity only       5194373.
15 Other Liver    NA                 1547357.
# ------
# How many pats within each group also have cirrhosis ? ------------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% select(patid, weight, code) %>% distinct()

LiverFailurePats <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  select(patid, condition) %>% distinct() %>% mutate(condition="Any Liver Failure") %>% distinct()

NASH_diagnosis <- NASH_diagnosis %>% left_join(LiverFailurePats, by=c("patient"="patid"))

NASH_diagnosis <- NASH_diagnosis %>% mutate(New_Diagnosis = ifelse(is.na(condition), NASH_diganosis, "Cirrhosis"))

NASH_diagnosis %>% group_by(NASH_diganosis, condition) %>% summarise(n=sum(weight))

NASH_diganosis condition                 n
<chr>          <chr>                 <dbl>
1 NAFLD          Any Liver Failure   608342.
2 NAFLD          NA                13473638.
3 NASH-Cirrohsis Any Liver Failure   303005.
4 NASH-Fibrosis  Any Liver Failure     1626.
5 NASH-Fibrosis  NA                   90863.
6 NASH-Only      Any Liver Failure     7388.
7 NASH-Only      NA                  937101.
8 Other Liver    Any Liver Failure  1174860.
9 Other Liver    NA                 9056308.

# -----
# Venn diagram NASH vs T2 DM vs Obesity ---------------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")

NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

DANU_Demographics <- fread("DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"

NASH_diagnosis <- NASH_diagnosis %>% left_join(DANU_Demographics %>% select(patient, weight, diagnosis))

NASH_diagnosis %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# diagnosis                n
# <chr>                <dbl>
# 1 -                   68944.
# 2 Diabetes            45373.
# 3 Diabetes + Obesity 746901.
# 4 Obesity            478765.

DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# diagnosis                   n
# <chr>                   <dbl>
# 1 -                    9159648.
# 2 Diabetes             7949715.
# 3 Diabetes + Obesity  40282960.
# 4 Obesity            106469049.
# --------
# How long has been since first NASH Dx ? --------------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))

NASH_diagnosis <- NASH_diagnosis %>% select(patient) %>% left_join(NASH_Events, by=c("patient"="patid"))
NASH_diagnosis <- NASH_diagnosis %>% filter(condition=="NASH")

NASH_diagnosis <- NASH_diagnosis %>% group_by(patient) %>% slice(1)
NASH_diagnosis <- NASH_diagnosis %>% mutate(claimed = as.Date(claimed))
NASH_diagnosis <- NASH_diagnosis %>% mutate(dayslapsed= as.Date("2021-04-30")-claimed) %>%
  mutate(monthslapsed=dayslapsed/30.5) 

NASH_diagnosis$monthslapsed <- as.numeric(NASH_diagnosis$monthslapsed)

NASH_diagnosis <- NASH_diagnosis %>% mutate(monthslapsedbucket = ifelse(monthslapsed<=12,1,
                                                      ifelse(monthslapsed<=24,2,
                                                             ifelse(monthslapsed<=36,3,
                                                                    ifelse(monthslapsed<=48,4,
                                                                           ifelse(monthslapsed<=60,5,
                                                                                  ifelse(monthslapsed<=72,6,NA)))))))

NASH_diagnosis %>% group_by(monthslapsedbucket) %>% summarise(n=sum(weight))


NASH_diagnosis %>% 
  ggplot(aes(monthslapsed))+
  geom_histogram(bins=1677)

# monthslapsedbucket       n
# <dbl>   <dbl>
# 1                  1 240482.
# 2                  2 232543.
# 3                  3 214718.
# 4                  4 214885.
# 5                  5 241779.
# 6                  6 195575.

# 
# ------
# Drug penetrance across different types of liver disease ----------
NASH_diagnosis         <- fread("NASH_diagnosis.txt")


DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_class)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)


OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_diagnosis <- NASH_diagnosis %>% left_join(OBE_Drug_Histories %>% select(-c(disease, weight)))

NASH_diagnosis <- gather(NASH_diagnosis, Month, Treat, month1:month60, factor_key=TRUE)

NASH_diagnosis <- NASH_diagnosis %>% filter(Treat!="-") %>% filter(!is.na(Treat))

NASH_diagnosis <- separate_rows(NASH_diagnosis, Treat, sep = ",", convert=T )

NASH_diagnosis <- NASH_diagnosis %>% select(-c(Month)) %>% group_by(patient, weight, NASH_diganosis) %>% distinct()

names(NASH_diagnosis)[4] <- "molecule"

NASH_diagnosis <- NASH_diagnosis %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_class))
NASH_diagnosis <- NASH_diagnosis %>% select(patient, weight, NASH_diganosis, drug_class)
NASH_diagnosis <- NASH_diagnosis %>% distinct()

data.frame(NASH_diagnosis %>% group_by(NASH_diganosis) %>% summarise(n=sum(weight)))

NASH_diganosis         n
1          NAFLD 720439.84
2 NASH-Cirrohsis   6258.51
3  NASH-Fibrosis   5171.14
4      NASH-Only  58579.82
5    Other Liver 439573.78

data.frame(NASH_diagnosis %>% group_by(NASH_diganosis, drug_class) %>% summarise(n=sum(weight)))

# NASH_diganosis      drug_class         n
# 1           NAFLD       Anorectic 470065.57
# 2           NAFLD     Antiobesity  49514.83
# 3           NAFLD GLP1 Injectable  35061.84
# 4           NAFLD       GLP1 Oral   4246.90
# 5           NAFLD         Surgery 158698.42
# 6           NAFLD     Weight Loss   2852.28
# 7  NASH-Cirrohsis       Anorectic   3671.52
# 8  NASH-Cirrohsis     Antiobesity    488.82
# 9  NASH-Cirrohsis GLP1 Injectable    289.04
# 10 NASH-Cirrohsis       GLP1 Oral    392.82
# 11 NASH-Cirrohsis         Surgery   1416.31
# 12  NASH-Fibrosis       Anorectic   2089.08
# 13  NASH-Fibrosis     Antiobesity    236.90
# 14  NASH-Fibrosis GLP1 Injectable    882.60
# 15  NASH-Fibrosis         Surgery   1796.65
# 16  NASH-Fibrosis     Weight Loss    165.91
# 17      NASH-Only       Anorectic  34350.84
# 18      NASH-Only     Antiobesity   3927.09
# 19      NASH-Only GLP1 Injectable   2938.57
# 20      NASH-Only       GLP1 Oral    175.23
# 21      NASH-Only         Surgery  17084.09
# 22      NASH-Only     Weight Loss    104.00
# 23    Other Liver       Anorectic 339874.70
# 24    Other Liver     Antiobesity  24562.54
# 25    Other Liver GLP1 Injectable  14497.32
# 26    Other Liver       GLP1 Oral   1032.32
# 27    Other Liver         Surgery  57468.24
# 28    Other Liver     Weight Loss   2138.66

# -----
# Anticholesterol Durgs NAFLD pats ------------------
# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NAFLD_Drug_Histories <- fread("NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient, weight, month1:month60)

NAFLD_Drug_Histories <- gather(NAFLD_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NAFLD_Drug_Histories <- separate_rows(NAFLD_Drug_Histories, Treat, sep = ",", convert=T )
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(Treat != "-")

names(NAFLD_Drug_Histories)[4] <- "molecule"
NAFLD_Drug_Histories$molecule <- as.numeric(NAFLD_Drug_Histories$molecule)

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                             select(molecule, drug_group, drug_class))

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(-c(Month))
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient, weight, drug_group, drug_class)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% distinct()

NAFLD_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 8841776 (65116 ever treated)

NAFLD_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 

# drug_group              n
# <chr>               <dbl>
#   1 Anticholesterol  6879160.
# 2 Antidiabetic     4587270.
# 3 Antiobesity      1070942.
# 4 GLP1 Injectable  1119155.
# 5 GLP1 Oral          59208.
# 6 Hepatoprotective  192428.
# 7 Hospitalization   539943

sum(NAFLD_Drug_Histories$weight) # 20938565


data.frame(NAFLD_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group, drug_class) %>% distinct() %>% 
             group_by(drug_group, drug_class) %>% summarise(n=sum(weight)))


# drug_group         drug_class          n
# 1   Anticholesterol    Anticholesterol  704407.63
# 2   Anticholesterol                BAS  466113.71
# 3   Anticholesterol           Biologic   94084.83
# 4   Anticholesterol            Fibrate  976000.88
# 5   Anticholesterol             Statin 6308226.05
# 6      Antidiabetic                AGI   34472.29
# 7      Antidiabetic       Antidiabetic    6115.19
# 8      Antidiabetic          Biguanide 3766490.04
# 9      Antidiabetic               DPP4  969264.01
# 10     Antidiabetic            Glinide   71045.14
# 11     Antidiabetic          Glitazone  306189.03
# 12     Antidiabetic            Insulin 1880758.74
# 13     Antidiabetic              SGLT2  938509.54
# 14     Antidiabetic       Sulfonylurea 1373500.43
# 15      Antiobesity          Anorectic  987483.83
# 16      Antiobesity        Antiobesity  123721.35
# 17      Antiobesity        Weight Loss    8731.24
# 18  GLP1 Injectable    GLP1 Injectable 1119154.51
# 19        GLP1 Oral          GLP1 Oral   59208.16
# 20 Hepatoprotective   Hepatoprotective  192427.70
# 21  Hospitalization  Bariatric Surgery  399687.02
# 22  Hospitalization Hospital Inpatient   96325.50
# 23  Hospitalization   Liver Transplant   11080.29
# 24  Hospitalization  Surgery Inpatient   45568.20

# --------


# NASH Drug Usage month60 ------------------
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))


NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month60)


NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, month60, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(month60 != "-")

names(NASH_Drug_Histories)[3] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 8841776 (65116 ever treated)

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 

drug_group             n
<chr>              <dbl>
  1 Anticholesterol  438784.
2 Antidiabetic     364638.
3 Antiobesity       30056.
4 GLP1 Injectable   79904.
5 GLP1 Oral          5480.
6 Hepatoprotective  16771.
7 Hospitalization    4934.

# ----------
# Lines of therapy NASH ----------
drgNASH <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

drgNASH <- data.frame(drgNASH, stringsAsFactors = F)

nrLines <- drgNASH[,c(1:3)] 
nrLines$month1 <- (drgNASH$month1 != "-")*1

for(i in 2:60){
  cat(i)
  nrLines[,i+3] <- apply(drgNASH[,(4:(i+3))], 1, function(x) length(unique(x[x!="-"])))
  names(nrLines)[i+3] <- paste0("month",i)
}

fwrite(nrLines,"NASH_nrLines_Histories.txt")

nrLines <- fread("NASH_nrLines_Histories.txt")

nrLines <- nrLines %>% select(patient, weight, month60)
nrLines <- nrLines %>% mutate(month60 = ifelse(month60>=8,8,month60))

BoxHistories <- fread("NASH Box Histories.txt", integer64 = "character", stringsAsFactors = F)
BoxHistories <- BoxHistories %>% select(patient, weight, month60)
BoxHistories <- BoxHistories %>% mutate(Stockm60 = str_sub(month60, 2L, 2L)) %>% select(patient, Stockm60)

data.frame(nrLines %>% left_join(BoxHistories) %>% group_by(Stockm60, month60) %>% summarise(n=sum(weight)))


nrLines <- fread("NASH_nrLines_Histories.txt")

nrLines <- nrLines %>% select(patient, weight, month60)

BoxHistories <- fread("NASH Box Histories.txt", integer64 = "character", stringsAsFactors = F)
BoxHistories <- BoxHistories %>% select(patient, weight, month60)
BoxHistories <- BoxHistories %>% mutate(Stockm60 = str_sub(month60, 2L, 2L)) %>% select(patient, Stockm60)

data.frame(nrLines %>% left_join(BoxHistories) %>% group_by(Stockm60) %>% summarise(n=weighted.mean(month60, weight)))

# -------
# Number of molecules on m60 NASH ------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

pats_to_keep <- NASH_Drug_Histories %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month60)
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, month60, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(month60 != "-")

N_drugs_m60 <- NASH_Drug_Histories %>% group_by(patient) %>% count()

N_drugs_m60 <- pats_to_keep %>% left_join(N_drugs_m60) %>% mutate(n=ifelse(is.na(n),0,n))

BoxHistories <- fread("NASH Box Histories.txt", integer64 = "character", stringsAsFactors = F)
BoxHistories <- BoxHistories %>% select(patient, weight, month60)
BoxHistories <- BoxHistories %>% mutate(Stockm60 = str_sub(month60, 2L, 2L)) %>% select(patient, Stockm60)

N_drugs_m60 <- N_drugs_m60 %>% left_join(BoxHistories)

N_drugs_m60 %>% group_by(Stockm60) %>% summarise(n=weighted.mean(n,weight))


N_drugs_m60 <- N_drugs_m60 %>% mutate(n = ifelse(n>=8,8,n))

data.frame(N_drugs_m60 %>% group_by(Stockm60, n) %>% summarise(pats=sum(weight)))

# ------
# Pick GLP1 patients to image NASH ------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

SemaglutideOralPats <- NASH_Drug_Histories %>% filter(grepl("69",month60)) %>% select(patient)
SemaglutideOralPats <- SemaglutideOralPats %>% mutate(patient = paste(patient, collapse= ","))
SemaglutideOralPats <- SemaglutideOralPats %>% distinct()
fwrite(SemaglutideOralPats, "SemaglutideOralPats.txt")

SemaglutideInjPats <- NASH_Drug_Histories %>% filter(grepl("75",month60)) %>% select(patient)
SemaglutideInjPats <- SemaglutideInjPats %>% mutate(patient = paste(patient, collapse= ","))
SemaglutideInjPats <- SemaglutideInjPats %>% distinct()
fwrite(SemaglutideInjPats, "SemaglutideInjPats.txt")


# ---------
# Lines of therapy NAFLD ----------
drgNAFLD <- fread("NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

drgNAFLD <- data.frame(drgNAFLD, stringsAsFactors = F)

nrLines <- drgNAFLD[,c(1:3)] 
nrLines$month1 <- (drgNAFLD$month1 != "-")*1

for(i in 2:60){
  cat(i)
  nrLines[,i+3] <- apply(drgNAFLD[,(4:(i+3))], 1, function(x) length(unique(x[x!="-"])))
  names(nrLines)[i+3] <- paste0("month",i)
}

fwrite(nrLines,"NAFLD_nrLines_Histories.txt")

nrLines <- fread("NAFLD_nrLines_Histories.txt")

nrLines <- nrLines %>% select(patient, weight, month60)
nrLines <- nrLines %>% mutate(month60 = ifelse(month60>=8,8,month60))

BoxHistories <- fread("NAFLD Box Histories.txt", integer64 = "character", stringsAsFactors = F)
BoxHistories <- BoxHistories %>% select(patient, weight, month60)
BoxHistories <- BoxHistories %>% mutate(Stockm60 = str_sub(month60, 2L, 2L)) %>% select(patient, Stockm60)

data.frame(nrLines %>% left_join(BoxHistories) %>% group_by(Stockm60, month60) %>% summarise(n=sum(weight)))


nrLines <- fread("NASH_nrLines_Histories.txt")

nrLines <- nrLines %>% select(patient, weight, month60)

BoxHistories <- fread("NAFLD Box Histories.txt", integer64 = "character", stringsAsFactors = F)
BoxHistories <- BoxHistories %>% select(patient, weight, month60)
BoxHistories <- BoxHistories %>% mutate(Stockm60 = str_sub(month60, 2L, 2L)) %>% select(patient, Stockm60)

data.frame(nrLines %>% left_join(BoxHistories) %>% group_by(Stockm60) %>% summarise(n=weighted.mean(month60, weight)))

# -------
# Number of molecules on m60 NAFLD ------
NAFLD_Drug_Histories <- fread("NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

pats_to_keep <- NAFLD_Drug_Histories %>% select(patient, weight)

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient, weight, month60)
NAFLD_Drug_Histories <- separate_rows(NAFLD_Drug_Histories, month60, sep = ",", convert=T )
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(month60 != "-")

N_drugs_m60 <- NAFLD_Drug_Histories %>% group_by(patient) %>% count()

N_drugs_m60 <- pats_to_keep %>% left_join(N_drugs_m60) %>% mutate(n=ifelse(is.na(n),0,n))

BoxHistories <- fread("NAFLD Box Histories.txt", integer64 = "character", stringsAsFactors = F)
BoxHistories <- BoxHistories %>% select(patient, weight, month60)
BoxHistories <- BoxHistories %>% mutate(Stockm60 = str_sub(month60, 2L, 2L)) %>% select(patient, Stockm60)

N_drugs_m60 <- N_drugs_m60 %>% left_join(BoxHistories)

N_drugs_m60 %>% group_by(Stockm60) %>% summarise(n=weighted.mean(n,weight))


N_drugs_m60 <- N_drugs_m60 %>% mutate(n = ifelse(n>=8,8,n))

data.frame(N_drugs_m60 %>% group_by(Stockm60, n) %>% summarise(pats=sum(weight)))

# ------
# Pick GLP1 patients to image NAFLD ------------
NAFLD_Drug_Histories <- fread("NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

SemaglutideOralPats <- NAFLD_Drug_Histories %>% filter(grepl("69",month60)) %>% select(patient)
SemaglutideOralPats <- SemaglutideOralPats %>% mutate(patient = paste(patient, collapse= ","))
SemaglutideOralPats <- SemaglutideOralPats %>% distinct()
fwrite(SemaglutideOralPats, "SemaglutideOralPats_NAFLD.txt")

SemaglutideInjPats <- NAFLD_Drug_Histories %>% filter(grepl("75",month60)) %>% select(patient)
SemaglutideInjPats <- SemaglutideInjPats %>% mutate(patient = paste(patient, collapse= ","))
SemaglutideInjPats <- SemaglutideInjPats %>% distinct()
fwrite(SemaglutideInjPats, "SemaglutideInjPats_NAFLD.txt")


# ------
# Penetrance of each Dx ----------
NASH_Events <- fread("NASH Events.txt")
Dx_code <- fread("NASH Diagnosis Codes.txt")
Dx_code <- Dx_code %>% select(code, condition, source, type, description)

NASH_Events %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #27167873
 
length(unique(NASH_Events$patid)) #194418

NASH_Events <- NASH_Events %>% left_join(Dx_code)

NASH_Pats <- NASH_Events %>% filter(condition=="NASH") %>% select(patid) %>% distinct()
NAFLD_Pats <-  NASH_Events %>% filter(condition=="NAFLD") %>% select(patid) %>% distinct()

NAFLD_Pats <- NAFLD_Pats %>% anti_join(NASH_Pats)

NASH_Pats <- NASH_Pats %>% left_join(NASH_Events)
NAFLD_Pats <- NAFLD_Pats %>% left_join(NASH_Events)

NASH_Pats %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #1339983
NASH_Pats %>% select(patid, weight, condition) %>% distinct() %>% group_by(condition) %>% summarise(n=sum(weight)) %>% arrange(desc(n))

# condition                    n
# <chr>                    <dbl>
# 1 NASH                  1339983.
# 2 NAFLD                  917865.
# 3 Chronic Liver Disease  512623.
# 4 Cirrhosis              303005.
# 5 Liver Biopsy           209823.
# 6 Hepatitis              201426.
# 7 Liver Ultrasound       181917.
# 8 Fibrosis               162328.
# 9 Liver Failure           89904.
# 10 Liver Imaging           83065.
# 11 Liver Cancer            40010.
# 12 Liver Transplant        28935.


NAFLD_Pats %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #14081980
NAFLD_Pats %>% select(patid, weight, condition) %>% distinct() %>% group_by(condition) %>% summarise(n=sum(weight)) %>% arrange(desc(n))

# condition                     n
# <chr>                     <dbl>
#   1 NAFLD                 14081980.
# 2 Chronic Liver Disease  2900130.
# 3 Hepatitis               954423.
# 4 Liver Imaging           842435.
# 5 Cirrhosis               525907.
# 6 Liver Ultrasound        494393.
# 7 Liver Biopsy            263488.
# 8 Fibrosis                201331.
# 9 Liver Failure           180709.
# 10 Liver Cancer            160443.
# 11 Liver Transplant         36711.

NASH_Pats <- NASH_Pats %>% mutate(claimed = as.Date(claimed))
NAFLD_Pats <- NAFLD_Pats %>% mutate(claimed = as.Date(claimed))

# First NASH Dx to First Liver Biopsy
data.frame(NASH_Pats %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct() %>%
  left_join(NASH_Pats)  %>% select(-c(description, code, source, type))  %>% group_by(patid, weight) %>% 
  slice(if(any(condition == "Liver Biopsy")) which.max(condition=="NASH"):which.max(condition == "Liver Biopsy") else row_number())   %>%
  filter(row_number()==1 | row_number()==n()) %>%
  mutate(NASH_To_Biopsy = claimed-lag(claimed)) %>% 
  mutate(NASH_To_Biopsy_months = parse_number(as.character((claimed-lag(claimed))/30.5))) %>%
  filter(!is.na(NASH_To_Biopsy_months)) %>% ungroup() %>%
  mutate(NASH_To_Biopsy_months = round(NASH_To_Biopsy_months)) %>%
  group_by(NASH_To_Biopsy_months) %>% summarise(n=sum(weight)))


# First Dx (Any other) to First Liver Biopsy
data.frame(NASH_Pats %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct() %>%
             left_join(NASH_Pats)  %>% select(-c(description, code, source, type))  %>% group_by(patid, weight) %>% 
             slice(if(any(condition == "Liver Biopsy")) which.max(condition != "Liver Biopsy"):which.max(condition == "Liver Biopsy") else row_number())   %>%
             filter(row_number()==1 | row_number()==n()) %>%
             mutate(AnyDx_To_Biopsy = claimed-lag(claimed)) %>% 
             mutate(AnyDx_To_Biopsy_months = parse_number(as.character((claimed-lag(claimed))/30.5))) %>%
             filter(!is.na(AnyDx_To_Biopsy_months)) %>% ungroup() %>%
             mutate(AnyDx_To_Biopsy_months = round(AnyDx_To_Biopsy_months)) %>%
             group_by(AnyDx_To_Biopsy_months) %>% summarise(n=sum(weight)))



# First NAFLD Dx to First Liver Biopsy
data.frame(NAFLD_Pats %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct() %>%
             left_join(NAFLD_Pats)  %>% select(-c(description, code, source, type))  %>% group_by(patid, weight) %>% 
             slice(if(any(condition == "Liver Biopsy")) which.max(condition=="NAFLD"):which.max(condition == "Liver Biopsy") else row_number())   %>%
             filter(row_number()==1 | row_number()==n()) %>%
             mutate(NAFLD_To_Biopsy = claimed-lag(claimed)) %>% 
             mutate(NAFLD_To_Biopsy_months = parse_number(as.character((claimed-lag(claimed))/30.5))) %>%
             filter(!is.na(NAFLD_To_Biopsy_months)) %>% ungroup() %>%
             mutate(NAFLD_To_Biopsy_months = round(NAFLD_To_Biopsy_months)) %>%
             group_by(NAFLD_To_Biopsy_months) %>% summarise(n=sum(weight)))


# First Dx (Any other) to First Liver Biopsy
data.frame(NAFLD_Pats %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct() %>%
             left_join(NAFLD_Pats)  %>% select(-c(description, code, source, type))  %>% group_by(patid, weight) %>% 
             slice(if(any(condition == "Liver Biopsy")) which.max(condition != "Liver Biopsy"):which.max(condition == "Liver Biopsy") else row_number())   %>%
             filter(row_number()==1 | row_number()==n()) %>%
             mutate(AnyDx_To_Biopsy = claimed-lag(claimed)) %>% 
             mutate(AnyDx_To_Biopsy_months = parse_number(as.character((claimed-lag(claimed))/30.5))) %>%
             filter(!is.na(AnyDx_To_Biopsy_months)) %>% ungroup() %>%
             mutate(AnyDx_To_Biopsy_months = round(AnyDx_To_Biopsy_months)) %>%
             group_by(AnyDx_To_Biopsy_months) %>% summarise(n=sum(weight)))

# ---------
# Penetrance of each Dx for cirrhosis/liver failure pats --------
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% select(patid, weight, code) %>% distinct()

LiverFailurePats <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  select(patid, condition) %>% distinct() %>% mutate(condition="Any Liver Failure") %>% distinct()

LiverFailurePats



NASH_Events <- fread("NASH Events.txt")
Dx_code <- fread("NASH Diagnosis Codes.txt")
Dx_code <- Dx_code %>% select(code, condition, source, type, description)

NASH_Events %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #27167873

length(unique(NASH_Events$patid)) #194418

NASH_Events <- NASH_Events %>% left_join(Dx_code)

NASH_Pats <- NASH_Events %>% filter(condition=="NASH") %>% select(patid) %>% distinct()

NASH_Pats <- NASH_Pats %>% left_join(NASH_Events)

NASH_Pats %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #1339983
NASH_Pats %>% select(patid, weight, condition) %>% distinct() %>% group_by(condition) %>% summarise(n=sum(weight)) %>% arrange(desc(n))


LiverFailurePats %>% select(patid) %>% distinct() %>% inner_join(NASH_Pats) %>% 
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 312019

LiverFailurePats %>% select(patid) %>% distinct() %>% inner_join(NASH_Pats) %>% 
  select(patid, weight, condition) %>% distinct() %>% group_by(condition) %>% summarise(n=sum(weight)) %>% arrange(desc(n))

condition                   n
<chr>                   <dbl>
  1 NASH                  312019.
2 Cirrhosis             303005.
3 Chronic Liver Disease 244895.
4 NAFLD                 234169.
5 Hepatitis              94933.
6 Liver Failure          89904.
7 Liver Biopsy           84045.
8 Fibrosis               71466.
9 Liver Ultrasound       61502.
10 Liver Cancer           28751.
11 Liver Imaging          28248.
12 Liver Transplant       26565.
# ----------
# Scripts per Specialty NASH -------------
NASH_US_Doses <- fread("NASH Doses.txt")
NASH_US_Doses <- NASH_US_Doses %>% filter(status != "G")
NASH_US_Doses <- NASH_US_Doses %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
NASH_US_Doses <- NASH_US_Doses %>% mutate(from_dt = as.Date(from_dt))
NASH_US_Doses <- NASH_US_Doses %>%filter(from_dt >= "2016-05-01" & from_dt <= "2021-04-30") 

Specialties_to_keep <- fread("Specialties_to_keep.txt")

temp1 <- data.frame(NASH_US_Doses %>% group_by(specialty) %>% summarise(n=n()) %>%
             left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
             ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))

temp2 <- data.frame(NASH_US_Doses %>% filter(drug_group == "Anticholesterol") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n3=sum(n)) %>% arrange(-n3) %>% filter(PHYSICIAN != "FACILITY"))


temp3 <- data.frame(NASH_US_Doses %>% filter(drug_group == "Antiobesity") %>% group_by(specialty) %>% summarise(n=n()) %>%
             left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
             ungroup() %>% group_by(PHYSICIAN) %>% summarise(n4=sum(n)) %>% arrange(-n4) %>% filter(PHYSICIAN != "FACILITY"))

temp4 <- data.frame(NASH_US_Doses %>% filter(drug_group == "Hepatoprotective") %>% group_by(specialty) %>% summarise(n=n()) %>%
             left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
             ungroup() %>% group_by(PHYSICIAN) %>% summarise(n5=sum(n)) %>% arrange(-n5) %>% filter(PHYSICIAN != "FACILITY"))


temp5 <- NASH_US_Doses %>% filter(drug_group == "Antidiabetic") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n6=sum(n)) %>% arrange(-n6) %>% filter(PHYSICIAN != "FACILITY")


temp6 <- data.frame(NASH_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% group_by(specialty) %>% summarise(n=n()) %>%
             left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
             ungroup() %>% group_by(PHYSICIAN) %>% summarise(n7=sum(n)) %>% arrange(-n7) %>% filter(PHYSICIAN != "FACILITY"))


temp7 <- data.frame(NASH_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% group_by(specialty) %>% summarise(n=n()) %>%
             left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
             ungroup() %>% group_by(PHYSICIAN) %>% summarise(n8=sum(n)) %>% arrange(-n8) %>% filter(PHYSICIAN != "FACILITY"))


temp8 <- data.frame(NASH_US_Doses %>% filter(drug_group == "Hospitalization") %>% group_by(specialty) %>% summarise(n=n()) %>%
             left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
             ungroup() %>% group_by(PHYSICIAN) %>% summarise(n9=sum(n)) %>% arrange(-n9) %>% filter(PHYSICIAN != "FACILITY"))


temp1 %>% left_join(temp2) %>% left_join(temp3) %>% left_join(temp4) %>% left_join(temp5) %>% left_join(temp6) %>% left_join(temp7) %>% left_join(temp8)

NASH_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")


NASH_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")


NASH_US_Doses %>% filter(drug_group == "Hepatoprotective") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")

# ------
# Dx per Specialty NASH/NAFLD ---------

# First NASH Dx

NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% filter(condition=="NASH") %>% 
  group_by(patid) %>% slice(1)

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

temp <- data.frame(NASH_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>%
                     group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n))

fwrite(temp, "First_NASH_Dx_Physicians.txt", sep="\t")


# All Events

NASH_Events <- fread("NASH Events.txt")

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

temp2 <- data.frame(NASH_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>%
                      group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n))

NASH_Events <- NASH_Events %>% left_join(NASH_Event_Claims_Providers)

fwrite(temp2, "ALLEvents_Physicians.txt", sep="\t")




# First Fibrosis Event


NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% filter(condition=="Fibrosis") %>% 
  group_by(patid) %>% slice(1)

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

temp <- data.frame(NASH_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>%
                     group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n))

fwrite(temp, "First_Fibrosis_Dx_Physicians.txt", sep="\t")



# First NAFLD Dx

NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NAFLD_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% filter(condition=="NAFLD") %>% 
  group_by(patid) %>% slice(1)

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

temp <- data.frame(NAFLD_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>%
                     group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n))

fwrite(temp, "First_NAFLD_Dx_Physicians.txt", sep="\t")






# First NASH Dx by type of NASH 

NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_diagnosis %>% filter(grepl("NASH",NASH_diganosis)) %>% summarise(n=sum(weight)) # 1339983
NASH_diagnosis <- NASH_diagnosis %>%filter(grepl("NASH",NASH_diganosis))

NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% filter(condition=="NASH") %>% 
  group_by(patid) %>% slice(1)

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)


NASH_Events <- NASH_diagnosis %>% select(patient, NASH_diganosis) %>% left_join(NASH_Events, by=c("patient"="patid"))


temp <- data.frame(NASH_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>%
                     group_by(NASH_diganosis, specialty) %>% summarise(n=sum(weight)) %>% arrange(NASH_diganosis,-n))

fwrite(temp, "First_NASH_TypeOfNASH_Dx_Physicians.txt", sep="\t")



# -----
# Scripts per Specialty NAFLD -------------
NAFLD_US_Doses <- fread("NAFLD Doses.txt")
NAFLD_US_Doses <- NAFLD_US_Doses %>% filter(status != "G")
NAFLD_US_Doses <- NAFLD_US_Doses %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
NAFLD_US_Doses <- NAFLD_US_Doses %>% mutate(from_dt = as.Date(from_dt))
NAFLD_US_Doses <- NAFLD_US_Doses %>%filter(from_dt >= "2016-05-01" & from_dt <= "2021-04-30") 

Specialties_to_keep <- fread("Specialties_to_keep.txt")

temp1 <- data.frame(NAFLD_US_Doses %>% group_by(specialty) %>% summarise(n=n()) %>%
                      left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
                      ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))

temp2 <- data.frame(NAFLD_US_Doses %>% filter(drug_group == "Anticholesterol") %>% group_by(specialty) %>% summarise(n=n()) %>%
                      left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
                      ungroup() %>% group_by(PHYSICIAN) %>% summarise(n3=sum(n)) %>% arrange(-n3) %>% filter(PHYSICIAN != "FACILITY"))


temp3 <- data.frame(NAFLD_US_Doses %>% filter(drug_group == "Antiobesity") %>% group_by(specialty) %>% summarise(n=n()) %>%
                      left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
                      ungroup() %>% group_by(PHYSICIAN) %>% summarise(n4=sum(n)) %>% arrange(-n4) %>% filter(PHYSICIAN != "FACILITY"))

temp4 <- data.frame(NAFLD_US_Doses %>% filter(drug_group == "Hepatoprotective") %>% group_by(specialty) %>% summarise(n=n()) %>%
                      left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
                      ungroup() %>% group_by(PHYSICIAN) %>% summarise(n5=sum(n)) %>% arrange(-n5) %>% filter(PHYSICIAN != "FACILITY"))


temp5 <- NAFLD_US_Doses %>% filter(drug_group == "Antidiabetic") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n6=sum(n)) %>% arrange(-n6) %>% filter(PHYSICIAN != "FACILITY")


temp6 <- data.frame(NAFLD_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% group_by(specialty) %>% summarise(n=n()) %>%
                      left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
                      ungroup() %>% group_by(PHYSICIAN) %>% summarise(n7=sum(n)) %>% arrange(-n7) %>% filter(PHYSICIAN != "FACILITY"))


temp7 <- data.frame(NAFLD_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% group_by(specialty) %>% summarise(n=n()) %>%
                      left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
                      ungroup() %>% group_by(PHYSICIAN) %>% summarise(n8=sum(n)) %>% arrange(-n8) %>% filter(PHYSICIAN != "FACILITY"))


temp8 <- data.frame(NAFLD_US_Doses %>% filter(drug_group == "Hospitalization") %>% group_by(specialty) %>% summarise(n=n()) %>%
                      left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
                      ungroup() %>% group_by(PHYSICIAN) %>% summarise(n9=sum(n)) %>% arrange(-n9) %>% filter(PHYSICIAN != "FACILITY"))


temp1 %>% left_join(temp2) %>% left_join(temp3) %>% left_join(temp4) %>% left_join(temp5) %>% left_join(temp6) %>% left_join(temp7) %>% left_join(temp8)

NAFLD_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")


data.frame(NAFLD_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))


data.frame(NAFLD_US_Doses %>% filter(drug_group == "Hepatoprotective") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))


# ------
# NASH patients into NASH only, T2DM, OBE, T2DM+OBE. Drug penetrance in each group -----------
NASH_diagnosis <- fread("NASH_diagnosis.txt")

NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

DANU_Demographics <- fread("DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"

NASH_diagnosis <- NASH_diagnosis %>% left_join(DANU_Demographics %>% select(patient, weight, diagnosis))

NASH_diagnosis %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# diagnosis                n
# <chr>                <dbl>
# 1 -                   68944.
# 2 Diabetes            45373.
# 3 Diabetes + Obesity 746901.
# 4 Obesity            478765.


# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group, drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 976705 (7208 ever treated)

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 
 
# drug_group             n
# <chr>              <dbl>
# 1 Anticholesterol  730499.
# 2 Antidiabetic     604496.
# 3 Antiobesity      114826.
# 4 GLP1 Injectable  170337.
# 5 GLP1 Oral          9950.
# 6 Hepatoprotective  51530.
# 7 Hospitalization  118449.

data.frame(NASH_Drug_Histories %>% ungroup() %>% distinct() %>% 
             group_by(drug_group, drug_class) %>% summarise(n=sum(weight))) 

# drug_group         drug_class         n
# 1   Anticholesterol    Anticholesterol  87325.07
# 2   Anticholesterol                BAS  67375.43
# 3   Anticholesterol           Biologic  11231.46
# 4   Anticholesterol            Fibrate 102446.01
# 5   Anticholesterol             Statin 665866.85
# 6      Antidiabetic                AGI   4323.68
# 7      Antidiabetic       Antidiabetic    837.53
# 8      Antidiabetic          Biguanide 486387.13
# 9      Antidiabetic               DPP4 132596.55
# 10     Antidiabetic            Glinide  11244.11
# 11     Antidiabetic          Glitazone  47443.92
# 12     Antidiabetic            Insulin 277256.63
# 13     Antidiabetic              SGLT2 134884.43
# 14     Antidiabetic       Sulfonylurea 180540.26
# 15      Antiobesity          Anorectic 104846.42
# 16      Antiobesity        Antiobesity  15417.98
# 17      Antiobesity        Weight Loss   1064.67
# 18  GLP1 Injectable    GLP1 Injectable 170337.49
# 19        GLP1 Oral          GLP1 Oral   9950.37
# 20 Hepatoprotective   Hepatoprotective  51529.64
# 21  Hospitalization  Bariatric Surgery  65208.23
# 22  Hospitalization Hospital Inpatient  34944.73
# 23  Hospitalization   Liver Transplant  14371.65
# 24  Hospitalization  Surgery Inpatient  12427.75

sum(NASH_diagnosis$weight) # 1339983

NASH_Drug_Histories <- NASH_diagnosis %>% left_join(NASH_Drug_Histories)

data.frame(NASH_Drug_Histories %>% select(patient, weight, diagnosis, drug_group) %>% 
             distinct() %>% group_by(diagnosis, drug_group) %>%  summarise(n=sum(weight)))


# diagnosis       drug_group         n
# 1                   -  Anticholesterol  20709.63
# 2                   -      Antiobesity   3636.26
# 3                   - Hepatoprotective   1790.81
# 4                   -  Hospitalization    849.92
# 5                   -             <NA>  43711.12
# 6            Diabetes  Anticholesterol  27187.59
# 7            Diabetes     Antidiabetic  28448.73
# 8            Diabetes      Antiobesity   1282.14
# 9            Diabetes  GLP1 Injectable   3998.08
# 10           Diabetes        GLP1 Oral    134.95
# 11           Diabetes Hepatoprotective   1513.93
# 12           Diabetes  Hospitalization   3227.52
# 13           Diabetes             <NA>   8972.47
# 14 Diabetes + Obesity  Anticholesterol 497021.29
# 15 Diabetes + Obesity     Antidiabetic 574653.65
# 16 Diabetes + Obesity      Antiobesity  66834.93
# 17 Diabetes + Obesity  GLP1 Injectable 162229.20
# 18 Diabetes + Obesity        GLP1 Oral   9247.37
# 19 Diabetes + Obesity Hepatoprotective  33081.19
# 20 Diabetes + Obesity  Hospitalization  83302.53
# 21 Diabetes + Obesity             <NA>  71354.89
# 22            Obesity  Anticholesterol 185580.18
# 23            Obesity     Antidiabetic   1393.57
# 24            Obesity      Antiobesity  43072.47
# 25            Obesity  GLP1 Injectable   4110.21
# 26            Obesity        GLP1 Oral    568.05
# 27            Obesity Hepatoprotective  15143.71
# 28            Obesity  Hospitalization  31069.02
# 29            Obesity             <NA> 239239.90

data.frame(NASH_Drug_Histories %>% select(patient, weight, diagnosis, drug_class) %>% 
             distinct() %>% group_by(diagnosis, drug_class) %>%  summarise(n=sum(weight)))


# 1                   -          Anorectic   3636.26
# 2                   -    Anticholesterol   3448.63
# 3                   -                BAS   2104.28
# 4                   -           Biologic    806.87
# 5                   -            Fibrate   3296.92
# 6                   -   Hepatoprotective   1790.81
# 7                   - Hospital Inpatient    849.92
# 8                   -             Statin  17729.59
# 9                   -               <NA>  43711.12
# 10           Diabetes                AGI    119.13
# 11           Diabetes          Anorectic   1282.14
# 12           Diabetes    Anticholesterol   2808.92
# 13           Diabetes  Bariatric Surgery    240.69
# 14           Diabetes                BAS   2558.22
# 15           Diabetes          Biguanide  21501.91
# 16           Diabetes           Biologic    363.26
# 17           Diabetes               DPP4   7106.74
# 18           Diabetes            Fibrate   4635.38
# 19           Diabetes            Glinide    344.41
# 20           Diabetes          Glitazone   2653.51
# 21           Diabetes    GLP1 Injectable   3998.08
# 22           Diabetes          GLP1 Oral    134.95
# 23           Diabetes   Hepatoprotective   1513.93
# 24           Diabetes Hospital Inpatient   2105.86
# 25           Diabetes            Insulin  11951.19
# 26           Diabetes   Liver Transplant   1054.13
# 27           Diabetes              SGLT2   4901.35
# 28           Diabetes             Statin  23535.52
# 29           Diabetes       Sulfonylurea   7410.38
# 30           Diabetes  Surgery Inpatient    707.89
# 31           Diabetes               <NA>   8972.47
# 32 Diabetes + Obesity                AGI   4204.55
# 33 Diabetes + Obesity          Anorectic  59816.58
# 34 Diabetes + Obesity    Anticholesterol  61919.60
# 35 Diabetes + Obesity       Antidiabetic    837.53
# 36 Diabetes + Obesity        Antiobesity  10765.17
# 37 Diabetes + Obesity  Bariatric Surgery  44670.49
# 38 Diabetes + Obesity                BAS  44354.23
# 39 Diabetes + Obesity          Biguanide 464581.34
# 40 Diabetes + Obesity           Biologic   7604.23
# 41 Diabetes + Obesity               DPP4 125489.81
# 42 Diabetes + Obesity            Fibrate  73262.89
# 43 Diabetes + Obesity            Glinide  10899.70
# 44 Diabetes + Obesity          Glitazone  44790.41
# 45 Diabetes + Obesity    GLP1 Injectable 162229.20
# 46 Diabetes + Obesity          GLP1 Oral   9247.37
# 47 Diabetes + Obesity   Hepatoprotective  33081.19
# 48 Diabetes + Obesity Hospital Inpatient  24714.58
# 49 Diabetes + Obesity            Insulin 264443.97
# 50 Diabetes + Obesity   Liver Transplant  11355.74
# 51 Diabetes + Obesity              SGLT2 129754.86
# 52 Diabetes + Obesity             Statin 460597.33
# 53 Diabetes + Obesity       Sulfonylurea 172977.94
# 54 Diabetes + Obesity  Surgery Inpatient   8261.45
# 55 Diabetes + Obesity        Weight Loss    794.76
# 56 Diabetes + Obesity               <NA>  71354.89
# 57            Obesity          Anorectic  40111.44
# 58            Obesity    Anticholesterol  19147.92
# 59            Obesity        Antiobesity   4652.81
# 60            Obesity  Bariatric Surgery  20297.05
# 61            Obesity                BAS  18358.70
# 62            Obesity          Biguanide    303.88
# 63            Obesity           Biologic   2457.10
# 64            Obesity            Fibrate  21250.82
# 65            Obesity    GLP1 Injectable   4110.21
# 66            Obesity          GLP1 Oral    568.05
# 67            Obesity   Hepatoprotective  15143.71
# 68            Obesity Hospital Inpatient   7274.37
# 69            Obesity            Insulin    861.47
# 70            Obesity   Liver Transplant   1961.78
# 71            Obesity              SGLT2    228.22
# 72            Obesity             Statin 164004.41
# 73            Obesity       Sulfonylurea    151.94
# 74            Obesity  Surgery Inpatient   3458.41
# 75            Obesity        Weight Loss    269.91
# 76            Obesity               <NA> 239239.90

# -----
# How many using SGLT2 ---------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))


NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)


NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

sum(NASH_Drug_Histories$weight) #1339983

NASH_Drug_Histories %>% filter(grepl("56", month60)|grepl("57", month60)|grepl("58", month60)|grepl("59", month60)) %>%
  summarise(n=sum(weight))  60485.22/1339983


NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories %>% filter(grepl("56", Treat)|grepl("57", Treat)|grepl("58", Treat)|grepl("59", Treat)) %>%
  select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) 134884/1339983

# ------
# NASH patients with dyslipidemia  -----------
NASH_diagnosis <- fread("NASH_diagnosis.txt")

NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

NASH_Demographics <- fread("NASH Demographics All.txt")
names(NASH_Demographics)[1] <- "patient"

NASH_Demographics <- NASH_Demographics %>% select(patient, weight, dyslipidemia) %>% filter(!is.na(dyslipidemia))

NASH_diagnosis <- NASH_diagnosis %>% inner_join(NASH_Demographics %>% select(patient, weight,dyslipidemia))

# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group, drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 976705 (7208 ever treated)

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 

# drug_group             n
# <chr>              <dbl>
# 1 Anticholesterol  730499.
# 2 Antidiabetic     604496.
# 3 Antiobesity      114826.
# 4 GLP1 Injectable  170337.
# 5 GLP1 Oral          9950.
# 6 Hepatoprotective  51530.
# 7 Hospitalization  118449.

data.frame(NASH_Drug_Histories %>% ungroup() %>% distinct() %>% 
             group_by(drug_group, drug_class) %>% summarise(n=sum(weight))) 

# drug_group         drug_class         n
# 1   Anticholesterol    Anticholesterol  87325.07
# 2   Anticholesterol                BAS  67375.43
# 3   Anticholesterol           Biologic  11231.46
# 4   Anticholesterol            Fibrate 102446.01
# 5   Anticholesterol             Statin 665866.85
# 6      Antidiabetic                AGI   4323.68
# 7      Antidiabetic       Antidiabetic    837.53
# 8      Antidiabetic          Biguanide 486387.13
# 9      Antidiabetic               DPP4 132596.55
# 10     Antidiabetic            Glinide  11244.11
# 11     Antidiabetic          Glitazone  47443.92
# 12     Antidiabetic            Insulin 277256.63
# 13     Antidiabetic              SGLT2 134884.43
# 14     Antidiabetic       Sulfonylurea 180540.26
# 15      Antiobesity          Anorectic 104846.42
# 16      Antiobesity        Antiobesity  15417.98
# 17      Antiobesity        Weight Loss   1064.67
# 18  GLP1 Injectable    GLP1 Injectable 170337.49
# 19        GLP1 Oral          GLP1 Oral   9950.37
# 20 Hepatoprotective   Hepatoprotective  51529.64
# 21  Hospitalization  Bariatric Surgery  65208.23
# 22  Hospitalization Hospital Inpatient  34944.73
# 23  Hospitalization   Liver Transplant  14371.65
# 24  Hospitalization  Surgery Inpatient  12427.75

sum(NASH_diagnosis$weight) # 1104605

NASH_Drug_Histories <- NASH_diagnosis %>% left_join(NASH_Drug_Histories)

data.frame(NASH_Drug_Histories %>% select(patient, weight, drug_group) %>% 
             distinct() %>% group_by(drug_group) %>%  summarise(n=sum(weight)))

# drug_group         n
# 1  Anticholesterol 705739.66
# 2     Antidiabetic 555222.24
# 3      Antiobesity  90767.57
# 4  GLP1 Injectable 162411.13
# 5        GLP1 Oral   9227.41
# 6 Hepatoprotective  41703.30
# 7  Hospitalization 101181.65
# 8             <NA> 220118.69
# ----


# Type of insurance / coverage plan  ------------
NASH_Demographics <- fread("NASH Demographics All.txt")

NASH_Demographics <- NASH_Demographics %>% filter(diagnosis != "-")

NASH_Demographics %>% group_by(plan) %>% summarise(n =sum(weight)) # 15421963

# <chr>    <dbl>
# 1 C     9798494.
# 2 D      854577.
# 3 M     4768892.
# 
# C	Commercial private health care plan
# M	Medicare public health care plan
# D	Dual public and private health care plans

NASH_Demographics %>% group_by(plan, product) %>% summarise(n =sum(weight))

# plan  product        n
# <chr> <chr>      <dbl>
# 1 C     ?       2789632.
# 2 C     EPO      376777.
# 3 C     HMO     1032581.
# 4 C     IND       25690.
# 5 C     OTH       57885.
# 6 C     POS     1101846.
# 7 C     PPO     4414084.
# 8 D     ?        374129.
# 9 D     EPO       11034.
# 10 D     HMO      268687.
# 11 D     IND       17515.
# 12 D     OTH       29565.
# 13 D     POS        8217.
# 14 D     PPO      145429.
# 15 M     ?       1194837.
# 16 M     HMO     2412169.
# 17 M     OTH      943451.
# 18 M     POS       17270.
# 19 M     PPO      201164.


# EPO	Exclusive provider organization
# GPO	Group purchasing organization
# HMO	Health maintenance organization
# IND	Indemnity
# IPP	Individual program plan
# OTH	Other
# POS	Point of service
# PPO	Preferred provider organization
# SPN	State policy network
# ?	Unknown
# ----------
# Type of insurance / coverage plan T2 DM as control  ------------
DANU_Demographics <- fread("DANU Demographics.txt")

DANU_Demographics <- DANU_Demographics %>% filter(diagnosis == "Diabetes" | diagnosis == "Diabetes + Obesity")

DANU_Demographics %>% group_by(plan) %>% summarise(n = sum(weight)) # 15421963

# plan          n
# <chr>     <dbl>
# 1 C     26250392.
# 2 D      3000625.
# 3 M     18981657.

# C	Commercial private health care plan
# M	Medicare public health care plan
# D	Dual public and private health care plans

data.frame(DANU_Demographics %>% group_by(plan, product) %>% summarise(n =sum(weight)))

# plan product           n
# 1     C       ?  8790215.60
# 2     C     EPO   978133.56
# 3     C     GPO      320.71
# 4     C     HMO  2364487.76
# 5     C     IND   104757.28
# 6     C     IPP      119.00
# 7     C     OTH   166564.68
# 8     C     POS  2626889.10
# 9     C     PPO 11218784.57
# 10    C     SPN      119.87
# 11    D       ?  1570104.76
# 12    D     EPO    27538.88
# 13    D     GPO      200.96
# 14    D     HMO   765121.72
# 15    D     IND    60108.99
# 16    D     OTH   100978.71
# 17    D     POS    27672.96
# 18    D     PPO   448675.79
# 19    D     SPN      222.70
# 20    M       ?  5395125.91
# 21    M     EPO      119.13
# 22    M     HMO  7918030.67
# 23    M     OTH  4494819.26
# 24    M     POS    60945.40
# 25    M     PPO  1112616.97


# EPO	Exclusive provider organization
# GPO	Group purchasing organization
# HMO	Health maintenance organization
# IND	Indemnity
# IPP	Individual program plan
# OTH	Other
# POS	Point of service
# PPO	Preferred provider organization
# SPN	State policy network
# ?	Unknown

# ----------
# Type of insurance / coverage plan T2 DM as control  ------------
DANU_Demographics <- fread("DANU Demographics.txt")

DANU_Demographics <- DANU_Demographics %>% filter(diagnosis == "Obesity" | diagnosis == "Diabetes + Obesity")

DANU_Demographics %>% group_by(plan) %>% summarise(n = sum(weight)) 

# plan           n
# <chr>      <dbl>
#   1 C     101465007.
# 2 D       6310241.
# 3 M      38976761.

# C	Commercial private health care plan
# M	Medicare public health care plan
# D	Dual public and private health care plans

data.frame(DANU_Demographics %>% group_by(plan, product) %>% summarise(n =sum(weight)))

# 1     C       ? 30007186.68
# 2     C     EPO  3486977.75
# 3     C     GPO     2746.75
# 4     C     HMO  9729791.90
# 5     C     IND   223227.75
# 6     C     IPP     1422.58
# 7     C     OTH   782840.14
# 8     C     POS 11699309.37
# 9     C     PPO 45529206.00
# 10    C     SPN     2298.20
# 11    D       ?  2678099.28
# 12    D     EPO    69218.13
# 13    D     GPO      394.91
# 14    D     HMO  2103689.63
# 15    D     IND   120854.99
# 16    D     IPP      137.70
# 17    D     OTH   232054.83
# 18    D     POS    61051.15
# 19    D     PPO  1044449.25
# 20    D     SPN      290.97
# 21    M       ? 10124557.89
# 22    M     EPO      650.52
# 23    M     HMO 18027245.09
# 24    M     OTH  8651381.32
# 25    M     POS   103454.87
# 26    M     PPO  2069470.94


# EPO	Exclusive provider organization
# GPO	Group purchasing organization
# HMO	Health maintenance organization
# IND	Indemnity
# IPP	Individual program plan
# OTH	Other
# POS	Point of service
# PPO	Preferred provider organization
# SPN	State policy network
# ?	Unknown
# ---------

# NASH patients into diagnosis: Drug penetrance in each group -----------
NASH_Demographics <- fread("NASH Demographics All.txt")

Diagnosis_Type <- NASH_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight, diagnosis)
Diagnosis_Type <- Diagnosis_Type %>% filter(grepl("NASH", diagnosis))

names(Diagnosis_Type)[1] <- "patient"

Diagnosis_Type  %>% summarise(n=sum(weight)) #1339983

Diagnosis_Type  %>% group_by(diagnosis) %>% summarise(n=sum(weight)) #1339983

# diagnosis                             n
# <chr>                             <dbl>
# 1 NASH                            656698.
# 2 NASH with Chronic Liver Disease 187171.
# 3 NASH with Cirrhosis             202641.
# 4 NASH with Fibrosis               88498.
# 5 NASH with Hepatitis              82813.
# 6 NASH with Liver Cancer           24698.
# 7 NASH with Liver Failure          68530.
# 8 NASH with Liver Transplant       28935.

# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group, drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 976705 (7208 ever treated)

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 

drug_group             n
<chr>              <dbl>
  1 Anticholesterol  730499.
2 Antidiabetic     604496.
3 Antiobesity      114826.
4 GLP1 Injectable  170337.
5 GLP1 Oral          9950.
6 Hepatoprotective  51530.
7 Hospitalization  118449.

sum(Diagnosis_Type$weight) # 1339983

NASH_Drug_Histories <- Diagnosis_Type %>% left_join(NASH_Drug_Histories)

data.frame(NASH_Drug_Histories %>% select(patient, weight, diagnosis, drug_group) %>% 
             distinct() %>% group_by(diagnosis, drug_group) %>%  summarise(n=sum(weight)) %>%
             spread(key = diagnosis, value=n))


# diagnosis       drug_group         n
# 1                             NASH  Anticholesterol 339168.62
# 2                             NASH     Antidiabetic 259465.95
# 3                             NASH      Antiobesity  61388.53
# 4                             NASH  GLP1 Injectable  74130.67
# 5                             NASH        GLP1 Oral   5138.65
# 6                             NASH Hepatoprotective  11582.19
# 7                             NASH  Hospitalization  40968.51
# 8                             NASH             <NA> 206458.20
# 9  NASH with Chronic Liver Disease  Anticholesterol 102931.27
# 10 NASH with Chronic Liver Disease     Antidiabetic  77093.35
# 11 NASH with Chronic Liver Disease      Antiobesity  17435.43
# 12 NASH with Chronic Liver Disease  GLP1 Injectable  23252.05
# 13 NASH with Chronic Liver Disease        GLP1 Oral   1216.81
# 14 NASH with Chronic Liver Disease Hepatoprotective   4727.78
# 15 NASH with Chronic Liver Disease  Hospitalization  12003.11
# 16 NASH with Chronic Liver Disease             <NA>  53306.50
# 17             NASH with Cirrhosis  Anticholesterol 122406.66
# 18             NASH with Cirrhosis     Antidiabetic 117883.25
# 19             NASH with Cirrhosis      Antiobesity  13939.14
# 20             NASH with Cirrhosis  GLP1 Injectable  35564.87
# 21             NASH with Cirrhosis        GLP1 Oral   2367.03
# 22             NASH with Cirrhosis Hepatoprotective  14468.65
# 23             NASH with Cirrhosis  Hospitalization  18568.86
# 24             NASH with Cirrhosis             <NA>  39887.07
# 25              NASH with Fibrosis  Anticholesterol  47331.07
# 26              NASH with Fibrosis     Antidiabetic  42373.38
# 27              NASH with Fibrosis      Antiobesity   8686.87
# 28              NASH with Fibrosis  GLP1 Injectable  13748.41
# 29              NASH with Fibrosis        GLP1 Oral    622.51
# 30              NASH with Fibrosis Hepatoprotective   3085.46
# 31              NASH with Fibrosis  Hospitalization   7329.96
# 32              NASH with Fibrosis             <NA>  21756.07
# 33             NASH with Hepatitis  Anticholesterol  46057.64
# 34             NASH with Hepatitis     Antidiabetic  32249.26
# 35             NASH with Hepatitis      Antiobesity   8686.96
# 36             NASH with Hepatitis  GLP1 Injectable   8789.75
# 37             NASH with Hepatitis        GLP1 Oral    129.11
# 38             NASH with Hepatitis Hepatoprotective   1948.55
# 39             NASH with Hepatitis  Hospitalization   4977.42
# 40             NASH with Hepatitis             <NA>  24137.20
# 41          NASH with Liver Cancer  Anticholesterol  15364.08
# 42          NASH with Liver Cancer     Antidiabetic  13183.07
# 43          NASH with Liver Cancer      Antiobesity    929.10
# 44          NASH with Liver Cancer  GLP1 Injectable   3218.62
# 45          NASH with Liver Cancer        GLP1 Oral    164.60
# 46          NASH with Liver Cancer Hepatoprotective    891.90
# 47          NASH with Liver Cancer  Hospitalization   2390.37
# 48          NASH with Liver Cancer             <NA>   4969.49
# 49         NASH with Liver Failure  Anticholesterol  40334.41
# 50         NASH with Liver Failure     Antidiabetic  41585.07
# 51         NASH with Liver Failure      Antiobesity   2881.01
# 52         NASH with Liver Failure  GLP1 Injectable   9184.02
# 53         NASH with Liver Failure        GLP1 Oral    215.08
# 54         NASH with Liver Failure Hepatoprotective   4859.62
# 55         NASH with Liver Failure  Hospitalization  14824.63
# 56         NASH with Liver Failure             <NA>  10759.84
# 57      NASH with Liver Transplant  Anticholesterol  16904.94
# 58      NASH with Liver Transplant     Antidiabetic  20662.62
# 59      NASH with Liver Transplant      Antiobesity    878.76
# 60      NASH with Liver Transplant  GLP1 Injectable   2449.10
# 61      NASH with Liver Transplant        GLP1 Oral     96.58
# 62      NASH with Liver Transplant Hepatoprotective   9965.49
# 63      NASH with Liver Transplant  Hospitalization  17386.13
# 64      NASH with Liver Transplant             <NA>   2004.01
# -----
# NASH patients into NASH type: Drug penetrance in each group -----------
NASH_diagnosis <- fread("NASH_diagnosis.txt")

NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

NASH_diagnosis  %>% summarise(n=sum(weight)) #1339983

NASH_diagnosis  %>% group_by(NASH_diganosis) %>% summarise(n=sum(weight)) #1339983

# NASH_diganosis       n
# <chr>            <dbl>
# 1 NASH-Cirrohsis 303005.
# 2 NASH-Fibrosis   92489.
# 3 NASH-Only      944489.

# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group, drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 976705 (7208 ever treated)

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 

# drug_group             n
# <chr>              <dbl>
#   1 Anticholesterol  730499.
# 2 Antidiabetic     604496.
# 3 Antiobesity      114826.
# 4 GLP1 Injectable  170337.
# 5 GLP1 Oral          9950.
# 6 Hepatoprotective  51530.
# 7 Hospitalization  118449.

sum(NASH_diagnosis$weight) # 1339983


data.frame(NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group, drug_class) %>% distinct() %>% 
             group_by(drug_group, drug_class) %>% summarise(n=sum(weight)))

# drug_group         drug_class         n
# 1   Anticholesterol    Anticholesterol  87325.07
# 2   Anticholesterol                BAS  67375.43
# 3   Anticholesterol           Biologic  11231.46
# 4   Anticholesterol            Fibrate 102446.01
# 5   Anticholesterol             Statin 665866.85
# 6      Antidiabetic                AGI   4323.68
# 7      Antidiabetic       Antidiabetic    837.53
# 8      Antidiabetic          Biguanide 486387.13
# 9      Antidiabetic               DPP4 132596.55
# 10     Antidiabetic            Glinide  11244.11
# 11     Antidiabetic          Glitazone  47443.92
# 12     Antidiabetic            Insulin 277256.63
# 13     Antidiabetic              SGLT2 134884.43
# 14     Antidiabetic       Sulfonylurea 180540.26
# 15      Antiobesity          Anorectic 104846.42
# 16      Antiobesity        Antiobesity  15417.98
# 17      Antiobesity        Weight Loss   1064.67
# 18  GLP1 Injectable    GLP1 Injectable 170337.49
# 19        GLP1 Oral          GLP1 Oral   9950.37
# 20 Hepatoprotective   Hepatoprotective  51529.64
# 21  Hospitalization  Bariatric Surgery  65208.23
# 22  Hospitalization Hospital Inpatient  34944.73
# 23  Hospitalization   Liver Transplant  14371.65
# 24  Hospitalization  Surgery Inpatient  12427.75

NASH_Drug_Histories <- NASH_diagnosis %>% left_join(NASH_Drug_Histories)

data.frame(NASH_Drug_Histories %>% select(patient, weight, NASH_diganosis, drug_group, drug_class) %>% 
             distinct() %>% group_by(NASH_diganosis, drug_group, drug_class) %>%  summarise(n=sum(weight)))


# NASH_diganosis       drug_group         drug_class         n
# 1  NASH-Cirrohsis  Anticholesterol    Anticholesterol  18306.37
# 2  NASH-Cirrohsis  Anticholesterol                BAS  23782.17
# 3  NASH-Cirrohsis  Anticholesterol           Biologic   2744.41
# 4  NASH-Cirrohsis  Anticholesterol            Fibrate  22076.09
# 5  NASH-Cirrohsis  Anticholesterol             Statin 165025.63
# 6  NASH-Cirrohsis     Antidiabetic                AGI   1336.94
# 7  NASH-Cirrohsis     Antidiabetic       Antidiabetic    109.09
# 8  NASH-Cirrohsis     Antidiabetic          Biguanide 133374.21
# 9  NASH-Cirrohsis     Antidiabetic               DPP4  47493.29
# 10 NASH-Cirrohsis     Antidiabetic            Glinide   4426.39
# 11 NASH-Cirrohsis     Antidiabetic          Glitazone  15237.84
# 12 NASH-Cirrohsis     Antidiabetic            Insulin 108326.39
# 13 NASH-Cirrohsis     Antidiabetic              SGLT2  41619.98
# 14 NASH-Cirrohsis     Antidiabetic       Sulfonylurea  69474.60
# 15 NASH-Cirrohsis      Antiobesity          Anorectic  15969.63
# 16 NASH-Cirrohsis      Antiobesity        Antiobesity   2289.86
# 17 NASH-Cirrohsis      Antiobesity        Weight Loss    368.66
# 18 NASH-Cirrohsis  GLP1 Injectable    GLP1 Injectable  48488.49
# 19 NASH-Cirrohsis        GLP1 Oral          GLP1 Oral   2843.29
# 20 NASH-Cirrohsis Hepatoprotective   Hepatoprotective  29232.63
# 21 NASH-Cirrohsis  Hospitalization  Bariatric Surgery  10810.40
# 22 NASH-Cirrohsis  Hospitalization Hospital Inpatient  24263.99
# 23 NASH-Cirrohsis  Hospitalization   Liver Transplant  14273.85
# 24 NASH-Cirrohsis  Hospitalization  Surgery Inpatient   8762.81
# 25 NASH-Cirrohsis             <NA>               <NA>  52312.15
# 26  NASH-Fibrosis  Anticholesterol    Anticholesterol   6098.84
# 27  NASH-Fibrosis  Anticholesterol                BAS   4921.81
# 28  NASH-Fibrosis  Anticholesterol           Biologic    342.13
# 29  NASH-Fibrosis  Anticholesterol            Fibrate   7367.70
# 30  NASH-Fibrosis  Anticholesterol             Statin  45025.29
# 31  NASH-Fibrosis     Antidiabetic                AGI    143.66
# 32  NASH-Fibrosis     Antidiabetic          Biguanide  37434.20
# 33  NASH-Fibrosis     Antidiabetic               DPP4   9011.59
# 34  NASH-Fibrosis     Antidiabetic            Glinide    749.36
# 35  NASH-Fibrosis     Antidiabetic          Glitazone   3928.66
# 36  NASH-Fibrosis     Antidiabetic            Insulin  17650.11
# 37  NASH-Fibrosis     Antidiabetic              SGLT2   9260.98
# 38  NASH-Fibrosis     Antidiabetic       Sulfonylurea  10435.33
# 39  NASH-Fibrosis      Antiobesity          Anorectic   8051.11
# 40  NASH-Fibrosis      Antiobesity        Antiobesity    706.75
# 41  NASH-Fibrosis      Antiobesity        Weight Loss    165.91
# 42  NASH-Fibrosis  GLP1 Injectable    GLP1 Injectable  14241.79
# 43  NASH-Fibrosis        GLP1 Oral          GLP1 Oral    622.51
# 44  NASH-Fibrosis Hepatoprotective   Hepatoprotective   3563.79
# 45  NASH-Fibrosis  Hospitalization  Bariatric Surgery   6515.27
# 46  NASH-Fibrosis  Hospitalization Hospital Inpatient    954.57
# 47  NASH-Fibrosis  Hospitalization  Surgery Inpatient    609.23
# 48  NASH-Fibrosis             <NA>               <NA>  22951.64
# 49      NASH-Only  Anticholesterol    Anticholesterol  62919.86
# 50      NASH-Only  Anticholesterol                BAS  38671.45
# 51      NASH-Only  Anticholesterol           Biologic   8144.92
# 52      NASH-Only  Anticholesterol            Fibrate  73002.22
# 53      NASH-Only  Anticholesterol             Statin 455815.93
# 54      NASH-Only     Antidiabetic                AGI   2843.08
# 55      NASH-Only     Antidiabetic       Antidiabetic    728.44
# 56      NASH-Only     Antidiabetic          Biguanide 315578.72
# 57      NASH-Only     Antidiabetic               DPP4  76091.67
# 58      NASH-Only     Antidiabetic            Glinide   6068.36
# 59      NASH-Only     Antidiabetic          Glitazone  28277.42
# 60      NASH-Only     Antidiabetic            Insulin 151280.13
# 61      NASH-Only     Antidiabetic              SGLT2  84003.47
# 62      NASH-Only     Antidiabetic       Sulfonylurea 100630.33
# 63      NASH-Only      Antiobesity          Anorectic  80825.68
# 64      NASH-Only      Antiobesity        Antiobesity  12421.37
# 65      NASH-Only      Antiobesity        Weight Loss    530.10
# 66      NASH-Only  GLP1 Injectable    GLP1 Injectable 107607.21
# 67      NASH-Only        GLP1 Oral          GLP1 Oral   6484.57
# 68      NASH-Only Hepatoprotective   Hepatoprotective  18733.22
# 69      NASH-Only  Hospitalization  Bariatric Surgery  47882.56
# 70      NASH-Only  Hospitalization Hospital Inpatient   9726.17
# 71      NASH-Only  Hospitalization   Liver Transplant     97.80
# 72      NASH-Only  Hospitalization  Surgery Inpatient   3055.71
# 73      NASH-Only             <NA>               <NA> 288014.59



# -----
# -----
# Claims Lab ----------------------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")

length(unique(NASH_Extract_Claims_Lab_Results$patid)) # 1753

length(unique(NASH_Extract_Claims_Lab_Results$tst_desc)) # 4587

NASH_Extract_Claims_Lab_Results %>% select(patid, tst_desc) %>% 
  distinct() %>% group_by(patid) %>% count() %>% arrange(-n) # MAX (389 Tests per pat)

NASH_Extract_Claims_Lab_Results_test_desc_countPats <- data.frame(NASH_Extract_Claims_Lab_Results %>% select(tst_desc, patid)  %>% distinct() %>% 
                                                                    group_by(tst_desc) %>% count() %>% arrange(-n))

# Enhanced Liver Fibrosis - - - -
# Hyaluronic acid (HA)   NONE
# Procollagen III amino-terminal peptide (PIIINP)   NONE
# Tissue inhibitor of matrix metalloproteinase 1 (TIMP-1)  NONE

# Fibrosis4 - - - - 
# Age (All I assume)
# AST +1000
# ALT  +1000
# Plat +1000

# MELD SCORES - - - - 
# INR ~300
# Creatinine +1000
# Bilirubin +1000
# Sodium +1000

# OTHER RELEVANT - - - - - 
# Hba1c  ~700
# SEX ?ll
# AGE all
# Alkaline Phospatase + 1000
# Albumin +1000
# Triglycerides +1000
# Hemoglobuin +1000
# Leukocyte counts for each population (EOS, BAS, LYMPH, etc) ~1000
# Eritroycyte Indexes : MCV, MHCV etc ~1000
# GFR / eGFR ~800


# Extract Labs ----------------
NASH_Extract_Labs <- fread("NASH Extract Labs.txt")

length(unique(NASH_Extract_Labs$patid)) # 5845
length(unique(NASH_Extract_Labs$test_type)) # 6
length(unique(NASH_Extract_Labs$encid))  # 110381
length(unique(NASH_Extract_Labs$test_name))  # 515

NASH_Extract_Labs_testname <- data.frame(NASH_Extract_Labs %>% select(test_name, patid)  %>% distinct() %>% 
                                           group_by(test_name) %>% count() %>% arrange(-n))

# NLP Measurements ------------------
NASH_Extract_NLP_Measurements <- fread("NASH Extract NLP Measurements.txt")

length(unique(NASH_Extract_NLP_Measurements$patid)) # 4774
length(unique(NASH_Extract_NLP_Measurements$measurement_type)) # 5380
length(unique(NASH_Extract_NLP_Measurements$encid))  # 110381
length(unique(NASH_Extract_NLP_Measurements$test_name))  # 515

NASH_Extract_Labs_testname <- data.frame(NASH_Extract_Labs %>% select(test_name, patid)  %>% distinct() %>% 
                                           group_by(test_name) %>% count() %>% arrange(-n))


# Observations --------------
NASH_Extract_Observations <- fread("NASH Extract Observations.txt")

length(unique(NASH_Extract_Observations$patid)) # 6485
length(unique(NASH_Extract_Observations$obs_type)) # 74


NASH_Extract_Observations_summary <- data.frame(NASH_Extract_Observations %>% select(obs_type, patid)  %>% distinct() %>% 
                                                  group_by(obs_type) %>% count() %>% arrange(-n))


# NASH Extract Symptom --------------
NASH_Extract_Symptoms <- fread("NASH Extract Symptoms.txt")

length(unique(NASH_Extract_Symptoms$patid)) # 6485
length(unique(NASH_Extract_Symptoms$code)) # 73217

Dx_code <- fread("NASH Diagnosis Codes.txt")
Dx_code <- Dx_code %>% select(code, condition, source, type, description)

NASH_Extract_Symptoms <- NASH_Extract_Symptoms %>% left_join(Dx_code)

unique(NASH_Extract_Symptoms$condition)

NASH_Extract_Symptoms %>% select(patid, condition) %>% distinct() %>% 
  filter(condition=="Liver Ultrasound")
# -----
# -----
# Pooling Lab Results across datasets FIB-4 --------------------
# ----- 
# AST -----------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, tst_desc )
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% drop_na()

NASH_Extract_Claims_Lab_Results %>% filter(grepl("AST", tst_desc)) %>% select(rslt_unit_nm, tst_desc) %>% distinct()

NASH_Extract_Claims_Lab_Results <- 
  NASH_Extract_Claims_Lab_Results %>% filter(tst_desc == "ASPARTATE TRANSAMINASE" | 
                                               tst_desc == "AST (SGOT)" | 
                                               tst_desc == "AST" | 
                                               tst_desc == "AST/SGOT" | 
                                               tst_desc == "AST (SGOT) P5P" | 
                                               tst_desc == "AST (SGOT)" | 
                                               tst_desc == "SGOT (AST)" | 
                                               tst_desc == "AST(SGOT)" | 
                                               tst_desc == "AST-SGOT" | 
                                               tst_desc == "AST(GOT) (POLY)")

unique(NASH_Extract_Claims_Lab_Results$rslt_unit_nm)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(TEST = "AST", ORIGIN = "ExtractClaimsLab")

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, TEST, ORIGIN)

names(NASH_Extract_Claims_Lab_Results)[3] <- "date"
names(NASH_Extract_Claims_Lab_Results)[4] <- "result"


NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, test_type, weight, test_name, result_date, test_result, result_unit)
NASH_Extract_Labs <- NASH_Extract_Labs %>% drop_na()

NASH_Extract_Labs %>% filter(grepl("Aspartate", test_name))

NASH_Extract_Labs %>% filter(grepl("AST", test_name)) %>% select(test_name, result_unit) %>% distinct()

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Aspartate aminotransferase (AST)")

unique(NASH_Extract_Labs$result_unit)
unique(NASH_Extract_Labs$test_result)

NASH_Extract_Labs$test_result <- as.numeric(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(!is.na(test_result))

NASH_Extract_Labs <- NASH_Extract_Labs %>% mutate(TEST = "AST", ORIGIN = "ExtractLabs")

NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs)

NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, weight, result_date, test_result, TEST, ORIGIN)

names(NASH_Extract_Labs)[3] <- "date"
names(NASH_Extract_Labs)[4] <- "result"

AST_Results_NASH_Pooled <-  NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% distinct()

AST_Results_NASH_Pooled <- AST_Results_NASH_Pooled %>% filter(result>4)

fwrite(AST_Results_NASH_Pooled, "AST_Results_NASH_Pooled.txt", sep="\t")






# -----
# ALT ----------------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, tst_desc )
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% drop_na()

NASH_Extract_Claims_Lab_Results %>% filter(grepl("ALT", tst_desc)) %>% select(rslt_unit_nm, tst_desc) %>% distinct()

NASH_Extract_Claims_Lab_Results <- 
  NASH_Extract_Claims_Lab_Results %>% filter(tst_desc == "ALANINE TRANSAMINASE" | 
                                               tst_desc == "ALT (SGPT)" | 
                                               tst_desc == "ALT" | 
                                               tst_desc == "ALT/SGPT" | 
                                               tst_desc == "ALT (SGPT) P5P" | 
                                               tst_desc == "ALT (SGPT)" | 
                                               tst_desc == "SGPT (ALT)" |
                                               tst_desc == "SGPT/ALT" | 
                                               tst_desc == "ALT(SGPT)" | 
                                               tst_desc == "ALT-SGOT" |
                                               tst_desc == "ALT (GPT) (POLY)")

unique(NASH_Extract_Claims_Lab_Results$rslt_unit_nm)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(TEST = "ALT", ORIGIN = "ExtractClaimsLab")

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, TEST, ORIGIN)

names(NASH_Extract_Claims_Lab_Results)[3] <- "date"
names(NASH_Extract_Claims_Lab_Results)[4] <- "result"


NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, test_type, weight, test_name, result_date, test_result, result_unit)
NASH_Extract_Labs <- NASH_Extract_Labs %>% drop_na()

NASH_Extract_Labs %>% filter(grepl("Alanine", test_name))

NASH_Extract_Labs %>% filter(grepl("ALT", test_name)) %>% select(test_name, result_unit) %>% distinct()

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Alanine aminotransferase (ALT)")

unique(NASH_Extract_Labs$result_unit)
unique(NASH_Extract_Labs$test_result)

NASH_Extract_Labs$test_result <- as.numeric(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(!is.na(test_result))

NASH_Extract_Labs <- NASH_Extract_Labs %>% mutate(TEST = "ALT", ORIGIN = "ExtractLabs")

NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, weight, result_date, test_result, TEST, ORIGIN)

names(NASH_Extract_Labs)[3] <- "date"
names(NASH_Extract_Labs)[4] <- "result"

ALT_Results_NASH_Pooled <-  NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% distinct()

ALT_Results_NASH_Pooled <- ALT_Results_NASH_Pooled %>% filter(result>4)


fwrite(ALT_Results_NASH_Pooled, "ALT_Results_NASH_Pooled.txt", sep="\t")

# ----
# Platelets ---------------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, tst_desc )
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% drop_na()

NASH_Extract_Claims_Lab_Results %>% filter(grepl("PLATELET", tst_desc)) %>% select(rslt_unit_nm, tst_desc) %>% distinct()

NASH_Extract_Claims_Lab_Results <- 
  NASH_Extract_Claims_Lab_Results %>% filter(tst_desc == "PLATELET" | 
                                               tst_desc == "PLATELET COUNT" | 
                                               tst_desc == "CBC W/DIFF, PLATELET CT." | 
                                               tst_desc == "CBC, NO DIFFERENTIAL/PLATELET" | 
                                               tst_desc == "CBC, PLATELET, NO DIFFERENTIAL" | 
                                               tst_desc == "PLATELET COUNT&PLATELET COUNT" | 
                                               tst_desc == "CBC WITH DIFFERENTIAL/PLATELET" |
                                               tst_desc == "PLATELETS")

unique(NASH_Extract_Claims_Lab_Results$rslt_unit_nm)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(TEST = "PLATELET", ORIGIN = "ExtractClaimsLab")

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, TEST, ORIGIN)

names(NASH_Extract_Claims_Lab_Results)[3] <- "date"
names(NASH_Extract_Claims_Lab_Results)[4] <- "result"
names(NASH_Extract_Claims_Lab_Results)[5] <- "units"


NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, test_type, weight, test_name, result_date, test_result, result_unit)
NASH_Extract_Labs <- NASH_Extract_Labs %>% drop_na()

NASH_Extract_Labs %>% filter(grepl("Platelet", test_name))

NASH_Extract_Labs %>% filter(grepl("Platelet", test_name)) %>% select(test_name, result_unit) %>% distinct()

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Platelet count (PLT)")

unique(NASH_Extract_Labs$result_unit)
unique(NASH_Extract_Labs$test_result)

NASH_Extract_Labs$test_result <- as.numeric(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(!is.na(test_result))

NASH_Extract_Labs <- NASH_Extract_Labs %>% mutate(TEST = "PLATELET", ORIGIN = "ExtractLabs")

NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, weight, result_date, test_result, result_unit, TEST, ORIGIN)

names(NASH_Extract_Labs)[3] <- "date"
names(NASH_Extract_Labs)[4] <- "result"
names(NASH_Extract_Labs)[5] <- "units"

Platelet_Results_NASH_Pooled <-  NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% distinct()

unique(Platelet_Results_NASH_Pooled$units)

Platelet_Results_NASH_Pooled <- Platelet_Results_NASH_Pooled %>% 
  filter(units != "" & units != "%"  & units != "G/DL" & units != "PG" & units != "FL" & 
           units != "GM/DL" & units != "fL" & units != "gm/dL" & units != "pg")

unique(Platelet_Results_NASH_Pooled$units)

range(Platelet_Results_NASH_Pooled$result)

Platelet_Results_NASH_Pooled <- Platelet_Results_NASH_Pooled %>% filter(!grepl("6", units))
Platelet_Results_NASH_Pooled <- Platelet_Results_NASH_Pooled %>% filter(units!="ml")
Platelet_Results_NASH_Pooled$units <- "Thousands/uL"
Platelet_Results_NASH_Pooled <- Platelet_Results_NASH_Pooled %>% filter(result>10)

fwrite(Platelet_Results_NASH_Pooled, "Platelet_Results_NASH_Pooled.txt", sep="\t")



# -----
# Fibrosis 4 Score Calculation Exact Date Match --------------
Platelet_Results_NASH_Pooled <- fread("Platelet_Results_NASH_Pooled.txt", sep="\t")
Platelet_Results_NASH_Pooled <- Platelet_Results_NASH_Pooled %>% select(-c(units))
AST_Results_NASH_Pooled <- fread("AST_Results_NASH_Pooled.txt", sep="\t")
ALT_Results_NASH_Pooled <- fread("ALT_Results_NASH_Pooled.txt", sep="\t")

DANU_Demographics <- fread("DANU Demographics.txt", sep="\t")

temp <- AST_Results_NASH_Pooled %>% inner_join(ALT_Results_NASH_Pooled, by = c("patid", "weight", "date")) %>%
  inner_join(Platelet_Results_NASH_Pooled, by = c("patid", "weight", "date"))

names(temp)[4] <- "resultAST"
names(temp)[5] <- "TESTAST"
names(temp)[6] <- "ORIGINAST"
names(temp)[7] <- "resultALT"
names(temp)[8] <- "TESTALT"
names(temp)[9] <- "ORIGINALT"
names(temp)[10] <- "resultPLATELETS"
names(temp)[11] <- "TESTPLATELETS"
names(temp)[12] <- "ORIGINPLATELETS"

temp <- temp %>% select(-c(ORIGINAST, ORIGINALT, ORIGINPLATELETS))

temp <- temp %>% left_join(DANU_Demographics %>% select(patid, age))

temp$Year <- format(as.POSIXct(temp$date, format = "%Y/%m/%d"), format="%Y")
temp$Year <- as.numeric(temp$Year)

temp$elapsed_time <- 2021-temp$Year

temp$ageactual <- temp$age-temp$elapsed_time

temp$fibrosis4 <- (temp$ageactual*temp$resultAST) / (temp$resultPLATELETS*sqrt(temp$resultALT))



temp <- fread("FIB4_ExactDate.txt", sep="\t")
temp <- temp %>% distinct()

fwrite(temp, "FIB4_ExactDate.txt", sep="\t")


range(temp$fibrosis4)

median(temp$fibrosis4)  # 1.565227
mean(temp$fibrosis4)  # 5.088158
length(unique(temp$patid)) # 4397
quantile(temp$fibrosis4, c(.25, .50, .75)) 
# 
# 25%       50%       75% 
# 0.9421246 1.5652274 3.3945945

temp %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")


temp %>% group_by(patid) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #3.64

temp %>% group_by(patid) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.58

temp2 <- temp %>% group_by(patid) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%      50%      75% 
# 1.000272 1.577639 2.851628 


temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")

fwrite(temp, "FIB4_ExactDate.txt", sep="\t")



# -----
# Compare FIB4 with Dx of NASH Dates Exact Date Match -------------
FIB4_ExactDate<- fread("FIB4_ExactDate.txt", sep="\t")

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"

FIB4_ExactDate <- FIB4_ExactDate %>% left_join(NASH_Events) 

fwrite(FIB4_ExactDate, "FIB4_ExactDate.txt", sep="\t")

# 2987 of the 4397 pats have lab date before the first NASH Dx

# Check MAX FIB-4 Before NASH Dx
FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(mean=mean(fibrosis4))

FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(median=median(fibrosis4))

FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(quantile25=quantile(fibrosis4, 0.25))

FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(quantile75=quantile(fibrosis4, 0.75))


FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="darkslategray4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")


# Check FIB-4 Closest to NASH Dx
FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(mean=mean(fibrosis4))

FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(median=median(fibrosis4))


FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(quantile25=quantile(fibrosis4, 0.25))

FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(quantile75=quantile(fibrosis4, 0.75))


FIB4_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% 
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="darkturquoise")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")




# -----
# After the first NASH Dx, how many are in each bucket of risk ? <1.3  1.2-2.67 >2.67 -----------

FIB4_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(mean=mean(fibrosis4)) # 3.71


FIB4_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(median=median(fibrosis4)) # 1.56


FIB4_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(quantile25=quantile(fibrosis4, 0.25)) # 1.01


FIB4_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(quantile75=quantile(fibrosis4, 0.75)) #2.84


FIB4_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% 
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



FIB4_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% select(fibrosis4) %>% arrange(-fibrosis4) %>%
  mutate(bucket=ifelse(fibrosis4<1.3,"low",
                       ifelse(fibrosis4>2.67,"High", "Medium"))) %>%
  group_by(bucket) %>% count()


# ------

# Pooling Lab Results across datasets MELD SCORES --------------------
# ----- 
# Creatinine -----------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, tst_desc )
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% drop_na()

NASH_Extract_Claims_Lab_Results %>% filter(grepl("CREATININE", tst_desc)) %>% select(rslt_unit_nm, tst_desc) %>% distinct()

NASH_Extract_Claims_Lab_Results <- 
  NASH_Extract_Claims_Lab_Results %>% filter(tst_desc == "CREATININE" | tst_desc == "CREATININE SERUM")

unique(NASH_Extract_Claims_Lab_Results$rslt_unit_nm)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% filter(rslt_nbr != 0.00)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(TEST = "CREATININE", ORIGIN = "ExtractClaimsLab")

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, TEST, ORIGIN)

names(NASH_Extract_Claims_Lab_Results)[3] <- "date"
names(NASH_Extract_Claims_Lab_Results)[4] <- "result"


NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, test_type, weight, test_name, result_date, test_result, result_unit)
NASH_Extract_Labs <- NASH_Extract_Labs %>% drop_na()

NASH_Extract_Labs %>% filter(grepl("Creatinine", test_name))

NASH_Extract_Labs %>% filter(grepl("Creatinine", test_name)) %>% select(test_name, result_unit) %>% distinct()

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Creatinine")

unique(NASH_Extract_Labs$result_unit)
unique(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% mutate(TEST = "CREATININE", ORIGIN = "ExtractLabs")

NASH_Extract_Labs %>% filter(result_unit != "mm" & result_unit != "units" )

NASH_Extract_Labs$test_result <- as.numeric(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(!is.na(test_result))

NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, weight, result_date, test_result, TEST, ORIGIN)

names(NASH_Extract_Labs)[3] <- "date"
names(NASH_Extract_Labs)[4] <- "result"

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(result != 0.000)

CREATININE_Results_NASH_Pooled <-  NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% distinct()


fwrite(CREATININE_Results_NASH_Pooled, "CREATININE_Results_NASH_Pooled.txt", sep="\t")




# -----
# Bilirubin ----------------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, tst_desc )
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% drop_na()

NASH_Extract_Claims_Lab_Results %>% filter(grepl("BILIRUBIN", tst_desc)) %>% select(rslt_unit_nm, tst_desc) %>% distinct()

NASH_Extract_Claims_Lab_Results <- 
  NASH_Extract_Claims_Lab_Results %>% filter(tst_desc == "BILIRUBIN, TOTAL" | 
                                               tst_desc == "TOTAL BILIRUBIN" | 
                                               tst_desc == "BILIRUBIN,TOTAL" | 
                                               tst_desc == "BILIRUBIN TOTAL" )

unique(NASH_Extract_Claims_Lab_Results$rslt_unit_nm)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(TEST = "BILIRUBIN", ORIGIN = "ExtractClaimsLab")

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, TEST, ORIGIN)

names(NASH_Extract_Claims_Lab_Results)[3] <- "date"
names(NASH_Extract_Claims_Lab_Results)[4] <- "result"

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% filter(result>0)

NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, test_type, weight, test_name, result_date, test_result, result_unit)
NASH_Extract_Labs <- NASH_Extract_Labs %>% drop_na()

NASH_Extract_Labs %>% filter(grepl("Bilirubin", test_name))

NASH_Extract_Labs %>% filter(grepl("Bilirubin", test_name)) %>% select(test_name, result_unit) %>% distinct()

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Bilirubin.total")

unique(NASH_Extract_Labs$result_unit)
unique(NASH_Extract_Labs$test_result)

NASH_Extract_Labs$test_result <- as.numeric(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(!is.na(test_result))

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_result>0)

NASH_Extract_Labs <- NASH_Extract_Labs %>% mutate(TEST = "BLIRUBIN", ORIGIN = "ExtractLabs")

NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, weight, result_date, test_result, TEST, ORIGIN)

names(NASH_Extract_Labs)[3] <- "date"
names(NASH_Extract_Labs)[4] <- "result"

BILIRUBIN_Results_NASH_Pooled <-  NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% distinct()

fwrite(BILIRUBIN_Results_NASH_Pooled, "BILIRUBIN_Results_NASH_Pooled.txt", sep="\t")

# ----
# Sodium ---------------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, tst_desc )
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% drop_na()

NASH_Extract_Claims_Lab_Results %>% filter(grepl("SODIUM", tst_desc)) %>% select(rslt_unit_nm, tst_desc) %>% distinct()

NASH_Extract_Claims_Lab_Results <- 
  NASH_Extract_Claims_Lab_Results %>% filter(tst_desc == "SODIUM" | 
                                               tst_desc == "SODIUM, SERUM" | 
                                               tst_desc == "CHEMISTRY&SODIUM")

unique(NASH_Extract_Claims_Lab_Results$rslt_unit_nm)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% filter(rslt_nbr > 0)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(TEST = "SODIUM", ORIGIN = "ExtractClaimsLab")

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, TEST, ORIGIN)

names(NASH_Extract_Claims_Lab_Results)[3] <- "date"
names(NASH_Extract_Claims_Lab_Results)[4] <- "result"
names(NASH_Extract_Claims_Lab_Results)[5] <- "units"

NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, test_type, weight, test_name, result_date, test_result, result_unit)
NASH_Extract_Labs <- NASH_Extract_Labs %>% drop_na()

NASH_Extract_Labs %>% filter(grepl("Sodium", test_name))

NASH_Extract_Labs %>% filter(grepl("Sodium", test_name)) %>% select(test_name, result_unit) %>% distinct()

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Sodium (Na)")

unique(NASH_Extract_Labs$result_unit)
unique(NASH_Extract_Labs$test_result)

NASH_Extract_Labs$test_result <- as.numeric(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(!is.na(test_result))

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_result>0)

NASH_Extract_Labs <- NASH_Extract_Labs %>% mutate(TEST = "SODIUM", ORIGIN = "ExtractLabs")

NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, weight, result_date, test_result, result_unit, TEST, ORIGIN)

names(NASH_Extract_Labs)[3] <- "date"
names(NASH_Extract_Labs)[4] <- "result"
names(NASH_Extract_Labs)[5] <- "units"

SODIUM_Results_NASH_Pooled <-  NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% distinct()

fwrite(SODIUM_Results_NASH_Pooled, "SODIUM_Results_NASH_Pooled.txt", sep="\t")



# -----
# INR ---------------
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, tst_desc )
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% drop_na()

NASH_Extract_Claims_Lab_Results %>% filter(grepl("INR", tst_desc)) %>% select(rslt_unit_nm, tst_desc) %>% distinct()

NASH_Extract_Claims_Lab_Results <- 
  NASH_Extract_Claims_Lab_Results %>% filter(tst_desc == "INR")

unique(NASH_Extract_Claims_Lab_Results$rslt_unit_nm)

NASH_Extract_Claims_Lab_Results$rslt_nbr <- as.numeric(NASH_Extract_Claims_Lab_Results$rslt_nbr)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(TEST = "INR", ORIGIN = "ExtractClaimsLab")

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, weight, fst_dt, rslt_nbr, rslt_unit_nm, TEST, ORIGIN)

names(NASH_Extract_Claims_Lab_Results)[3] <- "date"
names(NASH_Extract_Claims_Lab_Results)[4] <- "result"
names(NASH_Extract_Claims_Lab_Results)[5] <- "units"

NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, test_type, weight, test_name, result_date, test_result, result_unit)
NASH_Extract_Labs <- NASH_Extract_Labs %>% drop_na()

NASH_Extract_Labs %>% filter(grepl("INR", test_name))

NASH_Extract_Labs %>% filter(grepl("INR", test_name)) %>% select(test_name, result_unit) %>% distinct()

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Prothrombin.INR")

unique(NASH_Extract_Labs$result_unit)
unique(NASH_Extract_Labs$test_result)

NASH_Extract_Labs$test_result <- as.numeric(NASH_Extract_Labs$test_result)

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(!is.na(test_result))

NASH_Extract_Labs <- NASH_Extract_Labs %>% mutate(TEST = "INR", ORIGIN = "ExtractLabs")

NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, weight, result_date, test_result, result_unit, TEST, ORIGIN)

names(NASH_Extract_Labs)[3] <- "date"
names(NASH_Extract_Labs)[4] <- "result"
names(NASH_Extract_Labs)[5] <- "units"

INR_Results_NASH_Pooled <-  NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% distinct()

unique(INR_Results_NASH_Pooled$units)

INR_Results_NASH_Pooled <- INR_Results_NASH_Pooled %>% filter(result>0)
fwrite(INR_Results_NASH_Pooled, "INR_Results_NASH_Pooled.txt", sep="\t")



# -----
# MELD SCORES Calculation Exact Date Match --------------
CREATININE_Results_NASH_Pooled <- fread("CREATININE_Results_NASH_Pooled.txt", sep="\t")
BILIRUBIN_Results_NASH_Pooled <- fread("BILIRUBIN_Results_NASH_Pooled.txt", sep="\t")
SODIUM_Results_NASH_Pooled <- fread("SODIUM_Results_NASH_Pooled.txt", sep="\t")
SODIUM_Results_NASH_Pooled <- SODIUM_Results_NASH_Pooled %>% select(-c(units))
INR_Results_NASH_Pooled <- fread("INR_Results_NASH_Pooled.txt", sep="\t")
INR_Results_NASH_Pooled <- INR_Results_NASH_Pooled %>% select(-c(units))

temp <- CREATININE_Results_NASH_Pooled %>% inner_join(BILIRUBIN_Results_NASH_Pooled, by = c("patid", "weight", "date")) %>%
  inner_join(SODIUM_Results_NASH_Pooled, by = c("patid", "weight", "date")) %>% inner_join(INR_Results_NASH_Pooled, by = c("patid", "weight", "date"))

names(temp)[4] <- "resultCREATININE"
names(temp)[5] <- "TESTCREATININE"
names(temp)[6] <- "ORIGINCREATININE"
names(temp)[7] <- "resultBILIRUBIN"
names(temp)[8] <- "TESTBILIRUBIN"
names(temp)[9] <- "ORIGINBILIRUBIN"
names(temp)[10] <- "resultSODIUM"
names(temp)[11] <- "TESTSODIUM"
names(temp)[12] <- "ORIGINSODIUM"
names(temp)[13] <- "resultINR"
names(temp)[14] <- "TESTINR"
names(temp)[15] <- "ORIGININR"

temp <- temp %>% select(-c(ORIGINCREATININE, ORIGINBILIRUBIN, ORIGINSODIUM, ORIGININR))


temp <- temp %>% mutate(resultBILIRUBIN=ifelse(resultBILIRUBIN<1,1,resultBILIRUBIN)) %>%
  mutate(resultINR=ifelse(resultINR<1,1,resultINR)) %>%
  mutate(resultCREATININE=ifelse(resultCREATININE<1,1,resultCREATININE)) %>%
  mutate(resultCREATININE=ifelse(resultCREATININE>4,4,resultCREATININE)) %>%
  mutate(resultSODIUM=ifelse(resultSODIUM<125,125,ifelse(resultSODIUM>137,137,resultSODIUM)))


temp$MELD <- ((log(temp$resultBILIRUBIN)*3.78) + (log(temp$resultINR)*11.2) + (log(temp$resultCREATININE)*9.57) + 6.43)
temp$MELDNa <- temp$MELD + 1.32 * (137 - temp$resultSODIUM) - (0.033*temp$MELD*(137-temp$resultSODIUM))

temp <- temp %>% distinct()

mean(temp$MELD) # 14.98523
mean(temp$MELDNa) # 16.36341

median(temp$MELD) # 12.87395
median(temp$MELDNa) # 14.97155

length(unique(temp$patid)) # 1564


quantile(temp$MELDNa, c(.25, .50, .75)) 
# 
# 25%       50%       75% 
# 9.050096 14.971546 21.728735

min(temp$MELDNa) #6.43
max(temp$MELDNa) #49.36482


temp %>% select(MELDNa) %>%
  ggplot(aes(MELDNa)) +
  geom_density(colour="deepskyblue4", fill="deepskyblue4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n MELD Score")


temp %>% group_by(patid) %>% filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(MELDNa)) # 11.4

temp %>% group_by(patid) %>% filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(MELDNa)) #8.65

temp2 <- temp %>% group_by(patid) %>% filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup()  

quantile(temp2$MELDNa, c(.25, .50, .75)) 

# 25%      50%      75% 
# 7.077493 8.645620 13.632019 


temp2 %>% select(MELDNa) %>%
  ggplot(aes(MELDNa)) +
  geom_density(colour="deeppink4", fill="deeppink4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n MELD Score")

fwrite(temp, "MELD_ExactDate.txt", sep="\t")

# -----
# Compare MELD with Dx of NASH Dates Exact Date Match -------------
MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"

MELD_ExactDate <- MELD_ExactDate %>% left_join(NASH_Events) 

fwrite(MELD_ExactDate, "MELD_ExactDate.txt", sep="\t")

# 792 of the 1564 pats have lab date before the first NASH Dx

# Check MAX MELD Before NASH Dx
MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(mean=mean(MELDNa))

MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(median=median(MELDNa))

MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(quantile25=quantile(MELDNa, 0.25))

MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa, 0.25)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(quantile75=quantile(MELDNa, 0.75))


MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>%
  ggplot(aes(MELDNa)) +
  geom_density(colour="midnightblue", fill="midnightblue", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n MELD Score")


# Check MELD Closest to NASH Dx
MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(mean=mean(MELDNa))

MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(median=median(MELDNa))


MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(quantile25=quantile(MELDNa, 0.25))

MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(quantile75=quantile(MELDNa, 0.75))


MELD_ExactDate %>% group_by(patid) %>% filter(date < FirstNASHDx) %>% 
  filter(date==max(date)) %>% slice(1) %>% 
  ggplot(aes(MELDNa)) +
  geom_density(colour="darkturquoise", fill="darkturquoise", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n FIB-4 Score")



# After the first NASH Dx

MELD_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(mean=mean(MELDNa)) # 11.6


MELD_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(median=median(MELDNa)) # 9.06


MELD_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(quantile25=quantile(MELDNa, 0.25)) # 7.34


MELD_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% ungroup() %>% select(MELDNa) %>% arrange(-MELDNa) %>%
  mutate(quantile75=quantile(MELDNa, 0.75)) #13.9


MELD_ExactDate %>% group_by(patid) %>% filter(date > FirstNASHDx) %>% 
  filter(MELDNa==max(MELDNa)) %>% slice(1) %>% 
  ggplot(aes(MELDNa)) +
  geom_density(colour="deepskyblue4", fill="deepskyblue4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Density \n")+xlab("\n MELD Score")


# ----
# Combine FIB4 and MELD scores --------------------------

MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")
MELD_ExactDate <- MELD_ExactDate %>% select(patid, weight, date, MELDNa, FirstNASHDx)
MELD_ExactDate$Test <- "MELDNa"
names(MELD_ExactDate)[4] <- "Score"

FIB4_ExactDate<- fread("FIB4_ExactDate.txt", sep="\t")
FIB4_ExactDate <- FIB4_ExactDate %>% select(patid, weight, date, fibrosis4, FirstNASHDx)
FIB4_ExactDate$Test <- "fibrosis4"
names(FIB4_ExactDate)[4] <- "Score"

Comb_FIB4_MELD <- FIB4_ExactDate %>% bind_rows(MELD_ExactDate)

# How many unique pats?
length(unique(Comb_FIB4_MELD$patid)) #4409

Pats_ratio %>% inner_join(Comb_FIB4_MELD %>% select(patid) %>% distinct())
 
# How many with each test before/after the first Dx?
Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>% filter(date < FirstNASHDx) %>% select(patid) %>% distinct() %>% count() #2987
Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>% filter(date < FirstNASHDx) %>% select(patid) %>% distinct() %>% count() #802

Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>% filter(date > FirstNASHDx) %>% select(patid) %>% distinct() %>% count() #3093
Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>% filter(date > FirstNASHDx) %>% select(patid) %>% distinct() %>% count() #1001


# How many with ANY test before/after the first Dx?
Comb_FIB4_MELD %>% filter(date < FirstNASHDx) %>% select(patid) %>% distinct() #
Comb_FIB4_MELD %>% filter(date > FirstNASHDx) %>% select(patid) %>% distinct() #


# Elapsed Months from test to first Dx
Comb_FIB4_MELD$elapsedTime <- ((Comb_FIB4_MELD$FirstNASHDx - Comb_FIB4_MELD$date) / 30.5)

# How many with test 12m before/after first Dx
Comb_FIB4_MELD %>% filter(elapsedTime <= 12 & elapsedTime>0) %>% select(patid, Test) %>% 
  distinct() %>%group_by(Test) %>% count()

Comb_FIB4_MELD %>% filter(elapsedTime <= 12 & elapsedTime>0) %>% select(patid) %>% 
  distinct() %>% count() #2172

# Test          n
# <chr>     <int>
# 1 fibrosis4  2159
# 2 MELDNa      521

Comb_FIB4_MELD %>% filter(elapsedTime >= 12 & elapsedTime>=0) %>% select(patid, Test) %>% 
  distinct() %>%group_by(Test) %>% count()

Comb_FIB4_MELD %>% filter(elapsedTime >= 12 & elapsedTime>=0) %>% select(patid) %>% 
  distinct() %>% count() #1935

# Test          n
# <chr>     <int>
# 1 fibrosis4  1931
# 2 MELDNa      374


# How many above thresold ?
Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>% filter(Score>=1.3) %>% select(patid) %>% distinct() #2676

Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>% filter(Score>=7) %>% select(patid) %>% distinct() #1179

Comb_FIB4_MELD %>% filter((Test=="MELDNa"&Score>=7)|(Test=="fibrosis4"&Score>=1.3)) %>% select(patid) %>% distinct() #2873


# How many above thresold ? 12m before
Comb_FIB4_MELD %>% filter(elapsedTime <= 12 & elapsedTime>0) %>% filter(Test=="fibrosis4") %>%
  filter(Score>=1.3) %>% select(patid) %>% distinct() #1215

Comb_FIB4_MELD %>% filter(elapsedTime <= 12 & elapsedTime>0) %>% filter(Test=="MELDNa") %>%
  filter(Score>=7) %>% select(patid) %>% distinct() #378

Comb_FIB4_MELD %>% filter(elapsedTime <= 12 & elapsedTime>0) %>%
  filter((Test=="MELDNa"&Score>=7)|(Test=="fibrosis4"&Score>=1.3)) %>% select(patid) %>% distinct() #1299


# How many above thresold ? 12m after
Comb_FIB4_MELD %>% filter(elapsedTime >= 12 & elapsedTime>=0) %>% filter(Test=="fibrosis4") %>%
  filter(Score>=1.3) %>% select(patid) %>% distinct() #964

Comb_FIB4_MELD %>% filter(elapsedTime >= 12 & elapsedTime>=0) %>% filter(Test=="MELDNa") %>%
  filter(Score>=7) %>% select(patid) %>% distinct() #267

Comb_FIB4_MELD %>% filter(elapsedTime >= 12 & elapsedTime>=0) %>%
  filter((Test=="MELDNa"&Score>=7)|(Test=="fibrosis4"&Score>=1.3)) %>% select(patid) %>% distinct() #1030


# How many above thresold after Dx?
Comb_FIB4_MELD %>% filter(date > FirstNASHDx) %>% filter(Test=="fibrosis4") %>%
  filter(Score>=1.3) %>% select(patid) %>% distinct() #1870

Comb_FIB4_MELD %>% filter(date > FirstNASHDx) %>% filter(Test=="MELDNa") %>%
  filter(Score>=7) %>% select(patid) %>% distinct() #778

Comb_FIB4_MELD %>% filter(date > FirstNASHDx) %>%
  filter((Test=="MELDNa"&Score>=7)|(Test=="fibrosis4"&Score>=1.3)) %>% select(patid) %>% distinct() #1990


# -----
# FIB4 and MELD over time -----------------------------
FIB4_ExactDate <- fread("FIB4_ExactDate.txt", sep="\t")
FIB4_ExactDate <- FIB4_ExactDate %>% select(patid, weight, date, fibrosis4, FirstNASHDx)
FIB4_ExactDate$Test <- "fibrosis4"
names(FIB4_ExactDate)[4] <- "Score"
FIB4_ExactDate$elapsedTime <- round(((FIB4_ExactDate$date  - FIB4_ExactDate$FirstNASHDx) / 30.5))


MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")
MELD_ExactDate <- MELD_ExactDate %>% select(patid, weight, date, MELDNa, FirstNASHDx)
MELD_ExactDate$Test <- "MELDNa"
names(MELD_ExactDate)[4] <- "Score"
MELD_ExactDate$elapsedTime <- round(((MELD_ExactDate$date - MELD_ExactDate$FirstNASHDx ) / 30.5))


# MELD over time

MELD_ExactDate %>%
  group_by(patid) %>% filter(Score==max(Score)) %>%
  slice(1) %>%
  ggplot(aes(elapsedTime, Score)) +
  geom_jitter(size=0.3, alpha=0.3)+
  geom_smooth(method="lm")+
  #ylim(6,40)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


MELD_ExactDate %>% arrange(patid) %>% group_by(patid) %>% filter(Score==max(Score)) %>%
  slice(1)  %>%
  ungroup() %>%
  select(patid, Score, elapsedTime) %>% distinct() %>%
  ggplot(aes(elapsedTime, Score)) +
  geom_smooth(method="loess", colour="deepskyblue4", size= 2, fill="deepskyblue4", alpha=0.5)+
  xlim(-24,24)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Time From Lab Results to First NASH Dx")+
  ylab("% Difference \n (over mean throughout follow-up) \n")



MELD_ExactDate %>% arrange(patid) %>% group_by(patid) %>% 
  mutate(MeanScore=mean(Score)) %>% 
  mutate(ScoreDiff = ((Score-MeanScore)/MeanScore)*100)  %>%
  ungroup() %>%
  select(patid, ScoreDiff, elapsedTime) %>% distinct() %>%
  ggplot(aes(elapsedTime, ScoreDiff)) +
  geom_jitter()+
  xlim(-24,24)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Time From Lab Results to First NASH Dx")+
  ylab("% Difference \n (over mean throughout follow-up) \n")



temp <- MELD_ExactDate %>% arrange(patid) %>% group_by(patid) %>% 
  mutate(MeanScore=mean(Score)) %>% 
  mutate(ScoreDiff = ((Score-MeanScore)/MeanScore)*100)  %>%
  ungroup() %>%
  select(patid, ScoreDiff, elapsedTime) %>% distinct() %>%
  filter(elapsedTime>-40 & elapsedTime< & ScoreDiff<0)  %>%
  arrange(patid, elapsedTime)




# FIB4 over time

FIB4_ExactDate %>%
  filter(Score<=10) %>%
  ggplot(aes(elapsedTime, Score)) +
  geom_smooth(method="lm")+
  xlim(-24,24)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


FIB4_ExactDate %>% arrange(patid) %>% filter(Score<=20) %>%
  group_by(patid) %>% filter(Score==max(Score)) %>%
  slice(1)  %>%
  ungroup()  %>%
  select(patid, Score, elapsedTime) %>% distinct() %>%
  ggplot(aes(elapsedTime, Score)) +
  geom_smooth(method="lm", colour="firebrick", size= 2, fill="firebrick", alpha=0.5)+
  xlim(-24,24)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Time From Lab Results to First NASH Dx")+
  ylab("% Difference \n (over mean throughout follow-up) \n")



# ------
# Age NASH vs NAFLD ----------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")

DANU_Demographics <- fread("DANU Demographics.txt", sep="\t")

NASH_diagnosis <- NASH_diagnosis %>% left_join(DANU_Demographics %>% select(patid, weight, age), 
                                               by=c("patient"="patid", "weight"="weight"))


NAFLD_pats <- NASH_diagnosis %>% filter(grepl("NAFLD", NASH_diganosis))
NASH_pats <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

weighted.mean(NAFLD_pats$age, NAFLD_pats$weight) #55.95375

NAFLD_pats %>% select(age) %>%
  ggplot(aes(age)) +
  geom_histogram(bins=72, colour="gray", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Age")+ ylab("Number of Patients \n")

weighted.mean(NASH_pats$age, NASH_pats$weight) #56.899

NASH_pats %>% select(age) %>%
  ggplot(aes(age)) +
  geom_histogram(bins=72, colour="gray", fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Age")+ ylab("Number of Patients \n")
# ---------


# Determine whether the scores are different in treated vs non-treated pats ---------------

MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")
MELD_ExactDate <- MELD_ExactDate %>% select(patid, weight, date, MELDNa, FirstNASHDx)
MELD_ExactDate$Test <- "MELDNa"
names(MELD_ExactDate)[4] <- "Score"

FIB4_ExactDate<- fread("FIB4_ExactDate.txt", sep="\t")
FIB4_ExactDate <- FIB4_ExactDate %>% select(patid, weight, date, fibrosis4, FirstNASHDx)
FIB4_ExactDate$Test <- "fibrosis4"
names(FIB4_ExactDate)[4] <- "Score"

Comb_FIB4_MELD <- FIB4_ExactDate %>% bind_rows(MELD_ExactDate)
Comb_FIB4_MELD$elapsedTime <- ((Comb_FIB4_MELD$FirstNASHDx - Comb_FIB4_MELD$date) / 30.5)

length(unique(Comb_FIB4_MELD$patid)) # 4409

# Ever treated
NASH_Drug_Histories     <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

length(unique((NASH_Drug_Histories$patient))) # 9772

NASH_Drug_Histories %>% filter(Treat != "-") %>% select(patient, weight) %>% distinct() %>% count() # 7208
Ever_Treated_NASH <- NASH_Drug_Histories %>% filter(Treat != "-") %>% select(patient) %>% distinct() 
Ever_Treated_NASH$EverTreated <- "EverTreated"

Ever_Labs_NASH <- Comb_FIB4_MELD %>% select(patid) %>% distinct()

Ever_Labs_NASH$Labs <- "WithLabResults"

TreatLabsPats <- Ever_Treated_NASH %>% full_join(Ever_Labs_NASH, by=c("patient"="patid"))

NASH_pats <- NASH_Drug_Histories %>% select(patient) %>% distinct()

TreatLabsPats <- NASH_pats %>% full_join(TreatLabsPats)


# Check whether lab testing and treatment status are in any way correlated

TreatLabsPats %>% group_by(EverTreated, Labs) %>% count()

# EverTreated Labs               n
# <chr>       <chr>          <int>
# 1 EverTreated WithLabResults  3352
# 2 EverTreated NA              3856
# 3 NA          WithLabResults  1057
# 4 NA          NA              1507

TreatLabsPats %>% group_by(Labs, EverTreated) %>% count()

# Labs           EverTreated     n
# <chr>          <chr>       <int>
# 1 WithLabResults EverTreated  3352
# 2 WithLabResults NA           1057
# 3 NA             EverTreated  3856
# 4 NA             NA           1507


# Split based on labs/treatment status

TreatedPats_Scores <- TreatLabsPats %>% filter(Labs == "WithLabResults" & EverTreated =="EverTreated") 
NonTreatedPats_Scores <- TreatLabsPats %>% filter(Labs == "WithLabResults" & is.na(EverTreated)) 

TreatedPats_Scores <- TreatedPats_Scores %>% left_join(Comb_FIB4_MELD, by=c("patient"="patid"))
NonTreatedPats_Scores <- NonTreatedPats_Scores %>% left_join(Comb_FIB4_MELD, by=c("patient"="patid"))

length(unique(TreatedPats_Scores$patient)) # 3352
length(unique(NonTreatedPats_Scores$patient)) # 1057

TreatedPats_Scores$EverTreated
NonTreatedPats_Scores$EverTreated <- "No"

AllTreatsLabs <- TreatedPats_Scores %>% bind_rows(NonTreatedPats_Scores)

Fib4 <- AllTreatsLabs %>% filter(Test=="fibrosis4")
Meld <- AllTreatsLabs %>% filter(Test=="MELDNa")


# Pick only the mean score for each patient
Fib4 <- Fib4 %>% group_by(patient) %>% mutate(Score = mean(Score)) %>% slice(1)
Meld <- Meld %>% group_by(patient) %>% mutate(Score = mean(Score)) %>% slice(1)




# Mean FIB4 Ever Treated vs Never Treated
mean(Fib4$Score[Fib4$EverTreated == "EverTreated"]) # 2.106822
mean(Fib4$Score[Fib4$EverTreated == "No"]) # 1.733353

# Mean MELD Ever Treated vs Never Treated
mean(Meld$Score[Meld$EverTreated == "EverTreated"]) # 10.17628
mean(Meld$Score[Meld$EverTreated == "No"]) # 9.7159

# Median FIB4 Ever Treated vs Never Treated
median(Fib4$Score[Fib4$EverTreated == "EverTreated"]) # 1.329038
median(Fib4$Score[Fib4$EverTreated == "No"]) # 1.146745

# Median FIB4 Ever Treated vs Never Treated
median(Meld$Score[Meld$EverTreated == "EverTreated"]) # 8.472001
median(Meld$Score[Meld$EverTreated == "No"]) # 7.841641




# Wilcoxon-Mann-Whitney test FIB4 Score ~ Ever Treated Status (using the median, as it's very left skewed )

wilcox.test(Score ~ EverTreated, data = Fib4, exact = FALSE)

# Wilcoxon rank sum test with continuity correction
# data:  Score by EverTreated
# W = 2014922, p-value = 0.000000000001134
# alternative hypothesis: true location shift is not equal to 0




# Wilcoxon-Mann-Whitney test MELD Score ~ Ever Treated Status (using the median, as it's very left skewed )

wilcox.test(Score ~ EverTreated, data = Meld, exact = FALSE)

# Wilcoxon rank sum test with continuity correction
# data:  Score by EverTreated
# W = 204551, p-value = 0.03891
# alternative hypothesis: true location shift is not equal to 0

# ----
# Check Scores before / after Surgery ------------------
MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")
MELD_ExactDate <- MELD_ExactDate %>% select(patid, weight, date, MELDNa, FirstNASHDx)
MELD_ExactDate$Test <- "MELDNa"
names(MELD_ExactDate)[4] <- "Score"

FIB4_ExactDate<- fread("FIB4_ExactDate.txt", sep="\t")
FIB4_ExactDate <- FIB4_ExactDate %>% select(patid, weight, date, fibrosis4, FirstNASHDx)
FIB4_ExactDate$Test <- "fibrosis4"
names(FIB4_ExactDate)[4] <- "Score"

Comb_FIB4_MELD <- FIB4_ExactDate %>% bind_rows(MELD_ExactDate)
Comb_FIB4_MELD$elapsedTime <- ((Comb_FIB4_MELD$FirstNASHDx - Comb_FIB4_MELD$date) / 30.5)

length(unique(Comb_FIB4_MELD$patid)) # 4409

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

setDT(Comb_FIB4_MELD)[, Month_Yrdate := format(as.Date(date), "%Y-%m") ]
setDT(Comb_FIB4_MELD)[, Month_YrFirstNASHDx := format(as.Date(FirstNASHDx), "%Y-%m") ]

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% select(patid, weight, Score, Test, elapsedTime, Month_Yrdate)

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% left_join(Months_lookup, by = c("Month_Yrdate" = "Month")) %>% 
  filter(!is.na(Exact_Month))

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% select(-c(Month_Yrdate, elapsedTime))



# Ever treated
NASH_Drug_Histories     <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease))

length(unique((NASH_Drug_Histories$patient))) # 9772

NASH_Drug_Histories <- NASH_Drug_Histories %>%
  mutate(SurgeryStatus = 
           ifelse(grepl("79", Treat)|grepl("80", Treat)|grepl("81", Treat)|grepl("82", Treat), "Surgery", "No"))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month, SurgeryStatus)

SurgeryPats <- NASH_Drug_Histories %>% filter(SurgeryStatus== "Surgery") %>% select(patient) %>% distinct()

NASH_Drug_Histories <- SurgeryPats %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(SurgeryStatus== "Surgery")
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))

Comb_FIB4_MELD <- SurgeryPats %>% inner_join(Comb_FIB4_MELD, by=c("patient"="patid"))
length(unique(Comb_FIB4_MELD$patient)) # 289

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% inner_join(NASH_Drug_Histories, by=c("patient"="patient")) %>% arrange(patient)

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% mutate(Exact_Month = Exact_Month-Month) %>% select(-c(Month, SurgeryStatus))

Comb_FIB4_MELD <- data.frame(Comb_FIB4_MELD %>% group_by(patient, Exact_Month, Test) %>% 
                               mutate(Score=mean(Score)) %>% slice(1))


Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>% filter(Exact_Month < 0) %>% summarise(n=mean(Score)) #4.409762

Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>% filter(Exact_Month > 0) %>% summarise(n=mean(Score)) #1.80873

Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>%
  filter(Score<20) %>%
  ggplot(aes(Exact_Month, Score))+
  geom_jitter(colour="deepskyblue4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After Surgery")+
  ylab("Mean FIB-4 Score \n")

Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>%
  filter(Score<20) %>%
  ggplot(aes(Exact_Month, Score))+
  geom_smooth(colour="deepskyblue4", fill="deepskyblue4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After Surgery")+
  ylab("Mean FIB-4 Score \n")



Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>% filter(Exact_Month < 0) %>% summarise(n=mean(Score)) #15.00274

Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>% filter(Exact_Month > 0) %>% summarise(n=mean(Score)) #10.79643

Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>%
  ggplot(aes(Exact_Month, Score))+
  geom_jitter(colour="deeppink4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After Surgery")+
  ylab("Mean MELD Score \n")

Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>%
  ggplot(aes(Exact_Month, Score))+
  geom_smooth(colour="deeppink4", fill="deeppink4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After Surgery")+
  ylab("Mean MELD Score \n")

# -----
# Check Scores before / after Surgery NEW FIB4 ------------------

FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_NASH_Pats <- FIB4_NASH_Pats %>% select(patient, claimed, fibrosis4)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

FIB4_NASH_Pats$Month_Yrdate <- format(as.Date(FIB4_NASH_Pats$claimed), "%Y-%m")


FIB4_NASH_Pats <- FIB4_NASH_Pats %>% left_join(Months_lookup, by = c("Month_Yrdate" = "Month")) %>% 
  filter(!is.na(Exact_Month))

FIB4_NASH_Pats <- FIB4_NASH_Pats %>% select(-c(Month_Yrdate))


# Ever treated
NASH_Drug_Histories     <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease))

length(unique((NASH_Drug_Histories$patient))) # 9772

NASH_Drug_Histories <- NASH_Drug_Histories %>%
  mutate(SurgeryStatus = 
           ifelse(grepl("79", Treat)|grepl("80", Treat)|grepl("81", Treat)|grepl("82", Treat), "Surgery", "No"))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month, SurgeryStatus)

SurgeryPats <- NASH_Drug_Histories %>% filter(SurgeryStatus== "Surgery") %>% select(patient) %>% distinct()

NASH_Drug_Histories <- SurgeryPats %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(SurgeryStatus== "Surgery")
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))



FIB4_NASH_Pats <- SurgeryPats %>% inner_join(FIB4_NASH_Pats)
length(unique(FIB4_NASH_Pats$patient)) # 290

FIB4_NASH_Pats <- FIB4_NASH_Pats %>% inner_join(NASH_Drug_Histories, by=c("patient"="patient")) %>% arrange(patient)

FIB4_NASH_Pats <- FIB4_NASH_Pats %>% mutate(Exact_Month = Exact_Month-Month) %>% select(-c(Month, SurgeryStatus))

FIB4_NASH_Pats <- FIB4_NASH_Pats %>% select(-claimed)
FIB4_NASH_Pats <- data.frame(FIB4_NASH_Pats %>% group_by(patient, Exact_Month) %>% 
                               mutate(fibrosis4=mean(fibrosis4)) %>% slice(1))


FIB4_NASH_Pats %>% filter(Exact_Month < 0) %>% summarise(n=mean(fibrosis4)) #4.455225

FIB4_NASH_Pats %>% filter(Exact_Month > 0) %>% summarise(n=mean(fibrosis4)) #2.093822


FIB4_NASH_Pats %>% 
  filter(fibrosis4<20) %>%
  ggplot(aes(Exact_Month, fibrosis4))+
  geom_jitter(colour="deepskyblue4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After Surgery")+
  ylab("Mean FIB-4 Score \n")

FIB4_NASH_Pats %>% 
  filter(fibrosis4<20) %>%
  ggplot(aes(Exact_Month, fibrosis4))+
  geom_smooth(colour="deepskyblue4", fill="deepskyblue4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After Surgery")+
  ylab("Mean FIB-4 Score \n")




# --------

# Check Scores before / after GLP1 Injectable -------------
MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")
MELD_ExactDate <- MELD_ExactDate %>% select(patid, weight, date, MELDNa, FirstNASHDx)
MELD_ExactDate$Test <- "MELDNa"
names(MELD_ExactDate)[4] <- "Score"

FIB4_ExactDate<- fread("FIB4_ExactDate.txt", sep="\t")
FIB4_ExactDate <- FIB4_ExactDate %>% select(patid, weight, date, fibrosis4, FirstNASHDx)
FIB4_ExactDate$Test <- "fibrosis4"
names(FIB4_ExactDate)[4] <- "Score"

Comb_FIB4_MELD <- FIB4_ExactDate %>% bind_rows(MELD_ExactDate)
Comb_FIB4_MELD$elapsedTime <- ((Comb_FIB4_MELD$FirstNASHDx - Comb_FIB4_MELD$date) / 30.5)

length(unique(Comb_FIB4_MELD$patid)) # 4409

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

setDT(Comb_FIB4_MELD)[, Month_Yrdate := format(as.Date(date), "%Y-%m") ]
setDT(Comb_FIB4_MELD)[, Month_YrFirstNASHDx := format(as.Date(FirstNASHDx), "%Y-%m") ]

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% select(patid, weight, Score, Test, elapsedTime, Month_Yrdate)

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% left_join(Months_lookup, by = c("Month_Yrdate" = "Month")) %>% 
  filter(!is.na(Exact_Month))

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% select(-c(Month_Yrdate, elapsedTime))



# Ever treated
NASH_Drug_Histories     <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease))

length(unique((NASH_Drug_Histories$patient))) # 9772

NASH_Drug_Histories <- NASH_Drug_Histories %>%
  mutate(GLP1InjectableStatus = 
           ifelse(grepl("70", Treat)|grepl("71", Treat)|grepl("72", Treat)|grepl("73", Treat)|grepl("74", Treat)|grepl("75", Treat), "GLP1Injectable", "No"))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month, GLP1InjectableStatus)

GLP1InjectablePats <- NASH_Drug_Histories %>% filter(GLP1InjectableStatus== "GLP1Injectable") %>% select(patient) %>% distinct()

NASH_Drug_Histories <- GLP1InjectablePats %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(GLP1InjectableStatus== "GLP1Injectable")
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))

Comb_FIB4_MELD <- GLP1InjectablePats %>% inner_join(Comb_FIB4_MELD, by=c("patient"="patid"))
length(unique(Comb_FIB4_MELD$patient)) # 552

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% inner_join(NASH_Drug_Histories, by=c("patient"="patient")) %>% arrange(patient)

Comb_FIB4_MELD <- Comb_FIB4_MELD %>% mutate(Exact_Month = Exact_Month-Month) %>% select(-c(Month, GLP1InjectableStatus))

Comb_FIB4_MELD <- data.frame(Comb_FIB4_MELD %>% group_by(patient, Exact_Month, Test) %>% 
                               mutate(Score=mean(Score)) %>% slice(1))


Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>% filter(Exact_Month < 0 & Exact_Month > (-12)) %>% summarise(n=mean(Score)) #1.949488

Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>% filter(Exact_Month > 0& Exact_Month < 12) %>% summarise(n=mean(Score)) #2.198779

Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>%
  group_by(patient, Exact_Month) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  filter(Score<20) %>%
  filter(Exact_Month > (-12) & Exact_Month < 12) %>%
  ggplot(aes(Exact_Month, Score))+
  geom_jitter(colour="deepskyblue4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After GLP1 Injectable")+
  ylab("Mean FIB-4 Score \n")

Comb_FIB4_MELD %>% filter(Test=="fibrosis4") %>%
  group_by(patient, Exact_Month) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  filter(Score<20) %>%
  #filter(Exact_Month > (-12) & Exact_Month < 12) %>%
  ggplot(aes(Exact_Month, Score))+
  geom_smooth(method="lm",colour="deepskyblue4", fill="deepskyblue4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After GLP1 Injectable")+
  ylab("Mean FIB-4 Score \n")



Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>% filter(Exact_Month < 0 & Exact_Month > (-12)) %>% summarise(n=mean(Score)) #15.00274

Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>% filter(Exact_Month > 0& Exact_Month < 12) %>% summarise(n=mean(Score)) #10.79643

Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>%
  #filter(Exact_Month > (-12) & Exact_Month < 12) %>%
  group_by(patient, Exact_Month) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  ggplot(aes(Exact_Month, Score))+
  geom_jitter(colour="deeppink4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After GLP1 Injectable")+
  ylab("Mean MELD Score \n")


Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>%
  group_by(patient, Exact_Month) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  ggplot(aes(Exact_Month))+
  geom_density(colour="deeppink4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After GLP1 Injectable")+
  ylab("Mean MELD Score \n")


Comb_FIB4_MELD %>% filter(Test=="MELDNa") %>%
  group_by(patient, Exact_Month) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  ggplot(aes(Exact_Month, Score))+
  geom_smooth(colour="deeppink4", fill="deeppink4", alpha=0.4, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After GLP1 Injectable")+
  ylab("Mean MELD Score \n")



# ------------

# FIB4 and MELD over time -----------------------------
FIB4_ExactDate <- fread("FIB4_ExactDate.txt", sep="\t")
FIB4_ExactDate <- FIB4_ExactDate %>% select(patid, weight, date, fibrosis4, FirstNASHDx)
FIB4_ExactDate$Test <- "fibrosis4"
names(FIB4_ExactDate)[4] <- "Score"
FIB4_ExactDate$elapsedTime <- round(((FIB4_ExactDate$date  - FIB4_ExactDate$FirstNASHDx) / 30.5))
names(FIB4_ExactDate)[1] <- "patient"

MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")
MELD_ExactDate <- MELD_ExactDate %>% select(patid, weight, date, MELDNa, FirstNASHDx)
MELD_ExactDate$Test <- "MELDNa"
names(MELD_ExactDate)[4] <- "Score"
MELD_ExactDate$elapsedTime <- round(((MELD_ExactDate$date - MELD_ExactDate$FirstNASHDx ) / 30.5))
names(MELD_ExactDate)[1] <- "patient"


to_keep_FIB4 <- FIB4_ExactDate %>% select(patient, elapsedTime) %>% filter(elapsedTime< (0)) %>% select(patient) %>% distinct()
to_keep2_FIB4 <- FIB4_ExactDate %>% select(patient, elapsedTime) %>% filter(elapsedTime>= 0 ) %>% select(patient) %>% distinct()



FIB4_ExactDate %>% anti_join(SurgeryPats) %>%
  #inner_join(to_keep_FIB4) %>% inner_join(to_keep2_FIB4) %>%
  group_by(patient, elapsedTime) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  filter(Score<20) %>%
  ggplot(aes(elapsedTime, Score)) +
  geom_jitter(size=0.3, alpha=0.3, colour="firebrick")+
  geom_smooth(method="lm", colour="deepskyblue4", fill="deepskyblue4")+
  #ylim(6,40)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("FIB-4 Score \n")+xlab("Time before/after 1st NASH Dx")



to_keep_FIB4 <- FIB4_ExactDate %>% select(patient, elapsedTime) %>% filter(elapsedTime< (-24)) %>% select(patient) %>% distinct()
to_keep2_FIB4 <- FIB4_ExactDate %>% select(patient, elapsedTime) %>% filter(elapsedTime> 24 ) %>% select(patient) %>% distinct()


temp <- FIB4_ExactDate %>% anti_join(SurgeryPats) %>%
  inner_join(to_keep_FIB4) %>% inner_join(to_keep2_FIB4) %>%
  group_by(patient, elapsedTime) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  filter(Score<20) %>%
  filter( elapsedTime < (-24) | elapsedTime > 24) %>%
  select(Score, elapsedTime) %>%
  mutate(Dx=ifelse(elapsedTime>0, 1, 0))

summary(glm( Dx ~ Score, data = temp, family = binomial))



temp %>% 
  ggplot(aes(Score, Dx)) +
  geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4 (2 years prior to vs after 1st NASH Dx)")+
  ylab("Probability of NASH Positive \n")



to_keep_MELD <- MELD_ExactDate %>% select(patient, elapsedTime) %>% filter(elapsedTime< (-24)) %>% select(patient) %>% distinct()
to_keep2_MELD <- MELD_ExactDate %>% select(patient, elapsedTime) %>% filter(elapsedTime> 24 ) %>% select(patient) %>% distinct()



temp2 <- MELD_ExactDate %>% anti_join(SurgeryPats) %>%
  inner_join(to_keep_MELD) %>% inner_join(to_keep2_MELD) %>%
  group_by(patient, elapsedTime) %>% mutate(Score=median(Score)) %>%
  slice(1) %>%
  filter( elapsedTime < (-24) | elapsedTime > 24) %>%
  select(Score, elapsedTime) %>%
  mutate(Dx=ifelse(elapsedTime>0, 1, 0))



temp2 %>% 
  ggplot(aes(Score, Dx)) +
  geom_point(alpha = 0.2, colour="deeppink3") +
  xlim(0,20)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="deepskyblue4", fill="deepskyblue4") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n MELD (2 years prior to vs after 1st NASH Dx)")+
  ylab("Probability of NASH Positive \n")



# -----------
# Get each lab result over time -------------------


NASH_Drug_Histories     <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease))

length(unique((NASH_Drug_Histories$patient))) # 9772

NASH_Drug_Histories <- NASH_Drug_Histories %>%
  mutate(SurgeryStatus = 
           ifelse(grepl("79", Treat)|grepl("80", Treat)|grepl("81", Treat)|grepl("82", Treat), "Surgery", "No"))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month, SurgeryStatus)

SurgeryPats <- NASH_Drug_Histories %>% filter(SurgeryStatus== "Surgery") %>% select(patient) %>% distinct()


#  AST

AST_Results_NASH_Pooled <- fread("AST_Results_NASH_Pooled.txt")

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"

AST_Results_NASH_Pooled <- AST_Results_NASH_Pooled %>% inner_join(NASH_Events) 

AST_Results_NASH_Pooled$date <- as.Date(AST_Results_NASH_Pooled$date)
AST_Results_NASH_Pooled$FirstNASHDx <- as.Date(AST_Results_NASH_Pooled$FirstNASHDx)

AST_Results_NASH_Pooled$elapsedTime <- round(((AST_Results_NASH_Pooled$date - AST_Results_NASH_Pooled$FirstNASHDx ) / 30.5))

AST_Results_NASH_Pooled$elapsedTime <- as.numeric(AST_Results_NASH_Pooled$elapsedTime)



to_keep_AST <- AST_Results_NASH_Pooled %>% select(patid, elapsedTime) %>% filter(elapsedTime< (0)) %>% select(patid) %>% distinct()
to_keep2_AST <- AST_Results_NASH_Pooled %>% select(patid, elapsedTime) %>% filter(elapsedTime> 0 ) %>% select(patid) %>% distinct()


AST_Results_NASH_Pooled %>% anti_join(SurgeryPats, by= c("patid"="patient")) %>%
  inner_join(to_keep_AST) %>% inner_join(to_keep2_AST) %>%
  group_by(patid, elapsedTime) %>% filter(result==median(result)) %>%
  slice(1) %>%
  ggplot(aes(elapsedTime, result)) +
  geom_jitter(size=0.3, alpha=0.3)+
  geom_smooth(method="lm")+
  ylim(0,500)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


to_keep_AST <- AST_Results_NASH_Pooled %>% select(patid, elapsedTime) %>% filter(elapsedTime<0) %>% select(patid) %>% distinct()
to_keep2_AST <- AST_Results_NASH_Pooled %>% select(patid, elapsedTime) %>% filter(elapsedTime>0) %>% select(patid) %>% distinct()

to_keep_AST %>% inner_join(to_keep2_AST) %>% inner_join(AST_Results_NASH_Pooled) %>%
  anti_join(SurgeryPats, by=c("patid"="patient")) %>%
 # filter(elapsedTime<=0) %>%
  group_by(patid, elapsedTime) %>% filter(result==median(result)) %>%
  slice(1) %>%
  ggplot(aes(elapsedTime, result)) +
  geom_jitter(size=1, alpha=0.3, colour="deepskyblue4")+
 # geom_smooth(method="lm", colour="firebrick", fill="firebrick")+
  ylim(0,500)+
  xlab("\n Time to/from First NASH Dx")+ylab("AST (IU/L) \n")  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


 data.frame(to_keep_AST %>% inner_join(to_keep2_AST) %>% inner_join(AST_Results_NASH_Pooled) %>%
              anti_join(SurgeryPats, by=c("patid"="patient")) %>%
              filter(elapsedTime<=0)%>%
                   group_by(patid, elapsedTime) %>% filter(result==median(result)) %>%
                   slice(1) %>% ungroup() %>% group_by(elapsedTime) %>% summarise(n=median(result)))

# ALT 

ALT_Results_NASH_Pooled <- fread("ALT_Results_NASH_Pooled.txt")

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"

ALT_Results_NASH_Pooled <- ALT_Results_NASH_Pooled %>% inner_join(NASH_Events) 

ALT_Results_NASH_Pooled$date <- as.Date(ALT_Results_NASH_Pooled$date)
ALT_Results_NASH_Pooled$FirstNASHDx <- as.Date(ALT_Results_NASH_Pooled$FirstNASHDx)

ALT_Results_NASH_Pooled$elapsedTime <- round(((ALT_Results_NASH_Pooled$date - ALT_Results_NASH_Pooled$FirstNASHDx ) / 30.5))

ALT_Results_NASH_Pooled$elapsedTime <- as.numeric(ALT_Results_NASH_Pooled$elapsedTime)

ALT_Results_NASH_Pooled %>%
  group_by(patid, elapsedTime) %>% filter(result==max(result)) %>%
  slice(1) %>%
  ggplot(aes(elapsedTime, result)) +
  geom_jitter(size=0.3, alpha=0.3, colour="deepskyblue4")+
  geom_smooth(method="lm", colour="firebrick", fill="firebrick")+
  ylim(0,500)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


to_keep_ALT <- ALT_Results_NASH_Pooled %>% select(patid, elapsedTime) %>% filter(elapsedTime<0) %>% select(patid) %>% distinct()
to_keep2_ALT <- ALT_Results_NASH_Pooled %>% select(patid, elapsedTime) %>% filter(elapsedTime>0) %>% select(patid) %>% distinct()

to_keep_ALT %>% inner_join(to_keep2_ALT) %>% anti_join(SurgeryPats, by=c("patid"="patient")) %>%
  inner_join(ALT_Results_NASH_Pooled) %>%
  group_by(patid, elapsedTime) %>% filter(result==median(result)) %>%
  slice(1) %>%
  ggplot(aes(elapsedTime, result)) +
  geom_jitter(size=1, alpha=0.3, colour="deepskyblue4")+
  #geom_smooth(method="lm", colour="firebrick", fill="firebrick")+
  ylim(0,500)+xlim(-12,12)+
  xlab("\n Time to/from First NASH Dx")+ylab("ALT (IU/L) \n")  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


data.frame(to_keep_ALT %>% inner_join(to_keep2_ALT) %>% anti_join(SurgeryPats, by=c("patid"="patient")) %>%
             inner_join(ALT_Results_NASH_Pooled) %>%
             filter(elapsedTime<=0) %>%
             group_by(patid, elapsedTime) %>% filter(result==median(result)) %>%
             slice(1) %>% ungroup() %>% group_by(elapsedTime) %>% summarise(n=median(result)))



temp <- to_keep_ALT %>% inner_join(to_keep2_ALT) %>% anti_join(SurgeryPats, by=c("patid"="patient")) %>%
  inner_join(ALT_Results_NASH_Pooled) %>% bind_rows(to_keep_AST %>% inner_join(to_keep2_AST) %>% anti_join(SurgeryPats, by=c("patid"="patient")) %>%
                                                      inner_join(AST_Results_NASH_Pooled))



temp %>% select(-c(ORIGIN, date, FirstNASHDx, weight)) %>% group_by(patid, elapsedTime) %>% distinct() %>%
  pivot_wider(names_from = TEST, values_from = result, values_fn = median) %>%
  drop_na() %>% mutate(ratio=AST/ALT) %>%
  ungroup() %>% group_by(elapsedTime) %>% summarise(n=median(ratio)) %>%
  filter(elapsedTime<=0 & elapsedTime>= (-48)) %>%
  ggplot(aes(elapsedTime, n)) +
  geom_jitter(size=3, alpha=0.9, colour="deepskyblue4")+
  geom_smooth(method="lm", colour="firebrick", fill="firebrick")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

Pats_ratio_lessthan1 <- temp %>% select(-c(ORIGIN, date, FirstNASHDx, weight)) %>% group_by(patid, elapsedTime) %>% distinct() %>%
  pivot_wider(names_from = TEST, values_from = result, values_fn = median) %>% 
  drop_na() %>% mutate(ratio=AST/ALT) %>% ungroup() %>% filter(ratio<1) %>% select(patid) %>% distinct() 

Pats_ratio <- temp %>% select(-c(ORIGIN, date, FirstNASHDx, weight)) %>% group_by(patid, elapsedTime) %>% distinct() %>%
  pivot_wider(names_from = TEST, values_from = result, values_fn = median) %>% 
  drop_na() %>% mutate(ratio=AST/ALT) %>% ungroup()  %>% select(patid) %>% distinct() 


# Platelet  ---------
Platelet_Results_NASH_Pooled <- fread("Platelet_Results_NASH_Pooled.txt")


NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"


Platelet_Results_NASH_Pooled <- Platelet_Results_NASH_Pooled %>% inner_join(NASH_Events) 

Platelet_Results_NASH_Pooled$date <- as.Date(Platelet_Results_NASH_Pooled$date)
Platelet_Results_NASH_Pooled$FirstNASHDx <- as.Date(Platelet_Results_NASH_Pooled$FirstNASHDx)

Platelet_Results_NASH_Pooled$elapsedTime <- round(((Platelet_Results_NASH_Pooled$date - Platelet_Results_NASH_Pooled$FirstNASHDx ) / 30.5))

Platelet_Results_NASH_Pooled$elapsedTime <- as.numeric(Platelet_Results_NASH_Pooled$elapsedTime)


Platelet_Results_NASH_Pooled %>%
  group_by(patid, elapsedTime) %>% filter(result==median(result)) %>%
  slice(1) %>%
  ggplot(aes(elapsedTime, result)) +
  geom_jitter(size=0.3, alpha=0.3)+
  #geom_smooth(method="lm")+
  ylim(0,500)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())






# ---------
# INR -------------


INR_Results_NASH_Pooled <- fread("INR_Results_NASH_Pooled.txt")


NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"


INR_Results_NASH_Pooled <- INR_Results_NASH_Pooled %>% inner_join(NASH_Events) 

INR_Results_NASH_Pooled$date <- as.Date(INR_Results_NASH_Pooled$date)
INR_Results_NASH_Pooled$FirstNASHDx <- as.Date(INR_Results_NASH_Pooled$FirstNASHDx)

INR_Results_NASH_Pooled$elapsedTime <- round(((INR_Results_NASH_Pooled$date - INR_Results_NASH_Pooled$FirstNASHDx ) / 30.5))

INR_Results_NASH_Pooled$elapsedTime <- as.numeric(INR_Results_NASH_Pooled$elapsedTime)


INR_Results_NASH_Pooled %>%
  group_by(patid, elapsedTime) %>% filter(result==max(result)) %>%
  slice(1) %>%
  ggplot(aes(elapsedTime, result)) +
  geom_jitter(size=0.3, alpha=0.3)+
  geom_smooth(method="loess")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# ----------
# Which tests increase around Dx time ? ------------

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"
NASH_Events$FirstNASHDx <- as.Date(NASH_Events$FirstNASHDx)

DANU_Measures <- fread("DANU Measures.txt", integer64 = "character", stringsAsFactors = F)
DANU_Measures <- NASH_Events %>% select(patid) %>% left_join(DANU_Measures)
DANU_Measures <- DANU_Measures %>% select(patid, test, claimed, value)
DANU_Measures$claimed <- as.Date(DANU_Measures$claimed)
DANU_Measures <- DANU_Measures %>% left_join(NASH_Events)

DANU_Measures <- DANU_Measures %>% filter(!is.na(test))

DANU_Measures$elapsedTime <- round(((DANU_Measures$claimed - DANU_Measures$FirstNASHDx ) / 30.5))

DANU_Measures$elapsedTime <- as.numeric(DANU_Measures$elapsedTime)

DANU_Measures %>% group_by(test) %>% count()

# test                   n
# <chr>              <int>
# 1 Albumin Level      42010
# 2 ALT Level          46914
# 3 AST Level          46036
# 4 BMI               122659
# 5 Fasting Glucose     2224
# 6 Fibrosis Activity     35
# 7 Fibrosis Score       159
# 8 Fibrosis Stage       357
# 9 Gamma Globulin       630
# 10 HbA1c Level        27576
# 11 NASH Indicator        60
# 12 NASH Score           120
# 13 Platelet Count     47543


data.frame(DANU_Measures %>% group_by(test, elapsedTime) %>% count() %>% spread(key=test, value=n))


DANU_Measures %>% group_by(test, elapsedTime) %>% count() %>%
  ggplot(aes(elapsedTime, n, fill=test, colour=test)) +
  geom_col(show.legend = F)+
  facet_wrap(~test, scales = "free", ncol = 7) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  ylab("Number of test recoreds per month \n")+xlab("\nElapsed time to/from 1st NASH Dx")


# Repeat for unrelated test: WBC
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% filter(loinc_cd == "6690-2") %>% select(patid, fst_dt)
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% left_join(NASH_Events)  
NASH_Extract_Claims_Lab_Results$fst_dt <- as.Date(NASH_Extract_Claims_Lab_Results$fst_dt)

NASH_Extract_Claims_Lab_Results$elapsedTime <- round(((NASH_Extract_Claims_Lab_Results$fst_dt - NASH_Extract_Claims_Lab_Results$FirstNASHDx ) / 30.5))

NASH_Extract_Claims_Lab_Results$elapsedTime <- as.numeric(NASH_Extract_Claims_Lab_Results$elapsedTime)

data.frame(NASH_Extract_Claims_Lab_Results %>% group_by(elapsedTime) %>% count()) %>%
  ggplot(aes(elapsedTime, n)) +
  geom_col(show.legend = F, colour="darkslategray", fill="darkslategray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of test recoreds per month \n")+xlab("\nElapsed time to/from 1st NASH Dx")


# Repeat for unrelated test: WBC
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% filter(loinc_cd == "6690-2") %>% select(patid, fst_dt)
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% left_join(NASH_Events)  
NASH_Extract_Claims_Lab_Results$fst_dt <- as.Date(NASH_Extract_Claims_Lab_Results$fst_dt)

NASH_Extract_Claims_Lab_Results$elapsedTime <- round(((NASH_Extract_Claims_Lab_Results$fst_dt - NASH_Extract_Claims_Lab_Results$FirstNASHDx ) / 30.5))

NASH_Extract_Claims_Lab_Results$elapsedTime <- as.numeric(NASH_Extract_Claims_Lab_Results$elapsedTime)

data.frame(NASH_Extract_Claims_Lab_Results %>% group_by(elapsedTime) %>% count()) %>%
  ggplot(aes(elapsedTime, n)) +
  geom_col(show.legend = F, colour="darkslategray", fill="darkslategray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of test recoreds per month \n")+xlab("\nElapsed time to/from 1st NASH Dx")



# Repeat for unrelated test: Height Inches
NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% filter(grepl("HEIGHT",tst_desc)) %>% select(patid, fst_dt)
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% left_join(NASH_Events)  
NASH_Extract_Claims_Lab_Results$fst_dt <- as.Date(NASH_Extract_Claims_Lab_Results$fst_dt)

NASH_Extract_Claims_Lab_Results$elapsedTime <- round(((NASH_Extract_Claims_Lab_Results$fst_dt - NASH_Extract_Claims_Lab_Results$FirstNASHDx ) / 30.5))

NASH_Extract_Claims_Lab_Results$elapsedTime <- as.numeric(NASH_Extract_Claims_Lab_Results$elapsedTime)

data.frame(NASH_Extract_Claims_Lab_Results %>% group_by(elapsedTime) %>% count()) %>%
  ggplot(aes(elapsedTime, n)) +
  geom_col(show.legend = F, colour="deeppink4", fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of test records per month \n")+xlab("\nElapsed time to/from 1st NASH Dx")



# Repeat for unrelated test: o2 SATURATION
NASH_Extract_Labs <- fread("NASH Extract Labs.txt")

NASH_Extract_Labs <- NASH_Extract_Labs %>% filter(test_name == "Oxygen saturation (SpO2).pulse oximetry") %>% select(patid, result_date)
NASH_Extract_Labs <- NASH_Extract_Labs %>% left_join(NASH_Events)  
NASH_Extract_Labs$result_date <- as.Date(NASH_Extract_Labs$result_date)

NASH_Extract_Labs$elapsedTime <- round(((NASH_Extract_Labs$result_date - NASH_Extract_Labs$FirstNASHDx ) / 30.5))

NASH_Extract_Labs$elapsedTime <- as.numeric(NASH_Extract_Labs$elapsedTime)

data.frame(NASH_Extract_Labs %>% group_by(elapsedTime) %>% count()) %>%
  ggplot(aes(elapsedTime, n)) +
  geom_col(show.legend = F, colour="deeppink4", fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of test records per month \n")+xlab("\nElapsed time to/from 1st NASH Dx")

# --------
# New Measure All Danuglipron Pats -------------------------------------

NASH_Pats <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Pats <- NASH_Pats %>% select(patient)

DIA_Pats <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Pats <- DIA_Pats %>% select(patient)
DIA_Pats <- DIA_Pats %>% anti_join(NASH_Pats)

OBE_Pats <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Pats <- OBE_Pats %>% select(patient)
OBE_Pats <- OBE_Pats %>% anti_join(NASH_Pats)


DANU_Measures <- fread("DANU Measures.txt", integer64 = "character", stringsAsFactors = F)

NASH_Pats <- NASH_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))
DIA_Pats <- DIA_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))
OBE_Pats <- OBE_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))



# AST NASH vs DIA vs OBE
NASH_Pats_AST <- NASH_Pats %>% filter(test == "AST Level")
DIA_Pats_AST <- DIA_Pats %>% filter(test == "AST Level")
oBE_Pats_AST <- OBE_Pats %>% filter(test == "AST Level")

NASH_Pats_AST %>% filter(value <= 200 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=336, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH AST Level")

DIA_Pats_AST %>% filter(value <= 200 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=756, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nDIA AST Level")

oBE_Pats_AST %>% filter(value <= 200 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=1491, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nOBE AST Level")



# ALT NASH vs DIA vs OBE
NASH_Pats_ALT <- NASH_Pats %>% filter(test == "ALT Level")
DIA_Pats_ALT <- DIA_Pats %>% filter(test == "ALT Level")
oBE_Pats_ALT <- OBE_Pats %>% filter(test == "ALT Level")


NASH_Pats_ALT %>% filter(value <= 200 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=276, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH ALT Level")

DIA_Pats_ALT %>% filter(value <= 200 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=615, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nDIA ALT Level")

oBE_Pats_ALT %>% filter(value <= 200 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=615, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nOBE ALT Level")



# Platelets NASH vs DIA vs OBE
NASH_Pats_Platelets <- NASH_Pats %>% filter(test == "Platelet Count")
DIA_Pats_Platelets <- DIA_Pats %>% filter(test == "Platelet Count")
oBE_Pats_Platelets <- OBE_Pats %>% filter(test == "Platelet Count")


NASH_Pats_Platelets %>% filter(value <= 500 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=748, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Platelets Level")

DIA_Pats_Platelets %>% filter(value <= 500 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=1988, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nDIA Platelets Level")

oBE_Pats_Platelets %>% filter(value <= 500 & value >0) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=2097, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nOBE Platelets Level")






# Gamma Globulin NASH vs DIA vs OBE
NASH_Pats_GammaGlobulin <- NASH_Pats %>% filter(test == "Gamma Globulin")
DIA_Pats_GammaGlobulin <- DIA_Pats %>% filter(test == "Gamma Globulin")
oBE_Pats_GammaGlobulin <- OBE_Pats %>% filter(test == "Gamma Globulin")


NASH_Pats_GammaGlobulin %>%  
  ggplot(aes(value)) +
  xlim(0,10)+
  geom_histogram(bins=331, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Gamma Globulin Level")

DIA_Pats_GammaGlobulin %>% 
  ggplot(aes(value)) +
  xlim(0,10)+
  geom_histogram(bins=331, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nDIA Gamma Globulin Level")

oBE_Pats_GammaGlobulin %>% 
  ggplot(aes(value)) +
  xlim(0,10)+
  geom_histogram(bins=331, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nOBE Gamma Globulin Level")




# Albumin Level NASH vs DIA vs OBE
NASH_Pats_Albumin <- NASH_Pats %>% filter(test == "Albumin Level")
DIA_Pats_Albumin <- DIA_Pats %>% filter(test == "Albumin Level")
oBE_Pats_Albumin <- OBE_Pats %>% filter(test == "Albumin Level")

mean(NASH_Pats_Albumin$value) #3.960414
median(NASH_Pats_Albumin$value) #4.1

mean(DIA_Pats_Albumin$value) #4.026112
median(DIA_Pats_Albumin$value) #4.1

mean(oBE_Pats_Albumin$value) #4.139651
median(oBE_Pats_Albumin$value) #4.2


NASH_Pats_Albumin %>%  
  ggplot(aes(value)) +
  xlim(1,6)+
  geom_histogram(bins=203, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Albumin Level")

DIA_Pats_Albumin %>% 
  ggplot(aes(value)) +
  xlim(1,6)+
  geom_histogram(bins=470, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nDIA Albumin Level")

oBE_Pats_Albumin %>% 
  ggplot(aes(value)) +
  xlim(1,6)+
  geom_histogram(bins=498, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nOBE Albumin Level")



# BMI Level NASH vs DIA vs OBE
NASH_Pats_BMI <- NASH_Pats %>% filter(test == "BMI")
DIA_Pats_BMI <- DIA_Pats %>% filter(test == "BMI")
oBE_Pats_BMI <- OBE_Pats %>% filter(test == "BMI")


NASH_Pats_BMI %>%  
  ggplot(aes(value)) +
  xlim(10,50)+
  geom_histogram( colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH BMI Level")

DIA_Pats_BMI %>% 
  ggplot(aes(value)) +
  xlim(10,50)+
  geom_histogram(colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nDIA BMI Level")

oBE_Pats_BMI %>% 
  ggplot(aes(value)) +
  xlim(10,50)+
  geom_histogram( colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nOBE BMI Level")





# Split NASH Pats into type of NASH 

NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))
NASH_diagnosis  %>% summarise(n=sum(weight)) #1339983

NASH_Pats_Cirrhosis <- NASH_diagnosis  %>% filter(NASH_diganosis == "NASH-Cirrohsis") %>% select(patient) %>% left_join(NASH_Pats)
NASH_Pats_Fibrosis <- NASH_diagnosis  %>% filter(NASH_diganosis == "NASH-Fibrosis") %>% select(patient) %>% left_join(NASH_Pats)
NASH_Pats_NASHOnly <- NASH_diagnosis  %>% filter(NASH_diganosis == "NASH-Only") %>% select(patient) %>% left_join(NASH_Pats)



#  Fibrosis Score NASH only vs NASH Fibrosis vs NASH Cirrhosis
NASH_Pats_Cirrhosis_FibrosisScore <- NASH_Pats_Cirrhosis %>% filter(test == "Fibrosis Score")
NASH_Pats_Fibrosis_FibrosisScore <- NASH_Pats_Fibrosis %>% filter(test == "Fibrosis Score")
NASH_Pats_NASHOnly_FibrosisScore <- NASH_Pats_NASHOnly %>% filter(test == "Fibrosis Score")

NASH_Pats_Cirrhosis_FibrosisScore %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=37, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Cirrhosis Score")

NASH_Pats_Fibrosis_FibrosisScore %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=17, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Fibrosis  Score")

NASH_Pats_NASHOnly_FibrosisScore %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=42, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH only Fibrosis Score")




#  Fibrosis Stage NASH only vs NASH Fibrosis vs NASH Cirrhosis
NASH_Pats_Cirrhosis_FibrosisStage <- NASH_Pats_Cirrhosis %>% filter(test == "Fibrosis Stage")
NASH_Pats_Fibrosis_FibrosisStage <- NASH_Pats_Fibrosis %>% filter(test == "Fibrosis Stage")
NASH_Pats_NASHOnly_FibrosisStage <- NASH_Pats_NASHOnly %>% filter(test == "Fibrosis Stage")

NASH_Pats_Cirrhosis_FibrosisStage %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=8, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Cirrhosis, Fibrosis Stage")

NASH_Pats_Fibrosis_FibrosisStage %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=9, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Fibrosis, Fibrosis Stage")

NASH_Pats_NASHOnly_FibrosisStage %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=8, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH only, Fibrosis Stage")





#  Fibrosis Activity NASH only vs NASH Fibrosis vs NASH Cirrhosis
NASH_Pats_Cirrhosis_FibrosisActivity <- NASH_Pats_Cirrhosis %>% filter(test == "Fibrosis Activity")
NASH_Pats_Fibrosis_FibrosisActivity <- NASH_Pats_Fibrosis %>% filter(test == "Fibrosis Activity")
NASH_Pats_NASHOnly_FibrosisActivity <- NASH_Pats_NASHOnly %>% filter(test == "Fibrosis Activity")

NASH_Pats_Cirrhosis_FibrosisActivity %>% 
  ggplot(aes(value)) +
  xlim(0,10)+ylim(0,11)+
  geom_histogram(bins=10, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Cirrhosis, Fibrosis Activity")

NASH_Pats_Fibrosis_FibrosisActivity %>% 
  ggplot(aes(value)) +
  xlim(0,10)+ylim(0,11)+
  geom_histogram(bins=10, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH Fibrosis,Fibrosis Activity")

NASH_Pats_NASHOnly_FibrosisActivity %>% 
  ggplot(aes(value)) +
  xlim(0,10)+ylim(0,11)+
  geom_histogram(bins=10, colour="deepskyblue4", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\nNASH only, Fibrosis Activity")




#  NASH Indicator NASH only vs NASH Fibrosis vs NASH Cirrhosis
# NASH_Pats_Cirrhosis_NASHIndicator <- NASH_Pats_Cirrhosis %>% filter(test == "NASH Indicator")
# NASH_Pats_Fibrosis_NASHIndicator <- NASH_Pats_Fibrosis %>% filter(test == "NASH Indicator")
# NASH_Pats_NASHOnly_NASHIndicator <- NASH_Pats_NASHOnly %>% filter(test == "NASH Indicator")
# 
# NASH_Pats_Cirrhosis_NASHIndicator %>% 
#   ggplot(aes(value)) +
#   xlim(0,1)+ylim(0,11)+
#   geom_histogram(bins=10, colour="deepskyblue4", fill="deepskyblue4")+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   ylab("Number of Patients \n")+xlab("\nNASH Cirrhosis, NASH Indicator")
# 
# NASH_Pats_Fibrosis_NASHIndicator %>% 
#   ggplot(aes(value)) +
#   xlim(0,1)+ylim(0,11)+
#   geom_histogram(bins=10, colour="deepskyblue4", fill="deepskyblue4")+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   ylab("Number of Patients \n")+xlab("\nNASH Fibrosis, NASH Indicator")
# 
# NASH_Pats_NASHOnly_NASHIndicator %>% 
#   ggplot(aes(value)) +
#   #xlim(0,1)+ylim(0,11)+
#   geom_histogram(bins=10, colour="deepskyblue4", fill="deepskyblue4")+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   ylab("Number of Patients \n")+xlab("\nNASH only, NASH Indicator")


# ------
# Waterfall -----

# Biopsy pats
NASH_Events <- fread("NASH Events.txt")
Dx_code <- fread("NASH Diagnosis Codes.txt")
Dx_code <- Dx_code %>% select(code, condition, source, type, description)
NASH_Events <- NASH_Events %>% left_join(Dx_code)
NASH_Pats <- NASH_Events %>% filter(condition=="NASH") %>% select(patid) %>% distinct()
NASH_Pats <- NASH_Pats %>% left_join(NASH_Events)
NASH_Pats %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #1339983
Biopsy_Pats <- NASH_Pats %>% select(patid, weight, condition) %>% distinct() %>% filter(condition=="Liver Biopsy") %>% select(patid) %>% distinct()


# MELD Scores
MELD_ExactDate<- fread("MELD_ExactDate.txt", sep="\t")
MELD_ExactDate <- MELD_ExactDate %>% select(patid, weight, date, MELDNa, FirstNASHDx)
MELD_ExactDate$Test <- "MELDNa"
names(MELD_ExactDate)[4] <- "Score"

MELD_Thresold <- MELD_ExactDate %>% filter((Test=="MELDNa"&Score>=7)) %>% select(patid) %>% distinct() #1179


#FIB4
FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")

# NASH Patients to filter new tests 
NASH_Pats <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Pats <- NASH_Pats %>% select(patient) %>% distinct()

# NEw lab test (check for AST/ALT >50 or Platelets <100)
DANU_Measures <- fread("DANU Measures.txt", integer64 = "character", stringsAsFactors = F)

NASH_Pats <- NASH_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))

NASH_Pats %>% filter( (test=="AST Level" & value>50) | (test=="ALT Level" & value>50) | (test=="Platelet Count" & value<100) ) %>%
  select(patient) %>% distinct() %>% full_join(MELD_Thresold, by=c("patient"="patid")) %>% full_join(Biopsy_Pats,  by=c("patient"="patid")) %>% 
  full_join(FIB4_NASH_Pats%>% filter(fibrosis4>1.3)) %>% select(patient) %>% distinct() # 4789




MELD_ExactDate %>%  select(patid) %>% distinct() #1564
NASH_Pats %>% filter(test=="AST Level") %>% select(patient) %>% distinct() #4966
NASH_Pats %>% filter(test=="ALT Level") %>% select(patient) %>% distinct() #4976
NASH_Pats %>% filter(test=="Platelet Count") %>% select(patient) %>% distinct() #5006

MELD_ExactDate %>%  select(patid) %>% distinct() %>% full_join(NASH_Pats %>% filter(test=="AST Level") %>% select(patient) %>% distinct(), by=c("patid"="patient")) %>%
  full_join(NASH_Pats %>% filter(test=="ALT Level") %>% select(patient) %>% distinct(), by=c("patid"="patient")) %>% 
  full_join(NASH_Pats %>% filter(test=="Platelet Count") %>% select(patient) %>% distinct(), by=c("patid"="patient")) %>%
  full_join(Biopsy_Pats) %>% full_join(FIB4_NASH_Pats %>% select(patient), by=c("patid"="patient")) %>% select(patid)  %>% distinct()

# 5959

# ------
# Calculate FIB4 for the other populations -------------------------
NASH_Pats <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Pats <- NASH_Pats %>% select(patient)

NAFLD_Pats <- fread("NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Pats <- NAFLD_Pats %>% select(patient)
NAFLD_Pats <- NAFLD_Pats %>% anti_join(NASH_Pats)


DIA_Pats <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Pats <- DIA_Pats %>% select(patient)
DIA_Pats <- DIA_Pats %>% anti_join(NASH_Pats)

OBE_Pats <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Pats <- OBE_Pats %>% select(patient)
OBE_Pats <- OBE_Pats %>% anti_join(NASH_Pats)


DANU_Measures <- fread("DANU Measures.txt", integer64 = "character", stringsAsFactors = F)

NASH_Pats <- NASH_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))
DIA_Pats <- DIA_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))
OBE_Pats <- OBE_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))
NAFLD_Pats <- NAFLD_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))


NASH_Pats <- NASH_Pats %>% filter(test=="AST Level"|test=="ALT Level"|test=="Platelet Count")
DIA_Pats <- DIA_Pats %>% filter(test=="AST Level"|test=="ALT Level"|test=="Platelet Count")
OBE_Pats <- OBE_Pats %>% filter(test=="AST Level"|test=="ALT Level"|test=="Platelet Count")
NAFLD_Pats <- NAFLD_Pats %>% filter(test=="AST Level"|test=="ALT Level"|test=="Platelet Count")

NASH_Pats <- NASH_Pats %>% select(patient, test, claimed, value)
DIA_Pats <- DIA_Pats %>% select(patient, test, claimed, value)
OBE_Pats <- OBE_Pats %>% select(patient, test, claimed, value)
NAFLD_Pats <- NAFLD_Pats %>% select(patient, test, claimed, value)





# NASH only
NASH_Pats_AST <- NASH_Pats %>% filter(test=="AST Level")
NASH_Pats_ALT <- NASH_Pats %>% filter(test=="ALT Level")
NASH_Pats_Platelets <- NASH_Pats %>% filter(test=="Platelet Count")

names(NASH_Pats_AST)[4] <-"AST"
names(NASH_Pats_ALT)[4] <-"ALT"
names(NASH_Pats_Platelets)[4] <-"Platelets"

NASH_Pats_AST <- NASH_Pats_AST %>% select(1,3,4)
NASH_Pats_ALT <- NASH_Pats_ALT %>% select(1,3,4)
NASH_Pats_Platelets <- NASH_Pats_Platelets %>% select(1,3,4)

NASH_Pats <- NASH_Pats_AST %>% full_join(NASH_Pats_ALT, by = c("patient", "claimed")) %>% full_join(NASH_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()


DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

NASH_Pats <- NASH_Pats %>% left_join(DANU_Demographics, by=c("patient"="patid"))
NASH_Pats$claimed <- as.Date(NASH_Pats$claimed)

NASH_Pats$finalDate <- as.Date("2021-04-30")

NASH_Pats$diff <- round(((NASH_Pats$finalDate - NASH_Pats$claimed)/30.5)/12)

NASH_Pats$age <- NASH_Pats$age - NASH_Pats$diff 

NASH_Pats <- NASH_Pats %>% select(1,2,3,4,5,6)
NASH_Pats$age <- as.numeric(NASH_Pats$age)

NASH_Pats$fibrosis4 <- (NASH_Pats$age*NASH_Pats$AST) / (NASH_Pats$Platelets*sqrt(NASH_Pats$ALT))


NASH_Pats <- NASH_Pats %>% filter(Platelets>10)
NASH_Pats <- NASH_Pats %>% filter(AST>5)
NASH_Pats <- NASH_Pats %>% filter(ALT>5)

fwrite(NASH_Pats, "FIB4_NASH_Pats.txt")

NASH_Pats <- fread("FIB4_NASH_Pats.txt")

range(NASH_Pats$fibrosis4) # 0.01773381 1844.73340950

median(NASH_Pats$fibrosis4)  # 2.018538
mean(NASH_Pats$fibrosis4)  # 7.459196
length(unique(NASH_Pats$patient)) # 4541
quantile(NASH_Pats$fibrosis4, c(.25, .50, .75)) 

# 25%      50%      75% 
# 1.047984 2.018538 4.306434 


NASH_Pats %>% select(fibrosis4) %>% filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



NASH_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #5.74

NASH_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.66

temp2 <- NASH_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%      50%      75% 
# 1.025355 1.660889 3.198794 

temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")







# DIA only
DIA_Pats_AST <- DIA_Pats %>% filter(test=="AST Level")
DIA_Pats_ALT <- DIA_Pats %>% filter(test=="ALT Level")
DIA_Pats_Platelets <- DIA_Pats %>% filter(test=="Platelet Count")

names(DIA_Pats_AST)[4] <-"AST"
names(DIA_Pats_ALT)[4] <-"ALT"
names(DIA_Pats_Platelets)[4] <-"Platelets"

DIA_Pats_AST <- DIA_Pats_AST %>% select(1,3,4)
DIA_Pats_ALT <- DIA_Pats_ALT %>% select(1,3,4)
DIA_Pats_Platelets <- DIA_Pats_Platelets %>% select(1,3,4)

DIA_Pats <- DIA_Pats_AST %>% full_join(DIA_Pats_ALT, by = c("patient", "claimed")) %>% full_join(DIA_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

DIA_Pats <- DIA_Pats %>% left_join(DANU_Demographics, by=c("patient"="patid"))
DIA_Pats$claimed <- as.Date(DIA_Pats$claimed)

DIA_Pats$finalDate <- as.Date("2021-04-30")

DIA_Pats$diff <- round(((DIA_Pats$finalDate - DIA_Pats$claimed)/30.5)/12)

DIA_Pats$age <- DIA_Pats$age - DIA_Pats$diff 

DIA_Pats <- DIA_Pats %>% select(1,2,3,4,5,6)
DIA_Pats$age <- as.numeric(DIA_Pats$age)

DIA_Pats$fibrosis4 <- (DIA_Pats$age*DIA_Pats$AST) / (DIA_Pats$Platelets*sqrt(DIA_Pats$ALT))


DIA_Pats <- DIA_Pats %>% filter(Platelets>10)
DIA_Pats <- DIA_Pats %>% filter(AST>5)
DIA_Pats <- DIA_Pats %>% filter(ALT>5)

fwrite(DIA_Pats, "FIB4_Diabetes_Pats.txt")

DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")

range(DIA_Pats$fibrosis4) #0.008262961 5825.695284564

median(DIA_Pats$fibrosis4)  # 1.307857
mean(DIA_Pats$fibrosis4)  # 2.997771
length(unique(DIA_Pats$patient)) # 145796
quantile(DIA_Pats$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.8522559 1.3078574 2.1058930 


DIA_Pats %>% select(fibrosis4) %>% filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



DIA_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #2.95

DIA_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.34

temp2 <- DIA_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.8907215 1.3379286 2.0663735

temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")






# OBE only
OBE_Pats_AST <- OBE_Pats %>% filter(test=="AST Level")
OBE_Pats_ALT <- OBE_Pats %>% filter(test=="ALT Level")
OBE_Pats_Platelets <- OBE_Pats %>% filter(test=="Platelet Count")

names(OBE_Pats_AST)[4] <-"AST"
names(OBE_Pats_ALT)[4] <-"ALT"
names(OBE_Pats_Platelets)[4] <-"Platelets"

OBE_Pats_AST <- OBE_Pats_AST %>% select(1,3,4)
OBE_Pats_ALT <- OBE_Pats_ALT %>% select(1,3,4)
OBE_Pats_Platelets <- OBE_Pats_Platelets %>% select(1,3,4)

OBE_Pats <- OBE_Pats_AST %>% full_join(OBE_Pats_ALT, by = c("patient", "claimed")) %>% full_join(OBE_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

OBE_Pats <- OBE_Pats %>% left_join(DANU_Demographics, by=c("patient"="patid"))
OBE_Pats$claimed <- as.Date(OBE_Pats$claimed)

OBE_Pats$finalDate <- as.Date("2021-04-30")

OBE_Pats$diff <- round(((OBE_Pats$finalDate - OBE_Pats$claimed)/30.5)/12)

OBE_Pats$age <- OBE_Pats$age - OBE_Pats$diff 

OBE_Pats <- OBE_Pats %>% select(1,2,3,4,5,6)
OBE_Pats$age <- as.numeric(OBE_Pats$age)

OBE_Pats$fibrosis4 <- (OBE_Pats$age*OBE_Pats$AST) / (OBE_Pats$Platelets*sqrt(OBE_Pats$ALT))


OBE_Pats <- OBE_Pats %>% filter(Platelets>10)
OBE_Pats <- OBE_Pats %>% filter(AST>5)
OBE_Pats <- OBE_Pats %>% filter(ALT>5)

fwrite(OBE_Pats, "FIB4_Obesity_Pats.txt")

OBE_Pats <- fread("FIB4_Obesity_Pats.txt")

range(OBE_Pats$fibrosis4) # 0.006037801 1586.266847911

median(OBE_Pats$fibrosis4)  # 1.086311
mean(OBE_Pats$fibrosis4)  # 2.405577
length(unique(OBE_Pats$patient)) # 307381
quantile(OBE_Pats$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.6952373 1.0863112 1.7550412 


OBE_Pats %>% select(fibrosis4) %>% filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



OBE_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #2.04

OBE_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.03

temp2 <- OBE_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.6813441 1.0287167 1.5584716

temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")










# NAFLD only
NAFLD_Pats_AST <- NAFLD_Pats %>% filter(test=="AST Level")
NAFLD_Pats_ALT <- NAFLD_Pats %>% filter(test=="ALT Level")
NAFLD_Pats_Platelets <- NAFLD_Pats %>% filter(test=="Platelet Count")

names(NAFLD_Pats_AST)[4] <-"AST"
names(NAFLD_Pats_ALT)[4] <-"ALT"
names(NAFLD_Pats_Platelets)[4] <-"Platelets"

NAFLD_Pats_AST <- NAFLD_Pats_AST %>% select(1,3,4)
NAFLD_Pats_ALT <- NAFLD_Pats_ALT %>% select(1,3,4)
NAFLD_Pats_Platelets <- NAFLD_Pats_Platelets %>% select(1,3,4)

NAFLD_Pats <- NAFLD_Pats_AST %>% full_join(NAFLD_Pats_ALT, by = c("patient", "claimed")) %>% full_join(NAFLD_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

NAFLD_Pats <- NAFLD_Pats %>% left_join(DANU_Demographics, by=c("patient"="patid"))
NAFLD_Pats$claimed <- as.Date(NAFLD_Pats$claimed)

NAFLD_Pats$finalDate <- as.Date("2021-04-30")

NAFLD_Pats$diff <- round(((NAFLD_Pats$finalDate - NAFLD_Pats$claimed)/30.5)/12)

NAFLD_Pats$age <- NAFLD_Pats$age - NAFLD_Pats$diff 

NAFLD_Pats <- NAFLD_Pats %>% select(1,2,3,4,5,6)
NAFLD_Pats$age <- as.numeric(NAFLD_Pats$age)

NAFLD_Pats$fibrosis4 <- (NAFLD_Pats$age*NAFLD_Pats$AST) / (NAFLD_Pats$Platelets*sqrt(NAFLD_Pats$ALT))


NAFLD_Pats <- NAFLD_Pats %>% filter(Platelets>10)
NAFLD_Pats <- NAFLD_Pats %>% filter(AST>5)
NAFLD_Pats <- NAFLD_Pats %>% filter(ALT>5)

fwrite(NAFLD_Pats, "FIB4_NAFLD_Pats.txt")

NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")

range(NAFLD_Pats$fibrosis4) # 0.006457958 1586.266847911

median(NAFLD_Pats$fibrosis4)  # 1.197922
mean(NAFLD_Pats$fibrosis4)  # 3.238828
length(unique(NAFLD_Pats$patient)) # 46622
quantile(NAFLD_Pats$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.7660323 1.1979222 2.0473327


NAFLD_Pats %>% select(fibrosis4) %>% filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



NAFLD_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #3.44

NAFLD_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.27

temp2 <- NAFLD_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.8427931 1.2656335 2.0431454  

temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")






# --------
# AGE distribution NASH, Diabetes, Obesity --------------------------------------------------------
NASH_Pats <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Pats <- NASH_Pats %>% select(patient)

DIA_Pats <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Pats <- DIA_Pats %>% select(patient)

OBE_Pats <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Pats <- OBE_Pats %>% select(patient)


DANU_Demographics <- fread("DANU Demographics.txt", sep="\t")


# % above 65
NASH_Pats %>% summarise(n=sum(weight)) #1339983
DIA_Pats  %>% summarise(n=sum(weight)) #48244424
OBE_Pats  %>% summarise(n=sum(weight)) #106469049

NASH_Pats %>% filter(age>=65) %>% summarise(n=sum(weight)) #457898.7
DIA_Pats  %>% filter(age>=65) %>%summarise(n=sum(weight)) #22631982
OBE_Pats  %>% filter(age>=65) %>%summarise(n=sum(weight)) #23501177


NASH_Pats <- NASH_Pats %>% left_join(DANU_Demographics %>% select(patid, weight, age), 
                                     by=c("patient"="patid"))


DIA_Pats <- DIA_Pats %>% left_join(DANU_Demographics %>% select(patid, weight, age), 
                                   by=c("patient"="patid"))


OBE_Pats <- OBE_Pats %>% left_join(DANU_Demographics %>% select(patid, weight, age), 
                                   by=c("patient"="patid"))


weighted.mean(NASH_Pats$age, NASH_Pats$weight) #56.899
weighted.median(NASH_Pats$age, NASH_Pats$weight) #58.5

NASH_Pats %>% select(age) %>%
  ggplot(aes(age)) +
  geom_histogram(bins=72, colour="gray", fill="deepskyblue4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Age")+ ylab("Number of Patients \n")


weighted.mean(DIA_Pats$age, DIA_Pats$weight) #60.96606
weighted.median(DIA_Pats$age, DIA_Pats$weight) #62.5

DIA_Pats %>% select(age) %>%
  ggplot(aes(age)) +
  geom_histogram(bins=72, colour="gray", fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Age")+ ylab("Number of Patients \n")



weighted.mean(OBE_Pats$age, OBE_Pats$weight) #49.09858
weighted.median(OBE_Pats$age, OBE_Pats$weight) #47.5

OBE_Pats %>% select(age) %>%
  ggplot(aes(age)) +
  geom_histogram(bins=72, colour="gray", fill="mediumaquamarine")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Age")+ ylab("Number of Patients \n")


# ---------
# Logistic regression to classify patients as having NASH vs not having NASH -------------

NASH_Pats <- fread("FIB4_NASH_Pats.txt")
Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")


NASH_Pats$Has_Nash <- 1
Diabetes_Pats$Has_Nash <- 0
Obesity_Pats$Has_Nash <- 0

NASH_Pats$Condition <- "NASH"
Diabetes_Pats$Condition <- "DIA"
Obesity_Pats$Condition <- "OBE"

NAsh_Indicator <- NASH_Pats %>% bind_rows(Diabetes_Pats) %>% bind_rows(Obesity_Pats)

summary(glm( Has_Nash ~ fibrosis4, data = NAsh_Indicator, family = binomial))

# FIB4 is significantly associated with NASH, but this is highly biased by extremelly high values.

# We should come up with a thresold above which we remove FIB4 values (1000 ?)

# Call:
#   glm(formula = Has_Nash ~ fibrosis4, family = binomial, data = NAsh_Indicator)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -8.4904  -0.2250  -0.2245  -0.2242   2.7206  
# 
# Coefficients:
#   Estimate Std. Error z value            Pr(>|z|)    
# (Intercept) -3.675953   0.003871 -949.59 <0.0000000000000002 ***
#   fibrosis4    0.006277   0.000136   46.17 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 656307  on 2783706  degrees of freedom
# Residual deviance: 654099  on 2783705  degrees of freedom
# AIC: 654103
# 
# Number of Fisher Scoring iterations: 6



NAsh_Indicator %>% 
  ggplot(aes(fibrosis4, Has_Nash)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")



NAsh_Indicator_1000 <- NAsh_Indicator[fibrosis4<=1000]


# Only FIB4 below 1000

summary(glm( Has_Nash ~ fibrosis4, data = NAsh_Indicator_1000, family = binomial))

# Still significant
# And the regression still looks ok
# Our antecipated thresolds won't work in here though (1.3 or 2.67 are out of the quetsion)

# Call:
#   glm(formula = Has_Nash ~ fibrosis4, family = binomial, data = NAsh_Indicator_1000)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.0877  -0.2244  -0.2238  -0.2234   2.7237  
# 
# Coefficients:
#   Estimate Std. Error z value            Pr(>|z|)    
# (Intercept) -3.6847261  0.0038863 -948.12 <0.0000000000000002 ***
#   fibrosis4    0.0084657  0.0001467   57.69 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 656296  on 2783635  degrees of freedom
# Residual deviance: 652973  on 2783634  degrees of freedom
# AIC: 652977
# 
# Number of Fisher Scoring iterations: 6


NAsh_Indicator_1000 %>% 
  ggplot(aes(fibrosis4, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")


# What if we filter everything below 1000?


NAsh_Indicator_All1000 <- NAsh_Indicator[fibrosis4<=1000 & AST<=1000 & ALT <=1000]


# All below 1000

summary(glm( Has_Nash ~ fibrosis4, data = NAsh_Indicator_All1000, family = binomial))

# Still significant, but we still need massive increases in FIB4 to have a considerable chance of having NASH
# Above 200?

# Call:
#   glm(formula = Has_Nash ~ fibrosis4, family = binomial, data = NAsh_Indicator_All1000)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -4.5982  -0.2183  -0.2170  -0.2162   2.7497  
# 
# Coefficients:
#   Estimate Std. Error z value            Pr(>|z|)    
# (Intercept) -3.757568   0.004096 -917.28 <0.0000000000000002 ***
#   fibrosis4    0.017489   0.000316   55.34 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 622771  on 2755090  degrees of freedom
# Residual deviance: 620377  on 2755089  degrees of freedom
# AIC: 620381
# 
# Number of Fisher Scoring iterations: 6


NAsh_Indicator_All1000 %>% 
  ggplot(aes(fibrosis4, Has_Nash)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")



# All below 200

NAsh_Indicator_All200 <- NAsh_Indicator[fibrosis4<=200 & AST<=200 & ALT <=200]

summary(glm( Has_Nash ~ fibrosis4, data = NAsh_Indicator_All200, family = binomial))

# Still significant
# But now someone with FIB4 of 200 maybe (...) 60% ???

# Call:
#   glm(formula = Has_Nash ~ fibrosis4, family = binomial, data = NAsh_Indicator_All200)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5185  -0.2121  -0.2105  -0.2095   2.7734  
# 
# Coefficients:
#   Estimate Std. Error z value            Pr(>|z|)    
# (Intercept) -3.8249269  0.0043143 -886.57 <0.0000000000000002 ***
#   fibrosis4    0.0230481  0.0003995   57.69 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 571539  on 2650196  degrees of freedom
# Residual deviance: 569296  on 2650195  degrees of freedom
# AIC: 569300
# 
# Number of Fisher Scoring iterations: 6


NAsh_Indicator_All200 %>% 
  ggplot(aes(fibrosis4, Has_Nash)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")




# What about using the liver enzymes instead?
# All significant
# Higher AST or ALT -> higher chance of NASH
# Lower platelets or age -> higher chance of NASH 

# BUT !
# Not so great, we would need massive increases in AST or ALT
# Maybe we should try within the NASH and than extrapolate to the other conditions

summary(glm( Has_Nash ~ AST+ALT+Platelets+age, data = NAsh_Indicator_All1000, family = binomial))


# Call:
#   glm(formula = Has_Nash ~ AST + ALT + Platelets + age, family = binomial, 
#       data = NAsh_Indicator_All1000)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.2911  -0.2277  -0.2050  -0.1831   6.7415  
# 
# Coefficients:
#   Estimate  Std. Error z value            Pr(>|z|)    
# (Intercept) -2.73609640  0.01590994 -171.97 <0.0000000000000002 ***
#   AST          0.00119672  0.00004091   29.26 <0.0000000000000002 ***
#   ALT          0.00224658  0.00003876   57.97 <0.0000000000000002 ***
#   Platelets   -0.00398911  0.00004342  -91.87 <0.0000000000000002 ***
#   age         -0.00455249  0.00022195  -20.51 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 622771  on 2755090  degrees of freedom
# Residual deviance: 602863  on 2755086  degrees of freedom
# AIC: 602873
# 
# Number of Fisher Scoring iterations: 7

# Note tha age is probably very biased as we maybe have many more patients (Diabetes?) "without" NASH in older age groups !


NAsh_Indicator_All1000 %>% 
  ggplot(aes(AST, Has_Nash)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n AST")+
  ylab("Probability of NASH Dx \n")


NAsh_Indicator_All1000 %>% 
  ggplot(aes(ALT, Has_Nash)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n ALT")+
  ylab("Probability of NASH Dx \n")


NAsh_Indicator_All1000 %>% 
  ggplot(aes(Platelets, Has_Nash)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Platelet count")+
  ylab("Probability of NASH Dx \n")


NAsh_Indicator_All1000 %>% 
  ggplot(aes(age, Has_Nash)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Age (years)")+
  ylab("Probability of NASH Dx \n")




# -------
# Counting the patients with comorbidity ON both Dx (e.g. NASH & DIA) -----------

NASH_Pats <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Pats <- NASH_Pats %>% select(patient)

DIA_Pats <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Pats <- DIA_Pats %>% select(patient)

OBE_Pats <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Pats <- OBE_Pats %>% select(patient)

DANU_Measures <- fread("DANU Measures.txt", integer64 = "character", stringsAsFactors = F)

NASH_Pats <- NASH_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))
DIA_Pats <- DIA_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))
OBE_Pats <- OBE_Pats %>% left_join(DANU_Measures, by = c("patient"="patid"))

NASH_Pats <- NASH_Pats %>% filter(test=="AST Level"|test=="ALT Level"|test=="Platelet Count")
DIA_Pats <- DIA_Pats %>% filter(test=="AST Level"|test=="ALT Level"|test=="Platelet Count")
OBE_Pats <- OBE_Pats %>% filter(test=="AST Level"|test=="ALT Level"|test=="Platelet Count")

NASH_Pats <- NASH_Pats %>% select(patient, test, claimed, value)
DIA_Pats <- DIA_Pats %>% select(patient, test, claimed, value)
OBE_Pats <- OBE_Pats %>% select(patient, test, claimed, value)





# NASH only
NASH_Pats_AST <- NASH_Pats %>% filter(test=="AST Level")
NASH_Pats_ALT <- NASH_Pats %>% filter(test=="ALT Level")
NASH_Pats_Platelets <- NASH_Pats %>% filter(test=="Platelet Count")

names(NASH_Pats_AST)[4] <-"AST"
names(NASH_Pats_ALT)[4] <-"ALT"
names(NASH_Pats_Platelets)[4] <-"Platelets"

NASH_Pats_AST <- NASH_Pats_AST %>% select(1,3,4)
NASH_Pats_ALT <- NASH_Pats_ALT %>% select(1,3,4)
NASH_Pats_Platelets <- NASH_Pats_Platelets %>% select(1,3,4)

NASH_Pats <- NASH_Pats_AST %>% full_join(NASH_Pats_ALT, by = c("patient", "claimed")) %>% full_join(NASH_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

NASH_Pats <- NASH_Pats %>% left_join(DANU_Demographics, by=c("patient"="patid"))
NASH_Pats$claimed <- as.Date(NASH_Pats$claimed)

NASH_Pats$finalDate <- as.Date("2021-04-30")

NASH_Pats$diff <- round(((NASH_Pats$finalDate - NASH_Pats$claimed)/30.5)/12)

NASH_Pats$age <- NASH_Pats$age - NASH_Pats$diff 

NASH_Pats <- NASH_Pats %>% select(1,2,3,4,5,6)
NASH_Pats$age <- as.numeric(NASH_Pats$age)

NASH_Pats$fibrosis4 <- (NASH_Pats$age*NASH_Pats$AST) / (NASH_Pats$Platelets*sqrt(NASH_Pats$ALT))


NASH_Pats <- NASH_Pats %>% filter(Platelets>10)
NASH_Pats <- NASH_Pats %>% filter(AST>5)
NASH_Pats <- NASH_Pats %>% filter(ALT>5)

fwrite(NASH_Pats, "FIB4_NASH_Pats_ALL.txt")


range(NASH_Pats$fibrosis4) # 0.01773381 1844.73340950

median(NASH_Pats$fibrosis4)  # 2.018538
mean(NASH_Pats$fibrosis4)  # 7.459196
length(unique(NASH_Pats$patient)) # 4541
quantile(NASH_Pats$fibrosis4, c(.25, .50, .75)) 

# 25%      50%      75% 
# 1.047984 2.018538 4.306434 


NASH_Pats %>% select(fibrosis4) %>% filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=1000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



NASH_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #5.74

NASH_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.66

temp2 <- NASH_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%      50%      75% 
# 1.025355 1.660889 3.198794 

temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")







# DIA only
DIA_Pats_AST <- DIA_Pats %>% filter(test=="AST Level")
DIA_Pats_ALT <- DIA_Pats %>% filter(test=="ALT Level")
DIA_Pats_Platelets <- DIA_Pats %>% filter(test=="Platelet Count")

names(DIA_Pats_AST)[4] <-"AST"
names(DIA_Pats_ALT)[4] <-"ALT"
names(DIA_Pats_Platelets)[4] <-"Platelets"

DIA_Pats_AST <- DIA_Pats_AST %>% select(1,3,4)
DIA_Pats_ALT <- DIA_Pats_ALT %>% select(1,3,4)
DIA_Pats_Platelets <- DIA_Pats_Platelets %>% select(1,3,4)

DIA_Pats <- DIA_Pats_AST %>% full_join(DIA_Pats_ALT, by = c("patient", "claimed")) %>% full_join(DIA_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

DIA_Pats <- DIA_Pats %>% left_join(DANU_Demographics, by=c("patient"="patid"))
DIA_Pats$claimed <- as.Date(DIA_Pats$claimed)

DIA_Pats$finalDate <- as.Date("2021-04-30")

DIA_Pats$diff <- round(((DIA_Pats$finalDate - DIA_Pats$claimed)/30.5)/12)

DIA_Pats$age <- DIA_Pats$age - DIA_Pats$diff 

DIA_Pats <- DIA_Pats %>% select(1,2,3,4,5,6)
DIA_Pats$age <- as.numeric(DIA_Pats$age)

DIA_Pats$fibrosis4 <- (DIA_Pats$age*DIA_Pats$AST) / (DIA_Pats$Platelets*sqrt(DIA_Pats$ALT))


DIA_Pats <- DIA_Pats %>% filter(Platelets>10)
DIA_Pats <- DIA_Pats %>% filter(AST>5)
DIA_Pats <- DIA_Pats %>% filter(ALT>5)

fwrite(DIA_Pats, "FIB4_Diabetes_Pats_ALL.txt")


range(DIA_Pats$fibrosis4) #0.008262961 5825.695284564

median(DIA_Pats$fibrosis4)  # 1.318781
mean(DIA_Pats$fibrosis4)  # 3.092956
length(unique(DIA_Pats$patient)) # 148380
quantile(DIA_Pats$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.8563988 1.3187811 2.1447812 


DIA_Pats %>% select(fibrosis4) %>% filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



DIA_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #3.02

DIA_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.34

temp2 <- DIA_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.8939671 1.3441004 2.0842980 

temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")






# OBE only
OBE_Pats_AST <- OBE_Pats %>% filter(test=="AST Level")
OBE_Pats_ALT <- OBE_Pats %>% filter(test=="ALT Level")
OBE_Pats_Platelets <- OBE_Pats %>% filter(test=="Platelet Count")

names(OBE_Pats_AST)[4] <-"AST"
names(OBE_Pats_ALT)[4] <-"ALT"
names(OBE_Pats_Platelets)[4] <-"Platelets"

OBE_Pats_AST <- OBE_Pats_AST %>% select(1,3,4)
OBE_Pats_ALT <- OBE_Pats_ALT %>% select(1,3,4)
OBE_Pats_Platelets <- OBE_Pats_Platelets %>% select(1,3,4)

OBE_Pats <- OBE_Pats_AST %>% full_join(OBE_Pats_ALT, by = c("patient", "claimed")) %>% full_join(OBE_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age)

OBE_Pats <- OBE_Pats %>% left_join(DANU_Demographics, by=c("patient"="patid"))
OBE_Pats$claimed <- as.Date(OBE_Pats$claimed)

OBE_Pats$finalDate <- as.Date("2021-04-30")

OBE_Pats$diff <- round(((OBE_Pats$finalDate - OBE_Pats$claimed)/30.5)/12)

OBE_Pats$age <- OBE_Pats$age - OBE_Pats$diff 

OBE_Pats <- OBE_Pats %>% select(1,2,3,4,5,6)
OBE_Pats$age <- as.numeric(OBE_Pats$age)

OBE_Pats$fibrosis4 <- (OBE_Pats$age*OBE_Pats$AST) / (OBE_Pats$Platelets*sqrt(OBE_Pats$ALT))


OBE_Pats <- OBE_Pats %>% filter(Platelets>10)
OBE_Pats <- OBE_Pats %>% filter(AST>5)
OBE_Pats <- OBE_Pats %>% filter(ALT>5)

fwrite(OBE_Pats, "FIB4_Obesity_Pats_ALL.txt")


range(OBE_Pats$fibrosis4) # 0.006037801 1586.266847911

median(OBE_Pats$fibrosis4)  # 1.094585
mean(OBE_Pats$fibrosis4)  # 2.542138
length(unique(OBE_Pats$patient)) # 307309062381
quantile(OBE_Pats$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.6989222 1.0945851 1.7826334


OBE_Pats %>% select(fibrosis4) %>% filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")



OBE_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=mean(fibrosis4)) #2.05

OBE_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>% 
  summarise(n=median(fibrosis4)) #1.03

temp2 <- OBE_Pats %>% group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup()  

quantile(temp2$fibrosis4, c(.25, .50, .75)) 

# 25%       50%       75% 
# 0.6823691 1.0301348 1.5617891 

temp2 %>% select(fibrosis4) %>%
  filter(fibrosis4<=10)%>%
  ggplot(aes(fibrosis4)) +
  geom_histogram(bins=5000, fill="deeppink4")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Patients \n")+xlab("\n FIB-4 Score")


# Merging the 3 datasets together

Nash_Dx <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Nash_Dx <- Nash_Dx %>% select(patient)

Nash_Dx$Dx_Status <- "Yes"

NASH_Pats <- NASH_Pats %>% left_join(Nash_Dx) 
DIA_Pats <- DIA_Pats %>% left_join(Nash_Dx) %>% mutate(Dx_Status=ifelse(is.na(Dx_Status), "No", Dx_Status))
OBE_Pats <- OBE_Pats %>% left_join(Nash_Dx) %>% mutate(Dx_Status=ifelse(is.na(Dx_Status), "No", Dx_Status))



NASH_Pats <- NASH_Pats %>% mutate(Dx_Status=ifelse(Dx_Status=="Yes", 1, 0))
DIA_Pats <- DIA_Pats %>% mutate(Dx_Status=ifelse(Dx_Status=="Yes", 1, 0))
OBE_Pats <- OBE_Pats %>%  mutate(Dx_Status=ifelse(Dx_Status=="Yes", 1, 0))


NASH_Pats$Condition <- "NASH"
DIA_Pats$Condition <- "DIA"
OBE_Pats$Condition <- "OBE"

NAsh_Indicator <- NASH_Pats %>% bind_rows(DIA_Pats) %>% bind_rows(OBE_Pats)

summary(glm( Dx_Status ~ fibrosis4, data = NAsh_Indicator, family = binomial))

# Call:
#   glm(formula = Dx_Status ~ fibrosis4, family = binomial, data = NAsh_Indicator)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -8.4904  -0.3143  -0.3134  -0.3128   2.4687  
# 
# Coefficients:
#   Estimate Std. Error  z value            Pr(>|z|)    
# (Intercept) -2.9987669  0.0028014 -1070.46 <0.0000000000000002 ***
#   fibrosis4    0.0084310  0.0001275    66.15 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1114749  on 2852979  degrees of freedom
# Residual deviance: 1109353  on 2852978  degrees of freedom
# AIC: 1109357
# 
# Number of Fisher Scoring iterations: 6



NAsh_Indicator %>% 
  ggplot(aes(fibrosis4, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")




NAsh_Indicator_1000 <- NAsh_Indicator[fibrosis4<=1000]


# Only FIB4 below 1000

summary(glm( Dx_Status ~ fibrosis4, data = NAsh_Indicator_1000, family = binomial))


# Call:
#   glm(formula = Dx_Status ~ fibrosis4, family = binomial, data = NAsh_Indicator_1000)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.8289  -0.3137  -0.3126  -0.3119   2.4716  
# 
# Coefficients:
#   Estimate Std. Error  z value            Pr(>|z|)    
# (Intercept) -3.0062757  0.0028126 -1068.85 <0.0000000000000002 ***
#   fibrosis4    0.0103636  0.0001375    75.36 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1114730  on 2852907  degrees of freedom
# Residual deviance: 1107735  on 2852906  degrees of freedom
# AIC: 1107739
# 
# Number of Fisher Scoring iterations: 5


NAsh_Indicator_1000 %>% 
  ggplot(aes(fibrosis4, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")


# What if we filter everything below 1000?


NAsh_Indicator_All1000 <- NAsh_Indicator[fibrosis4<=1000 & AST<=1000 & ALT <=1000]


# All below 1000

summary(glm( Dx_Status ~ fibrosis4, data = NAsh_Indicator_All1000, family = binomial))


# Call:
#   glm(formula = Dx_Status ~ fibrosis4, family = binomial, data = NAsh_Indicator_All1000)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q  
# -5.1536  -0.3052  -0.3031  -0.3019  
# Max  
# 2.4998  
# 
# Coefficients:
#   Estimate Std. Error  z value
# (Intercept) -3.0797761  0.0029670 -1037.99
# fibrosis4    0.0199672  0.0002589    77.12
# Pr(>|z|)    
# (Intercept) <0.0000000000000002 ***
#   fibrosis4   <0.0000000000000002 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’
# 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1059266  on 2820000  degrees of freedom
# Residual deviance: 1054211  on 2819999  degrees of freedom
# AIC: 1054215
# 
# Number of Fisher Scoring iterations: 5




NAsh_Indicator_All1000 %>% 
  ggplot(aes(fibrosis4, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")



# All below 200

NAsh_Indicator_All200 <- NAsh_Indicator[fibrosis4<=200 & AST<=200 & ALT <=200]

summary(glm( Dx_Status ~ fibrosis4, data = NAsh_Indicator_All200, family = binomial))

# 
# Call:
#   glm(formula = Dx_Status ~ fibrosis4, family = binomial, data = NAsh_Indicator_All200)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0161  -0.2968  -0.2944  -0.2929   2.5245  
# 
# Coefficients:
#   Estimate Std. Error  z value            Pr(>|z|)    
# (Intercept) -3.1452210  0.0031209 -1007.79 <0.0000000000000002 ***
#   fibrosis4    0.0252456  0.0003297    76.57 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 973862  on 2708989  degrees of freedom
# Residual deviance: 969314  on 2708988  degrees of freedom
# AIC: 969318
# 
# Number of Fisher Scoring iterations: 6



NAsh_Indicator_All200 %>% 
  ggplot(aes(fibrosis4, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")





# All below 200 + Platelets above 10

NAsh_Indicator_All200Platelet10 <- NAsh_Indicator[fibrosis4<=200 & AST<=200 & ALT <=200 & Platelets>=10]

summary(glm( Dx_Status ~ fibrosis4, data = NAsh_Indicator_All200Platelet10, family = binomial))

# Call:
#   glm(formula = Dx_Status ~ fibrosis4, family = binomial, data = NAsh_Indicator_All200Platelet10)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0161  -0.2968  -0.2944  -0.2929   2.5245  
# 
# Coefficients:
#   Estimate Std. Error  z value            Pr(>|z|)    
# (Intercept) -3.1452210  0.0031209 -1007.79 <0.0000000000000002 ***
#   fibrosis4    0.0252456  0.0003297    76.57 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 973862  on 2708989  degrees of freedom
# Residual deviance: 969314  on 2708988  degrees of freedom
# AIC: 969318
# 
# Number of Fisher Scoring iterations: 6


NAsh_Indicator_All200Platelet10 %>% 
  ggplot(aes(fibrosis4, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n FIB-4")+
  ylab("Probability of NASH Dx \n")





# What about using the liver enzymes instead?
# All significant
# Higher AST or ALT -> higher chance of NASH
# Lower platelets or age -> higher chance of NASH 

# BUT !
# Not so great, we would need massive increases in AST or ALT
# Maybe we should try within the NASH and than extrapolate to the other conditions

summary(glm( Dx_Status ~ AST+ALT+Platelets+age, data = NAsh_Indicator_All200Platelet10, family = binomial))


# Call:
#   glm(formula = Dx_Status ~ AST + ALT + Platelets + age, family = binomial, 
#       data = NAsh_Indicator_All200Platelet10)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5320  -0.2881  -0.2526  -0.2274   5.4537  
# 
# Coefficients:
#   Estimate  Std. Error  z value             Pr(>|z|)    
# (Intercept) -3.37706748  0.01388485 -243.220 < 0.0000000000000002 ***
#   AST          0.01070714  0.00009467  113.097 < 0.0000000000000002 ***
#   ALT          0.01053176  0.00008954  117.616 < 0.0000000000000002 ***
#   Platelets   -0.00255859  0.00003139  -81.505 < 0.0000000000000002 ***
#   age          0.00104289  0.00018027    5.785        0.00000000724 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 973862  on 2708989  degrees of freedom
# Residual deviance: 903843  on 2708985  degrees of freedom
# AIC: 903853
# 
# Number of Fisher Scoring iterations: 6




NAsh_Indicator_All200Platelet10 %>% 
  ggplot(aes(AST, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n AST")+
  ylab("Probability of NASH Dx \n")


NAsh_Indicator_All200Platelet10 %>% 
  ggplot(aes(ALT, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n ALT")+
  ylab("Probability of NASH Dx \n")


NAsh_Indicator_All200Platelet10 %>% 
  ggplot(aes(Platelets, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Platelet count")+
  ylab("Probability of NASH Dx \n")


NAsh_Indicator_All200Platelet10 %>% 
  ggplot(aes(age, Dx_Status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Age (years)")+
  ylab("Probability of NASH Dx \n")





NAsh_Indicator %>% filter(Condition=="NASH") %>% select(patient) %>% distinct() #4541
NAsh_Indicator %>% filter(Condition=="DIA") %>% select(patient) %>% distinct() #148380
NAsh_Indicator %>% filter(Condition=="OBE") %>% select(patient) %>% distinct() #309062


NAsh_Indicator %>% filter(Condition=="NASH") %>% filter(AST>=100 & ALT>=100 & ALT>AST) %>% select(patient) %>% distinct() #392
NAsh_Indicator %>% filter(Condition=="NASH") %>% filter(AST>=200 | ALT>=200) %>% select(patient) %>% distinct() #31



NAsh_Indicator %>% filter(Condition=="NASH") %>% filter(AST>=100 | ALT>=100) %>% select(patient) %>% distinct() #1079 #24%
NAsh_Indicator %>% filter(Condition=="DIA") %>% filter(AST>=100 | ALT>=100) %>% select(patient) %>% distinct() #9319 #6%
NAsh_Indicator %>% filter(Condition=="OBE") %>% filter(AST>=100 | ALT>=100) %>% select(patient) %>% distinct() #13701 #4%


NAsh_Indicator %>% filter(Condition=="NASH") %>% filter(AST>=100 | ALT>=100) %>% select(patient) %>% distinct() #1079 #24%
NAsh_Indicator %>% filter(Condition=="DIA") %>% filter(AST>=100 | ALT>=100) %>% select(patient) %>% distinct() #9319 #6%
NAsh_Indicator %>% filter(Condition=="OBE") %>% filter(AST>=100 | ALT>=100) %>% select(patient) %>% distinct() #13701 #4%


NAsh_Indicator %>% filter(Condition=="NASH") %>% filter(fibrosis4>1.3) %>% select(patient) %>% distinct() #2848 #62%
NAsh_Indicator %>% filter(Condition=="DIA") %>% filter(fibrosis4>1.3) %>% select(patient) %>% distinct() #77360 #52%
NAsh_Indicator %>% filter(Condition=="OBE") %>% filter(fibrosis4>1.3) %>% select(patient) %>% distinct() #108335 #35%

NAsh_Indicator %>% filter(Condition=="NASH") %>% filter(fibrosis4>2.67) %>% select(patient) %>% distinct() #1394 #31%
NAsh_Indicator %>% filter(Condition=="DIA") %>% filter(fibrosis4>2.67) %>% select(patient) %>% distinct() #23594 #16%
NAsh_Indicator %>% filter(Condition=="OBE") %>% filter(fibrosis4>2.67) %>% select(patient) %>% distinct() #26311 #9%

NAsh_Indicator %>% filter(Condition=="NASH") %>% filter(ALT>AST) %>% select(patient) %>% distinct() #3636 #80%
NAsh_Indicator %>% filter(Condition=="DIA") %>% filter(ALT>AST) %>% select(patient) %>% distinct() #105681 #71%
NAsh_Indicator %>% filter(Condition=="OBE") %>% filter(ALT>AST) %>% select(patient) %>% distinct() #204558 #66%


# -------

# Create an indicator table with the tests of relevance each NASH patient had  -------------

# ALL NASH Pats
NASH_Pats_Index <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Pats_Index <- NASH_Pats_Index %>% select(patient)


# Pats with +2 Diagnoses
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))

NASH_diagnosis <- NASH_diagnosis %>% select(patient) %>% left_join(NASH_Events, by=c("patient"="patid"))
NASH_diagnosis <- NASH_diagnosis %>% filter(condition=="NASH")

Pats_2Plus_Dxs <- NASH_diagnosis %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% distinct() #5,676
Pats_2Plus_Dxs$PLus2_Dx <- "Plus2_Dxs"


# Biopsy pats
NASH_Events <- fread("NASH Events.txt")
Dx_code <- fread("NASH Diagnosis Codes.txt")
Dx_code <- Dx_code %>% select(code, condition, source, type, description)
NASH_Events <- NASH_Events %>% left_join(Dx_code)
NASH_Pats <- NASH_Events %>% filter(condition=="NASH") %>% select(patid) %>% distinct()
NASH_Pats <- NASH_Pats %>% left_join(NASH_Events)
NASH_Pats %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #1339983
Biopsy_Pats <- NASH_Pats %>% select(patid, weight, condition) %>% distinct() %>% filter(condition=="Liver Biopsy") %>% select(patid) %>% distinct()
Biopsy_Pats$Biopsy <- "Biopsy"
names(Biopsy_Pats)[1] <- "patient"

NASH_Pats_Index <- NASH_Pats_Index %>% left_join(Pats_2Plus_Dxs) %>% left_join(Biopsy_Pats)


# Scores above / below threshold 
NASH_Pats <- fread("FIB4_NASH_Pats.txt")

AST_above50 <- NASH_Pats %>% filter(AST>50) %>% select(patient) %>% distinct()
AST_above50$AST_above50 <- "AST_above50"

ALT_above50 <- NASH_Pats %>% filter(ALT>50) %>% select(patient) %>% distinct()
ALT_above50$ALT_above50 <- "AST_above50"

AST_ALT_above50 <- NASH_Pats %>% filter(ALT>50 & AST>50) %>% select(patient) %>% distinct()
AST_ALT_above50$AST_ALT_above50 <- "AST_ALT_above50"

Platelets_below150 <- NASH_Pats %>% filter(Platelets<150) %>% select(patient) %>% distinct()
Platelets_below150$Platelets_below150 <- "Platelets_below150"

FIB4_1p3 <- NASH_Pats %>% filter(fibrosis4>1.3) %>% select(patient) %>% distinct()
FIB4_1p3$FIB4_1p3 <- "FIB4_1p3"

FIB4_2p7 <- NASH_Pats %>% filter(fibrosis4>2.7) %>% select(patient) %>% distinct()
FIB4_2p7$FIB4_2p7 <- "FIB4_2p7"

NASH_Pats_Index <- NASH_Pats_Index %>% left_join(AST_above50) %>% left_join(ALT_above50) %>% left_join(AST_ALT_above50) %>% 
  left_join(Platelets_below150) %>% left_join(FIB4_1p3) %>% left_join(FIB4_2p7)


Pats_to_remove <- data.frame(NASH_Pats_Index[is.na(PLus2_Dx)&is.na(Biopsy)&is.na(AST_above50)&is.na(ALT_above50)&is.na(AST_ALT_above50)&
                                               is.na(Platelets_below150)&is.na(FIB4_1p3)&is.na(FIB4_2p7),patient])

names(Pats_to_remove) <- "patient"

NASH_Pats_Index <- NASH_Pats_Index %>% anti_join(Pats_to_remove)

NASH_Pats_Index %>% filter(PLus2_Dx=="Plus2_Dxs" & (AST_above50=="AST_above50"|ALT_above50=="ALT_above50") & FIB4_1p3=="FIB4_1p3")

NASH_Pats_Index %>% filter(AST_above50=="AST_above50"&ALT_above50=="AST_above50"&Platelets_below150=="Platelets_below150")



# Define a "NEGATIVE" Population and a "POSITIVE" population to train the model and find an acceptable cut-off
# Check how many on the entire populations would be above the cut-off

DIA_Pats <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Pats <- DIA_Pats %>% select(patient)

NASH_Dossiers <- fread("NASH Dossiers.txt")

DIA_Pats <- DIA_Pats %>% left_join(NASH_Dossiers, by=c("patient"="patid"))
unique(DIA_Pats$condition)


NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")

DIA_Pats_to_remove <- DIA_Pats %>% filter((AST>50|ALT>50|Platelets<150)) %>% select(patient) %>% distinct()

DIA_Pats <- DIA_Pats %>% anti_join(DTA_Pats_to_remove)



NASH_Pats <- fread("FIB4_NASH_Pats.txt")

NASH_To_keep <- NASH_Pats %>% filter(AST>50&ALT>50&Platelets<150) %>% select(patient) %>% distinct()

NASH_Pats <- NASH_To_keep %>% left_join(NASH_Pats)

NASH_Pats$Dx_status <- 1
DIA_Pats$Dx_status <- 0

Indicator <- NASH_Pats %>% bind_rows(DIA_Pats)


summary(glm( Dx_status ~ AST, data = Indicator, family = binomial))



# --------
# FIB4 distribution by type of NASH --------------

NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_diagnosis <- NASH_diagnosis %>% filter(grepl("NASH", NASH_diganosis))

NASH_Pats_Cirrhosis <- NASH_diagnosis  %>% filter(NASH_diganosis == "NASH-Cirrohsis") %>% select(patient) 
NASH_Pats_Fibrosis <- NASH_diagnosis  %>% filter(NASH_diganosis == "NASH-Fibrosis") %>% select(patient)
NASH_Pats_NASHOnly <- NASH_diagnosis  %>% filter(NASH_diganosis == "NASH-Only") %>% select(patient) 

NASH_Pats_Cirrhosis$group <- "Cirrhosis"
NASH_Pats_Fibrosis$group <- "Fibrosis"
NASH_Pats_NASHOnly$group <- "NASH Only"

NASH_Pats <- fread("FIB4_NASH_Pats.txt")
NASH_Pats <- NASH_Pats %>% select(patient, fibrosis4)


NASH_Pats_Cirrhosis <- NASH_Pats_Cirrhosis %>% inner_join(NASH_Pats)
NASH_Pats_Fibrosis <- NASH_Pats_Fibrosis %>% inner_join(NASH_Pats)
NASH_Pats_NASHOnly <- NASH_Pats_NASHOnly %>% inner_join(NASH_Pats)


Temp_Ridges <- NASH_Pats_Cirrhosis %>% bind_rows(NASH_Pats_Fibrosis) %>% bind_rows(NASH_Pats_NASHOnly)

Temp_Ridges %>% filter(fibrosis4<10) %>% ggplot(aes(x = fibrosis4, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n FIB4") + ylab("Disease Group \n")


length(unique(NASH_Pats$patient))

# -----------
# Train a logistic regression model with POSITIVE cases as anyone who ever had a BIOPSY or AST&ALT&PLATELETS altered ----------
# Those who never did but for which we have all the tests as negative controls 

# Remove everyone with Liver Cancer or Alcohol Abuse
NASH_Dossiers <- fread("NASH Dossiers.txt")
Cancer_Alcohol_pats <- NASH_Dossiers %>% filter(condition == "Liver Cancer" | condition == "Alcohol Abuse") %>% select(patid) %>% distinct()
Biopsy_pats <- NASH_Dossiers %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct()
names(Cancer_Alcohol_pats)[1] <- "patient"
names(Biopsy_pats)[1] <- "patient"
Biopsy_pats$Biopsy_Ever <- "Biopsy"

# Get Pats with all labs on the same date 
NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")

NASH_Pats$group <- "NASH"
DIA_Pats$group <- "DIA"
OBE_Pats$group <- "OBE"
NAFLD_Pats$group <- "NAFLD"


Temp_Ridges <- NASH_Pats %>% bind_rows(DIA_Pats) %>% bind_rows(OBE_Pats) %>% bind_rows(NAFLD_Pats)

Temp_Ridges$group <- factor(Temp_Ridges$group, levels = c("OBE", "DIA", "NAFLD", "NASH"))

Temp_Ridges %>% filter(AST>0 & AST<200) %>% ggplot(aes(x = AST, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n AST (IU/L)") + ylab("Disease Group \n")



Temp_Ridges %>% filter(ALT>0 & ALT<200) %>% ggplot(aes(x = ALT, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n ALT (IU/L)") + ylab("Disease Group \n")



Temp_Ridges %>% filter(Platelets>0 & Platelets<600) %>% ggplot(aes(x = Platelets, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "A", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Platelet Count (x10^3 / uL)") + ylab("Disease Group \n")



Temp_Ridges %>% filter(fibrosis4<10) %>% ggplot(aes(x = fibrosis4, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n FIB4") + ylab("Disease Group \n")


DANU_Measures <- fread("DANU Measures.txt")



# Remove Cancer/Alcohol
NASH_Pats <- NASH_Pats %>% anti_join(Cancer_Alcohol_pats)
DIA_Pats <- DIA_Pats %>% anti_join(Cancer_Alcohol_pats)
OBE_Pats<- OBE_Pats %>% anti_join(Cancer_Alcohol_pats)
NAFLD_Pats<- NAFLD_Pats %>% anti_join(Cancer_Alcohol_pats)

# Add Biopsy ever status
NASH_Pats <- NASH_Pats %>% left_join(Biopsy_pats)
DIA_Pats <- DIA_Pats %>% left_join(Biopsy_pats)
OBE_Pats<- OBE_Pats %>% left_join(Biopsy_pats)
NAFLD_Pats<- NAFLD_Pats %>% left_join(Biopsy_pats)


# Flag patients as Positive / negative
NASH_To_keep <- NASH_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
NASH_To_keep$Dx_status <- 1
NASH_To_keep <- NASH_To_keep %>% left_join(NASH_Pats)
NASH_To_keep <- NASH_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
NASH_To_keep <- NASH_To_keep %>% select(-Biopsy_Ever)
NASH_To_keep <- NASH_To_keep %>% select(1,3,4,5,6,7,8,2)


DIA_To_keep <- DIA_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
DIA_To_keep$Dx_status <- 1
DIA_To_keep <- DIA_To_keep %>% left_join(DIA_Pats)
DIA_To_keep <- DIA_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
DIA_To_keep <- DIA_To_keep %>% select(-Biopsy_Ever)
DIA_To_keep <- DIA_To_keep %>% select(1,3,4,5,6,7,8,2)


OBE_To_keep <- OBE_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
OBE_To_keep$Dx_status <- 1
OBE_To_keep <- OBE_To_keep %>% left_join(OBE_Pats)
OBE_To_keep <- OBE_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
OBE_To_keep <- OBE_To_keep %>% select(-Biopsy_Ever)
OBE_To_keep <- OBE_To_keep %>% select(1,3,4,5,6,7,8,2)


NAFLD_To_keep <- NAFLD_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
NAFLD_To_keep$Dx_status <- 1
NAFLD_To_keep <- NAFLD_To_keep %>% left_join(NAFLD_Pats)
NAFLD_To_keep <- NAFLD_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
NAFLD_To_keep <- NAFLD_To_keep %>% select(-Biopsy_Ever)
NAFLD_To_keep <- NAFLD_To_keep %>% select(1,3,4,5,6,7,8,2)


NASH_Pats_to_remove <- NASH_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
NASH_Negative <- NASH_Pats %>% anti_join(NASH_Pats_to_remove)
NASH_Negative <- NASH_Negative %>% select(-Biopsy_Ever)
NASH_Negative$Dx_status <- 0


DIA_Pats_to_remove <- DIA_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
DIA_Negative <- DIA_Pats %>% anti_join(DIA_Pats_to_remove)
DIA_Negative <- DIA_Negative %>% select(-Biopsy_Ever)
DIA_Negative$Dx_status <- 0

OBE_Pats_to_remove <- OBE_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
OBE_Negative <- OBE_Pats %>% anti_join(OBE_Pats_to_remove)
OBE_Negative <- OBE_Negative %>% select(-Biopsy_Ever)
OBE_Negative$Dx_status <- 0

NAFLD_Pats_to_remove <- NAFLD_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
NAFLD_Negative <- NAFLD_Pats %>% anti_join(NAFLD_Pats_to_remove)
NAFLD_Negative <- NAFLD_Negative %>% select(-Biopsy_Ever)
NAFLD_Negative$Dx_status <- 0


NASH_To_keep <- NASH_To_keep %>% sample_n(5000)
DIA_To_keep <- DIA_To_keep %>% sample_n(5000)
OBE_To_keep <- OBE_To_keep %>% sample_n(5000)
NAFLD_To_keep <- NAFLD_To_keep %>% sample_n(5000)
NASH_Negative <- NASH_Negative %>% sample_n(5000)
DIA_Negative <- DIA_Negative %>% sample_n(5000)
OBE_Negative <- OBE_Negative %>% sample_n(5000)
NAFLD_Negative <- NAFLD_Negative %>% sample_n(5000)

Indicator <- NASH_To_keep %>% bind_rows(DIA_To_keep) %>% bind_rows(OBE_To_keep) %>% bind_rows(NAFLD_To_keep) %>%
  bind_rows(DIA_Negative) %>% bind_rows(OBE_Negative) %>% bind_rows(NAFLD_Negative)

Indicator %>% filter(Dx_status==1) %>% summarise(n=mean(fibrosis4))
Indicator %>% filter(Dx_status==0) %>% summarise(n=mean(fibrosis4))
Indicator %>% filter(Dx_status==1) %>% summarise(n=median(fibrosis4))
Indicator %>% filter(Dx_status==0) %>% summarise(n=median(fibrosis4))


Risk_pred_model <- glm( Dx_status ~ AST+ALT+Platelets, data = Indicator, family = binomial)

NASH_Pats <- NASH_Pats %>% select(-c(Biopsy_Ever))
DIA_Pats <- DIA_Pats %>% select(-c(Biopsy_Ever))
OBE_Pats <- OBE_Pats %>% select(-c(Biopsy_Ever))
NAFLD_Pats <- NAFLD_Pats %>% select(-c(Biopsy_Ever))



Indicator %>% 
  ggplot(aes(fibrosis4, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,25)+
  xlab("\n FIB-4")+
  ylab("Probability of High-Risk patient \n")


Indicator %>% 
  ggplot(aes(AST, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,200)+
  xlab("\n AST (IU/L)")+
  ylab("Probability of High-Risk patient \n")


Indicator %>% 
  ggplot(aes(ALT, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,200)+
  xlab("\n ALT (IU/L)")+
  ylab("Probability of High-Risk patient \n")



Indicator %>% 
  ggplot(aes(Platelets, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray", level=0.99 ) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,1000)+
  xlab("\n Platelets (x 10^3 / uL)")+
  ylab("Probability of High-Risk patient \n")



NASH_Probability <- data.frame(Risk_pred_model %>% predict(NASH_Pats, type = "response"))
DIA_Probability <- data.frame(Risk_pred_model %>% predict(DIA_Pats, type = "response"))
OBE_Probability <- data.frame(Risk_pred_model %>% predict(OBE_Pats, type = "response"))
NAFLD_Probability <- data.frame(Risk_pred_model %>% predict(NAFLD_Pats, type = "response"))


NASH_Probability <- NASH_Pats %>% select(patient) %>% bind_cols(NASH_Probability)
DIA_Probability <- DIA_Pats %>% select(patient) %>% bind_cols(DIA_Probability)
OBE_Probability <- OBE_Pats %>% select(patient) %>% bind_cols(OBE_Probability)
NAFLD_Probability <- NAFLD_Pats %>% select(patient) %>% bind_cols(NAFLD_Probability)


length(unique(NASH_Probability$patient)) # 3747
length(unique(DIA_Probability$patient)) # 134827
length(unique(OBE_Probability$patient)) # 288951
length(unique(NAFLD_Probability$patient)) # 40686



NASH_Probability %>% filter(Risk_pred_model.....predict.NASH_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() # 2150 (57%)   (62% if not using the NASH as negative)

DIA_Probability %>% filter(Risk_pred_model.....predict.DIA_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() # 27648 (20%)   (24% if not using the NASH as negative)

OBE_Probability %>% filter(Risk_pred_model.....predict.OBE_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() # 43010 (15%)   (18% if not using the NASH as negative)

NAFLD_Probability %>% filter(Risk_pred_model.....predict.NAFLD_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() # 14449 (35%)   (40% if not using the NASH as negative)

# --------
# NASH patients Dx vs FBI4 --------------------------------------
NASH_Dossiers <- fread("NASH Dossiers.txt")
Cancer_Alcohol_pats <- NASH_Dossiers %>% filter(condition == "Liver Cancer" | condition == "Alcohol Abuse") %>% select(patid) %>% distinct()
names(Cancer_Alcohol_pats)[1] <- "patient"


NASH_Pats_FIB4 <- fread("FIB4_NASH_Pats.txt")
NASH_Pats_FIB4 <- NASH_Pats_FIB4 %>% anti_join(Cancer_Alcohol_pats)
NASH_Pats_FIB4 <- NASH_Pats_FIB4 %>% group_by(patient) %>% filter(fibrosis4 == max(fibrosis4)) %>% slice(1)
NASH_Pats_FIB4 <- NASH_Pats_FIB4 %>% select(patient, age, fibrosis4)

NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_diagnosis %>% filter(grepl("NASH",NASH_diganosis)) %>% summarise(n=sum(weight)) # 1339983
NASH_diagnosis <- NASH_diagnosis %>%filter(grepl("NASH",NASH_diganosis))
NASH_diagnosis <- NASH_diagnosis %>% anti_join(Cancer_Alcohol_pats)


NASH_diagnosis <- NASH_diagnosis %>% left_join(NASH_Pats_FIB4) %>% drop_na()

NASH_diagnosis <- NASH_diagnosis %>% mutate(FIB4_Bucket = ifelse(fibrosis4>4.12, "F4",
                                                                 ifelse(fibrosis4>2.67&fibrosis4<=4.12,"F3","F1-F2")))




NASH_diagnosis <- NASH_diagnosis %>% mutate(FIB4_Bucket = ifelse(fibrosis4>1.3&age<60, "Fibrosis",
                                                                 ifelse(fibrosis4>1.88&age<70,"Fibrosis",
                                                                        ifelse(fibrosis4>1.95&age>70,"Fibrosis", "NASH-only"))))


NASH_diagnosis <- NASH_diagnosis %>% mutate(FIB4_Bucket = ifelse(NASH_diagnosis$NASH_diganosis=="NASH-Cirrohsis", "NASH-Cirrhosis", FIB4_Bucket))

NASH_diagnosis%>% group_by(FIB4_Bucket) %>% summarise(n=sum(weight))


FIB4_Bucket <- NASH_diagnosis %>% select(patient, FIB4_Bucket)

fwrite(FIB4_Bucket, "FIB4_Bucket_Fibrosis.txt", sep=)


NASH_diagnosis %>% group_by(NASH_diganosis) %>% summarise(n=sum(weight))

# NASH_diganosis       n
# <chr>            <dbl>
# 1 NASH-Cirrohsis 101377.
# 2 NASH-Fibrosis   40352.
# 3 NASH-Only      372630.

NASH_diagnosis %>% group_by(FIB4_Bucket) %>% summarise(n=sum(weight))

# FIB4_Bucket       n
# <chr>         <dbl>
# 1 F1-F2       382259.
# 2 F3           57828.
# 3 F4           74274.

NASH_diagnosis <- NASH_diagnosis %>% mutate(FIB4_Bucket = ifelse(NASH_diagnosis$NASH_diganosis=="NASH-Cirrohsis", "NASH-Cirrhosis", FIB4_Bucket))


NASH_diagnosis %>% group_by(NASH_diganosis) %>% summarise(n=sum(weight))

# 
# NASH_diganosis       n
# <chr>            <dbl>
# 1 NASH-Cirrohsis 101377.
# 2 NASH-Fibrosis   40352.
# 3 NASH-Only      372630.

NASH_diagnosis %>% group_by(FIB4_Bucket) %>% summarise(n=sum(weight))

# FIB4_Bucket          n
# <chr>            <dbl>
# 1 F1-F2          343913.
# 2 F3              36334.
# 3 F4              32735.
# 4 NASH-Cirrhosis 101377.


NASH_diagnosis %>% mutate(FIB4_Bucket = ifelse(FIB4_Bucket=="NASH-Cirrhosis", "NASH-Cirrhosis", ifelse(fibrosis4>1.45,"NASH-Fibrosis","NASH-only"))) %>% 
  group_by(FIB4_Bucket) %>% summarise(n=sum(weight))

# FIB4_Bucket          n
# <chr>            <dbl>
#   1 NASH-Cirrhosis 101377.
# 2 NASH-Fibrosis  185210.
# 3 NASH-only      227773


NASH_diagnosis %>% mutate(FIB4_Bucket = ifelse(FIB4_Bucket=="NASH-Cirrhosis", "NASH-Cirrhosis", ifelse(fibrosis4>2.67,"NASH-Fibrosis","NASH-only"))) %>% 
  group_by(FIB4_Bucket) %>% summarise(n=sum(weight))

# FIB4_Bucket          n
# <chr>            <dbl>
#   1 NASH-Cirrhosis 101377.
# 2 NASH-Fibrosis   69069.
# 3 NASH-only      343913.

# -------
# Random Patient Sample to predict risk ------------------------
Rand_pts_Lab_Results_lst5y <- fread("Rand_pts_Lab_Results_lst5y.txt")
DANU_Measure_Codes <- fread("DANU Measure Codes.txt")
DANU_Measure_Codes <- DANU_Measure_Codes %>% select(test, code)

Rand_pts_Lab_Results_lst5y <- Rand_pts_Lab_Results_lst5y %>% left_join(DANU_Measure_Codes, by=c("loinc_cd"="code"))

Rand_AST <- Rand_pts_Lab_Results_lst5y %>% filter(test == "AST Level") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_AST)[3] <- "AST"

Rand_ALT <- Rand_pts_Lab_Results_lst5y %>% filter(test == "ALT Level") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_ALT)[3] <- "ALT"

Rand_Platelets <- Rand_pts_Lab_Results_lst5y %>% filter(test == "Platelet Count") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_Platelets)[3] <- "Platelets"


Rand_pats_Labs <- Rand_AST %>% full_join(Rand_ALT) %>% full_join(Rand_Platelets) %>% drop_na() %>% distinct()

names(Rand_pats_Labs)[1] <- "patient"
names(Rand_pats_Labs)[2] <- "claimed"

Rand_pats_Labs <- Rand_pats_Labs %>% filter(AST>5&ALT>5&Platelets>5)

Rand_Ages <- Rand_pts_Lab_Results_lst5y %>% select(ptid, age)
names(Rand_Ages)[1] <- "patient"
Rand_Ages <- Rand_Ages %>% distinct()
Rand_pats_Labs <- Rand_pats_Labs %>% left_join(Rand_Ages)


Rand_pats_Labs$finalDate <- as.Date("2021-04-30")
Rand_pats_Labs$claimed <- as.Date(Rand_pats_Labs$claimed)

Rand_pats_Labs$diff <- round(((Rand_pats_Labs$finalDate - Rand_pats_Labs$claimed)/30.5)/12)

Rand_pats_Labs$age <- Rand_pats_Labs$age - Rand_pats_Labs$diff 

Rand_pats_Labs <- Rand_pats_Labs %>% select(-c(finalDate, diff))

Rand_pats_Labs$fibrosis4 <- (Rand_pats_Labs$age*Rand_pats_Labs$AST) / (Rand_pats_Labs$Platelets*sqrt(Rand_pats_Labs$ALT))

Rand_pats_Labs$age <- as.numeric(Rand_pats_Labs$age)
Rand_pats_Labs$fibrosis4 <- as.numeric(Rand_pats_Labs$fibrosis4)

fwrite(Rand_pats_Labs, "FIB4_Random_Pats.txt", sep="\t")

# -----
# Filter the random patients for the right age buckets -----------------

FIB4_Random_Pats <- fread("FIB4_Random_Pats.txt")

FIB4_Random_Pats <- FIB4_Random_Pats %>% select(patient, age) %>% group_by(patient) %>% filter(age==max(age)) %>% slice(1) %>% arrange(age)
FIB4_Random_Pats <- as.data.frame(FIB4_Random_Pats)

sample_scheme  <- fread("Number of patients by age for NASH logisitic regression.csv")
sample_scheme <- as.data.frame(sample_scheme)

temp <- FIB4_Random_Pats %>% filter(age>0) %>%
  nest(data = -age) %>%
  inner_join(sample_scheme, by=c("age"="age")) %>% 
  mutate(Sample = map2(data, PatientsNeeded, sample_n)) %>% 
  unnest(Sample)

FIB4_Random_Pats <- fread("FIB4_Random_Pats.txt")

FIB4_Random_Pats <- temp %>% select(patient) %>% left_join(FIB4_Random_Pats)

fwrite(FIB4_Random_Pats, "FIB4_Random_Pats_Filtered.txt")


# -----------

# Train a logistic regression model with POSITIVE cases as anyone who ever had a BIOPSY or AST&ALT&PLATELETS altered ----------
# Those who never did but for which we have all the tests as negative controls 

# Remove everyone with Liver Cancer or Alcohol Abuse
NASH_Dossiers <- fread("NASH Dossiers.txt")
Cancer_Alcohol_pats <- NASH_Dossiers %>% filter(condition == "Liver Cancer" | condition == "Alcohol Abuse") %>% select(patid) %>% distinct()
Biopsy_pats <- NASH_Dossiers %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct()
names(Cancer_Alcohol_pats)[1] <- "patient"
names(Biopsy_pats)[1] <- "patient"
Biopsy_pats$Biopsy_Ever <- "Biopsy"

# Get Pats with all labs on the same date 
NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt") 

NAFLD <- NAFLD_Pats %>% select(patient)
DIA_Pats <- DIA_Pats %>% anti_join(NAFLD)
OBE_Pats <- OBE_Pats %>% anti_join(NAFLD)


NASH_Pats$group <- "NASH"
DIA_Pats$group <- "DIA"
OBE_Pats$group <- "OBE"
NAFLD_Pats$group <- "NAFLD"
Random_Pats$group <- "Random Sample"



Temp_Ridges <- NASH_Pats %>% bind_rows(DIA_Pats) %>% bind_rows(OBE_Pats) %>% bind_rows(NAFLD_Pats) %>% bind_rows(Random_Pats)

Temp_Ridges %>% filter(AST>0 & AST<200) %>% ggplot(aes(x = AST, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n AST (IU/L)") + ylab("Disease Group \n")


Temp_Ridges %>% filter(ALT>0 & ALT<200) %>% ggplot(aes(x = ALT, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n ALT (IU/L)") + ylab("Disease Group \n")



Temp_Ridges %>% filter(Platelets>0 & Platelets<600) %>% ggplot(aes(x = Platelets, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "A", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Platelet Count (x10^3 / uL)") + ylab("Disease Group \n")



Temp_Ridges %>% filter(fibrosis4<10) %>% ggplot(aes(x = fibrosis4, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n FIB4") + ylab("Disease Group \n")




# Remove Cancer/Alcohol
NASH_Pats <- NASH_Pats %>% anti_join(Cancer_Alcohol_pats)
DIA_Pats <- DIA_Pats %>% anti_join(Cancer_Alcohol_pats)
OBE_Pats<- OBE_Pats %>% anti_join(Cancer_Alcohol_pats)
NAFLD_Pats<- NAFLD_Pats %>% anti_join(Cancer_Alcohol_pats)


# Add Biopsy ever status
NASH_Pats <- NASH_Pats %>% left_join(Biopsy_pats)
DIA_Pats <- DIA_Pats %>% left_join(Biopsy_pats)
OBE_Pats<- OBE_Pats %>% left_join(Biopsy_pats)
NAFLD_Pats<- NAFLD_Pats %>% left_join(Biopsy_pats)


# Flag patients as Positive / negative
NASH_To_keep <- NASH_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
NASH_To_keep$Dx_status <- 1
NASH_To_keep <- NASH_To_keep %>% left_join(NASH_Pats)
NASH_To_keep <- NASH_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
NASH_To_keep <- NASH_To_keep %>% select(-Biopsy_Ever)
NASH_To_keep <- NASH_To_keep %>% select(1,3,4,5,6,7,8,2)


DIA_To_keep <- DIA_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
DIA_To_keep$Dx_status <- 1
DIA_To_keep <- DIA_To_keep %>% left_join(DIA_Pats)
DIA_To_keep <- DIA_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
DIA_To_keep <- DIA_To_keep %>% select(-Biopsy_Ever)
DIA_To_keep <- DIA_To_keep %>% select(1,3,4,5,6,7,8,2)


OBE_To_keep <- OBE_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
OBE_To_keep$Dx_status <- 1
OBE_To_keep <- OBE_To_keep %>% left_join(OBE_Pats)
OBE_To_keep <- OBE_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
OBE_To_keep <- OBE_To_keep %>% select(-Biopsy_Ever)
OBE_To_keep <- OBE_To_keep %>% select(1,3,4,5,6,7,8,2)


NAFLD_To_keep <- NAFLD_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
NAFLD_To_keep$Dx_status <- 1
NAFLD_To_keep <- NAFLD_To_keep %>% left_join(NAFLD_Pats)
NAFLD_To_keep <- NAFLD_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
NAFLD_To_keep <- NAFLD_To_keep %>% select(-Biopsy_Ever)
NAFLD_To_keep <- NAFLD_To_keep %>% select(1,3,4,5,6,7,8,2)


NASH_Pats_to_remove <- NASH_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
NASH_Negative <- NASH_Pats %>% anti_join(NASH_Pats_to_remove)
NASH_Negative <- NASH_Negative %>% select(-Biopsy_Ever)
NASH_Negative$Dx_status <- 0


DIA_Pats_to_remove <- DIA_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
DIA_Negative <- DIA_Pats %>% anti_join(DIA_Pats_to_remove)
DIA_Negative <- DIA_Negative %>% select(-Biopsy_Ever)
DIA_Negative$Dx_status <- 0

OBE_Pats_to_remove <- OBE_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
OBE_Negative <- OBE_Pats %>% anti_join(OBE_Pats_to_remove)
OBE_Negative <- OBE_Negative %>% select(-Biopsy_Ever)
OBE_Negative$Dx_status <- 0

NAFLD_Pats_to_remove <- NAFLD_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
NAFLD_Negative <- NAFLD_Pats %>% anti_join(NAFLD_Pats_to_remove)
NAFLD_Negative <- NAFLD_Negative %>% select(-Biopsy_Ever)
NAFLD_Negative$Dx_status <- 0


NASH_To_keep <- NASH_To_keep %>% sample_n(5000)
DIA_To_keep <- DIA_To_keep %>% sample_n(5000)
OBE_To_keep <- OBE_To_keep %>% sample_n(5000)
NAFLD_To_keep <- NAFLD_To_keep %>% sample_n(5000)
NASH_Negative <- NASH_Negative %>% sample_n(5000)
DIA_Negative <- DIA_Negative %>% sample_n(5000)
OBE_Negative <- OBE_Negative %>% sample_n(5000)
NAFLD_Negative <- NAFLD_Negative %>% sample_n(5000)

Indicator <- NASH_To_keep %>% bind_rows(DIA_To_keep) %>% bind_rows(OBE_To_keep) %>% bind_rows(NAFLD_To_keep) %>%
  bind_rows(DIA_Negative) %>% bind_rows(OBE_Negative) %>% bind_rows(NAFLD_Negative)

Indicator %>% filter(Dx_status==1) %>% summarise(n=mean(fibrosis4))
Indicator %>% filter(Dx_status==0) %>% summarise(n=mean(fibrosis4))
Indicator %>% filter(Dx_status==1) %>% summarise(n=median(fibrosis4))
Indicator %>% filter(Dx_status==0) %>% summarise(n=median(fibrosis4))


Risk_pred_model <- glm( Dx_status ~ AST+ALT+Platelets, data = Indicator, family = binomial)

NASH_Pats <- NASH_Pats %>% select(-c(Biopsy_Ever))
DIA_Pats <- DIA_Pats %>% select(-c(Biopsy_Ever))
OBE_Pats <- OBE_Pats %>% select(-c(Biopsy_Ever))
NAFLD_Pats <- NAFLD_Pats %>% select(-c(Biopsy_Ever))



Indicator %>% 
  ggplot(aes(fibrosis4, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,25)+
  xlab("\n FIB-4")+
  ylab("Probability of High-Risk patient \n")


Indicator %>% 
  ggplot(aes(AST, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,200)+
  xlab("\n AST (IU/L)")+
  ylab("Probability of High-Risk patient \n")


Indicator %>% 
  ggplot(aes(ALT, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,200)+
  xlab("\n ALT (IU/L)")+
  ylab("Probability of High-Risk patient \n")



Indicator %>% 
  ggplot(aes(Platelets, Dx_status)) +
  #geom_point(alpha = 0.2, colour="firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour="darkslategray", fill="darkslategray", level=0.99 ) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(0,1000)+
  xlab("\n Platelets (x 10^3 / uL)")+
  ylab("Probability of High-Risk patient \n")




NASH_Probability <- data.frame(Risk_pred_model %>% predict(NASH_Pats, type = "response"))
DIA_Probability <- data.frame(Risk_pred_model %>% predict(DIA_Pats, type = "response"))
OBE_Probability <- data.frame(Risk_pred_model %>% predict(OBE_Pats, type = "response"))
NAFLD_Probability <- data.frame(Risk_pred_model %>% predict(NAFLD_Pats, type = "response"))
Random_Probability <- data.frame(Risk_pred_model %>% predict(Random_Pats, type = "response"))


NASH_Probability <- NASH_Pats %>% select(patient) %>% bind_cols(NASH_Probability)
DIA_Probability <- DIA_Pats %>% select(patient) %>% bind_cols(DIA_Probability)
OBE_Probability <- OBE_Pats %>% select(patient) %>% bind_cols(OBE_Probability)
NAFLD_Probability <- NAFLD_Pats %>% select(patient) %>% bind_cols(NAFLD_Probability)
Random_Probability <- Random_Pats %>% select(patient) %>% bind_cols(Random_Probability)


NASH_Probability <- NASH_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(NASH_Probability) %>% ungroup()
DIA_Probability <- DIA_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(DIA_Probability) %>% ungroup()
OBE_Probability <- OBE_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(OBE_Probability) %>% ungroup()
NAFLD_Probability <- NAFLD_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(NAFLD_Probability) %>% ungroup()
Random_Probability <- Random_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(Random_Probability) %>% ungroup()



length(unique(NASH_Probability$patient)) # 2886
length(unique(DIA_Probability$patient)) # 99120
length(unique(OBE_Probability$patient)) # 184714
length(unique(NAFLD_Probability$patient)) # 29967
length(unique(Random_Probability$patient)) # 2207


NASH_Probability %>% filter(Risk_pred_model.....predict.NASH_Pats..type....response..>0.75) %>%
  select(patient) %>% distinct() # (62% if not using the NASH as negative)

DIA_Probability %>% filter(Risk_pred_model.....predict.DIA_Pats..type....response..>0.75) %>%
  select(patient) %>% distinct() # (24% if not using the NASH as negative)

OBE_Probability %>% filter(Risk_pred_model.....predict.OBE_Pats..type....response..>0.75) %>%
  select(patient) %>% distinct() #  (18% if not using the NASH as negative)

NAFLD_Probability %>% filter(Risk_pred_model.....predict.NAFLD_Pats..type....response..>0.75) %>%
  select(patient) %>% distinct() #   (40% if not using the NASH as negative)

Random_Probability %>% filter(Risk_pred_model.....predict.Random_Pats..type....response..>0.75) %>%
  select(patient) %>% distinct() #  (13% if not using the NASH as negative)




NASH_Probability %>% filter(Risk_pred_model.....predict.NASH_Pats..type....response..>0.85) %>%
  select(patient) %>% distinct() # (54% if not using the NASH as negative)

DIA_Probability %>% filter(Risk_pred_model.....predict.DIA_Pats..type....response..>0.85) %>%
  select(patient) %>% distinct() # (10% if not using the NASH as negative)

OBE_Probability %>% filter(Risk_pred_model.....predict.OBE_Pats..type....response..>0.85) %>%
  select(patient) %>% distinct() #  (13% if not using the NASH as negative)

NAFLD_Probability %>% filter(Risk_pred_model.....predict.NAFLD_Pats..type....response..>0.85) %>%
  select(patient) %>% distinct() #   (24% if not using the NASH as negative)

Random_Probability %>% filter(Risk_pred_model.....predict.Random_Pats..type....response..>0.85) %>%
  select(patient) %>% distinct() #  (9% if not using the NASH as negative)



length(unique(NASH_Probability$patient)) # 2886
length(unique(DIA_Probability$patient)) # 99120
length(unique(OBE_Probability$patient)) # 184714
length(unique(NAFLD_Probability$patient)) # 29967
length(unique(Random_Probability$patient)) # 2207

LiverDiseaseComorbidPats <- fread("liver disease comorbid patients.txt", sep = "\t", header = F)
names(LiverDiseaseComorbidPats)[1] <- "patient"

NASH_Probability %>% select(patient) %>% distinct() %>% anti_join(LiverDiseaseComorbidPats)

NASH_Probability %>% filter(Risk_pred_model.....predict.NASH_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() %>% anti_join(LiverDiseaseComorbidPats)

NASH_Probability %>% filter(Risk_pred_model.....predict.NASH_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() # (47% if not using the NASH as negative)

DIA_Probability %>% filter(Risk_pred_model.....predict.DIA_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() # (15% if not using the NASH as negative)

OBE_Probability %>% filter(Risk_pred_model.....predict.OBE_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() #  (11% if not using the NASH as negative)

NAFLD_Probability %>% filter(Risk_pred_model.....predict.NAFLD_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() #   (28% if not using the NASH as negative)

Random_Probability %>% filter(Risk_pred_model.....predict.Random_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() #  (9% if not using the NASH as negative)



# -----

# How events evolve over time and % share of physicians ------------
NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code, prov) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"


FirstNASHDx <- NASH_Events

NASH_Pats <- FirstNASHDx %>% select(patid)

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, claimed, code, prov) 
NASH_Events <- NASH_Events %>% filter(!is.na(claimed))
NASH_Events <- NASH_Pats %>% left_join(NASH_Events)

NASH_Events <- NASH_Events %>% left_join(FirstNASHDx)

NASH_Events$claimed <- as.Date(NASH_Events$claimed)
NASH_Events$FirstNASHDx <- as.Date(NASH_Events$FirstNASHDx)

NASH_Events <- NASH_Events %>% mutate(ElapsedTime = round(as.numeric((claimed-FirstNASHDx)/30.5)))

data.frame(NASH_Events %>% group_by(ElapsedTime) %>% count())

# data.frame(NASH_Events %>% group_by(ElapsedTime, patid) %>% count() %>% ungroup() %>% 
#   group_by(ElapsedTime) %>% summarise(n=round(mean(n))))

NASH_Events %>% ggplot(aes(ElapsedTime)) +
  geom_density(fill="darkcyan", colour="darkcyan", size=1, alpha=0.8) +
  xlim(-24,24)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After First NASH Dx")+
  ylab("Density of Events (Diagnoses & Procedures) \n")


NASH_Events %>% filter(grepl("D=",code)) %>% ggplot(aes(ElapsedTime)) +
  geom_density(fill="deepskyblue4", colour="deepskyblue4", size=1, alpha=0.8) +
  xlim(-24,24)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After First NASH Dx")+
  ylab("Density of Diagnoses \n")


NASH_Events %>% filter(grepl("R=",code)|grepl("P=",code)) %>% ggplot(aes(ElapsedTime)) +
  geom_density(fill="firebrick", colour="firebrick", size=1, alpha=0.8) +
  xlim(-24,24)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Month(s) Before / After First NASH Dx")+
  ylab("Density of Procedures \n")



# On month 0, we have 1324 Procedures and 9772 Dxs.

NASH_Events <- NASH_Events %>% drop_na()


NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")

NASH_Events <- NASH_Events %>% left_join(NASH_Event_Claims_Providers %>% select(prov, specialty)) %>% 
  filter(ElapsedTime>= (-12) & ElapsedTime<= (12))


NASH_Events <- NASH_Events %>% select(-c(weight, FirstNASHDx, claimed, code, prov))

length(unique(NASH_Events$specialty))


fwrite(NASH_Events, "Specialty_Over_Time_UniquePerPat.txt", sep="\t")


NASH_Events <- NASH_Events %>% group_by(ElapsedTime, specialty) %>% count()

fwrite(NASH_Events, "Specialty_Over_Time.txt", sep="\t")

unique(NASH_Events$specialty)
 

Specialty_Over_Time_UniquePerPat <- fread("Specialty_Over_Time_UniquePerPat.csv")



Specialty_Over_Time_UniquePerPat <- Specialty_Over_Time_UniquePerPat %>% filter(specialty!="UNKNOWN" & specialty!="FACILITY" & specialty!="FAICLITY" & specialty!="INDEPENDENT LABORATORY") 

Specialty_Over_Time_UniquePerPat <- Specialty_Over_Time_UniquePerPat %>% mutate(rank = ifelse(specialty=="GASTRO / HEPATO",1,
                                                          ifelse(specialty=="PATHOLOGY",2,
                                                                 ifelse(specialty=="RADIOLOGY"|specialty=="RAIDOLOGY",3,
                                                                        ifelse(specialty=="HEMATO / ONCO",4,
                                                                               ifelse(specialty=="CARDIOLOGY",5,
                                                                                      ifelse(specialty=="EMERGENCY MEDICINE",6,
                                                                                             ifelse(specialty=="INTERNAL MEDICINE",7,
                                                                                                    ifelse(specialty=="GP",8,
                                                                                                           ifelse(specialty=="SURGERY",9,
                                                                                                                  ifelse(specialty=="OTHER PHYSICIAN",10,
                                                                                                                         ifelse(specialty=="OTHER HCP",11,NA))))))))))))


Specialty_Over_Time_UniquePerPat <- Specialty_Over_Time_UniquePerPat %>% group_by(patid, ElapsedTime) %>% filter(rank==min(rank)) %>% slice(1)

Specialty_Over_Time_UniquePerPat <- Specialty_Over_Time_UniquePerPat %>% ungroup() %>% group_by(ElapsedTime, specialty) %>% count()

Specialty_Over_Time_UniquePerPat <- Specialty_Over_Time_UniquePerPat %>% ungroup() %>% group_by(ElapsedTime) %>% mutate(TOTAL = sum(n))

temp2 <- Specialty_Over_Time_UniquePerPat %>% mutate(share = n/TOTAL) %>%
  select(-c(n, TOTAL)) %>% spread(key=ElapsedTime, value=share)

fwrite(temp2, "Specialty_Over_Time_UniquePerPat_Wide.txt", sep="\t")

# --------------

# Create a matrix with the evolution of all labs, labs above threshold, any Liver Dx, gastro visit, over time ------------

# Labs over time above/below predicted threshold
Liver_Risk_OVerTime <- fread("Liver_Risk_OVerTime.txt")

NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, weight, code, claimed) %>% distinct()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
NASH_Events <- NASH_Events %>% filter(condition=="NASH")
NASH_Events <- NASH_Events %>% group_by(patid) %>% slice(1)
NASH_Events <- NASH_Events %>% select(patid, claimed)
names(NASH_Events)[2] <- "FirstNASHDx"
names(NASH_Events)[1] <- "patient"

FirstNASHDx <- NASH_Events


Liver_Risk_OVerTime <- FirstNASHDx %>% left_join(Liver_Risk_OVerTime)

Liver_Risk_OVerTime$claimed <- as.Date(Liver_Risk_OVerTime$claimed)
Liver_Risk_OVerTime$FirstNASHDx <- as.Date(Liver_Risk_OVerTime$FirstNASHDx)


Liver_Risk_OVerTime <- Liver_Risk_OVerTime %>% mutate(ElapsedTime = round(as.numeric((claimed-FirstNASHDx)/30.5)))

length(unique(Liver_Risk_OVerTime$patient))

Liver_Risk_OVerTime <- Liver_Risk_OVerTime %>% mutate(LabResults = ifelse(GLM_NASH_Prob>0.75,2,
                                                                          ifelse(GLM_NASH_Prob<0.75, 1,
                                                                                 ifelse(is.na(GLM_NASH_Prob), 0, NA))))



temp <- Liver_Risk_OVerTime %>% select(patient, ElapsedTime, LabResults) %>% 
  group_by(patient, ElapsedTime) %>% filter(LabResults == max(LabResults)) %>% slice(1)


temp <- temp %>% ungroup() %>% filter(ElapsedTime>= (-12) & ElapsedTime<=12) %>% spread(key=ElapsedTime, value=LabResults)

length(unique(temp$patient))

NASH_Pats <- Liver_Risk_OVerTime %>% select(patient) %>% distinct()

temp <- NASH_Pats %>% left_join(temp)


temp <- fread("PositiveVSNegative_labs_over_time.txt")

temp <- temp %>% replace(is.na(.), 0)

fwrite(temp, "PositiveVSNegative_labs_over_time.txt", sep="\t")



# Any (liver) labs over time

DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="ALT Level"|test=="AST Level"|test=="Platelet Count")
DANU_Measures <- DANU_Measures %>% select(patid, claimed)
DANU_Measures <- DANU_Measures %>% distinct()
names(DANU_Measures)[1] <- "patient"

DANU_Measures <- FirstNASHDx %>% left_join(DANU_Measures)

length(unique(DANU_Measures$patient))

DANU_Measures$claimed <- as.Date(DANU_Measures$claimed)
DANU_Measures$FirstNASHDx <- as.Date(DANU_Measures$FirstNASHDx)

DANU_Measures <- DANU_Measures %>% mutate(ElapsedTime = round(as.numeric((claimed-FirstNASHDx)/30.5)))

DANU_Measures <- DANU_Measures %>% select(-c(FirstNASHDx, claimed)) %>% distinct()

DANU_Measures <- DANU_Measures %>% mutate(AnyLabs = ifelse(is.na(ElapsedTime),0,1))

DANU_Measures <- DANU_Measures %>% ungroup() %>% filter(ElapsedTime>= (-12) & ElapsedTime<=12) %>% spread(key=ElapsedTime, value=AnyLabs)


DANU_Measures <- NASH_Pats %>% left_join(DANU_Measures)


DANU_Measures <- DANU_Measures %>% replace(is.na(.), 0)


fwrite(DANU_Measures, "Any_Labs_Liver_over_time.txt", sep="\t")






# Any labs results over time

NASH_Extract_Claims_Lab_Results <- fread("NASH Extract Claims Lab Results.txt")
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(patid, fst_dt)
NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% distinct()
names(NASH_Extract_Claims_Lab_Results)[1] <- "patient"
names(NASH_Extract_Claims_Lab_Results)[2] <- "claimed"


NASH_Extract_Labs <- fread("NASH Extract Labs.txt")
NASH_Extract_Labs <- NASH_Extract_Labs %>% select(patid, result_date) %>% distinct()
names(NASH_Extract_Labs)[1] <- "patient"
names(NASH_Extract_Labs)[2] <- "claimed"


NASH_Extract_NLP_Measurements <- fread("NASH Extract NLP Measurements.txt")
NASH_Extract_NLP_Measurements <- NASH_Extract_NLP_Measurements %>% select(patid, note_date)
names(NASH_Extract_NLP_Measurements)[1] <- "patient"
names(NASH_Extract_NLP_Measurements)[2] <- "claimed"


NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% bind_rows(NASH_Extract_Labs) %>% bind_rows(NASH_Extract_NLP_Measurements) %>% distinct()


NASH_Extract_Claims_Lab_Results <- FirstNASHDx %>% left_join(NASH_Extract_Claims_Lab_Results)

length(unique(NASH_Extract_Claims_Lab_Results$patient))

NASH_Extract_Claims_Lab_Results$claimed <- as.Date(NASH_Extract_Claims_Lab_Results$claimed)
NASH_Extract_Claims_Lab_Results$FirstNASHDx <- as.Date(NASH_Extract_Claims_Lab_Results$FirstNASHDx)

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(ElapsedTime = round(as.numeric((claimed-FirstNASHDx)/30.5)))


NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% select(-c(FirstNASHDx, claimed)) %>% distinct()

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% mutate(AnyResults = ifelse(is.na(ElapsedTime),0,1))

NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% ungroup() %>% filter(ElapsedTime>= (-12) & ElapsedTime<=12) %>% spread(key=ElapsedTime, value=AnyResults)


NASH_Extract_Claims_Lab_Results <-  FirstNASHDx %>% select(patient) %>% left_join(NASH_Extract_Claims_Lab_Results)


NASH_Extract_Claims_Lab_Results <- NASH_Extract_Claims_Lab_Results %>% replace(is.na(.), 0)

sum(NASH_Extract_Claims_Lab_Results==1) # 29216

fwrite(NASH_Extract_Claims_Lab_Results, "Any_LabsResults_over_time.txt", sep="\t")








# Any Liver Dx over time
NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, claimed, code) %>% distinct()
NASH_Events <- NASH_Events %>% drop_na()

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Diagnosis_Codes <- NASH_Diagnosis_Codes %>% select(code, diagnosis) %>% filter(diagnosis == "Liver Disease")

LiverDisease_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes) %>% drop_na() %>% select(-code)

names(LiverDisease_Events)[1] <- "patient"

LiverDisease_Events <- NASH_Pats %>% left_join(LiverDisease_Events) 

LiverDisease_Events$claimed <- as.Date(LiverDisease_Events$claimed)
FirstNASHDx$FirstNASHDx <- as.Date(FirstNASHDx$FirstNASHDx)

LiverDisease_Events <- FirstNASHDx %>% left_join(LiverDisease_Events) %>% mutate(ElapsedTime = round(as.numeric((claimed-FirstNASHDx)/30.5)))

LiverDisease_Events <- LiverDisease_Events %>% select(-c(FirstNASHDx, claimed))

LiverDisease_Events <- LiverDisease_Events %>% mutate(diagnosis = ifelse(diagnosis=="Liver Disease",1,0))

LiverDisease_Events <- LiverDisease_Events %>% filter(ElapsedTime >= (-12) & ElapsedTime <= 12)

LiverDisease_Events <- LiverDisease_Events %>% distinct() %>% ungroup()  %>% spread(key=ElapsedTime, value = diagnosis)


LiverDisease_Events <- LiverDisease_Events %>% replace(is.na(.), 0)


fwrite(LiverDisease_Events, "LiverDisease_Events_over_time.txt", sep="\t")






# Seen any gastro over time
NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(patid, claimed, prov) %>% distinct()
NASH_Events <- NASH_Events %>% drop_na()


NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% filter(grepl("GASTRO", specialty) | grepl("HEPATO", specialty) )

GastroEvents <- NASH_Events %>% left_join(NASH_Event_Claims_Providers)

GastroEvents <- GastroEvents %>% drop_na()
names(GastroEvents)[1] <- "patient"

GastroEvents <- NASH_Pats %>% left_join(GastroEvents)

GastroEvents <- FirstNASHDx %>% left_join(GastroEvents)

GastroEvents$FirstNASHDx <- as.Date(GastroEvents$FirstNASHDx)
GastroEvents$claimed <- as.Date(GastroEvents$claimed)

GastroEvents <- GastroEvents %>% mutate(ElapsedTime = round(as.numeric((claimed-FirstNASHDx)/30.5)))

GastroEvents <- GastroEvents %>% select(-prov)
GastroEvents <- GastroEvents %>% select(-c(FirstNASHDx, claimed))
GastroEvents$specialty <- 1

GastroEvents <- GastroEvents %>% filter(ElapsedTime >= (-12) & ElapsedTime <= 12)

GastroEvents <- GastroEvents %>% ungroup() %>% distinct() %>% spread(key=ElapsedTime, value=specialty)

GastroEvents <- GastroEvents %>% replace(is.na(.), 0)

GastroEvents <- NASH_Pats %>% left_join(GastroEvents)

GastroEvents <- GastroEvents %>% replace(is.na(.), 0)

fwrite(GastroEvents, "GastroEvents_Physician_over_time.txt", sep="\t")



# Pool them all together

LiverDisease_Events <- fread("LiverDisease_Events_over_time.txt", header = T)
GastroEvents <- fread("GastroEvents_Physician_over_time.txt", header = T)
Any_LabsResults_over_time <- fread("Any_LabsResults_over_time.txt", header = T)
#Any_labs <- fread("Any_Labs_Liver_over_time.txt", header = T)



EmptyDF <- data.frame(matrix(ncol = 26, nrow = 9772))
EmptyDF$X1 <- LiverDisease_Events$patient
names(EmptyDF)[1] <- "patient"

colnames(EmptyDF) <- colnames(LiverDisease_Events)

EmptyDF <- EmptyDF %>% mutate(across(everything(), as.character))



for(j in 1:nrow(EmptyDF))
{
  cat(j)
  cat("\n")
  for(k in 2:ncol(EmptyDF))
  {
    EmptyDF[j,k] <- paste(LiverDisease_Events[j,k, with=FALSE], GastroEvents[j,k, with=FALSE], Any_LabsResults_over_time[j,k, with=FALSE], sep="-")
  }
}


fwrite(EmptyDF, "Merged_AnyResultsGlobal.txt", sep="\t")

# ----------

# How many patients have any liver test over the past 6 years ? And on the exact same day ? ----------------------

NASH_Pats <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Pats <- NASH_Pats %>% select(patient) # 9772


NAFLD_Pats <- fread("NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Pats <- NAFLD_Pats %>% select(patient)
NAFLD_Pats <- NAFLD_Pats %>% anti_join(NASH_Pats) # 101794

DIA_Pats <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Pats <- DIA_Pats %>% select(patient)
DIA_Pats <- DIA_Pats %>% anti_join(NASH_Pats)  %>% anti_join(NAFLD_Pats) # 290347


OBE_Pats <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Pats <- OBE_Pats %>% select(patient)
OBE_Pats <- OBE_Pats %>% anti_join(NASH_Pats)  %>% anti_join(NAFLD_Pats) # 675316


DANU_Measures <- fread("DANU Measures.txt", integer64 = "character", stringsAsFactors = F)
DANU_Measures <- DANU_Measures %>% filter(test=="Platelet Count" | test=="AST Level" | test=="ALT Level")
names(DANU_Measures)[1] <- "patient"

min(DANU_Measures$claimed) # "2015-10-01"
max(DANU_Measures$claimed) # "2021-04-30"

NASH_Pats <- NASH_Pats %>% inner_join(DANU_Measures)
NAFLD_Pats <- NAFLD_Pats %>% inner_join(DANU_Measures)
DIA_Pats <- DIA_Pats %>% inner_join(DANU_Measures)
OBE_Pats <- OBE_Pats %>% inner_join(DANU_Measures)

length(unique(NASH_Pats$patient)) #5320   54%
length(unique(NAFLD_Pats$patient)) #54677 54%
length(unique(DIA_Pats$patient)) #152444   53%
length(unique(OBE_Pats$patient)) #360431   53%



NASH_Pats <- NASH_Pats %>% select(patient, test, claimed, value)
NAFLD_Pats <- NAFLD_Pats %>% select(patient, test, claimed, value)
DIA_Pats <- DIA_Pats %>% select(patient, test, claimed, value)
OBE_Pats <- OBE_Pats %>% select(patient, test, claimed, value)



# NASH only
NASH_Pats_AST <- NASH_Pats %>% filter(test=="AST Level")
NASH_Pats_ALT <- NASH_Pats %>% filter(test=="ALT Level")
NASH_Pats_Platelets <- NASH_Pats %>% filter(test=="Platelet Count")

names(NASH_Pats_AST)[4] <-"AST"
names(NASH_Pats_ALT)[4] <-"ALT"
names(NASH_Pats_Platelets)[4] <-"Platelets"

NASH_Pats_AST <- NASH_Pats_AST %>% select(1,3,4)
NASH_Pats_ALT <- NASH_Pats_ALT %>% select(1,3,4)
NASH_Pats_Platelets <- NASH_Pats_Platelets %>% select(1,3,4)

NASH_Pats <- NASH_Pats_AST %>% full_join(NASH_Pats_ALT, by = c("patient", "claimed")) %>% full_join(NASH_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()



# NAFLD only
NAFLD_Pats_AST <- NAFLD_Pats %>% filter(test=="AST Level")
NAFLD_Pats_ALT <- NAFLD_Pats %>% filter(test=="ALT Level")
NAFLD_Pats_Platelets <- NAFLD_Pats %>% filter(test=="Platelet Count")

names(NAFLD_Pats_AST)[4] <-"AST"
names(NAFLD_Pats_ALT)[4] <-"ALT"
names(NAFLD_Pats_Platelets)[4] <-"Platelets"

NAFLD_Pats_AST <- NAFLD_Pats_AST %>% select(1,3,4)
NAFLD_Pats_ALT <- NAFLD_Pats_ALT %>% select(1,3,4)
NAFLD_Pats_Platelets <- NAFLD_Pats_Platelets %>% select(1,3,4)

NAFLD_Pats <- NAFLD_Pats_AST %>% full_join(NAFLD_Pats_ALT, by = c("patient", "claimed")) %>% full_join(NAFLD_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()



# DIA only
DIA_Pats_AST <- DIA_Pats %>% filter(test=="AST Level")
DIA_Pats_ALT <- DIA_Pats %>% filter(test=="ALT Level")
DIA_Pats_Platelets <- DIA_Pats %>% filter(test=="Platelet Count")

names(DIA_Pats_AST)[4] <-"AST"
names(DIA_Pats_ALT)[4] <-"ALT"
names(DIA_Pats_Platelets)[4] <-"Platelets"

DIA_Pats_AST <- DIA_Pats_AST %>% select(1,3,4)
DIA_Pats_ALT <- DIA_Pats_ALT %>% select(1,3,4)
DIA_Pats_Platelets <- DIA_Pats_Platelets %>% select(1,3,4)

DIA_Pats <- DIA_Pats_AST %>% full_join(DIA_Pats_ALT, by = c("patient", "claimed")) %>% full_join(DIA_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()



# OBE only
OBE_Pats_AST <- OBE_Pats %>% filter(test=="AST Level")
OBE_Pats_ALT <- OBE_Pats %>% filter(test=="ALT Level")
OBE_Pats_Platelets <- OBE_Pats %>% filter(test=="Platelet Count")

names(OBE_Pats_AST)[4] <-"AST"
names(OBE_Pats_ALT)[4] <-"ALT"
names(OBE_Pats_Platelets)[4] <-"Platelets"

OBE_Pats_AST <- OBE_Pats_AST %>% select(1,3,4)
OBE_Pats_ALT <- OBE_Pats_ALT %>% select(1,3,4)
OBE_Pats_Platelets <- OBE_Pats_Platelets %>% select(1,3,4)

OBE_Pats <- OBE_Pats_AST %>% full_join(OBE_Pats_ALT, by = c("patient", "claimed")) %>% full_join(OBE_Pats_Platelets, by = c("patient", "claimed"))  %>% drop_na()


length(unique(NASH_Pats$patient)) # 4543   #46%
length(unique(NAFLD_Pats$patient)) # 46643  #46%
length(unique(DIA_Pats$patient)) # 123584   #43%
length(unique(OBE_Pats$patient)) # 285579   #42%





FIB4_Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt")
Random_Pats <- FIB4_Random_Pats %>% select(patient)




Rand_pts_Lab_Results_lst5y <- fread("Rand_pts_Lab_Results_lst5y.txt")

DANU_Measure_Codes <- fread("DANU Measure Codes.txt")
DANU_Measure_Codes <- DANU_Measure_Codes %>% select(test, code)

Rand_pts_Lab_Results_lst5y <- Rand_pts_Lab_Results_lst5y %>% left_join(DANU_Measure_Codes, by=c("loinc_cd"="code"))

length(unique(Rand_pts_Lab_Results_lst5y$ptid)) # 20000   # 100%

Rand_AST <- Rand_pts_Lab_Results_lst5y %>% filter(test == "AST Level") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_AST)[3] <- "AST"

Rand_ALT <- Rand_pts_Lab_Results_lst5y %>% filter(test == "ALT Level") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_ALT)[3] <- "ALT"

Rand_Platelets <- Rand_pts_Lab_Results_lst5y %>% filter(test == "Platelet Count") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_Platelets)[3] <- "Platelets"


Rand_pats_Labs <- Rand_AST %>% full_join(Rand_ALT) %>% full_join(Rand_Platelets) %>% drop_na() %>% distinct()

names(Rand_pats_Labs)[1] <- "patient"
names(Rand_pats_Labs)[2] <- "claimed"


length(unique(Rand_pats_Labs$patient)) # 15886    # 79%

length(unique(FIB4_Random_Pats$patient)) # 3870    # 19%




Rand_pts_Lab_Results_18p_lst5y <- fread("10kRand_pts18+_lab_Results_lst5y.txt")

DANU_Measure_Codes <- fread("DANU Measure Codes.txt")
DANU_Measure_Codes <- DANU_Measure_Codes %>% select(test, code)

Rand_pts_Lab_Results_18p_lst5y <- Rand_pts_Lab_Results_18p_lst5y %>% left_join(DANU_Measure_Codes, by=c("loinc_cd"="code"))

length(unique(Rand_pts_Lab_Results_18p_lst5y$ptid)) # 860 with any test (from 10k)    9%


Rand_AST <- Rand_pts_Lab_Results_18p_lst5y %>% filter(test == "AST Level") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_AST)[3] <- "AST"

Rand_ALT <- Rand_pts_Lab_Results_18p_lst5y %>% filter(test == "ALT Level") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_ALT)[3] <- "ALT"

Rand_Platelets <- Rand_pts_Lab_Results_18p_lst5y %>% filter(test == "Platelet Count") %>% select(ptid, fst_dt, rslt_nbr)
names(Rand_Platelets)[3] <- "Platelets"


Rand_pats_Labs <- Rand_AST %>% full_join(Rand_ALT) %>% full_join(Rand_Platelets) %>% drop_na() %>% distinct()

names(Rand_pats_Labs)[1] <- "patient"
names(Rand_pats_Labs)[2] <- "claimed"

length(unique(Rand_pats_Labs$patient)) # 688    # 7%

# ----------
# Compare patients with first NASH Dx gastro/hepato vs GP/IM  ---------------
NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% filter(condition=="NASH") %>% 
  group_by(patid) %>% slice(1)

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Event_Claims_Providers <- NASH_Events %>% left_join(NASH_Event_Claims_Providers)

NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% filter(specialty == "FAMILY PRACTICE/CAPITATED CLINIC" | specialty == "GENERAL PRACTITIONER" |
                                                                        specialty == "FAMILY PRACTICE SPECIALIST" | specialty == "HEPATOLOGY-LIVER DISEASE" |
                                                                        specialty == "FAMILY PRACTITIONER" | specialty == "FAMILY PRACTICE/GENERAL PRACTICE" |
                                                                        specialty == "GASTROENTEROLOGIST"  | specialty == "INTERNAL MED PEDIATRICS" | 
                                                                        specialty ==  "INTERNIST/GENERAL INTERNIST" | specialty == "INTERNAL MEDICINE SPECIALIST")


NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(patid, specialty) %>% 
  mutate(spciality = ifelse(specialty== "GASTROENTEROLOGIST" | specialty == "HEPATOLOGY-LIVER DISEASE", "GASTRO / HEPATO", "GP / IM"))

NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(patid, spciality)

First_Specialty_Group <- NASH_Event_Claims_Providers

fwrite(First_Specialty_Group, "First_Specialty_Group.txt", sep="\t")

# Any difference in age?

DANU_Demographics <- fread("DANU Demographics.txt")

DANU_Demographics <- DANU_Demographics %>% select(patid, weight, age)

First_Specialty_Group <- First_Specialty_Group %>% left_join(DANU_Demographics)

First_Specialty_Group %>% group_by(spciality) %>% summarise(n=weighted.mean(age, weight))

# spciality           n
# <chr>           <dbl>
# 1 GASTRO / HEPATO  58.3
# 2 GP / IM          56.6

First_Specialty_Group %>% group_by(spciality) %>% summarise(n=weighted.median(age, weight))

# spciality           n
# <chr>           <dbl>
# 1 GASTRO / HEPATO  59.5
# 2 GP / IM          57.5

# Any difference in max FIB4?

FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")

FIB4_NASH_Pats <- FIB4_NASH_Pats %>% group_by(patient) %>% filter(fibrosis4 == max(fibrosis4)) %>% slice(1)
FIB4_NASH_Pats <- FIB4_NASH_Pats %>% select(patient, fibrosis4)

First_Specialty_Group  %>% left_join(FIB4_NASH_Pats, by=c("patid"="patient")) %>% filter(!is.na(fibrosis4)) %>% 
  group_by(spciality) %>% summarise(n=weighted.mean(fibrosis4, weight))

# spciality           n
# <chr>           <dbl>
# 1 GASTRO / HEPATO  5.85
# 2 GP / IM          3.91

First_Specialty_Group  %>% left_join(FIB4_NASH_Pats, by=c("patid"="patient")) %>% filter(!is.na(fibrosis4)) %>% 
  group_by(spciality) %>% summarise(n=weighted.median(fibrosis4, weight))

# spciality           n
# <chr>           <dbl>
# 1 GASTRO / HEPATO  1.74
# 2 GP / IM          1.44


# GLP1 Experience

NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight,  drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(drug_class == "GLP1 Injectable" | drug_class == "GLP1 Oral" ) %>% 
  select(patient, drug_class) %>% distinct()

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient) %>% distinct() %>% mutate(GLP1_Exp = "GLP1_Exp")

First_Specialty_Group %>% left_join(NASH_Drug_Histories, by=c("patid"="patient")) %>%
  group_by(spciality, GLP1_Exp) %>% summarise(n=sum(weight))

# 14% vs 11%
# spciality       GLP1_Exp       n
# <chr>           <chr>      <dbl>
# 1 GASTRO / HEPATO GLP1_Exp  42495.
# 2 GASTRO / HEPATO NA       251314.
# 3 GP / IM         GLP1_Exp  41084.
# 4 GP / IM         NA       328475.

# Drug Experience

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient) %>% distinct() %>% mutate(Treat_Exp = "Treat_Exp")

First_Specialty_Group %>% left_join(NASH_Drug_Histories, by=c("patid"="patient")) %>%
  group_by(spciality, Treat_Exp) %>% summarise(n=sum(weight))

# 73% vs 70%
# spciality       Treat_Exp       n
# <chr>           <chr>       <dbl>
# 1 GASTRO / HEPATO Treat_Exp 213058.
# 2 GASTRO / HEPATO NA         80751.
# 3 GP / IM         Treat_Exp 259026.
# 4 GP / IM         NA        110534.


# Biopsy experience
NASH_Events <- fread("NASH Events.txt")
Dx_code <- fread("NASH Diagnosis Codes.txt")
Dx_code <- Dx_code %>% select(code, condition, source, type, description)
NASH_Events <- NASH_Events %>% left_join(Dx_code)
NASH_Pats <- NASH_Events %>% filter(condition=="NASH") %>% select(patid) %>% distinct()
NASH_Pats <- NASH_Pats %>% left_join(NASH_Events)
NASH_Pats %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #1339983
Biopsy_Pats <- NASH_Pats %>% select(patid, weight, condition) %>% distinct() %>% filter(condition=="Liver Biopsy") %>% select(patid) %>% distinct()
Biopsy_Pats$Biopsy <- "Biopsy"

First_Specialty_Group %>% left_join(Biopsy_Pats) %>%
  group_by(spciality, Biopsy) %>% summarise(n=sum(weight))

# 16% vs 5%
# spciality       Biopsy       n
# <chr>           <chr>    <dbl>
# 1 GASTRO / HEPATO Biopsy  46621.
# 2 GASTRO / HEPATO NA     247188.
# 3 GP / IM         Biopsy  19550.
# 4 GP / IM         NA     350009.


# Surgery exp


NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease))

NASH_Drug_Histories <- NASH_Drug_Histories %>%
  mutate(SurgeryStatus = 
           ifelse(grepl("79", Treat)|grepl("80", Treat)|grepl("81", Treat)|grepl("82", Treat), "Surgery", "No"))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month, SurgeryStatus)

SurgeryPats <- NASH_Drug_Histories %>% filter(SurgeryStatus== "Surgery") %>% select(patient) %>% distinct()

SurgeryPats$Surgery <- "Surgery"

First_Specialty_Group %>% left_join(SurgeryPats, by=c("patid"="patient")) %>%
  group_by(spciality, Surgery) %>% summarise(n=sum(weight))

# 4% vs 3%
# spciality       Surgery       n
# <chr>           <chr>     <dbl>
# 1 GASTRO / HEPATO Surgery  10691.
# 2 GASTRO / HEPATO NA      283118.
# 3 GP / IM         Surgery  11410.
# 4 GP / IM         NA      358150.
# ---------
# Check for drug experience ever in Diabetes (treat-exp) VS Diabetes predicted as "high-risk 95%"  -------------

DIA_Drug_Histories     <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
# Treatment_exp_Vector   <-fread("Treatment_exp_Vector.txt")
# sum(Treatment_exp_Vector$weight) #30625690
# DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)

DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)


DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease, Month))

names(DIA_Drug_Histories)[3] <- "molecule"
DIA_Drug_Histories$molecule <- as.character(DIA_Drug_Histories$molecule)

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_group))

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight) %>% distinct()

DIA_Drug_Histories <- data.frame(DIA_Drug_Histories %>% group_by(drug_group) %>% 
                                   summarise(sum_weights = sum(as.numeric(weight))))


# drug_group sum_weights
# 1    Antidiabetic  11755753.3
# 2       Biguanide  23551992.6
# 3            DPP4   6119130.3
# 4 GLP1 Injectable   7043706.6
# 5       GLP1 Oral    264137.4
# 6         Insulin  22903425.0
# 7           SGLT2   5808411.5


DIA_Drug_Histories     <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Pats_Score95_2plus     <- fread("DIA_Pats_Score95_2plus.txt", integer64 = "character", stringsAsFactors = F)

DIA_Drug_Histories <- DIA_Pats_Score95_2plus %>% left_join(DIA_Drug_Histories)

sum(DIA_Drug_Histories$weight) #2053529 



DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)


DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease, Month))

names(DIA_Drug_Histories)[3] <- "molecule"
DIA_Drug_Histories$molecule <- as.character(DIA_Drug_Histories$molecule)

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_group))

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight) %>% distinct()

DIA_Drug_Histories <- data.frame(DIA_Drug_Histories %>% group_by(drug_group) %>% 
                                   summarise(sum_weights = sum(as.numeric(weight))))


drug_group sum_weights
1    Antidiabetic   601017.87
2       Biguanide  1090869.63
3            DPP4   309301.71
4 GLP1 Injectable   362869.32
5       GLP1 Oral    10219.14
6         Insulin  1320002.33
7           SGLT2   290004.49

# ----------
# Check for drug experience ever in Obesity (treat-exp) VS Obesity predicted as "high-risk 95%"  -------------

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
sum(OBE_Drug_Histories$weight) # 106469049

DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)


OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
OBE_Drug_Histories <- separate_rows(OBE_Drug_Histories)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-c(disease, Month))

names(OBE_Drug_Histories)[3] <- "molecule"
OBE_Drug_Histories$molecule <- as.character(OBE_Drug_Histories$molecule)

OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_group))

OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient, weight) %>% distinct()

OBE_Drug_Histories <- data.frame(OBE_Drug_Histories %>% group_by(drug_group) %>% 
                                   summarise(sum_weights = sum(as.numeric(weight))))


# drug_group sum_weights
# 1     Antiobesity  2723751.27
# 2 GLP1 Injectable   263843.94
# 3       GLP1 Oral    24815.68
# 4         Surgery   746366.92


OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Pats_Score95_2plus     <- fread("OBE_Pats_Score95_2plus.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Pats_Score95_2plus %>% left_join(OBE_Drug_Histories)

sum(OBE_Drug_Histories$weight) #2875324 



DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)


OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
OBE_Drug_Histories <- separate_rows(OBE_Drug_Histories, Treat, sep = ",", convert=T )

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-c(disease, Month))

names(OBE_Drug_Histories)[3] <- "molecule"
OBE_Drug_Histories$molecule <- as.character(OBE_Drug_Histories$molecule)

OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_group))

OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient, weight) %>% distinct()

OBE_Drug_Histories <- data.frame(OBE_Drug_Histories %>% group_by(drug_group) %>% 
                                   summarise(sum_weights = sum(as.numeric(weight))))



# drug_group sum_weights
# 1     Antiobesity    61933.83
# 2 GLP1 Injectable     7133.32
# 3       GLP1 Oral      842.95
# 4         Surgery    38630.23

# --------
# Compare lab results between diabetes/obesity and high risk diabetes/obesity --------------
# Get Pats with all labs on the same date 
NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt") 

NAFLD <- NAFLD_Pats %>% select(patient)
DIA_Pats <- DIA_Pats %>% anti_join(NAFLD)
OBE_Pats <- OBE_Pats %>% anti_join(NAFLD)

NASH_Pats$group <- "NASH"
DIA_Pats$group <- "DIA"
OBE_Pats$group <- "OBE"
NAFLD_Pats$group <- "NAFLD"
Random_Pats$group <- "Random Sample"

DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")

DIA_Pats_HighRisk <- DIA_Pats_Score95_2plus %>% left_join(DIA_Pats)
OBE_Pats_HighRisk <- OBE_Pats_Score95_2plus %>% left_join(OBE_Pats)


DIA_Pats <- DIA_Pats %>% anti_join(DIA_Pats_Score95_2plus)
OBE_Pats <- OBE_Pats %>% anti_join(OBE_Pats_Score95_2plus)



DIA_Pats_HighRisk$group <- "High Risk DIA"
OBE_Pats_HighRisk$group <- "High Risk OBE"

Temp_Ridges <- DIA_Pats %>% bind_rows(DIA_Pats_HighRisk) %>% bind_rows(OBE_Pats) %>% bind_rows(OBE_Pats_HighRisk)

Temp_Ridges %>% filter(AST>0 & AST<200) %>% ggplot(aes(x = AST, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n AST (IU/L)") + ylab("Disease Group \n")

Temp_Ridges %>% filter(ALT>0 & ALT<200) %>% ggplot(aes(x = ALT, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n ALT (IU/L)") + ylab("Disease Group \n")


Temp_Ridges %>% filter(Platelets>0 & Platelets<600) %>% ggplot(aes(x = Platelets, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "A", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Platelet Count (x10^3 / uL)") + ylab("Disease Group \n")


Temp_Ridges %>% filter(fibrosis4<10) %>% ggplot(aes(x = fibrosis4, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n FIB4") + ylab("Disease Group \n")


# -------
# NASH patients into NASH type (lab based): Drug penetrance in each group -----------
FIB4_Bucket_Fibrosis <- fread("FIB4_Bucket_Fibrosis.txt")

FIB4_Bucket_Fibrosis %>% group_by(FIB4_Bucket) %>% count()

# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()



NASH_Drug_Histories <- FIB4_Bucket_Fibrosis %>% left_join(NASH_Drug_Histories)

NASH_Drug_Histories %>% select(patient, FIB4_Bucket, weight) %>% distinct() %>% group_by(FIB4_Bucket) %>%
  summarise(n=sum(weight, na.rm = T))

# FIB4_Bucket          n
# <chr>            <dbl>
#   1 Fibrosis       127082.
# 2 NASH-Cirrhosis  83720.
# 3 NASH-only      174136.

data.frame(NASH_Drug_Histories %>% select(patient, weight, FIB4_Bucket, drug_group) %>% 
             distinct() %>% group_by(FIB4_Bucket, drug_group) %>%  summarise(n=sum(weight)))

FIB4_Bucket       drug_group         n
1        Fibrosis  Anticholesterol 100685.94
2        Fibrosis     Antidiabetic  77717.47
3        Fibrosis      Antiobesity  12550.16
4        Fibrosis  GLP1 Injectable  21651.79
5        Fibrosis        GLP1 Oral   1209.35
6        Fibrosis Hepatoprotective   3716.77
7        Fibrosis  Hospitalization  13220.98
8        Fibrosis             <NA>        NA
9  NASH-Cirrhosis  Anticholesterol  62137.39
10 NASH-Cirrhosis     Antidiabetic  62299.47
11 NASH-Cirrhosis      Antiobesity   4682.84
12 NASH-Cirrhosis  GLP1 Injectable  17630.06
13 NASH-Cirrhosis        GLP1 Oral   1024.57
14 NASH-Cirrhosis Hepatoprotective   9283.10
15 NASH-Cirrhosis  Hospitalization  16304.69
16 NASH-Cirrhosis             <NA>        NA
17      NASH-only  Anticholesterol 127795.71
18      NASH-only     Antidiabetic  96296.38
19      NASH-only      Antiobesity  27472.70
20      NASH-only  GLP1 Injectable  27459.32
21      NASH-only        GLP1 Oral   1807.64
22      NASH-only Hepatoprotective   6828.00
23      NASH-only  Hospitalization  19465.27
24      NASH-only             <NA>        NA


NASH_Drug_Histories %>% filter(!is.na(weight)) %>% select(patient, FIB4_Bucket) %>% distinct() %>%
  group_by(FIB4_Bucket) %>% count()

FIB4_Bucket        n
<chr>          <int>
  1 Fibrosis         967
2 NASH-Cirrhosis   618
3 NASH-only       1234

# ------
# Proportion of Fibrosis / NASH only in DIA/OBE/NAFLD --------

Diabetes_Pats_FIB4 <- fread("FIB4_Diabetes_Pats.txt")
NAFLD_Pats_FIB4 <- fread("FIB4_NAFLD_Pats.txt")
Obesity_Pats_FIB4 <- fread("FIB4_Obesity_Pats.txt")



NAFLD <- NAFLD_Pats_FIB4 %>% select(patient)
Diabetes_Pats_FIB4 <- Diabetes_Pats_FIB4 %>% anti_join(NAFLD)
Obesity_Pats_FIB4 <- Obesity_Pats_FIB4 %>% anti_join(NAFLD)



Diabetes_Pats_FIB4 <- Diabetes_Pats_FIB4 %>% group_by(patient) %>% filter(fibrosis4 == max(fibrosis4)) %>% slice(1)
Diabetes_Pats_FIB4 <- Diabetes_Pats_FIB4 %>% select(patient, age, fibrosis4)

Diabetes_Pats_FIB4 <- Diabetes_Pats_FIB4 %>% mutate(FIB4_Bucket = ifelse(fibrosis4>1.3&age<60, "Fibrosis",
                                                                         ifelse(fibrosis4>1.88&age<70,"Fibrosis",
                                                                                ifelse(fibrosis4>1.95&age>70,"Fibrosis", "NASH-only"))))

Diabetes_Pats_FIB4 %>% group_by(FIB4_Bucket) %>% count()

# FIB4_Bucket     n
# <chr>       <int>
# 1 Fibrosis    41585
# 2 NASH-only   81859


# 34 / 66



Obesity_Pats_FIB4 <- fread("FIB4_Obesity_Pats.txt")
Obesity_Pats_FIB4 <- Obesity_Pats_FIB4 %>% group_by(patient) %>% filter(fibrosis4 == max(fibrosis4)) %>% slice(1)
Obesity_Pats_FIB4 <- Obesity_Pats_FIB4 %>% select(patient, age, fibrosis4)

Obesity_Pats_FIB4 <- Obesity_Pats_FIB4 %>% mutate(FIB4_Bucket = ifelse(fibrosis4>1.3&age<60, "Fibrosis",
                                                                       ifelse(fibrosis4>1.88&age<70,"Fibrosis",
                                                                              ifelse(fibrosis4>1.95&age>70,"Fibrosis", "NASH-only"))))

Obesity_Pats_FIB4 %>% group_by(FIB4_Bucket) %>% count()


FIB4_Bucket      n
<chr>        <int>
  1 Fibrosis     73921
2 NASH-only   233460

# 24 / 76




NAFLD_Pats_FIB4 <- NAFLD_Pats_FIB4 %>% group_by(patient) %>% filter(fibrosis4 == max(fibrosis4)) %>% slice(1)
NAFLD_Pats_FIB4 <- NAFLD_Pats_FIB4 %>% select(patient, age, fibrosis4)

NAFLD_Pats_FIB4 <- NAFLD_Pats_FIB4 %>% mutate(FIB4_Bucket = ifelse(fibrosis4>1.3&age<60, "Fibrosis",
                                                                   ifelse(fibrosis4>1.88&age<70,"Fibrosis",
                                                                          ifelse(fibrosis4>1.95&age>70,"Fibrosis", "NASH-only"))))

NAFLD_Pats_FIB4 %>% group_by(FIB4_Bucket) %>% count()

FIB4_Bucket     n
<chr>       <int>
  1 Fibrosis    17192
2 NASH-only   29430


# 37 / 63
# -----------
# HbA1c and BMI for high risk Diabetes and Obesity ----------------------

# NASH Pats
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Pats <- NASH_Drug_Histories %>% select(patient)

# NAFLD Pats
NAFLD_Drug_Histories <- fread("NAFLD Drug Histories.txt")
NAFLD_Pats <- NAFLD_Drug_Histories %>% select(patient)


# High Risk Diabetes and Obesity patients

DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")

# Remove those whith NASH and NAFLD as well 

DIA_Pats_Score95_2plus <- DIA_Pats_Score95_2plus %>% anti_join(NASH_Pats) %>% anti_join(NAFLD_Pats)
OBE_Pats_Score95_2plus <- OBE_Pats_Score95_2plus %>% anti_join(NASH_Pats) %>% anti_join(NAFLD_Pats)

DANU_Measures_DIAOBE <- DANU_Measures_DIAOBE %>% anti_join(NASH_Pats) %>% anti_join(NAFLD_Pats)
DANU_Measures_DIAOBE <- DANU_Measures_DIAOBE %>% select(patient, weight, test, claimed, value)



# All Diabetes and obesity patients to compare againts 
# Excluding the high risk ones as well as those with NASH or NAFLD

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
DIA_Pats <- DIA_Drug_Histories %>% select(patient)
OBE_Pats <- OBE_Drug_Histories %>% select(patient)

DIA_Pats <- DIA_Pats %>% anti_join(DIA_Pats_Score95_2plus %>% select(patient) %>% distinct())
OBE_Pats <- OBE_Pats %>% anti_join(OBE_Pats_Score95_2plus %>% select(patient) %>% distinct())

DIA_Pats <- DIA_Pats %>% anti_join(NASH_Pats) %>% anti_join(NAFLD_Pats)
OBE_Pats <- OBE_Pats %>% anti_join(NASH_Pats) %>% anti_join(NAFLD_Pats)


DIA_Pats <- DIA_Pats %>% left_join(DANU_Measures_DIAOBE)
OBE_Pats <- OBE_Pats %>% left_join(DANU_Measures_DIAOBE)

DIA_Pats_Score95_2plus <- DIA_Pats_Score95_2plus %>% left_join(DANU_Measures_DIAOBE)
OBE_Pats_Score95_2plus <- OBE_Pats_Score95_2plus %>% left_join(DANU_Measures_DIAOBE)

rm(DANU_Measures_DIAOBE)

# using MAX HbA1c or MAX BMI per patient

# Diabetes (means)

DIA_Pats %>% filter(test == "HbA1c Level") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 7.50

DIA_Pats_Score95_2plus %>% filter(test == "HbA1c Level" )%>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 7.95

DIA_Pats %>% filter(test == "BMI") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 33.6

DIA_Pats_Score95_2plus %>% filter(test == "BMI" )%>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 35.8


# Obesity (means)

OBE_Pats %>% filter(test == "HbA1c Level") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 5.63

OBE_Pats_Score95_2plus %>% filter(test == "HbA1c Level" )%>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 5.72

OBE_Pats %>% filter(test == "BMI") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 31.8

OBE_Pats_Score95_2plus %>% filter(test == "BMI" )%>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% summarise(n=weighted.mean(value, weight)) # 33.2




# HbA1c Diabetes

DIA_Pats %>% filter(test == "HbA1c Level") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% mutate(group="All Diabetes") %>% 
  bind_rows(DIA_Pats_Score95_2plus %>% filter(test == "HbA1c Level" )%>% group_by(patient) %>% filter(value==max(value)) %>%
              slice(1) %>% ungroup() %>% mutate(group="High Liver Risk Diabetes")) %>%
  filter(value>4 & value<15) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Max HbA1c (%) per patient") + ylab("Diabetes Group \n")


# HbA1c Obesity

OBE_Pats %>% filter(test == "HbA1c Level") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% mutate(group="All Obesity") %>% 
  bind_rows(OBE_Pats_Score95_2plus %>% filter(test == "HbA1c Level" )%>% group_by(patient) %>% filter(value==max(value)) %>%
              slice(1) %>% ungroup() %>% mutate(group="High Liver Risk Obesity")) %>%
  filter(value>4 & value<7) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Max HbA1c (%) per patient") + ylab("Obesity Group \n")


# BMI Diabetes

DIA_Pats %>% filter(test == "BMI") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% mutate(group="All Diabetes") %>% 
  bind_rows(DIA_Pats_Score95_2plus %>% filter(test == "BMI" )%>% group_by(patient) %>% filter(value==max(value)) %>%
              slice(1) %>% ungroup() %>% mutate(group="High Liver Risk Diabetes")) %>%
  filter(value>15 & value<50) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "A", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Max BMI per patient") + ylab("Diabetes Group \n")


# BMI Obesity

OBE_Pats %>% filter(test == "BMI") %>% group_by(patient) %>% filter(value==max(value)) %>%
  slice(1) %>% ungroup() %>% mutate(group="All Obesity") %>% 
  bind_rows(OBE_Pats_Score95_2plus %>% filter(test == "BMI" )%>% group_by(patient) %>% filter(value==max(value)) %>%
              slice(1) %>% ungroup() %>% mutate(group="High Liver Risk Obesity")) %>%
  filter(value>25 & value<50) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "A", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Max BMI per patient") + ylab("Obesity Group \n")







# ---
# --------
# Compare lab results between NASH Dx with High Liver Risk vs NASH Dx Low Liver Risk vs High Liver Risk Other -----------

NASH_Pats_95ConfLiver_2plusHits <- fread("NASH_Pats_95ConfLiver_2plusHits.txt")
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
NAFLD_Pats_95ConfLiver_2plusHits <- fread("NAFLD_Pats_95ConfLiver_2plusHits.txt")



FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")

FIB4_Diabetes_Pats_HighRisk <- DIA_Pats_95ConfLiver_2plusHits %>% left_join(FIB4_Diabetes_Pats)
FIB4_Obesity_Pats_HighRisk <- OBE_Pats_95ConfLiver_2plusHits %>% left_join(FIB4_Obesity_Pats)
FIB4_NAFLD_Pats_HighRisk <- NAFLD_Pats_95ConfLiver_2plusHits %>% left_join(FIB4_NAFLD_Pats)

FIB4_Diabetes_Pats_LowRisk <- FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)
FIB4_Obesity_Pats_LowRisk <- FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)
FIB4_NAFLD_Pats_LowRisk <- FIB4_NAFLD_Pats %>% anti_join(NAFLD_Pats_95ConfLiver_2plusHits)




FIB4_NASH_Pats_HighRisk <- NASH_Pats_95ConfLiver_2plusHits %>% left_join(FIB4_NASH_Pats)

FIB4_NASH_Pats_AllRemaining <- FIB4_NASH_Pats %>% anti_join(NASH_Pats_95ConfLiver_2plusHits)


FIB4_NASH_Pats_HighRisk$group <- "NASH Dx - High Liver Risk Predicted"
FIB4_NASH_Pats_AllRemaining$group <- "NASH Dx - Low Liver Risk Predicted"

Other_Predicted_HighRisk <- FIB4_Diabetes_Pats_HighRisk %>% bind_rows(FIB4_Obesity_Pats_HighRisk) %>% bind_rows(FIB4_NAFLD_Pats_HighRisk)
Other_Predicted_HighRisk$group <- "No Dx - High Liver Risk Predicted"

Other_Predicted_LowRisk <- FIB4_Diabetes_Pats_LowRisk %>% bind_rows(FIB4_Obesity_Pats_LowRisk) %>% bind_rows(FIB4_NAFLD_Pats_LowRisk)
Other_Predicted_LowRisk$group <- "No Dx - Low Liver Risk Predicted"



FIB4_NASH_Pats_HighRisk %>% bind_rows(FIB4_NASH_Pats_AllRemaining) %>% bind_rows(Other_Predicted_HighRisk) %>% bind_rows(Other_Predicted_LowRisk) %>%
  group_by(patient) %>% filter(AST==max(AST)) %>% slice(1) %>% ungroup() %>%
  filter(AST>0 & AST<200) %>% 
  ggplot(aes(x = AST, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Max AST (IU/L)") + ylab("Disease Group \n")


FIB4_NASH_Pats_HighRisk %>% bind_rows(FIB4_NASH_Pats_AllRemaining) %>% bind_rows(Other_Predicted_HighRisk) %>%  bind_rows(Other_Predicted_LowRisk) %>%
  group_by(patient) %>% filter(ALT==max(ALT)) %>% slice(1) %>% ungroup() %>%
  filter(ALT>0 & ALT<200) %>% 
  ggplot(aes(x = ALT, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Max ALT (IU/L)") + ylab("Disease Group \n")


FIB4_NASH_Pats_HighRisk %>% bind_rows(FIB4_NASH_Pats_AllRemaining) %>% bind_rows(Other_Predicted_HighRisk) %>%  bind_rows(Other_Predicted_LowRisk) %>%
  group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>% ungroup() %>%
  filter(fibrosis4>0 & fibrosis4<6) %>% 
  ggplot(aes(x = fibrosis4, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Max FIB4") + ylab("Disease Group \n")


FIB4_NASH_Pats_HighRisk %>% bind_rows(FIB4_NASH_Pats_AllRemaining) %>% bind_rows(Other_Predicted_HighRisk) %>%  bind_rows(Other_Predicted_LowRisk) %>%
  group_by(patient) %>% filter(Platelets==min(Platelets)) %>% slice(1) %>% ungroup() %>%
  filter(Platelets>50 & Platelets<400) %>% 
  ggplot(aes(x = Platelets, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Min Platelet Count (x10^3 / uL)") + ylab("Disease Group \n")




FIB4_NASH_Pats_HighRisk
FIB4_NASH_Pats_AllRemaining
Other_Predicted_HighRisk
Other_Predicted_LowRisk



length(unique(FIB4_NASH_Pats_HighRisk$patient))  
# All 1362

FIB4_NASH_Pats_HighRisk %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct() 
# All above threshold 243

FIB4_NASH_Pats_HighRisk %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct() %>%
  anti_join(FIB4_NASH_Pats_HighRisk %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct()) 
# Any above threshold 1119

FIB4_NASH_Pats_HighRisk %>% filter(AST<=50 & ALT<=50 & Platelets>=150) %>% select(patient) %>% distinct() %>% 
  anti_join(FIB4_NASH_Pats_HighRisk %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct()) 
# None above threshold 0




length(unique(FIB4_NASH_Pats_AllRemaining$patient))  
# All 3179

FIB4_NASH_Pats_AllRemaining %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct() 
# All above threshold 174

FIB4_NASH_Pats_AllRemaining %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct() %>%
  anti_join(FIB4_NASH_Pats_AllRemaining %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct()) 
# Any above threshold 1455

FIB4_NASH_Pats_AllRemaining %>% filter(AST<=50 & ALT<=50 & Platelets>=150) %>% select(patient) %>% distinct() %>% 
  anti_join(FIB4_NASH_Pats_AllRemaining %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct()) 
# None above threshold 1550




length(unique(Other_Predicted_HighRisk$patient))  
# All 14999

Other_Predicted_HighRisk %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct() 
# All above threshold 1927

Other_Predicted_HighRisk %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct() %>%
  anti_join(Other_Predicted_HighRisk %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct()) 
# Any above threshold 13072

Other_Predicted_HighRisk %>% filter(AST<=50 & ALT<=50 & Platelets>=150) %>% select(patient) %>% distinct() %>% 
  anti_join(Other_Predicted_HighRisk %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct()) 
# None above threshold 0




length(unique(Other_Predicted_LowRisk$patient))  
# All 445218

Other_Predicted_LowRisk %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct() 
# All above threshold 5336

Other_Predicted_LowRisk %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct() %>%
  anti_join(Other_Predicted_LowRisk %>% filter(AST>50 & ALT>50 & Platelets<150) %>% select(patient) %>% distinct()) 
# Any above threshold 95199

Other_Predicted_LowRisk %>% filter(AST<=50 & ALT<=50 & Platelets>=150) %>% select(patient) %>% distinct() %>% 
  anti_join(Other_Predicted_LowRisk %>% filter(AST>50 | ALT>50 | Platelets<150) %>% select(patient) %>% distinct()) 
# None above threshold 344683




NASH_Events <- fread("NASH Events.txt")
names(NASH_Events)[1] <- "patient"

NASH_Dx_HighRisk_Events <- FIB4_NASH_Pats_HighRisk %>% select(patient) %>% distinct() %>% inner_join(NASH_Events)
No_Dx_HighRisk_Events <- Other_Predicted_HighRisk %>% select(patient) %>% distinct() %>% inner_join(NASH_Events)
FIB4_NASH_Pats_AllRemaining <- FIB4_NASH_Pats_AllRemaining %>% select(patient) %>% distinct() %>% inner_join(NASH_Events)
Other_Predicted_LowRisk <- Other_Predicted_LowRisk %>% select(patient) %>% distinct() %>% inner_join(NASH_Events)



NASH_Dx_HighRisk_Events <- NASH_Dx_HighRisk_Events %>% drop_na()
No_Dx_HighRisk_Events <- No_Dx_HighRisk_Events %>% drop_na()
FIB4_NASH_Pats_AllRemaining <- FIB4_NASH_Pats_AllRemaining %>% drop_na()
Other_Predicted_LowRisk <- Other_Predicted_LowRisk %>% drop_na()




NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Dx_HighRisk_Events <- NASH_Dx_HighRisk_Events %>% left_join(NASH_Event_Claims_Providers) %>% drop_na()
No_Dx_HighRisk_Events <- No_Dx_HighRisk_Events %>% left_join(NASH_Event_Claims_Providers) %>% drop_na()

data.frame(NASH_Dx_HighRisk_Events %>% group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n)) %>% summarise(n2=sum(n))


No_Dx_HighRisk_Events %>% group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n) %>% summarise(n2=sum(n))

# -----------
# Check total number of lab entries for each of the 4 cohorts, compare high risk vs low risk -----------

# Those above threshold

NASH_Pats_95ConfLiver_2plusHits <- fread("NASH_Pats_95ConfLiver_2plusHits.txt")
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
NAFLD_Pats_95ConfLiver_2plusHits <- fread("NAFLD_Pats_95ConfLiver_2plusHits.txt")

# Those with labs

FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")

FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% anti_join(FIB4_NASH_Pats %>% select(patient) %>% distinct()) %>% anti_join(FIB4_NAFLD_Pats %>% select(patient) %>% distinct())
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% anti_join(FIB4_NASH_Pats %>% select(patient) %>% distinct()) %>% anti_join(FIB4_NAFLD_Pats %>% select(patient) %>% distinct())



DIA_HighRisk <- DIA_Pats_95ConfLiver_2plusHits
DIA_Remaining <-  FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits) %>% select(patient) %>% distinct()

OBE_HighRisk <- OBE_Pats_95ConfLiver_2plusHits
OBE_Remaining <-  FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits) %>% select(patient) %>% distinct()



# Count how many tests per patient
DANU_NLP_Measurement <- fread("DANU NLP Measurement Sample.txt")
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% select(patid)

DANU_EHR_Observations <- fread("DANU EHR Observations Sample.txt")
DANU_EHR_Observations <- DANU_EHR_Observations %>% select(patid) 

DANU_EHR_Labs <- fread("DANU EHR Labs Sample.txt")
DANU_EHR_Labs <- DANU_EHR_Labs %>% select(patid) 

DANU_Claims <- fread("DANU Claims Lab Results Sample.txt")
DANU_Claims <- DANU_Claims %>% select(patid)

All_Pats_Tests <- DANU_NLP_Measurement %>% bind_rows(DANU_EHR_Observations) %>% bind_rows(DANU_EHR_Labs) %>% bind_rows(DANU_Claims)
names(All_Pats_Tests)[1] <- "patient"


All_Pats_Tests <- All_Pats_Tests %>% group_by(patient) %>% count()


N_Tests_DIA <- FIB4_Diabetes_Pats %>% select(patient) %>% distinct() %>% inner_join(All_Pats_Tests)
N_Tests_OBE <- FIB4_Obesity_Pats %>% select(patient) %>% distinct() %>% inner_join(All_Pats_Tests)

N_Tests_DIA_HighRisk <- N_Tests_DIA %>% inner_join(DIA_Pats_95ConfLiver_2plusHits)
N_Tests_DIA_LowRisk <- N_Tests_DIA %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)

N_Tests_OBE_HighRisk <- N_Tests_OBE %>% inner_join(OBE_Pats_95ConfLiver_2plusHits)
N_Tests_OBE_LowRisk <- N_Tests_OBE %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)


mean(N_Tests_DIA_HighRisk$n) # 2689.343
mean(N_Tests_DIA_LowRisk$n) # 1085.626

median(N_Tests_DIA_HighRisk$n) # 1522.5
median(N_Tests_DIA_LowRisk$n) # 565


mean(N_Tests_OBE_HighRisk$n) # 1673.542
mean(N_Tests_OBE_LowRisk$n) # 673.05

median(N_Tests_OBE_HighRisk$n) # 918
median(N_Tests_OBE_LowRisk$n) # 381



N_Tests_DIA_HighRisk %>% mutate(group="Diabetes High Risk") %>% 
  bind_rows(N_Tests_DIA_LowRisk %>% mutate(group="Diabetes Low Risk"))  %>%
  filter(n<5000) %>%
  ggplot(aes(x = n, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Number of Individual Records") + ylab("Diabetes Predicted Group \n")



N_Tests_OBE_HighRisk %>% mutate(group="Obesity High Risk") %>% 
  bind_rows(N_Tests_OBE_LowRisk %>% mutate(group="Obesity Low Risk"))  %>%
  filter(n<3000) %>%
  ggplot(aes(x = n, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Number of Individual Records") + ylab("Obesity Predicted Group \n")






# ---------

# Check number of LIVER ! lab entries for each of the 4 cohorts, compare high risk vs low risk -----------

# Those above threshold

NASH_Pats_95ConfLiver_2plusHits <- fread("NASH_Pats_95ConfLiver_2plusHits.txt")
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
NAFLD_Pats_95ConfLiver_2plusHits <- fread("NAFLD_Pats_95ConfLiver_2plusHits.txt")

# Those with labs

FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")

FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% anti_join(FIB4_NASH_Pats %>% select(patient) %>% distinct()) %>% anti_join(FIB4_NAFLD_Pats %>% select(patient) %>% distinct())
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% anti_join(FIB4_NASH_Pats %>% select(patient) %>% distinct()) %>% anti_join(FIB4_NAFLD_Pats %>% select(patient) %>% distinct())



DIA_HighRisk <- DIA_Pats_95ConfLiver_2plusHits
DIA_Remaining <-  FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits) %>% select(patient) %>% distinct()

OBE_HighRisk <- OBE_Pats_95ConfLiver_2plusHits
OBE_Remaining <-  FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits) %>% select(patient) %>% distinct()



# Count how many tests per patient
DANU_Measures <- fread("DANU Measures.txt")
names(DANU_Measures)[1] <- "patient"

DANU_Measures <- DANU_Measures %>% group_by(patient) %>% count()


N_Tests_DIA <- FIB4_Diabetes_Pats %>% select(patient) %>% distinct() %>% inner_join(DANU_Measures)
N_Tests_OBE <- FIB4_Obesity_Pats %>% select(patient) %>% distinct() %>% inner_join(DANU_Measures)

N_Tests_DIA_HighRisk <- N_Tests_DIA %>% inner_join(DIA_Pats_95ConfLiver_2plusHits)
N_Tests_DIA_LowRisk <- N_Tests_DIA %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)

N_Tests_OBE_HighRisk <- N_Tests_OBE %>% inner_join(OBE_Pats_95ConfLiver_2plusHits)
N_Tests_OBE_LowRisk <- N_Tests_OBE %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)


mean(N_Tests_DIA_HighRisk$n) # 105.6374
mean(N_Tests_DIA_LowRisk$n) # 52.19834

median(N_Tests_DIA_HighRisk$n) # 71
median(N_Tests_DIA_LowRisk$n) # 35


mean(N_Tests_OBE_HighRisk$n) # 69.709
mean(N_Tests_OBE_LowRisk$n) # 33.54515

median(N_Tests_OBE_HighRisk$n) # 47
median(N_Tests_OBE_LowRisk$n) # 23



N_Tests_DIA_HighRisk %>% mutate(group="Diabetes High Risk") %>% 
  bind_rows(N_Tests_DIA_LowRisk %>% mutate(group="Diabetes Low Risk"))  %>%
  filter(n<200) %>%
  ggplot(aes(x = n, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Number of Individual Records") + ylab("Diabetes Predicted Group \n")



N_Tests_OBE_HighRisk %>% mutate(group="Obesity High Risk") %>% 
  bind_rows(N_Tests_OBE_LowRisk %>% mutate(group="Obesity Low Risk"))  %>%
  filter(n<150) %>%
  ggplot(aes(x = n, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "C", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Number of Individual Records") + ylab("Obesity Predicted Group \n")





# ---------
# Check how many diabetes / obesity also have cirrhosis or fibrosis -----------

NASH_Demographics_All <- fread("NASH Demographics All.txt")
names(NASH_Demographics_All)[1] <- "patient"
length(unique(NASH_Demographics_All$patient))



DIA_Pats <- fread("DIA Drug Histories.txt")
DIA_Pats <- DIA_Pats %>% select(patient, weight)

OBE_Pats <- fread("OBE Drug Histories.txt")
OBE_Pats <- OBE_Pats %>% select(patient,weight)

NASH_Pats <- fread("NASH Drug Histories.txt")
NASH_Pats <- NASH_Pats %>% select(patient,weight)

NAFLD_Pats <- fread("NAFLD Drug Histories.txt")
NAFLD_Pats <- NAFLD_Pats %>% select(patient,weight)

DIA_Pats <- DIA_Pats %>% anti_join(NASH_Pats) %>% anti_join(NAFLD_Pats)                                                                
OBE_Pats <- OBE_Pats %>% anti_join(NASH_Pats) %>% anti_join(NAFLD_Pats)                                                                


DIA_Pats <- DIA_Pats %>% left_join(NASH_Demographics_All)
OBE_Pats <- OBE_Pats %>% left_join(NASH_Demographics_All)


sum(DIA_Pats$weight) # 40724926

DIA_Pats %>% filter(!is.na(chronic_liver_disease)) %>% summarise(n=sum(weight)) # 3143586 (8%)
DIA_Pats %>% filter(!is.na(nash)) %>% summarise(n=sum(weight)) # 0
DIA_Pats %>% filter(!is.na(nafld)) %>% summarise(n=sum(weight)) # 0
DIA_Pats %>% filter(!is.na(hepatitis)) %>% summarise(n=sum(weight)) # 1287121 (3%)
DIA_Pats %>% filter(!is.na(fibrosis)) %>% summarise(n=sum(weight)) # 78509.97 (0.2%)
DIA_Pats %>% filter(!is.na(cirrhosis)) %>% summarise(n=sum(weight)) # 489638.5 (1%)
DIA_Pats %>% filter(!is.na(liver_cancer)) %>% summarise(n=sum(weight)) # 223307.5 (0.5%)
DIA_Pats %>% filter(!is.na(liver_failure)) %>% summarise(n=sum(weight)) # 211777.8 (0.5%)
DIA_Pats %>% filter(!is.na(liver_transplant)) %>% summarise(n=sum(weight)) # 74550.95 (0.2%)
DIA_Pats %>% filter(!is.na(liver_ultrasound)) %>% summarise(n=sum(weight)) # 105464.9 (0.3%)
DIA_Pats %>% filter(!is.na(liver_imaging)) %>% summarise(n=sum(weight)) # 618995.4 (2%)
DIA_Pats %>% filter(!is.na(liver_biopsy)) %>% summarise(n=sum(weight)) # 130053.3 (0.3%)
DIA_Pats %>% filter(!is.na(alcohol_abuse)) %>% summarise(n=sum(weight)) # 2450108 (6%)



sum(OBE_Pats$weight) # 99449618

OBE_Pats %>% filter(!is.na(chronic_liver_disease)) %>% summarise(n=sum(weight)) # 4153743 (4%)
OBE_Pats %>% filter(!is.na(nash)) %>% summarise(n=sum(weight)) # 0
OBE_Pats %>% filter(!is.na(nafld)) %>% summarise(n=sum(weight)) # 0
OBE_Pats %>% filter(!is.na(hepatitis)) %>% summarise(n=sum(weight)) # 1245348 (1%)
OBE_Pats %>% filter(!is.na(fibrosis)) %>% summarise(n=sum(weight)) # 85351.65 (0.09%)
OBE_Pats %>% filter(!is.na(cirrhosis)) %>% summarise(n=sum(weight)) # 1 374150.2 (0.4%)
OBE_Pats %>% filter(!is.na(liver_cancer)) %>% summarise(n=sum(weight)) # 204691.8 (0.2%)
OBE_Pats %>% filter(!is.na(liver_failure)) %>% summarise(n=sum(weight)) # 167382.9 (0.2%)
OBE_Pats %>% filter(!is.na(liver_transplant)) %>% summarise(n=sum(weight)) # 44504.11 (0.04%)
OBE_Pats %>% filter(!is.na(liver_ultrasound)) %>% summarise(n=sum(weight)) # 136295.9 (0.1%)
OBE_Pats %>% filter(!is.na(liver_imaging)) %>% summarise(n=sum(weight)) # 1188366 (1%)
OBE_Pats %>% filter(!is.na(liver_biopsy)) %>% summarise(n=sum(weight)) # 152191.2 (0.2%)
OBE_Pats %>% filter(!is.na(alcohol_abuse)) %>% summarise(n=sum(weight)) # 5072429 (5%)



FIB4_Diabetes_Pats_ALL <- fread("FIB4_Diabetes_Pats_ALL.txt")
FIB4_Obesity_Pats_ALL <- fread("FIB4_Obesity_Pats_ALL.txt")
FIB4_Diabetes_Pats_ALL <- FIB4_Diabetes_Pats_ALL %>% select(patient)
FIB4_Obesity_Pats_ALL <- FIB4_Obesity_Pats_ALL %>% select(patient)

DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")

FIB4_Diabetes_Pats_ALL <- FIB4_Diabetes_Pats_ALL %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)
FIB4_Obesity_Pats_ALL <- FIB4_Obesity_Pats_ALL %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)

FIB4_Diabetes_Pats_ALL <- FIB4_Diabetes_Pats_ALL %>% left_join(DIA_Pats)
DIA_Pats_95ConfLiver_2plusHits <- DIA_Pats_95ConfLiver_2plusHits %>% left_join(DIA_Pats)

FIB4_Obesity_Pats_ALL <- FIB4_Obesity_Pats_ALL %>% left_join(OBE_Pats)
OBE_Pats_95ConfLiver_2plusHits <- OBE_Pats_95ConfLiver_2plusHits %>% left_join(OBE_Pats)

FIB4_Diabetes_Pats_ALL
FIB4_Obesity_Pats_ALL
DIA_Pats_95ConfLiver_2plusHits
OBE_Pats_95ConfLiver_2plusHits



sum(DIA_Pats_95ConfLiver_2plusHits$weight, na.rm=T) # 686835.8

DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(chronic_liver_disease)) %>% summarise(n=sum(weight, na.rm=T)) # 97428.77 (14%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(nash)) %>% summarise(n=sum(weight, na.rm=T)) # 0
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(nafld)) %>% summarise(n=sum(weight, na.rm=T)) # 0
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(hepatitis)) %>% summarise(n=sum(weight, na.rm=T)) # 42838.5 (6%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(fibrosis)) %>% summarise(n=sum(weight, na.rm=T)) # 5349.16 (0.8%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(cirrhosis)) %>% summarise(n=sum(weight, na.rm=T)) # 16038.96 (2%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_cancer)) %>% summarise(n=sum(weight, na.rm=T)) # 204.31 (0.03%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_failure)) %>% summarise(n=sum(weight, na.rm=T)) # 14848.91 (2%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_transplant)) %>% summarise(n=sum(weight, na.rm=T)) # 3465.14 (0.5%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_ultrasound)) %>% summarise(n=sum(weight, na.rm=T)) # 4350.54 (0.6%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_imaging)) %>% summarise(n=sum(weight, na.rm=T)) # 23087.12 (3%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_biopsy)) %>% summarise(n=sum(weight, na.rm=T)) # 8599.96 (1%)
DIA_Pats_95ConfLiver_2plusHits %>% filter(!is.na(alcohol_abuse)) %>% summarise(n=sum(weight, na.rm=T)) # 5706.8 (0.8%)






sum(OBE_Pats_95ConfLiver_2plusHits$weight, na.rm=T) # 715254

OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(chronic_liver_disease)) %>% summarise(n=sum(weight, na.rm=T)) # 82110.81 (11%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(nash)) %>% summarise(n=sum(weight, na.rm=T)) # 0
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(nafld)) %>% summarise(n=sum(weight, na.rm=T)) # 0
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(hepatitis)) %>% summarise(n=sum(weight, na.rm=T)) # 38309.66 (5%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(fibrosis)) %>% summarise(n=sum(weight, na.rm=T)) # 3469.08 (0.5%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(cirrhosis)) %>% summarise(n=sum(weight, na.rm=T)) # 11904.74 (2%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_cancer)) %>% summarise(n=sum(weight, na.rm=T)) # 384.71 (0.05%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_failure)) %>% summarise(n=sum(weight, na.rm=T)) # 7961.78 (1%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_transplant)) %>% summarise(n=sum(weight, na.rm=T)) # 1353.74 (0.2%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_ultrasound)) %>% summarise(n=sum(weight, na.rm=T)) # 5213.34 (0.7%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_imaging)) %>% summarise(n=sum(weight, na.rm=T)) # 22200.64 (3%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(liver_biopsy)) %>% summarise(n=sum(weight, na.rm=T)) # 6275.55 (0.9%)
OBE_Pats_95ConfLiver_2plusHits %>% filter(!is.na(alcohol_abuse)) %>% summarise(n=sum(weight, na.rm=T)) # 7387.37 (1%)


# -------------
# Mean Platelet Volume High Risk vs Low Risk -----------
DANU_Claims <- fread("DANU Claims Lab Results Sample.txt") 
DANU_Claims <- DANU_Claims %>% filter(tst_desc == "MEAN PLATELET VOLUME" | tst_desc == "MPV")
DANU_Claims <- DANU_Claims %>% filter(rslt_nbr>0)
DANU_Claims <- DANU_Claims %>% select(patid, weight, fst_dt, rslt_nbr)
names(DANU_Claims)[3] <- "MPV_Date"
names(DANU_Claims)[4] <- "MPV"



DANU_NLP_Measurement <- fread("DANU NLP Measurement Sample.txt")
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_type == "MPV")
DANU_NLP_Measurement$measurement_value <- as.numeric(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(!is.na(measurement_value))
range(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_value>0 & measurement_value<20)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% select(patid, weight, note_date, measurement_value)
names(DANU_NLP_Measurement)[3] <- "MPV_Date"
names(DANU_NLP_Measurement)[4] <- "MPV"

DANU_EHR_Labs <- fread("DANU EHR Labs Sample.txt")
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(test_name == "Platelet mean volume (MPV)")
DANU_EHR_Labs$test_result <- as.numeric(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(!is.na(test_result))
range(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(test_result>0 & test_result<20)
DANU_EHR_Labs <- DANU_EHR_Labs %>% select(patid, weight, result_date,  test_result)
names(DANU_EHR_Labs)[3] <- "MPV_Date"
names(DANU_EHR_Labs)[4] <- "MPV"

temp <- DANU_Claims %>% bind_rows(DANU_NLP_Measurement) %>% bind_rows(DANU_EHR_Labs)

names(temp)[1] <- "patient"



FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% select(patient)
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)

FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% inner_join(temp)
DIA_Pats_95ConfLiver_2plusHits <- DIA_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Diabetes_Pats %>% group_by(patient) %>% filter(MPV == max(MPV)) %>% slice(1) %>% ungroup()

temp2 <- DIA_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(MPV == max(MPV)) %>% slice(1) %>% ungroup() 

temp1$group <- "Low Risk Diabetes"
temp2$group <- "High Risk Diabetes"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = MPV, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(7,14) +
  xlab("\n Mean Platelet Volume (fL)") + ylab("Diabetes Predicted Group \n")






FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% select(patient)
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)

FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% inner_join(temp)
OBE_Pats_95ConfLiver_2plusHits <- OBE_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Obesity_Pats %>% group_by(patient) %>% filter(MPV == max(MPV)) %>% slice(1) %>% ungroup() %>% summarise(n=mean(MPV))
# 10.1
temp2 <- OBE_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(MPV == max(MPV)) %>% slice(1) %>% ungroup() %>% summarise(n=mean(MPV))
# 10.5
temp1$group <- "Low Risk Obesity"
temp2$group <- "High Risk Obesity"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = MPV, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(7,14) +
  xlab("\n Mean Platelet Volume (fL)") + ylab("Obesity Predicted Group \n")

# ---------
# Physicians for high risk patients ------------------

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- fread("NAFLD_Pats_95ConfLiver_2plusHits_Provider.txt")
DIA_Pats_95ConfLiver_2plusHits_Provider <- fread("DIA_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))

Summary_Specialties <- fread("Summary_Specialties.txt")


# NASH 
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
NASH_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(NASH_Pats_95ConfLiver_2plusHits_Provider$prov)

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()

tempNASH <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                       ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                              ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                     ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                            ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                   ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                          ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                 ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                        ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)
  
names(tempNASH)[2] <- "n_NASH"




NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
NAFLD_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(NAFLD_Pats_95ConfLiver_2plusHits_Provider$prov)

NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()


# NAFLD
tempNAFLD <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempNAFLD)[2] <- "n_NAFLD"


# Diabetes
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(DIA_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
DIA_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(DIA_Pats_95ConfLiver_2plusHits_Provider$prov)

DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()


tempDIA <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempDIA)[2] <- "n_DIA"




# Obesity

OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
OBE_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(OBE_Pats_95ConfLiver_2plusHits_Provider$prov)

OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()

tempOBE <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempOBE)[2] <- "n_OBE"


temp <- tempNASH %>% left_join(tempNAFLD) %>% left_join(tempDIA) %>% left_join(tempOBE)


fwrite(temp, "Physicians_All_Events_HighRisk_Summary.txt", sep="\t")

# -----
# Physicians Es and Ks only ---------------
NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- fread("NAFLD_Pats_95ConfLiver_2plusHits_Provider.txt")
DIA_Pats_95ConfLiver_2plusHits_Provider <- fread("DIA_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("E|K", diag))
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("E|K", diag))
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("E|K", diag))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("E|K", diag))

Summary_Specialties <- fread("Summary_Specialties.txt")




NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
NASH_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(NASH_Pats_95ConfLiver_2plusHits_Provider$prov)

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()

tempNASH <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempNASH)[2] <- "n_NASH"





NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
NAFLD_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(NAFLD_Pats_95ConfLiver_2plusHits_Provider$prov)

NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()


tempNAFLD <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempNAFLD)[2] <- "n_NAFLD"








DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(DIA_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
DIA_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(DIA_Pats_95ConfLiver_2plusHits_Provider$prov)

DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()


tempDIA <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempDIA)[2] <- "n_DIA"








OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
OBE_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(OBE_Pats_95ConfLiver_2plusHits_Provider$prov)

OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()

tempOBE <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempOBE)[2] <- "n_OBE"


temp <- tempNASH %>% left_join(tempNAFLD) %>% left_join(tempDIA) %>% left_join(tempOBE)


fwrite(temp, "Physicians_All_Events_HighRisk_EsKs.txt", sep="\t")

# -----------
# Monocytes Fraction (%)  High Risk vs Low Risk -----------

DANU_Claims <- fread("DANU Claims Lab Results Sample.txt") 
DANU_Claims <- DANU_Claims %>% filter(grepl("MONOCYTES", tst_desc) & rslt_unit_nm == "%")
DANU_Claims <- DANU_Claims %>% filter(rslt_nbr>0)
DANU_Claims <- DANU_Claims %>% select(patid, weight, fst_dt, rslt_nbr)
names(DANU_Claims)[3] <- "Percent_Monos_Date"
names(DANU_Claims)[4] <- "Percent_Monos"



DANU_NLP_Measurement <- fread("DANU NLP Measurement Sample.txt")
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(grepl("MONOCYTES",measurement_type) & measurement_detail == "%")
DANU_NLP_Measurement$measurement_value <- as.numeric(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(!is.na(measurement_value))
range(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_value>0 & measurement_value<50)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% select(patid, weight, note_date, measurement_value)
names(DANU_NLP_Measurement)[3] <- "Percent_Monos_Date"
names(DANU_NLP_Measurement)[4] <- "Percent_Monos"

DANU_EHR_Labs <- fread("DANU EHR Labs Sample.txt")
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(grepl("Monocyte",test_name) & result_unit == "%")
DANU_EHR_Labs$test_result <- as.numeric(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(!is.na(test_result))
range(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% select(patid, weight, result_date,  test_result)
names(DANU_EHR_Labs)[3] <- "Percent_Monos_Date"
names(DANU_EHR_Labs)[4] <- "Percent_Monos"

temp <- DANU_Claims %>% bind_rows(DANU_NLP_Measurement) %>% bind_rows(DANU_EHR_Labs)

names(temp)[1] <- "patient"


FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% select(patient)
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)

FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% inner_join(temp)
DIA_Pats_95ConfLiver_2plusHits <- DIA_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Diabetes_Pats %>% group_by(patient) %>% filter(Percent_Monos == mean(Percent_Monos)) %>% slice(1) %>% ungroup()  %>% summarise(n=mean(Percent_Monos))
# 9.42
temp2 <- DIA_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Percent_Monos == mean(Percent_Monos)) %>% slice(1) %>% ungroup() %>% summarise(n=mean(Percent_Monos)) 
# 10.6

temp1$group <- "Low Risk Diabetes"
temp2$group <- "High Risk Diabetes"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Percent_Monos, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(4,16) +
  xlab("\n Monocytes (%)") + ylab("Diabetes Predicted Group \n")






FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% select(patient)
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)

FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% inner_join(temp)
OBE_Pats_95ConfLiver_2plusHits <- OBE_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Obesity_Pats %>% group_by(patient) %>% filter(Percent_Monos == max(Percent_Monos)) %>% slice(1) %>% ungroup() 
# 9.23
temp2 <- OBE_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Percent_Monos == max(Percent_Monos)) %>% slice(1) %>% ungroup()
# 11.2

temp1$group <- "Low Risk Obesity"
temp2$group <- "High Risk Obesity"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Percent_Monos, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(4,16) +
  xlab("\n Monocytes (%)") + ylab("Obesity Predicted Group \n")
# ------ 

# Flow "matrix" of Targets vs Origin/Comorbidities ------------

Matrix_Targets_TypeVSComorb <- fread("Matrix_Targets_TypeVSComorb.txt")

row.names(Matrix_Targets_TypeVSComorb) <- Matrix_Targets_TypeVSComorb$V1

Matrix_Targets_TypeVSComorb <- Matrix_Targets_TypeVSComorb %>% select(-c(V1))

rownames(Matrix_Targets_TypeVSComorb)
colnames(Matrix_Targets_TypeVSComorb)



grid.bubble.plot <- function(df, 
                             axis_labels_size=10, 
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
  res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("NASH-only","NASH-fibrosis","NASH-cirrhosis"))))
                          
   gg <- ggplot(res_df, aes(x=values_x, y=values_y, size = values, fill=factor(values_x))) +
    geom_point(alpha=bubble_alpha, shape=bubble_shape, stroke=bubble_edge_stroke) +
    scale_size(range = bubble_size_range) +
     scale_fill_brewer(palette = "YlOrRd") +
    scale_x_discrete(position = x_axis_position) +
    scale_y_discrete(limits=rev)+
    #geom_text(aes(label=paste0(values,"%")), fontface="bold", size=values_text_size, color=values_text_color,) +
    geom_text(aes(label=paste0(values,"k")), fontface="bold", size=values_text_size, color=values_text_color,) +
     theme(line=element_blank(), 
          panel.background=element_blank(),
          legend.position="none",
          axis.title=element_blank(),
          axis.text=element_text(size=axis_labels_size),
          aspect.ratio=aspect_ratio)
  gg
}

grid.bubble.plot(Matrix_Targets_TypeVSComorb)

# ---------
# Select Dates WHEN patients where high risk >95% (at least 2x!), check all the physicians seen ON THAT MONTH -------- 

# Remove everyone with Liver Cancer or Alcohol Abuse
NASH_Dossiers <- fread("NASH Dossiers.txt")
Cancer_Alcohol_pats <- NASH_Dossiers %>% filter(condition == "Liver Cancer" | condition == "Alcohol Abuse") %>% select(patid) %>% distinct()
Biopsy_pats <- NASH_Dossiers %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct()
names(Cancer_Alcohol_pats)[1] <- "patient"
names(Biopsy_pats)[1] <- "patient"
Biopsy_pats$Biopsy_Ever <- "Biopsy"

# Get Pats with all labs on the same date 
NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt") 

NAFLD <- NAFLD_Pats %>% select(patient)
DIA_Pats <- DIA_Pats %>% anti_join(NAFLD)
OBE_Pats <- OBE_Pats %>% anti_join(NAFLD)


# Remove Cancer/Alcohol
NASH_Pats <- NASH_Pats %>% anti_join(Cancer_Alcohol_pats)
DIA_Pats <- DIA_Pats %>% anti_join(Cancer_Alcohol_pats)
OBE_Pats<- OBE_Pats %>% anti_join(Cancer_Alcohol_pats)
NAFLD_Pats<- NAFLD_Pats %>% anti_join(Cancer_Alcohol_pats)


# Add Biopsy ever status
NASH_Pats <- NASH_Pats %>% left_join(Biopsy_pats)
DIA_Pats <- DIA_Pats %>% left_join(Biopsy_pats)
OBE_Pats<- OBE_Pats %>% left_join(Biopsy_pats)
NAFLD_Pats<- NAFLD_Pats %>% left_join(Biopsy_pats)


# Flag patients as Positive / negative
NASH_To_keep <- NASH_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
NASH_To_keep$Dx_status <- 1
NASH_To_keep <- NASH_To_keep %>% left_join(NASH_Pats)
NASH_To_keep <- NASH_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
NASH_To_keep <- NASH_To_keep %>% select(-Biopsy_Ever)
NASH_To_keep <- NASH_To_keep %>% select(1,3,4,5,6,7,8,2)


DIA_To_keep <- DIA_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
DIA_To_keep$Dx_status <- 1
DIA_To_keep <- DIA_To_keep %>% left_join(DIA_Pats)
DIA_To_keep <- DIA_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
DIA_To_keep <- DIA_To_keep %>% select(-Biopsy_Ever)
DIA_To_keep <- DIA_To_keep %>% select(1,3,4,5,6,7,8,2)


OBE_To_keep <- OBE_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
OBE_To_keep$Dx_status <- 1
OBE_To_keep <- OBE_To_keep %>% left_join(OBE_Pats)
OBE_To_keep <- OBE_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
OBE_To_keep <- OBE_To_keep %>% select(-Biopsy_Ever)
OBE_To_keep <- OBE_To_keep %>% select(1,3,4,5,6,7,8,2)


NAFLD_To_keep <- NAFLD_Pats %>% filter((AST>50&ALT>50&Platelets<150)|Biopsy_Ever=="Biopsy") %>% select(patient) %>% distinct()
NAFLD_To_keep$Dx_status <- 1
NAFLD_To_keep <- NAFLD_To_keep %>% left_join(NAFLD_Pats)
NAFLD_To_keep <- NAFLD_To_keep %>% mutate(Biopsy_Ever=ifelse(Biopsy_Ever=="Biopsy",1,0))
NAFLD_To_keep <- NAFLD_To_keep %>% select(-Biopsy_Ever)
NAFLD_To_keep <- NAFLD_To_keep %>% select(1,3,4,5,6,7,8,2)


NASH_Pats_to_remove <- NASH_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
NASH_Negative <- NASH_Pats %>% anti_join(NASH_Pats_to_remove)
NASH_Negative <- NASH_Negative %>% select(-Biopsy_Ever)
NASH_Negative$Dx_status <- 0

DIA_Pats_to_remove <- DIA_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
DIA_Negative <- DIA_Pats %>% anti_join(DIA_Pats_to_remove)
DIA_Negative <- DIA_Negative %>% select(-Biopsy_Ever)
DIA_Negative$Dx_status <- 0

OBE_Pats_to_remove <- OBE_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
OBE_Negative <- OBE_Pats %>% anti_join(OBE_Pats_to_remove)
OBE_Negative <- OBE_Negative %>% select(-Biopsy_Ever)
OBE_Negative$Dx_status <- 0

NAFLD_Pats_to_remove <- NAFLD_Pats %>% filter((AST>50|ALT>50|Platelets<150|Biopsy_Ever=="Biopsy")) %>% select(patient) %>% distinct()
NAFLD_Negative <- NAFLD_Pats %>% anti_join(NAFLD_Pats_to_remove)
NAFLD_Negative <- NAFLD_Negative %>% select(-Biopsy_Ever)
NAFLD_Negative$Dx_status <- 0


NASH_To_keep <- NASH_To_keep %>% sample_n(5000)
DIA_To_keep <- DIA_To_keep %>% sample_n(5000)
OBE_To_keep <- OBE_To_keep %>% sample_n(5000)
NAFLD_To_keep <- NAFLD_To_keep %>% sample_n(5000)
NASH_Negative <- NASH_Negative %>% sample_n(5000)
DIA_Negative <- DIA_Negative %>% sample_n(5000)
OBE_Negative <- OBE_Negative %>% sample_n(5000)
NAFLD_Negative <- NAFLD_Negative %>% sample_n(5000)

Indicator <- NASH_To_keep %>% bind_rows(DIA_To_keep) %>% bind_rows(OBE_To_keep) %>% bind_rows(NAFLD_To_keep) %>%
  bind_rows(DIA_Negative) %>% bind_rows(OBE_Negative) %>% bind_rows(NAFLD_Negative)

Indicator %>% filter(Dx_status==1) %>% summarise(n=mean(fibrosis4))
Indicator %>% filter(Dx_status==0) %>% summarise(n=mean(fibrosis4))
Indicator %>% filter(Dx_status==1) %>% summarise(n=median(fibrosis4))
Indicator %>% filter(Dx_status==0) %>% summarise(n=median(fibrosis4))

Risk_pred_model <- glm( Dx_status ~ AST+ALT+Platelets, data = Indicator, family = binomial)

NASH_Pats <- NASH_Pats %>% select(-c(Biopsy_Ever))
DIA_Pats <- DIA_Pats %>% select(-c(Biopsy_Ever))
OBE_Pats <- OBE_Pats %>% select(-c(Biopsy_Ever))
NAFLD_Pats <- NAFLD_Pats %>% select(-c(Biopsy_Ever))


NASH_Probability <- data.frame(Risk_pred_model %>% predict(NASH_Pats, type = "response"))
DIA_Probability <- data.frame(Risk_pred_model %>% predict(DIA_Pats, type = "response"))
OBE_Probability <- data.frame(Risk_pred_model %>% predict(OBE_Pats, type = "response"))
NAFLD_Probability <- data.frame(Risk_pred_model %>% predict(NAFLD_Pats, type = "response"))
Random_Probability <- data.frame(Risk_pred_model %>% predict(Random_Pats, type = "response"))


NASH_Probability <- NASH_Pats %>% select(patient, claimed) %>% bind_cols(NASH_Probability)
DIA_Probability <- DIA_Pats %>% select(patient, claimed) %>% bind_cols(DIA_Probability)
OBE_Probability <- OBE_Pats %>% select(patient, claimed) %>% bind_cols(OBE_Probability)
NAFLD_Probability <- NAFLD_Pats %>% select(patient, claimed) %>% bind_cols(NAFLD_Probability)
Random_Probability <- Random_Pats %>% select(patient, claimed) %>% bind_cols(Random_Probability)


NASH_Probability <- NASH_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(NASH_Probability) %>% ungroup()
DIA_Probability <- DIA_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(DIA_Probability) %>% ungroup()
OBE_Probability <- OBE_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(OBE_Probability) %>% ungroup()
NAFLD_Probability <- NAFLD_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(NAFLD_Probability) %>% ungroup()
Random_Probability <- Random_Probability %>% group_by(patient) %>% count() %>% filter(n>1) %>% select(patient) %>% left_join(Random_Probability) %>% ungroup()


HighRiskPats_Dates <- NASH_Probability %>% filter(Risk_pred_model.....predict.NASH_Pats..type....response..>0.95) %>%
  select(patient, claimed) %>% distinct() %>% 
  bind_rows(DIA_Probability %>% filter(Risk_pred_model.....predict.DIA_Pats..type....response..>0.95) %>%
              select(patient, claimed) %>% distinct()) %>%
  bind_rows(OBE_Probability %>% filter(Risk_pred_model.....predict.OBE_Pats..type....response..>0.95) %>%
              select(patient, claimed) %>% distinct()) %>%
  bind_rows(NAFLD_Probability %>% filter(Risk_pred_model.....predict.NAFLD_Pats..type....response..>0.95) %>%
              select(patient, claimed) %>% distinct()) %>%
  bind_rows(Random_Probability %>% filter(Risk_pred_model.....predict.Random_Pats..type....response..>0.95) %>%
              select(patient, claimed) %>% distinct()) %>% distinct()


fwrite(HighRiskPats_Dates, "HighRiskPats_Dates.txt", sep="\t")

HighRiskPats_Dates <- fread("HighRiskPats_Dates.txt")

HighRiskPats_Dates$claimed <- as.Date(HighRiskPats_Dates$claimed)

setDT(HighRiskPats_Dates)[, Month_Yr := format(as.Date(claimed), "%Y-%m") ]
HighRiskPats_Dates <- HighRiskPats_Dates %>% select(-claimed)



# Fetch All Events 
NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- fread("NAFLD_Pats_95ConfLiver_2plusHits_Provider.txt")
DIA_Pats_95ConfLiver_2plusHits_Provider <- fread("DIA_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))

Summary_Specialties <- fread("Summary_Specialties.txt")


# NASH 
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
NASH_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(NASH_Pats_95ConfLiver_2plusHits_Provider$prov)

NASH_Pats_95ConfLiver_2plusHits_Provider$fst_dt <- as.Date(NASH_Pats_95ConfLiver_2plusHits_Provider$fst_dt)
setDT(NASH_Pats_95ConfLiver_2plusHits_Provider)[, Month_Yr := format(as.Date(fst_dt), "%Y-%m") ]
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(-fst_dt)
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[2] <- "patient"
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(HighRiskPats_Dates)

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(patient, Month_Yr) %>% distinct()

tempNASH <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(patient, Month_Yr, SUMMARY_SPECIALTY) %>%
  group_by(patient, Month_Yr) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(patient, Month_Yr) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempNASH)[2] <- "n_NASH"



# NAFLD

NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
NAFLD_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(NAFLD_Pats_95ConfLiver_2plusHits_Provider$prov)

NAFLD_Pats_95ConfLiver_2plusHits_Provider$fst_dt <- as.Date(NAFLD_Pats_95ConfLiver_2plusHits_Provider$fst_dt)
setDT(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[, Month_Yr := format(as.Date(fst_dt), "%Y-%m") ]
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(-fst_dt)
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[2] <- "patient"
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(HighRiskPats_Dates)

NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% group_by(patient, Month_Yr) %>% distinct()

tempNAFLD <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(patient, Month_Yr, SUMMARY_SPECIALTY) %>%
  group_by(patient, Month_Yr) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(patient, Month_Yr) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempNAFLD)[2] <- "n_NAFLD"



# Diabetes
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(DIA_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
DIA_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(DIA_Pats_95ConfLiver_2plusHits_Provider$prov)

DIA_Pats_95ConfLiver_2plusHits_Provider$fst_dt <- as.Date(DIA_Pats_95ConfLiver_2plusHits_Provider$fst_dt)
setDT(DIA_Pats_95ConfLiver_2plusHits_Provider)[, Month_Yr := format(as.Date(fst_dt), "%Y-%m") ]
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(-fst_dt)
names(DIA_Pats_95ConfLiver_2plusHits_Provider)[2] <- "patient"
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(HighRiskPats_Dates)

DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% group_by(patient, Month_Yr) %>% distinct()

tempDIA <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(patient, Month_Yr, SUMMARY_SPECIALTY) %>%
  group_by(patient, Month_Yr) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(patient, Month_Yr) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempDIA)[2] <- "n_DIA"



# Obesity
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
OBE_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(OBE_Pats_95ConfLiver_2plusHits_Provider$prov)

OBE_Pats_95ConfLiver_2plusHits_Provider$fst_dt <- as.Date(OBE_Pats_95ConfLiver_2plusHits_Provider$fst_dt)
setDT(OBE_Pats_95ConfLiver_2plusHits_Provider)[, Month_Yr := format(as.Date(fst_dt), "%Y-%m") ]
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(-fst_dt)
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[2] <- "patient"
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(HighRiskPats_Dates)

OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(patient, Month_Yr) %>% distinct()

tempOBE <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(patient, Month_Yr, SUMMARY_SPECIALTY) %>%
  group_by(patient, Month_Yr) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(patient, Month_Yr) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup() %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

names(tempOBE)[2] <- "n_OBE"



temp <- tempNASH %>% left_join(tempNAFLD) %>% left_join(tempDIA) %>% left_join(tempOBE)

fwrite(temp, "Physicians_Events_HighRisk_MonthPredictedRisk.txt", sep="\t")

# -----------
# WBC  High Risk vs Low Risk (! not working yet) -----------
DANU_Claims <- fread("DANU Claims Lab Results Sample.txt") 
DANU_Claims <- DANU_Claims %>% filter(tst_desc == "ABS LYMPHOCYTES" | tst_desc == "ABSOLUTE LYMPHOCYTES" | tst_desc == "LYMPHOCYTES, ABSOLUTE")
DANU_Claims$rslt_nbr <- as.numeric(DANU_Claims$rslt_nbr)
DANU_Claims <- DANU_Claims %>% filter(rslt_nbr>0)
DANU_Claims <- DANU_Claims %>% mutate(rslt_nbr = ifelse(rslt_nbr>10, rslt_nbr/1000, rslt_nbr))
DANU_Claims <- DANU_Claims %>% select(patid, weight, fst_dt, rslt_nbr)
names(DANU_Claims)[3] <- "Absolute_Lymphocyte_Date"
names(DANU_Claims)[4] <- "Absolute_Lymphocyte"


DANU_NLP_Measurement <- fread("DANU NLP Measurement Sample.txt")
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_type == "WHITE BLOOD CELL COUNT")
DANU_NLP_Measurement$measurement_value <- as.numeric(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(!is.na(measurement_value))
range(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_value>0)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% mutate(measurement_value = ifelse(measurement_value>500000, measurement_value/1000000000, measurement_value))
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_value<100000)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_value<1000)

DANU_NLP_Measurement <- DANU_NLP_Measurement %>% select(patid, weight, note_date, measurement_value)
names(DANU_NLP_Measurement)[3] <- "Absolute_WBC_Date"
names(DANU_NLP_Measurement)[4] <- "Absolute_WBC"



DANU_EHR_Labs <- fread("DANU EHR Labs Sample.txt")
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(test_name=="Lymphocyte.absolute")
DANU_EHR_Labs$test_result <- as.numeric(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(!is.na(test_result))
range(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(test_result>0)
DANU_EHR_Labs <- DANU_EHR_Labs %>% select(patid, weight, result_date,  test_result)
names(DANU_EHR_Labs)[3] <- "Absolute_Lymphocyte_Date"
names(DANU_EHR_Labs)[4] <- "Absolute_Lymphocyte"



temp <- DANU_Claims  

names(temp)[1] <- "patient"


FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% select(patient)
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)

FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% inner_join(temp)
DIA_Pats_95ConfLiver_2plusHits <- DIA_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Diabetes_Pats %>% group_by(patient) %>% filter(Absolute_Lymphocyte == max(Absolute_Lymphocyte)) %>% slice(1) %>% ungroup()  %>% summarise(n=mean(Absolute_Lymphocyte))
# 7.22
temp2 <- DIA_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Absolute_Lymphocyte == max(Absolute_Lymphocyte)) %>% slice(1) %>% ungroup()   %>% summarise(n=mean(Absolute_Lymphocyte))
# 4.05

temp1$group <- "Low Risk Diabetes"
temp2$group <- "High Risk Diabetes"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Absolute_Lymphocyte, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(0,10) +
  xlab("\n Absolute Lymphocyte Count (x10^3 / uL") + ylab("Diabetes Predicted Group \n")






FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% select(patient)
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)

FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% inner_join(temp)
OBE_Pats_95ConfLiver_2plusHits <- OBE_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Obesity_Pats %>% group_by(patient) %>% filter(Percent_Monos == max(Percent_Monos)) %>% slice(1) %>% ungroup() 
# 9.23
temp2 <- OBE_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Percent_Monos == max(Percent_Monos)) %>% slice(1) %>% ungroup()
# 11.2

temp1$group <- "Low Risk Obesity"
temp2$group <- "High Risk Obesity"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Percent_Monos, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(4,16) +
  xlab("\n Monocytes (%)") + ylab("Obesity Predicted Group \n")
# -----------
# Lymphocytes Percentage High Risk vs Low Risk ------------------------------

DANU_Claims <- fread("DANU Claims Lab Results Sample.txt") 
DANU_Claims <- DANU_Claims %>% filter(tst_desc == "LYMPHOCYTES" & rslt_unit_nm =="%")
DANU_Claims$rslt_nbr <- as.numeric(DANU_Claims$rslt_nbr)
DANU_Claims <- DANU_Claims %>% select(patid, weight, fst_dt, rslt_nbr)
names(DANU_Claims)[3] <- "Percent_Lymphocyte_Date"
names(DANU_Claims)[4] <- "Percent_Lymphocyte"


DANU_NLP_Measurement <- fread("DANU NLP Measurement Sample.txt")
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_type == "LYMPHOCYTE" & measurement_detail == "%")
DANU_NLP_Measurement$measurement_value <- as.numeric(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(!is.na(measurement_value))
range(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% select(patid, weight, note_date, measurement_value)
names(DANU_NLP_Measurement)[3] <- "Percent_Lymphocyte_Date"
names(DANU_NLP_Measurement)[4] <- "Percent_Lymphocyte"


DANU_EHR_Labs <- fread("DANU EHR Labs Sample.txt")
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(test_name=="Lymphocyte.percent" & result_unit == "%")
DANU_EHR_Labs$test_result <- as.numeric(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(!is.na(test_result))
range(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% select(patid, weight, result_date,  test_result)
names(DANU_EHR_Labs)[3] <- "Percent_Lymphocyte_Date"
names(DANU_EHR_Labs)[4] <- "Percent_Lymphocyte"


temp <- DANU_Claims  %>% bind_rows(DANU_NLP_Measurement) %>% bind_rows(DANU_EHR_Labs)

names(temp)[1] <- "patient"


FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% select(patient)
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)

FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% inner_join(temp)
DIA_Pats_95ConfLiver_2plusHits <- DIA_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Diabetes_Pats %>% group_by(patient) %>% filter(Percent_Lymphocyte == mean(Percent_Lymphocyte)) %>% slice(1) %>% ungroup()  %>% summarise(n=mean(Percent_Lymphocyte))
# 21
temp2 <- DIA_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Percent_Lymphocyte == mean(Percent_Lymphocyte)) %>% slice(1) %>% ungroup()   %>% summarise(n=mean(Percent_Lymphocyte))
# 14.7

temp1$group <- "Low Risk Diabetes"
temp2$group <- "High Risk Diabetes"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Percent_Lymphocyte, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(-5,50) +
  xlab("\n Peripheral Lymphocyte %") + ylab("Diabetes Predicted Group \n")




FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% select(patient)
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)

FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% inner_join(temp)
OBE_Pats_95ConfLiver_2plusHits <- OBE_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Obesity_Pats %>% group_by(patient) %>% filter(Percent_Lymphocyte == min(Percent_Lymphocyte)) %>% slice(1) %>% ungroup()#  %>% summarise(n=mean(Percent_Lymphocyte))
# 23.5
temp2 <- OBE_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Percent_Lymphocyte == min(Percent_Lymphocyte)) %>% slice(1) %>% ungroup() #%>% summarise(n=mean(Percent_Lymphocyte))
# 18.8

temp1$group <- "Low Risk Obesity"
temp2$group <- "High Risk Obesity"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Percent_Lymphocyte, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(-5,50) +
  xlab("\n Peripheral Lymphocyte %") + ylab("Obesity Predicted Group \n")

# ---------

# Neutrophils Percentage High Risk vs Low Risk ------------------------------

DANU_Claims <- fread("DANU Claims Lab Results Sample.txt") 
DANU_Claims <- DANU_Claims %>% filter(tst_desc == "NEUTROPHILS" & rslt_unit_nm =="%")
DANU_Claims$rslt_nbr <- as.numeric(DANU_Claims$rslt_nbr)
DANU_Claims <- DANU_Claims %>% select(patid, weight, fst_dt, rslt_nbr)
names(DANU_Claims)[3] <- "Percent_Neutrophil_Date"
names(DANU_Claims)[4] <- "Percent_Neutrophil"


DANU_NLP_Measurement <- fread("DANU NLP Measurement Sample.txt")
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(measurement_type == "NEUTROPHILS"  & measurement_detail == "%")
DANU_NLP_Measurement$measurement_value <- as.numeric(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% filter(!is.na(measurement_value))
range(DANU_NLP_Measurement$measurement_value)
DANU_NLP_Measurement <- DANU_NLP_Measurement %>% select(patid, weight, note_date, measurement_value)
names(DANU_NLP_Measurement)[3] <- "Percent_Neutrophil_Date"
names(DANU_NLP_Measurement)[4] <- "Percent_Neutrophil"


DANU_EHR_Labs <- fread("DANU EHR Labs Sample.txt")
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(test_name=="Neutrophil.percent" & result_unit == "%")
DANU_EHR_Labs$test_result <- as.numeric(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% filter(!is.na(test_result))
range(DANU_EHR_Labs$test_result)
DANU_EHR_Labs <- DANU_EHR_Labs %>% select(patid, weight, result_date,  test_result)
names(DANU_EHR_Labs)[3] <- "Percent_Neutrophil_Date"
names(DANU_EHR_Labs)[4] <- "Percent_Neutrophil"


temp <- DANU_Claims  %>% bind_rows(DANU_NLP_Measurement) %>% bind_rows(DANU_EHR_Labs)

names(temp)[1] <- "patient"


FIB4_Diabetes_Pats <- fread("FIB4_Diabetes_Pats.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% select(patient)
DIA_Pats_95ConfLiver_2plusHits <- fread("DIA_Pats_95ConfLiver_2plusHits.txt")
FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% anti_join(DIA_Pats_95ConfLiver_2plusHits)

FIB4_Diabetes_Pats <- FIB4_Diabetes_Pats %>% inner_join(temp)
DIA_Pats_95ConfLiver_2plusHits <- DIA_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Diabetes_Pats %>% group_by(patient) %>% filter(Percent_Neutrophil == max(Percent_Neutrophil)) %>% slice(1) %>% ungroup() # %>% summarise(n=mean(Percent_Neutrophil))
# 68.7
temp2 <- DIA_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Percent_Neutrophil == max(Percent_Neutrophil)) %>% slice(1) %>% ungroup() #  %>% summarise(n=mean(Percent_Neutrophil))
# 76.5

temp1$group <- "Low Risk Diabetes"
temp2$group <- "High Risk Diabetes"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Percent_Neutrophil, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(30,100) +
  xlab("\n Peripheral Neutrophil %") + ylab("Diabetes Predicted Group \n")




FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% select(patient)
OBE_Pats_95ConfLiver_2plusHits <- fread("OBE_Pats_95ConfLiver_2plusHits.txt")
FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% anti_join(OBE_Pats_95ConfLiver_2plusHits)

FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% inner_join(temp)
OBE_Pats_95ConfLiver_2plusHits <- OBE_Pats_95ConfLiver_2plusHits %>% inner_join(temp)


temp1 <- FIB4_Obesity_Pats %>% group_by(patient) %>% filter(Percent_Neutrophil == max(Percent_Neutrophil)) %>% slice(1) %>% ungroup() #  %>% summarise(n=mean(Percent_Neutrophil))
# 65.9
temp2 <- OBE_Pats_95ConfLiver_2plusHits %>% group_by(patient) %>% filter(Percent_Neutrophil == max(Percent_Neutrophil)) %>% slice(1) %>% ungroup() # %>% summarise(n=mean(Percent_Neutrophil))
# 71.4

temp1$group <- "Low Risk Obesity"
temp2$group <- "High Risk Obesity"

FIB4_Diabetes_Pats$group <- "Diabetes"
DIA_Pats_95ConfLiver_2plusHits$group <- "High Risk Diabetes"

temp1 %>% bind_rows(temp2) %>%
  ggplot(aes(x = Percent_Neutrophil, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(30,100) +
  xlab("\n Peripheral Neutrophil %") + ylab("Obesity Predicted Group \n")
# ------------


NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt") 

NASH_Pats$group <- "NASH"
DIA_Pats$group <- "DIA"
OBE_Pats$group <- "OBE"
NAFLD_Pats$group <- "NAFLD"
Random_Pats$group <- "Random Sample"

temp <- NASH_Pats %>% bind_rows(NAFLD_Pats) %>% bind_rows(DIA_Pats) %>% bind_rows(OBE_Pats, Random_Pats)
temp <- temp %>% group_by(patient) %>% select(-c(AST, ALT, Platelets, age)) %>% arrange(patient, claimed)

Pats_to_track <- temp %>% filter(fibrosis4>2.67) %>% select(patient) %>% distinct()

Pats_to_track <- Pats_to_track %>% left_join(temp) %>% slice(if(any(fibrosis4>2.67)) 1:which.max(fibrosis4>2.67) else row_number()) 

setDT(Pats_to_track)[, Month_Yr := format(as.Date(claimed), "%Y-%m") ]

Pats_to_track <- Pats_to_track %>% group_by(patient) %>% arrange(patient, desc(Month_Yr))

Pats_to_track <- Pats_to_track %>% select(patient, Month_Yr) %>% distinct() %>% group_by(patient) %>% arrange(patient, desc(Month_Yr))

Pats_to_track <- Pats_to_track %>% group_by(patient) %>% mutate(rowN = row_number()) %>%
  group_by(patient) %>% arrange(patient, Month_Yr)

# Using 1.96
Pats_to_track %>% filter(rowN==1) %>% select(patient) %>% distinct() # 91362
Pats_to_track %>% filter(rowN==2) %>% select(patient) %>% distinct() # 38237
Pats_to_track %>% filter(rowN==3) %>% select(patient) %>% distinct() # 22392
Pats_to_track %>% filter(rowN==4) %>% select(patient) %>% distinct() # 13953
Pats_to_track %>% filter(rowN==5) %>% select(patient) %>% distinct() # 9058
Pats_to_track %>% filter(rowN==6) %>% select(patient) %>% distinct() # 6032

# Using 2.97
Pats_to_track %>% filter(rowN==1) %>% select(patient) %>% distinct() # 50644
Pats_to_track %>% filter(rowN==2) %>% select(patient) %>% distinct() # 26192
Pats_to_track %>% filter(rowN==3) %>% select(patient) %>% distinct() # 16457
Pats_to_track %>% filter(rowN==4) %>% select(patient) %>% distinct() # 10894
Pats_to_track %>% filter(rowN==5) %>% select(patient) %>% distinct() # 7368
Pats_to_track %>% filter(rowN==6) %>% select(patient) %>% distinct() # 5156


# Cumulative sum of events per physician - Physicians for high risk patients ------------------

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- fread("NAFLD_Pats_95ConfLiver_2plusHits_Provider.txt")
DIA_Pats_95ConfLiver_2plusHits_Provider <- fread("DIA_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))

Summary_Specialties <- fread("Summary_Specialties.txt")


# NASH 
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
NASH_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(NASH_Pats_95ConfLiver_2plusHits_Provider$prov)

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(ptid, fst_dt) %>% distinct()

tempNASH <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, prov, SUMMARY_SPECIALTY) %>%
  group_by(ptid, fst_dt) %>% distinct() %>% 
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% drop_na() %>%
  group_by(ptid, fst_dt) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup()

tempNASH # 21493

tempNASH <- tempNASH %>% group_by(SUMMARY_SPECIALTY) %>% mutate(N_spec=n()) %>% ungroup() %>% group_by(prov) %>%
  mutate(N_prov=n()) %>% ungroup() %>% select(prov, SUMMARY_SPECIALTY, N_spec, N_prov) %>% distinct() %>% arrange(-N_spec, -N_prov) %>%
  mutate(Cumsum = cumsum(N_prov)) %>% mutate(rowindex = row_number())


tempNASH %>% group_by(SUMMARY_SPECIALTY) %>% slice(n()) %>% arrange(rowindex)


# prov        SUMMARY_SPECIALTY  N_spec N_prov Cumsum rowindex
# <chr>       <chr>               <int>  <int>  <int>    <int>
# 1 14448644850 RADIOLOGY            5570      1   5570     1873
# 2 14447423924 INTERNAL MEDICINE    4500      1  10070     3033
# 3 14448699612 GP                   3286      1  13356     3826
# 4 14448047889 GASTRO/HEPATO        2607      1  15963     4660
# 5 14448767783 OTHER PHYSICIAN      2022      1  17985     5151
# 6 14448885753 OTHER HCP            1032      1  19017     5366
# 7 14447562591 PATHOLOGY             983      1  20000     5628
# 8 14448781233 CARDIOLOGY            707      1  20707     5901
# 9 14448469737 EMERGENCY MEDICINE    411      1  21118     6071
# 10 14448093726 SURGERY               375      1  21493     6249

tempNASH %>% select(SUMMARY_SPECIALTY, N_spec) %>% group_by(SUMMARY_SPECIALTY) %>% mutate(n=n()) %>% mutate(scriptsperphysician=N_spec/n) %>%
  select(SUMMARY_SPECIALTY, scriptsperphysician) %>% distinct()



tempNASH %>% ggplot(aes(rowindex, Cumsum)) +
  geom_line(size=2, colour="deepskyblue4") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Physician Index") + ylab("Cumulative Sum of Events \n")+
  geom_vline(xintercept=c(1873, 3033, 3826, 4660, 5151, 5366, 5628, 5901, 6071, 6249),
             linetype='dashed', color='firebrick')+
  scale_x_discrete(limits=c(1873,3033,3826,4660,5151, 5366, 5628, 5901 ))




# -----------
# Cumulative sum of events per physician - 1st NASH Dx  ------------------


NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% filter(condition=="NASH") %>% 
  group_by(patid) %>% slice(1)

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)


Summary_Specialties <- fread("Summary_Specialties.txt")



tempNASH <-  NASH_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>%
  left_join(Summary_Specialties) %>% select(patid, prov, SUMMARY_SPECIALTY) %>% drop_na() %>%
  mutate(rank = ifelse(SUMMARY_SPECIALTY=="GASTRO/HEPATO",1,
                       ifelse(SUMMARY_SPECIALTY=="PATHOLOGY",2,
                              ifelse(SUMMARY_SPECIALTY=="RADIOLOGY",3,
                                     ifelse(SUMMARY_SPECIALTY=="HEMATO/ONCO",4,
                                            ifelse(SUMMARY_SPECIALTY=="CARDIOLOGY",5,
                                                   ifelse(SUMMARY_SPECIALTY=="EMERGENCY MEDICINE",6,
                                                          ifelse(SUMMARY_SPECIALTY=="INTERNAL MEDICINE",7,
                                                                 ifelse(SUMMARY_SPECIALTY=="GP",8,
                                                                        ifelse(SUMMARY_SPECIALTY=="SURGERY",9,
                                                                               ifelse(SUMMARY_SPECIALTY=="OTHER PHYSICIAN",10,
                                                                                      ifelse(SUMMARY_SPECIALTY=="OTHER HCP",11,NA)))))))))))) %>% 
  drop_na() %>% group_by(patid) %>% filter(rank==min(rank)) %>% slice(1) %>% ungroup()


tempNASH # 7592



tempNASH <- tempNASH %>% group_by(SUMMARY_SPECIALTY) %>% mutate(N_spec=n()) %>% ungroup() %>% group_by(prov) %>%
  mutate(N_prov=n()) %>% ungroup() %>% select(prov, SUMMARY_SPECIALTY, N_spec, N_prov) %>% distinct() %>% arrange(-N_spec, -N_prov) %>%
  mutate(Cumsum = cumsum(N_prov)) %>% mutate(rowindex = row_number())


tempNASH %>% group_by(SUMMARY_SPECIALTY) %>% slice(n()) %>% arrange(rowindex)

# prov         SUMMARY_SPECIALTY  N_spec N_prov Cumsum rowindex
# <chr>        <chr>               <int>  <int>  <int>    <int>
# 1 fbef36dc3af4 GASTRO/HEPATO        2177      1   2177     1473
# 2 411f546409ab INTERNAL MEDICINE    1496      1   3673     2773
# 3 8c5b04ff59a3 GP                   1348      1   5021     3930
# 4 8008e534cbf7 OTHER PHYSICIAN       693      1   5714     4494
# 5 9868ad12b104 PATHOLOGY             533      1   6247     4932
# 6 14449475958  OTHER HCP             473      1   6720     5317
# 7 d76da9b4681b RADIOLOGY             348      1   7068     5621
# 8 d5e2512bad36 SURGERY               315      1   7383     5870
# 9 367e983f9ed9 CARDIOLOGY            116      1   7499     5967
# 10 14447077031  EMERGENCY MEDICINE    103      1   7602     6069



tempNASH %>% select(SUMMARY_SPECIALTY, N_spec) %>% group_by(SUMMARY_SPECIALTY) %>% mutate(n=n()) %>% mutate(scriptsperphysician=N_spec/n) %>%
  select(SUMMARY_SPECIALTY, scriptsperphysician) %>% distinct()


# SUMMARY_SPECIALTY  scriptsperphysician
# <chr>                            <dbl>
# 1 GASTRO/HEPATO                     1.48
# 2 INTERNAL MEDICINE                 1.15
# 3 GP                                1.17
# 4 OTHER PHYSICIAN                   1.23
# 5 PATHOLOGY                         1.22
# 6 OTHER HCP                         1.23
# 7 RADIOLOGY                         1.14
# 8 SURGERY                           1.27
# 9 CARDIOLOGY                        1.20
# 10 EMERGENCY MEDICINE                1.01


tempNASH %>% ggplot(aes(rowindex, Cumsum)) +
  geom_line(size=2, colour="deepskyblue4") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Physician Index") + ylab("Cumulative Sum of Events \n")+
  geom_vline(xintercept=c(1473, 2773, 3930, 4494, 4932, 5317, 5621, 5870, 5967, 6069),
             linetype='dashed', color='firebrick')+
  scale_x_discrete(limits=c(1473, 2773, 3930, 4494, 4932, 5317, 5621, 5870 ))



# ---------
# Onset Discourse ---------------------------
NASH_Onset_Discouse <- fread("NASH Onset Discourse.txt")

data.frame(NASH_Onset_Discouse %>% select(term, during_patient_population) %>% arrange(-during_patient_population))

data.frame(NASH_Onset_Discouse %>% select(term, after_patient_population) %>% arrange(-after_patient_population))

data.frame(NASH_Onset_Discouse %>% select(term, before_patient_population, after_patient_population) %>%
  filter(before_patient_population!=0 & after_patient_population!=0) %>% 
  mutate(fold_change=after_patient_population/before_patient_population) %>%
  arrange(-abs(fold_change)))




NAFLD_Onset_Discouse <- fread("NAFLD Onset Discourse.txt")

data.frame(NAFLD_Onset_Discouse %>% select(term, during_patient_population) %>% arrange(-during_patient_population))

data.frame(NAFLD_Onset_Discouse %>% select(term, after_patient_population) %>% arrange(-after_patient_population))

data.frame(NAFLD_Onset_Discouse %>% select(term, before_patient_population, after_patient_population) %>%
             filter(before_patient_population!=0 & after_patient_population!=0) %>% 
             mutate(fold_change=after_patient_population/before_patient_population) %>%
             arrange(-abs(fold_change)))

# --------

NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Ingredients <- fread("NASH Ingredients.txt")

# Remaglinide 49
# Nateglinide 48





# Persistency / visibility Netaglinide  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(48{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(48{1})(\\D|$)', .), "Nateglinide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Nateglinide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Nateglinide_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Nateglinide_Periods)[3] <- "Duration"

Nateglinide_Periods_VIZ <- Nateglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Nateglinide_Periods_VIZ, "Nateglinide_Periods_VIZ.csv")

temp <- Nateglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                    select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 11.11
weighted.median(temp$Total_duration, temp$n) # 3.5




# Persistency / visibility Remaglinide  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(49{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(49{1})(\\D|$)', .), "Remaglinide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Remaglinide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Remaglinide_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Remaglinide_Periods)[3] <- "Duration"

Remaglinide_Periods_VIZ <- Remaglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Remaglinide_Periods_VIZ, "Remaglinide_Periods_VIZ.csv")

temp <- Remaglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 14.91535
weighted.median(temp$Total_duration, temp$n) # 7.5






# Persistency / visibility Netaglinide 1st episode  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(48{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(48{1})(\\D|$)', .), "Nateglinide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Nateglinide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Nateglinide_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Nateglinide_Periods)[3] <- "Duration"

Nateglinide_Periods <- Nateglinide_Periods %>% group_by(patient) %>% filter(grp == min(grp))
  
Nateglinide_Periods_VIZ <- Nateglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Nateglinide_Periods_VIZ, "Nateglinide_Periods_VIZ_FIRST.csv")

temp <- Nateglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 9.4
weighted.median(temp$Total_duration, temp$n) # 3.5




# Persistency / visibility Remaglinide 1st episode  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(49{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(49{1})(\\D|$)', .), "Remaglinide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Remaglinide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Remaglinide_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Remaglinide_Periods)[3] <- "Duration"

Remaglinide_Periods <- Remaglinide_Periods %>% group_by(patient) %>% filter(grp == min(grp))


Remaglinide_Periods_VIZ <- Remaglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Remaglinide_Periods_VIZ, "Remaglinide_Periods_VIZ_FIRST.csv")

temp <- Remaglinide_Periods %>% left_join(NASH_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 10.9
weighted.median(temp$Total_duration, temp$n) # 4.5
# -----------

# Persistency / visibility Alogliptin  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(55{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(55{1})(\\D|$)', .), "Alogliptin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Alogliptin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Alogliptin_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Alogliptin_Periods)[3] <- "Duration"

Alogliptin_Periods_VIZ <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Alogliptin_Periods_VIZ, "Alogliptin_Periods_VIZ.csv")

temp <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 20
weighted.median(temp$Total_duration, temp$n) # 13.5




# Persistency / visibility Alogliptin 1st episode  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)({1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(55{1})(\\D|$)', .), "Alogliptin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Alogliptin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Alogliptin_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Alogliptin_Periods)[3] <- "Duration"

Alogliptin_Periods <- Alogliptin_Periods %>% group_by(patient) %>% filter(grp == min(grp))

Alogliptin_Periods_VIZ <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Alogliptin_Periods_VIZ, "Alogliptin_Periods_VIZ_FIRST.csv")

temp <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 15
weighted.median(temp$Total_duration, temp$n) # 7.5





# --------


# Persistency / visibility Alogliptin  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(52{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(52{1})(\\D|$)', .), "Alogliptin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Alogliptin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Alogliptin_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Alogliptin_Periods)[3] <- "Duration"

Alogliptin_Periods_VIZ <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Alogliptin_Periods_VIZ, "Alogliptin_Periods_VIZ.csv")

temp <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 10.6
weighted.median(temp$Total_duration, temp$n) # 5.5




# Persistency / visibility Alogliptin 1st episode  -------------------------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(52{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(52{1})(\\D|$)', .), "Alogliptin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Alogliptin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Alogliptin_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Alogliptin_Periods)[3] <- "Duration"

Alogliptin_Periods <- Alogliptin_Periods %>% group_by(patient) %>% filter(grp == min(grp))

Alogliptin_Periods_VIZ <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Alogliptin_Periods_VIZ, "Alogliptin_Periods_VIZ_FIRST.csv")

temp <- Alogliptin_Periods %>% left_join(NASH_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 10
weighted.median(temp$Total_duration, temp$n) # 5.5





# --------

# Check All comorbidites for High Risk pats --------------------------

DIA_Pats_95ConfLiver_2plusHits_Provider <- fread("DIA_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- fread("NAFLD_Pats_95ConfLiver_2plusHits_Provider.txt")
NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")


DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()


DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  

length(unique(DIA_Pats_95ConfLiver_2plusHits_Provider$ptid)) # 4975
length(unique(OBE_Pats_95ConfLiver_2plusHits_Provider$ptid)) # 4979
length(unique(NAFLD_Pats_95ConfLiver_2plusHits_Provider$ptid)) # 4980
length(unique(NASH_Pats_95ConfLiver_2plusHits_Provider$ptid)) # 1357

DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()

DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4975)*100) %>% arrange(-n)
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4979)*100) %>% arrange(-n)
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4980)*100) %>% arrange(-n)
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/1357)*100) %>% arrange(-n)

names(DIA_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"

fwrite(DIA_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskDiabetes.txt", sep="\t")
fwrite(OBE_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskObesity.txt", sep="\t")
fwrite(NAFLD_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskNAFLD.txt", sep="\t")
fwrite(NASH_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskNASH.txt", sep="\t")

# -------
# ------
# DIABETES Persistency / visibility Netaglinide  -------------------------------------------
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)

DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(4:63)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(26{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(26{1})(\\D|$)', .), "Nateglinide"))

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Nateglinide",1,0))

DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <-  fread("DIA Drug Histories.txt")

DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories_LONG <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)


DIA_Drug_Histories <- DIA_Drug_Histories  %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)


DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Nateglinide_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Nateglinide_Periods)[3] <- "Duration"

Nateglinide_Periods_VIZ <- Nateglinide_Periods %>% left_join(DIA_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Nateglinide_Periods_VIZ, "Nateglinide_Periods_VIZ_DIA.csv")

temp <- Nateglinide_Periods %>% left_join(DIA_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 18.68435
weighted.median(temp$Total_duration, temp$n) # 10.5

#892 pats


# ------

# DIABETES Persistency / visibility Remaglinide  -------------------------------------------

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories)

DIA_Drug_Histories <-  DIA_Drug_Histories %>%  select(4:63)

DIA_Drug_Histories <- DIA_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(27{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(27{1})(\\D|$)', .), "Remaglinide"))

DIA_Drug_Histories <-  DIA_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Remaglinide",1,0))

DIA_Drug_Histories[] <-  lapply(DIA_Drug_Histories,as.numeric)

DIA_Drug_Histories_LONG <-  fread("DIA Drug Histories.txt")

DIA_Drug_Histories_LONG <- DIA_Drug_Histories_LONG %>% select(patient, weight)
DIA_Drug_Histories_LONG <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories_LONG)

DIA_Drug_Histories <- DIA_Drug_Histories_LONG %>% bind_cols(DIA_Drug_Histories)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)


DIA_Drug_Histories <- DIA_Drug_Histories  %>% arrange(patient)

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat == 1)


DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Remaglinide_Periods <- DIA_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Remaglinide_Periods)[3] <- "Duration"

Remaglinide_Periods_VIZ <- Remaglinide_Periods %>% left_join(DIA_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Remaglinide_Periods_VIZ, "Remaglinide_Periods_VIZ_DIA.csv")

temp <- Remaglinide_Periods %>% left_join(DIA_Drug_Histories %>% 
                                            select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 16.19606
weighted.median(temp$Total_duration, temp$n) # 8.5

#2127 pats




# -----------
# Time from NASH to Cirrhosis/Fibrosis ------------------------

NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_Cirrhosis_Pats <- NASH_diagnosis %>% filter(NASH_diganosis=="NASH-Cirrohsis") 
NASH_Fibrosis_Pats <- NASH_diagnosis %>% filter(NASH_diganosis=="NASH-Fibrosis") 

NASH_Events <- fread("NASH Events.txt")

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")

NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))

names(NASH_Events)[1] <- "patient"


# NASH Cirrhosis, how many had NASH before or fibrosis before? Time to cirrhosis from NASH or from fibrosis

Pats_to_remove <- NASH_Cirrhosis_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  filter(claimed<"2016-10-02") %>% select(patient) %>% distinct()


Data_First_Cirrhosis <- NASH_Cirrhosis_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1) %>% anti_join(Pats_to_remove)

Data_First_Cirrhosis <- Data_First_Cirrhosis %>% select(patient, weight, claimed)

names(Data_First_Cirrhosis)[3] <- "Date_First_Cirrhosis"


Data_First_NASH <- NASH_Cirrhosis_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="NASH") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)

Data_First_NASH <- Data_First_NASH %>% select(patient, weight, claimed)

names(Data_First_NASH)[3] <- "Date_First_NASH"

sum(Data_First_Cirrhosis$weight) #205937.7

Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 70559

Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH == Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 34428

Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH > Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 100951


Data_First_NASH$Date_First_NASH <- as.Date(Data_First_NASH$Date_First_NASH)
Data_First_Cirrhosis$Date_First_Cirrhosis <- as.Date(Data_First_Cirrhosis$Date_First_Cirrhosis)


Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Cirrhosis) %>% ungroup() %>% select(patient) %>% 
  left_join(Data_First_Cirrhosis %>% left_join(Data_First_NASH)) %>%
  mutate(Elapsed_Time = as.numeric((Date_First_Cirrhosis-Date_First_NASH)/30.5)) %>%
  select(Elapsed_Time) %>%
  ggplot(aes(Elapsed_Time)) +
  geom_density()+
  geom_density(colour="darkslategray4", fill="darkslategray4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n Elapsed Time From 1st NASH Dx to 1st Cirrhosis Dx")



Data_First_Fibrosis <- NASH_Cirrhosis_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Fibrosis") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)

Data_First_Fibrosis <- Data_First_Fibrosis %>% select(patient, weight, claimed)

names(Data_First_Fibrosis)[3] <- "Date_First_Fibrosis"

sum(Data_First_Fibrosis$weight) #69839.74 (only 23% has Fibrosis)

Data_First_Cirrhosis %>% inner_join(Data_First_Fibrosis) %>% ungroup %>% summarise(n=sum(weight)) # 49157

Data_First_Cirrhosis %>% inner_join(Data_First_Fibrosis) %>% 
  filter(Date_First_Fibrosis < Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 19339

Data_First_Cirrhosis %>% inner_join(Data_First_Fibrosis) %>% 
  filter(Date_First_Fibrosis == Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 4812

Data_First_Cirrhosis %>% inner_join(Data_First_Fibrosis) %>% 
  filter(Date_First_Fibrosis > Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 25006


Data_First_Fibrosis$Date_First_Fibrosis <- as.Date(Data_First_Fibrosis$Date_First_Fibrosis)
Data_First_Cirrhosis$Date_First_Cirrhosis <- as.Date(Data_First_Cirrhosis$Date_First_Cirrhosis)

Data_First_Cirrhosis %>% left_join(Data_First_Fibrosis) %>% drop_na() %>% 
  filter(Date_First_Fibrosis < Date_First_Cirrhosis) %>% ungroup() %>% select(patient) %>% 
  left_join(Data_First_Cirrhosis %>% left_join(Data_First_Fibrosis)) %>%
  mutate(Elapsed_Time = as.numeric((Date_First_Cirrhosis-Date_First_Fibrosis)/30.5)) %>%
  select(Elapsed_Time) %>%
  ggplot(aes(Elapsed_Time)) +
  geom_density()+
  geom_density(colour="darkslategray4", fill="darkslategray4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n Elapsed Time From 1st Fibrosis Dx to 1st Cirrhosis Dx")





# NASH Fibrosis, how many had NASH before or fibrosis before? Time to cirrhosis from NASH or from fibrosis

Pats_to_remove <- NASH_Fibrosis_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Fibrosis") %>%
  filter(claimed<"2016-10-02") %>% select(patient) %>% distinct()


NASH_Fibrosis_Pats <- NASH_Fibrosis_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Fibrosis") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1) %>% anti_join(Pats_to_remove)

NASH_Fibrosis_Pats <- NASH_Fibrosis_Pats %>% select(patient, weight, claimed)

names(NASH_Fibrosis_Pats)[3] <- "Date_First_Fibrosis"


Data_First_NASH <- NASH_Fibrosis_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="NASH") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)

Data_First_NASH <- Data_First_NASH %>% select(patient, weight, claimed)

names(Data_First_NASH)[3] <- "Date_First_NASH"

sum(NASH_Fibrosis_Pats$weight) #82843

NASH_Fibrosis_Pats %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 30360

NASH_Fibrosis_Pats %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH == Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 34890

NASH_Fibrosis_Pats %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH > Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 17593


Data_First_NASH$Date_First_NASH <- as.Date(Data_First_NASH$Date_First_NASH)
NASH_Fibrosis_Pats$Date_First_Fibrosis <- as.Date(NASH_Fibrosis_Pats$Date_First_Fibrosis)


NASH_Fibrosis_Pats %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Fibrosis) %>% ungroup() %>% select(patient) %>% 
  left_join(NASH_Fibrosis_Pats %>% left_join(Data_First_NASH)) %>%
  mutate(Elapsed_Time = as.numeric((Date_First_Fibrosis-Date_First_NASH)/30.5)) %>%
  select(Elapsed_Time) %>%
  ggplot(aes(Elapsed_Time)) +
  geom_density()+
  geom_density(colour="darkslategray4", fill="darkslategray4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n Elapsed Time From 1st NASH Dx to 1st Fibrosis Dx")


# -----
# Time from NAFLD to Cirrhosis/Fibrosis -------------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NAFLD_Pats <- NASH_diagnosis %>% filter(NASH_diganosis=="NAFLD") 


NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
names(NASH_Events)[1] <- "patient"

# NAFLD Cirrhosis
Pats_to_remove <- NAFLD_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  filter(claimed<"2017-10-02") %>% select(patient) %>% distinct()

Data_First_Cirrhosis <- NAFLD_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1) %>% anti_join(Pats_to_remove)

Data_First_Cirrhosis <- Data_First_Cirrhosis %>% select(patient, weight, claimed)

names(Data_First_Cirrhosis)[3] <- "Date_First_Cirrhosis"


Data_First_NAFLD <- Data_First_Cirrhosis %>% left_join(NASH_Events) %>% 
  filter(condition=="NAFLD") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)

Data_First_NAFLD <- Data_First_NAFLD %>% select(patient, weight, claimed)

names(Data_First_NAFLD)[3] <- "Date_First_NAFLD"

sum(Data_First_Cirrhosis$weight) #354294.8

Data_First_Cirrhosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD < Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 225096

Data_First_Cirrhosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD == Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 42759

Data_First_Cirrhosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD > Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 86439


Data_First_NAFLD$Date_First_NAFLD <- as.Date(Data_First_NAFLD$Date_First_NAFLD)
Data_First_Cirrhosis$Date_First_Cirrhosis <- as.Date(Data_First_Cirrhosis$Date_First_Cirrhosis)

# Mean 20.8
Data_First_Cirrhosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD < Date_First_Cirrhosis) %>% ungroup() %>% select(patient) %>% 
  left_join(Data_First_Cirrhosis %>% left_join(Data_First_NAFLD)) %>%
  mutate(Elapsed_Time = as.numeric((Date_First_Cirrhosis-Date_First_NAFLD)/30.5)) %>%
  select(Elapsed_Time) %>%
  ggplot(aes(Elapsed_Time)) +
  geom_density()+
  geom_density(colour="darkslategray4", fill="darkslategray4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n Elapsed Time From 1st NAFLD Dx to 1st Cirrhosis Dx")



# NAFLD Fibrosis 

NASH_diagnosis <- fread("NASH_diagnosis.txt")
NAFLD_Pats <- NASH_diagnosis %>% filter(NASH_diganosis=="NAFLD") 


NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
names(NASH_Events)[1] <- "patient"


Pats_to_remove <- NAFLD_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Fibrosis") %>%
  filter(claimed<"2017-10-02") %>% select(patient) %>% distinct()

Data_First_Fibrosis <- NAFLD_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Fibrosis") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1) %>% anti_join(Pats_to_remove)

Data_First_Fibrosis <- Data_First_Fibrosis %>% select(patient, weight, claimed)

names(Data_First_Fibrosis)[3] <- "Date_First_Fibrosis"


Data_First_NAFLD <- Data_First_Fibrosis %>% left_join(NASH_Events) %>% 
  filter(condition=="NAFLD") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)

Data_First_NAFLD <- Data_First_NAFLD %>% select(patient, weight, claimed)

names(Data_First_NAFLD)[3] <- "Date_First_NAFLD"

sum(Data_First_Fibrosis$weight) #146903.3

Data_First_Fibrosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD < Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 97981

Data_First_Fibrosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD == Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 27618

Data_First_Fibrosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD > Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 21304


Data_First_NAFLD$Date_First_NAFLD <- as.Date(Data_First_NAFLD$Date_First_NAFLD)
Data_First_Fibrosis$Date_First_Fibrosis <- as.Date(Data_First_Fibrosis$Date_First_Fibrosis)

# Mean 17.1
Data_First_Fibrosis %>% left_join(Data_First_NAFLD) %>% 
  filter(Date_First_NAFLD < Date_First_Fibrosis) %>% ungroup() %>% select(patient) %>% 
  left_join(Data_First_Fibrosis %>% left_join(Data_First_NAFLD)) %>%
  mutate(Elapsed_Time = as.numeric((Date_First_Fibrosis-Date_First_NAFLD)/30.5)) %>%
  select(Elapsed_Time) %>%
  ggplot(aes(Elapsed_Time)) +
  geom_density()+
  geom_density(colour="darkslategray4", fill="darkslategray4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n Elapsed Time From 1st NAFLD Dx to 1st Fibrosis Dx")



# --------


# Time from NASH to Cirrhosis/Fibrosis -------------------
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_Pats <- NASH_diagnosis %>% filter(NASH_diganosis!="NAFLD" & NASH_diganosis!="Other Liver") 


NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
names(NASH_Events)[1] <- "patient"

# NASH Cirrhosis
Pats_to_remove <- NASH_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  filter(claimed<"2017-10-02") %>% select(patient) %>% distinct()

Data_First_Cirrhosis <- NASH_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Cirrhosis"|condition=="Liver Failure") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1) %>% anti_join(Pats_to_remove)

Data_First_Cirrhosis <- Data_First_Cirrhosis %>% select(patient, weight, claimed)

names(Data_First_Cirrhosis)[3] <- "Date_First_Cirrhosis"


Data_First_NASH <- Data_First_Cirrhosis %>% left_join(NASH_Events) %>% 
  filter(condition=="NASH") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)

Data_First_NASH <- Data_First_NASH %>% select(patient, weight, claimed)

names(Data_First_NASH)[3] <- "Date_First_NASH"

sum(Data_First_Cirrhosis$weight) #160190.4

Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 60244

Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH == Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 27254

Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH > Date_First_Cirrhosis) %>% ungroup() %>% summarise(n=sum(weight)) # 72693


Data_First_NASH$Date_First_NASH <- as.Date(Data_First_NASH$Date_First_NASH)
Data_First_Cirrhosis$Date_First_Cirrhosis <- as.Date(Data_First_Cirrhosis$Date_First_Cirrhosis)

# Mean 18.9   , 1180
Data_First_Cirrhosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Cirrhosis) %>% ungroup() %>% select(patient) %>% 
  left_join(Data_First_Cirrhosis %>% left_join(Data_First_NASH)) %>%
  mutate(Elapsed_Time = as.numeric((Date_First_Cirrhosis-Date_First_NASH)/30.5)) %>%
  select(Elapsed_Time) %>%
  ggplot(aes(Elapsed_Time)) +
  geom_density()+
  geom_density(colour="darkslategray4", fill="darkslategray4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n Elapsed Time From 1st NASH Dx to 1st Cirrhosis Dx")



# NASH Fibrosis 
NASH_diagnosis <- fread("NASH_diagnosis.txt")
NASH_Pats <- NASH_diagnosis %>% filter(NASH_diganosis!="NASH" & NASH_diganosis!="Other Liver") 


NASH_Events <- fread("NASH Events.txt")
NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition))
names(NASH_Events)[1] <- "patient"


Pats_to_remove <- NASH_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Fibrosis") %>%
  filter(claimed<"2017-10-02") %>% select(patient) %>% distinct()

Data_First_Fibrosis <- NASH_Pats %>% left_join(NASH_Events) %>% 
  filter(condition=="Fibrosis") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1) %>% anti_join(Pats_to_remove)

Data_First_Fibrosis <- Data_First_Fibrosis %>% select(patient, weight, claimed)

names(Data_First_Fibrosis)[3] <- "Date_First_Fibrosis"


Data_First_NASH <- Data_First_Fibrosis %>% left_join(NASH_Events) %>% 
  filter(condition=="NASH") %>%
  group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)

Data_First_NASH <- Data_First_NASH %>% select(patient, weight, claimed)

names(Data_First_NASH)[3] <- "Date_First_NASH"

sum(Data_First_Fibrosis$weight) #146903.3

Data_First_Fibrosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 97981

Data_First_Fibrosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH == Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 27618

Data_First_Fibrosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH > Date_First_Fibrosis) %>% ungroup() %>% summarise(n=sum(weight)) # 21304


Data_First_NASH$Date_First_NASH <- as.Date(Data_First_NASH$Date_First_NASH)
Data_First_Fibrosis$Date_First_Fibrosis <- as.Date(Data_First_Fibrosis$Date_First_Fibrosis)

# Mean 18.8
Data_First_Fibrosis %>% left_join(Data_First_NASH) %>% 
  filter(Date_First_NASH < Date_First_Fibrosis) %>% ungroup() %>% select(patient) %>% 
  left_join(Data_First_Fibrosis %>% left_join(Data_First_NASH)) %>%
  mutate(Elapsed_Time = as.numeric((Date_First_Fibrosis-Date_First_NASH)/30.5)) %>%
  select(Elapsed_Time) %>%
  ggplot(aes(Elapsed_Time)) +
  geom_density()+
  geom_density(colour="darkslategray4", fill="darkslategray4", alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Proportion \n")+xlab("\n Elapsed Time From 1st NASH Dx to 1st Fibrosis Dx")









# -----------------

# Calculate Percentage of NASH patients with each comorbidty ------------

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)


# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, weight, diagnosis)

# # NAFLD Vector
# NAFLD_Drug_histories <- fread("NAFLD Drug Histories.txt")
# NAFLD_Drug_histories <- NAFLD_Drug_histories %>% select(patient) %>% mutate(condition_2="NAFLD")


# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient, weight) %>% mutate(diagnosis="Obesity")



NASH_Drug_histories <- fread("NASH Drug Histories.txt")
NASH_Drug_histories <- NASH_Drug_histories %>% select(patient, weight) %>% mutate(diagnosisNASH="NASH")

Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories) %>% left_join(NASH_Drug_histories) %>% group_by(diagnosis, diagnosisNASH) %>% count()



diagnosis          diagnosisNASH          n
<chr>              <chr>              <dbl>
  1 Diabetes           NASH              45373.
2 Diabetes           NA              7904342.
3 Diabetes + Obesity NASH             746901.
4 Diabetes + Obesity NA             39536059.
5 Obesity            NASH             478765.
6 Obesity            NA            106002033.


# Diabetes-only : (45373./(7904342.+45373.))*100   = 0.57075%  45373. out of  7949715
# Diabetes + Obesity : (746901./(39536059.+746901.))*100   = 1.854136%  746901. out of  40282960
#  Obesity : (478765./(106002033.+478765.))*100   = 0.4496257%  478765. out of  106480798



NASH_Drug_histories %>% left_join(Diabetes_OBesity_Pats) %>% left_join(OBE_Drug_Histories, by=c("patient"="patient")) %>%
  filter(is.na(diagnosis.y)) %>% group_by(diagnosis.x) %>% summarise(n=sum(weight))

# diagnosis.x              n
# <chr>                <dbl>
# 1 Diabetes            45373.
# 2 Diabetes + Obesity 746901.
# 3 NA                  68944.

NASH_Drug_histories %>% left_join(Diabetes_OBesity_Pats) %>% left_join(OBE_Drug_Histories, by=c("patient"="patient")) %>%
  filter(!is.na(diagnosis.y)) %>% group_by(diagnosis.y) %>% summarise(n=sum(weight))


Diabetes + Obesity 746901. (56%)
Diabetes            45373. (3%)
Obesity             478765 (36%)
NASH only            69  5%


# ---------------

# Proportion ever treated with GLP1s --------------------------
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories_Final <- NASH_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))%>% anti_join(OBE_Drug_Histories %>% select(patient))


NAFLD_Drug_Histories <- fread("NAFLD Drug Histories.txt")
NAFLD_Drug_Histories_Final <-  NAFLD_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient)) %>% anti_join(OBE_Drug_Histories %>% select(patient))

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")


OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories_Final <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient)) 


NASH_Drug_Histories <- NASH_Drug_Histories_Final
NAFLD_Drug_Histories <- NAFLD_Drug_Histories_Final
DIA_Drug_Histories
OBE_Drug_Histories <- OBE_Drug_Histories_Final

rm(NASH_Drug_Histories_Final, OBE_Drug_Histories_Final, NAFLD_Drug_Histories_Final)





sum(NASH_Drug_Histories$weight) # 68944.33
NASH_Ingredients       <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% select(molecule, drug_group)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease, Month))

names(NASH_Drug_Histories)[3] <- "molecule"
NASH_Drug_Histories$molecule <- as.character(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) %>% filter(!is.na(drug_group))

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient, weight) %>% distinct()

NASH_Drug_Histories <- data.frame(NASH_Drug_Histories %>% group_by(drug_group) %>% 
                                    summarise(sum_weights = sum(as.numeric(weight))))


# drug_group sum_weights
# 1  Anticholesterol    26782.22
# 2      Antiobesity     3967.54
# 3 Hepatoprotective     1643.33
# 4  Hospitalization      701.08



sum(NAFLD_Drug_Histories$weight) # 14081980

NASH_Ingredients       <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% select(molecule, drug_group)

NAFLD_Drug_Histories <- gather(NAFLD_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
NAFLD_Drug_Histories <- separate_rows(NAFLD_Drug_Histories)

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(-c(disease, Month))

names(NAFLD_Drug_Histories)[3] <- "molecule"
NAFLD_Drug_Histories$molecule <- as.character(NAFLD_Drug_Histories$molecule)

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% left_join(NASH_Ingredients) %>% filter(!is.na(drug_group))

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% group_by(patient, weight) %>% distinct()

NAFLD_Drug_Histories <- data.frame(NAFLD_Drug_Histories %>% group_by(drug_group) %>% 
                                     summarise(sum_weights = sum(as.numeric(weight))))


# drug_group sum_weights
# 1  Anticholesterol   425836.13
# 2     Antidiabetic     1204.32
# 3      Antiobesity    68781.04
# 4  GLP1 Injectable       96.58
# 5 Hepatoprotective    10748.11
# 6  Hospitalization    14823.77



sum(DIA_Drug_Histories$weight) # 48244424
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories)

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-c(disease, Month))

names(DIA_Drug_Histories)[3] <- "molecule"
DIA_Drug_Histories$molecule <- as.character(DIA_Drug_Histories$molecule)

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_group))

DIA_Drug_Histories <- DIA_Drug_Histories %>% group_by(patient, weight) %>% distinct()

DIA_Drug_Histories <- data.frame(DIA_Drug_Histories %>% group_by(drug_group) %>% 
                                   summarise(sum_weights = sum(as.numeric(weight))))

# drug_group sum_weights
# 1    Antidiabetic  4858356.86
# 2            DPP4  1634164.52
# 3 GLP1 Injectable  2319699.50
# 4       GLP1 Oral    84751.81
# 5         Insulin 11195189.71
# 6           SGLT2  1392048.68


sum(OBE_Drug_Histories$weight) # 106457299
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)

OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat!="-") %>% filter(!is.na(Treat))
OBE_Drug_Histories <- separate_rows(OBE_Drug_Histories)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-c(disease, Month))

names(OBE_Drug_Histories)[3] <- "molecule"
OBE_Drug_Histories$molecule <- as.character(OBE_Drug_Histories$molecule)

OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_group))

OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient, weight) %>% distinct()

OBE_Drug_Histories <- data.frame(OBE_Drug_Histories %>% group_by(drug_group) %>% 
                                   summarise(sum_weights = sum(as.numeric(weight))))

# drug_group sum_weights
# 1     Antiobesity  2723751.27
# 2 GLP1 Injectable   255256.26
# 3       GLP1 Oral    23430.49
# 4         Surgery   746366.92
# -----------------
# Summary table for Bella ------------------

# Split NASH into types of NASH based on FIB4 + Age

FIB4_Bucket_Fibrosis <- fread("FIB4_Bucket_Fibrosis.txt")

NASH_Drug_Histories <- fread("NASH Drug Histories.txt")

FIB4_Bucket_Fibrosis <- FIB4_Bucket_Fibrosis %>% left_join(NASH_Drug_Histories %>% select(patient, weight)) 


DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

DIA_Drug_Histories %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)

FIB4_Bucket_Fibrosis <- FIB4_Bucket_Fibrosis %>% left_join(Diabetes_OBesity_Pats) %>% mutate(diagnosis = ifelse(is.na(diagnosis), "NASH", diagnosis))

sum(FIB4_Bucket_Fibrosis$weight)

FIB4_Bucket_Fibrosis$weight <- FIB4_Bucket_Fibrosis$weight * 2.605148

sum(FIB4_Bucket_Fibrosis$weight)

FIB4_Bucket_Fibrosis %>% group_by(diagnosis, FIB4_Bucket) %>% summarise(n=sum(weight)/1000)


# Biopsy 
NASH_Dossiers <- fread("NASH Dossiers.txt")
Biopsy_pats <- NASH_Dossiers %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct()

Biopsy_pats <- Biopsy_pats %>% mutate(Biopsy_Status = "Biopsy")
names(Biopsy_pats)[1] <- "patient"

data.frame(FIB4_Bucket_Fibrosis %>% left_join(Biopsy_pats) %>% group_by(diagnosis, FIB4_Bucket, Biopsy_Status) %>%
             summarise(n=sum(weight)/1000))

# diagnosis    FIB4_Bucket Biopsy_Status           n
# 1            Diabetes       Fibrosis        Biopsy   0.8920027
# 2            Diabetes       Fibrosis          <NA>   7.8176584
# 3            Diabetes NASH-Cirrhosis        Biopsy   1.5735875
# 4            Diabetes NASH-Cirrhosis          <NA>   4.2920855
# 5            Diabetes      NASH-only        Biopsy   0.5903265
# 6            Diabetes      NASH-only          <NA>  10.4248123
# 7  Diabetes + Obesity       Fibrosis        Biopsy  39.9956128
# 8  Diabetes + Obesity       Fibrosis          <NA> 210.5096354
# 9  Diabetes + Obesity NASH-Cirrhosis        Biopsy  47.4155433
# 10 Diabetes + Obesity NASH-Cirrhosis          <NA> 152.5978206
# 11 Diabetes + Obesity      NASH-only        Biopsy  32.3720119
# 12 Diabetes + Obesity      NASH-only          <NA> 289.6004438
# 13               NASH       Fibrosis        Biopsy   1.7595170
# 14               NASH       Fibrosis          <NA>   9.3588379
# 15               NASH NASH-Cirrhosis        Biopsy   0.2710396
# 16               NASH NASH-Cirrhosis          <NA>   0.5493215
# 17               NASH      NASH-only        Biopsy   2.1523472
# 18               NASH      NASH-only          <NA>  21.7309202
# 19            Obesity       Fibrosis        Biopsy  24.6639521
# 20            Obesity       Fibrosis          <NA> 133.8540152
# 21            Obesity NASH-Cirrhosis        Biopsy  20.2064136
# 22            Obesity NASH-Cirrhosis          <NA>  37.1969805
# 23            Obesity      NASH-only        Biopsy  31.0159282
# 24            Obesity      NASH-only          <NA> 259.1424080



# Ultrasound 
NASH_Dossiers <- fread("NASH Dossiers.txt")
Ultrasound_pats <- NASH_Dossiers %>% filter(condition == "Liver Ultrasound") %>% select(patid) %>% distinct()

Ultrasound_pats <- Ultrasound_pats %>% mutate(Ultrasound_pats = "Ultrasound")
names(Ultrasound_pats)[1] <- "patient"

data.frame(FIB4_Bucket_Fibrosis %>% left_join(Ultrasound_pats) %>% group_by(diagnosis, FIB4_Bucket, Ultrasound_pats) %>%
             summarise(n=sum(weight)/1000))

# diagnosis    FIB4_Bucket Ultrasound_pats           n
# 1            Diabetes       Fibrosis      Ultrasound   0.5117032
# 2            Diabetes       Fibrosis            <NA>   8.1979579
# 3            Diabetes NASH-Cirrhosis      Ultrasound   2.4641313
# 4            Diabetes NASH-Cirrhosis            <NA>   3.4015417
# 5            Diabetes      NASH-only      Ultrasound   1.0592011
# 6            Diabetes      NASH-only            <NA>   9.9559378
# 7  Diabetes + Obesity       Fibrosis      Ultrasound  32.1177234
# 8  Diabetes + Obesity       Fibrosis            <NA> 218.3875248
# 9  Diabetes + Obesity NASH-Cirrhosis      Ultrasound  42.5810659
# 10 Diabetes + Obesity NASH-Cirrhosis            <NA> 157.4322980
# 11 Diabetes + Obesity      NASH-only      Ultrasound  35.8145847
# 12 Diabetes + Obesity      NASH-only            <NA> 286.1578709
# 13               NASH       Fibrosis      Ultrasound   0.6050456
# 14               NASH       Fibrosis            <NA>  10.5133092
# 15               NASH NASH-Cirrhosis      Ultrasound   0.2829191
# 16               NASH NASH-Cirrhosis            <NA>   0.5374420
# 17               NASH      NASH-only      Ultrasound   2.2781759
# 18               NASH      NASH-only            <NA>  21.6050915
# 19            Obesity       Fibrosis      Ultrasound  22.8680412
# 20            Obesity       Fibrosis            <NA> 135.6499261
# 21            Obesity NASH-Cirrhosis      Ultrasound  15.2114592
# 22            Obesity NASH-Cirrhosis            <NA>  42.1919349
# 23            Obesity      NASH-only      Ultrasound  27.8416074
# 24            Obesity      NASH-only            <NA> 262.3167288




# Liver Imaging 
NASH_Dossiers <- fread("NASH Dossiers.txt")
Imaging_pats <- NASH_Dossiers %>% filter(condition == "Liver Imaging") %>% select(patid) %>% distinct()

Imaging_pats <- Imaging_pats %>% mutate(Imaging_Status = "Imaging")
names(Imaging_pats)[1] <- "patient"

data.frame(FIB4_Bucket_Fibrosis %>% left_join(Imaging_pats) %>% group_by(diagnosis, FIB4_Bucket, Imaging_Status) %>%
             summarise(n=sum(weight)/1000))

# 1            Diabetes       Fibrosis        Imaging   0.2516052
# 2            Diabetes       Fibrosis           <NA>   8.4580559
# 3            Diabetes NASH-Cirrhosis        Imaging   0.3802995
# 4            Diabetes NASH-Cirrhosis           <NA>   5.4853736
# 5            Diabetes      NASH-only        Imaging   0.6363595
# 6            Diabetes      NASH-only           <NA>  10.3787794
# 7  Diabetes + Obesity       Fibrosis        Imaging  15.7401479
# 8  Diabetes + Obesity       Fibrosis           <NA> 234.7651003
# 9  Diabetes + Obesity NASH-Cirrhosis        Imaging  16.1352968
# 10 Diabetes + Obesity NASH-Cirrhosis           <NA> 183.8780671
# 11 Diabetes + Obesity      NASH-only        Imaging  18.3062187
# 12 Diabetes + Obesity      NASH-only           <NA> 303.6662370
# 13               NASH       Fibrosis        Imaging   0.3087361
# 14               NASH       Fibrosis           <NA>  10.8096188
# 15               NASH NASH-Cirrhosis           <NA>   0.8203611
# 16               NASH      NASH-only        Imaging   1.4227755
# 17               NASH      NASH-only           <NA>  22.4604919
# 18            Obesity       Fibrosis        Imaging   7.8922959
# 19            Obesity       Fibrosis           <NA> 150.6256715
# 20            Obesity NASH-Cirrhosis        Imaging   3.2603948
# 21            Obesity NASH-Cirrhosis           <NA>  54.1429993
# 22            Obesity      NASH-only        Imaging  13.7114931
# 23            Obesity      NASH-only           <NA> 276.4468431


data.frame(FIB4_Bucket_Fibrosis %>% group_by(diagnosis, FIB4_Bucket) %>%
             summarise(n=sum(weight)/1000))

# diagnosis    FIB4_Bucket           n
# 1            Diabetes       Fibrosis   8.7096611
# 2            Diabetes NASH-Cirrhosis   5.8656731
# 3            Diabetes      NASH-only  11.0151389
# 4  Diabetes + Obesity       Fibrosis 250.5052483
# 5  Diabetes + Obesity NASH-Cirrhosis 200.0133639
# 6  Diabetes + Obesity      NASH-only 321.9724557
# 7                NASH       Fibrosis  11.1183548
# 8                NASH NASH-Cirrhosis   0.8203611
# 9                NASH      NASH-only  23.8832674
# 10            Obesity       Fibrosis 158.5179673
# 11            Obesity NASH-Cirrhosis  57.4033941
# 12            Obesity      NASH-only 290.1583362

FIB4_Bucket_Fibrosis %>% left_join(Biopsy_pats) %>% left_join(Ultrasound_pats) %>% left_join(Imaging_pats) %>%
  filter(Biopsy_Status=="Biopsy" | Ultrasound_pats=="Ultrasound" | Imaging_Status=="Imaging") %>% group_by(diagnosis, FIB4_Bucket) %>%
  summarise(n=sum(weight)/1000)

# diagnosis          FIB4_Bucket         n
# <chr>              <chr>           <dbl>
#   1 Diabetes           Fibrosis        0.892
# 2 Diabetes           NASH-Cirrhosis  2.46 
# 3 Diabetes           NASH-only       1.70 
# 4 Diabetes + Obesity Fibrosis       75.1  
# 5 Diabetes + Obesity NASH-Cirrhosis 85.0  
# 6 Diabetes + Obesity NASH-only      73.4  
# 7 NASH               Fibrosis        2.67 
# 8 NASH               NASH-Cirrhosis  0.554
# 9 NASH               NASH-only       5.54 
# 10 Obesity            Fibrosis       47.4  
# 11 Obesity            NASH-Cirrhosis 31.1  
# 12 Obesity            NASH-only      65.1  


FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_NASH_Pats <- FIB4_NASH_Pats %>% select(patient) %>% distinct() %>% mutate(FIB4_Status="FIB4")

NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Pats <- NASH_Drug_Histories %>% select(patient)

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

DIA_Drug_Histories %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)


NASH_Pats <- NASH_Pats %>% left_join(Diabetes_OBesity_Pats) %>% mutate(diagnosis = ifelse(is.na(diagnosis), "NASH", diagnosis))

NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories %>% select(patient, weight)

NASH_Pats %>% left_join(NASH_Drug_Histories %>% select(patient, weight)) %>%
  left_join(FIB4_NASH_Pats) %>% group_by(diagnosis, FIB4_Status) %>% summarise(n=sum(weight)) 

# diagnosis          FIB4_Status       n
# <chr>              <chr>         <dbl>
# 1 Diabetes           FIB4         11778.
# 2 Diabetes           NA           33595.
# 3 Diabetes + Obesity FIB4        363763.
# 4 Diabetes + Obesity NA          383138.
# 5 NASH               FIB4         16827.
# 6 NASH               NA           52117.
# 7 Obesity            FIB4        231582.
# 8 Obesity            NA          247182.

sum(NASH_Drug_Histories$weight)
# 
# NASH_Pats %>% left_join(NASH_Drug_Histories %>% select(patient, weight)) %>%
#   left_join(FIB4_NASH_Pats) %>% left_join(Biopsy_pats) %>% left_join(Ultrasound_pats) %>% left_join(Imaging_pats) %>%
#   filter(Biopsy_Status=="Biopsy" | Ultrasound_pats=="Ultrasound" | Imaging_Status=="Imaging" | FIB4_Status=="FIB4") %>% 
#   summarise(n=sum(weight) 
#             
#             
#             FIB4_Bucket_Fibrosis
#             
#             
#             
#             
#             NASH_diagnosis <- fread("NASH_diagnosis.txt")
#             NASH_diagnosis %>% filter(grepl("NASH",NASH_diganosis)) %>% summarise(n=sum(weight)) # 1339983
#             NASH_diagnosis <- NASH_diagnosis %>%filter(grepl("NASH",NASH_diganosis))
#             
#             NASH_Events <- fread("NASH Events.txt")
#             NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
#             
#             NASH_Events <- NASH_Events %>% left_join(NASH_Diagnosis_Codes %>% select(code, condition)) %>% filter(condition=="NASH") %>% 
#               group_by(patid) %>% slice(1)
#             
#             Summary_Specialties <- fread("Summary_Specialties.txt")
#             
#             NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
#             NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)
#             
#             NASH_Events <- NASH_diagnosis %>% select(patient, NASH_diganosis) %>% left_join(NASH_Events, by=c("patient"="patid"))
#             
#             temp <- data.frame(NASH_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>% left_join(Summary_Specialties) %>%
#                                  select(patient, SUMMARY_SPECIALTY))
#             
#             
#             temp2 <- data.frame(FIB4_Bucket_Fibrosis %>% left_join(temp) %>% group_by(diagnosis, FIB4_Bucket, SUMMARY_SPECIALTY) %>%
#                                   summarise(n=sum(weight)) %>% spread(key=diagnosis, value=n)) %>% arrange(FIB4_Bucket, -Diabetes)
#             
#             fwrite(temp2, "First_NASH_Dx_Specialty_NASHTypeVScomorbidity.txt" , sep="\t")
#             
# ---------
            
# Continue ------
# Biopsy 
# NASH_Dossiers <- fread("NASH Dossiers.txt")
# Biopsy_pats <- NASH_Dossiers %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct()
# Biopsy_pats <- Biopsy_pats %>% mutate(BiopsyStatus = "BIopsy")
#             
# FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
# FIB4_NASH_Pats <- FIB4_NASH_Pats %>% left_join(Biopsy_pats, by=c("patient"="patid"))
# FIB4_NASH_Pats <- FIB4_NASH_Pats %>% mutate(BiopsyStatus=ifelse(is.na(BiopsyStatus), "No", BiopsyStatus))
#             
# FIB4_NASH_Pats %>% group_by(BiopsyStatus, patient) %>% filter(AST==max(AST)) %>% slice(1) %>%
#   ungroup() %>% group_by(BiopsyStatus) %>% summarise(n=mean(AST))
#             
#             # BiopsyStatus     n
#             # <chr>        <dbl>
#             # 1 BIopsy       141. 
#             # 2 No            65.8
#             
#             
# FIB4_NASH_Pats %>% group_by(BiopsyStatus, patient) %>% filter(ALT==max(ALT)) %>% slice(1) %>%
#   ungroup() %>% group_by(BiopsyStatus) %>% summarise(n=mean(ALT))
#             
#             # BiopsyStatus     n
#             # <chr>        <dbl>
#             # 1 BIopsy       121. 
#             # 2 No            77.2
#             
#             
#             
# FIB4_NASH_Pats %>% group_by(BiopsyStatus, patient) %>% filter(Platelets==max(Platelets)) %>% slice(1) %>%
#   ungroup() %>% group_by(BiopsyStatus) %>% summarise(n=mean(Platelets))
#             
#             # BiopsyStatus     n
#             # <chr>        <dbl>
#             # 1 BIopsy        227
#             # 2 No            234
#             
#             
# FIB4_NASH_Pats %>% group_by(BiopsyStatus, patient) %>% filter(age==max(age)) %>% slice(1) %>%
#   ungroup() %>% group_by(BiopsyStatus) %>% summarise(n=mean(age))
#             # 
#             # BiopsyStatus     n
#             # <chr>        <dbl>
#             #   1 BIopsy        55.0
#             # 2 No            56.9
#             
#             
# FIB4_NASH_Pats %>% group_by(BiopsyStatus, patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>%
#               ungroup() %>% group_by(BiopsyStatus) %>% summarise(n=mean(fibrosis4))
#             
#             # BiopsyStatus     n
#             # <chr>        <dbl>
#             # 1 BIopsy        9.50
#             # 2 No            4.87
#             
#             
#             
# # Diagnosis / FIB4 Bucket 
# FIB4_Bucket_Fibrosis <- fread("FIB4_Bucket_Fibrosis.txt")
#             
# NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
#             
# FIB4_Bucket_Fibrosis <- FIB4_Bucket_Fibrosis %>% left_join(NASH_Drug_Histories %>% select(patient, weight)) 
#             
#             
# DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
# DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)
#             
# DANU_Demographics <- fread("DANU Demographics.txt")
# DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
# names(DANU_Demographics)[1] <- "patient"
#             
# DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)
#             
# DIA_Drug_Histories %>% group_by(diagnosis) %>% summarise(n=sum(weight))
#             
# # DIA / OBE Vector
# Diabetes_OBesity_Pats <- DIA_Drug_Histories
# Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)
#             
# # OBE Vector
# OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
# OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
# OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))
# Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)
#             
# FIB4_Bucket_Fibrosis <- FIB4_Bucket_Fibrosis %>% left_join(Diabetes_OBesity_Pats) %>% mutate(diagnosis = ifelse(is.na(diagnosis), "NASH", diagnosis))
#             
# sum(FIB4_Bucket_Fibrosis$weight)
#             
# FIB4_Bucket_Fibrosis$weight <- FIB4_Bucket_Fibrosis$weight * 2.605148
#             
# sum(FIB4_Bucket_Fibrosis$weight)
#             
# FIB4_Bucket_Fibrosis %>% group_by(diagnosis, FIB4_Bucket) %>% summarise(n=sum(weight)/1000)
#             
#             
# FIB4_NASH_Pats <- FIB4_NASH_Pats %>% left_join(FIB4_Bucket_Fibrosis)
#             
#             
# data.frame(FIB4_NASH_Pats %>% drop_na() %>% group_by(patient) %>% filter(AST==max(AST)) %>% slice(1) %>%
#                          ungroup() %>% group_by(diagnosis, FIB4_Bucket, BiopsyStatus) %>% summarise(n=mean(AST)) %>%
#                          spread(key=diagnosis, value=n))
#             
#             # FIB4_Bucket BiopsyStatus  Diabetes Diabetes...Obesity      NASH   Obesity
#             # 1       Fibrosis       BIopsy 109.66667          139.08500  61.80000 179.00000
#             # 2       Fibrosis           No  60.72727           84.42356 126.92593 109.92308
#             # 3 NASH-Cirrhosis       BIopsy  48.50000          117.83553  33.00000 158.26667
#             # 4 NASH-Cirrhosis           No  48.75000           81.86618  30.00000  79.63000
#             # 5      NASH-only       BIopsy  46.50000           47.26471  33.80000  47.65882
#             # 6      NASH-only           No  29.74074           35.47368  29.94828  33.66961
#             
#             
#             
# data.frame(FIB4_NASH_Pats %>% drop_na() %>% group_by(patient) %>% filter(ALT==max(ALT)) %>% slice(1) %>%
#                          ungroup() %>% group_by(diagnosis, FIB4_Bucket, BiopsyStatus) %>% summarise(n=mean(ALT))%>% spread(key=diagnosis, value=n))
#             
#             
#             # FIB4_Bucket BiopsyStatus  Diabetes Diabetes...Obesity      NASH   Obesity
#             # 1       Fibrosis       BIopsy 344.33333          131.92500  61.40000 199.94186
#             # 2       Fibrosis           No  80.13636           99.80725 102.33333 128.19744
#             # 3 NASH-Cirrhosis       BIopsy  51.50000           87.66447  39.00000 141.85000
#             # 4 NASH-Cirrhosis           No  49.83333           72.83455  33.50000 100.67500
#             # 5      NASH-only       BIopsy  53.50000           72.67647  44.40000  88.82353
#             # 6      NASH-only           No  38.00000           53.72103  43.87931  52.79016
#             
#             
# data.frame(FIB4_NASH_Pats %>% drop_na() %>% group_by(patient) %>% filter(Platelets==min(Platelets)) %>% slice(1) %>%
#                          ungroup() %>% group_by(diagnosis, FIB4_Bucket, BiopsyStatus) %>% summarise(n=mean(Platelets))%>%
#                          spread(key=diagnosis, value=n))
#             
#             #  
#             # FIB4_Bucket BiopsyStatus Diabetes Diabetes...Obesity     NASH  Obesity
#             # 1       Fibrosis       BIopsy 173.1333           172.6958 152.7780 173.6081
#             # 2       Fibrosis           No 176.4091           177.0049 169.8593 169.5094
#             # 3 NASH-Cirrhosis       BIopsy 118.6250           159.1382 210.0000 173.1533
#             # 4 NASH-Cirrhosis           No 102.0000           134.7282 257.5000 174.0590
#             # 5      NASH-only       BIopsy 253.0000           264.1078 279.8000 254.4118
#             # 6      NASH-only           No 258.0000           252.9187 259.7414 254.5297
#             
#             
# data.frame(FIB4_NASH_Pats %>% drop_na() %>% group_by(patient) %>% filter(age==max(age)) %>% slice(1) %>%
#                          ungroup() %>% group_by(diagnosis, FIB4_Bucket, BiopsyStatus) %>% summarise(n=mean(age))%>%
#                          spread(key=diagnosis, value=n))
#             
#             # FIB4_Bucket BiopsyStatus Diabetes Diabetes...Obesity     NASH  Obesity
#             # 1       Fibrosis       BIopsy 65.33333           55.85833 60.00000 55.39535
#             # 2       Fibrosis           No 63.27273           60.09555 54.14815 58.26410
#             # 3 NASH-Cirrhosis       BIopsy 62.00000           59.56579 55.00000 59.06667
#             # 4 NASH-Cirrhosis           No 70.16667           64.24818 54.00000 62.43000
#             # 5      NASH-only       BIopsy 47.50000           47.92157 45.20000 44.91765
#             # 6      NASH-only           No 57.37037           54.35097 52.06897 50.73227
#             
#             
#             
# 
# data.frame(FIB4_NASH_Pats %>% drop_na() %>%  group_by(patient) %>% filter(fibrosis4==max(fibrosis4)) %>% slice(1) %>%
#              ungroup() %>% group_by(diagnosis, FIB4_Bucket, BiopsyStatus) %>% summarise(n=mean(fibrosis4))%>%
#              spread(key=diagnosis, value=n))
#             
#             # FIB4_Bucket BiopsyStatus  Diabetes Diabetes...Obesity      NASH   Obesity
#             # 1       Fibrosis       BIopsy 25.395134           6.130464 2.5778500 6.3447116
#             # 2       Fibrosis           No  2.469815           4.008494 8.8127392 9.5238278
#             # 3 NASH-Cirrhosis       BIopsy 13.804488          10.820951 1.3839648 9.8122781
#             # 4 NASH-Cirrhosis           No  7.740836          12.204167 1.4385890 3.5840762
#             # 5      NASH-only       BIopsy  1.099442           0.984236 0.9281548 0.8820136
#            # 6      NASH-only           No  1.109823           1.081661 0.9945616 0.9695418

# ---------------

# NASH Onsets -----------------

          
# What drugs increase the most after 1st NASH Dx ?
# Only drugs that already existed, only population >1000 afterwards (~10 pats)

NASH_Onset_Drug <- fread("NASH Onset Drug.txt")
          
Tops_Drugs_Increase <- NASH_Onset_Drug %>% select(drug, before_patient_population, after_patient_population) %>% 
  mutate(fold_change = after_patient_population/before_patient_population) %>% arrange(-fold_change)

data.frame(Tops_Drugs_Increase %>% filter(fold_change != "Inf" & fold_change>2 & after_patient_population>1000))

# Antidiabetic drugs?
# Antibiotics ?
# It does seem that the 1st NASH dx is associated with increased use of antidiabetics and glucose monitorining,
# most notably semaglutide

# drug before_patient_population after_patient_population fold_change
# 1                     Valganciclovir hcl                    100.94                  2537.62   25.139885
# 2              Blood-glucose transmitter                    216.89                  2820.74   13.005394
# 3                  Rapid-acting insulins                    593.47                  7163.31   12.070214
# 4                            Semaglutide                    967.94                 11016.02   11.380891
# 5                              Meropenem                    108.40                  1036.48    9.561624
# 6         Blood-glucose meter,continuous                    224.48                  1915.61    8.533544
# 7         Kit for prep tc-99m/mebrofenin                    138.05                  1028.21    7.448099
# 8                   Blood-glucose sensor                    343.61                  2538.41    7.387474
# 9         Fluticasone/umeclidin/vilanter                    262.36                  1572.33    5.993025
# 10                           Telmisartan                    596.29                  3187.76    5.345989
# 11   Penicillinase-resistant penicillins                    211.35                  1058.05    5.006151
# 12         Ciprofloxacin in 5 % dextrose                    395.61                  1780.29    4.500114
# 13                         Ascorbic acid                    338.20                  1510.59    4.466558
# 14                             Vitamin c                    338.20                  1510.59    4.466558
# 15          Varicella-zoster ge/as01b/pf                   7160.27                 31634.79    4.418100
# 16                    Sodium bicarbonate                    478.25                  2054.00    4.294825
# 17        Varicella-zoster ge vac,2 of 2                    315.95                  1326.15    4.197341
# 18             Heparin sodium,porcine/pf                    416.36                  1689.64    4.058123
# 19         Cefazolin sodium/dextrose,iso                    813.75                  3299.47    4.054648
# 20             Potassium-removing agents                    466.29                  1810.11    3.881940
# 21        Irbesartan/hydrochlorothiazide                    368.63                  1421.55    3.856306
# 22          Heparin sodium,porcine/ns/pf                    312.74                  1173.85    3.753437
# 23                        Liver function                    493.35                  1836.26    3.722023
# 24                   Gadoxetate disodium                    493.35                  1836.26    3.722023
# 25              Succinylcholine chloride                    875.04                  3190.45    3.646062
# 26                     Sugammadex sodium                    294.18                  1062.23    3.610817
# 27                            Calcitriol                    540.19                  1843.53    3.412744
# 28         Measles,mumps,rubella vacc/pf                    453.07                  1521.77    3.358797
# 29        Flu vacc quad 2018-19(6mos up)                   1120.87                  3734.00    3.331341
# 30                   Polyene antifungals                    972.75                  3202.97    3.292696
# 31        Kit for prep tc-99m/tetrofosmn                    741.76                  2429.31    3.275062
# 32                  Flash glucose sensor                   2264.63                  7388.86    3.262723
# 33   Polyenes (skin and mucous membrane)                   2106.52                  6632.06    3.148349
# 34                    Alcohol deterrents                    323.00                  1015.54    3.144087
# 35                            Disulfiram                    323.00                  1015.54    3.144087
# 36        Blood pressure test kit-medium                    399.37                  1254.68    3.141648
# 37          Multivitamin with folic acid                    772.36                  2393.23    3.098594
# 38                  Mycophenolate sodium                    452.42                  1386.90    3.065514
# 39                       Cariprazine hcl                    678.92                  2063.52    3.039416




NAFLD_Onset_Drug <- fread("NAFLD Onset Drug.txt")

Tops_Drugs_Increase <- NAFLD_Onset_Drug %>% select(drug, before_patient_population, after_patient_population) %>% 
  mutate(fold_change = after_patient_population/before_patient_population) %>% arrange(-fold_change)

data.frame(Tops_Drugs_Increase %>% filter(fold_change != "Inf" & fold_change>2 & after_patient_population>1000))

data.frame(Tops_Drugs_Increase %>% filter(fold_change != "Inf" & fold_change>2 & after_patient_population>1000))



# What Procedures increase the most after 1st NASH Dx ?
# Only procedures that already existed, only population >1000 afterwards (~10 pats)
# Nothing interesting

NASH_Onset_Procedure <- fread("NASH Onset Procedure.txt")

Tops_Procedures_Increase <- NASH_Onset_Procedure %>% select(description, before_patient_population, after_patient_population) %>% 
  mutate(fold_change = after_patient_population/before_patient_population) %>% arrange(-fold_change) %>% drop_na()

Tops_Procedures_Increase <- data.frame(Tops_Procedures_Increase %>% filter(fold_change != "Inf" & fold_change>2 & after_patient_population>1000))


# Liver allotransplantation, orthotopic, partial or whole, from cadaver or living donor, any age       21.078284 
# Transfusion of nonautologous platelets into peripheral vein                                          15.887953
# Supply allowance for therapeutic continuous glucose moni                                             14.337613
# Ultrasound, elastography; parenchyma (eg, organ)                                                     12.844429




NASH_Onset_Diagnosis <- fread("NASH Onset Diagnosis.txt")

Tops_Procedures_Diagnosis <- NASH_Onset_Diagnosis %>% select(description, before_patient_population, after_patient_population) %>% 
  mutate(fold_change = after_patient_population/before_patient_population) %>% arrange(-fold_change) %>% drop_na()

Tops_Procedures_Diagnosis <- data.frame(Tops_Procedures_Diagnosis %>% filter(fold_change != "Inf" & fold_change>2 & after_patient_population>1000))

data.frame(Tops_Procedures_Diagnosis %>% filter(fold_change != "Inf" & fold_change>2 & after_patient_population>1000))


# -------------
# DIA vs OBE vs DIA+OBE Comorbidity penetrance --------

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

DIA_Drug_Histories %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)



DIA_Pats_95ConfLiver_2plusHits_Provider <- fread("DIA_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- fread("NAFLD_Pats_95ConfLiver_2plusHits_Provider.txt")
NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")

DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()

names(DIA_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"


DIAandOBE_Pats_95ConfLiver_2plusHits_Provider  <- Diabetes_OBesity_Pats %>%  filter(diagnosis == "Diabetes + Obesity") %>% select(patient) %>% inner_join(DIA_Pats_95ConfLiver_2plusHits_Provider)
DIA_Pats_95ConfLiver_2plusHits_Provider  <- Diabetes_OBesity_Pats %>%  filter(diagnosis == "Diabetes") %>% select(patient) %>% inner_join(DIA_Pats_95ConfLiver_2plusHits_Provider)
OBE_Pats_95ConfLiver_2plusHits_Provider  <- Diabetes_OBesity_Pats %>%  filter(diagnosis == "Obesity") %>% select(patient) %>% inner_join(OBE_Pats_95ConfLiver_2plusHits_Provider)
NAFLD_Pats_95ConfLiver_2plusHits_Provider
NASH_Pats_95ConfLiver_2plusHits_Provider


DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- DIAandOBE_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  


length(unique(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider$patient)) # 4567
length(unique(DIA_Pats_95ConfLiver_2plusHits_Provider$patient)) # 407
length(unique(OBE_Pats_95ConfLiver_2plusHits_Provider$patient)) # 4979
length(unique(NAFLD_Pats_95ConfLiver_2plusHits_Provider$patient)) # 4980
length(unique(NASH_Pats_95ConfLiver_2plusHits_Provider$patient)) # 1357

DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- DIAandOBE_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()

DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- DIAandOBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4567)*100) %>% arrange(-n)
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/407)*100) %>% arrange(-n)
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4979)*100) %>% arrange(-n)
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4980)*100) %>% arrange(-n)
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/1357)*100) %>% arrange(-n)

names(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(DIA_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"



diag_lookup <- fread("diag_lookup.txt", sep="\t")

DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider) 
DIA_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(DIA_Pats_95ConfLiver_2plusHits_Provider) 
OBE_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(OBE_Pats_95ConfLiver_2plusHits_Provider) 
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(NAFLD_Pats_95ConfLiver_2plusHits_Provider) 
NASH_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(NASH_Pats_95ConfLiver_2plusHits_Provider) 



fwrite(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskDiabetesPlusObe_new.txt", sep="\t")
fwrite(DIA_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskDiabetes_new.txt", sep="\t")
fwrite(OBE_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskObesity_new.txt", sep="\t")
fwrite(NAFLD_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskNAFLD_new.txt", sep="\t")
fwrite(NASH_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskNASH_new.txt", sep="\t")



# -------------
# DIA vs OBE vs DIA+OBE Comorbidity penetrance SPlit NAFLD into NAFLD with/without comorb --------

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

DIA_Drug_Histories %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)



DIA_Pats_95ConfLiver_2plusHits_Provider <- fread("DIA_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- fread("NAFLD_Pats_95ConfLiver_2plusHits_Provider.txt")
NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")
Rand_pts_ICD10_lst5y_dx <- fread("Rand_pts_ICD10_lst5y_dx.txt")

DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
Rand_pts_ICD10_lst5y_dx <- Rand_pts_ICD10_lst5y_dx %>% select(ptid, diag) %>% distinct()


names(DIA_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
names(Rand_pts_ICD10_lst5y_dx)[1] <- "patient"



DIAandOBE_Pats_95ConfLiver_2plusHits_Provider  <- Diabetes_OBesity_Pats %>%  filter(diagnosis == "Diabetes + Obesity") %>% select(patient) %>% inner_join(DIA_Pats_95ConfLiver_2plusHits_Provider)
DIA_Pats_95ConfLiver_2plusHits_Provider  <- Diabetes_OBesity_Pats %>%  filter(diagnosis == "Diabetes") %>% select(patient) %>% inner_join(DIA_Pats_95ConfLiver_2plusHits_Provider)
OBE_Pats_95ConfLiver_2plusHits_Provider  <- Diabetes_OBesity_Pats %>%  filter(diagnosis == "Obesity") %>% select(patient) %>% inner_join(OBE_Pats_95ConfLiver_2plusHits_Provider)
NAFLD_Pats_95ConfLiver_2plusHits_Provider
NASH_Pats_95ConfLiver_2plusHits_Provider
Rand_pts_ICD10_lst5y_dx

DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- DIAandOBE_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  

length(unique(Rand_pts_ICD10_lst5y_dx$patient)) #9999

Rand_comorb_pts_ICD10_lst5y_dx <- Rand_pts_ICD10_lst5y_dx %>% filter(grepl("E65",diag)| grepl("E66",diag)| grepl("E67",diag)|  grepl("E68",diag)|
                                                                       grepl("E08",diag)| grepl("E09",diag)| grepl("E10",diag)|
                                                                       grepl("E11",diag)|  grepl("E12",diag)|  grepl("E13",diag)| grepl("E14",diag)|
                                                                       grepl("E15",diag)| grepl("E16",diag)| grepl("K70",diag)| grepl("K71",diag)|
                                                                       grepl("K72",diag)| grepl("K73",diag)| grepl("K74",diag)| grepl("K75",diag)|
                                                                       grepl("K76",diag)| grepl("K77",diag))%>% select(patient) %>% distinct() %>%
  left_join(Rand_pts_ICD10_lst5y_dx)


Rand_only_pts_ICD10_lst5y_dx <- Rand_pts_ICD10_lst5y_dx %>% anti_join(Rand_pts_ICD10_lst5y_dx %>% filter(grepl("E65",diag)| grepl("E66",diag)| grepl("E67",diag)|  grepl("E68",diag)|
                                                                                                           grepl("E08",diag)| grepl("E09",diag)| grepl("E10",diag)|
                                                                                                           grepl("E11",diag)|  grepl("E12",diag)|  grepl("E13",diag)| grepl("E14",diag)|
                                                                                                           grepl("E15",diag)| grepl("E16",diag)| grepl("K70",diag)| grepl("K71",diag)|
                                                                                                           grepl("K72",diag)| grepl("K73",diag)| grepl("K74",diag)| grepl("K75",diag)|
                                                                                                           grepl("K76",diag)| grepl("K77",diag))%>% select(patient) %>% distinct())





length(unique(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider$patient)) # 4567
length(unique(DIA_Pats_95ConfLiver_2plusHits_Provider$patient)) # 407
length(unique(OBE_Pats_95ConfLiver_2plusHits_Provider$patient)) # 4979
length(unique(NAFLD_Pats_95ConfLiver_2plusHits_Provider$patient)) # 4980
length(unique(NASH_Pats_95ConfLiver_2plusHits_Provider$patient)) # 1357

DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- DIAandOBE_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()


# NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(Diabetes_OBesity_Pats %>% select(patient))
# NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% anti_join(Diabetes_OBesity_Pats %>% select(patient))



Rand_comorb_pts_ICD10_lst5y_dx <- Rand_comorb_pts_ICD10_lst5y_dx %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))
Rand_only_pts_ICD10_lst5y_dx <- Rand_only_pts_ICD10_lst5y_dx %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))


DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- DIAandOBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4567)*100) %>% arrange(-n)
DIA_Pats_95ConfLiver_2plusHits_Provider <- DIA_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/407)*100) %>% arrange(-n)
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4979)*100) %>% arrange(-n)
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4980)*100) %>% arrange(-n)
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/1357)*100) %>% arrange(-n)


# length(unique(NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider$patient)) #4825
# length(unique(NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider$patient)) #155

# length(unique(Rand_comorb_pts_ICD10_lst5y_dx$patient)) #6238
# length(unique(Rand_only_pts_ICD10_lst5y_dx$patient)) #3761




# NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/4825)*100) %>% arrange(-n)
# NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider <- NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider %>% group_by(diag) %>% count() %>% mutate(n=(n/155)*100) %>% arrange(-n)

Rand_comorb_pts_ICD10_lst5y_dx <- Rand_comorb_pts_ICD10_lst5y_dx %>% select(patient, diag) %>% distinct()
Rand_only_pts_ICD10_lst5y_dx <- Rand_only_pts_ICD10_lst5y_dx %>% select(patient, diag) %>% distinct()


# Rand_comorb_pts_ICD10_lst5y_dx <- Rand_comorb_pts_ICD10_lst5y_dx %>% group_by(diag) %>% count() %>% mutate(n=(n/6238)*100) %>% arrange(-n)
# Rand_only_pts_ICD10_lst5y_dx <- Rand_only_pts_ICD10_lst5y_dx %>% group_by(diag) %>% count() %>% mutate(n=(n/3761)*100) %>% arrange(-n)




names(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(DIA_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(NAFLD_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[2] <- "Proportion"




diag_lookup <- fread("diag_lookup.txt", sep="\t")

DIAandOBE_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider) 
DIA_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(DIA_Pats_95ConfLiver_2plusHits_Provider) 
OBE_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(OBE_Pats_95ConfLiver_2plusHits_Provider) 
NAFLD_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(NAFLD_Pats_95ConfLiver_2plusHits_Provider) 
NASH_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(NASH_Pats_95ConfLiver_2plusHits_Provider) 

#NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider) 
#NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider) 
#Rand_comorb_pts_ICD10_lst5y_dx <- diag_lookup %>% left_join(Rand_comorb_pts_ICD10_lst5y_dx) 
#Rand_only_pts_ICD10_lst5y_dx <- diag_lookup %>% left_join(Rand_only_pts_ICD10_lst5y_dx) 



fwrite(DIAandOBE_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskDiabetesPlusObe_new.txt", sep="\t")
fwrite(DIA_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskDiabetes_new.txt", sep="\t")
fwrite(OBE_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskObesity_new.txt", sep="\t")
fwrite(NAFLD_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskNAFLD_new.txt", sep="\t")
fwrite(NASH_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskNASH_new.txt", sep="\t")


fwrite(NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider, "NAFLD_Comorb_Pats_95ConfLiver_2plusHits_Provider.txt", sep="\t")
fwrite(NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider, "NAFLD_Only_Pats_95ConfLiver_2plusHits_Provider", sep="\t")



fwrite(Rand_comorb_pts_ICD10_lst5y_dx, "Rand_comorb_pts_ICD10_lst5y_dx.txt", sep="\t")
fwrite(Rand_only_pts_ICD10_lst5y_dx, "Rand_only_pts_ICD10_lst5y_dx.txt", sep="\t")

# -----
# Split NAFLD into comorbid vs NAFLD only --------

NAFLD_Pats_95ConfLiver_2plusHits <- fread("NAFLD_Pats_95ConfLiver_2plusHits.txt")


DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

DIA_Drug_Histories %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient)



NAFLD_Pats_95ConfLiver_2plusHits %>% left_join(Diabetes_OBesity_Pats %>% left_join(DANU_Demographics)) %>%
  group_by(diagnosis) %>% count()

# ---------


# BMI distribution within those predicted as high risk -----------------
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% select(patid, code)


OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid) %>% distinct()


OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% left_join(DANU_Events, by=c("ptid"="patid")) %>% drop_na()

OBE_Pats_95ConfLiver_2plusHits_Provider %>% mutate(bucket= ifelse(code<25, "<25",
                                                                  ifelse(code>=25&code<=30,"25-30",
                                                                         ifelse(code>30&code<=35,"30-35",
                                                                                ifelse(code>35&code<=40, "35-40",">40"))))) %>%
  group_by(bucket) %>% count()

# >40 618/4756 (0.1299411%)
# 35-40 681/4756 (0.1431876%)
#  30-35 1447/4756 (0.3042473%)
#  <30 (1939+71)/4756 (0.4226241%)

mean(OBE_Pats_95ConfLiver_2plusHits_Provider$code)




# BMI dis for entire Obe population
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% select(patid, code)


DANU_Events <- DANU_Events %>% mutate(bucket= ifelse(code<25, "<25",
                                                     ifelse(code>=25&code<=30,"25-30",
                                                            ifelse(code>30&code<=35,"30-35",
                                                                   ifelse(code>35&code<=40, "35-40",">40")))))



#OBE with tests
FIB4_Obesity_Pats <- fread("FIB4_Obesity_Pats.txt")

FIB4_Obesity_Pats <- FIB4_Obesity_Pats %>% select(patient) %>% inner_join(DANU_Events, by=c("patient"="patid"))

OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")

OBE_Pats_Score95_2plus <- OBE_Pats_Score95_2plus %>% mutate(HighRisk = "HighRisk")

FIB4_Obesity_Pats %>% group_by(bucket) %>% count()

FIB4_Obesity_Pats %>% select(patient, bucket) %>% distinct() %>% 
  left_join(OBE_Pats_Score95_2plus) %>% group_by(HighRisk) %>% count()


bucket HighRisk      n
<chr>  <chr>     <int>
  1 <25    HighRisk    268
2 <25    NA         5110
3 >40    HighRisk   2591
4 >40    NA        25874
5 25-30  HighRisk   7288
6 25-30  NA       112489
7 30-35  HighRisk   5597
8 30-35  NA        68002
9 35-40  HighRisk   2905
10 35-40  NA        32745

# ---------










# Files for t-SNE classification in python ---------------


All_NASH <- NASH_Probability %>% select(patient) %>% distinct()

HighRisk_NASH <- NASH_Probability %>% filter(Risk_pred_model.....predict.NASH_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() 

All_DIA <- DIA_Probability %>% select(patient) %>% distinct()

HighRisk_DIA <- DIA_Probability %>% filter(Risk_pred_model.....predict.DIA_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() 

All_OBE <- OBE_Probability %>% select(patient) %>% distinct()

HighRisk_OBE <- OBE_Probability %>% filter(Risk_pred_model.....predict.OBE_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() 

All_NAFLD <- NAFLD_Probability %>% select(patient) %>% distinct()

HighRisk_NAFLD <- NAFLD_Probability %>% filter(Risk_pred_model.....predict.NAFLD_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() 


All_Random <- Random_Probability %>% select(patient) %>% distinct()

HighRisk_Random <- Random_Probability %>% filter(Risk_pred_model.....predict.Random_Pats..type....response..>0.95) %>%
  select(patient) %>% distinct() 


fwrite(All_NASH, "All_NASH.txt", sep="\t")
fwrite(All_DIA, "All_DIA.txt", sep="\t")
fwrite(All_OBE, "All_OBE.txt", sep="\t")
fwrite(All_NAFLD, "All_NAFLD.txt", sep="\t")
fwrite(All_Random, "All_Random.txt", sep="\t")

fwrite(HighRisk_NASH, "HighRisk_NASH.txt", sep="\t")
fwrite(HighRisk_DIA, "HighRisk_DIA.txt", sep="\t")
fwrite(HighRisk_OBE, "HighRisk_OBE.txt", sep="\t")
fwrite(HighRisk_NAFLD, "HighRisk_NAFLD.txt", sep="\t")
fwrite(HighRisk_Random, "HighRisk_Random.txt", sep="\t")


All_NASH <- fread("All_NASH.txt")
All_DIA <- fread("All_DIA.txt")
All_OBE <- fread("All_OBE.txt")
All_NAFLD <- fread("All_NAFLD.txt")
All_Random <- fread("All_Random.txt")


HighRisk_NASH <- fread("HighRisk_NASH.txt")
HighRisk_DIA <- fread("HighRisk_DIA.txt")
HighRisk_OBE <- fread("HighRisk_OBE.txt")
HighRisk_NAFLD <- fread("HighRisk_NAFLD.txt")
HighRisk_Random <- fread("HighRisk_Random.txt")

All_NASH <- All_NASH %>% anti_join(HighRisk_NASH)
All_DIA <- All_DIA %>% anti_join(HighRisk_DIA)
All_OBE <- All_OBE %>% anti_join(HighRisk_OBE)
All_NAFLD <- All_NAFLD %>% anti_join(HighRisk_NAFLD)
All_Random <- All_Random %>% anti_join(HighRisk_Random)


All_NASH <- All_NASH %>% mutate(Group = "Low_Risk")
All_DIA <- All_DIA %>% mutate(Group = "Low_Risk")
All_OBE <- All_OBE %>% mutate(Group = "Low_Risk")
All_NAFLD <- All_NAFLD %>% mutate(Group = "Low_Risk")
All_Random <- All_Random %>% mutate(Group = "Low_Risk")



HighRisk_NASH <- HighRisk_NASH %>% mutate(Group = "High_Risk")
HighRisk_DIA <- HighRisk_DIA %>% mutate(Group = "High_Risk")
HighRisk_OBE <- HighRisk_OBE %>% mutate(Group = "High_Risk")
HighRisk_NAFLD <- HighRisk_NAFLD %>% mutate(Group = "High_Risk")
HighRisk_Random <- HighRisk_Random %>% mutate(Group = "High_Risk")

ALL_NASH_Preds <- All_NASH %>% bind_rows(All_DIA) %>% bind_rows(All_OBE) %>% bind_rows(All_NAFLD) %>% bind_rows(All_Random) %>% 
  bind_rows(HighRisk_NASH) %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE) %>% 
  bind_rows(HighRisk_NAFLD) %>% bind_rows(HighRisk_Random)



# Get Pats with all labs on the same date 
NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt") 

All_records <- NASH_Pats %>% bind_rows(DIA_Pats) %>% bind_rows(OBE_Pats) %>% bind_rows(NAFLD_Pats) %>% bind_rows(Random_Pats)

ALL_NASH_Preds <- ALL_NASH_Preds %>% left_join(All_records) %>% select(-c(patient, claimed))

fwrite(ALL_NASH_Preds, "ALL_NASH_Preds.txt", sep="\t")

ALL_NASH_Preds <- fread("ALL_NASH_Preds.txt", sep="\t")
ALL_NASH_Preds <- ALL_NASH_Preds %>% drop_na() %>% distinct()
fwrite(ALL_NASH_Preds, "ALL_NASH_Preds.txt", sep="\t")







DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")

HighRisk_DIA <- fread("HighRisk_DIA.txt")
All_DIA <- fread("All_DIA.txt", sep="\t")
All_DIA <- All_DIA %>% anti_join(HighRisk_DIA)

All_DIA <- All_DIA %>% left_join(DIA_Pats) %>% mutate(Group="Low")
HighRisk_DIA <- HighRisk_DIA %>% left_join(DIA_Pats) %>% mutate(Group="High")

Diabetes_To_Predict <- All_DIA %>% bind_rows(HighRisk_DIA) %>% distinct() %>% select(-c("patient", "claimed"))

fwrite(Diabetes_To_Predict, "Diabetes_To_Predict.txt", sep="\t")



Random_Pats <- fread("FIB4_Random_Pats.txt")

HighRisk_Random <- fread("HighRisk_Random.txt")
All_Random <- fread("All_Random.txt", sep="\t")
All_Random <- All_Random %>% anti_join(HighRisk_Random)

All_Random <- All_Random %>% left_join(Random_Pats) %>% mutate(Group="Low")
HighRisk_Random <- HighRisk_Random %>% left_join(Random_Pats) %>% mutate(Group="High")

Random_To_Predict <- All_Random %>% bind_rows(HighRisk_Random) %>% distinct() %>% select(-c("patient", "claimed"))

fwrite(Random_To_Predict, "Random_To_Predict.txt", sep="\t")





NASH_Pats <- fread("FIB4_NASH_Pats.txt")

HighRisk_NASH <- fread("HighRisk_NASH.txt")
All_NASH <- fread("All_NASH.txt", sep="\t")
All_NASH <- All_NASH %>% anti_join(HighRisk_NASH)

All_NASH <- All_NASH %>% left_join(NASH_Pats) %>% mutate(Group="Low")
HighRisk_NASH <- HighRisk_NASH %>% left_join(NASH_Pats) %>% mutate(Group="High")

NASH_To_Predict <- All_NASH %>% bind_rows(HighRisk_NASH) %>% distinct() %>% select(-c("patient", "claimed"))


NASH_To_Predict <- HighRisk_NASH %>% arrange(Group, AST, ALT) %>% slice_tail(n=20000) %>% 
  bind_rows(All_NASH %>% arrange(Group, AST, ALT) %>% slice_head(n=20000)) 


NASH_To_Predict <- HighRisk_NASH %>% arrange(Group, AST, ALT) %>%
  bind_rows(All_NASH %>% arrange(Group, AST, ALT)) %>% group_by(patient) %>%
  filter(AST==max(AST)) %>% slice(1) %>% ungroup() %>% select(-c("patient","claimed"))

fwrite(NASH_To_Predict, "NASH_To_Predict.txt", sep="\t")






NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")

HighRisk_NAFLD <- fread("HighRisk_NAFLD.txt")
All_NAFLD <- fread("All_NAFLD.txt", sep="\t")
All_NAFLD <- All_NAFLD %>% anti_join(HighRisk_NAFLD)

All_NAFLD <- All_NAFLD %>% left_join(NAFLD_Pats) %>% mutate(Group="Low")
HighRisk_NAFLD <- HighRisk_NAFLD %>% left_join(NAFLD_Pats) %>% mutate(Group="High")

NAFLD_To_Predict <- All_NAFLD %>% bind_rows(HighRisk_NAFLD) %>% distinct() %>% select(-c("patient", "claimed"))


NAFLD_To_Predict <- HighRisk_NAFLD %>% arrange(Group, AST, ALT) %>% slice_tail(n=20000) %>% 
  bind_rows(All_NAFLD %>% arrange(Group, AST, ALT) %>% slice_head(n=20000)) 


NAFLD_To_Predict <- HighRisk_NAFLD %>% arrange(Group, AST, ALT) %>%
  bind_rows(All_NAFLD %>% arrange(Group, AST, ALT)) %>% group_by(patient) %>%
  filter(AST==max(AST)) %>% slice(1) %>% ungroup() %>% select(-c("patient","claimed"))

fwrite(NAFLD_To_Predict, "NAFLD_To_Predict.txt", sep="\t")











DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")

HighRisk_DIA <- fread("HighRisk_DIA.txt")
All_DIA <- fread("All_DIA.txt", sep="\t")
All_DIA <- All_DIA %>% anti_join(HighRisk_DIA)

All_DIA <- All_DIA %>% left_join(DIA_Pats) %>% mutate(Group="Low")
HighRisk_DIA <- HighRisk_DIA %>% left_join(DIA_Pats) %>% mutate(Group="High")

DIA_To_Predict <- All_DIA %>% bind_rows(HighRisk_DIA) %>% distinct() %>% select(-c("patient", "claimed"))


DIA_To_Predict <- HighRisk_DIA %>% arrange(Group, AST, ALT) %>% slice_tail(n=20000) %>% 
  bind_rows(All_DIA %>% arrange(Group, AST, ALT) %>% slice_head(n=20000)) 


DIA_To_Predict <- HighRisk_DIA %>% arrange(Group, AST, ALT) %>%
  bind_rows(All_DIA %>% arrange(Group, AST, ALT)) %>% group_by(patient) %>%
  filter(AST==max(AST)) %>% slice(1) %>% ungroup() %>% select(-c("patient","claimed"))

fwrite(DIA_To_Predict, "DIA_To_Predict.txt", sep="\t")










OBE_Pats <- fread("FIB4_Obesity_Pats.txt")

HighRisk_OBE <- fread("HighRisk_OBE.txt")
All_OBE <- fread("All_OBE.txt", sep="\t")
All_OBE <- All_OBE %>% anti_join(HighRisk_OBE)

All_OBE <- All_OBE %>% left_join(OBE_Pats) %>% mutate(Group="Low")
HighRisk_OBE <- HighRisk_OBE %>% left_join(OBE_Pats) %>% mutate(Group="High")

OBE_To_Predict <- All_OBE %>% bind_rows(HighRisk_OBE) %>% distinct() %>% select(-c("patient", "claimed"))


OBE_To_Predict <- HighRisk_OBE %>% arrange(Group, AST, ALT) %>% slice_tail(n=20000) %>% 
  bind_rows(All_OBE %>% arrange(Group, AST, ALT) %>% slice_head(n=20000)) 


OBE_To_Predict <- HighRisk_OBE %>% arrange(Group, AST, ALT) %>%
  bind_rows(All_OBE %>% arrange(Group, AST, ALT)) %>% group_by(patient) %>%
  filter(AST==max(AST)) %>% slice(1) %>% ungroup() %>% select(-c("patient","claimed"))

fwrite(OBE_To_Predict, "OBE_To_Predict.txt", sep="\t")



# --------------
# Ages for diagnosed vs undiagnosed bs nash stage vs comorbidity --------

All_NASH <- fread("All_NASH.txt")
All_DIA <- fread("All_DIA.txt")
All_OBE <- fread("All_OBE.txt")
All_NAFLD <- fread("All_NAFLD.txt")
All_Random <- fread("All_Random.txt")

HighRisk_NASH <- fread("HighRisk_NASH.txt")
HighRisk_DIA <- fread("HighRisk_DIA.txt")
HighRisk_OBE <- fread("HighRisk_OBE.txt")
HighRisk_NAFLD <- fread("HighRisk_NAFLD.txt")
HighRisk_Random <- fread("HighRisk_Random.txt")

All_NASH <- All_NASH %>% anti_join(HighRisk_NASH)
All_DIA <- All_DIA %>% anti_join(HighRisk_DIA)
All_OBE <- All_OBE %>% anti_join(HighRisk_OBE)
All_NAFLD <- All_NAFLD %>% anti_join(HighRisk_NAFLD)
All_Random <- All_Random %>% anti_join(HighRisk_Random)



DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)

All_DIA <- All_DIA %>% left_join(Diabetes_OBesity_Pats) 
All_NAFLD  <- All_NAFLD %>% left_join(Diabetes_OBesity_Pats) %>% mutate(ifelse(is.na(diagnosis), "NAFLD-only", diagnosis))
All_NASH  <- All_NASH %>% left_join(Diabetes_OBesity_Pats) %>% mutate(ifelse(is.na(diagnosis), "NASH-only", diagnosis))
All_OBE  <- All_OBE %>% left_join(Diabetes_OBesity_Pats) 
All_Random  <- All_Random %>% mutate(diagnosis="All_Random")


HighRisk_DIA <- HighRisk_DIA %>% left_join(Diabetes_OBesity_Pats) 
All_NAFLD  <- All_NAFLD %>% left_join(Diabetes_OBesity_Pats) %>% mutate(ifelse(is.na(diagnosis), "NAFLD-only", diagnosis))
HighRisk_NASH  <- HighRisk_NASH %>% left_join(Diabetes_OBesity_Pats) %>% mutate(ifelse(is.na(diagnosis), "NASH-only", diagnosis))
HighRisk_OBE  <- HighRisk_OBE %>% left_join(Diabetes_OBesity_Pats) 
HighRisk_Random  <- HighRisk_Random %>% mutate(diagnosis="All_Random")





# For the already diagnosed

FIB4_Bucket_dx <- fread("FIB4_Bucket_Fibrosis.txt")
NASH_Demographics <- fread("NASH Demographics.txt")

FIB4_Bucket_dx %>% left_join(NASH_Demographics %>% select(patid, weight, age), by = c("patient"="patid")) %>%
  left_join(Diabetes_OBesity_Pats) %>% group_by(diagnosis, FIB4_Bucket) %>% summarise(n=weighted.mean(age, weight))

# diagnosis          FIB4_Bucket        n
# <chr>              <chr>          <dbl>
#   1 Diabetes           Fibrosis        63.8
# 2 Diabetes           NASH-Cirrhosis  70.2
# 3 Diabetes           NASH-only       58.2
# 4 Diabetes + Obesity Fibrosis        61.3
# 5 Diabetes + Obesity NASH-Cirrhosis  65.4
# 6 Diabetes + Obesity NASH-only       55.0
# 7 Obesity            Fibrosis        59.4
# 8 Obesity            NASH-Cirrhosis  62.9
# 9 Obesity            NASH-only       50.4
# 10 NA                 Fibrosis        54.5
# 11 NA                 NASH-Cirrhosis  54.6
# 12 NA                 NASH-only       52.4


FIB4_Bucket_dx %>% left_join(NASH_Demographics %>% select(patid, weight, age), by = c("patient"="patid")) %>%
  left_join(Diabetes_OBesity_Pats) %>% group_by(FIB4_Bucket) %>% summarise(n=weighted.mean(age, weight))

# 
# FIB4_Bucket        n
# <chr>          <dbl>
#   1 Fibrosis        60.5
# 2 NASH-Cirrhosis  64.9
# 3 NASH-only       52.9


FIB4_Bucket_dx %>% left_join(NASH_Demographics %>% select(patid, weight, age), by = c("patient"="patid")) %>%
  left_join(Diabetes_OBesity_Pats) %>% summarise(n=weighted.mean(age, weight))
# 
# n
# 1 57.68568


FIB4_Bucket_dx %>% left_join(NASH_Demographics %>% select(patid, weight, age), by = c("patient"="patid")) %>%
  left_join(Diabetes_OBesity_Pats) %>% group_by(diagnosis) %>% summarise(n=weighted.mean(age, weight))
# 
# diagnosis              n
# <chr>              <dbl>
#   1 Diabetes            62.8
# 2 Diabetes + Obesity  59.7
# 3 Obesity             54.6
# 4 NA                  53.1


# Ages For the undiagnosed high risk -------------
HighRisk_DIA <- fread("HighRisk_DIA.txt")
HighRisk_OBE <- fread("HighRisk_OBE.txt")
HighRisk_NAFLD <- fread("HighRisk_NAFLD.txt")
HighRisk_Random <- fread("HighRisk_Random.txt")


# Get Ages

Rand_pts_Lab_Results_lst5y <- fread("Rand_pts_Lab_Results_lst5y.txt")
Rand_pts_Lab_Results_lst5y <- Rand_pts_Lab_Results_lst5y %>% select(ptid, age) %>% distinct()
names(Rand_pts_Lab_Results_lst5y)[1] <- "patient"

NAFLD_Demographics <- fread("NAFLD Demographics.txt")
names(NAFLD_Demographics)[1] <- "patient"

DANU_Demographics <- fread("DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"


HighRisk_DIA <- HighRisk_DIA %>% left_join(DANU_Demographics %>% select(patient, age))
HighRisk_OBE <- HighRisk_OBE %>% left_join(DANU_Demographics %>% select(patient, age))
HighRisk_NAFLD <- HighRisk_NAFLD %>% left_join(NAFLD_Demographics %>% select(patient, age))
HighRisk_Random <- HighRisk_Random %>%left_join(Rand_pts_Lab_Results_lst5y)


NASH_Demographics_All <- fread("NASH Demographics All.txt")
names(NASH_Demographics_All)[1] <- "patient"
length(unique(NASH_Demographics_All$patient))
NASH_Demographics_All <- NASH_Demographics_All %>% select(patient, weight, fibrosis, cirrhosis)

HighRisk_DIA <- HighRisk_DIA %>% inner_join(NASH_Demographics_All)
HighRisk_OBE <- HighRisk_OBE %>% inner_join(NASH_Demographics_All)
HighRisk_NAFLD <- HighRisk_NAFLD %>% inner_join(NASH_Demographics_All) 


# Comorbidity status

DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)

HighRisk_DIA <- HighRisk_DIA %>% left_join(Diabetes_OBesity_Pats)
HighRisk_OBE <- HighRisk_OBE %>% left_join(Diabetes_OBesity_Pats)
HighRisk_NAFLD <- HighRisk_NAFLD %>% left_join(Diabetes_OBesity_Pats)


HighRisk_DIA %>% filter(!is.na(cirrhosis)) %>% group_by(diagnosis) %>% summarise(n=mean(age))
HighRisk_DIA %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(diagnosis) %>% summarise(n=mean(age))
HighRisk_DIA %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(diagnosis) %>% summarise(n=mean(age))
HighRisk_DIA  %>% group_by(diagnosis) %>% summarise(n=mean(age))


HighRisk_OBE %>% filter(!is.na(cirrhosis)) %>% group_by(diagnosis) %>% summarise(n=mean(age))
HighRisk_OBE %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(diagnosis) %>% summarise(n=mean(age))
HighRisk_OBE %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(diagnosis) %>% summarise(n=mean(age))
HighRisk_OBE  %>% group_by(diagnosis) %>% summarise(n=mean(age))



HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>% filter(!is.na(cirrhosis))  %>% summarise(n=mean(age, na.rm=T))
HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>% filter(!is.na(cirrhosis))  %>% summarise(n=mean(age, na.rm=T))


HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>% filter(is.na(cirrhosis) & !is.na(fibrosis))  %>% summarise(n=mean(age, na.rm=T))
HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>% filter(is.na(cirrhosis) & !is.na(fibrosis))  %>% summarise(n=mean(age, na.rm=T))

HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>% filter(is.na(cirrhosis) & is.na(fibrosis))  %>% summarise(n=mean(age, na.rm=T))
HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>% filter(is.na(cirrhosis) & is.na(fibrosis))  %>% summarise(n=mean(age, na.rm=T))

HighRisk_NAFLD  %>% filter(is.na(diagnosis)) %>% summarise(n=mean(age, na.rm=T))
HighRisk_NAFLD  %>% filter(!is.na(diagnosis)) %>% summarise(n=mean(age, na.rm=T))


HighRisk_DIA %>% bind_rows(HighRisk_OBE) %>% bind_rows(HighRisk_NAFLD) %>%
  filter(!is.na(cirrhosis))  %>% summarise(n=mean(age, na.rm=T))


HighRisk_DIA %>% bind_rows(HighRisk_OBE) %>% bind_rows(HighRisk_NAFLD) %>%
  filter(is.na(cirrhosis) & !is.na(fibrosis))  %>% summarise(n=mean(age, na.rm=T))


HighRisk_DIA %>% bind_rows(HighRisk_OBE) %>% bind_rows(HighRisk_NAFLD) %>%
  filter(is.na(cirrhosis) & is.na(fibrosis))  %>% summarise(n=mean(age, na.rm=T))

# ----------
# Age Biopsy vs no Biopsy ------------------
NASH_Dossiers <- fread("NASH Dossiers.txt")
Biopsy_pats <- NASH_Dossiers %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct()
Biopsy_pats$Biopsy = "Biopsy"
names(Biopsy_pats)[1] <- "patient"

FIB4_Bucket_dx <- fread("FIB4_Bucket_Fibrosis.txt")



DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)

DANU_Demographics <- fread("DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"

FIB4_Bucket_dx <- FIB4_Bucket_dx %>% left_join(Diabetes_OBesity_Pats) %>% left_join(Biopsy_pats) %>% 
  left_join(DANU_Demographics %>% select(patient, age))

FIB4_Bucket_dx %>% group_by(Biopsy) %>% summarise(n=mean(age))

Biopsy     n
<chr>  <dbl>
  1 Biopsy  55.9
2 NA      58.5

data.frame(FIB4_Bucket_dx %>% group_by(diagnosis, FIB4_Bucket, Biopsy) %>% summarise(n=mean(age)))

# 1            Diabetes       Fibrosis Biopsy 66.33333
# 2            Diabetes       Fibrosis   <NA> 64.86364
# 3            Diabetes NASH-Cirrhosis Biopsy 64.25000
# 4            Diabetes NASH-Cirrhosis   <NA> 71.33333
# 5            Diabetes      NASH-only Biopsy 49.00000
# 6            Diabetes      NASH-only   <NA> 59.48148
# 7  Diabetes + Obesity       Fibrosis Biopsy 57.31667
# 8  Diabetes + Obesity       Fibrosis   <NA> 61.60626
# 9  Diabetes + Obesity NASH-Cirrhosis Biopsy 61.15132
# 10 Diabetes + Obesity NASH-Cirrhosis   <NA> 65.82725
# 11 Diabetes + Obesity      NASH-only Biopsy 50.14706
# 12 Diabetes + Obesity      NASH-only   <NA> 56.23484
# 13            Obesity       Fibrosis Biopsy 57.27907
# 14            Obesity       Fibrosis   <NA> 59.73077
# 15            Obesity NASH-Cirrhosis Biopsy 60.60000
# 16            Obesity NASH-Cirrhosis   <NA> 63.98000
# 17            Obesity      NASH-only Biopsy 46.63529
# 18            Obesity      NASH-only   <NA> 52.66570
# 19               <NA>       Fibrosis Biopsy 61.40000
# 20               <NA>       Fibrosis   <NA> 56.14815
# 21               <NA> NASH-Cirrhosis Biopsy 56.00000
# 22               <NA> NASH-Cirrhosis   <NA> 54.00000
# 23               <NA>      NASH-only Biopsy 47.20000
# 24               <NA>      NASH-only   <NA> 54.36207
# -------
# Penetrance of other imaging tests: Biopsy vs no Biopsy --------------
# from above

# Ultrasound 
NASH_Dossiers <- fread("NASH Dossiers.txt")
Ultrasound_pats <- NASH_Dossiers %>% filter(condition == "Liver Ultrasound") %>% select(patid) %>% distinct()

Ultrasound_pats <- Ultrasound_pats %>% mutate(Ultrasound_pats = "Ultrasound")
names(Ultrasound_pats)[1] <- "patient"


# Liver Imaging 
NASH_Dossiers <- fread("NASH Dossiers.txt")
Imaging_pats <- NASH_Dossiers %>% filter(condition == "Liver Imaging") %>% select(patid) %>% distinct()

Imaging_pats <- Imaging_pats %>% mutate(Imaging_Status = "Imaging")
names(Imaging_pats)[1] <- "patient"

FIB4_Bucket_dx <- FIB4_Bucket_dx %>% left_join(Ultrasound_pats) %>% left_join(Imaging_pats)

FIB4_Bucket_dx %>% group_by(Biopsy, Ultrasound_pats) %>% count()

Biopsy Ultrasound_pats     n
<chr>  <chr>           <int>
  1 Biopsy Ultrasound        126
2 Biopsy NA                499
3 NA     Ultrasound        402
4 NA     NA               2720

FIB4_Bucket_dx %>% group_by(Biopsy, Imaging_Status) %>% count()

Biopsy Imaging_Status     n
<chr>  <chr>          <int>
  1 Biopsy Imaging           68
2 Biopsy NA               557
3 NA     Imaging          173
4 NA     NA              2949


data.frame(FIB4_Bucket_dx %>% group_by(diagnosis, FIB4_Bucket, Biopsy, Ultrasound_pats) %>% count())
# 
# 1            Diabetes       Fibrosis Biopsy      Ultrasound   2
# 2            Diabetes       Fibrosis Biopsy            <NA>   1
# 3            Diabetes       Fibrosis   <NA>            <NA>  22
# 4            Diabetes NASH-Cirrhosis Biopsy      Ultrasound   4
# 5            Diabetes NASH-Cirrhosis   <NA>      Ultrasound   3
# 6            Diabetes NASH-Cirrhosis   <NA>            <NA>   9
# 7            Diabetes      NASH-only Biopsy      Ultrasound   1
# 8            Diabetes      NASH-only Biopsy            <NA>   1
# 9            Diabetes      NASH-only   <NA>      Ultrasound   2
# 10           Diabetes      NASH-only   <NA>            <NA>  25
# 11 Diabetes + Obesity       Fibrosis Biopsy      Ultrasound  22
# 12 Diabetes + Obesity       Fibrosis Biopsy            <NA>  98
# 13 Diabetes + Obesity       Fibrosis   <NA>      Ultrasound  72
# 14 Diabetes + Obesity       Fibrosis   <NA>            <NA> 535
# 15 Diabetes + Obesity NASH-Cirrhosis Biopsy      Ultrasound  35
# 16 Diabetes + Obesity NASH-Cirrhosis Biopsy            <NA> 117
# 17 Diabetes + Obesity NASH-Cirrhosis   <NA>      Ultrasound  91
# 18 Diabetes + Obesity NASH-Cirrhosis   <NA>            <NA> 320
# 19 Diabetes + Obesity      NASH-only Biopsy      Ultrasound  18
# 20 Diabetes + Obesity      NASH-only Biopsy            <NA>  84
# 21 Diabetes + Obesity      NASH-only   <NA>      Ultrasound  82
# 22 Diabetes + Obesity      NASH-only   <NA>            <NA> 693
# 23            Obesity       Fibrosis Biopsy      Ultrasound  16
# 24            Obesity       Fibrosis Biopsy            <NA>  70
# 25            Obesity       Fibrosis   <NA>      Ultrasound  53
# 26            Obesity       Fibrosis   <NA>            <NA> 337
# 27            Obesity NASH-Cirrhosis Biopsy      Ultrasound  15
# 28            Obesity NASH-Cirrhosis Biopsy            <NA>  45
# 29            Obesity NASH-Cirrhosis   <NA>      Ultrasound  26
# 30            Obesity NASH-Cirrhosis   <NA>            <NA>  74
# 31            Obesity      NASH-only Biopsy      Ultrasound  13
# 32            Obesity      NASH-only Biopsy            <NA>  72
# 33            Obesity      NASH-only   <NA>      Ultrasound  64
# 34            Obesity      NASH-only   <NA>            <NA> 627
# 35               <NA>       Fibrosis Biopsy            <NA>   5
# 36               <NA>       Fibrosis   <NA>      Ultrasound   2
# 37               <NA>       Fibrosis   <NA>            <NA>  25
# 38               <NA> NASH-Cirrhosis Biopsy            <NA>   1
# 39               <NA> NASH-Cirrhosis   <NA>      Ultrasound   1
# 40               <NA> NASH-Cirrhosis   <NA>            <NA>   1
# 41               <NA>      NASH-only Biopsy            <NA>   5
# 42               <NA>      NASH-only   <NA>      Ultrasound   6
# 43               <NA>      NASH-only   <NA>            <NA>  52

data.frame(FIB4_Bucket_dx %>% group_by(diagnosis, FIB4_Bucket, Biopsy, Imaging_Status) %>% count())

# -----

# Payer Mix across comorbidity and NASH stages --------------------------------
NASH_Demographics <- fread("NASH Demographics All.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, plan)
names(NASH_Demographics)[1] <- "patient"

# Comorbidity status
DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)
DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"
DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)
# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)
# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)


# Diagnosed
FIB4_Bucket_dx <- fread("FIB4_Bucket_Fibrosis.txt")


data.frame(FIB4_Bucket_dx %>% left_join(NASH_Demographics) %>%
             left_join(Diabetes_OBesity_Pats) %>% group_by(diagnosis, FIB4_Bucket) %>% count())

# diagnosis          FIB4_Bucket        n
# <chr>              <chr>          <int>
# 1 Diabetes           Fibrosis          25
# 2 Diabetes           NASH-Cirrhosis    16
# 3 Diabetes           NASH-only         29
# 4 Diabetes + Obesity Fibrosis         727
# 5 Diabetes + Obesity NASH-Cirrhosis   563
# 6 Diabetes + Obesity NASH-only        877
# 7 Obesity            Fibrosis         476
# 8 Obesity            NASH-Cirrhosis   160
# 9 Obesity            NASH-only        776
# 10 NA                 Fibrosis          32
# 11 NA                 NASH-Cirrhosis     3
# 12 NA                 NASH-only         63

FIB4_Bucket_dx %>% left_join(NASH_Demographics) %>%
  left_join(Diabetes_OBesity_Pats) %>% group_by(plan) %>% count()


data.frame(FIB4_Bucket_dx %>% left_join(NASH_Demographics) %>%
             left_join(Diabetes_OBesity_Pats) %>% group_by(diagnosis) %>% count())

# diagnosis    n
# 1           Diabetes   70
# 2 Diabetes + Obesity 2167
# 3            Obesity 1412
# 4               <NA>   98


data.frame(FIB4_Bucket_dx %>% left_join(NASH_Demographics) %>%
             left_join(Diabetes_OBesity_Pats) %>% group_by(FIB4_Bucket) %>% count())

# FIB4_Bucket    n
# 1       Fibrosis 1260
# 2 NASH-Cirrhosis  742
# 3      NASH-only 1745
# 

data.frame(FIB4_Bucket_dx %>% left_join(NASH_Demographics) %>%
             left_join(Diabetes_OBesity_Pats) %>% group_by(diagnosis, FIB4_Bucket, plan) %>% count())

# diagnosis    FIB4_Bucket plan   n
# 1            Diabetes       Fibrosis    C  15
# 2            Diabetes       Fibrosis    D   1
# 3            Diabetes       Fibrosis    M   9
# 4            Diabetes NASH-Cirrhosis    C   8
# 5            Diabetes NASH-Cirrhosis    D   2
# 6            Diabetes NASH-Cirrhosis    M   6
# 7            Diabetes      NASH-only    C  20
# 8            Diabetes      NASH-only    D   1
# 9            Diabetes      NASH-only    M   8

# 10 Diabetes + Obesity       Fibrosis    C 449
# 11 Diabetes + Obesity       Fibrosis    D  30
# 12 Diabetes + Obesity       Fibrosis    M 248
# 13 Diabetes + Obesity NASH-Cirrhosis    C 284
# 14 Diabetes + Obesity NASH-Cirrhosis    D  27
# 15 Diabetes + Obesity NASH-Cirrhosis    M 252
# 16 Diabetes + Obesity      NASH-only    C 596
# 17 Diabetes + Obesity      NASH-only    D  50
# 18 Diabetes + Obesity      NASH-only    M 231

# 19            Obesity       Fibrosis    C 343
# 20            Obesity       Fibrosis    D  14
# 21            Obesity       Fibrosis    M 119
# 22            Obesity NASH-Cirrhosis    C  98
# 23            Obesity NASH-Cirrhosis    D   3
# 24            Obesity NASH-Cirrhosis    M  59
# 25            Obesity      NASH-only    C 634
# 26            Obesity      NASH-only    D  21
# 27            Obesity      NASH-only    M 121

# 28               <NA>       Fibrosis    C  22
# 29               <NA>       Fibrosis    D   3
# 30               <NA>       Fibrosis    M   7
# 31               <NA> NASH-Cirrhosis    C   3
# 32               <NA>      NASH-only    C  48
# 33               <NA>      NASH-only    M  15


# High Risk Undiagnosed 
HighRisk_DIA <- fread("HighRisk_DIA.txt")
HighRisk_OBE <- fread("HighRisk_OBE.txt")
HighRisk_NAFLD <- fread("HighRisk_NAFLD.txt")
HighRisk_Random <- fread("HighRisk_Random.txt")

NASH_Demographics # from before, with plan

NASH_Demographics_All <- fread("NASH Demographics All.txt")
names(NASH_Demographics_All)[1] <- "patient"
NASH_Demographics_All <- NASH_Demographics_All %>% select(patient, fibrosis, cirrhosis)

HighRisk_DIA <- HighRisk_DIA %>% inner_join(NASH_Demographics_All) %>% left_join(NASH_Demographics)
HighRisk_OBE <- HighRisk_OBE %>% inner_join(NASH_Demographics_All) %>% left_join(NASH_Demographics)
HighRisk_NAFLD <- HighRisk_NAFLD %>% inner_join(NASH_Demographics_All) %>% left_join(NASH_Demographics)

HighRisk_DIA <- HighRisk_DIA %>% left_join(Diabetes_OBesity_Pats)
HighRisk_OBE <- HighRisk_OBE %>% left_join(Diabetes_OBesity_Pats)
HighRisk_NAFLD <- HighRisk_NAFLD %>% left_join(Diabetes_OBesity_Pats)



#T2 DM only
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>% count()) # 832
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>% filter(!is.na(cirrhosis)) %>% count()) # 28
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% count()) # 10
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% count()) # 794

data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>% group_by(plan) %>% count()) # 832

data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>% filter(!is.na(cirrhosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>%filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes") %>%filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(plan) %>% count()) 


#T2 DM + OBE
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>% count()) # 9656
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>% filter(!is.na(cirrhosis)) %>% count()) # 227
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% count()) # 27
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% count()) # 9402

data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>% group_by(plan) %>% count()) #

data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>% filter(!is.na(cirrhosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>%filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_DIA %>% filter(diagnosis=="Diabetes + Obesity") %>%filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(plan) %>% count()) 



# OBE
data.frame(HighRisk_OBE %>% count()) # 16282
data.frame(HighRisk_OBE %>% filter(!is.na(cirrhosis)) %>% count()) # 2
data.frame(HighRisk_OBE %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% count()) # 52
data.frame(HighRisk_OBE %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% count()) # 16000

data.frame(HighRisk_OBE %>% group_by(plan) %>% count()) #

data.frame(HighRisk_OBE %>% filter(!is.na(cirrhosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_OBE %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_OBE %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(plan) %>% count()) 



# NAFLD-only
data.frame(HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>%  count()) # 257
data.frame(HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>%  filter(!is.na(cirrhosis)) %>% count()) # 17
data.frame(HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>%  filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% count()) # 6
data.frame(HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>%  filter(is.na(cirrhosis) & is.na(fibrosis)) %>% count()) # 234

data.frame(HighRisk_NAFLD %>% filter(is.na(diagnosis)) %>%  group_by(plan) %>% count()) 

data.frame(HighRisk_NAFLD %>%  filter(is.na(diagnosis)) %>% filter(!is.na(cirrhosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_NAFLD %>%  filter(is.na(diagnosis)) %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_NAFLD %>%  filter(is.na(diagnosis)) %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(plan) %>% count()) 




# NAFLD-only
data.frame(HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>%  count()) # 8022
data.frame(HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>%  filter(!is.na(cirrhosis)) %>% count()) # 338
data.frame(HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>%  filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% count()) # 138
data.frame(HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>%  filter(is.na(cirrhosis) & is.na(fibrosis)) %>% count()) # 7546

data.frame(HighRisk_NAFLD %>% filter(!is.na(diagnosis)) %>%  group_by(plan) %>% count()) 

data.frame(HighRisk_NAFLD %>%  filter(!is.na(diagnosis)) %>% filter(!is.na(cirrhosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_NAFLD %>%  filter(!is.na(diagnosis)) %>% filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(plan) %>% count()) 
data.frame(HighRisk_NAFLD %>%  filter(!is.na(diagnosis)) %>% filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(plan) %>% count()) 


HighRisk_NAFLD %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE) %>% 
  filter(!is.na(cirrhosis)) %>% count()

HighRisk_NAFLD %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE) %>% 
  filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% count()

HighRisk_NAFLD %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE) %>% 
  filter(is.na(cirrhosis) & is.na(fibrosis)) %>% count()




HighRisk_NAFLD %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE)%>% 
  group_by(plan) %>% count()

HighRisk_NAFLD %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE) %>% 
  filter(!is.na(cirrhosis))  %>% group_by(plan) %>% count()

HighRisk_NAFLD %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE) %>% 
  filter(is.na(cirrhosis) & !is.na(fibrosis)) %>% group_by(plan) %>% count()


HighRisk_NAFLD %>% bind_rows(HighRisk_DIA) %>% bind_rows(HighRisk_OBE) %>% 
  filter(is.na(cirrhosis) & is.na(fibrosis)) %>% group_by(plan) %>% count()





# ---------
# Specialties associated with liver biopsy events -----------
NASH_Events <- fread("NASH Events.txt")
NASH_Events <- NASH_Events %>% select(code, prov) 

NASH_Diagnosis_Codes <- fread("NASH Diagnosis Codes.txt")
NASH_Diagnosis_Codes <- NASH_Diagnosis_Codes %>% filter(condition=="Liver Biopsy") %>% select(code, condition)

NASH_Events <- NASH_Events %>% inner_join(NASH_Diagnosis_Codes)

NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")

NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

NASH_Events <- NASH_Events %>% left_join(NASH_Event_Claims_Providers)

data.frame(NASH_Events %>% group_by(specialty) %>% count() %>% arrange(-n))

Summary_Specialties <- fread("Summary_Specialties.txt")

NASH_Events <- NASH_Events %>% left_join(Summary_Specialties)

temp <- data.frame(NASH_Events %>% group_by(specialty) %>% count() %>% arrange(-n))

fwrite(temp, "Specialties_Biopsy.txt", sep="\t")

# -------

# Comorbidities based on biopsy status ------
NASH_Pats_95ConfLiver_2plusHits_Provider <- fread("NASH_Pats_95ConfLiver_2plusHits_Provider.txt")
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag) %>% distinct()
names(NASH_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"
NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% mutate(diag = str_sub(string = diag, start = 1, end = 2))  

length(unique(NASH_Pats_95ConfLiver_2plusHits_Provider$patient)) # 1357

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, diag) %>% distinct()


NASH_Dossiers <- fread("NASH Dossiers.txt")
Biopsy_pats <- NASH_Dossiers %>% filter(condition == "Liver Biopsy") %>% select(patid) %>% distinct()
Biopsy_pats$Biopsy = "Biopsy"
names(Biopsy_pats)[1] <- "patient"


NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% left_join(Biopsy_pats)

NASH_Pats_95ConfLiver_2plusHits_Provider %>% select(patient, Biopsy) %>% distinct() %>% group_by(Biopsy) %>% count()

Biopsy     n
<chr>  <int>
  1 Biopsy   292
2 NA      1065

NASH_Pats_95ConfLiver_2plusHits_Provider <- data.frame(NASH_Pats_95ConfLiver_2plusHits_Provider %>% group_by(Biopsy, diag) %>% count())

diag_lookup <- fread("diag_lookup.txt", sep="\t")

NASH_Pats_95ConfLiver_2plusHits_Provider <- diag_lookup %>% left_join(NASH_Pats_95ConfLiver_2plusHits_Provider) 

NASH_Pats_95ConfLiver_2plusHits_Provider <- NASH_Pats_95ConfLiver_2plusHits_Provider %>% mutate(n2 = ifelse(is.na(Biopsy),n/1065,n/292))

fwrite(NASH_Pats_95ConfLiver_2plusHits_Provider, "Comorbidities_Percentage_HighRiskNASH_BIOPSY.txt", sep="\t")

# ------
# Total duration on treatment -------
FIB4_Bucket_dx <- fread("FIB4_Bucket_Fibrosis.txt")
NASH_only <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="NASH-only") %>% select(patient)
NASH_fibrosis <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="Fibrosis") %>% select(patient)
NASH_cirrhosis <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="NASH-Cirrhosis") %>% select(patient)



NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('-',.), ~replace(., grepl('-', .), "Lapsed"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Lapsed",0,1))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Total_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 30.37262
weighted.median(temp$Total_duration, temp$n) # 28.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('-',.), ~replace(., grepl('-', .), "Lapsed"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Lapsed",0,1))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Total_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 35.12016
weighted.median(temp$Total_duration, temp$n) # 38.5






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('-',.), ~replace(., grepl('-', .), "Lapsed"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Lapsed",0,1))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Total_Periods_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 39.29523
weighted.median(temp$Total_duration, temp$n) # 49.5
# ---
# Most common drugs to track -----

# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, molecule, drug_group, drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 976705 (7208 ever treated)

data.frame(NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group, drug_class, molecule) %>% distinct() %>% 
             group_by( drug_group, drug_class, molecule) %>% summarise(n=sum(weight))  %>% arrange(-n))

# drug_group         drug_class molecule         n
# 1      Antidiabetic          Biguanide       36 486387.13
# 2   Anticholesterol             Statin       10 413229.15
# 3   Anticholesterol             Statin       15 173646.38
# 4      Antidiabetic            Insulin       64 148417.32
# 5      Antidiabetic            Insulin       68 137275.52
# 6   Anticholesterol             Statin       16 123328.41
# 7      Antidiabetic               DPP4       55 105306.39
# 8   Anticholesterol             Statin       14 104307.27
# 9      Antidiabetic            Insulin       67 100171.04
# 10     Antidiabetic       Sulfonylurea       45  95513.62
# 11  Anticholesterol            Fibrate        4  86578.68
# 12     Antidiabetic              SGLT2       58  84345.72
# 13     Antidiabetic       Sulfonylurea       44  83940.84
# 14     Antidiabetic            Insulin       61  79572.34
# 15  GLP1 Injectable    GLP1 Injectable       71  72019.07
# 16  GLP1 Injectable    GLP1 Injectable       73  67457.91
# 17  Hospitaaflization  Bariatric Surgery       79  61521.39
# 18  Anticholesterol    Anticholesterol        7  61471.31
# 19  GLP1 Injectable    GLP1 Injectable       75  51750.29
# 20     Antidiabetic            Insulin       63  49983.58
# 21     Antidiabetic          Glitazone       50  47165.17
# 22     Antidiabetic            Insulin       66  45774.04
# 23 Hepatoprotective   Hepatoprotective       35  44623.04
# 24      Antiobesity          Anorectic       29  43477.88
# 25     Antidiabetic              SGLT2       56  42027.04
# 26  Anticholesterol                BAS        1  39670.39
# 27      Antiobesity          Anorectic       23  39487.66
# 28     Antidiabetic            Insulin       62  36614.62
# 29     Antidiabetic              SGLT2       57  36456.17
# 30  Anticholesterol    Anticholesterol        8  31411.71
# 31  Hospitalization Hospital Inpatient       76  28739.49
# 32  GLP1 Injectable    GLP1 Injectable       72  27855.73
# 33     Antidiabetic               DPP4       53  27228.78
# 34      Antiobesity          Anorectic       25  23236.51
# 35  Anticholesterol            Fibrate        5  20460.23
# 36      Antiobesity          Anorectic       27  20075.63
# 37  Anticholesterol                BAS        3  19704.78
# 38     Antidiabetic       Sulfonylurea       43  18271.56
# 39  Anticholesterol             Statin       12  17695.42
# 40  Anticholesterol                BAS        2  15006.63
# 41  Hospitalization   Liver Transplant       82  14371.65
# 42  Hospitalization Hospital Inpatient       77  13313.90
# 43  Hospitalization  Surgery Inpatient       78  12427.75
# 44     Antidiabetic               DPP4       54  11985.96
# 45      Antiobesity        Antiobesity       31  11603.23
# 46        GLP1 Oral          GLP1 Oral       69   9950.37
# 47  Anticholesterol             Statin       13   9001.06
# 48  Anticholesterol           Biologic       19   8929.10
# 49 Hepatoprotective   Hepatoprotective       34   7108.36
# 50     Antidiabetic            Glinide       49   6597.38
# 51     Antidiabetic               DPP4       52   6476.40
# 52     Antidiabetic            Insulin       65   5318.46
# 53     Antidiabetic            Glinide       48   4869.43
# 54     Antidiabetic              SGLT2       59   4638.00
# 55      Antiobesity        Antiobesity       30   4486.01
# 56  GLP1 Injectable    GLP1 Injectable       74   4275.93
# 57     Antidiabetic                AGI       39   4221.42
# 58  Hospitalization  Bariatric Surgery       80   4205.87
# 59  Anticholesterol           Biologic       17   4155.52
# 60  GLP1 Injectable    GLP1 Injectable       70   4055.57
# 61      Antiobesity          Anorectic       28   1804.35
# 62      Antiobesity          Anorectic       24   1339.19
# 63  Anticholesterol             Statin       11   1320.73
# 64      Antiobesity        Weight Loss       21   1064.67
# 65     Antidiabetic       Antidiabetic       37    837.53
# 66  Anticholesterol    Anticholesterol        6    677.79
# 67 Hepatoprotective   Hepatoprotective       33    304.96
# 68     Antidiabetic          Glitazone       51    278.75
# 69      Antiobesity          Anorectic       22    151.94
# 70     Antidiabetic                AGI       40    102.26
# --------
# Total duration on individual molecules per NASH Stage --------------
FIB4_Bucket_dx <- fread("FIB4_Bucket_Fibrosis.txt")
NASH_only <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="NASH-only") %>% select(patient)
NASH_fibrosis <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="Fibrosis") %>% select(patient)
NASH_cirrhosis <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="NASH-Cirrhosis") %>% select(patient)


# Sitagliptin
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(55{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(55{1})(\\D|$)', .), "Sitagliptin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Sitagliptin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Sitagliptin_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 17.54485
weighted.median(temp$Total_duration, temp$n) # 7.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(55{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(55{1})(\\D|$)', .), "Sitagliptin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Sitagliptin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Sitagliptin_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 15.90619
weighted.median(temp$Total_duration, temp$n) # 10.5






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(55{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(55{1})(\\D|$)', .), "Sitagliptin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Sitagliptin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Sitagliptin_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 22.41477
weighted.median(temp$Total_duration, temp$n) # 13.5












# Glipizide
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(45{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(45{1})(\\D|$)', .), "Glipizide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Glipizide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Glipizide_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 21.13234
weighted.median(temp$Total_duration, temp$n) # 14.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(45{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(45{1})(\\D|$)', .), "Glipizide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Glipizide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Glipizide_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 27.2865
weighted.median(temp$Total_duration, temp$n) # 22.5






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(45{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(45{1})(\\D|$)', .), "Glipizide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Glipizide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Glipizide_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 25.18047
weighted.median(temp$Total_duration, temp$n) # 17.5











# Insulin Glargine
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(64{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(64{1})(\\D|$)', .), "Insulin Glargine"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin Glargine",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "InsulinGlargine_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 19.19121
weighted.median(temp$Total_duration, temp$n) # 12.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(64{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(64{1})(\\D|$)', .), "Insulin Glargine"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin Glargine",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "InsulinGlargine_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 22.06705
weighted.median(temp$Total_duration, temp$n) # 16.5






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(64{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(64{1})(\\D|$)', .), "Insulin Glargine"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin Glargine",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "InsulinGlargine_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 25.5817
weighted.median(temp$Total_duration, temp$n) # 19.5




# Empagliflozin
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(58{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(58{1})(\\D|$)', .), "Empagliflozin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Empagliflozin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Empagliflozin_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 12.79377
weighted.median(temp$Total_duration, temp$n) # 7.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(58{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(58{1})(\\D|$)', .), "Empagliflozin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Empagliflozin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Empagliflozin_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 11.77188
weighted.median(temp$Total_duration, temp$n) # 7.5






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(58{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(58{1})(\\D|$)', .), "Empagliflozin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Empagliflozin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Empagliflozin_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 12.34805
weighted.median(temp$Total_duration, temp$n) # 5.5









# Dulaglutide
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Dulaglutide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Dulaglutide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Dulaglutide_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 12.79873
weighted.median(temp$Total_duration, temp$n) # 6.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Dulaglutide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Dulaglutide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Dulaglutide_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 16.62818
weighted.median(temp$Total_duration, temp$n) # 13.5






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Dulaglutide"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Dulaglutide",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Dulaglutide_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 15.64786
weighted.median(temp$Total_duration, temp$n) # 9.5








# Metformin
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(36{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(36{1})(\\D|$)', .), "Metformin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Metformin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Metformin_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 27.02888
weighted.median(temp$Total_duration, temp$n) # 22.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(36{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(36{1})(\\D|$)', .), "Metformin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Metformin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Metformin_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 29.7685
weighted.median(temp$Total_duration, temp$n) # 29.5






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(36{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(36{1})(\\D|$)', .), "Metformin"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Metformin",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Metformin_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 35.35389
weighted.median(temp$Total_duration, temp$n) # 40.5






# Fenofibrate
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "Fenofibrate"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Fenofibrate",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Fenofibrate_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 26.46475
weighted.median(temp$Total_duration, temp$n) # 20.5
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "Fenofibrate"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Fenofibrate",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Fenofibrate_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 26.57474
weighted.median(temp$Total_duration, temp$n) # 22






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "Fenofibrate"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Fenofibrate",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Fenofibrate_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 25.8553
weighted.median(temp$Total_duration, temp$n) # 19.5















# Orlistat
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "Orlistat"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Orlistat",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Orlistat_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 1
weighted.median(temp$Total_duration, temp$n) # 1
















NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_fibrosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "Orlistat"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Orlistat",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_fibrosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Orlistat_Periods_NASH_fibrosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 1.70
weighted.median(temp$Total_duration, temp$n) # 1






NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "Orlistat"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Orlistat",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_cirrhosis %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Orlistat_NASH_cirrhosis_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 1
weighted.median(temp$Total_duration, temp$n) # 1


















# Ursodiol
NASH_Drug_Histories <- fread("NASH Drug Histories.txt")
NASH_Drug_Histories <- NASH_only %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <-  NASH_Drug_Histories %>%  select(4:63)

NASH_Drug_Histories <- NASH_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(35{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(35{1})(\\D|$)', .), "Ursodiol"))

NASH_Drug_Histories <-  NASH_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ursodiol",1,0))

NASH_Drug_Histories[] <-  lapply(NASH_Drug_Histories,as.numeric)

NASH_Drug_Histories_LONG <-  fread("NASH Drug Histories.txt")
NASH_Drug_Histories_LONG <- NASH_only %>% left_join(NASH_Drug_Histories_LONG)

NASH_Drug_Histories_LONG <- NASH_Drug_Histories_LONG %>% select(patient, weight)

NASH_Drug_Histories <- NASH_Drug_Histories_LONG %>% bind_cols(NASH_Drug_Histories)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat == 1)

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Total_Periods <- NASH_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Total_Periods)[3] <- "Duration"

Total_Periods_VIZ <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Total_Periods_VIZ, "Ursodiol_Periods_NASH_only_VIZ.csv")

temp <- Total_Periods %>% left_join(NASH_Drug_Histories %>% 
                                      select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% group_by(Total_duration) %>% summarise(n=sum(weight))

library(spatstat)
weighted.mean(temp$Total_duration, temp$n)  # 9.649722
weighted.median(temp$Total_duration, temp$n) # 3.5

# -------
# Drug Penetrance by NASH Stage (using our classification) ------------
FIB4_Bucket_dx <- fread("FIB4_Bucket_Fibrosis.txt")
NASH_only <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="NASH-only") %>% select(patient)
NASH_fibrosis <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="Fibrosis") %>% select(patient)
NASH_cirrhosis <- FIB4_Bucket_dx %>% filter(FIB4_Bucket=="NASH-Cirrhosis") %>% select(patient)




# Drugs ever tried 
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group, drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 976705 (7208 ever treated)

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 

# drug_group             n
# <chr>              <dbl>
#   1 Anticholesterol  730499.
# 2 Antidiabetic     604496.
# 3 Antiobesity      114826.
# 4 GLP1 Injectable  170337.
# 5 GLP1 Oral          9950.
# 6 Hepatoprotective  51530.
# 7 Hospitalization  118449.



NASH_Drug_Histories <- FIB4_Bucket_dx %>% inner_join(NASH_Drug_Histories)

NASH_Drug_Histories %>% select(patient, FIB4_Bucket, weight) %>% distinct() %>% group_by(FIB4_Bucket) %>% summarise(n=sum(weight))


FIB4_Bucket          n
<chr>            <dbl>
  1 Fibrosis       127082
2 NASH-Cirrhosis 83720
3 NASH-only      174136

data.frame(NASH_Drug_Histories %>% select(patient, weight, FIB4_Bucket, drug_group) %>% 
             distinct() %>% group_by(FIB4_Bucket , drug_group) %>%  summarise(n=sum(weight)) %>%
             spread(key = FIB4_Bucket , value=n))


# drug_group  Fibrosis NASH.Cirrhosis NASH.only
# 1  Anticholesterol 100685.94       62137.39 127795.71
# 2     Antidiabetic  77717.47       62299.47  96296.38
# 3      Antiobesity  12550.16        4682.84  27472.70
# 4  GLP1 Injectable  21651.79       17630.06  27459.32
# 5        GLP1 Oral   1209.35        1024.57   1807.64
# 6 Hepatoprotective   3716.77        9283.10   6828.00
# 7  Hospitalization  13220.98       16304.69  19465.27

# ------

# Enzyme levels in GLP1 experienced vs naive patients --------------------
FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")

FIB4_NASH_Pats

NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(Month))
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, drug_group, drug_class)
NASH_Drug_Histories <- NASH_Drug_Histories %>% distinct()

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 976705 (7208 ever treated)

NASH_Drug_Histories %>% ungroup() %>% select(patient, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(n=sum(weight)) 

drug_group             n
<chr>              <dbl>
  1 Anticholesterol  730499.
2 Antidiabetic     604496.
3 Antiobesity      114826.
4 GLP1 Injectable  170337.
5 GLP1 Oral          9950.
6 Hepatoprotective  51530.
7 Hospitalization  118449.

FIB4_NASH_Pats <- FIB4_NASH_Pats %>% left_join(NASH_Drug_Histories %>% filter(drug_group=="GLP1 Oral" | drug_group=="GLP1 Injectable") %>% 
                                                 select(patient) %>% distinct() %>% mutate(GLP1_Exp="GLP1_Exp"))

FIB4_NASH_Pats <- FIB4_NASH_Pats %>% mutate(GLP1_Exp = ifelse(is.na(GLP1_Exp), "None", GLP1_Exp))

FIB4_NASH_Pats %>% filter(AST<3000 & ALT<3000) %>% group_by(GLP1_Exp) %>% summarise(mean(AST))

FIB4_NASH_Pats %>% filter(AST<3000 & ALT<3000) %>% group_by(GLP1_Exp) %>% count()

FIB4_NASH_Pats %>% filter(AST<200 & ALT<200) %>% group_by(GLP1_Exp) %>%  sample_n(1000) %>%
  ggplot(aes(AST, ALT, colour=factor(GLP1_Exp, levels=c("None", "GLP1_Exp")))) +
  geom_point(alpha=0.5) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("ALT (IU/L) \n")+xlab("\n AST (IU/L")

# ------
# LAB scores before/after SGLT" --------------

NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))


First_SGLT2 <- NASH_Drug_Histories %>% filter(drug_class=="SGLT2") %>% 
  group_by(patient) %>% slice(1) %>% select(patient, Month) 

First_SGLT2$Month <- as.character(First_SGLT2$Month)
First_SGLT2$Month <- parse_number(First_SGLT2$Month)

FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_NASH_Pats$claimed <- as.Date(FIB4_NASH_Pats$claimed)

First_SGLT2 <- First_SGLT2 %>% inner_join(FIB4_NASH_Pats)

First_SGLT2$claimed <- format(as.Date(First_SGLT2$claimed), "%Y-%m")


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

First_SGLT2 <- First_SGLT2 %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% 
  drop_na() %>% select(-claimed)

First_SGLT2 <- First_SGLT2 %>% mutate(elapsedTime=Month-Exact_Month) %>% select(-c(Month, Exact_Month, age))

Pats_to_keep <- First_SGLT2 %>% filter(elapsedTime<0) %>% select(patient) %>% distinct() %>%
  inner_join(First_SGLT2 %>% filter(elapsedTime>0) %>% select(patient) %>% distinct())


Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(AST)) # 40.1

Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(AST)) # 46.5


Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(ALT)) # 50.4

Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(ALT)) # 55.0


Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(fibrosis4)) # 2.55

Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(fibrosis4)) # 2.84


Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(Platelets)) # 213

Pats_to_keep %>% left_join(First_SGLT2) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(Platelets)) # 201















# LAB scores before/after GLP1 Inj ------

NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))


First_GLP1_Inj <- NASH_Drug_Histories %>% filter(drug_group=="GLP1 Injectable") %>% 
  group_by(patient) %>% slice(1) %>% select(patient, Month) 

First_GLP1_Inj$Month <- as.character(First_GLP1_Inj$Month)
First_GLP1_Inj$Month <- parse_number(First_GLP1_Inj$Month)

FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_NASH_Pats$claimed <- as.Date(FIB4_NASH_Pats$claimed)

First_GLP1_Inj <- First_GLP1_Inj %>% inner_join(FIB4_NASH_Pats)

First_GLP1_Inj$claimed <- format(as.Date(First_GLP1_Inj$claimed), "%Y-%m")


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

First_GLP1_Inj <- First_GLP1_Inj %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% 
  drop_na() %>% select(-claimed)

First_GLP1_Inj <- First_GLP1_Inj %>% mutate(elapsedTime=Month-Exact_Month) %>% select(-c(Month, Exact_Month, age))

Pats_to_keep <- First_GLP1_Inj %>% filter(elapsedTime<0) %>% select(patient) %>% distinct() %>%
  inner_join(First_GLP1_Inj %>% filter(elapsedTime>0) %>% select(patient) %>% distinct())



Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(AST)) # 63.5

Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(AST)) # 42.6


Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(ALT)) # 75.0

Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(ALT)) # 49.6


Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(fibrosis4)) # 3.85

Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(fibrosis4)) # 2.46


Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(Platelets)) # 203

Pats_to_keep %>% left_join(First_GLP1_Inj) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(Platelets)) # 207














# LAB scores before/after Surgery ------

NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

NASH_Ingredients$class <- as.numeric(NASH_Ingredients$class)
NASH_Ingredients$molecule <- as.numeric(NASH_Ingredients$molecule)

NASH_Drug_Histories <- fread("NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, month1:month60)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Treat, sep = ",", convert=T )
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Treat != "-")

names(NASH_Drug_Histories)[4] <- "molecule"
NASH_Drug_Histories$molecule <- as.numeric(NASH_Drug_Histories$molecule)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients %>% 
                                                           select(molecule, drug_group, drug_class))


First_Surgery <- NASH_Drug_Histories %>% filter(drug_class=="Liver Transplant") %>% 
  group_by(patient) %>% slice(1) %>% select(patient, Month) 

First_Surgery$Month <- as.character(First_Surgery$Month)
First_Surgery$Month <- parse_number(First_Surgery$Month)

FIB4_NASH_Pats <- fread("FIB4_NASH_Pats.txt")
FIB4_NASH_Pats$claimed <- as.Date(FIB4_NASH_Pats$claimed)

First_Surgery <- First_Surgery %>% inner_join(FIB4_NASH_Pats)

First_Surgery$claimed <- format(as.Date(First_Surgery$claimed), "%Y-%m")


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

First_Surgery <- First_Surgery %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% 
  drop_na() %>% select(-claimed)

First_Surgery <- First_Surgery %>% mutate(elapsedTime=Month-Exact_Month) %>% select(-c(Month, Exact_Month, age))

Pats_to_keep <- First_Surgery %>% filter(elapsedTime<0) %>% select(patient) %>% distinct() %>%
  inner_join(First_Surgery %>% filter(elapsedTime>0) %>% select(patient) %>% distinct())



Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(AST)) # 45.2

Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(AST)) # 302


Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(ALT)) # 51.4

Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(ALT)) # 140


Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(fibrosis4)) # 4.65

Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(fibrosis4)) # 15.7


Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(Platelets)) # 175

Pats_to_keep %>% left_join(First_Surgery) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(Platelets)) # 175








# ---------
# Drug Specialties all ----------------

NASH_Drug_Specialties_Annual <- fread("NASH Drug Specialties Annual.txt")

NASH_Drug_Specialties_Annual %>% group_by(specialty) %>% 
  summarise(n=sum(physician_sample)) %>%
  arrange(-n)

specialty                n
<chr>                <int>
1 Primary Care         55136
2 Internal Medicine    41969
3 Other Provider       28567
4 Diabetes Specialist  16017
5 Other Physician       9551
6 Cardiologist          8586
7 Unknown               8265
8 Gastroenterologist    3320
9 Endocrinologist       3213
10 Surgeon               2726
11 Facility              1937
12 Nephrologist          1093
13 Neurologist            581
14 Hepatologist           123
15 Nutrition Specialist    27



NAFLD_Drug_Specialties_Annual <- fread("NAFLD Drug Specialties Annual.txt")

NAFLD_Drug_Specialties_Annual %>% group_by(specialty) %>% 
  summarise(n=sum(physician_sample)) %>%
  arrange(-n)

# specialty                 n
# <chr>                 <int>
#   1 Primary Care         422812
# 2 Internal Medicine    291073
# 3 Other Provider       213265
# 4 Other Physician       72058
# 5 Diabetes Specialist   70681
# 6 Cardiologist          70079
# 7 Unknown               49272
# 8 Endocrinologist       18883
# 9 Surgeon               13993
# 10 Gastroenterologist    13296
# 11 Nephrologist           6383
# 12 Facility               5851
# 13 Neurologist            3692
# 14 Hepatologist            245
# 15 Nutrition Specialist    119



# Drug Specialties all ----------------

NASH_Drug_Specialties_Annual <- fread("NASH Drug Specialties Annual.txt")

NASH_Drug_Specialties_Annual %>% group_by(specialty) %>% 
  summarise(n=sum(physician_sample)) %>%
  arrange(-n)

specialty                n
<chr>                <int>
  1 Primary Care         55136
2 Internal Medicine    41969
3 Other Provider       28567
4 Diabetes Specialist  16017
5 Other Physician       9551
6 Cardiologist          8586
7 Unknown               8265
8 Gastroenterologist    3320
9 Endocrinologist       3213
10 Surgeon               2726
11 Facility              1937
12 Nephrologist          1093
13 Neurologist            581
14 Hepatologist           123
15 Nutrition Specialist    27



NAFLD_Drug_Specialties_Annual <- fread("NAFLD Drug Specialties Annual.txt")

NAFLD_Drug_Specialties_Annual %>% group_by(specialty) %>% 
  summarise(n=sum(physician_sample)) %>%
  arrange(-n)

# specialty                 n
# <chr>                 <int>
#   1 Primary Care         422812
# 2 Internal Medicine    291073
# 3 Other Provider       213265
# 4 Other Physician       72058
# 5 Diabetes Specialist   70681
# 6 Cardiologist          70079
# 7 Unknown               49272
# 8 Endocrinologist       18883
# 9 Surgeon               13993
# 10 Gastroenterologist    13296
# 11 Nephrologist           6383
# 12 Facility               5851
# 13 Neurologist            3692
# 14 Hepatologist            245
# 15 Nutrition Specialist    119

# -----


# -------
# ---------

# Proportion  GLP1 exp last year in DIA / OB , Dxed, predited ----------
# All DIA treat exp
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1Injectable <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1Oral <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories     <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector   <- fread("Treatment_exp_Vector.txt")
DIA_Drug_Histories     <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, month49:month60)
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 30625690
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Month)

DIA_Drug_Histories %>% filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3831181  (0.125097)


#NASH Dxed
NASH_Dx <- fread("NASH Drug Histories.txt")
NASH_Dx <- NASH_Dx %>% select(4:63)
NASH_Dx[NASH_Dx != "-"] <- 1  # on drug 
NASH_Dx[NASH_Dx == "-"] <- 0  # no drug

NASH_Dx[] <- lapply(NASH_Dx,as.numeric)

NASH_Dx$SUM <- rowSums(NASH_Dx)

NASH_Dx_LONG <- fread("NASH Drug Histories.txt")

Pats_vec <- NASH_Dx_LONG %>% select(patient, weight)

NASH_Dx <- Pats_vec %>% bind_cols(NASH_Dx)

NASH_Dx <- NASH_Dx %>% filter(SUM != 0)

sum(NASH_Dx$weight) # 976704.5

Treatment_exp_Vector <- NASH_Dx %>% select(patient, weight)

fwrite(Treatment_exp_Vector, "Treatment_exp_Vector.txt")
NASH_Treatment_exp_Vector <- Treatment_exp_Vector

NASH_Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

NASH_Dx <- fread("NASH Drug Histories.txt") 
NASH_Dx <- NASH_Dx %>% inner_join(NASH_Treatment_exp_Vector) %>% inner_join(DIA_Drug_Histories %>% select(patient))
NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients$drug_id <- unlist(lapply(NASH_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1InjectableNASH <- paste0("\\b(",paste0(NASH_Ingredients$drug_id[NASH_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1OralNASH <- paste0("\\b(",paste0(NASH_Ingredients$drug_id[NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
NASH_Dx %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 611186.6

NASH_Dx <- NASH_Dx %>% select(patient, weight, month49:month60)
NASH_Dx <- gather(NASH_Dx, Month, Drugs, month49:month60, factor_key=TRUE)
NASH_Dx <- NASH_Dx %>% select(-Month)

NASH_Dx %>% filter(grepl(string_GLP1InjectableNASH,Drugs)|grepl(string_GLP1OralNASH,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 118630.7  (0.1234318)  


# High risk predicted
DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
DIA_Pats_Score95_2plus %>% inner_join(DIA_Drug_Histories) %>% select(patient, weight) %>%
  distinct() %>% summarise(n=sum(weight)) # 1440396

DIA_Pats_Score95_2plus %>% inner_join(DIA_Drug_Histories) %>% 
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 194430.4 (0.134984)


DIA_Drug_Histories %>% anti_join(DIA_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 28574108

# Non NASH
DIA_Drug_Histories %>% anti_join(DIA_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3518120








# All OBE treat exp
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1Injectable <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1Oral <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector   <- fread("Treatment_exp_Vector.txt")
OBE_Drug_Histories     <- Treatment_exp_Vector %>% left_join(OBE_Drug_Histories) 

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient, weight, month49:month60)
OBE_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 9155116
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Month)

OBE_Drug_Histories %>% filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 158857.6  (0.01735178)


#NASH Dxed
NASH_Dx <- fread("NASH Drug Histories.txt")
NASH_Dx <- NASH_Dx %>% select(4:63)
NASH_Dx[NASH_Dx != "-"] <- 1  # on drug 
NASH_Dx[NASH_Dx == "-"] <- 0  # no drug

NASH_Dx[] <- lapply(NASH_Dx,as.numeric)

NASH_Dx$SUM <- rowSums(NASH_Dx)

NASH_Dx_LONG <- fread("NASH Drug Histories.txt")

Pats_vec <- NASH_Dx_LONG %>% select(patient, weight)

NASH_Dx <- Pats_vec %>% bind_cols(NASH_Dx)

NASH_Dx <- NASH_Dx %>% filter(SUM != 0)

sum(NASH_Dx$weight) # 976704.5

Treatment_exp_Vector <- NASH_Dx %>% select(patient, weight)

fwrite(Treatment_exp_Vector, "Treatment_exp_Vector.txt")
NASH_Treatment_exp_Vector <- Treatment_exp_Vector

NASH_Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

NASH_Dx <- fread("NASH Drug Histories.txt")
NASH_Dx <- NASH_Dx %>% inner_join(NASH_Treatment_exp_Vector)

NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients$drug_id <- unlist(lapply(NASH_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1InjectableNASH <- paste0("\\b(",paste0(NASH_Ingredients$drug_id[NASH_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1OralNASH <- paste0("\\b(",paste0(NASH_Ingredients$drug_id[NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
NASH_Dx %>% inner_join(OBE_Drug_Histories %>% select(patient)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 61842.46
NASH_Dx <- NASH_Dx %>% inner_join(OBE_Drug_Histories %>% select(patient))
NASH_Dx <- NASH_Dx %>% select(patient, weight, month49:month60)
NASH_Dx <- gather(NASH_Dx, Month, Drugs, month49:month60, factor_key=TRUE)
NASH_Dx <- NASH_Dx %>% select(-Month)

NASH_Dx %>% filter(grepl(string_GLP1InjectableNASH,Drugs)|grepl(string_GLP1OralNASH,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1925.65  (0.03113799)  


# High risk predicted
OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")
OBE_Pats_Score95_2plus %>% inner_join(OBE_Drug_Histories) %>% select(patient, weight) %>%
  distinct() %>% summarise(n=sum(weight)) # 225370.2

OBE_Pats_Score95_2plus %>% inner_join(OBE_Drug_Histories) %>% 
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 4078.61 (0.01809738)


OBE_Drug_Histories %>% anti_join(OBE_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 8867903

# Non NASH
OBE_Drug_Histories %>% anti_join(OBE_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 152853.4 0.0172367

# ------------














# HF in DIA
DIA_Comorbidity_Inventories <- fread("DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories %>% filter(diagnosis=="I51") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))
# n
#  7861486
#  7861486/48233000
#  0.1629898

# HF in OBE
OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories %>% filter(diagnosis=="I51") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))
n
1 6488700









DIA_Drug_Histories <- fread("DIA Drug Histories.txt")
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis)
names(DANU_Demographics)[1] <- "patient"

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Demographics)

DIA_Drug_Histories %>% group_by(diagnosis) %>% summarise(n=sum(weight))


# DIA / OBE Vector
Diabetes_OBesity_Pats <- DIA_Drug_Histories
Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% select(patient, diagnosis)

# OBE Vector
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% mutate(diagnosis="Obesity")
OBE_Drug_Histories <- OBE_Drug_Histories %>% anti_join(DIA_Drug_Histories %>% select(patient))

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% bind_rows(OBE_Drug_Histories)

DANU_Demographics <- fread("DANU Demographics.txt")

Diabetes_OBesity_Pats <- Diabetes_OBesity_Pats %>% left_join(DANU_Demographics %>% select(patid, weight), by=c("patient"="patid")) 

Diabetes_OBesity_Pats %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# diagnosis                   n
# <chr>                   <dbl>
#   1 Diabetes             7949715.
# 2 Diabetes + Obesity  40282960.
# 3 Obesity            106469049.

OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")
DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")


Diabetes_OBesity_Pats %>% inner_join(DIA_Pats_Score95_2plus) %>% summarise(n=sum(weight))
Diabetes_OBesity_Pats %>% inner_join(OBE_Pats_Score95_2plus) %>% summarise(n=sum(weight))











NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt")

NAFLD <- NAFLD_Pats %>% select(patient)
DIA_Pats <- DIA_Pats %>% anti_join(NAFLD)
OBE_Pats <- OBE_Pats %>% anti_join(NAFLD)

NASH_Pats$group <- "NASH"
DIA_Pats$group <- "DIA"
OBE_Pats$group <- "OBE"
NAFLD_Pats$group <- "NAFLD"
Random_Pats$group <- "Random Sample"

DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")

DIA_Pats_HighRisk <- DIA_Pats_Score95_2plus %>% left_join(DIA_Pats)
OBE_Pats_HighRisk <- OBE_Pats_Score95_2plus %>% left_join(OBE_Pats)


DIA_Pats <- DIA_Pats %>% anti_join(DIA_Pats_Score95_2plus)
OBE_Pats <- OBE_Pats %>% anti_join(OBE_Pats_Score95_2plus)


# % share of GLP1: source Dx, Predicted or other DIA/OBE ----------------------

# All DIA treat exp
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1Injectable <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1Oral <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories     <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector   <- fread("Treatment_exp_Vector.txt")
DIA_Drug_Histories     <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) 

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, month49:month60)
DIA_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 30625690
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-Month)

DIA_Drug_Histories %>% filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3831181  (0.125097)

GLP1_Pats <- 
  DIA_Drug_Histories %>% filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct()


#NASH Dxed

NASH_Dx <- fread("NASH Drug Histories.txt") 
NASH_Dx <- NASH_Dx %>% select(patient) %>% inner_join(DIA_Drug_Histories)
NASH_Dx %>%  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 611186.6

NASH_Dx %>% filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 118630.7  (0.125097)


GLP1_Pats_NASHDx <- NASH_Dx %>% filter(grepl(string_GLP1InjectableNASH,Drugs)|grepl(string_GLP1OralNASH,Drugs)) %>% 
  select(patient, weight) %>% distinct()



# High risk predicted
DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
DIA_Pats_Score95_2plus %>% inner_join(DIA_Drug_Histories) %>% select(patient, weight) %>%
  distinct() %>% summarise(n=sum(weight)) # 1440396

DIA_Pats_Score95_2plus %>% inner_join(DIA_Drug_Histories) %>% 
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 194430.4 (0.134984)

GLP1_Pats_NASHPred <- 
  DIA_Pats_Score95_2plus %>% inner_join(DIA_Drug_Histories) %>%  
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct()


DIA_Drug_Histories %>% anti_join(DIA_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 28574108

# Non NASH
DIA_Drug_Histories %>% anti_join(DIA_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3518120


# > 118630.7/3831181
# [1] 0.03096453
# > 194430.4/3831181
# [1] 0.05074947
# > 1440396/30625690
# [1] 0.04703228
# > 0.1/0.04703228
# [1] 2.126199
# > 0.05074947*2.126199









# All OBE treat exp
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients$drug_id <- unlist(lapply(DANU_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1Injectable <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1Oral <- paste0("\\b(",paste0(DANU_Ingredients$drug_id[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector   <- fread("Treatment_exp_Vector.txt")
OBE_Drug_Histories     <- Treatment_exp_Vector %>% left_join(OBE_Drug_Histories) 

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient, weight, month49:month60)
OBE_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 9155116
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Month)

OBE_Drug_Histories %>% filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 158857.6  (0.01735178)


GLP1_Pats <- 
  OBE_Drug_Histories %>% filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct()


#NASH Dxed
NASH_Dx <- fread("NASH Drug Histories.txt")
NASH_Dx <- NASH_Dx %>% select(4:63)
NASH_Dx[NASH_Dx != "-"] <- 1  # on drug 
NASH_Dx[NASH_Dx == "-"] <- 0  # no drug

NASH_Dx[] <- lapply(NASH_Dx,as.numeric)

NASH_Dx$SUM <- rowSums(NASH_Dx)

NASH_Dx_LONG <- fread("NASH Drug Histories.txt")

Pats_vec <- NASH_Dx_LONG %>% select(patient, weight)

NASH_Dx <- Pats_vec %>% bind_cols(NASH_Dx)

NASH_Dx <- NASH_Dx %>% filter(SUM != 0)

sum(NASH_Dx$weight) # 976704.5

Treatment_exp_Vector <- NASH_Dx %>% select(patient, weight)

fwrite(Treatment_exp_Vector, "Treatment_exp_Vector.txt")
NASH_Treatment_exp_Vector <- Treatment_exp_Vector

NASH_Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

NASH_Dx <- fread("NASH Drug Histories.txt")

NASH_Ingredients <- fread("NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients$drug_id <- unlist(lapply(NASH_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_GLP1InjectableNASH <- paste0("\\b(",paste0(NASH_Ingredients$drug_id[NASH_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1OralNASH <- paste0("\\b(",paste0(NASH_Ingredients$drug_id[NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
NASH_Dx %>% inner_join(OBE_Drug_Histories %>% select(patient)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 61842.46
NASH_Dx <- NASH_Dx %>% inner_join(OBE_Drug_Histories %>% select(patient))
NASH_Dx <- NASH_Dx %>% select(patient, weight, month49:month60)
NASH_Dx <- gather(NASH_Dx, Month, Drugs, month49:month60, factor_key=TRUE)
NASH_Dx <- NASH_Dx %>% select(-Month)
NASH_Dx %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 61842.46
NASH_Dx %>% filter(grepl(string_GLP1InjectableNASH,Drugs)|grepl(string_GLP1OralNASH,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1925.65  (0.03113799)  

GLP1_Pats_NASHDx <- NASH_Dx %>% filter(grepl(string_GLP1InjectableNASH,Drugs)|grepl(string_GLP1OralNASH,Drugs)) %>% 
  select(patient, weight) %>% distinct()


# High risk predicted
OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")
OBE_Pats_Score95_2plus %>% inner_join(OBE_Drug_Histories) %>% select(patient, weight) %>%
  distinct() %>% summarise(n=sum(weight)) # 225370.2

OBE_Pats_Score95_2plus %>% inner_join(OBE_Drug_Histories) %>% 
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 4078.61 (0.01809738)


GLP1_Pats_NASHPred <- 
  OBE_Pats_Score95_2plus %>% inner_join(OBE_Drug_Histories) %>%  
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct()


OBE_Drug_Histories %>% anti_join(OBE_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 8867903

# Non NASH
OBE_Drug_Histories %>% anti_join(OBE_Pats_Score95_2plus %>% select(patient)) %>% anti_join(NASH_Dx %>% select(patient)) %>%
  filter(grepl(string_GLP1Injectable,Drugs)|grepl(string_GLP1Oral,Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 152853.4 0.0172367

# [1] 0.1079035


GLP1_Pats %>% summarise(n=sum(weight)) # 158857.6

GLP1_Pats %>% inner_join(GLP1_Pats_NASHDx) %>% summarise(n=sum(weight)) # 1925.65 (0.01212186)

GLP1_Pats %>% inner_join(GLP1_Pats_NASHPred) %>% summarise(n=sum(weight)) # 4078.61 (0.02567463) but only 0.02461686, should 3x this


# -----------
# ------------
# Drug Usage T2D with NASH Dx vs non NASH ---------------
DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients$class <- as.numeric(DANU_Ingredients$class)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, month49:month60)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)

DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T )
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")

names(DIA_Drug_Histories)[3] <- "Month"
names(DIA_Drug_Histories)[4] <- "molecule"
DIA_Drug_Histories$molecule <- as.numeric(DIA_Drug_Histories$molecule)

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% 
                                                         select(molecule, drug_group))

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, drug_group)
DIA_Drug_Histories <- DIA_Drug_Histories %>% distinct()
Drugs_temp <- DIA_Drug_Histories


Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
NASH_Pats <- fread("NASH Drug Histories.txt") %>% select(patient, weight)
DIA_Pats <- fread("DIA Drug Histories.txt") %>% select(patient, weight)
DIA_Pats %>% select(patient, weight) %>% distinct() %>% inner_join(Treatment_exp_Vector) %>% summarise(n=sum(weight)) # 30625690
DIA_Pats %>% inner_join(NASH_Pats) %>% inner_join(Treatment_exp_Vector) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 611186.6
NASH_Pats$Group <- "NASH_Diagnosed"



Drugs_temp %>% left_join(NASH_Pats) %>% inner_join(Treatment_exp_Vector) %>% ungroup() %>% select(patient, weight, drug_group,Group) %>% distinct() %>% group_by(Group, drug_group) %>% summarise(n=sum(weight)) 

Group          drug_group              n
<chr>          <chr>               <dbl>
  1 NASH_Diagnosed Antidiabetic      130548.
2 NASH_Diagnosed Biguanide         334040.
3 NASH_Diagnosed DPP4               66868.
4 NASH_Diagnosed GLP1 Injectable   111333.
5 NASH_Diagnosed GLP1 Oral           9031.
6 NASH_Diagnosed Insulin           189566.
7 NASH_Diagnosed SGLT2              86736.
8 NA             Antidiabetic     5924009.
9 NA             Biguanide       15747503.
10 NA             DPP4             2778203.
11 NA             GLP1 Injectable  3526807.
12 NA             GLP1 Oral         239248.
13 NA             Insulin          7428099.
14 NA             SGLT2            3039571.


# ------------
# Drug Usage T2D with High risk NASH vs Low risk NASH ----------

DANU_Ingredients <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients$class <- as.numeric(DANU_Ingredients$class)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)

DIA_Drug_Histories <- fread("DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, month49:month60)
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)

DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T )
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs != "-")

names(DIA_Drug_Histories)[3] <- "Month"
names(DIA_Drug_Histories)[4] <- "molecule"
DIA_Drug_Histories$molecule <- as.numeric(DIA_Drug_Histories$molecule)

DIA_Drug_Histories <- DIA_Drug_Histories %>% left_join(DANU_Ingredients %>% 
                                                         select(molecule, drug_group))

DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient, weight, drug_group)
DIA_Drug_Histories <- DIA_Drug_Histories %>% distinct()
Drugs_temp <- DIA_Drug_Histories

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Drugs_temp <- Drugs_temp %>% inner_join(Treatment_exp_Vector)

NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
DIA_Pats <- DIA_Pats %>% anti_join(NAFLD_Pats %>% select(patient)) %>% anti_join(NASH_Pats %>% select(patient))
DIA_Pats <- DIA_Pats %>% select(patient)%>% distinct()
DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
DIA_Pats_Score95_2plus$Group <-"HighRisk"


Drugs_temp <- Drugs_temp %>% inner_join(DIA_Pats)
Drugs_temp <- Drugs_temp %>% left_join(DIA_Pats_Score95_2plus)

DIA_Pats <- fread("DIA Drug Histories.txt") %>% select(patient, weight)
DIA_Pats %>% select(patient, weight) %>% distinct() %>% inner_join(Drugs_temp %>% select(patient)) %>% summarise(n=sum(weight)) # 14249936
DIA_Pats %>% inner_join(DIA_Pats_Score95_2plus) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 2053529

Drugs_temp %>% ungroup() %>% select(patient, weight, drug_group,Group) %>% distinct() %>% group_by(Group, drug_group) %>% summarise(n=sum(weight)) 

Group    drug_group             n
<chr>    <chr>              <dbl>
  1 HighRisk Antidiabetic     204879.
2 HighRisk Biguanide        489103.
3 HighRisk DPP4              94871.
4 HighRisk GLP1 Injectable  113073.
5 HighRisk GLP1 Oral          6571.
6 HighRisk Insulin          298695.
7 HighRisk SGLT2             97074.
8 NA       Antidiabetic    2084667.
9 NA       Biguanide       5334432.
10 NA       DPP4             901773.
11 NA       GLP1 Injectable 1097434.
12 NA       GLP1 Oral         70023.
13 NA       Insulin         2530248.
14 NA       SGLT2            927092.



# ------


# Within each stock,  % high risk vs % low risk -------

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

DIA_Box_Histories     <- fread("DIA Box Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Box_Histories     <- DIA_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L)) %>% select(patient, month60)
names(DIA_Box_Histories)[2] <- "Stock_m60"
DIA_Box_Histories <- DIA_Box_Histories %>% inner_join(Treatment_exp_Vector)

DIA_Box_Histories 



NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
DIA_Pats <- DIA_Pats %>% anti_join(NAFLD_Pats %>% select(patient)) %>% anti_join(NASH_Pats %>% select(patient))
DIA_Pats <- DIA_Pats %>% select(patient)%>% distinct()
DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
DIA_Pats_Score95_2plus$Group <-"HighRisk"

DIA_Box_Histories <- DIA_Box_Histories %>% inner_join(DIA_Pats)
DIA_Box_Histories <- DIA_Box_Histories %>% left_join(DIA_Pats_Score95_2plus)
DIA_Box_Histories %>% group_by(Group) %>% summarise(n=sum(weight))

DIA_Box_Histories %>% group_by(Stock_m60, Group) %>% summarise(n=sum(weight))


Stock_m60 Group           n
<chr>     <chr>       <dbl>
1 b         HighRisk  176137.
2 b         NA       2082092.
3 d         HighRisk   80985.
4 d         NA        897441.
5 D         HighRisk   36776.
6 D         NA        395312.
7 g         HighRisk    3310.
8 g         NA         40444.
9 G         HighRisk   80856.
10 G         NA        771862.
11 I         HighRisk  169197.
12 I         NA       1417139.
13 S         HighRisk   37155.
14 S         NA        370433.
15 x         HighRisk  405022.
16 x         NA       4154753.
# -------------




# NASH DIA OBE vs Heart Failure --------------

DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")
DIA_Pats_Score95_2plus$Group <- "HighRisk"
OBE_Pats_Score95_2plus$Group <- "HighRisk"


NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt") 
Random_Pats <- Random_Pats %>% select(patient)

DIA_Pats <- DIA_Pats %>% anti_join(NAFLD_Pats %>% select(patient)) %>% anti_join(NASH_Pats %>% select(patient))
DIA_Pats <- DIA_Pats %>% select(patient)%>% distinct()
OBE_Pats <- OBE_Pats %>% anti_join(NAFLD_Pats %>% select(patient)) %>% anti_join(NASH_Pats %>% select(patient))
OBE_Pats <- OBE_Pats %>% select(patient)%>% distinct()

NASH_Dossiers <- fread("NASH Dossiers.txt")
Cancer_Alcohol_pats <- NASH_Dossiers %>% filter(condition == "Liver Cancer" | condition == "Alcohol Abuse") %>% select(patid) %>% distinct()
names(Cancer_Alcohol_pats)[1] <- "patient"

DIA_Pats <- DIA_Pats %>% anti_join(Cancer_Alcohol_pats)
OBE_Pats <- OBE_Pats %>% anti_join(Cancer_Alcohol_pats)

DIA_Pats <- DIA_Pats %>% left_join(DIA_Pats_Score95_2plus) %>% mutate(Group=ifelse(is.na(Group),"LowRisk",Group))
OBE_Pats <- OBE_Pats %>% left_join(OBE_Pats_Score95_2plus) %>% mutate(Group=ifelse(is.na(Group),"LowRisk",Group))


DANU_Demographics <- fread("DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"

DIA_Pats <- DIA_Pats %>% left_join(DANU_Demographics %>% select(patient, weight))
OBE_Pats <- OBE_Pats %>% left_join(DANU_Demographics %>% select(patient, weight))




ce18HFdxs <- fread("ce18HFdxs.txt")
ce18HFdxs %>% filter(diag!="I97131"&diag!="I97130") %>% select(ptid) %>% distinct()
# ce18HFdxs %>% filter(diag=="I501") %>% select(ptid) %>% distinct()

ce18HFpts <- fread("ce18HFpts.txt")
ce18HFpts <- ce18HFpts %>% mutate(gender=ifelse(gender=="Male","M","F"))
#ce18HFpts <- ce18HFpts %>% left_join(DANU_Demographics %>% select(age, gender, weight) %>% distinct())
sum(ce18HFpts$weight) # 69157993
ce18HFpts$HeartFailure <- "HeartFailure"
names(ce18HFpts)[1] <- "patient"
ce18HFpts <- ce18HFpts %>% select(patient, HeartFailure)
DIA_Pats %>% left_join(ce18HFpts) %>% group_by(Group, HeartFailure) %>% summarise(n=sum(weight))

Group    HeartFailure         n
<chr>    <chr>            <dbl>
  1 HighRisk HeartFailure   351620.
2 HighRisk NA            1118014.
3 LowRisk  HeartFailure  2131347.
4 LowRisk  NA           12671883.

OBE_Pats %>% left_join(ce18HFpts) %>% group_by(Group, HeartFailure) %>% summarise(n=sum(weight))

Group    HeartFailure         n
<chr>    <chr>            <dbl>
  1 HighRisk HeartFailure   224897.
2 HighRisk NA            2129214.
3 LowRisk  HeartFailure  1363490.
4 LowRisk  NA           35243437.



# Mar's data 
DIA_Pats_Score95_2plus <- fread("DIA_Pats_Score95_2plus.txt")
OBE_Pats_Score95_2plus <- fread("OBE_Pats_Score95_2plus.txt")
DIA_Pats_Score95_2plus$Group <- "HighRisk"
OBE_Pats_Score95_2plus$Group <- "HighRisk"


NASH_Pats <- fread("FIB4_NASH_Pats.txt")
DIA_Pats <- fread("FIB4_Diabetes_Pats.txt")
OBE_Pats <- fread("FIB4_Obesity_Pats.txt")
NAFLD_Pats <- fread("FIB4_NAFLD_Pats.txt")
Random_Pats <- fread("FIB4_Random_Pats_Filtered.txt") 
Random_Pats <- Random_Pats %>% select(patient)


DIA_Pats <- DIA_Pats %>% anti_join(NAFLD_Pats %>% select(patient)) %>% anti_join(NASH_Pats %>% select(patient))
DIA_Pats <- DIA_Pats %>% select(patient)%>% distinct()
OBE_Pats <- OBE_Pats %>% anti_join(NAFLD_Pats %>% select(patient)) %>% anti_join(NASH_Pats %>% select(patient))
OBE_Pats <- OBE_Pats %>% select(patient)%>% distinct()

NASH_Dossiers <- fread("NASH Dossiers.txt")
Cancer_Alcohol_pats <- NASH_Dossiers %>% filter(condition == "Liver Cancer" | condition == "Alcohol Abuse") %>% select(patid) %>% distinct()
names(Cancer_Alcohol_pats)[1] <- "patient"

DIA_Pats <- DIA_Pats %>% anti_join(Cancer_Alcohol_pats)
OBE_Pats <- OBE_Pats %>% anti_join(Cancer_Alcohol_pats)

DIA_Pats <- DIA_Pats %>% left_join(DIA_Pats_Score95_2plus) %>% mutate(Group=ifelse(is.na(Group),"LowRisk",Group))
OBE_Pats <- OBE_Pats %>% left_join(OBE_Pats_Score95_2plus) %>% mutate(Group=ifelse(is.na(Group),"LowRisk",Group))


DANU_Demographics <- fread("DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(patient, weight,diagnosis , heart_failure_condition)
DANU_Demographics <- DANU_Demographics %>% mutate(heart_failure_condition=ifelse(heart_failure_condition=="-",0,1))

DIA_Pats <- DIA_Pats %>% left_join(DANU_Demographics)
OBE_Pats <- OBE_Pats %>% left_join(DANU_Demographics)


DIA_Pats %>% ungroup() %>% group_by(diagnosis, Group, heart_failure_condition) %>% summarise(n=sum(weight))

# diagnosis          Group    heart_failure_condition         n
# <chr>              <chr>                      <dbl>     <dbl>
# 1 Diabetes           HighRisk                       0    92010.
# 2 Diabetes           HighRisk                       1    29113.
# 3 Diabetes           LowRisk                        0  1521264.
# 4 Diabetes           LowRisk                        1   249646.
# 5 Diabetes + Obesity HighRisk                       0   975960.
# 6 Diabetes + Obesity HighRisk                       1   372383.
# 7 Diabetes + Obesity LowRisk                        0 10733604.
# 8 Diabetes + Obesity LowRisk                        1  2293881.
# 9 Obesity            HighRisk                       0      168.
# 10 Obesity            LowRisk                        0     4835.


OBE_Pats %>% ungroup() %>% group_by(diagnosis, Group, heart_failure_condition) %>% summarise(n=sum(weight))

# diagnosis Group    heart_failure_condition         n
# <chr>     <chr>                      <dbl>     <dbl>
# 1 Obesity   HighRisk                       0  2083707.
# 2 Obesity   HighRisk                       1   270404.
# 3 Obesity   LowRisk                        0 34927223.
# 4 Obesity   LowRisk                        1  1679705.



NASH_Dx <- fread("NASH Drug Histories.txt")
NASH_Dx %>% select(patient) %>% left_join(DANU_Demographics) %>% group_by(diagnosis, heart_failure_condition) %>% summarise(n=sum(weight))

diagnosis          heart_failure_condition       n
<chr>                                <dbl>   <dbl>
  # 1 -                                        0  66071.
  # 2 -                                        1   2873.
  # 3 Diabetes                                 0  39783.
  # 4 Diabetes                                 1   5590.
  # 5 Diabetes + Obesity                       0 607159.
  # 6 Diabetes + Obesity                       1 139742.
  # 7 Obesity                                  0 448171.
  # 8 Obesity                                  1  30594.
  
  
  
DANU_Demographics <- fread("DANU Demographics.txt")
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% filter(heart_failure_condition!="-")

DANU_Demographics %>% group_by(diagnosis, heart_failure_condition) %>%  summarise(n=sum(weight))


# ------------

# Statins med strength  -----------
NAFLD_US_Doses <- fread("NAFLD Doses.txt")
NAFLD_US_Doses <- NAFLD_US_Doses %>% filter(paid == "P")
unique(NAFLD_US_Doses$drug_class)
NAFLD_US_Doses <- NAFLD_US_Doses %>% filter(drug_class=="Statin") %>% select(pat_id, generic_name, drug_id, from_dt) %>% distinct()

NASH_Medication_Surveys <- fread("NASH Medication Surveys.txt")

NAFLD_US_Doses <- NAFLD_US_Doses %>% left_join(NASH_Medication_Surveys %>% select(drug_id, med_strength))

NAFLD_US_Doses %>% select(generic_name, med_strength) %>% distinct() %>% arrange(generic_name, med_strength)

data.frame(NAFLD_US_Doses %>% group_by(generic_name, med_strength) %>% count()) %>% arrange(generic_name, n)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis!="-") %>% select(patid, weight, diagnosis)
DANU_Demographics <- DANU_Demographics %>% mutate(diagnosis=ifelse(grepl("Diabetes", diagnosis), "Diabetes", diagnosis))
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))

NAFLD_Demographics <- fread("NAFLD Demographics.txt")
DANU_Demographics <- NAFLD_Demographics %>% select(patid) %>% inner_join(DANU_Demographics)
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 Diabetes  6700497.
# 2 Obesity   6518298.

sum(NAFLD_Demographics$weight) #14471091

names(NAFLD_US_Doses)[1] <- "patid"

max(NAFLD_US_Doses$from_dt)

NAFLD_US_Doses <- NAFLD_US_Doses %>% filter(from_dt>="2020-07-01")

DANU_Demographics %>% inner_join(NAFLD_US_Doses %>% select(patid) %>% distinct()) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))
  
# 1 Diabetes  4227007.
# 2 Obesity   2074579.

DANU_Demographics %>% inner_join(NAFLD_US_Doses %>% select(patid, generic_name) %>% distinct()) %>%
  group_by(diagnosis, generic_name) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=generic_name, value=n)

#   diagnosis Atorvastatin Fluvastatin Lovastatin Pitavastatin Pravastatin Rosuvastatin Simvastatin
# 1 Diabetes      2761769.       6373.    144848.       46677.     624054.     1019557.     918576.
# 2 Obesity       1279623.       3028.     58364.       22414.     284119.      502069.     398770.


data.frame(DANU_Demographics %>% inner_join(NAFLD_US_Doses %>% select(patid, generic_name, med_strength) %>% distinct()) %>%
  group_by(diagnosis, generic_name, med_strength) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=generic_name, value=n))