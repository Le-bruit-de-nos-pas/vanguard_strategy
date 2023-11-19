library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)


# import files ---------------------------
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 <- fread("Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2.txt")
names(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2)[1] <- "patid"


# ---------------------------------
# PCOS ----------------


# Check https://academic.oup.com/humrep/article/31/12/2841/2730240
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>%
  filter(PCOS==1) %>% select(patid, weight2, PCOS_E25, PCOS_N97, PCOS_N970) %>% 
  #filter(PCOS_N970==1|PCOS_E25==1) %>%
  left_join(DANU_Demographics) %>%
  group_by(PCOS_E25, PCOS_N970) %>% summarise(n=sum(weight2))

#    diagnosis          PCOS_E25 PCOS_N97        n
#  1 Diabetes                  0        1   5588. 
#  2 Diabetes                  1        0    131. 
#  3 Diabetes                  1        1     52.4
#  4 Diabetes + Obesity        0        1  38019. 
#  5 Diabetes + Obesity        1        0    743. 
#  6 Diabetes + Obesity        1        1    166. 
#  7 Obesity                   0        1  18773. 
#  8 Obesity                   1        0    450. 
#  9 Obesity                   1        1    139. 
# 10 NA                        0        1 505973. 
# 11 NA                        1        0  13748. 
# 12 NA                        1        1   2577.


# ---------------------------------

# PAD --------------------------------


names(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2)

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8509737/
# https://www.dovepress.com/screening-of-peripheral-arterial-disease-in-primary-health-care-peer-reviewed-fulltext-article-VHRM

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>%
  filter(PAD_restrict==1) %>% select(patid, weight2, PAD_2, PAD_3, PAD_4) %>% 
  #filter(PAD_2==1|PAD_3==1|PAD_4==1) %>%
  mutate(PAD_2=ifelse(PAD_3==1|PAD_4==1,0,PAD_2)) %>%
    mutate(PAD_3=ifelse(PAD_4==1,0,PAD_3)) %>%
  left_join(DANU_Demographics) %>%
  group_by(PAD_2, PAD_3, PAD_4) %>% summarise(n=sum(weight2))

#    diagnosis          PAD_2 PAD_3 PAD_4        n
#  1 Diabetes               0     0     1   96997.
#  2 Diabetes               0     1     0   61299.
#  3 Diabetes               1     0     0     128.
#  4 Diabetes + Obesity     0     0     1  623961.
#  5 Diabetes + Obesity     0     1     0  329621.
#  6 Diabetes + Obesity     1     0     0     491.
#  7 Obesity                0     0     1  203027.
#  8 Obesity                0     1     0  386821.
#  9 Obesity                1     0     0     575.
# 10 NA                     0     0     1 6036090.
# 11 NA                     0     1     0 5646762.
# 12 NA                     1     0     0    9202.



# ---------------------------------

# OSLAP -----------------------------

OSLAP <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>%
  filter(OSLAP==1) %>% select(patid, weight2)


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- OSLAP %>% select(patid) %>% inner_join(DANU_Measures)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
#DANU_Measures <- DANU_Measures %>% ungroup() %>% filter(value>25 & value<80)

# 47% Mild, 25% Mod, 28% Severe

 DANU_Measures <- DANU_Measures %>% ungroup() %>% arrange(value) %>% left_join(OSLAP)  %>% #  summarise(n=sum(weight2)) # 2374569
   mutate(cum=cumsum(weight2)) %>%
   mutate(Group=ifelse(cum<0.47*2374569, "Mild", 
                       ifelse(cum<0.72*2374569, "Moderate", "Severe")))
 
 

 
 DANU_Measures %>%  ungroup() %>% left_join(DANU_Demographics) %>% 
     mutate(Group=ifelse(diagnosis=="Diabetes" | diagnosis=="-", "Mild", Group)) %>%
    # group_by(diagnosis, Group) %>% summarise(n=sum(weight2)) %>%
   ggplot(aes(value, colour=Group, fill=Group)) +
   geom_density(alpha=0.75) +
   theme_minimal() +
   xlab("\n BMI level") +  ylab("Patient density \n") +
   ggsci::scale_color_jama() +
   ggsci::scale_fill_jama() 
   

 
 DANU_Measures %>%  ungroup() %>% left_join(DANU_Demographics) %>% 
     mutate(Group=ifelse(diagnosis=="Diabetes" | diagnosis=="-", "Mild", Group)) %>%
    group_by(diagnosis, Group) %>% summarise(n=sum(weight2)) 

# ---------------------------------

# DANU Weights WEIGHTS ---------------------

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) # %>% filter(diagnosis != "-")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))


Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% filter(PCOS==1) %>%
  select(patid) %>%
  left_join(DANU_Demographics) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight, na.rm=T))

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% filter(PAD_restrict==1) %>%
  select(patid) %>%
  left_join(DANU_Demographics) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight, na.rm=T))


# ---------------------------------

# New target patients vactor - Comorbidity- Obesity adjusted -------------------
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 <- fread("Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2.txt")
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% select(ptid, weight2, PAD_restrict, PCOS, OSLAP, CKD, HFpEF)
names(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2)[1] <- "patid"


sum(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$weight2)

New_Comorbidity_Groups_Jun1 <- fread("New_Comorbidity_Groups_Jun1.txt")
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(patid, weight, diagnosis) %>% distinct()

New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% left_join(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2)

sum(New_Comorbidity_Groups_Jun1$weight[New_Comorbidity_Groups_Jun1$PAD_restrict==1 & New_Comorbidity_Groups_Jun1$diagnosis=="Diabetes + Obesity"], na.rm=T)


DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% select(patid, diagnosis)

DIA_ASCVD <- DIA_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

DIA_ASCVD$DIA_ASCVD <- "DIA_ASCVD"

New_Comorbidity_Groups_Jun1 %>% select(patid,weight,diagnosis) %>% distinct() %>% inner_join(DIA_ASCVD) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))

OBE2_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- OBE2_Comorbidity_Inventories %>% select(patid, diagnosis)


OBE_ASCVD <- OBE2_Comorbidity_Inventories %>% filter(grepl("G45", diagnosis)|
                                         grepl("I63", diagnosis)|
                                         grepl("H34", diagnosis)|
                                         grepl("I65", diagnosis)|
                                         grepl("I66", diagnosis)|
                                         grepl("I69", diagnosis)|
                                         grepl("I20", diagnosis)|
                                         grepl("I21", diagnosis)|
                                         grepl("I22", diagnosis)|
                                         grepl("I23", diagnosis)|
                                         grepl("I24", diagnosis)|
                                         grepl("I25", diagnosis)|
                                         grepl("I70", diagnosis)|
                                         grepl("I73", diagnosis)) %>% select(patid) %>% distinct()

OBE_ASCVD$OBE_ASCVD <- "OBE_ASCVD"


ASCVD <- OBE_ASCVD %>% select(patid) %>% bind_rows(DIA_ASCVD %>% select(patid)) %>% distinct() %>% mutate(ASCVD="ASCVD")

New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% left_join(ASCVD)
New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% select(-weight2)

New_Comorbidity_Groups_Jun1[is.na(New_Comorbidity_Groups_Jun1)] <- 0

New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(Comorb=ifelse(ASCVD=="ASCVD"|PAD_restrict==1|PCOS==1|OSLAP==1|CKD==1|HFpEF==1, 1,0))

New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(weight=ifelse(diagnosis=="Obesity"&Comorb==1, weight*1.515, weight))

sum(New_Comorbidity_Groups_Jun1$weight[New_Comorbidity_Groups_Jun1$diagnosis=="Obesity" & New_Comorbidity_Groups_Jun1$Comorb==1]) # 25628225

# 95506721 - 25628225 -> 69878496

sum(New_Comorbidity_Groups_Jun1$weight[New_Comorbidity_Groups_Jun1$diagnosis=="Obesity" & New_Comorbidity_Groups_Jun1$Comorb==0]) # 32234819

# 69878496/32234819

New_Comorbidity_Groups_Jun1 <- New_Comorbidity_Groups_Jun1 %>% mutate(weight=ifelse(diagnosis=="Obesity"&Comorb==0, weight*2.167796, weight))

New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

fwrite(New_Comorbidity_Groups_Jun1, "Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")


New_Comorbidity_Groups_Jun1 %>% group_by(diagnosis) %>% summarise(n=sum(weight))

New_Comorbidity_Groups_Jun1 %>% summarise(n=sum(weight))

sum(New_Comorbidity_Groups_Jun1$weight[New_Comorbidity_Groups_Jun1$Comorb==1])


sum(New_Comorbidity_Groups_Jun1$weight[New_Comorbidity_Groups_Jun1$ASCVD=="ASCVD" & 
                                       New_Comorbidity_Groups_Jun1$diagnosis=="Obesity" ])

sum(New_Comorbidity_Groups_Jun1$weight[New_Comorbidity_Groups_Jun1$Comorb==1 & 
                                         New_Comorbidity_Groups_Jun1$ASCVD=="ASCVD" & 
                                         New_Comorbidity_Groups_Jun1$OSLAP==1 & 
+                                          New_Comorbidity_Groups_Jun1$CKD==0 & New_Comorbidity_Groups_Jun1$PAD_restrict==0 &
+                                         New_Comorbidity_Groups_Jun1$PCOS==0 & New_Comorbidity_Groups_Jun1$HFpEF==0 ])


# -----------------