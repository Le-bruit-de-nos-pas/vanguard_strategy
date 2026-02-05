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


sum(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$weight2[Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$PCOS==1])



# ---------------------------------
# PCOS ----------------


# Check https://academic.oup.com/humrep/article/31/12/2841/2730240
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>%
  filter(PCOS==1) %>% select(patid, weight2, PCOS_E25, PCOS_N97, PCOS_N970) %>% 
  #filter(PCOS_N970==1|PCOS_E25==1) %>%
  left_join(DANU_Demographics) %>%
  group_by(PCOS_E25, PCOS_N970) %>% summarise(n=sum(weight2))




# NEW
Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 <- fread("Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2.txt")

length(unique(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$ptid)) # 3107856

sum(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$weight2[Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$PCOS==1])



PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx <- fread("PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx.txt")

PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>% select(PTID, weight2) %>% distinct() %>%
  summarise(n=sum(weight2))

length(unique(PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx$PTID)) # 2340531

PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>% select(DIAG, inclusion_codes, exclusion_codes,
                                                   criteria_1, criteria_2, criteria_3) %>% distinct() %>%
  arrange(inclusion_codes, exclusion_codes, criteria_1, criteria_2, criteria_3)

PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx <- PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>% select(-FST_DT) %>% distinct()

Criteria_1_pats <- PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>%
  filter(inclusion_codes==1&criteria_1==1) %>% select(PTID) %>% distinct() %>%
  anti_join(PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>%
  filter(exclusion_codes==1&criteria_1==1) %>% select(PTID)  %>% distinct()) # 187694

Criteria_2_pats <- PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>%
  filter(inclusion_codes==1&criteria_2==1) %>% select(PTID) %>% distinct() %>%
  anti_join(PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>%
  filter(exclusion_codes==1&criteria_2==1) %>% select(PTID)  %>% distinct()) # 1070587

Criteria_3_pats <- PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>%
  filter(inclusion_codes==1&criteria_3==1) %>% select(PTID) %>% distinct() %>%
  anti_join(PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>%
  filter(exclusion_codes==1&criteria_3==1) %>% select(PTID)  %>% distinct()) # 1406655

Included <- Criteria_1_pats %>% inner_join(Criteria_2_pats) %>%
  bind_rows(Criteria_2_pats %>% inner_join(Criteria_3_pats)) %>%
  bind_rows(Criteria_1_pats %>% inner_join(Criteria_3_pats)) %>% distinct()

Included <- Included %>% 
  left_join(Criteria_1_pats %>% mutate(Criteria_1=1)) %>%
  left_join(Criteria_2_pats %>% mutate(Criteria_2=1)) %>%
  left_join(Criteria_3_pats %>% mutate(Criteria_3=1)) 

Included %>% group_by(Criteria_1, Criteria_2, Criteria_3) %>% count()

Included %>% filter(Criteria_1==1) %>% count()  #135066
Included %>% filter(Criteria_2==1) %>% count()  #278745
Included %>% filter(Criteria_3==1) %>% count()  #333988



DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt", integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) # %>% filter(diagnosis != "-")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))


Included <- Included %>% left_join(PCOS_ce18_pts_dxed_with_ICD10_Exhaustive_5yDx %>% select(PTID, weight2) %>% distinct())

mean(Included$weight2)

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% filter(PCOS ==1) %>% select(ptid) %>% distinct() %>% 
  inner_join(Included, by=c("ptid"="PTID")) %>% select(ptid , weight2) %>% distinct() %>% summarise(n=sum(weight2))


Included %>% select(PTID, weight2) %>% distinct() %>% summarise(n=sum(weight2))

Included %>% left_join(DANU_Demographics, by=c("PTID"="patid")) %>%
  group_by(diagnosis, Criteria_1, Criteria_2, Criteria_3) %>% summarise(n=sum(weight))

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% filter(PCOS ==1) %>% select(ptid) %>% distinct() %>%
  inner_join(Included, by=c("ptid"="PTID")) %>% left_join(DANU_Demographics, by=c("ptid"="patid")) %>%
    mutate(diagnosis=ifelse(is.na(diagnosis), "-", diagnosis)) %>%
  group_by(diagnosis, Criteria_1, Criteria_2, Criteria_3) %>% summarise(n=sum(weight2))





# ---------------------------------

# PAD --------------------------------


names(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2)

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>%
  filter(PAD_restrict==1) %>% select(patid, weight2, PAD_2, PAD_3, PAD_4) %>% 
  #filter(PAD_2==1|PAD_3==1|PAD_4==1) %>%
  mutate(PAD_2=ifelse(PAD_3==1|PAD_4==1,0,PAD_2)) %>%
    mutate(PAD_3=ifelse(PAD_4==1,0,PAD_3)) %>%
  left_join(DANU_Demographics) %>%
  group_by(PAD_2, PAD_3, PAD_4) %>% summarise(n=sum(weight2))


#NEW

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 <- fread("Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2.txt")

length(unique(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$ptid)) # 3107856
sum(Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2$weight2)



PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx <- fread("PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx.txt")

length(unique(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx$PTID)) # 1079671

PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% select(PTID, weight2) %>% distinct() %>% summarise(n=sum(weight2)) # 21763068
PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx <- PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% select(-c(DIAG, I7300, I7301, I739)) 
PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx <- gather(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx, Code, Exp, I7020x:I7079x, factor_key=TRUE)
PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx <- PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% distinct()
PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx  %>% select(PTID, weight2) %>% distinct() %>% summarise(n=sum(weight2)) # 21763068
PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Exp==1) %>% select(PTID, weight2) %>% distinct() %>% summarise(n=sum(weight2)) # 15244268 # 6028681


length(unique(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx$Code))

PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx <- PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Exp==1)

PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx <- PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% mutate(Stage=ifelse(grepl("0x", Code)|grepl("9x", Code), "I",
                                                                          ifelse(grepl("1x", Code), "II",
                                                                                 ifelse(grepl("2x", Code), "III",
                                                                                        ifelse(Code=="I739"|Code=="I7300", "I", "IV" )))))


PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% group_by(Code) %>% summarise(n=sum(weight2)) %>% arrange(-n)

PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="IV") %>% select(PTID, weight2) %>% distinct() %>% summarise(n=sum(weight2)) # 742481.2 # 703802.6

PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="III") %>% select(PTID, weight2) %>% distinct() %>%
  anti_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="IV") %>% select(PTID, weight2) %>% distinct()) %>%
  summarise(n=sum(weight2)) # 706987.2 # 708026.9
 
PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="II") %>% select(PTID, weight2) %>% distinct() %>%
  anti_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="IV") %>% select(PTID, weight2) %>% distinct()) %>%
    anti_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="III") %>% select(PTID, weight2) %>% distinct()) %>%
  summarise(n=sum(weight2)) # 2516199 # 2518170
 
PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="I") %>% select(PTID, weight2) %>% distinct() %>%
  anti_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="IV") %>% select(PTID, weight2) %>% distinct()) %>%
    anti_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="III") %>% select(PTID, weight2) %>% distinct()) %>%
      anti_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx %>% filter(Stage=="II") %>% select(PTID, weight2) %>% distinct()) %>%
  summarise(n=sum(weight2)) # 11278601 #2098681
 


Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% filter(PAD_restrict==1) %>% select(ptid) %>%
  inner_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx, by=c("ptid"="PTID")) %>%
  select(ptid, weight2) %>% distinct() %>% summarise(n=sum(weight2)) # 6028643 


Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% filter(PAD_restrict==1) %>% select(ptid) %>%
  inner_join(PAD_ce18_pts_dxs_with_ICD10_Exhaustive_5ydx, by=c("ptid"="PTID")) %>%
  select(ptid, weight2, Stage) %>% distinct() %>%
  left_join(DANU_Demographics, by=c("ptid"="patid")) %>%
  mutate(diagnosis=ifelse(is.na(diagnosis), "-", diagnosis)) %>%
    group_by(diagnosis, Stage) %>%
  summarise(n=sum(weight2)) %>%
  spread(key=diagnosis, value=n)


# ---------------------------------

# OSLAP -----------------------------

OSLAP <- Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>%
  filter(OSLAP==1) %>% select(patid, weight2)


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures <- OSLAP %>% select(patid) %>% inner_join(DANU_Measures)
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(patid, value) %>% distinct() %>% group_by(patid ) %>% filter(value==max(value)) %>% slice(1)
#DANU_Measures <- DANU_Measures %>% ungroup() %>% filter(value>25 & value<80)

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
  select(ptid) %>%
  left_join(DANU_Demographics, by=c("ptid"="patid")) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight, na.rm=T))

Mkt_Clarity_PAD_PCOS_SLAP_CKD_HFpEF_ce18_pts_5yDx_v2 %>% filter(PAD_restrict==1) %>%
  select(patid) %>%
  left_join(DANU_Demographics) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight, na.rm=T))


# ---------------------------------
