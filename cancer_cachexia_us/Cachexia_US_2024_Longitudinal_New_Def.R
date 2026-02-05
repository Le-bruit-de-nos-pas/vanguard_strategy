

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
options(scipen = 999)


# Create NEW Pre-cachexia, Cachexia, Recovered Groups dynamically 5 years  -----------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)


length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 


temp %>% filter(Recovered==1) %>% select(patid) %>% distinct()


data.frame(temp %>% filter(patid=="PT078228913") %>%
  select(Month_Min, Min, CCh, CUM_CCh, Pre_CCh, Recovered, Delta) %>% distinct() %>%
  group_by(Month_Min) %>%
  mutate(CCh=max(CCh), CUM_CCh=max(CUM_CCh), Pre_CCh=max(Pre_CCh), Recovered=max(Recovered), Delta=max(Delta)) %>%
  ungroup()) %>%
  left_join(
    temp %>% filter(patid=="PT078228913") %>% select(Month_Max,Max) %>% distinct(), by=c("Month_Min"="Month_Max") 
  ) %>%
  mutate(Group=ifelse(CCh==1&Recovered==0,paste0("CCh_", Delta), 
                      ifelse(CUM_CCh==1&Recovered==1, "Recovered",
                        ifelse(Pre_CCh==1&CUM_CCh==0, "Pre_CCh", 
                             ifelse(Pre_CCh==1&CUM_CCh==1, paste0("CCh_", Delta),
                             ifelse(Recovered==1, "Recovered", 
                                    ifelse(CUM_CCh==1, "Post_cachexia", "Never_cachexia"))))))) %>%
    mutate(Group=ifelse(Group=="CCh_-1", "Cachexia_Decreasing",
                      ifelse(Group=="CCh_0", "Cachexia_Stable",
                             ifelse(Group=="CCh_1", "Cachexia_Increasing", 
                                    ifelse(Group=="Pre_CCh", "Pre-cachexia", Group))))) %>%
    select(Month_Min, Min, Group, Max) %>% distinct() %>%
  ggplot(aes(Month_Min, fill=Group, colour=Group)) +
  geom_segment(aes(x = Month_Min, xend = Month_Min, y = Min, yend = Max), size = 4.0, alpha=0.9) +
 # geom_point(aes(x = Month_Min, y = Min), size = 3) +
  scale_fill_manual(values = c( "#b4bfeb", "#586dbb", "#0e226b",  "#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315")) +
  scale_colour_manual(values = c( "#b4bfeb", "#586dbb", "#0e226b", "#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315")) +
  theme_minimal() +
  #ylim(24,31) +
  xlab("\n Exact Observed BMI Month") + ylab("Minimum BMI Observed \n ON that Month \n")

  
 
#  "#b4bfeb",





# PreCCh if already CCh -> CCh
# CC if None/PreCCh -> CCh Inc/Dec/Maintain


summary_figs <- data.frame(temp %>% group_by(patid) %>%
  select(patid, Month_Min, Min, CCh, CUM_CCh, Pre_CCh, Recovered, Delta) %>% distinct() %>%
  group_by(patid, Month_Min) %>%
  mutate(CCh=max(CCh), CUM_CCh=max(CUM_CCh), Pre_CCh=max(Pre_CCh), Recovered=max(Recovered), Delta=max(Delta)) %>%
  ungroup()) %>% group_by(patid) %>%
  mutate(Group=ifelse(CCh==1&Recovered==0,paste0("CCh_", Delta), 
                      ifelse(CUM_CCh==1&Recovered==1, "Recovered",
                        ifelse(Pre_CCh==1&CUM_CCh==0, "Pre_CCh", 
                             ifelse(Pre_CCh==1&CUM_CCh==1, paste0("CCh_", Delta),
                             ifelse(Recovered==1, "Recovered", 
                                    ifelse(CUM_CCh==1, "Post_cachexia", "Never_cachexia"))))))) %>%
    select(patid, Month_Min, Min, Group) %>% distinct() %>% ungroup() %>%
  group_by(Month_Min, Group) %>% count()
  



summary_figs %>% ungroup() %>% group_by(Month_Min) %>% mutate(tot=sum(n)) %>% ungroup() %>%
  mutate(perc=n/tot) %>%
  mutate(Group=ifelse(Group=="CCh_-1", "Cachexia_Decreasing",
                      ifelse(Group=="CCh_0", "Cachexia_Stable",
                             ifelse(Group=="CCh_1", "Cachexia_Increasing", 
                                    ifelse(Group=="Pre_CCh", "Pre-cachexia", Group))))) %>%
  ggplot(aes(Month_Min, 100*perc, colour=Group, fill=Group)) +
  geom_smooth(size=2, alpha=0.3, se=F) +
  theme_minimal() +
  xlab("\n Exact Month") + ylab("Patient Proportion (%) \n") +
  scale_colour_manual(values = c("#b4bfeb","#586dbb", "#0e226b","#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315")) +
  scale_fill_manual(values = c("#b4bfeb","#586dbb", "#0e226b", "#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315"))



# Time on each group
# Distribution of BMI
# Per cancer type


Monthly_groups <- data.frame(temp %>% group_by(patid) %>%
  select(patid, Month_Min, Min, CCh, CUM_CCh, Pre_CCh, Recovered, Delta) %>% distinct() %>%
  group_by(patid, Month_Min) %>%
  mutate(CCh=max(CCh), CUM_CCh=max(CUM_CCh), Pre_CCh=max(Pre_CCh), Recovered=max(Recovered), Delta=max(Delta)) %>%
  ungroup()) %>% group_by(patid) %>%
  mutate(Group=ifelse(CCh==1&Recovered==0,paste0("CCh_", Delta), 
                      ifelse(CUM_CCh==1&Recovered==1, "Recovered",
                        ifelse(Pre_CCh==1&CUM_CCh==0, "Pre_CCh", 
                             ifelse(Pre_CCh==1&CUM_CCh==1, paste0("CCh_", Delta),
                             ifelse(Recovered==1, "Recovered", 
                                    ifelse(CUM_CCh==1, "Post_cachexia", "Never_cachexia"))))))) %>%
    select(patid, Month_Min, Min, Group) %>% distinct() %>% ungroup() 


Monthly_groups <- Monthly_groups %>% mutate(Group=ifelse(Group=="Recovered", "Post_cachexia", Group))




data.frame(Monthly_groups %>% group_by(patid) %>% 
  mutate(grp = rle(Group)$lengths %>% {rep(seq(length(.)), .)})) %>%
  select(patid, grp) %>% distinct() %>%
  group_by(patid) %>% count() %>% ungroup() %>% #  summarise(n=mean(n))
  ggplot(aes(n)) +
  geom_density(colour="black", size=2, alpha=0.5) +
  theme_minimal() +
  xlab("\n Number of different sequential stages") +
  ylab("Patient density \n")

Monthly_groups %>% filter(Month_Min>=48) %>% group_by(patid, Group) %>% count() %>%
  ungroup() %>% group_by(Group) %>% summarise(mean=mean(n))



Monthly_groups %>% group_by(Group) %>% summarise(mean=mean(Min))

Monthly_groups %>% group_by(patid, Group) %>% count() %>% ungroup() %>%
   mutate(Tot=sum(n)) %>% ungroup() %>%
  group_by(Group) %>% mutate(Group_Tot=sum(n)) %>% 
  select(Group, Tot, Group_Tot) %>% distinct() %>%
  mutate(perc=Group_Tot/Tot) %>% mutate(months=perc*60)



data.frame(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>%
  inner_join(Monthly_groups) %>%
  group_by(patid, Primary_Cancer, Group) %>% count() %>% ungroup() %>%
  group_by(Primary_Cancer) %>%
   mutate(Tot=sum(n)) %>% ungroup() %>%
  group_by(Primary_Cancer, Group) %>% mutate(Group_Tot=sum(n)) %>% 
  select(Primary_Cancer, Group, Tot, Group_Tot) %>% distinct() %>%
  mutate(perc=Group_Tot/Tot) %>% ungroup() %>%
  select(Primary_Cancer, Group, perc) %>% distinct() %>%
  spread(key=Group, value=perc)) %>% arrange(CCh)
  


# -----------------


# Metastatic Dynamic version ---------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(metastasis = ifelse(is.na(metastasis_onset), 0, 1))

New_Primary_Cancer_Box %>% group_by(metastasis) %>% summarise(n=sum(weight))

length(unique(New_Primary_Cancer_Box$metastasis_onset))



New_Primary_Cancer_Box %>% group_by(metastasis_onset) %>% summarise(n=sum(weight)) %>%
  drop_na() %>% mutate(Metastatic=cumsum(n)) %>% select(-n) %>%
  mutate(Non_metastatic=13958669) %>%
  gather(Group, pop, Metastatic:Non_metastatic) %>%
  ggplot(aes(metastasis_onset, pop, colour=Group, fill=Group)) +
  geom_line(size=2) +
  theme_minimal() +
  xlab("\n Exact Follow-up Month") + ylab("Population \n") +
  scale_colour_manual(values=c( "#C86060", "#66A4B9")) +
  scale_fill_manual(values=c( "#C86060", "#66A4B9")) 



New_Primary_Cancer_Box %>% group_by(metastasis_onset) %>% summarise(n=sum(weight)) %>%
  drop_na() %>% mutate(Metastatic=cumsum(n)) %>% select(-n) %>%
  mutate(Non_metastatic=13958669) %>%
  mutate(Non_metastatic_T=22984889-Metastatic) %>% select(-Non_metastatic) %>%
  gather(Group, pop, Metastatic:Non_metastatic_T) %>%
  ggplot(aes(metastasis_onset, pop, colour=Group, fill=Group)) +
  geom_line(size=2) +
  theme_minimal() +
  xlab("\n Exact Follow-up Month") + ylab("Population \n") +
  scale_colour_manual(values=c( "#C86060", "#66A4B9")) +
  scale_fill_manual(values=c( "#C86060", "#66A4B9")) 



CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, Month)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)




CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, weight, metastasis_onset, metastasis) %>% rename("patient"="patid")) 

length(unique(CAN_Drug_Histories$patient))*60

CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month) %>% group_by(patient)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(already_mets=ifelse(Month>=metastasis_onset,1,0))

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(already_mets=ifelse(is.na(already_mets),0,already_mets))



PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% distinct() %>% ungroup() %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(patient, claimed, value)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed))
PONS_Measures <- PONS_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))

PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% rename("BMIrecord"="Exact_Month") %>% select(-claimed)
PONS_Measures <- PONS_Measures %>% distinct()
# PONS_Measures <- PONS_Measures %>% select(-test)

PONS_Measures <- CAN_Drug_Histories %>% select(patient, Month, already_mets) %>%
  inner_join(PONS_Measures %>% select(patient, value, BMIrecord), by=c("patient"="patient", "Month"="BMIrecord"))

PONS_Measures <- PONS_Measures %>% group_by(patient, Month, already_mets) %>% summarise(value=mean(value)) %>% ungroup()


PONS_Measures %>% select(patient) %>% distinct() %>% ungroup() %>% sample_n(40000) %>%
  left_join(PONS_Measures) %>%
  ggplot(aes(Month, value, colour=as.factor(already_mets) , fill=as.factor(already_mets) )) +
  geom_smooth(size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3"))
  


PONS_Measures %>% filter(already_mets==1) %>% select(patient) %>% distinct() %>% ungroup() %>% sample_n(50000) %>%
  left_join(PONS_Measures) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>% rename("patient"="patid")) %>%
  ggplot(aes(Month, value, colour=as.factor(already_mets) , fill=as.factor(already_mets) )) +
  geom_smooth(size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  facet_wrap(~Primary_Cancer, ncol=6)




PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_onset)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics <- PONS_Demographics %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")




PONS_Measures %>% filter(already_mets==1) %>% select(patient) %>% distinct() %>% ungroup() %>% # sample_n(50000) %>%
  left_join(PONS_Measures) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>% rename("patient"="patid")) %>%
  inner_join(PONS_Demographics, by=c("patient"="patid")) %>%
  mutate(elapsed=Month-cancer_onset) %>%
  ggplot(aes(elapsed, value, colour=as.factor(already_mets) , fill=as.factor(already_mets) )) +
  geom_smooth(size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  facet_wrap(~Primary_Cancer, ncol=6)



Mets_exp <- PONS_Measures %>% filter(already_mets==1) %>% select(patient) %>%
  distinct() %>% mutate(mets_exp=1)


PONS_Measures%>% ungroup()  %>% 
  inner_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>% rename("patient"="patid")) %>%
  inner_join(PONS_Demographics, by=c("patient"="patid")) %>%
  mutate(elapsed=Month-cancer_onset) %>%
  left_join(Mets_exp) %>% mutate(mets_exp=ifelse(is.na(mets_exp),0,mets_exp)) %>% 
  mutate(already_mets=ifelse(mets_exp==1&already_mets==0,"1pre", already_mets)) %>%
  ggplot(aes(elapsed, value, colour=as.factor(already_mets) , fill=as.factor(already_mets) )) +
  geom_smooth(size=1, alpha=0.7) +
  ylim(25,40) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3", "#dacd54")) +
  scale_fill_manual(values = c("steelblue4", "tomato3", "#dacd54")) +
  facet_wrap(~Primary_Cancer, ncol=6)




PONS_Measures %>%
  inner_join(New_Primary_Cancer_Box %>%  rename("patient"="patid")) %>%
  filter(!is.na(metastasis_onset)) %>%
  mutate(elapsed=Month-metastasis_onset ) %>%
  filter(Primary_Cancer %in% c("Breast Cancer", "Prostate Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")) %>%
  ggplot(aes(elapsed, value, colour=as.factor(already_mets) , fill=as.factor(already_mets) )) +
  geom_smooth(size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  facet_wrap(~Primary_Cancer, ncol=3)





# ----------------
  

# # 8 Survival % Still Alive Counting from Metastasis Onset ------------
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)
PONS_Demographics_temp <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_metastasis, death_date)
PONS_Demographics_temp$cancer_metastasis <- as.Date(PONS_Demographics_temp$cancer_metastasis)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(!is.na(cancer_metastasis))
missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_metastasis)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

New_Primary_Cancer_Box <- PONS_Demographics_temp %>% inner_join(New_Primary_Cancer_Box)

data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>%  summarise(weighted.mean(Survived, weight)))


temp <- data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer, Survived) %>%
             summarise(n=sum(weight)) %>% spread(key=Primary_Cancer, value=n) )

fwrite(temp, "temp.csv")

# -----------
# Survival Curves From Cancer Onset Using Data From paper US (change folders!!) -----------


PONS_Demographics <- fread("PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_onset)) %>% filter(cancer_onset<="2020-07-31")


PONS_Demographics <- PONS_Demographics %>% filter(age>=18) %>% 
  select(patid, weight, age, died, cancer_metastasis, cachexia_onset) %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics) %>% rename("diagnosis"="Primary_Cancer")


Pats_to_track_BMI <- PONS_Demographics  %>% 
  select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset) %>% 
  filter(diagnosis!="-" & diagnosis != "Unspecified Cancer") 


# BMI records

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")

PONS_Measures <- PONS_Measures %>% filter(test=="BMI") %>% select(-weight) %>%
  inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats) %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), 
                                                                                   median=median(value), 
                                                                                   min=min(value), 
                                                                                   max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(
    PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate), by="patid"
    ) %>% select(patid, mindate, maxdate) %>% distinct()


PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate) ; PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

# Only patients with +10 BMI records

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% 
  inner_join(PONS_Measures) %>% select(-weight)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset) %>% 
  mutate(cachexia_onset=as.Date(cachexia_onset))

Pats_to_track_BMI <- fread("Pats_to_track_BMI.txt")

At_risk_population <- fread("At_risk_population.txt")

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()


CachexiaPats_ALL_NEW <- data.frame(
  New_Cachexia_Pred %>% left_join(
    Pats_to_track_BMI
    ) %>% inner_join(
      PONS_Measures %>% select(patid) %>% distinct()
      ) %>%
    bind_rows(
      Pats_to_track_BMI %>% filter(!is.na(cachexia_onset))
      ) %>%
             distinct()
  )



Cachexia_Dx <- CachexiaPats_ALL_NEW %>% filter(!is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Dx")

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct()%>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")


PONS_Demographics <- fread("PONS Demographics.txt")
 
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, died, cancer_metastasis, cancer_onset, death_date)  %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics$cancer_onset <- as.Date(PONS_Demographics$cancer_onset)

PONS_Demographics$death_date  <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- PONS_Demographics %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))

Cachexia_Dx %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1081.006
Cachexia_Pred %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1604.114
No_Cachexia %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1724.091


# Dx
data.frame(Cachexia_Dx %>% inner_join(PONS_Demographics) %>% group_by(Survived) %>% count())

# Pred
data.frame(Cachexia_Pred %>% inner_join(PONS_Demographics) %>% group_by(Survived) %>% count())

# None
data.frame(No_Cachexia %>% inner_join(PONS_Demographics) %>% group_by(Survived) %>% count())

temp <- Cachexia_Dx %>% select(patid, group) %>%
  bind_rows(Cachexia_Pred %>% select(patid, group) ) %>%
  bind_rows(No_Cachexia %>% select(patid, group)) %>%
  inner_join(PONS_Demographics %>% select(patid, died, Survived)) %>% 
  mutate(status=ifelse(died=="Y",1,0)) %>% select(-died)

Surv(temp$Survived, temp$status)[1:10]
  

s1 <- survfit(Surv(Survived, status) ~ 1, data = temp)

str(s1)


survfit2(Surv(Survived, status) ~ 1, data = temp) %>% 
  ggsurvfit() +
  labs(x = "Months",y = "Overall survival probability") + 
  ylim(0,1) +
  theme_minimal() +
  add_confidence_interval() +
  add_risktable()

survfit2(Surv(Survived, status) ~ group, data = temp) %>% 
  ggsurvfit() +
  labs(x = "Months",y = "Overall survival probability") + 
  ylim(0,1) +
  theme_minimal() +
  add_confidence_interval() +
  add_risktable()


summary(survfit(Surv(Survived, status) ~ group, data = temp), times = 60)


temp$group <- factor(temp$group, levels = c("None","Pred","Dx"))


sfit <- survfit(Surv(Survived, status)~group, data=temp)
sfit
summary(sfit)

ggsurvplot(sfit)

image <- ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("None", "Pred", "Dx"), legend.title="Cachexia Status",  
           palette=c("honeydew4", "midnightblue", "firebrick"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia Status", 
           risk.table.height=0.2)

image <- image$plot

image <- image$table

ggsave(file="fig_km_a_table.svg", plot=image, width=8, height=2.5)



 
fit <- coxph(Surv(Survived, status)~group, data=temp)

fit
 
# Call:
# coxph(formula = Surv(Survived, status) ~ group, data = temp)
# 
#               coef exp(coef) se(coef)      z                   p
# groupPred  0.85548   2.35251  0.01182  72.41 <0.0000000000000002
# groupDx    2.37207  10.71958  0.01472 161.17 <0.0000000000000002
# 
# Likelihood ratio test=22557  on 2 df, p=< 0.00000000000000022
# n= 286816, number of events= 52955 


temp <- temp %>% left_join(PONS_Demographics %>% select(patid, cancer_metastasis ))

temp <- temp %>% mutate(group=ifelse(group=="Dx" & cancer_metastasis==1, "Dx Metastatic",
                             ifelse(group=="Dx" & cancer_metastasis==0, "Dx no mets", 
                                    ifelse(group=="Pred" & cancer_metastasis==0, "Pred no mets", 
                                           ifelse(group=="Pred" & cancer_metastasis==1, "Pred  Metastatic", 
                                                  ifelse(group=="None" & cancer_metastasis==0, "None no mets", 
                                                         ifelse(group=="None" & cancer_metastasis==1, "None Metastatic", NA))))))) 

temp$group <- factor(temp$group, levels = c("None no mets","None Metastatic","Pred no mets", "Pred  Metastatic", "Dx no mets", "Dx Metastatic"))


temp <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>%
  inner_join(temp)



temp_2 <- temp[temp$Primary_Cancer=="Pancreatic Cancer"]

sfit <- survfit(Surv(Survived, status)~group, data=temp_2)
sfit

unique(temp_2$group)

summary(sfit)


ggsurvplot(sfit, conf.int=TRUE, pval=FALSE, risk.table=FALSE, 
           #legend.labs=c("Dx Metastatic", "Dx no mets",  "None Metastatic", "None no mets", "Pred Metastatic", "Pred no mets"), legend.title="Cachexia Status",  
           palette=c("#929baa", "#515967", "#17b7ba", "#1753ba", "coral2", "firebrick"), 
           title="PANCREATIC \n Kaplan-Meier Curve for Cancer Survival \n Cachexia & Metastasis Status", 
           risk.table.height=0.3)




fit <- coxph(Surv(Survived, status)~group, data=temp)

fit


# Call:
# coxph(formula = Surv(Survived, status) ~ group, data = temp)
# 
#                           coef exp(coef) se(coef)      z                   p
# groupNone Metastatic   1.13845   3.12193  0.02164  52.60 <0.0000000000000002
# groupPred no mets      0.84271   2.32266  0.01946  43.31 <0.0000000000000002
# groupPred  Metastatic  1.85967   6.42161  0.01826 101.83 <0.0000000000000002
# groupDx no mets        2.70454  14.94750  0.02693 100.42 <0.0000000000000002
# groupDx Metastatic     3.00222  20.13012  0.02073 144.79 <0.0000000000000002
# 
# Likelihood ratio test=34049  on 5 df, p=< 0.00000000000000022
# n= 286816, number of events= 52955 








# -------
# Survival Curves From Metastasis Using Data From paper US (change folders!!) -----------


PONS_Demographics <- fread("PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_onset)) %>% filter(cancer_onset<="2020-07-31")


PONS_Demographics <- PONS_Demographics %>% filter(age>=18) %>% 
  select(patid, weight, age, died, cancer_metastasis, cachexia_onset) %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics) %>% rename("diagnosis"="Primary_Cancer")


Pats_to_track_BMI <- PONS_Demographics  %>% 
  select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset) %>% 
  filter(diagnosis!="-" & diagnosis != "Unspecified Cancer") 


# BMI records

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")

PONS_Measures <- PONS_Measures %>% filter(test=="BMI") %>% select(-weight) %>%
  inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats) %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), 
                                                                                   median=median(value), 
                                                                                   min=min(value), 
                                                                                   max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(
    PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate), by="patid"
    ) %>% select(patid, mindate, maxdate) %>% distinct()


PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate) ; PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

# Only patients with +10 BMI records

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% 
  inner_join(PONS_Measures) %>% select(-weight)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset) %>% 
  mutate(cachexia_onset=as.Date(cachexia_onset))

Pats_to_track_BMI <- fread("Pats_to_track_BMI.txt")

At_risk_population <- fread("At_risk_population.txt")

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()


CachexiaPats_ALL_NEW <- data.frame(
  New_Cachexia_Pred %>% left_join(
    Pats_to_track_BMI
    ) %>% inner_join(
      PONS_Measures %>% select(patid) %>% distinct()
      ) %>%
    bind_rows(
      Pats_to_track_BMI %>% filter(!is.na(cachexia_onset))
      ) %>%
             distinct()
  )



Cachexia_Dx <- CachexiaPats_ALL_NEW %>% filter(!is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Dx")

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct()%>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")


PONS_Demographics <- fread("PONS Demographics.txt")
 
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, died, cancer_metastasis, death_date)  %>% 
  filter(!is.na(cancer_metastasis))

PONS_Demographics$cancer_metastasis <- as.Date(PONS_Demographics$cancer_metastasis)

PONS_Demographics$death_date  <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- PONS_Demographics %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_metastasis)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))


Cachexia_Dx <- Cachexia_Dx %>% rename("metastasis"="cancer_metastasis")
Cachexia_Pred <- Cachexia_Pred %>% rename("metastasis"="cancer_metastasis")
No_Cachexia <- No_Cachexia %>% rename("metastasis"="cancer_metastasis")

Cachexia_Dx %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1081.006
Cachexia_Pred %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1604.114
No_Cachexia %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1724.091


# Dx
data.frame(Cachexia_Dx %>% inner_join(PONS_Demographics) %>% group_by(Survived) %>% count())

# Pred
data.frame(Cachexia_Pred %>% inner_join(PONS_Demographics) %>% group_by(Survived) %>% count())

# None
data.frame(No_Cachexia %>% inner_join(PONS_Demographics) %>% group_by(Survived) %>% count())

temp <- Cachexia_Dx %>% select(patid, group) %>%
  bind_rows(Cachexia_Pred %>% select(patid, group) ) %>%
  bind_rows(No_Cachexia %>% select(patid, group)) %>%
  inner_join(PONS_Demographics %>% select(patid, died, Survived)) %>% 
  mutate(status=ifelse(died=="Y",1,0)) %>% select(-died)



s1 <- survfit(Surv(Survived, status) ~ 1, data = temp)

str(s1)


survfit2(Surv(Survived, status) ~ 1, data = temp) %>% 
  ggsurvfit() +
  labs(x = "Months",y = "Overall survival probability") + 
  ylim(0,1) +
  theme_minimal() +
  add_confidence_interval() +
  add_risktable()

survfit2(Surv(Survived, status) ~ group, data = temp) %>% 
  ggsurvfit() +
  labs(x = "Months",y = "Overall survival probability") + 
  ylim(0,1) +
  theme_minimal() +
  add_confidence_interval() +
  add_risktable()


summary(survfit(Surv(Survived, status) ~ group, data = temp), times = 60)


temp$group <- factor(temp$group, levels = c("None","Pred","Dx"))


sfit <- survfit(Surv(Survived, status)~group, data=temp)
sfit
summary(sfit)

ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("None", "Pred", "Dx"), legend.title="Cachexia Status",  
           palette=c("honeydew4", "midnightblue", "firebrick"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia Status", 
           risk.table.height=0.2)




 
fit <- coxph(Surv(Survived, status)~group, data=temp)

fit
 
# coxph(formula = Surv(Survived, status) ~ group, data = temp)
# 
#              coef exp(coef) se(coef)      z                   p
# groupPred 0.72195   2.05845  0.01490  48.47 <0.0000000000000002
# groupDx   1.86121   6.43154  0.01785 104.26 <0.0000000000000002
# 
# Likelihood ratio test=10344  on 2 df, p=< 0.00000000000000022
# n= 123273, number of events= 35912 


temp <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>%
  inner_join(temp)


temp_2 <- temp[temp$Primary_Cancer=="Breast Cancer"]

sfit <- survfit(Surv(Survived, status)~group, data=temp_2)
sfit

unique(temp_2$group)

summary(sfit)


ggsurvplot(sfit, conf.int=TRUE, pval=FALSE, risk.table=FALSE, 
           #legend.labs=c("Dx Metastatic", "Dx no mets",  "None Metastatic", "None no mets", "Pred Metastatic", "Pred no mets"), legend.title="Cachexia Status",  
           palette=c("honeydew4", "midnightblue", "firebrick"),
           title="Breast \n Kaplan-Meier Curve for Cancer Survival \n Cachexia Status", 
           risk.table.height=0.3)




fit <- coxph(Surv(Survived, status)~group, data=temp)

fit

# -------
# BIM drop vs survival TOP 5 BY Stage and ECOG -------------------------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer %in% c("Lung Cancer", "Intestinal Cancer", "Prostate Cancer", "Breast Cancer", "Pancreatic Cancer"))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)

# Death/Survival
PONS_Demographics_temp <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_onset, death_date)

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date)

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

New_Primary_Cancer_Box <- PONS_Demographics_temp %>% inner_join(New_Primary_Cancer_Box)


Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% mutate(Exact_Month=ifelse(is.na(Exact_Month),60,Exact_Month))


temp_max <- fread("Diagnosed Population 1.0/MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("Diagnosed Population 1.0/MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)

temp <- temp %>% ungroup() %>% filter(Month_Min>Month_Max)

temp <- temp %>% mutate(Diff=(Min-Max)/Max) %>% group_by(patid) %>% filter(Diff==min(Diff)) %>% slice(1) %>% ungroup()


PONS_Demographics_temp <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, cancer_metastasis) %>%
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis), "Non_mets", "Metastatic"))



temp %>% select(patid, Month_Min, Diff) %>% 
  inner_join(New_Primary_Cancer_Box ) %>%
  inner_join(PONS_Demographics_temp) %>%
  mutate(DropToDeath=Exact_Month-Month_Min) %>%
  filter(Exact_Month != 60) %>%
  filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>%
   ggplot(aes(100*abs(Diff), DropToDeath, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth()+
    scale_fill_manual(values = c("firebrick", "deepskyblue4")) +
  scale_colour_manual(values = c("firebrick", "deepskyblue4")) +
  theme_minimal() +
  xlab("\n MAX Abs. BMI Drop") + ylab("No. Months Survived \n") +
  facet_wrap(~Primary_Cancer)


temp %>% inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_temp) %>%
   filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>%
  ggplot(aes(100*abs(Diff), Survived, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth()+
  scale_fill_manual(values = c("firebrick", "deepskyblue4")) +
  scale_colour_manual(values = c("firebrick", "deepskyblue4")) +
  theme_minimal() +
  xlab("\n MAX Abs. BMI Drop") + ylab("No. Months Survived \n") +
  facet_wrap(~Primary_Cancer)


ECOG_NLPMeas_All_Records <- fread("Diagnosed Population 1.0/ECOG_NLPMeas_All_Records.txt")
unique(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records$measurement_value <- parse_number(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% filter(measurement_value>0&measurement_value<=5)
ECOG_NLPMeas_All_Records$measurement_value <- round(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% group_by(patid) %>% summarise(ECOG=max(measurement_value))

ECOG_NLPMeas_All_Records %>% inner_join(New_Primary_Cancer_Box)

temp %>% inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_temp) %>%
    inner_join(ECOG_NLPMeas_All_Records %>% mutate(ECOG=as.factor(ECOG))) %>%
   filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>%
  ggplot(aes(100*abs(Diff), Survived, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth()+
  scale_fill_manual(values = c("firebrick", "deepskyblue4")) +
  scale_colour_manual(values = c("firebrick", "deepskyblue4")) +
  theme_minimal() +
  xlab("\n MAX Abs. BMI Drop") + ylab("No. Months Survived \n") +
  facet_wrap(~Primary_Cancer) +
  facet_wrap(~ECOG, ncol=6)

# ------------------

# Last BMI and Age Per Cachexia Status, Metastis, Top 5, ECOG -------

ECOG_NLPMeas_All_Records <- fread("Diagnosed Population 1.0/ECOG_NLPMeas_All_Records.txt")
unique(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records$measurement_value <- parse_number(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% filter(measurement_value>0&measurement_value<=5)
ECOG_NLPMeas_All_Records$measurement_value <- round(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% group_by(patid) %>% summarise(ECOG=max(measurement_value))


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer %in% c("Lung Cancer", "Intestinal Cancer", "Prostate Cancer", "Breast Cancer", "Pancreatic Cancer"))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)


PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- New_Primary_Cancer_Box %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 
PONS_Measures <- PONS_Measures %>% select(-test)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)
PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% select(value)
valuebucket <- PONS_Measures %>% mutate(valuebucket=ifelse(value<20,"<20",
                                            ifelse(value>=20&value<25,"20-25",
                                                   ifelse(value>=25&value<27,"25-27",
                                                          ifelse(value>=27&value<30,"27-30",">30"))))) %>%
  select(patid, weight, valuebucket) 



PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_onset)) %>% filter(cancer_onset<="2020-07-31")
PONS_Demographics <- PONS_Demographics %>% filter(age>=18) %>% 
  select(patid, weight, age, died, cancer_metastasis, cachexia_onset) %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
PONS_Demographics <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(PONS_Demographics) 
Pats_to_track_BMI <- PONS_Demographics  %>% 
  select(patid, weight, died,  cancer_metastasis, cachexia_onset)


PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI") %>% select(-weight) %>%
  inner_join(Pats_to_track_BMI %>% select(patid, weight))
Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats) %>% arrange(patid, claimed)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 
Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), 
                                                                                   median=median(value), 
                                                                                   min=min(value), 
                                                                                   max=max(value))
PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)
Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(
    PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate), by="patid"
    ) %>% select(patid, mindate, maxdate) %>% distinct()
PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))
PONS_Measures$mindate <- as.Date(PONS_Measures$mindate) ; PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)
PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% 
  inner_join(PONS_Measures) %>% select(-weight)
Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, cancer_metastasis, cachexia_onset) %>% 
  mutate(cachexia_onset=as.Date(cachexia_onset))

temp <- fread("Diagnosed Population 1.0/All_drops.txt")
New_Cachexia_Pred <- temp %>% filter( Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
CachexiaPats_ALL_NEW <- data.frame(
  New_Cachexia_Pred %>% left_join(
    Pats_to_track_BMI
    ) %>% inner_join(
      PONS_Measures %>% select(patid) %>% distinct()
      ) %>%
    bind_rows(
      Pats_to_track_BMI %>% filter(!is.na(cachexia_onset))
      ) %>%
             distinct()
  )


Cachexia_Dx <- CachexiaPats_ALL_NEW %>% filter(!is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Dx")
Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct()%>% mutate(group="Pred")
No_Cachexia <- Pats_to_track_BMI %>% select(patid) %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")


temp <- Cachexia_Dx %>% select(patid, group) %>%
  bind_rows(Cachexia_Pred %>% select(patid, group) ) %>%
  bind_rows(No_Cachexia %>% select(patid, group)) %>%
  inner_join(New_Primary_Cancer_Box ) %>%
  inner_join(Pats_to_track_BMI %>% select(patid, cancer_metastasis))


temp <- temp %>% inner_join(valuebucket)

data.frame(temp %>% group_by(Primary_Cancer, group, valuebucket) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(Primary_Cancer, group) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)) %>% select(-c(n, tot)) %>% spread(key=valuebucket, value=perc)


data.frame(temp %>%  inner_join(ECOG_NLPMeas_All_Records) %>%
             group_by(ECOG, group, valuebucket) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(ECOG, group) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)) %>% select(-c(n, tot)) %>% spread(key=valuebucket, value=perc)

data.frame(temp %>% group_by(cancer_metastasis , group, valuebucket) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(cancer_metastasis , group) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)) %>% select(-c(n, tot)) %>% spread(key=valuebucket, value=perc)



PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18) %>%
  mutate(age=ifelse(age<=55, "<55",
                    ifelse(age<=65,"55-65",
                           ifelse(age<=75,"65-75",
                                  ifelse(age<=85,"75-85",">85")))))


data.frame(PONS_Demographics %>% inner_join(temp) %>% group_by(Primary_Cancer, group, age) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(Primary_Cancer, group) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)) %>% select(-c(n, tot)) %>% spread(key=age, value=perc)


data.frame(temp %>%  inner_join(PONS_Demographics) %>% inner_join(ECOG_NLPMeas_All_Records) %>%
             group_by(ECOG, group, age) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(ECOG, group) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)) %>% select(-c(n, tot)) %>% spread(key=age, value=perc)

data.frame(temp %>%  inner_join(PONS_Demographics) %>% group_by(cancer_metastasis , group, age) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(cancer_metastasis , group) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)) %>% select(-c(n, tot)) %>% spread(key=age, value=perc)

# ------
# Cachexia Patients by ECOG stage --------

Cachexia_pats <- fread("Diagnosed Population 1.0/Cachexia_pats.txt")
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
Cachexia_pats <- New_Primary_Cancer_Box %>% inner_join(Cachexia_pats) %>% select(-died)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

Cachexia_pats <- Cachexia_pats %>% inner_join(PONS_Demographics)

ECOG_NLPMeas_All_Records <- fread("Diagnosed Population 1.0/ECOG_NLPMeas_All_Records.txt")
unique(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records$measurement_value <- parse_number(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% filter(measurement_value>0&measurement_value<=5)
ECOG_NLPMeas_All_Records$measurement_value <- round(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% group_by(patid) %>% summarise(ECOG=max(measurement_value))

Cachexia_pats <- Cachexia_pats %>% inner_join(ECOG_NLPMeas_All_Records)


data.frame(Cachexia_pats %>% group_by(Primary_Cancer, cancer_metastasis, ECOG) %>%
  summarise(n=sum(weight)) %>% ungroup() %>%
  group_by(Primary_Cancer, cancer_metastasis) %>% mutate(tot=sum(n)) %>%
  ungroup() %>% mutate(perc=n/tot) %>% select(-c(n, tot)) %>%
  spread(key=ECOG, value=perc))

data.frame(New_Primary_Cancer_Box %>% inner_join(ECOG_NLPMeas_All_Records) %>% group_by( ECOG) %>%
  summarise(n=sum(weight)) %>% ungroup())

data.frame(Cachexia_pats %>% group_by( ECOG) %>%
  summarise(n=sum(weight)) %>% ungroup())

CachexiaPats_ALL_NEW <- fread("Diagnosed Population 1.0/CachexiaPats_ALL_NEW.txt")

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% inner_join(PONS_Demographics)

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% inner_join(ECOG_NLPMeas_All_Records)
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% inner_join(New_Primary_Cancer_Box)


data.frame(CachexiaPats_ALL_NEW %>% group_by( ECOG) %>%
  summarise(n=sum(weight)) %>% ungroup())




# --------
# BMI drops ~ Genetic Background -----------------

# All Pats 
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")

# Cachexia Pred
Cachexia_pats <- fread("Diagnosed Population 1.0/Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pats <- Cachexia_pats %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_pats$weight)
PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% inner_join(Cachexia_pats)
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(New_Primary_Cancer_Box)
PONS_Measures <- PONS_Measures %>% select(-c(died, test)) %>% distinct()
Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))
Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))
PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)
Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)
Min_Max_Dates <- Min_Max_Dates %>% distinct()
PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(mean, median))
PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)
PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% 
  filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(patid,weight,min,max) %>% distinct()
PONS_Measures <-  PONS_Measures %>% mutate(absdiff=min-max) %>% select(-c(min, max))

data.frame(PONS_Measures %>% inner_join(New_Primary_Cancer_Box) %>% group_by(Primary_Cancer) %>%
  summarise(mean=mean(absdiff)) %>% arrange(mean))

PONS_Measures <- PONS_Measures %>% inner_join(New_Primary_Cancer_Box)


biomarkers <- fread("CAN Analysis Results 3.0/biomarkers.txt", sep=",")
biomarkers <- biomarkers %>% select(-c(chunck_id, SOURCEID, ENCID)) %>% distinct()
biomarkers <- biomarkers %>% inner_join(PONS_Measures %>% select(patid), by=c("PTID"="patid"))

unique(biomarkers$BIOMARKER_STATUS)
biomarkers <- biomarkers %>% filter(BIOMARKER_STATUS!="equivocal")

unique(biomarkers$VARIATION_DETAIL)

biomarkers <- biomarkers %>% inner_join(PONS_Measures, by=c("PTID"="patid"))

biomarkers_breast <- biomarkers %>% filter(Primary_Cancer=="Breast Cancer")

unique(biomarkers_breast$BIOMARKER)
unique(biomarkers$BIOMARKER)

biomarkers %>% group_by(BIOMARKER) %>% count() %>% arrange(-n)

biomarkers_breast <- biomarkers_breast %>% filter(BIOMARKER %in% c("HER2/NEU", "ER", "ER/PR", "ER/PR/HER2/NEU", "PR")) %>%
  select(PTID, BIOMARKER, VARIATION_DETAIL, BIOMARKER_STATUS) %>% distinct() %>% 
  mutate(BIOMARKER_STATUS=ifelse(BIOMARKER_STATUS=="amplified"|BIOMARKER_STATUS=="positive",1,0)) %>% distinct()  %>%
  # mutate(BIOMARKER=ifelse(BIOMARKER %in% c("ER", "ER/PR", "PR"), "HR", BIOMARKER)) %>% 
  select(-VARIATION_DETAIL) %>%
  distinct() %>% ungroup() %>% 
  group_by(PTID, BIOMARKER) %>% summarise(BIOMARKER_STATUS=max(BIOMARKER_STATUS)) %>% ungroup() 
biomarkers_breast[is.na(biomarkers_breast)] <- 9

biomarkers_breast %>% 
  left_join(biomarkers %>% select(PTID, absdiff)) %>%
  group_by(BIOMARKER, BIOMARKER_STATUS) %>% summarise(median=median(absdiff)) %>%
  spread(key=BIOMARKER, value=median)



biomarkers %>% group_by(BIOMARKER) %>% count() %>% arrange(-n)


biomarkers_lung <- biomarkers %>% filter(Primary_Cancer=="Lung Cancer")

unique(biomarkers_lung$BIOMARKER)

biomarkers_lung <- biomarkers_lung %>%
  mutate(BIOMARKER_STATUS=ifelse(BIOMARKER_STATUS=="amplified"|BIOMARKER_STATUS=="positive",1,0)) %>% distinct()  %>%
  select(-VARIATION_DETAIL) %>%
  distinct() %>% ungroup() %>% 
  group_by(PTID, BIOMARKER) %>% summarise(BIOMARKER_STATUS=max(BIOMARKER_STATUS)) %>% ungroup() 


biomarkers_lung %>% group_by(BIOMARKER) %>% count() %>% arrange(-n)

data.frame(biomarkers_lung %>% 
  left_join(biomarkers %>% select(PTID, absdiff)) %>%
  group_by(BIOMARKER, BIOMARKER_STATUS) %>% summarise(median=median(absdiff, na.rm=T)) %>%
  spread(key=BIOMARKER_STATUS, value=median))




biomarkers_Intestinal <- biomarkers %>% filter(Primary_Cancer=="Intestinal Cancer")

unique(biomarkers_Intestinal$BIOMARKER_STATUS)

biomarkers_Intestinal <- biomarkers_Intestinal %>%
  mutate(BIOMARKER_STATUS=ifelse(BIOMARKER_STATUS=="amplified"|BIOMARKER_STATUS=="positive",1,0)) %>% distinct()  %>%
  select(-VARIATION_DETAIL) %>%
  distinct() %>% ungroup() %>% 
  group_by(PTID, BIOMARKER) %>% summarise(BIOMARKER_STATUS=max(BIOMARKER_STATUS)) %>% ungroup() 


biomarkers_Intestinal %>% group_by(BIOMARKER) %>% count() %>% arrange(-n)

data.frame(biomarkers_Intestinal %>% 
  left_join(biomarkers %>% select(PTID, absdiff)) %>%
  group_by(BIOMARKER, BIOMARKER_STATUS) %>% summarise(median=median(absdiff, na.rm=T)) %>%
  spread(key=BIOMARKER_STATUS, value=median))

# -------------


# BMI drops Timing relative to cancer onset -----------------

# All Pats 
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")

# Cachexia Pred
Cachexia_pats <- fread("Diagnosed Population 1.0/Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pats <- Cachexia_pats %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_pats$weight)
PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% inner_join(Cachexia_pats)
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(New_Primary_Cancer_Box)
PONS_Measures <- PONS_Measures %>% select(-c(died, test)) %>% distinct()
Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))
Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))
PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)
Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)
Min_Max_Dates <- Min_Max_Dates %>% distinct()
PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(mean, median))
PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)
PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% 
  filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(patid,weight,min,max, mindate, maxdate, Primary_Cancer) %>% distinct()
PONS_Measures <- PONS_Measures %>% ungroup() %>% distinct()



Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

PONS_Measures <- PONS_Measures %>% mutate(mindate=format(as.Date(mindate), "%Y-%m")) %>%
  mutate(maxdate=format(as.Date(maxdate), "%Y-%m"))

PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("mindate"="Month")) %>% rename("MIN_DATE"="Exact_Month") 
PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("maxdate"="Month")) %>% rename("MAX_DATE"="Exact_Month") 



data.frame(PONS_Measures %>% mutate(Elapsed=MAX_DATE-MIN_DATE) %>% group_by(Primary_Cancer) %>% summarise(mean=mean(Elapsed))) %>%
  arrange(-mean)



PONS_Demographics_temp <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid,cancer_onset)
PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_onset=format(as.Date(cancer_onset), "%Y-%m"))
PONS_Demographics_temp <- PONS_Demographics_temp %>% left_join(Months_lookup, by=c("cancer_onset"="Month")) %>% 
  rename("ONSET_DATE"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(size=sum(weight))


 data.frame(PONS_Measures %>% left_join(PONS_Demographics_temp %>% select(-cancer_onset)) %>%
  mutate(MIN_REL=MIN_DATE-ONSET_DATE) %>% mutate(MAX_REL=MAX_DATE-ONSET_DATE) %>%
  group_by(Primary_Cancer) %>% summarise(mean_min=mean(MIN_REL), mean_max=mean(MAX_REL)) %>%
  arrange(-mean_min)) %>% mutate(diff=mean_min-mean_max) %>% arrange(diff) %>%
   left_join(New_Primary_Cancer_Box) %>%
  mutate(Primary_Cancer=str_replace(Primary_Cancer, " Cancer", "")) %>%
  ggplot(aes(mean_max, mean_min, colour=Primary_Cancer, fill=Primary_Cancer, size=size)) +
  geom_point(alpha=0.5, show.legend=FALSE) +
  scale_size(range = c(5, 30)) +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  xlim(-3,18) + ylim(-3,18) +
  geom_text(aes(label=Primary_Cancer), colour="black" , fontface="bold", size=3) +
  theme_minimal() +
  xlab("Number of Elapsed Months \n From Cancer Onset to MAX BMI") +
  ylab("Number of Elapsed Months \n From Cancer Onset to MIN BMI")
    



data.frame(PONS_Measures %>% left_join(PONS_Demographics_temp %>% select(-cancer_onset)) %>%
  mutate(MIN_REL=MIN_DATE-ONSET_DATE) %>% mutate(MAX_REL=MAX_DATE-ONSET_DATE) %>%
  group_by(Primary_Cancer) %>% summarise(mean_min=mean(MIN_REL), mean_max=mean(MAX_REL)) %>%
  arrange(-mean_min)) %>% mutate(diff=mean_min-mean_max) %>% arrange(diff) 

# -------------

# Number of Months on different nutrition therapies and time from anticancer to nutrition --------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, Treat) %>% count() %>% ungroup() %>%
  left_join(PONS_Ingredients %>% select(molecule, generic_name), by=c("Treat"="molecule")) %>% 
  select(-Treat)

unique(CAN_Drug_Histories$generic_name)

CAN_Drug_Histories %>% group_by(generic_name) %>% summarise(n=median(n))

Medroxyprogesterone <- CAN_Drug_Histories %>% filter(generic_name=="Medroxyprogesterone")
  
ggplot(CAN_Drug_Histories, 
       aes(x = log(n), fill=generic_name, colour=generic_name )) +  
  geom_density( alpha = 0.7) + 
  geom_boxplot(width = 0.3, position = position_nudge(y = -0.1), aes(y = -0.1), alpha=0.6) +  
  theme_minimal() +  
  labs(x = "\n Log10 \n # Number of Months ON Drugs", 
       y="Patient density \n") +
  theme(plot.title = element_text(hjust = 0.5))   +
  facet_wrap(~generic_name)  +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama() 


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))


CAN_Drug_Histories %>%
  inner_join(PONS_Demographics, by=c("patient"="patid")) %>%
  group_by(generic_name, cancer_metastasis) %>% summarise(n=mean(n)) %>%
  spread(key=cancer_metastasis, value=n)








New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)




PONS_Ingredients_nutrition <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))
string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients_nutrition$molecule, collapse = "|"),")\\b")

PONS_Ingredients_Anticancer <- PONS_Ingredients %>% filter(drug_group %in% c("Anticancer"))
string_Anticancer  <- paste0("\\b(",paste0(PONS_Ingredients_Anticancer$molecule, collapse = "|"),")\\b")



CAN_Drug_Histories_nutrition <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) %>%
  select(patient, weight, Month) %>% distinct() %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% ungroup()


CAN_Drug_Histories_Anticancer <- CAN_Drug_Histories %>% inner_join(CAN_Drug_Histories_nutrition %>% select(patient)) %>%
  filter(grepl(string_Anticancer,Treat)) %>%
  select(patient, weight, Month) %>% distinct() %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% ungroup()


CAN_Drug_Histories <- CAN_Drug_Histories_Anticancer %>% rename("Month_cancer"="Month") %>%
  inner_join(CAN_Drug_Histories_nutrition %>% rename("Month_nutrition"="Month"))



PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_onset)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics <- PONS_Demographics %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")

mean(CAN_Drug_Histories$Month_nutrition-CAN_Drug_Histories$Month_cancer)

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics, by=c("patient"="patid")) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(elapsed_cancer=Month_cancer-cancer_onset) %>%
  mutate(elapsed_nutrition=Month_nutrition -cancer_onset) 

CAN_Drug_Histories %>% mutate(elapsed=Month_nutrition-Month_cancer) %>%
  summarise(mean=mean(elapsed), median=median(elapsed))
  

CAN_Drug_Histories %>%
  ggplot(aes(Month_nutrition-Month_cancer)) +
  geom_density( alpha = 0.7, fill="midnightblue", colour="midnightblue") + 
  theme_minimal() +  
  labs(x = "\n Number of Months Between \n Anticancer Start and Nutrition Therapy Start", 
       y="Patient density \n") +
  theme(plot.title = element_text(hjust = 0.5)) 
  


# -----------



# % Nutrition Therapy patients by primary cancer site -------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-c(disease, Month))
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients_nutrition <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Testosterone", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))
string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients_nutrition$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories_nutrition <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat))

CAN_Drug_Histories_nutrition <- separate_rows(CAN_Drug_Histories_nutrition, Treat, sep = ",", convert=T)

CAN_Drug_Histories_nutrition <- CAN_Drug_Histories_nutrition %>% distinct() %>% filter(grepl(string_nutrition,Treat))

CAN_Drug_Histories_nutrition <- CAN_Drug_Histories_nutrition %>% inner_join(PONS_Ingredients_nutrition, by=c("Treat"="molecule"))


PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories 3.0/PONS Comorbidity Inventories.txt" , sep="\t")

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(grepl("I26", diagnosis)|
                                          grepl("I80", diagnosis)|
                                          grepl("I81", diagnosis)|
                                          grepl("I82", diagnosis)|
                                          grepl("I65", diagnosis)|
                                          grepl("I66", diagnosis)|
                                          grepl("I63", diagnosis)|
                                          grepl("I21", diagnosis))

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(patid) %>% distinct()

num <- CAN_Drug_Histories_nutrition %>% anti_join(PONS_Comorbidity_Inventories, by=c("patient"="patid")) %>%
  select(patient, weight, generic_name) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box, by=c("patient"="patid", "weight"="weight")) %>%
  group_by(generic_name, diagnosis) %>% summarise(num=sum(weight))

num <- num %>% ungroup() %>% group_by(generic_name) %>% mutate(tot=sum(num))

num <- num %>% mutate(perc=num/tot)  %>% select(-c(num, tot))

num <- num %>% spread(key=diagnosis, value=perc)

fwrite(num, "temp.csv")


# ---------------

# % Radiotherapy by cancer stage and ECOG Score -------

PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories 3.0/PONS Comorbidity Inventories.txt" , sep="\t")

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(grepl("I26", diagnosis)|
                                          grepl("I80", diagnosis)|
                                          grepl("I81", diagnosis)|
                                          grepl("I82", diagnosis)|
                                          grepl("I65", diagnosis)|
                                          grepl("I66", diagnosis)|
                                          grepl("I63", diagnosis)|
                                          grepl("I21", diagnosis))

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(patid) %>% distinct()



PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer %in% c("Lung Cancer", "Intestinal Cancer", "Prostate Cancer", "Breast Cancer", "Pancreatic Cancer"))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% anti_join(PONS_Comorbidity_Inventories) %>%
  inner_join(PONS_Demographics)



ECOG_NLPMeas_All_Records <- fread("Diagnosed Population 1.0/ECOG_NLPMeas_All_Records.txt")
unique(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records$measurement_value <- parse_number(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% filter(measurement_value>0&measurement_value<=5)
ECOG_NLPMeas_All_Records$measurement_value <- round(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% group_by(patid) %>% summarise(ECOG=max(measurement_value))

ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% inner_join(New_Primary_Cancer_Box) %>% select(-Primary_Cancer)

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))
CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-c(disease, Month))
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_radio <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Radiotherapy"], collapse = "|"),")\\b")
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_radio,Treat)) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient) %>% distinct() %>% rename("patid"="patient")


temp <- ECOG_NLPMeas_All_Records %>% group_by(cancer_metastasis, ECOG) %>% summarise(tot=sum(weight)) %>%
  left_join(
    ECOG_NLPMeas_All_Records %>% inner_join(CAN_Drug_Histories) %>% group_by(cancer_metastasis, ECOG) %>% summarise(num=sum(weight))
  ) %>% mutate(perc=num/tot) %>% mutate(perc=ifelse(is.na(perc),0,perc))


temp %>% select(cancer_metastasis, ECOG, perc) %>%
  spread(key=cancer_metastasis, value=perc)

# --------

# Specialty Responsible for 1st Nutrition Initiation and Number Anticancer Lines per Nutrition Molecule Exp ---------
CachexiaPats_ALL_NEW <- fread("Diagnosed Population 1.0/CachexiaPats_ALL_NEW.txt") 

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CachexiaPats_ALL_NEW %>% left_join(CAN_Drug_Histories %>% select(patient, weight), by=c("patid"="patient")) %>% summarise(n=sum(weight))
# 1589765
CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CancerDrug_Experienced %>% inner_join(CachexiaPats_ALL_NEW)  # 30719

CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
names(CAN_Drug_Histories)[1] <- "patient"

sum(CAN_Drug_Histories$weight) # 935784.2


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name=="Dronabinol"|
                                                  generic_name=="Megestrol"|
                                                  generic_name=="Cyproheptadine"|
                                                  generic_name=="Nutrition Therapy"|
                                                  generic_name=="Medroxyprogesterone"|
                                                  generic_name=="Testosterone") %>%  select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")

string_Appetite <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

Appetite_Exp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% select(patient, weight) %>% distinct()
sum(Appetite_Exp$weight) # 137187.8   # 0.1460499  4454

temp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% group_by(patient) %>% 
   select(patient, weight, Drugs) %>% distinct()
temp <- separate_rows(temp, Drugs, sep = ",", convert=T)
temp <- temp %>% filter(grepl(string_Appetite, Drugs))
 temp <- temp %>% select(patient, Drugs) %>% distinct()

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

temp <- temp %>% left_join(PONS_Ingredients %>% select(molecule, generic_name), by=c("Drugs"="molecule")) %>%
  select(patient, generic_name) %>% distinct()

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- temp %>% select(patient) %>% left_join(CAN_Drug_Histories %>% select(-disease))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()

PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 3.0/PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass) %>%
  select(patient, chemo_class) %>% distinct()

unique(CAN_Drug_Histories$chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(chemo_class=="Alkylating Agent"|
                                chemo_class=="Immuno/Targeted"|
                                chemo_class=="Hormonal Therapy"|
                                chemo_class=="Biologic"|
                                chemo_class=="Radio"|
                                  chemo_class=="Antimicrotubule Agent"|
                                  chemo_class=="Platinum agent"|
                                  chemo_class=="Antimetabolites"|
                                  chemo_class=="Topoisomerase Inhibitor"|
                                  chemo_class=="Other Antineoplastics"|
                                  chemo_class=="PD1/PDL1"|
                                  chemo_class=="Surgery Inpatient")


CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Exp=1) %>% ungroup()
CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0
temp <-temp %>% left_join(CAN_Drug_Histories)
names(temp)[2] <- "AppetiteStimulantGroup"

temp$AppetiteStimulantGroup <- as.factor(temp$AppetiteStimulantGroup)
length(unique(temp$AppetiteStimulantGroup))

for (i in names(temp[,3:14])){
  print(i)
  print(temp  %>% group_by(get(i),AppetiteStimulantGroup) %>% count())
}

names(temp)
temp %>% filter(`Topoisomerase Inhibitor`==1) %>% group_by(AppetiteStimulantGroup) %>% count()

for (i in names(temp[,3:14])){
  print(i)
}

for (i in names(temp[,3:14])){
  print(i)
  print(temp  %>% filter(get(i)==1) %>% group_by(get(i),AppetiteStimulantGroup) %>% count())
}


temp



noLines <- fread("CAN Analysis Results 2.2/Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")
noLines <- temp %>% select(patient) %>% left_join(noLines)

noLines <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
noLines <- noLines %>% filter(!is.na(Drugs)) %>% filter(Drugs!="")  %>% select(patient, weight, Drugs) %>% distinct()

temp <- temp %>% left_join(noLines %>% group_by(patient, weight) %>% count())
names(temp)[16] <- "NoLines"

temp %>% ungroup() %>% summarise(n=weighted.mean(NoLines, weight))
temp %>% ungroup() %>% group_by(AppetiteStimulantGroup)  %>% summarise(n=weighted.mean(NoLines, weight))



PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis)
names(PONS_Demographics)[1] <- "patient"
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
temp <- temp %>% left_join(PONS_Demographics)

temp %>% group_by(cancer_metastasis, AppetiteStimulantGroup) %>% count() %>% ungroup() %>%
  spread(key=cancer_metastasis, value=n)

temp %>% ungroup() %>% group_by(cancer_metastasis, AppetiteStimulantGroup)  %>% summarise(n=weighted.mean(NoLines, weight))




New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)
names(New_Primary_Cancer_Box)[1] <- "patient"

temp <- temp %>% left_join(New_Primary_Cancer_Box)

data.frame(temp %>% group_by(AppetiteStimulantGroup, Primary_Cancer) %>% count() %>% ungroup() %>%
               mutate(Primary_Cancer=str_replace(Primary_Cancer, " Cancer", "")) %>%
  spread(key=Primary_Cancer, value=n))

data.frame(temp %>% group_by(Primary_Cancer, AppetiteStimulantGroup) %>% count() %>% ungroup() %>%
               mutate(Primary_Cancer=str_replace(Primary_Cancer, " Cancer", "")) %>%
  spread(key=AppetiteStimulantGroup, value=n))

temp %>% group_by(Primary_Cancer, AppetiteStimulantGroup) %>% count() %>% ungroup() %>%
  spread(key=AppetiteStimulantGroup, value=n)






CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt")
names(CancerDrug_Experienced)[1] <- "patient"

data.frame(New_Primary_Cancer_Box %>% inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>% inner_join(CancerDrug_Experienced) %>%
  group_by(Primary_Cancer) %>% count() %>% rename("Total"="n") %>% ungroup() %>%
  left_join(temp %>% group_by(Primary_Cancer, AppetiteStimulantGroup) %>% count() %>% ungroup() %>%
  spread(key=AppetiteStimulantGroup, value=n)) %>%
  mutate(Primary_Cancer=str_replace(Primary_Cancer, " Cancer", ""))) %>%
  select(Total)


data.frame(New_Primary_Cancer_Box %>% inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>% inner_join(CancerDrug_Experienced) %>%
  inner_join(temp %>% select(patient) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% count())



New_Primary_Cancer_Box
PONS_Demographics


CAN_Doses <- fread("CAN Analysis Results 3.0/CAN Doses.txt")

CAN_Doses <- temp %>% select(patient) %>% distinct() %>% left_join(CAN_Doses, by=c("patient"="pat_id"))
CAN_Doses <- temp %>% ungroup() %>% select(AppetiteStimulantGroup) %>% distinct() %>% left_join(CAN_Doses, by=c("AppetiteStimulantGroup"="generic_name"))

CAN_Doses <- CAN_Doses %>% select(AppetiteStimulantGroup, patient, from_dt, prov, specialty)
CAN_Doses$from_dt <- as.Date(CAN_Doses$from_dt)

CAN_Doses <- CAN_Doses %>% group_by(patient) %>% filter(from_dt==min(from_dt))


PONS_Event_Claims_Providers <- fread("PONS Events/PONS Event Claims Providers.txt")
unique(PONS_Event_Claims_Providers$taxonomy1_specialty)

PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers %>% select(prov, specialty_classification)

CAN_Doses %>% inner_join(PONS_Demographics %>% filter(cancer_metastasis==0)) %>%
             left_join(PONS_Event_Claims_Providers) %>%
  group_by(AppetiteStimulantGroup, specialty_classification) %>% ungroup() %>%
  filter(!is.na(specialty_classification)) %>%
  filter(specialty_classification %in% c("Internal Medicine", "Other Physician",
                                         "Primary Care", "Oncologist", "Other Provider")) 


data.frame(CAN_Doses %>% inner_join(PONS_Demographics %>% filter(cancer_metastasis==0)) %>%
             left_join(PONS_Event_Claims_Providers) %>%
  group_by(AppetiteStimulantGroup, specialty_classification) %>% count() %>%
  ungroup() %>% spread(key=specialty_classification, value=n))

# ----------------------
# Megestrol Pats - Persistency Megestrol vs Anticancer drugs -------------------------------
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Megestrol <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$generic_name == "Megestrol"], collapse = "|"),")\\b")

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CancerDrug_Experienced %>% select(patid) %>% inner_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Megestrol_Pats <- CAN_Drug_Histories %>% filter(grepl(string_Megestrol, Drugs)) %>% select(patid) %>% distinct()

Megestrol_Pats


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(Megestrol_Pats, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Megestrol,Treat)) 


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
metastasis_flag <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
names(metastasis_flag)[1] <- "patient"


CAN_Drug_Histories %>% select(patient, weight) %>% inner_join(metastasis_flag) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% inner_join(metastasis_flag) %>% select(patient,cancer_metastasis) %>% group_by(patient,cancer_metastasis) %>% count()
)  %>% ungroup() %>% group_by(cancer_metastasis) %>% summarise(n=weighted.mean(n, weight)) # 3.711574

# 1                 0  4.60
# 2                 1  3.44

temp <- CAN_Drug_Histories %>% inner_join(metastasis_flag) %>%   select(patient, weight, cancer_metastasis) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>%inner_join(metastasis_flag) %>%  select(patient,cancer_metastasis) %>% group_by(patient,cancer_metastasis) %>% count()
)  %>%
  group_by(cancer_metastasis,n) %>% summarise(total=sum(weight)) %>%
  ungroup() 

Persistency_Megestrol <- temp
data.frame(Persistency_Megestrol)

data.frame(Persistency_Megestrol %>% spread(key=cancer_metastasis, value=total))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(Megestrol_Pats, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")


unique(PONS_Ingredients$drug_class)

string_Cancer <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                  PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                  PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                  PONS_Ingredients$drug_class == "GDF15"|
                                                                  PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")





CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer,Treat)) 

CAN_Drug_Histories %>% inner_join(metastasis_flag) %>%  select(patient, weight, cancer_metastasis) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% inner_join(metastasis_flag) %>%  select(patient,cancer_metastasis) %>% group_by(patient,cancer_metastasis) %>% count()
)  %>% ungroup() %>% group_by(cancer_metastasis) %>% summarise(n=weighted.mean(n, weight)) 

# 1                 0  12.0
# 2                 1  14.7


temp <- CAN_Drug_Histories %>% inner_join(metastasis_flag) %>%  select(patient, weight,cancer_metastasis) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% inner_join(metastasis_flag) %>%  select(patient,cancer_metastasis) %>% group_by(patient,cancer_metastasis) %>% count()
)  %>%
  group_by(cancer_metastasis,n) %>% summarise(total=sum(weight)) %>%
  ungroup() 

Persistency_Anticancer <- temp

data.frame(Persistency_Anticancer)

data.frame(Persistency_Anticancer %>% spread(key=cancer_metastasis, value=total))

# -------------------------------------------------------
# Number patient-months per type, metastasis, cachexia status ----------------------------------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=49)

sum(CAN_Drug_Histories$weight)

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
unique(PONS_Ingredients$drug_class)

string_Cancer <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                  PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                  PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                  PONS_Ingredients$drug_class == "GDF15"|
                                                                  PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)
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
string_Surgery        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Surgery Inpatient"], collapse = "|"),")\\b")



string_Other        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Hormonal Therapy"|
                                                                                      PONS_Ingredients_JN_ChemoClass$chemo_class == "Alkylating Agent"|
                                                                                      PONS_Ingredients_JN_ChemoClass$chemo_class == "Other Antineoplastics"|
                                                                                      PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimetabolites"|
                                                                                      PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimicrotubule Agent"|
                                                                                      PONS_Ingredients_JN_ChemoClass$chemo_class == "Topoisomerase Inhibitor"|
                                                                                      PONS_Ingredients_JN_ChemoClass$chemo_class == "Radio"|
                                                                                      PONS_Ingredients_JN_ChemoClass$chemo_class == "Surgery Inpatient"], collapse = "|"),")\\b")


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat))
length(unique(CAN_Drug_Histories$patient)) # 140478 (last year, out of 320197)



Metastasis_stage <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
Metastasis_stage <- Metastasis_stage %>%  select(patid, cancer_metastasis)
Metastasis_stage <- Metastasis_stage %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
names(Metastasis_stage)[1] <- "patient"


CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, weight) %>% count()
CAN_Drug_Histories %>% ungroup() %>% summarise(n=sum(weight*n)) # 27094965 patient months

CAN_Drug_Histories %>% inner_join(Metastasis_stage) %>% ungroup() %>% group_by(cancer_metastasis) %>%
  summarise(n=sum(weight*n)) # 27094965 patient months
# 1                 0 12500622.
# 2                 1 16464893.


CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4151912
CAN_Drug_Histories %>% select(patient, weight) %>% inner_join(Metastasis_stage) %>% 
  distinct() %>% ungroup() %>% group_by(cancer_metastasis) %>% summarise(n=sum(weight)) # 4151912
# 1                 0 1891923.
# 2                 1 2429902.


CachexiaPats_ALL_NEW %>% rename("patient"="patid") %>% inner_join(CAN_Drug_Histories) %>%
  inner_join(Metastasis_stage) %>% ungroup() %>% group_by(cancer_metastasis) %>%
  summarise(n=sum(weight*n))


# BY primary cancer
data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))
                                 

# BY metastasis state
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

data.frame(CAN_Drug_Histories %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(cancer_metastasis , diagnosis) %>% summarise(n=sum(weight*n)) %>% 
    spread(key=cancer_metastasis, value=n))

# BY cachexia dx/pred 
CachexiaPats_ALL_NEW <- fread("Diagnosed Population 1.0/CachexiaPats_ALL_NEW.txt")

CachexiaPats_ALL_NEW %>% left_join(New_Primary_Cancer_Box) %>% summarise(n=sum(weight))

CAN_Drug_Histories %>% inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4151912
# 664132

data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))



data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
  ungroup() %>% summarise(n=sum(weight*n)))


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset) %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))

CAN_Drug_Histories %>% ungroup() %>% inner_join(PONS_Demographics %>% filter(cachexia_onset == 1), by=c("patient"="patid")) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 95145

data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(PONS_Demographics %>% filter(cachexia_onset == 1), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))

 
# ------------------------------------------------
# Radiotherapy and Apetite Stimulants -----------
CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
names(CAN_Drug_Histories)[1] <- "patient"
sum(CAN_Drug_Histories$weight) # 9861087

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

unique(PONS_Ingredients$drug_class)

PONS_Ingredients %>% filter(drug_group=="Anticachexia")

PONS_Ingredients <- PONS_Ingredients %>% 
  filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-") %>% select(-Month) %>% distinct()

string_Appetite <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

Appetite_Exp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% select(patient, weight) %>% distinct()
sum(Appetite_Exp$weight) # 1281392   

CachexiaPats_ALL_NEW <- fread("Diagnosed Population 1.0/CachexiaPats_ALL_NEW.txt")
names(CachexiaPats_ALL_NEW)[1] <- "patient"

Appetite_Exp %>% inner_join(CachexiaPats_ALL_NEW) %>% summarise(n=sum(weight))

# Appetite_Exp <- Appetite_Exp %>% inner_join(CachexiaPats_ALL_NEW) 

temp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% group_by(patient) %>% 
   select(patient, weight, Drugs) %>% distinct()

temp <- separate_rows(temp, Drugs, sep = ",", convert=T)
temp <- temp %>% filter(grepl(string_Appetite, Drugs))
temp <- temp %>% select(patient, Drugs) %>% distinct()

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

temp <- temp %>% left_join(PONS_Ingredients %>% select(molecule, generic_name), by=c("Drugs"="molecule")) %>%
  select(patient, generic_name) %>% distinct()

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-") %>% select(-Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()

string_Radio <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class=="Radiotherapy"], collapse = "|"),")\\b")


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Radio, Drugs)) %>% 
  select(patient, weight) %>% distinct()
CAN_Drug_Histories$Radio <- 1

PONS_Demographics_temp <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_temp$cancer_metastasis <- as.Date(PONS_Demographics_temp$cancer_metastasis)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-weight)


CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(temp)


CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Demographics_temp %>% rename("patient"="patid"))

CAN_Drug_Histories <- temp %>% select(patient) %>% distinct() %>% inner_join(CAN_Drug_Histories)



ECOG_NLPMeas_All_Records <- fread("Diagnosed Population 1.0/ECOG_NLPMeas_All_Records.txt")
unique(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records$measurement_value <- parse_number(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% filter(measurement_value>0&measurement_value<=5)
ECOG_NLPMeas_All_Records$measurement_value <- round(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% group_by(patid) %>% summarise(ECOG=max(measurement_value))
names(ECOG_NLPMeas_All_Records)[1] <- "patient"

CAN_Drug_Histories %>% inner_join(ECOG_NLPMeas_All_Records) %>% select(patient, weight, cancer_metastasis, ECOG) %>% distinct() %>%
  group_by(cancer_metastasis,ECOG) %>% summarise(n=sum(weight)) %>%
  spread(key=ECOG, value=n)

CAN_Drug_Histories %>%  inner_join(ECOG_NLPMeas_All_Records) %>% group_by(cancer_metastasis, Radio, generic_name, ECOG) %>% summarise(tot=sum(weight)) %>%
  spread(key=ECOG, value=tot) 


# ------------

# Probability of having a Cachexia Dx ~ Lab test results ---------------

# Albumin 

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
Cachexia_pats <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, cachexia_onset, cancer_onset)

PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
unique(PONS_Measures$test)
PONS_Measures <- PONS_Measures %>% filter(test=="Albumin")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value) %>% distinct() 

temp <- PONS_Measures %>% left_join(PONS_Demographics %>% select(patid, cancer_onset)) %>%
  drop_na() %>% mutate(claimed=as.Date(claimed)) %>% mutate(cancer_onset=as.Date(cancer_onset)) %>%
  mutate(elapsed=round(as.numeric(claimed-cancer_onset)/30.5,0)) 


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-") %>% select(patid)

temp <- temp %>% inner_join(New_Primary_Cancer_Box)

temp %>% select(patid, value, elapsed) %>% distinct() %>% group_by(patid, elapsed) %>%
  summarise(value=mean(value)) %>% ungroup() %>%
  ggplot(aes(elapsed, value)) +
  geom_smooth(method="looes") +
  xlim(-24,24) +
  theme_minimal() + 
  xlab("\n Number of Elapsed Months") + ylab("Serum Albumin Level \n")

last <- temp %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% ungroup()
 


last %>% filter(value>0) %>% left_join(Cachexia_pats %>% select(patid) %>% mutate(CCh=1)) %>%
  mutate(CCh=ifelse(is.na(CCh) , 0, CCh)) %>% 
  ggplot(aes(x=value, y=CCh)) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial)) +
  theme_minimal() + 
  xlim(1,6) +
  xlab("\n Serum Albumin Level") + ylab("Probability of Cachexia ICD Diagnosis \n")



# Hemoglobin 

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
Cachexia_pats <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, cachexia_onset, cancer_onset)

PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="Hemoglobin")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value) %>% distinct() 

temp <- PONS_Measures %>% left_join(PONS_Demographics %>% select(patid, cancer_onset)) %>%
  drop_na() %>% mutate(claimed=as.Date(claimed)) %>% mutate(cancer_onset=as.Date(cancer_onset)) %>%
  mutate(elapsed=round(as.numeric(claimed-cancer_onset)/30.5,0)) 


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-") %>% select(patid)

temp <- temp %>% inner_join(New_Primary_Cancer_Box)

temp %>% select(patid, value, elapsed) %>% distinct() %>% group_by(patid, elapsed) %>%
  inner_join(Cachexia_pats %>% select(patid)) %>%
  summarise(value=mean(value)) %>% ungroup() %>%
  ggplot(aes(elapsed, value)) +
  geom_smooth( colour="firebrick") +
  xlim(-24,24) +
  theme_minimal() + 
  xlab("\n Number of Elapsed Months") + ylab("Serum Hemoglobin Level \n")

last <- temp %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% ungroup()
 
last %>% filter(value>0) %>% left_join(Cachexia_pats %>% select(patid) %>% mutate(CCh=1)) %>%
  mutate(CCh=ifelse(is.na(CCh) , 0, CCh)) %>% 
  ggplot(aes(x=value, y=CCh)) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="firebrick") +
  theme_minimal() + 
  xlim(4,18) +
  xlab("\n Serum Hemoglobin Level") + ylab("Probability of Cachexia ICD Diagnosis \n")








# CRP

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
Cachexia_pats <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, cachexia_onset, cancer_onset)

PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
unique(PONS_Measures$test)
PONS_Measures <- PONS_Measures %>% filter(test=="C-Reactive Protein")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value) %>% distinct() 

temp <- PONS_Measures %>% left_join(PONS_Demographics %>% select(patid, cancer_onset)) %>%
  drop_na() %>% mutate(claimed=as.Date(claimed)) %>% mutate(cancer_onset=as.Date(cancer_onset)) %>%
  mutate(elapsed=round(as.numeric(claimed-cancer_onset)/30.5,0)) 


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-") %>% select(patid)

temp <- temp %>% inner_join(New_Primary_Cancer_Box)

temp %>% select(patid, value, elapsed) %>% distinct() %>% group_by(patid, elapsed) %>%
  inner_join(Cachexia_pats %>% select(patid)) %>%
  summarise(value=mean(value)) %>% ungroup() %>%
  ggplot(aes(elapsed, value)) +
  geom_smooth( colour="firebrick") +
  xlim(-24,24) +
  theme_minimal() + 
  xlab("\n Number of Elapsed Months") + ylab("Serum C-Reactive Protein Level \n")

last <- temp %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% ungroup()
 
last %>% filter(value>0) %>% left_join(Cachexia_pats %>% select(patid) %>% mutate(CCh=1)) %>%
  mutate(CCh=ifelse(is.na(CCh) , 0, CCh)) %>% 
  ggplot(aes(x=value, y=CCh)) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="darkgreen") +
  theme_minimal() + 
  #xlim(4,18) +
  xlab("\n Serum C-Reactive Protein Level") + ylab("Probability of Cachexia ICD Diagnosis \n")



# BMI


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
Cachexia_pats <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, cachexia_onset, cancer_onset)

PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
unique(PONS_Measures$test)
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value) %>% distinct() 

temp <- PONS_Measures %>% left_join(PONS_Demographics %>% select(patid, cancer_onset)) %>%
  drop_na() %>% mutate(claimed=as.Date(claimed)) %>% mutate(cancer_onset=as.Date(cancer_onset)) %>%
  mutate(elapsed=round(as.numeric(claimed-cancer_onset)/30.5,0)) 


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-") %>% select(patid)

temp <- temp %>% inner_join(New_Primary_Cancer_Box)

temp %>% select(patid, value, elapsed) %>% distinct() %>% group_by(patid, elapsed) %>%
  inner_join(Cachexia_pats %>% select(patid)) %>%
  summarise(value=mean(value)) %>% ungroup() %>%
  ggplot(aes(elapsed, value)) +
  geom_smooth( colour="firebrick") +
  xlim(-24,24) +
  theme_minimal() + 
  xlab("\n Number of Elapsed Months") + ylab("BMI Level \n")

last <- temp %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% ungroup()
 
last %>% filter(value>0) %>% left_join(Cachexia_pats %>% select(patid) %>% mutate(CCh=1)) %>%
  mutate(CCh=ifelse(is.na(CCh) , 0, CCh)) %>% 
  ggplot(aes(x=value, y=CCh)) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="orange") +
  theme_minimal() + 
  xlim(15,40) +
  xlab("\n BMI Level") + ylab("Probability of Cachexia ICD Diagnosis \n")





# Height


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
Cachexia_pats <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, cachexia_onset, cancer_onset)

PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
unique(PONS_Measures$test)
PONS_Measures <- PONS_Measures %>% filter(test=="Body Height")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value) %>% distinct() 

temp <- PONS_Measures %>% left_join(PONS_Demographics %>% select(patid, cancer_onset)) %>%
  drop_na() %>% mutate(claimed=as.Date(claimed)) %>% mutate(cancer_onset=as.Date(cancer_onset)) %>%
  mutate(elapsed=round(as.numeric(claimed-cancer_onset)/30.5,0)) 


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-") %>% select(patid)

temp <- temp %>% inner_join(New_Primary_Cancer_Box)

temp %>% select(patid, value, elapsed) %>% distinct() %>% group_by(patid, elapsed) %>%
  inner_join(Cachexia_pats %>% select(patid)) %>%
  summarise(value=mean(value)) %>% ungroup() %>%
  ggplot(aes(elapsed, value)) +
  geom_smooth( colour="firebrick") +
  xlim(-24,24) +
  theme_minimal() + 
  xlab("\n Number of Elapsed Months") + ylab("Height \n")

last <- temp %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% ungroup()
 
last %>% filter(value>0) %>% left_join(Cachexia_pats %>% select(patid) %>% mutate(CCh=1)) %>%
  mutate(CCh=ifelse(is.na(CCh) , 0, CCh)) %>% 
  ggplot(aes(x=value, y=CCh)) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="purple") +
  theme_minimal() + 
  xlim(150,220) +
  coord_cartesian(ylim=c(0,0.05)) +
  xlab("\n Height") + ylab("Probability of Cachexia ICD Diagnosis \n")


# ---------------

# ICD Cachexia Dx vs Cachexia Pred vs None - Mets vs Non-mets ----------
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)
Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_Dx$weight)

# Cachexia Pred
Cachexia_pats <- fread("Diagnosed Population 1.0/Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pats <- Cachexia_pats %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_pats$weight)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60) %>% filter(Status=="Earliest"|Status=="Metastasis") %>% ungroup() %>% select(patid)


# All Pats
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight) %>% anti_join(Cachexia_Dx %>% select(patid))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight) %>% anti_join(Cachexia_pats %>% select(patid))
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight)) # 20070541
All_Pats <- New_Primary_Cancer_Box

# Cachecia Dx
Cachexia_Dx <- Cachexia_Dx %>% inner_join(PONS_Demographics)
sum(Cachexia_Dx$weight) # 135536.1

# Cachexia Pred
Cachexia_pats <- Cachexia_pats %>% inner_join(PONS_Demographics)
sum(Cachexia_pats$weight) # 1270184


PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories 3.0/PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[3] <- "ICD"
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-weight)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% inner_join(All_Pats %>% full_join(Cachexia_Dx) %>% full_join(Cachexia_pats))
PONS_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 21340695

PONS_Comorbidity_Inventories$ICD <- substr(PONS_Comorbidity_Inventories$ICD,1,3)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories  %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("R", ICD)) 



PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

All_Pats %>% inner_join(PONS_Demographics) %>% group_by(cancer_metastasis) %>% summarise(n=sum(weight))
# 1                 0 12572314.
# 2                 1  7498227.

Cachexia_Dx %>% inner_join(PONS_Demographics) %>% group_by(cancer_metastasis) %>% summarise(n=sum(weight))
# 1                 0 46621.
# 2                 1 88915.

Cachexia_pats %>% inner_join(PONS_Demographics) %>% group_by(cancer_metastasis) %>% summarise(n=sum(weight))
#1                 0 638145.
# 2                 1 632039.


data.frame(All_Pats %>% mutate(group="none") %>%
  bind_rows(Cachexia_Dx %>% mutate(group="Dx")) %>%
  bind_rows(Cachexia_pats %>% mutate(group="Pred")) %>%
  inner_join(PONS_Comorbidity_Inventories) %>%
  inner_join(PONS_Demographics) %>%
  filter(ICD %in% c("D50", "D51", "D52", "D63", "D64",
                    "E43", "E44",  "E46", "D46", "D53", "E53", "E56", "E61",
                    "F32", "F33", "F39", "F41", 
                    "R10", "R11", "R18", "R19", "R40", "R41", "R42", "R44", "R45", "R46", "R52", "R53", "R63", "R64", "R65")) %>%
  group_by(group, cancer_metastasis, ICD) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(cancer_metastasis==1&group=="none", n/7498227.,
                  ifelse(cancer_metastasis==0&group=="none", n/12572314.,
                         ifelse(cancer_metastasis==1&group=="Dx", n/88915.,
                                ifelse(cancer_metastasis==0&group=="Dx", n/46621.,
                                       ifelse(cancer_metastasis==1&group=="Pred", n/632039.,
                                              ifelse(cancer_metastasis==0&group=="Pred", n/638145.,)))))))) %>%
  spread(key=group, value=n)



# ---------------------


# Health Care Utilizations by Site and Metastasis --------------------------------
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, weight)
names(Cachexia_Dx)[1] <- "patient"
Cachexia_Dx$Group <- "Dx"


CachexiaPats_ALL_NEW <- fread("Diagnosed Population 1.0/CachexiaPats_ALL_NEW.txt")
names(CachexiaPats_ALL_NEW)[1] <- "patient"
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% anti_join(Cachexia_Dx)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics %>% select(patid, weight), by=c("patient"="patid"))
CachexiaPats_ALL_NEW$Group <- "Pred"


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer != "-" & Primary_Cancer != "Unspecified Cancer")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight)
names(New_Primary_Cancer_Box)[1] <- "patient"

Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box)
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% inner_join(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% anti_join(Cachexia_Dx) %>% anti_join(CachexiaPats_ALL_NEW)
New_Primary_Cancer_Box$Group <- "none"

Groups <- Cachexia_Dx %>% bind_rows(CachexiaPats_ALL_NEW) %>% bind_rows(New_Primary_Cancer_Box)


PONS_Utilizations <- fread("PONS Utilizations/PONS Utilizations.txt")
PONS_Utilizations <- PONS_Utilizations %>% inner_join(Groups, by=c("patid"="patient")) %>% select(-weight.y)

for (i in names(PONS_Utilizations[,3:25])){
  print(i)
  print(PONS_Utilizations  %>% group_by(Group) %>% summarise(n=mean(get(i))))
}

encounter_visits
medical_visits
rx_visits

hospital_days
hospital_stays
surgery_visits
emergency_visits

encounter_providers
medical_providers
rx_providers

drug_supply_days
drug_ingredients





PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer %in% c("Lung Cancer", "Intestinal Cancer", "Prostate Cancer", "Breast Cancer", "Pancreatic Cancer"))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics) %>%
  inner_join(PONS_Utilizations)


data.frame(New_Primary_Cancer_Box %>% group_by(cancer_metastasis, Group, Primary_Cancer) %>%
  summarise(encounter_visits=mean(encounter_visits),
            medical_visits=mean(medical_visits),
            rx_visits=mean(rx_visits),
            hospital_days=mean(hospital_days),
            hospital_stays=mean(hospital_stays),
            surgery_visits=mean(surgery_visits),
            emergency_visits=mean(emergency_visits),
            encounter_providers=mean(encounter_providers),
            medical_providers=mean(medical_providers),
            rx_providers=mean(rx_providers),
            drug_supply_days=mean(drug_ingredients)))



ECOG_NLPMeas_All_Records <- fread("Diagnosed Population 1.0/ECOG_NLPMeas_All_Records.txt")
unique(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records$measurement_value <- parse_number(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% filter(measurement_value>0&measurement_value<=5)
ECOG_NLPMeas_All_Records$measurement_value <- round(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% group_by(patid) %>% summarise(ECOG=max(measurement_value))

data.frame(New_Primary_Cancer_Box %>% inner_join(ECOG_NLPMeas_All_Records) %>%
             group_by(cancer_metastasis, Group, ECOG) %>%
  summarise(encounter_visits=mean(encounter_visits),
            medical_visits=mean(medical_visits),
            rx_visits=mean(rx_visits),
            hospital_days=mean(hospital_days),
            hospital_stays=mean(hospital_stays),
            surgery_visits=mean(surgery_visits),
            emergency_visits=mean(emergency_visits),
            encounter_providers=mean(encounter_providers),
            medical_providers=mean(medical_providers),
            rx_providers=mean(rx_providers),
            drug_supply_days=mean(drug_ingredients)))



# ------------------------------------------------------
# Number patient-months per type, metastasis, cachexia status ----------------------------------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=49) 

sum(CAN_Drug_Histories$weight) # 62121994

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
unique(PONS_Ingredients$drug_class)

string_Cancer <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                  PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                  PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                  PONS_Ingredients$drug_class == "GDF15"|
                                                                  PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(generic_name, drug_class,chemo_class) 


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat))
length(unique(CAN_Drug_Histories$patient)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, weight) %>% count()
CAN_Drug_Histories %>% ungroup() %>% summarise(n=sum(weight*n)) # 28965515 patient months

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4321825

# BY primary cancer
data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))
                                 

# BY metastasis state
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

data.frame(CAN_Drug_Histories %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(cancer_metastasis , diagnosis) %>% summarise(n=sum(weight*n)) %>% 
    spread(key=cancer_metastasis, value=n))


# BY cachexia dx/pred 
CachexiaPats_ALL_NEW <- fread("Diagnosed Population 1.0/CachexiaPats_ALL_NEW.txt")

CachexiaPats_ALL_NEW %>% left_join(New_Primary_Cancer_Box) %>% summarise(n=sum(weight))

CAN_Drug_Histories %>% inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4151912
# 664132


data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
  ungroup() %>% summarise(n=sum(weight*n))) # 4126200

data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))


Cachexi_mets <- CachexiaPats_ALL_NEW %>% inner_join(PONS_Demographics)



CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(Cachexi_mets %>% select(patid), by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=49) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat))
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat)) %>% 
  mutate(Treat=as.character(Treat)) %>%
  left_join(PONS_Ingredients %>% select(molecule, generic_name) , by=c("Treat"="molecule"))
 
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(-drug_class)) %>%
  drop_na() %>% select(-c(drug_id, drug_group, indication, Treat, generic_name)) %>% distinct()





data.frame(New_Primary_Cancer_Box %>% select(-died) %>%
  inner_join(CAN_Drug_Histories %>% group_by(patient, weight, chemo_class) %>% count() %>%
  mutate(n=n*weight), by=c("patid"="patient")) %>%
  group_by(diagnosis, chemo_class) %>% summarise(n=sum(n)) %>% spread(key=chemo_class  , value=n))




# ------------------------------------------------
# Realocate Respiratory to HEad & Neck, Split Intestnal into Large ve Small Intestine ------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))
unique(New_Primary_Cancer_Box$Primary_Cancer)

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")

Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")


New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer") %>%
  select(patid) %>% # 53553 
inner_join(
  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large")
) # 51276
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")




New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))


data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


# ------------
# Number patient-months per type, metastasis, cachexia status  % METASTATIC ----------------------------------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=49) 

sum(CAN_Drug_Histories$weight) # 62121994

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
unique(PONS_Ingredients$drug_class)

string_Cancer <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                  PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                  PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                  PONS_Ingredients$drug_class == "GDF15"|
                                                                  PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(generic_name, drug_class,chemo_class) 


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat))
length(unique(CAN_Drug_Histories$patient)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, weight) %>% count()
CAN_Drug_Histories %>% ungroup() %>% summarise(n=sum(weight*n)) # 28965515 patient months

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4321825

# BY primary cancer
data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))
                                 

# BY metastasis state
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

data.frame(CAN_Drug_Histories %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(cancer_metastasis , diagnosis) %>% summarise(n=sum(weight*n)) %>% 
    spread(key=cancer_metastasis, value=n))


# BY cachexia dx/pred 
CachexiaPats_ALL_NEW <- fread("Diagnosed Population 1.0/CachexiaPats_ALL_NEW.txt")

CachexiaPats_ALL_NEW %>% left_join(New_Primary_Cancer_Box) %>% summarise(n=sum(weight))

CAN_Drug_Histories %>% inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4151912
# 664132


data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
                          left_join(PONS_Demographics, by=c("patient"="patid")) %>%
             filter(cancer_metastasis==1) %>%
  ungroup() %>% summarise(n=sum(weight*n))) # 4126200

data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
             left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))




data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
                          left_join(PONS_Demographics, by=c("patient"="patid")) %>%
              filter(diagnosis %in% c("Lung Cancer", "Intestinal Cancer", "Prostate Cancer", "Breast Cancer", "Pancreatic Cancer")) %>%
             filter(cancer_metastasis==1) %>%
  ungroup() %>% summarise(n=sum(weight*n))) # 4126200


Cachexi_mets <- CachexiaPats_ALL_NEW %>% inner_join(PONS_Demographics)



# BY CCh Dx state
PONS_Demographics_Cachexia_Dx <- fread("Diagnosed Population 1.0/PONS Demographics.txt", stringsAsFactors = F)
PONS_Demographics_Cachexia_Dx <- PONS_Demographics_Cachexia_Dx %>% select(patid, cachexia_onset) %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))


data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(PONS_Demographics_Cachexia_Dx, by=c("patient"="patid")) %>%
             left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  ungroup() %>% group_by(cancer_metastasis) %>% summarise(n=sum(weight*n)))





CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(Cachexi_mets %>% select(patid), by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=49) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat))
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat)) %>% 
  mutate(Treat=as.character(Treat)) %>%
  left_join(PONS_Ingredients %>% select(molecule, generic_name) , by=c("Treat"="molecule"))
 
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(-drug_class)) %>%
  drop_na() %>% select(patient, weight, Month, chemo_class) %>% distinct()


New_Primary_Cancer_Box %>% select(-died) %>%
  inner_join(CAN_Drug_Histories %>% group_by(patient, weight, chemo_class) %>% count() %>%
  mutate(n=n*weight), by=c("patid"="patient")) %>%
  inner_join(PONS_Demographics) %>%
  filter(chemo_class %in% c("Platinum agent", "PD1/PDL1")) %>%
  ungroup() %>% group_by(chemo_class, cancer_metastasis) %>%
  summarise(n=sum(n)) 

data.frame(New_Primary_Cancer_Box %>% select(-died) %>%
  inner_join(CAN_Drug_Histories %>% group_by(patient, weight, chemo_class) %>% count() %>%
  mutate(n=n*weight), by=c("patid"="patient")) %>%
  group_by(diagnosis, chemo_class) %>% summarise(n=sum(n)) %>% spread(key=chemo_class  , value=n))




# ------------------------------------------------
# Number of Dx Moments per month-over-month population 10+ BMIs ------------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)

PONS_Events <- fread("PONS Events/PONS Events.txt")
PONS_Events <- PONS_Events %>% select(patid, claimed) %>% distinct() %>% mutate(claimed=as.Date(claimed))

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

PONS_Events <- PONS_Events %>% inner_join(Pats_to_track_BMI %>% select(patid))

PONS_Events <- PONS_Events %>% mutate(claimed=as.character(claimed))
PONS_Events <- PONS_Events %>% mutate(claimed=str_sub(claimed, 1L, 7L))

PONS_Events <- PONS_Events %>% group_by(patid, claimed) %>% count() %>% ungroup

length(unique(Pats_to_track_BMI$patid))

data.frame(PONS_Events %>% group_by(claimed) %>% summarise(n=sum(n))) %>%
  ggplot(aes(claimed, n)) +
  geom_line() +
  geom_point() 


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-") %>% select(patid) 

ignore <- PONS_Events %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(claimed, n) %>% count() %>% mutate(nn=nn/753743)

data.frame(ignore %>% spread(key=n, value=nn))


# -------

# NEW Cachexia Sizing -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Overall 

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)







CCh_5_years <- temp %>%  filter(CCh==1) %>% select(patid) %>% distinct()

CCh_Last_year <- temp %>% filter(Month_Min>=49) %>%  filter(CCh==1) %>% select(patid) %>% distinct()



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Pre_CCh ==1) %>% select(patid) %>% distinct()) %>%
             anti_join(CCh_5_years) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  
                                                   filter(Pre_CCh ==1) %>% select(patid) %>% distinct()) %>%
             anti_join(CCh_Last_year) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  
                                                   filter(Pre_CCh ==1) %>% select(patid) %>% distinct()) %>%
             anti_join(CCh_Last_year) %>%
                            inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)




data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>%  
                                                   filter(CCh==1) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))




Pats_BMI30_Above <- temp %>% filter(Max>30) %>% select(patid) %>% distinct()
Pats_BMI30_Below <- temp %>% select(patid) %>% distinct() %>% anti_join(Pats_BMI30_Above)

# BMI >30

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


# BMI<30

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Drop90 ==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Drop95  ==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))




# Pre cachexia 5y and 1y

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Pre_CCh==1) %>%  select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Pre_CCh==1) %>%  select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

CCh_Last_year <- temp %>% filter(Month_Min>=49) %>%  filter(CCh==1) %>% select(patid) %>% distinct()

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Pre_CCh==1) %>%  select(patid) %>% distinct()) %>%
  inner_join(CCh_Last_year) %>%
             group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(temp %>% filter(Month_Min>=49) %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  inner_join(
    temp %>% filter(Month_Min>=49) %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(First_CCh-First_Pre)))


data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  inner_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(First_CCh-First_Pre)))



PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")


data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  full_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  full_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedPre=(First_Pre-cancer_onset)) %>%
  filter(!is.na(ElapsedPre)) %>%
  filter(ElapsedPre>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedPre)))



data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  full_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
  mutate(ElapsedCCh=(First_CCh-cancer_onset))) %>%
  filter(!is.na(ElapsedCCh)) %>%
    filter(ElapsedCCh>(-12)) %>%
  ggplot(aes(ElapsedCCh)) + geom_density(size=2) +
  theme_minimal() +
  xlab("\n Number of Elapsed Months \n From Cancer Onset to 1st Cachexia Flag") +
  ylab("Patient density \n")




CCh_5_years <- temp %>%  filter(CCh==1) %>% select(patid) %>% distinct() %>% mutate(CCh="CCh")
Pre_CCh_5_years <- temp %>%  filter(Pre_CCh==1) %>% select(patid) %>% distinct() %>% mutate(Pre="Pre")


data.frame(temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  left_join(CCh_5_years %>% full_join(Pre_CCh_5_years)) %>%
  group_by(Primary_Cancer, CCh, Pre) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(Primary_Cancer) %>% mutate(Tot=sum(n)) %>%
  mutate(perc=n/Tot) %>% select(-c(n, Tot)))


CCh_1_year <- temp %>%  filter(CCh==1 & Month_Min>=49) %>% select(patid) %>% distinct() %>% mutate(CCh="CCh")
Pre_CCh_1_year <- temp %>%  filter(Pre_CCh==1 & Month_Min>=49) %>% select(patid) %>% distinct() %>% mutate(Pre="Pre")



data.frame(temp %>%  filter(Month_Min>=49) %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  left_join(CCh_1_year %>% full_join(Pre_CCh_1_year)) %>%
    left_join(temp %>% filter(Month_Min>=49) %>% filter(Recovered==1) %>% select(patid, Recovered) %>% distinct()) %>%
  group_by( CCh, Pre, Recovered) %>% summarise(n=sum(weight)) %>%
  ungroup())





temp %>% filter(Month_Max>48) %>% group_by(patid) %>% summarise(Max=max(Max)) %>%
  inner_join(temp %>% group_by(patid) %>% summarise(Min=min(Min))) %>%
  ungroup() %>% mutate(percdrop=100*(Max-Min)/Max) %>%
  mutate(Max=ifelse(Max<20, "<20",
                    ifelse(Max<22.2, "<22.5",
                           ifelse(Max<25, "<25",
                                  ifelse(Max<27.5,"<27.5", 
                                         ifelse(Max<30,"<30",
                                                ifelse(Max<32.5,"<32.5",
                                                       ifelse(Max<35,"<35",
                                                              ifelse(Max<37.5,"<37.5",
                                                                     ifelse(Max<40,"<40",
                                                                            ifelse(Max<42.5,"<42.5",
                                                                                   ifelse(Max<45,"<45",
                                                                                          ifelse(Max<47.5,"<47.5",
                                                                                                 ifelse(Max<50,"<50",">50")))))))))))))) %>%
  mutate(Min=ifelse(Min<20, "<20",
                    ifelse(Min<22.2, "<22.5",
                           ifelse(Min<25, "<25",
                                  ifelse(Min<27.5,"<27.5", 
                                         ifelse(Min<30,"<30",
                                                ifelse(Min<32.5,"<32.5",
                                                       ifelse(Min<35,"<35",
                                                              ifelse(Min<37.5,"<37.5",
                                                                     ifelse(Min<40,"<40",
                                                                            ifelse(Min<42.5,"<42.5",
                                                                                   ifelse(Min<45,"<45",
                                                                                          ifelse(Min<47.5,"<47.5",
                                                                                                 ifelse(Min<50,"<50",">50")))))))))))))) %>%
  mutate(percdrop=ifelse(percdrop<2,"<2",
                         ifelse(percdrop<5,"<5",
                                ifelse(percdrop<7.5,"<7.5",
                                       ifelse(percdrop<10,"<10",
                                              ifelse(percdrop<12.5,"<12.5",
                                                     ifelse(percdrop<15,"<15",
                                                            ifelse(percdrop<17.5,"<17.5",
                                                                   ifelse(percdrop<20,"<20",">20"))))))))) %>%
  group_by(Max, percdrop) %>% count() %>%
  spread(key=Max, value=n)
  


data.frame(temp %>% select(patid, Month_Min, CUM_CCh) %>% distinct() %>%
  group_by(patid) %>% filter(CUM_CCh==1&lag(CUM_CCh)==0) %>% ungroup() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Month_Min) %>% summarise(n=sum(weight))) %>% ungroup() %>%
  filter(Month_Min>=49) %>% summarise(tot=sum(n))



# ------------

# Number Elapsed MOnths Exactly Before Dead Metastasis ---------
metastasis_flag <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
metastasis_flag <- metastasis_flag %>%  select(patid, cancer_metastasis, died, death_date)
metastasis_flag <- metastasis_flag %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
setDT(metastasis_flag)
metastasis_flag[, death_date := as.character(death_date)][, death_date := substr(death_date, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

metastasis_flag <- metastasis_flag %>% left_join(Months_lookup, by=c("death_date" = "Month"))
metastasis_flag <- metastasis_flag %>% select(-death_date) %>% rename("death_date"="Exact_Month")



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)

metastasis_flag <- New_Primary_Cancer_Box %>% inner_join(metastasis_flag)


CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))
CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Treat=ifelse(Treat=="-",0,1))

CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month) %>% 
  group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})



CAN_Drug_Histories %>% rename("patid"="patient") %>% ungroup() %>%
  inner_join(metastasis_flag %>% filter(cancer_metastasis==1)) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))


CAN_Drug_Histories %>% rename("patid"="patient") %>% ungroup() %>%
  inner_join(metastasis_flag %>% filter(cancer_metastasis==1&died=="Y"))  %>%
  select(-c(died, cancer_metastasis)) %>%
  filter(Month<death_date) %>%
  group_by(patid) %>% filter(grp==max(grp)) %>% filter(Treat==0) %>%
  group_by(patid, weight, Primary_Cancer ) %>% count() %>% ungroup() %>%
  filter(n>=2) %>% summarise(n=sum(weight)) # 380915


# --------

# Olanzapine use -----------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight) %>% filter(Primary_Cancer!="-")


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_onset)) %>% filter(cancer_onset<="2020-07-31")
PONS_Demographics <- PONS_Demographics %>% filter(age>=18) %>% 
  select(patid, weight, age, died, cancer_metastasis, cachexia_onset) %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
PONS_Demographics <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(PONS_Demographics) 
Pats_to_track_BMI <- PONS_Demographics  %>% 
  select(patid, weight, died,  cancer_metastasis, cachexia_onset)


PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI") %>% select(-weight) %>%
  inner_join(Pats_to_track_BMI %>% select(patid, weight))
Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats) %>% arrange(patid, claimed)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 
Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), 
                                                                                   median=median(value), 
                                                                                   min=min(value), 
                                                                                   max=max(value))
PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)
Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(
    PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate), by="patid"
    ) %>% select(patid, mindate, maxdate) %>% distinct()
PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))
PONS_Measures$mindate <- as.Date(PONS_Measures$mindate) ; PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)
PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% 
  inner_join(PONS_Measures) %>% select(-weight)
Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, cancer_metastasis, cachexia_onset) %>% 
  mutate(cachexia_onset=as.Date(cachexia_onset))

temp <- fread("Diagnosed Population 1.0/All_drops.txt")
New_Cachexia_Pred <- temp %>% filter( Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
CachexiaPats_ALL_NEW <- data.frame(
  New_Cachexia_Pred %>% left_join(
    Pats_to_track_BMI
    ) %>% inner_join(
      PONS_Measures %>% select(patid) %>% distinct()
      ) %>%
    bind_rows(
      Pats_to_track_BMI %>% filter(!is.na(cachexia_onset))
      ) %>%
             distinct()
  )


Cachexia_Dx <- CachexiaPats_ALL_NEW %>% filter(!is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Dx")
Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct()%>% mutate(group="Pred")
No_Cachexia <- Pats_to_track_BMI %>% select(patid) %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")

USCAX24_Olanzpine_Doses <- fread("CAN Analysis Results 3.0/USCAX24 Olanzpine Doses.txt", sep=",")
Olanzapine_pats <- USCAX24_Olanzpine_Doses %>% select(patid) %>% distinct()

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt")


sum(New_Primary_Cancer_Box$weight) # 22984889
New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced) %>% summarise(n=sum(weight)) #9861087
New_Primary_Cancer_Box %>%  inner_join(CancerDrug_Experienced) %>% inner_join(Olanzapine_pats) %>% 
summarise(n=sum(weight)) #406370.5 (0.0412095)


Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box)  %>% inner_join(CancerDrug_Experienced) %>% summarise(n=sum(weight)) # 334148.3
Cachexia_Pred %>% inner_join(New_Primary_Cancer_Box)  %>% inner_join(CancerDrug_Experienced) %>% summarise(n=sum(weight)) # 2670655
No_Cachexia %>% inner_join(New_Primary_Cancer_Box)  %>% inner_join(CancerDrug_Experienced) %>% summarise(n=sum(weight)) # 6077572


Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box)  %>% inner_join(CancerDrug_Experienced) %>%
  inner_join(Olanzapine_pats) %>% summarise(n=sum(weight)) # 29846.54 0.08932124
Cachexia_Pred %>% inner_join(New_Primary_Cancer_Box)  %>% inner_join(CancerDrug_Experienced) %>%
  inner_join(Olanzapine_pats) %>% summarise(n=sum(weight)) # 114324.6 0.0428077
No_Cachexia %>% inner_join(New_Primary_Cancer_Box)  %>% inner_join(CancerDrug_Experienced) %>%
  inner_join(Olanzapine_pats) %>% summarise(n=sum(weight)) # 215919.4 0.03552725



Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")


First_CCh_Pred <- temp %>% filter( Drop95==1 | Drop90==1 | Drop2_20==1) %>% group_by(patid) %>% summarise(Month_Min=min(Month_Min)) %>% ungroup()


PONS_Demographics_temp <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid,cachexia_onset)
PONS_Demographics_temp$cachexia_onset <- as.Date(PONS_Demographics_temp$cachexia_onset)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cachexia_onset=format(as.Date(cachexia_onset), "%Y-%m"))
PONS_Demographics_temp <- PONS_Demographics_temp %>% drop_na() 
PONS_Demographics_temp <- PONS_Demographics_temp %>% left_join(Months_lookup, by=c("cachexia_onset"="Month")) 

USCAX24_Olanzpine_Doses <- USCAX24_Olanzpine_Doses %>% group_by(patid) %>% summarise(from_dt=min(from_dt)) %>%
  mutate(from_dt=as.Date(from_dt)) %>% mutate(from_dt=format(as.Date(from_dt), "%Y-%m")) %>% 
  left_join(Months_lookup, by=c("from_dt"="Month")) 

USCAX24_Olanzpine_Doses <- USCAX24_Olanzpine_Doses %>% drop_na()
USCAX24_Olanzpine_Doses <- USCAX24_Olanzpine_Doses %>% select(-from_dt) %>% rename("OLA"="Exact_Month")

USCAX24_Olanzpine_Doses %>% inner_join(PONS_Demographics_temp) %>%
  summarise(mean=mean(OLA-Exact_Month)) # -2.73


USCAX24_Olanzpine_Doses %>% inner_join(First_CCh_Pred) %>%
  summarise(mean=mean(OLA-Month_Min)) # 9.8


USCAX24_Olanzpine_Doses %>% inner_join(PONS_Demographics_temp) %>%
  summarise(mean=mean(OLA-Exact_Month)) # 12



USCAX24_Olanzapine_Drug_Histories <- fread("CAN Analysis Results 3.0/USCAX24 Olanzapine Drug Histories.txt", sep=",")
USCAX24_Olanzapine_Drug_Histories <- gather(USCAX24_Olanzapine_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
USCAX24_Olanzapine_Drug_Histories$Month <- as.character(USCAX24_Olanzapine_Drug_Histories$Month)
USCAX24_Olanzapine_Drug_Histories$Month <- parse_number(USCAX24_Olanzapine_Drug_Histories$Month)


USCAX24_Olanzapine_Drug_Histories %>% inner_join(PONS_Demographics_temp) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  inner_join(New_Primary_Cancer_Box) %>% inner_join(CancerDrug_Experienced) %>%
  mutate(Month=Month-Exact_Month) %>% filter(Treat=="999") %>%
  group_by(Month) %>% summarise(n=sum(weight)) %>%
  ggplot(aes(Month, n)) +
  geom_line(size=2) +
  theme_minimal() +
  xlab("\n Number of Months Relative to \n Cachexia ICD Diagnosis") +
  ylab("Popultion ON Olanzapine \n")





PONS_Demographics_temp <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid,cancer_onset)
PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_onset=format(as.Date(cancer_onset), "%Y-%m"))
PONS_Demographics_temp <- PONS_Demographics_temp %>% drop_na() 
PONS_Demographics_temp <- PONS_Demographics_temp %>% left_join(Months_lookup, by=c("cancer_onset"="Month")) 

USCAX24_Olanzpine_Doses <- USCAX24_Olanzpine_Doses %>% group_by(patid) %>% summarise(from_dt=min(from_dt)) %>%
  mutate(from_dt=as.Date(from_dt)) %>% mutate(from_dt=format(as.Date(from_dt), "%Y-%m")) %>% 
  left_join(Months_lookup, by=c("from_dt"="Month")) 


USCAX24_Olanzapine_Drug_Histories %>% inner_join(PONS_Demographics_temp) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  inner_join(New_Primary_Cancer_Box) %>% inner_join(CancerDrug_Experienced) %>%
  mutate(Month=Month-Exact_Month) %>% filter(Treat=="999") %>%
  group_by(Month) %>% summarise(n=sum(weight)) %>%
  ggplot(aes(Month, n)) +
  geom_line(size=2) +
  theme_minimal() +
  xlab("\n Number of Months Relative to \n 1st Cancer Diagnosis") +
  ylab("Popultion ON Olanzapine \n")


metastasis_flag <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
metastasis_flag <- metastasis_flag %>%  select(patid, cancer_metastasis)
metastasis_flag <- metastasis_flag %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),"Non","Mets"))


USCAX24_Olanzapine_Drug_Histories %>% inner_join(PONS_Demographics_temp) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  inner_join(New_Primary_Cancer_Box) %>% inner_join(CancerDrug_Experienced) %>%
  mutate(Month=Month-Exact_Month) %>% filter(Treat=="999") %>%
  inner_join(metastasis_flag) %>%
  group_by(Month,cancer_metastasis) %>% summarise(n=sum(weight)) %>%
  ggplot(aes(Month, n, colour=cancer_metastasis)) +
  geom_line(size=2) +
  theme_minimal() +
  xlab("\n Number of Months Relative to \n 1st Cancer Diagnosis") +
  ylab("Popultion ON Olanzapine \n")


USCAX24_Olanzapine_Drug_Histories %>% inner_join(First_CCh_Pred) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  inner_join(New_Primary_Cancer_Box) %>% inner_join(CancerDrug_Experienced) %>%
  mutate(Month=Month-Month_Min) %>% filter(Treat=="999") %>%
  group_by(Month) %>% summarise(n=sum(weight)) %>%
  ggplot(aes(Month, n)) +
  geom_line(size=2) +
  theme_minimal() +
  xlab("\n Number of Months Relative to \n 1st Cachexia Observation") +
  ylab("Popultion ON Olanzapine \n")


# ---------
# Number of Dx Cancer and Dx Cachexia------------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

PONS_Events <- fread("PONS Events/PONS Events.txt")

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
unique(PONS_Dossiers$diagnosis)


Cancer <- PONS_Dossiers %>% filter(diagnosis!="Cachexia Diagnosis"&diagnosis!="Benign Diagnosis"&diagnosis!="Benign History")

Cancer <- Cancer %>% select(code) %>% distinct() %>%
  inner_join(PONS_Events) %>% select(patid, claimed) %>% distinct()

CCh <- PONS_Dossiers %>% filter(diagnosis=="Cachexia Diagnosis")

CCh <- CCh %>% select(code) %>% distinct() %>%
  inner_join(PONS_Events) %>% select(patid, claimed) %>% distinct()

CCh %>%  inner_join(New_Primary_Cancer_Box) %>% group_by(patid) %>% count() %>% ungroup() 

data.frame(Cancer %>% inner_join(New_Primary_Cancer_Box) %>% group_by(patid) %>% count() %>% ungroup()  %>%
  mutate(n=ifelse(n>=200,200,n)) %>% group_by(n) %>% count() %>%
  mutate(nn=nn/753415))


length(unique(CCh$patid))

data.frame(CCh %>% inner_join(New_Primary_Cancer_Box) %>% group_by(patid) %>% count() %>% ungroup()  %>%
  mutate(n=ifelse(n>=200,200,n)) %>% group_by(n) %>% count() %>%
  mutate(nn=nn/20974))




# -------

# NEW Cachexia Sizing -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Overall 

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp  %>% filter(Month_Min>=49) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp  %>% filter(Month_Min>=49) %>% 
                                                  # mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 )) %>%
                                                   filter(CCh ==1) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



CCh_5_years <- temp %>%  filter(CCh==1) %>% select(patid) %>% distinct()


Pre_CCh_5_years <- temp %>%  filter(Pre_CCh==1) %>% select(patid) %>% distinct() %>% anti_join(CCh_5_years)

fwrite(Pre_CCh_5_years, "Diagnosed Population 1.0/Pre_CCh_5_years.txt")
fwrite(CCh_5_years, "Diagnosed Population 1.0/CCh_5_years.txt")

CCh_Last_year <- temp %>% filter(Month_Min>=49) %>%  filter(CCh==1) %>% select(patid) %>% distinct()



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Pre_CCh ==1) %>% select(patid) %>% distinct()) %>%
             anti_join(CCh_5_years) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  
                                                   filter(Pre_CCh ==1) %>% select(patid) %>% distinct()) %>%
             anti_join(CCh_Last_year) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  
                                                   filter(Pre_CCh ==1) %>% select(patid) %>% distinct()) %>%
             anti_join(CCh_Last_year) %>%
                            inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)




data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>%  
                                                   filter(CCh==1) %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))




Pats_BMI30_Above <- temp %>% filter(Max>30) %>% select(patid) %>% distinct()
Pats_BMI30_Below <- temp %>% select(patid) %>% distinct() %>% anti_join(Pats_BMI30_Above)

# BMI >30

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


# BMI<30

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=37) %>% filter(CCh==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Drop90 ==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Above) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Drop95  ==1) %>%  select(patid) %>% distinct()) %>%
             inner_join(Pats_BMI30_Below) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))




# Pre cachexia 5y and 1y

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Pre_CCh==1) %>%  select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%  select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Pre_CCh==1) %>%  select(patid) %>% distinct()) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

CCh_Last_year <- temp %>% filter(Month_Min>=49) %>%  filter(CCh==1) %>% select(patid) %>% distinct()

data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>% filter(Pre_CCh==1) %>%  select(patid) %>% distinct()) %>%
  inner_join(CCh_Last_year) %>%
             group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(temp %>% filter(Month_Min>=49) %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  inner_join(
    temp %>% filter(Month_Min>=49) %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(First_CCh-First_Pre)))


data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  inner_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(First_CCh-First_Pre)))



PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")


data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  full_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  full_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedPre=(First_Pre-cancer_onset)) %>%
  filter(!is.na(ElapsedPre)) %>%
  filter(ElapsedPre>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedPre)))



data.frame(temp  %>% filter(Pre_CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_Pre"="Month_Min") %>%
  full_join(
    temp  %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min")
  ) %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
  mutate(ElapsedCCh=(First_CCh-cancer_onset))) %>%
  filter(!is.na(ElapsedCCh)) %>%
    filter(ElapsedCCh>(-12)) %>%
  ggplot(aes(ElapsedCCh)) + geom_density(size=2) +
  theme_minimal() +
  xlab("\n Number of Elapsed Months \n From Cancer Onset to 1st Cachexia Flag") +
  ylab("Patient density \n")




CCh_5_years <- temp %>%  filter(CCh==1) %>% select(patid) %>% distinct() %>% mutate(CCh="CCh")
Pre_CCh_5_years <- temp %>%  filter(Pre_CCh==1) %>% select(patid) %>% distinct() %>% mutate(Pre="Pre")


data.frame(temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  left_join(CCh_5_years %>% full_join(Pre_CCh_5_years)) %>%
  group_by(Primary_Cancer, CCh, Pre) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% group_by(Primary_Cancer) %>% mutate(Tot=sum(n)) %>%
  mutate(perc=n/Tot) %>% select(-c(n, Tot)))


CCh_1_year <- temp %>%  filter(CCh==1 & Month_Min>=49) %>% select(patid) %>% distinct() %>% mutate(CCh="CCh")
Pre_CCh_1_year <- temp %>%  filter(Pre_CCh==1 & Month_Min>=49) %>% select(patid) %>% distinct() %>% mutate(Pre="Pre")



data.frame(temp %>%  filter(Month_Min>=49) %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  left_join(CCh_1_year %>% full_join(Pre_CCh_1_year)) %>%
    left_join(temp %>% filter(Month_Min>=49) %>% filter(Recovered==1) %>% select(patid, Recovered) %>% distinct()) %>%
  group_by( CCh, Pre, Recovered) %>% summarise(n=sum(weight)) %>%
  ungroup())





temp %>% filter(Month_Max>48) %>% group_by(patid) %>% summarise(Max=max(Max)) %>%
  inner_join(temp %>% group_by(patid) %>% summarise(Min=min(Min))) %>%
  ungroup() %>% mutate(percdrop=100*(Max-Min)/Max) %>%
  mutate(Max=ifelse(Max<20, "<20",
                    ifelse(Max<22.2, "<22.5",
                           ifelse(Max<25, "<25",
                                  ifelse(Max<27.5,"<27.5", 
                                         ifelse(Max<30,"<30",
                                                ifelse(Max<32.5,"<32.5",
                                                       ifelse(Max<35,"<35",
                                                              ifelse(Max<37.5,"<37.5",
                                                                     ifelse(Max<40,"<40",
                                                                            ifelse(Max<42.5,"<42.5",
                                                                                   ifelse(Max<45,"<45",
                                                                                          ifelse(Max<47.5,"<47.5",
                                                                                                 ifelse(Max<50,"<50",">50")))))))))))))) %>%
  mutate(Min=ifelse(Min<20, "<20",
                    ifelse(Min<22.2, "<22.5",
                           ifelse(Min<25, "<25",
                                  ifelse(Min<27.5,"<27.5", 
                                         ifelse(Min<30,"<30",
                                                ifelse(Min<32.5,"<32.5",
                                                       ifelse(Min<35,"<35",
                                                              ifelse(Min<37.5,"<37.5",
                                                                     ifelse(Min<40,"<40",
                                                                            ifelse(Min<42.5,"<42.5",
                                                                                   ifelse(Min<45,"<45",
                                                                                          ifelse(Min<47.5,"<47.5",
                                                                                                 ifelse(Min<50,"<50",">50")))))))))))))) %>%
  mutate(percdrop=ifelse(percdrop<2,"<2",
                         ifelse(percdrop<5,"<5",
                                ifelse(percdrop<7.5,"<7.5",
                                       ifelse(percdrop<10,"<10",
                                              ifelse(percdrop<12.5,"<12.5",
                                                     ifelse(percdrop<15,"<15",
                                                            ifelse(percdrop<17.5,"<17.5",
                                                                   ifelse(percdrop<20,"<20",">20"))))))))) %>%
  group_by(Max, percdrop) %>% count() %>%
  spread(key=Max, value=n)
  


data.frame(temp %>% select(patid, Month_Min, CUM_CCh) %>% distinct() %>%
  group_by(patid) %>% filter(CUM_CCh==1&lag(CUM_CCh)==0) %>% ungroup() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Month_Min) %>% summarise(n=sum(weight))) %>% ungroup() %>%
  filter(Month_Min>=49) %>% summarise(tot=sum(n))



# ------------

# Age distribution cachexia vs pre-cachexia ------------

Pre_CCh_5_years <- fread("Diagnosed Population 1.0/Pre_CCh_5_years.txt")
CCh_5_years <- fread("Diagnosed Population 1.0/CCh_5_years.txt")

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age)

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))
unique(New_Primary_Cancer_Box$Primary_Cancer)
PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")

Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))



New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(-c(died, Primary))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Urinary Cancer")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics %>% mutate(age=ifelse(age>=65,">65","<65")))

data.frame(New_Primary_Cancer_Box %>% inner_join(Pre_CCh_5_years) %>% group_by(Primary_Cancer,age) %>% summarise(n=sum(weight)) %>%
  spread(key=age, value=n) %>% mutate(perc=`>65`/(`>65`+`<65`)) %>% 
  mutate(belowperc=1-perc) %>% select(-c(`<65`,`>65`)) %>% arrange(perc))


data.frame(New_Primary_Cancer_Box %>% inner_join(CCh_5_years) %>% group_by(age) %>% summarise(n=sum(weight)) %>%
  spread(key=age, value=n) %>% mutate(perc=`>65`/(`>65`+`<65`)) %>% 
  mutate(belowperc=1-perc) %>% select(-c(`<65`,`>65`)) %>% arrange(perc))

# ---------
# NEW Cachexia Sizing -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)

setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("cancer_metastasis"="Exact_Month")

data.frame(PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp %>% select(patid) %>% distinct()) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))) # 3488475



PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp %>% select(patid, Month_Min, CCh) %>% distinct()) %>% 
  mutate(When=ifelse(Month_Min>=cancer_metastasis,"After", "Before")) %>%
  select(patid, weight, When, CCh) %>% distinct() %>% filter(CCh==1) %>% select(-CCh) %>%
  distinct() %>% group_by(patid, weight) %>% arrange(patid, weight, When) %>%
  mutate(When=paste0(When, collapse = ",")) %>% distinct() %>%
  ungroup() %>% group_by(When) %>% summarise(n=sum(weight)/3488475)



data.frame(PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp %>% select(patid, Month_Min, CCh) %>% distinct()) %>% 
  mutate(When=ifelse(Month_Min>=cancer_metastasis,"After", "Before")) %>%
  select(patid, weight, When, CCh) %>% distinct() %>% filter(CCh==1) %>% select(-CCh) %>%
  distinct() %>% group_by(patid, weight) %>% arrange(patid, weight, When) %>%
  mutate(When=paste0(When, collapse = ",")) %>% distinct() %>%
  ungroup() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer, When) %>% summarise(n=sum(weight)) %>%
  spread(key=When, value=n))



data.frame(PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp) %>% 
  filter(Month_Min>=cancer_metastasis & CCh==1) %>%
  group_by(patid, cancer_metastasis) %>% summarise(Month_Min=min(Month_Min)) %>%
  mutate(Elapsed=Month_Min-cancer_metastasis) %>% ungroup() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Elapsed) %>% summarise(n=sum(weight)))


ignore <- PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp) %>% 
  filter(Month_Min>=cancer_metastasis & CCh==1 & Month_Min>=49) %>%
  select(patid) %>% distinct()

data.frame(PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp) %>% 
  filter(Month_Min>=cancer_metastasis & Pre_CCh==1 & Month_Min>=49) %>%
  select(patid) %>% distinct() %>%
    anti_join(ignore) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))

 



temp %>% select(patid, Recovered) %>% distinct() %>% filter(Recovered==1) %>% select(patid) %>% distinct()

temp %>% select(patid, Month_Min, Recovered) %>% distinct() %>% arrange(patid, Month_Min) %>%
  group_by(patid) %>% filter(Recovered==0&lag(Recovered)==1) %>% select(patid) %>% distinct()


temp %>% select(patid, Month_Min, Recovered) %>% distinct() %>% arrange(patid, Month_Min) %>%
  group_by(patid) %>% filter( (Recovered==0&lag(Recovered)==1) | (Recovered==1&lead(Recovered)==0) ) %>%
  mutate(Elapsed=Month_Min-lag(Month_Min)) %>% ungroup() %>% # summarise(mean=mean(Elapsed, na.rm=T))
  ggplot(aes(Elapsed)) +
  geom_density(size=2, colour="firebrick") + xlim(0,20) +
  theme_minimal() + xlab("\n Number of Months until Relapse \n [e.g., Recovered -> Cachexia]") +
  ylab("Patient density \n")


temp %>% select(patid, Month_Min, Recovered) %>% distinct() %>% arrange(patid, Month_Min) %>%
  group_by(patid) %>% filter( (Recovered==1&lag(Recovered)==0) | (Recovered==0&lead(Recovered)==1) ) %>%
  mutate(Elapsed=Month_Min-lag(Month_Min)) %>% ungroup() %>% # summarise(mean=mean(Elapsed, na.rm=T))
  ggplot(aes(Elapsed)) +
  geom_density(size=2, colour="midnightblue") + xlim(0,20) +
  theme_minimal() + xlab("\n Number of Months until Recovery \n [e.g., Cachexia -> Recovered]") +
  ylab("Patient density \n")



temp %>% select(patid, Month_Min, Recovered) %>% distinct() %>% filter(Recovered==1) %>% select(-Recovered) %>%
  rename("Months_Rec"="Month_Min") %>%
  inner_join(temp %>% select(patid, Month_Min, CCh) %>% distinct() %>% filter(CCh==1) %>% select(-CCh) %>%
  rename("Months_CCh"="Month_Min")) %>% filter(Months_Rec>=Months_CCh) %>% mutate(Elapsed=Months_Rec-Months_CCh) %>%
  summarise(mean=mean(Elapsed))

temp %>% select(patid, Month_Min, Recovered) %>% distinct() %>% filter(Recovered==1) %>% select(-Recovered) %>%
  rename("Months_Rec"="Month_Min") %>%
  inner_join(temp %>% select(patid, Month_Min, CCh) %>% distinct() %>% filter(CCh==1) %>% select(-CCh) %>%
  rename("Months_CCh"="Month_Min")) %>% filter(Months_Rec<Months_CCh) %>% mutate(Elapsed=Months_Rec-Months_CCh) %>%
  summarise(mean=mean(Elapsed))



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")


PONS_Demographics_CCh_Dx_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_CCh_Dx_onset <- PONS_Demographics_CCh_Dx_onset %>%  select(patid, cachexia_onset)
setDT(PONS_Demographics_CCh_Dx_onset)
PONS_Demographics_CCh_Dx_onset[, cachexia_onset := as.character(cachexia_onset)][, cachexia_onset := substr(cachexia_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_CCh_Dx_onset <- PONS_Demographics_CCh_Dx_onset %>% left_join(Months_lookup, by=c("cachexia_onset" = "Month"))
PONS_Demographics_CCh_Dx_onset <- PONS_Demographics_CCh_Dx_onset %>% select(-cachexia_onset) %>% rename("cachexia_onset"="Exact_Month")

data.frame(PONS_Demographics_CCh_Dx_onset %>% inner_join(PONS_Demographics_onset) %>%
  inner_join(temp %>% select(patid) %>% distinct()) %>% drop_na() %>%
  inner_join(New_Primary_Cancer_Box) %>% group_by(Primary_Cancer) %>%
  summarise(mean=mean(cachexia_onset-cancer_onset)))



# ------------

# Drug Penetrance Ever using the drug class classification ---------------------------------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt")
CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
sum(CAN_Drug_Histories$weight)  # 9861087

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass)

CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>% summarise(n=sum(weight)) # 9861087


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
metastasis_flag <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

CAN_Drug_Histories %>% select(patid, weight) %>% distinct() %>%
  inner_join(metastasis_flag) %>%
  group_by(cancer_metastasis) %>% summarise(n=sum(weight))
  
data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(metastasis_flag) %>%
  group_by(cancer_metastasis, chemo_class) %>% summarise(n=sum(weight))) %>% 
  mutate(n=ifelse(cancer_metastasis==1,n/5571494,n/4289593)) %>%
  arrange(cancer_metastasis, desc(n)) %>%
  spread(key=cancer_metastasis, value=n)


temp <- data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(metastasis_flag) %>%
             inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer, chemo_class) %>% summarise(n=sum(weight))) %>% 
  spread(key=Primary_Cancer, value=n) 

temp <- CAN_Drug_Histories %>% select(patid, weight) %>%
  distinct() %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(metastasis_flag) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)) %>%
  spread(key=Primary_Cancer, value=n)


At_risk <- fread("Diagnosed Population 1.0/At_risk.txt")
CCh_5_years <- fread("Diagnosed Population 1.0/CCh_5_years.txt")

No_CCh <- At_risk %>% anti_join(CCh_5_years)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, cachexia_onset) %>% filter(!is.na(cachexia_onset))
CachexiaDx <- PONS_Demographics %>% select(patid)


groups <- CachexiaDx %>% mutate(Group="Dx") %>% 
  bind_rows(CCh_5_years %>% mutate(Group="Pred")) %>%
  bind_rows(No_CCh %>% mutate(Group="None")) %>%
  inner_join(New_Primary_Cancer_Box)



groups %>% inner_join(metastasis_flag)  %>% inner_join(CAN_Drug_Histories %>% select(patid) %>% distinct()) %>%
  group_by(Group, cancer_metastasis) %>% summarise(n=sum(weight))



data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% distinct() %>%
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(metastasis_flag) %>%
             inner_join(groups) %>%
  group_by(cancer_metastasis, Group, chemo_class) %>% summarise(n=sum(weight))) %>% 
  spread(key=Group, value=n) %>%
  mutate(Dx=Dx/350361, None=None/1133217, Pred=Pred/2683170)



# -----------------------------------------------------------------
# Compare Drug Usage patients with cachexia before and/or after metastasis ---------  
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)

setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("cancer_metastasis"="Exact_Month")

PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp %>% select(patid) %>% distinct()) %>% summarise(n=sum(weight)) # 3488475



PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp %>% select(patid, Month_Min, CCh) %>% distinct()) %>% 
  mutate(When=ifelse(Month_Min>=cancer_metastasis,"After", "Before")) %>%
  select(patid, weight, When, CCh) %>% distinct() %>% filter(CCh==1) %>% select(-CCh) %>%
  distinct() %>% group_by(patid, weight) %>% arrange(patid, weight, When) %>%
  mutate(When=paste0(When, collapse = ",")) %>% distinct() %>%
  ungroup() %>% group_by(When) %>% summarise(n=sum(weight)/3488475)



Before_vs_After_mets_groups <- PONS_Demographics_mets %>% drop_na() %>%
  inner_join(temp %>% select(patid, Month_Min, CCh) %>% distinct()) %>% 
  mutate(When=ifelse(Month_Min>=cancer_metastasis,"After", "Before")) %>%
  select(patid, weight, When, CCh) %>% distinct() %>% filter(CCh==1) %>% select(-CCh) %>%
  distinct() %>% group_by(patid, weight) %>% arrange(patid, weight, When) %>%
  mutate(When=paste0(When, collapse = ",")) %>% distinct() %>%
  ungroup()


CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt")
CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
sum(CAN_Drug_Histories$weight)  # 9861087
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(Before_vs_After_mets_groups %>% select(patid))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass)

CAN_Drug_Histories %>% select(patid, weight) %>% distinct() %>%
  inner_join(Before_vs_After_mets_groups) %>%
  group_by(When) %>% summarise(n=sum(weight))
  
data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(Before_vs_After_mets_groups) %>%
  group_by(When, chemo_class) %>% summarise(n=sum(weight))) %>% 
  mutate(n=ifelse(When=="After",n/967727,
                  ifelse(When=="Before",n/250187,n/533279))) %>%
  arrange(When, desc(n)) %>%
  spread(key=When, value=n)


# ------------------

# Cachexia Boxes over time with refractory  -----------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)


length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 


temp %>% filter(Recovered==1) %>% select(patid) %>% distinct()



CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")
CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% select(patid, fst_dt) %>% distinct()
CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% mutate(fst_dt=as.character(fst_dt))
CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% mutate(fst_dt=str_sub(fst_dt, 1L, 7L))

CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% left_join(Months_lookup, by=c("fst_dt"="Month")) %>%
  select(patid, Exact_Month) %>% distinct() %>% mutate(Refract=1)
names(CaxPts_ICD10_Z515)[2] <- "Month_Min"


summary_figs <- data.frame(temp %>% left_join(CaxPts_ICD10_Z515) %>%
                             mutate(Refract=ifelse(is.na(Refract),0,Refract)) %>%
                             group_by(patid) %>% mutate(Refract=ifelse(CUM_CCh==0,0,Refract)) %>%
                             mutate(Refract=cumsum(Refract)) %>% mutate(Refract=ifelse(Refract==0,0,1)) %>%
  select(patid, Month_Min, Min, CCh, CUM_CCh, Pre_CCh, Recovered, Delta, Refract) %>% distinct() %>%
  group_by(patid, Month_Min) %>%
  mutate(CCh=max(CCh), CUM_CCh=max(CUM_CCh), Pre_CCh=max(Pre_CCh), Recovered=max(Recovered), Delta=max(Delta), Refract=max(Refract)) %>%
  ungroup()) %>% group_by(patid) %>%
  mutate(Group=ifelse(CUM_CCh==1&Refract==1,"Refract",
                      ifelse(CCh==1&Recovered==0,paste0("CCh_", Delta), 
                      ifelse(CUM_CCh==1&Recovered==1, "Recovered",
                        ifelse(Pre_CCh==1&CUM_CCh==0, "Pre_CCh", 
                             ifelse(Pre_CCh==1&CUM_CCh==1, paste0("CCh_", Delta),
                             ifelse(Recovered==1, "Recovered", 
                                    ifelse(CUM_CCh==1, "Post_cachexia", "Never_cachexia")))))))) %>%
    select(patid, Month_Min, Min, Group) %>% distinct() %>% ungroup() %>%
  group_by(Month_Min, Group) %>% count()
  



summary_figs %>% ungroup() %>% group_by(Month_Min) %>% mutate(tot=sum(n)) %>% ungroup() %>%
  mutate(perc=n/tot) %>%
  mutate(Group=ifelse(Group=="CCh_-1", "Cachexia_Decreasing",
                      ifelse(Group=="CCh_0", "Cachexia_Stable",
                             ifelse(Group=="CCh_1", "Cachexia_Increasing", 
                                    ifelse(Group=="Pre_CCh", "Pre-cachexia", Group))))) %>%
  ggplot(aes(Month_Min, 100*perc, colour=Group, fill=Group)) +
  geom_smooth(size=1.5, alpha=0.3, se=F) +
  theme_minimal() +
  xlab("\n Exact Month") + ylab("Patient Proportion (%) \n") +
  scale_colour_manual(values = c("#b4bfeb","#586dbb", "#0e226b","#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315", "black")) +
  scale_fill_manual(values = c("#b4bfeb","#586dbb", "#0e226b", "#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315", "black"))




# -----------------


# NEW Simi Framework Oxt 31 -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Overall 

data.frame(New_Primary_Cancer_Box %>% inner_join(temp  %>% select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(Drop2_20 ==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")



data.frame(temp %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh))) %>%
  spread(key=cancer_metastasis, value=mean)


# Cachexia Exp vs Naive 

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced$exp <- 1




data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
             left_join(CancerDrug_Experienced) %>%
  group_by(exp, cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct()) %>%
                          left_join(CancerDrug_Experienced) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(exp, cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(Drop90_6m==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
                                       left_join(CancerDrug_Experienced) %>%
             group_by(exp, cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)






# Incidence

data.frame(temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis,Primary_Cancer) %>% summarise(tot=sum(weight))) 

Incidence <- temp %>% filter(CCh==1) %>% select(patid, Month_Min) %>% distinct() %>% group_by(patid) %>% 
  summarise(Month_Min=min(Month_Min)) %>% ungroup()

ignore <- data.frame(Incidence %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
    inner_join(PONS_Demographics_onset %>% drop_na()) %>%
   # mutate(Month_Min=Month_Min-cancer_onset) %>%
  group_by(cancer_metastasis,Primary_Cancer, Month_Min) %>% summarise(n=sum(weight))  %>%
  group_by(cancer_metastasis,Primary_Cancer)  ) %>%
  left_join(
    data.frame(temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis,Primary_Cancer) %>% summarise(tot=sum(weight))) 
  )


ignore <- ignore %>% select(-dx) %>%
  left_join(PONS_Demographics_onset %>% inner_join(PONS_Demographics_mets) %>%
              inner_join(temp %>% select(patid) %>% distinct()) %>%
  filter(!is.na(cancer_onset)) %>%
  inner_join(New_Primary_Cancer_Box) %>% rename("Month_Min"="cancer_onset") %>%
  group_by(cancer_metastasis, Primary_Cancer, Month_Min) %>% summarise(dx=sum(weight)))




fwrite(ignore, "New_CCh_per_month.csv")



ignore <- ignore %>% group_by(cancer_metastasis, Primary_Cancer) %>% 
  mutate(perc=n/tot) %>%
   mutate(roll_sum = zoo::rollapplyr(perc, 12, sum, partial=TRUE))




ignore <- ignore %>% group_by(cancer_metastasis, Primary_Cancer) %>% mutate(cum=cumsum(n))




# Persistency

CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, Treat) %>% count() %>% ungroup() %>%
  left_join(PONS_Ingredients %>% select(molecule, generic_name), by=c("Treat"="molecule")) %>% 
  select(-Treat)

unique(CAN_Drug_Histories$generic_name)

data.frame(CAN_Drug_Histories %>%
             inner_join(temp %>% filter(CCh==1) %>% select(patid) %>% distinct(), by=c("patient"="patid")) %>%
  inner_join(PONS_Demographics_mets, by=c("patient"="patid")) %>%
  inner_join(New_Primary_Cancer_Box, by=c("patient"="patid")) %>%
  group_by(cancer_metastasis, Primary_Cancer, generic_name) %>% summarise(n=mean(n)) %>%
  spread(key=generic_name, value=n))


data.frame(CAN_Drug_Histories %>%
             inner_join(temp %>% filter(CCh==1) %>% select(patid) %>% distinct(), by=c("patient"="patid")) %>%
  inner_join(PONS_Demographics_mets, by=c("patient"="patid")) %>%
  inner_join(New_Primary_Cancer_Box, by=c("patient"="patid")) %>%
  group_by(cancer_metastasis, generic_name) %>% summarise(n=mean(n)) %>%
  spread(key=generic_name, value=n))


data.frame(CAN_Drug_Histories %>% group_by(patient) %>% summarise(n=sum(n)) %>%
             inner_join(temp %>% filter(CCh==1) %>% select(patid) %>% distinct(), by=c("patient"="patid")) %>%
  inner_join(PONS_Demographics_mets, by=c("patient"="patid")) %>%
  inner_join(New_Primary_Cancer_Box, by=c("patient"="patid")) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=mean(n)))


# Metastatic Remission vs Denovo cachexia timing 

PONS_Demographics_mets_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets_onset <- PONS_Demographics_mets_onset %>%  select(patid, cancer_metastasis)
setDT(PONS_Demographics_mets_onset)
PONS_Demographics_mets_onset[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets_onset <- PONS_Demographics_mets_onset %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets_onset <- PONS_Demographics_mets_onset %>% select(-cancer_metastasis) %>% rename("cancer_metastasis"="Exact_Month")

PONS_Demographics_mets_onset %>% inner_join(PONS_Demographics_onset) %>%
  drop_na() %>% mutate(Denovo=ifelse(cancer_metastasis==cancer_onset,1,0)) %>%
  group_by(Denovo) %>% count()


Denovo <- PONS_Demographics_mets_onset %>% inner_join(PONS_Demographics_onset) %>%
  drop_na() %>% mutate(Denovo=ifelse(cancer_metastasis==cancer_onset,1,0)) %>%
  select(patid, Denovo)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>%  select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
             inner_join(Denovo) %>%
  group_by(Denovo, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=Denovo, value=n)



data.frame(PONS_Demographics_mets_onset %>% drop_na() %>%
  inner_join(temp %>% select(patid, Month_Min, CCh) %>% distinct() %>% group_by(patid, Month_Min) %>% summarise(CCh=max(CCh)) %>% ungroup()) %>% 
  mutate(When=ifelse(Month_Min>=cancer_metastasis,"After", "Before")) %>%
  select(patid, When, CCh) %>% distinct() %>% filter(CCh==1) %>% select(-CCh) %>%
  distinct() %>% group_by(patid) %>% arrange(patid, When) %>%
  mutate(When=paste0(When, collapse = ",")) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
               inner_join(Denovo) %>%
  ungroup() %>% group_by(Denovo, Primary_Cancer, When) %>% summarise(n=sum(weight)) %>%
  spread(key=When, value=n))


data.frame(PONS_Demographics_mets_onset %>% drop_na() %>%
  inner_join(temp) %>% 
  filter(Month_Min>=cancer_metastasis & CCh==1) %>%
  group_by(patid, cancer_metastasis) %>% summarise(Month_Min=min(Month_Min)) %>%
  mutate(Elapsed=Month_Min-cancer_metastasis) %>% ungroup() %>%
  inner_join(New_Primary_Cancer_Box) %>% 
    mutate(Elapsed=ifelse(Elapsed<12,1,
                          ifelse(Elapsed<24,2,
                                 ifelse(Elapsed<36,3,
                                        ifelse(Elapsed<48,4,5))))) %>%
    inner_join(Denovo) %>%
  group_by(Denovo, Primary_Cancer, Elapsed) %>% summarise(n=sum(weight))) %>%
  spread(key=Elapsed, value=n)


# BMI breakdown

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")


CCh_pats <- data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% filter(Month_Min>=49) %>%
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct())) 

PONS_Measures <- PONS_Measures %>% inner_join(CCh_pats %>% select(patid))

PONS_Measures <- PONS_Measures %>% mutate(claimed=as.Date(claimed)) %>%
  group_by(patid) %>%  filter(claimed==max(claimed)) %>%
  filter(value==max(value)) %>% select(patid, value) %>% distinct() %>%
  mutate(value=ifelse(value<20,"<20",
                      ifelse(value<25,"<25",
                             ifelse(value<30,"<30",">30"))))


data.frame(PONS_Measures %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer, value) %>% summarise(n=sum(weight)) %>%
  spread(key=value, value=n))



data.frame(PONS_Measures %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>% filter(cancer_metastasis==1) %>%
    inner_join(Denovo) %>%
  group_by(Denovo, Primary_Cancer, value) %>% summarise(n=sum(weight)) %>%
  spread(key=value, value=n))


data.frame(PONS_Measures %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>% filter(cancer_metastasis==1) %>%
    inner_join(Denovo) %>%
  group_by(Denovo, Primary_Cancer) %>% summarise(n=sum(weight)) %>%
    spread(key=Denovo, value=n))

# ECOG breakdown

ECOG_NLPMeas_All_Records <- fread("Diagnosed Population 1.0/ECOG_NLPMeas_All_Records.txt")
unique(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records$measurement_value <- parse_number(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% filter(measurement_value>0&measurement_value<=5)
ECOG_NLPMeas_All_Records$measurement_value <- round(ECOG_NLPMeas_All_Records$measurement_value)
ECOG_NLPMeas_All_Records <- ECOG_NLPMeas_All_Records %>% group_by(patid) %>% summarise(ECOG=max(measurement_value))



CCh_pats <- data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct())) 


data.frame(ECOG_NLPMeas_All_Records %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
    inner_join(CCh_pats) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight)) %>%
  spread(key=cancer_metastasis, value=n))


data.frame(ECOG_NLPMeas_All_Records %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
    inner_join(CCh_pats) %>%
    mutate(ECOG=ifelse(ECOG==5,4,ECOG)) %>%
  group_by(cancer_metastasis, Primary_Cancer, ECOG) %>% summarise(n=sum(weight)) %>%
  spread(key=ECOG, value=n))


data.frame(ECOG_NLPMeas_All_Records %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>% filter(cancer_metastasis==1) %>%
    inner_join(CCh_pats) %>%
    inner_join(Denovo) %>%
  group_by(Denovo, Primary_Cancer) %>% summarise(n=sum(weight)) %>%
  spread(key=Denovo, value=n))



data.frame(ECOG_NLPMeas_All_Records %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>% filter(cancer_metastasis==1) %>%
    inner_join(Denovo) %>%
        inner_join(CCh_pats) %>%
        mutate(ECOG=ifelse(ECOG==5,4,ECOG)) %>%
  group_by(Denovo, Primary_Cancer, ECOG) %>% summarise(n=sum(weight)) %>%
  spread(key=ECOG, value=n))


# By line of therapy

Oncology_LinesOfTherapy <- fread("Diagnosed Population 1.0/Oncology_LinesOfTherapy.txt")

Oncology_LinesOfTherapy %>% group_by(patid) %>% summarise(lot=max(lot)) %>%
  ungroup() %>% group_by(lot) %>% count()


Oncology_LinesOfTherapy <- Oncology_LinesOfTherapy %>% group_by(patid) %>% summarise(lot=max(lot)) %>%
  mutate(lot=ifelse(lot>=4,4,lot)) %>% ungroup()



data.frame(Oncology_LinesOfTherapy %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  inner_join(temp %>% select(patid) %>% distinct()) %>%
    group_by(cancer_metastasis, Primary_Cancer, lot) %>% summarise(n=sum(weight)) %>%
  spread(key=lot, value=n))


data.frame(Oncology_LinesOfTherapy %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
    inner_join(CCh_pats) %>%
  group_by(cancer_metastasis, Primary_Cancer, lot) %>% summarise(n=sum(weight)) %>%
  spread(key=lot, value=n))


data.frame(Oncology_LinesOfTherapy %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
    inner_join(CCh_pats) %>%
  group_by(cancer_metastasis, Primary_Cancer, lot) %>% summarise(n=sum(weight)) %>%
  spread(key=lot, value=n))




data.frame(Oncology_LinesOfTherapy %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>% filter(cancer_metastasis==1) %>%
    inner_join(CCh_pats) %>%
    inner_join(Denovo) %>%
  group_by(Denovo, Primary_Cancer) %>% summarise(n=sum(weight)) %>%
  spread(key=Denovo, value=n))



data.frame(Oncology_LinesOfTherapy %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>% filter(cancer_metastasis==1) %>%
    inner_join(Denovo) %>%
        inner_join(CCh_pats) %>%
  group_by(Denovo, Primary_Cancer, lot) %>% summarise(n=sum(weight)) %>%
  spread(key=lot, value=n))




data.frame(Oncology_LinesOfTherapy %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(Denovo) %>%
  inner_join(temp %>% select(patid) %>% distinct()) %>%
    group_by(Primary_Cancer, Denovo, lot) %>% summarise(n=sum(weight)) %>%
  spread(key=lot, value=n)) %>%
  arrange(Denovo, Primary_Cancer)
  


data.frame(Oncology_LinesOfTherapy %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(Denovo) %>%
    inner_join(CCh_pats) %>%
  group_by( Primary_Cancer, Denovo, lot) %>% summarise(n=sum(weight)) %>%
  spread(key=lot, value=n)) %>%
  arrange(Denovo, Primary_Cancer)
  


# ------------

# Refractory -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")





data.frame(temp %>% inner_join(PONS_Demographics_onset %>% drop_na())  %>%
  filter(Month_Min>=cancer_onset & Month_Min<=(cancer_onset+12)) %>%
  inner_join(PONS_Demographics_mets) %>%
   filter(CCh==1) %>% 
   select(patid, cancer_metastasis) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight))) %>%
  left_join(
    data.frame(temp %>% inner_join(PONS_Demographics_onset %>% drop_na())  %>%
  filter(Month_Min>=cancer_onset & Month_Min<=(cancer_onset+12)) %>%
  inner_join(PONS_Demographics_mets) %>%
   #filter(CCh==1) %>% 
   select(patid, cancer_metastasis) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(nn=sum(weight)))
  ) %>% mutate(perc=n/nn) %>% select(-c(n,nn))
  


CCh_pats <- data.frame(New_Primary_Cancer_Box %>% 
                         inner_join(temp %>% filter(CCh==1) %>% select(patid) %>% distinct())) 


Weight_Refract_Criteria <- temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()



CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")
CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()


data.frame(CCh_pats %>% 
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))





data.frame(CCh_pats %>% inner_join(Weight_Refract_Criteria) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


data.frame(CCh_pats %>% inner_join(CaxPts_ICD10_Z515) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))



CAN_Doses <- fread("CAN Analysis Results 3.0/CAN Doses.txt")
CAN_Doses <- temp %>% select(patid) %>% distinct() %>% left_join(CAN_Doses, by=c("patid"="pat_id"))
CAN_Doses <- CAN_Doses %>% select(patid, prov, specialty) %>% distinct()

PONS_Event_Claims_Providers <- fread("PONS Events/PONS Event Claims Providers.txt")
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers %>% select(prov, specialty_classification)
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers%>% filter(specialty_classification=="Palliative Medicine") %>% select(prov) %>% distinct()

Paliat_Rxs <-  PONS_Event_Claims_Providers %>% inner_join(CAN_Doses) %>% select(patid) %>% distinct()

data.frame(CCh_pats %>% inner_join(Paliat_Rxs) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))



data.frame(CCh_pats %>% 
             inner_join(Paliat_Rxs %>% 
                          full_join(CaxPts_ICD10_Z515) %>% 
                          full_join(
                            temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()
                          )
  ) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


PONS_Demographics_dead <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_dead <- PONS_Demographics_dead %>% filter(died=="Y")
PONS_Demographics_dead <- PONS_Demographics_dead %>% select(patid, death_date)
setDT(PONS_Demographics_dead)
PONS_Demographics_dead[, death_date := as.character(death_date)][, death_date := substr(death_date, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_dead <- PONS_Demographics_dead %>% left_join(Months_lookup, by=c("death_date" = "Month"))
PONS_Demographics_dead <- PONS_Demographics_dead %>% select(-death_date) %>% rename("death_date"="Exact_Month")


data.frame(CCh_pats %>% inner_join(PONS_Demographics_dead) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CCh_pats %>% select(patid) %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Lapsed_before_dead <- CAN_Drug_Histories %>% filter(Drugs=="355") %>% group_by(patid) %>% summarise(first=min(Month)) %>%
  inner_join(CAN_Drug_Histories) %>% filter(Month==first-1) %>% filter(Drugs=="-") %>% select(patid) %>% distinct()


data.frame(CCh_pats %>% inner_join(Lapsed_before_dead) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))





data.frame(CCh_pats %>% 
             inner_join(Paliat_Rxs %>% 
                          full_join(CaxPts_ICD10_Z515) %>% 
                          full_join(
                            temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()
                          ) %>%
    full_join(PONS_Demographics_dead %>% select(patid) %>% distinct())
  ) %>%
    inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


# ------------

# Dx 1Year 5years 1year after onset  -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")


# Mets Dx 

PONS_Demographics_mets_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets_onset <- PONS_Demographics_mets_onset %>%  select(patid, cancer_metastasis)
setDT(PONS_Demographics_mets_onset)
PONS_Demographics_mets_onset[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets_onset <- PONS_Demographics_mets_onset %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets_onset <- PONS_Demographics_mets_onset %>% select(-cancer_metastasis) %>% rename("cancer_metastasis"="Exact_Month")




#  Cachexia Dx

PONS_Demographics_cch_dx <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_cch_dx <- PONS_Demographics_cch_dx %>%  select(patid, cachexia_onset)
setDT(PONS_Demographics_cch_dx)
PONS_Demographics_cch_dx[, cachexia_onset := as.character(cachexia_onset)][, cachexia_onset := substr(cachexia_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_cch_dx <- PONS_Demographics_cch_dx %>% left_join(Months_lookup, by=c("cachexia_onset" = "Month"))
PONS_Demographics_cch_dx <- PONS_Demographics_cch_dx %>% select(-cachexia_onset) %>% rename("cachexia_onset"="Exact_Month")
PONS_Demographics_cch_dx <- PONS_Demographics_cch_dx %>% drop_na()


temp <- temp %>% select(patid, Month_Max, Month_Min, Min, Max, CCh)


Pats_BMI30_Above_mets <-  temp %>% inner_join(PONS_Demographics_mets_onset %>% drop_na()) %>% mutate(elapsed=abs(Month_Max-cancer_metastasis)) %>%
  group_by(patid) %>% filter(elapsed==min(elapsed)) %>% filter(Max==max(Max)) %>%
  select(patid, Max) %>% distinct() %>% filter(Max>30) %>% ungroup() %>% select(patid) %>% distinct()

Pats_BMI30_Below_mets <- temp %>% inner_join(PONS_Demographics_mets_onset %>% drop_na()) %>% mutate(elapsed=abs(Month_Max-cancer_metastasis)) %>%
  group_by(patid) %>% filter(elapsed==min(elapsed)) %>% filter(Max==max(Max)) %>%
  select(patid, Max) %>% distinct() %>% anti_join(Pats_BMI30_Above_mets) %>% ungroup() %>% select(patid) %>% distinct()


Pats_BMI30_Above_non_mets <-  temp %>% inner_join(PONS_Demographics_mets %>% filter(cancer_metastasis==0) %>% select(patid)) %>%
  inner_join(PONS_Demographics_onset %>% drop_na()) %>% mutate(elapsed=abs(Month_Max-cancer_onset)) %>%
  group_by(patid) %>% filter(elapsed==min(elapsed)) %>% filter(Max==max(Max)) %>%
  select(patid, Max) %>% distinct() %>% filter(Max>30) %>% ungroup() %>% select(patid) %>% distinct()

Pats_BMI30_Below__non_mets <- temp %>% inner_join(PONS_Demographics_mets %>% filter(cancer_metastasis==0) %>% select(patid)) %>%
  inner_join(PONS_Demographics_onset %>% drop_na()) %>% mutate(elapsed=abs(Month_Max-cancer_onset)) %>%
  group_by(patid) %>% filter(elapsed==min(elapsed)) %>% filter(Max==max(Max)) %>%
  select(patid, Max) %>% distinct() %>% anti_join(Pats_BMI30_Above_mets) %>% ungroup() %>% select(patid) %>% distinct()


BMI_groups <- Pats_BMI30_Above_mets %>% mutate(Above30="Above30") %>%
  bind_rows(Pats_BMI30_Below_mets %>% mutate(Above30="Below30")) %>%
  bind_rows(Pats_BMI30_Above_non_mets %>% mutate(Above30="Above30")) %>%
  bind_rows(Pats_BMI30_Below__non_mets %>% mutate(Above30="Below30"))


BMI_groups <- BMI_groups %>% filter(Above30=="Above30")  %>% distinct() %>%
  bind_rows(BMI_groups %>% filter(Above30=="Below30") %>% distinct() %>% 
  anti_join(BMI_groups %>% filter(Above30=="Above30") %>% select(patid) %>% distinct()))

BMI_groups <- BMI_groups %>% distinct()

CCh_pats <- data.frame(New_Primary_Cancer_Box %>% ungroup() %>%
                         inner_join(temp %>% filter(CCh==1) %>% select(patid, Month_Min) %>% distinct())) 



CCh_pats <- data.frame(New_Primary_Cancer_Box %>% ungroup() %>%
                         inner_join(PONS_Demographics_cch_dx %>%  select(patid, cachexia_onset) %>% distinct())) 



data.frame(temp %>% select(patid) %>% distinct() %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
    inner_join(BMI_groups) %>%
    group_by(Above30, cancer_metastasis, Primary_Cancer ) %>%
  summarise(n=sum(weight)) %>% spread(key=cancer_metastasis, value=n))



data.frame(temp %>% select(patid) %>% distinct() %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer ) %>%
    inner_join(CCh_pats) %>%
  summarise(n=sum(weight)) %>% spread(key=cancer_metastasis, value=n))



data.frame(temp %>% select(patid) %>% distinct() %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
     inner_join(BMI_groups) %>%
    group_by(Above30, cancer_metastasis, Primary_Cancer ) %>%
    inner_join(CCh_pats) %>%
  summarise(n=sum(weight)) %>% spread(key=cancer_metastasis, value=n))


data.frame(temp %>% select(patid) %>% distinct() %>% inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
                          inner_join(BMI_groups) %>%
    group_by(Above30, cancer_metastasis, Primary_Cancer ) %>%
    inner_join(CCh_pats %>% 
                 inner_join(PONS_Demographics_onset) %>% drop_na() %>%
                 filter(cachexia_onset>=cancer_onset&cachexia_onset<=(cancer_onset+12) ) %>%
                 select(patid) %>% distinct()
               ) %>%
  summarise(n=sum(weight)) %>% spread(key=cancer_metastasis, value=n))







# ------------

# Time onset to cachexia pred or dx metastatic only and top 5 ---------
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Overall 

data.frame(New_Primary_Cancer_Box %>% inner_join(temp  %>% select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(Drop2_20 ==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")



data.frame(temp %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh))) %>%
  spread(key=cancer_metastasis, value=mean)




# Time to Cachexia Dx

PONS_Demographics_cachexia_dx <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>%  select(patid, cachexia_onset) %>% drop_na()
setDT(PONS_Demographics_cachexia_dx)
PONS_Demographics_cachexia_dx[, cachexia_onset := as.character(cachexia_onset)][, cachexia_onset := substr(cachexia_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% left_join(Months_lookup, by=c("cachexia_onset" = "Month"))
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% select(-cachexia_onset) %>% rename("cachexia_onset"="Exact_Month")



data.frame(PONS_Demographics_cachexia_dx %>%
  rename("First_CCh"="cachexia_onset") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)) %>%
  spread(key=cancer_metastasis, value=mean))


# -----------
# Create NEW Pre-cachexia, Cachexia, Recovered Groups dynamically 5 years after mets only new Simi  -----------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)


length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 


temp %>% filter(Recovered==1) %>% select(patid) %>% distinct()


data.frame(temp %>% filter(patid=="PT078228913") %>%
  select(Month_Min, Min, CCh, CUM_CCh, Pre_CCh, Recovered, Delta) %>% distinct() %>%
  group_by(Month_Min) %>%
  mutate(CCh=max(CCh), CUM_CCh=max(CUM_CCh), Pre_CCh=max(Pre_CCh), Recovered=max(Recovered), Delta=max(Delta)) %>%
  ungroup()) %>%
  left_join(
    temp %>% filter(patid=="PT078228913") %>% select(Month_Max,Max) %>% distinct(), by=c("Month_Min"="Month_Max") 
  ) %>%
  mutate(Group=ifelse(CCh==1&Recovered==0,paste0("CCh_", Delta), 
                      ifelse(CUM_CCh==1&Recovered==1, "Recovered",
                        ifelse(Pre_CCh==1&CUM_CCh==0, "Pre_CCh", 
                             ifelse(Pre_CCh==1&CUM_CCh==1, paste0("CCh_", Delta),
                             ifelse(Recovered==1, "Recovered", 
                                    ifelse(CUM_CCh==1, "Post_cachexia", "Never_cachexia"))))))) %>%
    mutate(Group=ifelse(Group=="CCh_-1", "Cachexia_Decreasing",
                      ifelse(Group=="CCh_0", "Cachexia_Stable",
                             ifelse(Group=="CCh_1", "Cachexia_Increasing", 
                                    ifelse(Group=="Pre_CCh", "Pre-cachexia", Group))))) %>%
    select(Month_Min, Min, Group, Max) %>% distinct() %>%
  ggplot(aes(Month_Min, fill=Group, colour=Group)) +
  geom_segment(aes(x = Month_Min, xend = Month_Min, y = Min, yend = Max), size = 4.0, alpha=0.9) +
 # geom_point(aes(x = Month_Min, y = Min), size = 3) +
  scale_fill_manual(values = c( "#b4bfeb", "#586dbb", "#0e226b",  "#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315")) +
  scale_colour_manual(values = c( "#b4bfeb", "#586dbb", "#0e226b", "#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315")) +
  theme_minimal() +
  #ylim(24,31) +
  xlab("\n Exact Observed BMI Month") + ylab("Minimum BMI Observed \n ON that Month \n")

  
 
#  "#b4bfeb",





# PreCCh if already CCh -> CCh
# CC if None/PreCCh -> CCh Inc/Dec/Maintain


summary_figs <- data.frame(temp %>% group_by(patid) %>%
  select(patid, Month_Min, Min, CCh, CUM_CCh, Pre_CCh, Recovered, Delta) %>% distinct() %>%
  group_by(patid, Month_Min) %>%
  mutate(CCh=max(CCh), CUM_CCh=max(CUM_CCh), Pre_CCh=max(Pre_CCh), Recovered=max(Recovered), Delta=max(Delta)) %>%
  ungroup()) %>% group_by(patid) %>%
  mutate(Group=ifelse(CCh==1&Recovered==0,paste0("CCh_", Delta), 
                      ifelse(CUM_CCh==1&Recovered==1, "Recovered",
                        ifelse(Pre_CCh==1&CUM_CCh==0, "Pre_CCh", 
                             ifelse(Pre_CCh==1&CUM_CCh==1, paste0("CCh_", Delta),
                             ifelse(Recovered==1, "Recovered", 
                                    ifelse(CUM_CCh==1, "Post_cachexia", "Never_cachexia"))))))) %>%
    select(patid, Month_Min, Min, Group) %>% distinct() %>% ungroup() %>%
  group_by(Month_Min, Group) %>% count()
  



summary_figs %>% ungroup() %>% group_by(Month_Min) %>% mutate(tot=sum(n)) %>% ungroup() %>%
  mutate(perc=n/tot) %>%
  mutate(Group=ifelse(Group=="CCh_-1", "Cachexia_Decreasing",
                      ifelse(Group=="CCh_0", "Cachexia_Stable",
                             ifelse(Group=="CCh_1", "Cachexia_Increasing", 
                                    ifelse(Group=="Pre_CCh", "Pre-cachexia", Group))))) %>%
  ggplot(aes(Month_Min, 100*perc, colour=Group, fill=Group)) +
  geom_smooth(size=2, alpha=0.3, se=F) +
  theme_minimal() +
  xlab("\n Exact Month") + ylab("Patient Proportion (%) \n") +
  scale_colour_manual(values = c("#b4bfeb","#586dbb", "#0e226b","#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315")) +
  scale_fill_manual(values = c("#b4bfeb","#586dbb", "#0e226b", "#aaaaaa",   "#eb8f8f", "#d8d83f", "#af2315"))



# Time on each group
# Distribution of BMI
# Per cancer type


Monthly_groups <- data.frame(temp %>% group_by(patid) %>%
  select(patid, Month_Min, Min, CCh, CUM_CCh, Pre_CCh, Recovered, Delta) %>% distinct() %>%
  group_by(patid, Month_Min) %>%
  mutate(CCh=max(CCh), CUM_CCh=max(CUM_CCh), Pre_CCh=max(Pre_CCh), Recovered=max(Recovered), Delta=max(Delta)) %>%
  ungroup()) %>% group_by(patid) %>%
  mutate(Group=ifelse(CCh==1&Recovered==0,paste0("CCh_", Delta), 
                      ifelse(CUM_CCh==1&Recovered==1, "Recovered",
                        ifelse(Pre_CCh==1&CUM_CCh==0, "Pre_CCh", 
                             ifelse(Pre_CCh==1&CUM_CCh==1, paste0("CCh_", Delta),
                             ifelse(Recovered==1, "Recovered", 
                                    ifelse(CUM_CCh==1, "Post_cachexia", "Never_cachexia"))))))) %>%
    select(patid, Month_Min, Min, Group) %>% distinct() %>% ungroup() 






data.frame(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>%
  inner_join(Monthly_groups %>% mutate(Group=ifelse(grepl("CCh_", Group), "CCh", Group))) %>%
  group_by(patid, Primary_Cancer, Group) %>% count() %>% ungroup() %>%
  group_by(Primary_Cancer) %>%
   mutate(Tot=sum(n)) %>% ungroup() %>%
  group_by(Primary_Cancer, Group) %>% mutate(Group_Tot=sum(n)) %>% 
  select(Primary_Cancer, Group, Tot, Group_Tot) %>% distinct() %>%
  mutate(perc=Group_Tot/Tot) %>% ungroup() %>%
  select(Primary_Cancer, Group, perc) %>% distinct() %>%
  spread(key=Group, value=perc)) %>% arrange(CCh)
  


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis) %>% drop_na()
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("cancer_metastasis"="Exact_Month")


data.frame(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>%
  inner_join(Monthly_groups %>% mutate(Group=ifelse(grepl("CCh_", Group), "CCh", Group)) %>%
               inner_join(PONS_Demographics) %>% filter(Month_Min>=cancer_metastasis)) %>%
  group_by(patid, Primary_Cancer, Group) %>% count() %>% ungroup() %>%
  group_by(Primary_Cancer) %>%
   mutate(Tot=sum(n)) %>% ungroup() %>%
  group_by(Primary_Cancer, Group) %>% mutate(Group_Tot=sum(n)) %>% 
  select(Primary_Cancer, Group, Tot, Group_Tot) %>% distinct() %>%
  mutate(perc=Group_Tot/Tot) %>% ungroup() %>%
  select(Primary_Cancer, Group, perc) %>% distinct() %>%
  spread(key=Group, value=perc)) %>% arrange(CCh)
  

data.frame(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>%
  inner_join(Monthly_groups %>% filter((grepl("CCh_", Group))) %>%
               inner_join(PONS_Demographics) %>% filter(Month_Min>=cancer_metastasis)) %>%
  group_by(patid, Primary_Cancer, Group) %>% count() %>% ungroup() %>%
  group_by(Primary_Cancer) %>%
   mutate(Tot=sum(n)) %>% ungroup() %>%
  group_by(Primary_Cancer, Group) %>% mutate(Group_Tot=sum(n)) %>% 
  select(Primary_Cancer, Group, Tot, Group_Tot) %>% distinct() %>%
  mutate(perc=Group_Tot/Tot) %>% ungroup() %>%
  select(Primary_Cancer, Group, perc) %>% distinct() %>%
  spread(key=Group, value=perc)) %>% arrange(CCh)
  

# -----------------


# Drug Penetrance Ever using the drug class classification after metastasis ---------------------------------------

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt")
CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
sum(CAN_Drug_Histories$weight)  # 9861087

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass)

CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>% summarise(n=sum(weight)) # 9861087





PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis) 
metastasis_flag <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

CAN_Drug_Histories %>% select(patid, weight) %>% distinct() %>%
  inner_join(metastasis_flag) %>%
  group_by(cancer_metastasis) %>% summarise(n=sum(weight))
  
data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(metastasis_flag) %>%
  group_by(cancer_metastasis, chemo_class) %>% summarise(n=sum(weight))) %>% 
  mutate(n=ifelse(cancer_metastasis==1,n/5571494,n/4289593)) %>%
  arrange(cancer_metastasis, desc(n)) %>%
  spread(key=cancer_metastasis, value=n)







# PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
# PONS_Demographics_mets <- PONS_Demographics_mets %>%  select(patid, cancer_metastasis) %>% drop_na()
# setDT(PONS_Demographics_mets)
# PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]
# 
# Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
# 
# Months_lookup$Month <- as.character(
#   format(
#     as.Date(
#       paste0(Months_lookup$Month,"-1")
#       ), "%Y-%m"
#     )
#   )
# 
# PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
# PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("cancer_metastasis"="Exact_Month")
# 
# 
# CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))



temp <- data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(metastasis_flag) %>%
             inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer, chemo_class) %>% summarise(n=sum(weight))) %>% 
  spread(key=Primary_Cancer, value=n) 

# temp <- CAN_Drug_Histories %>% select(patid, weight) %>%
#   distinct() %>% inner_join(New_Primary_Cancer_Box) %>%
#   inner_join(metastasis_flag) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
#   summarise(n=sum(weight)) %>%
#   spread(key=Primary_Cancer, value=n)


At_risk <- fread("Diagnosed Population 1.0/At_risk.txt")
CCh_5_years <- fread("Diagnosed Population 1.0/CCh_5_years.txt")

No_CCh <- At_risk %>% anti_join(CCh_5_years)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, cachexia_onset) %>% filter(!is.na(cachexia_onset))
CachexiaDx <- PONS_Demographics %>% select(patid)


groups <- CachexiaDx %>% mutate(Group="Dx") %>% 
  bind_rows(CCh_5_years %>% mutate(Group="Pred")) %>%
  bind_rows(No_CCh %>% mutate(Group="None")) %>%
  inner_join(New_Primary_Cancer_Box)



groups %>% inner_join(metastasis_flag)  %>% inner_join(CAN_Drug_Histories %>% select(patid) %>% distinct()) %>%
  group_by(Group, cancer_metastasis) %>% summarise(n=sum(weight))



data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% distinct() %>%
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(metastasis_flag) %>%
             inner_join(groups) %>%
  group_by(cancer_metastasis, Group, chemo_class) %>% summarise(n=sum(weight))) %>%
  mutate(n=ifelse(cancer_metastasis==0&Group=="Dx", n/55019,
                  ifelse(cancer_metastasis==0&Group=="Pred", n/931977,
                         ifelse(cancer_metastasis==0&Group=="None", n/514049,
                                ifelse(cancer_metastasis==1&Group=="None", n/597885,
                                       ifelse(cancer_metastasis==1&Group=="Pred", n/1719459 ,
                                              ifelse(cancer_metastasis==1&Group=="Dx", n/294238, NA ))))))) %>%
  spread(key=Group, value=n) 


unique(CAN_Drug_Histories$chemo_class)

data.frame(CAN_Drug_Histories %>% inner_join(groups) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(metastasis_flag) %>%
  group_by(Group, Primary_Cancer, cancer_metastasis) %>%
    select(patid, weight, Primary_Cancer, Group, cancer_metastasis) %>% distinct() %>%
 summarise(den=sum(weight)) %>%
  filter(Primary_Cancer %in% c("Breast Cancer", "Prostate Cancer", "Lung Cancer", "Intestinal Cancer", "Pancreatic Cancer")) %>%
  left_join(
    CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% distinct() %>%
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% inner_join(metastasis_flag) %>%
  filter(chemo_class %in% c("Progestin", "Appetite Stimulant", "Cannabinoid", "Nutrition")) %>%
             inner_join(groups) %>%
      inner_join(metastasis_flag) %>%
  select(patid, weight, Group, cancer_metastasis) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
    filter(Primary_Cancer %in% c("Breast Cancer", "Prostate Cancer", "Lung Cancer", "Intestinal Cancer", "Pancreatic Cancer")) %>%
  group_by(Group, Primary_Cancer, cancer_metastasis) %>% summarise(num=sum(weight))
  ) %>% mutate(perc=100*num/den)) %>% select(-c(den, num)) %>%
  spread(key=cancer_metastasis, value=perc)


data.frame(groups %>% inner_join(metastasis_flag) %>%
  group_by(cancer_metastasis, Group, Primary_Cancer) %>%
  filter(Primary_Cancer %in% c("Breast Cancer", "Prostate Cancer", "Lung Cancer", "Intestinal Cancer", "Pancreatic Cancer")) %>%
  summarise(n=sum(weight)) %>% ungroup() %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% mutate(tot=sum(n)) %>% 
  mutate(perc=n/tot)  %>% select(-c(n, tot)) %>%
  spread(key=cancer_metastasis, value=perc)
)


# ----------
# Cumulative year over year --------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)


length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 


temp %>% filter(Recovered==1) %>% select(patid) %>% distinct()

ignore <- temp %>% select(patid, Month_Min, CUM_CCh) %>%
  group_by(patid, Month_Min) %>% summarise(CUM_CCh=max(CUM_CCh)) %>% ungroup() %>%
  inner_join(Pats_to_track_BMI %>% select(patid, weight, cancer_metastasis)) %>% distinct()

ignore <- ignore %>% inner_join(temp %>% filter(CUM_CCh==1) %>% select(patid) %>% distinct())  

ignore %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 4670521
ignore %>% select(patid, weight, cancer_metastasis) %>% distinct() %>% 
  group_by(cancer_metastasis) %>% summarise(n=sum(weight)) # 0 2202819. , 1 2467702.


ignore %>% filter(CUM_CCh==1) %>% group_by(patid) %>%  filter(Month_Min==min(Month_Min)) %>%
  group_by(Month_Min) %>% summarise(n=sum(weight)/4670521) %>%
  ungroup() %>%
  ggplot(aes(Month_Min, n)) +
  geom_line()



ignore %>% filter(CUM_CCh==1) %>% group_by(patid, cancer_metastasis) %>%  
  filter(Month_Min==min(Month_Min)) %>%
  group_by(Month_Min, cancer_metastasis) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(cancer_metastasis==0,n/2202819.,n/2467702.)) %>%
  mutate(cancer_metastasis=as.factor(cancer_metastasis)) %>%
  ungroup() %>%
  ggplot(aes(Month_Min, n, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_line()



ignore %>% filter(CUM_CCh==1) %>% group_by(patid, cancer_metastasis) %>%  
  filter(Month_Min==min(Month_Min)) %>%
  group_by(Month_Min, cancer_metastasis) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(cancer_metastasis==0,n/2202819.,n/2467702.)) %>%
  mutate(cancer_metastasis=as.factor(cancer_metastasis)) %>%
  group_by(cancer_metastasis) %>% mutate(n=cumsum(n)) %>%
  ungroup() %>%
  rename("Metastasis"="cancer_metastasis") %>%
  mutate(Metastasis=ifelse(Metastasis==0,"No", "Yes")) %>%
  ggplot(aes(Month_Min, n, colour=Metastasis, fill=Metastasis)) +
  geom_line(size=2, alpha=0.5) +
  theme_minimal() +
  xlab("\n Exact Data Month of Cachexia Onset") + ylab("Cumulative Patient Proportion \n") +
  scale_fill_manual(values=c("firebrick", "midnightblue")) +
  scale_colour_manual(values=c("firebrick", "midnightblue")) 




ignore %>% filter(CUM_CCh==1) %>% group_by(patid) %>%  
  filter(Month_Min==min(Month_Min)) %>%
  group_by(Month_Min) %>% summarise(n=sum(weight)/4670521) %>%
   mutate(n=cumsum(n)) %>%
  ungroup() %>%
  ggplot(aes(Month_Min, n)) +
  geom_line(size=2, alpha=0.5, colour="midnightblue", fill="midnightblue") +
  theme_minimal() +
  xlab("\n Exact Data Month of Cachexia Onset") + ylab("Cumulative Patient Proportion \n") 

# ---------
# Refractory Profiles -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")





data.frame(temp %>% inner_join(PONS_Demographics_onset %>% drop_na())  %>%
  filter(Month_Min>=cancer_onset & Month_Min<=(cancer_onset+12)) %>%
  inner_join(PONS_Demographics_mets) %>%
   filter(CCh==1) %>% 
   select(patid, cancer_metastasis) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight))) %>%
  left_join(
    data.frame(temp %>% inner_join(PONS_Demographics_onset %>% drop_na())  %>%
  filter(Month_Min>=cancer_onset & Month_Min<=(cancer_onset+12)) %>%
  inner_join(PONS_Demographics_mets) %>%
   #filter(CCh==1) %>% 
   select(patid, cancer_metastasis) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(nn=sum(weight)))
  ) %>% mutate(perc=n/nn) %>% select(-c(n,nn))
  


CCh_pats <- data.frame(New_Primary_Cancer_Box %>% 
                         inner_join(temp %>% filter(CCh==1) %>% select(patid) %>% distinct())) 


Weight_Refract_Criteria <- temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()



CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")
CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()


data.frame(CCh_pats %>% 
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))





data.frame(CCh_pats %>% inner_join(Weight_Refract_Criteria) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


data.frame(CCh_pats %>% inner_join(CaxPts_ICD10_Z515) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))



CAN_Doses <- fread("CAN Analysis Results 3.0/CAN Doses.txt")
CAN_Doses <- temp %>% select(patid) %>% distinct() %>% left_join(CAN_Doses, by=c("patid"="pat_id"))
CAN_Doses <- CAN_Doses %>% select(patid, prov, specialty) %>% distinct()

PONS_Event_Claims_Providers <- fread("PONS Events/PONS Event Claims Providers.txt")
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers %>% select(prov, specialty_classification)
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers%>% filter(specialty_classification=="Palliative Medicine") %>% select(prov) %>% distinct()

Paliat_Rxs <-  PONS_Event_Claims_Providers %>% inner_join(CAN_Doses) %>% select(patid) %>% distinct()

data.frame(CCh_pats %>% inner_join(Paliat_Rxs) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))



data.frame(CCh_pats %>% 
             inner_join(Paliat_Rxs %>% 
                          full_join(CaxPts_ICD10_Z515) %>% 
                          full_join(
                            temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()
                          )
  ) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


PONS_Demographics_dead <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_dead <- PONS_Demographics_dead %>% filter(died=="Y")
PONS_Demographics_dead <- PONS_Demographics_dead %>% select(patid, death_date)
setDT(PONS_Demographics_dead)
PONS_Demographics_dead[, death_date := as.character(death_date)][, death_date := substr(death_date, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_dead <- PONS_Demographics_dead %>% left_join(Months_lookup, by=c("death_date" = "Month"))
PONS_Demographics_dead <- PONS_Demographics_dead %>% select(-death_date) %>% rename("death_date"="Exact_Month")


data.frame(CCh_pats %>% inner_join(PONS_Demographics_dead) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CCh_pats %>% select(patid) %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Lapsed_before_dead <- CAN_Drug_Histories %>% filter(Drugs=="355") %>% group_by(patid) %>% summarise(first=min(Month)) %>%
  inner_join(CAN_Drug_Histories) %>% filter(Month==first-1) %>% filter(Drugs=="-") %>% select(patid) %>% distinct()


data.frame(CCh_pats %>% inner_join(Lapsed_before_dead) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


data.frame(CCh_pats %>% 
             inner_join(Paliat_Rxs %>% 
                          full_join(CaxPts_ICD10_Z515) %>% 
                          full_join(
                            temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()
                          ) %>%
    full_join(PONS_Demographics_dead %>% select(patid) %>% distinct())
  ) %>%
    inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


Dead_dates <- PONS_Demographics_dead


CCh_pats

CCh_pats_no_paliat <- CCh_pats %>% select(patid) %>% anti_join(Lapsed_before_dead) %>% anti_join(CaxPts_ICD10_Z515) %>% anti_join(Weight_Refract_Criteria)
Lapsed_before_dead
CaxPts_ICD10_Z515
Weight_Refract_Criteria


data.frame(temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid, Month_Min) %>% distinct() %>% group_by(patid) %>% summarise(First_Pali=min(Month_Min)) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-First_Pali) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(median=mean(elapsed)) %>%
  spread(key=cancer_metastasis, value=median))



CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")
data.frame(CaxPts_ICD10_Z515 %>% select(patid, fst_dt) %>% distinct() %>% group_by(patid) %>%
  summarise(fst_dt=min(fst_dt)) %>% ungroup() %>%
  mutate(fst_dt=substr(as.character(fst_dt), 1L, 7L)) %>%
  inner_join(Months_lookup, by=c("fst_dt" = "Month")) %>%
  select(-fst_dt) %>% rename("fst_dt"="Exact_Month") %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-fst_dt) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(median=mean(elapsed)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=median))
  

data.frame(CaxPts_ICD10_Z515 %>% select(patid, fst_dt) %>% distinct() %>% group_by(patid) %>%
  summarise(fst_dt=min(fst_dt)) %>% ungroup() %>%
  mutate(fst_dt=substr(as.character(fst_dt), 1L, 7L)) %>%
  inner_join(Months_lookup, by=c("fst_dt" = "Month")) %>%
  select(-fst_dt) %>% rename("fst_dt"="Exact_Month") %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-fst_dt) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) %>%
  bind_rows(
    temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid, Month_Min) %>% distinct() %>% group_by(patid) %>% summarise(First_Pali=min(Month_Min)) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-First_Pali) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) 
  ) %>% distinct() %>%
  group_by(patid, weight, Primary_Cancer,cancer_metastasis) %>% summarise(elapsed=max(elapsed)) %>% 
  ungroup() %>%
   group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(median=median(elapsed)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=median))


CCh_pats_no_paliat 
CCh_pats
Lapsed_before_dead
CaxPts_ICD10_Z515
Weight_Refract_Criteria



data.frame(temp %>%  group_by(patid) %>%
  summarise(Min=min(Min)) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  # inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
  #              full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
  #                          full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
  summarise(mean=median(Min)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))


PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="Hemoglobin")
PONS_Measures <- PONS_Measures %>% select(patid, value) %>% distinct() %>%
  group_by(patid) %>% summarise(value=min(value)) %>% ungroup()



data.frame(PONS_Measures %>%  
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  #inner_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>%
   inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
                full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
                            full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
  summarise(mean=median(value)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))



PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="Albumin")
PONS_Measures <- PONS_Measures %>% select(patid, value) %>% distinct() %>%
  group_by(patid) %>% summarise(value=min(value)) %>% ungroup()



data.frame(PONS_Measures %>%  
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  # inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
  #              full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
  #                          full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
  summarise(mean=median(value)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))


PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories 3.0/PONS Comorbidity Inventories.txt")
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% 
  filter(grepl("R53", diagnosis)) %>% select(patid) %>% distinct() %>%
  mutate(Dx=1)




data.frame(New_Primary_Cancer_Box %>%
  inner_join(PONS_Demographics_mets) %>%
   inner_join(CCh_pats_no_paliat %>% select(patid) %>% distinct()) %>%
   #inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
    #            full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
     #                       full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  left_join(PONS_Comorbidity_Inventories) %>% mutate(Dx=ifelse(is.na(Dx),0,Dx)) %>%
    group_by(cancer_metastasis, Primary_Cancer) %>% 
  summarise(mean=mean(Dx)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))





CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)
names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, chemo_class) %>% distinct() %>%
  drop_na() %>% rename("patid"="patient") %>% mutate(exp=1)



data.frame(New_Primary_Cancer_Box %>%
  inner_join(PONS_Demographics_mets) %>%
   inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
   #inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
    #            full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
     #                       full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  left_join(CAN_Drug_Histories) %>% mutate(exp=ifelse(is.na(exp),0,exp)) %>%
    group_by(cancer_metastasis, Primary_Cancer) %>% 
  summarise(mean=mean(Dx)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))




# ------------

# Time onset to cachexia pred or dx metastatic only and top 5 ---------
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Overall 

data.frame(New_Primary_Cancer_Box %>% inner_join(temp  %>% select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(Drop2_20 ==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_metastasis) %>% drop_na() %>% rename("cancer_onset"="cancer_metastasis")
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")



data.frame(temp %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh))) %>%
  spread(key=cancer_metastasis, value=mean) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer"))



data.frame(temp %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh))) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer"))




# Time to Cachexia Dx

PONS_Demographics_cachexia_dx <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>%  select(patid, cachexia_onset) %>% drop_na()
setDT(PONS_Demographics_cachexia_dx)
PONS_Demographics_cachexia_dx[, cachexia_onset := as.character(cachexia_onset)][, cachexia_onset := substr(cachexia_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% left_join(Months_lookup, by=c("cachexia_onset" = "Month"))
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% select(-cachexia_onset) %>% rename("cachexia_onset"="Exact_Month")



data.frame(PONS_Demographics_cachexia_dx %>%
  rename("First_CCh"="cachexia_onset") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh)) %>%
  spread(key=cancer_metastasis, value=mean))  %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer"))


data.frame(PONS_Demographics_cachexia_dx %>%
  rename("First_CCh"="cachexia_onset") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh)) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")))


temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets %>% filter(cancer_metastasis==1)) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))



temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_cachexia_dx %>% inner_join(PONS_Demographics_onset) %>% filter(cachexia_onset>=cancer_onset) %>% select(patid)) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))

PONS_Demographics_cachexia_dx %>% inner_join(PONS_Demographics_onset) %>% filter(cachexia_onset>=cancer_onset) %>% select(patid)

CCh_mets <- temp %>% select(patid, Month_Min, CCh) %>% distinct() %>%
  inner_join(PONS_Demographics_onset) %>% filter(Month_Min>cancer_onset) %>%
  filter(CCh==1) %>% select(patid) %>% distinct()


temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets %>% filter(cancer_metastasis==1)) %>%
  inner_join(CCh_mets) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))



# -----------
# Refractory Profiles: Weight Loss exc. Dx Paliative  -----------------

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_onset)
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")





data.frame(temp %>% inner_join(PONS_Demographics_onset %>% drop_na())  %>%
  filter(Month_Min>=cancer_onset & Month_Min<=(cancer_onset+12)) %>%
  inner_join(PONS_Demographics_mets) %>%
   filter(CCh==1) %>% 
   select(patid, cancer_metastasis) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight))) %>%
  left_join(
    data.frame(temp %>% inner_join(PONS_Demographics_onset %>% drop_na())  %>%
  filter(Month_Min>=cancer_onset & Month_Min<=(cancer_onset+12)) %>%
  inner_join(PONS_Demographics_mets) %>%
   #filter(CCh==1) %>% 
   select(patid, cancer_metastasis) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(nn=sum(weight)))
  ) %>% mutate(perc=n/nn) %>% select(-c(n,nn))
  


CCh_pats <- data.frame(New_Primary_Cancer_Box %>% 
                         inner_join(temp %>% filter(CCh==1) %>% select(patid) %>% distinct())) 


Weight_Refract_Criteria <- temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()



CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")
CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()


data.frame(CCh_pats %>% 
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))





data.frame(CCh_pats %>% inner_join(Weight_Refract_Criteria) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


data.frame(CCh_pats %>% inner_join(CaxPts_ICD10_Z515) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))



CAN_Doses <- fread("CAN Analysis Results 3.0/CAN Doses.txt")
CAN_Doses <- temp %>% select(patid) %>% distinct() %>% left_join(CAN_Doses, by=c("patid"="pat_id"))
CAN_Doses <- CAN_Doses %>% select(patid, prov, specialty) %>% distinct()

PONS_Event_Claims_Providers <- fread("PONS Events/PONS Event Claims Providers.txt")
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers %>% select(prov, specialty_classification)
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers%>% filter(specialty_classification=="Palliative Medicine") %>% select(prov) %>% distinct()

Paliat_Rxs <-  PONS_Event_Claims_Providers %>% inner_join(CAN_Doses) %>% select(patid) %>% distinct()

data.frame(CCh_pats %>% inner_join(Paliat_Rxs) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))



data.frame(CCh_pats %>% 
             inner_join(Paliat_Rxs %>% 
                          full_join(CaxPts_ICD10_Z515) %>% 
                          full_join(
                            temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()
                          )
  ) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


PONS_Demographics_dead <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_dead <- PONS_Demographics_dead %>% filter(died=="Y")
PONS_Demographics_dead <- PONS_Demographics_dead %>% select(patid, death_date)
setDT(PONS_Demographics_dead)
PONS_Demographics_dead[, death_date := as.character(death_date)][, death_date := substr(death_date, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_dead <- PONS_Demographics_dead %>% left_join(Months_lookup, by=c("death_date" = "Month"))
PONS_Demographics_dead <- PONS_Demographics_dead %>% select(-death_date) %>% rename("death_date"="Exact_Month")


data.frame(CCh_pats %>% inner_join(PONS_Demographics_dead) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CCh_pats %>% select(patid) %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Lapsed_before_dead <- CAN_Drug_Histories %>% filter(Drugs=="355") %>% group_by(patid) %>% summarise(first=min(Month)) %>%
  inner_join(CAN_Drug_Histories) %>% filter(Month==first-1) %>% filter(Drugs=="-") %>% select(patid) %>% distinct()


data.frame(CCh_pats %>% inner_join(Lapsed_before_dead) %>%
             inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


data.frame(CCh_pats %>% 
             inner_join(Paliat_Rxs %>% 
                          full_join(CaxPts_ICD10_Z515) %>% 
                          full_join(
                            temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()
                          ) %>%
    full_join(PONS_Demographics_dead %>% select(patid) %>% distinct())
  ) %>%
    inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))


data.frame(CCh_pats %>% 
             inner_join(Paliat_Rxs %>% 
                          full_join(CaxPts_ICD10_Z515) %>% 
                          full_join(
                            temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid) %>% distinct()
                          ) %>%
    full_join(PONS_Demographics_dead %>% select(patid) %>% distinct())
  ) %>%
    inner_join(PONS_Demographics_mets) %>% group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))



Dead_dates <- PONS_Demographics_dead


CCh_pats

CCh_pats_no_paliat <- CCh_pats %>% select(patid) %>% anti_join(Lapsed_before_dead) %>% anti_join(CaxPts_ICD10_Z515) %>% anti_join(Weight_Refract_Criteria)
Lapsed_before_dead
CaxPts_ICD10_Z515
Weight_Refract_Criteria
Weight_Refract_Criteria_excDx <- Weight_Refract_Criteria %>% anti_join(CaxPts_ICD10_Z515)

data.frame(Weight_Refract_Criteria %>% inner_join(CCh_pats) %>%
  inner_join(PONS_Demographics_mets) %>% 
  anti_join(CaxPts_ICD10_Z515) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(n=sum(weight)))

data.frame(temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid, Month_Min) %>% distinct() %>% group_by(patid) %>% summarise(First_Pali=min(Month_Min)) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-First_Pali) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) %>%
    inner_join(Weight_Refract_Criteria_excDx) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(median=median(elapsed)) %>%
  spread(key=cancer_metastasis, value=median))



CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")
data.frame(CaxPts_ICD10_Z515 %>% select(patid, fst_dt) %>% distinct() %>% group_by(patid) %>%
  summarise(fst_dt=min(fst_dt)) %>% ungroup() %>%
  mutate(fst_dt=substr(as.character(fst_dt), 1L, 7L)) %>%
  inner_join(Months_lookup, by=c("fst_dt" = "Month")) %>%
  select(-fst_dt) %>% rename("fst_dt"="Exact_Month") %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-fst_dt) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(median=mean(elapsed)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=median))
  

data.frame(CaxPts_ICD10_Z515 %>% select(patid, fst_dt) %>% distinct() %>% group_by(patid) %>%
  summarise(fst_dt=min(fst_dt)) %>% ungroup() %>%
  mutate(fst_dt=substr(as.character(fst_dt), 1L, 7L)) %>%
  inner_join(Months_lookup, by=c("fst_dt" = "Month")) %>%
  select(-fst_dt) %>% rename("fst_dt"="Exact_Month") %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-fst_dt) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) %>%
  bind_rows(
    temp %>% 
  mutate(Ref_1=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max) & (Min<23) ,1,0 )) %>% 
  mutate(Ref_2=ifelse( (Min<(Max*0.80)) & (Month_Min>Month_Max) &  (Min<27),1,0 )) %>%
  filter(Ref_1==1|Ref_2==1) %>%
  select(patid, Month_Min) %>% distinct() %>% group_by(patid) %>% summarise(First_Pali=min(Month_Min)) %>%
  left_join(Dead_dates) %>% mutate(death_date=ifelse(is.na(death_date),60,death_date)) %>%
  mutate(elapsed=death_date-First_Pali) %>%
  inner_join(New_Primary_Cancer_Box) %>% select(patid, elapsed, weight, Primary_Cancer) %>%
  inner_join(PONS_Demographics_mets) 
  ) %>% distinct() %>%
  group_by(patid, weight, Primary_Cancer,cancer_metastasis) %>% summarise(elapsed=max(elapsed)) %>% 
  ungroup() %>%
   group_by(cancer_metastasis, Primary_Cancer) %>%
  summarise(median=median(elapsed)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=median))


CCh_pats_no_paliat 
CCh_pats
Lapsed_before_dead
CaxPts_ICD10_Z515
Weight_Refract_Criteria
Weight_Refract_Criteria_excDx


data.frame(temp %>%  group_by(patid) %>%
  summarise(Min=min(Min)) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  # inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
  #              full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
  #                          full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    inner_join(Weight_Refract_Criteria_excDx) %>%
  summarise(mean=mean(Min)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))



data.frame(temp %>% select(patid, Max, Min) %>% distinct() %>%
             mutate(Drop=(Max-Min)/Max) %>%
             group_by(patid) %>%
  summarise(Drop=max(Drop)) %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  # inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
  #              full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
  #                          full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    inner_join(Weight_Refract_Criteria_excDx) %>%
  summarise(mean=median(Drop)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))




PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="Hemoglobin")
PONS_Measures <- PONS_Measures %>% select(patid, value) %>% distinct() %>%
  group_by(patid) %>% summarise(value=min(value)) %>% ungroup()



data.frame(PONS_Measures %>%  
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  #inner_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>%
  # inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
  #              full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
   #                         full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
        inner_join(Weight_Refract_Criteria_excDx) %>%
  summarise(mean=mean(value)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))



PONS_Measures <- fread("PONS Measures (Labs)/PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="Albumin")
PONS_Measures <- PONS_Measures %>% select(patid, value) %>% distinct() %>%
  group_by(patid) %>% summarise(value=min(value)) %>% ungroup()



data.frame(PONS_Measures %>%  
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets) %>%
  #inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
  # inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
  #              full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
  #                          full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
            inner_join(Weight_Refract_Criteria_excDx) %>%
  summarise(mean=mean(value)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))


PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories 3.0/PONS Comorbidity Inventories.txt")
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% 
  filter(grepl("E43", diagnosis)) %>% select(patid) %>% distinct() %>%
  mutate(Dx=1)




data.frame(New_Primary_Cancer_Box %>%
  inner_join(PONS_Demographics_mets) %>%
  # inner_join(CCh_pats_no_paliat %>% select(patid) %>% distinct()) %>%
   #inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
    #            full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
     #                       full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  left_join(PONS_Comorbidity_Inventories) %>% mutate(Dx=ifelse(is.na(Dx),0,Dx)) %>%
                inner_join(Weight_Refract_Criteria_excDx) %>%
    group_by(cancer_metastasis, Primary_Cancer) %>% 
  summarise(mean=mean(Dx)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))





CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 2.2/PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)
names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, chemo_class) %>% distinct() %>%
  drop_na() %>% rename("patid"="patient") %>% mutate(exp=1)



data.frame(New_Primary_Cancer_Box %>%
  inner_join(PONS_Demographics_mets) %>%
   inner_join(CCh_pats %>% select(patid) %>% distinct()) %>%
   #inner_join(Lapsed_before_dead %>% select(patid) %>% distinct() %>%
    #            full_join(CaxPts_ICD10_Z515 %>% select(patid) %>% distinct()) %>%
     #                       full_join(Lapsed_before_dead %>% select(patid) %>% distinct()) %>% distinct()) %>%
  left_join(CAN_Drug_Histories) %>% mutate(exp=ifelse(is.na(exp),0,exp)) %>%
    group_by(cancer_metastasis, Primary_Cancer) %>% 
  summarise(mean=mean(Dx)) %>% ungroup() %>%
  spread(key=cancer_metastasis, value=mean))




# ------------
# Time onset to cachexia pred metastatic only  ---------
PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
PONS_Demographics_mets <- PONS_Demographics_mets %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))



# Overall 

data.frame(New_Primary_Cancer_Box %>% inner_join(temp  %>% select(patid) %>% distinct()) %>%
             inner_join(PONS_Demographics_mets) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)


data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(CCh==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



data.frame(New_Primary_Cancer_Box %>% inner_join(temp %>% 
                                                   filter(Drop2_20 ==1) %>%
                                                   select(patid) %>% distinct()) %>%
               inner_join(PONS_Demographics_mets) %>%
             group_by(cancer_metastasis, Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=cancer_metastasis, value=n)



# Time to Cachexia

PONS_Demographics_onset <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_onset <- PONS_Demographics_onset %>%  select(patid, cancer_metastasis) %>% drop_na() %>% rename("cancer_onset"="cancer_metastasis")
setDT(PONS_Demographics_onset)
PONS_Demographics_onset[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_onset <- PONS_Demographics_onset %>% left_join(Months_lookup, by=c("cancer_onset" = "Month"))
PONS_Demographics_onset <- PONS_Demographics_onset %>% select(-cancer_onset) %>% rename("cancer_onset"="Exact_Month")



data.frame(temp %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh))) %>%
  spread(key=cancer_metastasis, value=mean) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer"))



data.frame(temp %>% filter(CCh==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh))) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer"))






data.frame(temp %>% filter(Drop95==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh))) %>%
  spread(key=cancer_metastasis, value=mean) 



# Time to Cachexia Dx

PONS_Demographics_cachexia_dx <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>%  select(patid, cachexia_onset) %>% drop_na()
setDT(PONS_Demographics_cachexia_dx)
PONS_Demographics_cachexia_dx[, cachexia_onset := as.character(cachexia_onset)][, cachexia_onset := substr(cachexia_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% left_join(Months_lookup, by=c("cachexia_onset" = "Month"))
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% select(-cachexia_onset) %>% rename("cachexia_onset"="Exact_Month")



data.frame(PONS_Demographics_cachexia_dx %>%
  rename("First_CCh"="cachexia_onset") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(cancer_metastasis, Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh)) %>%
  spread(key=cancer_metastasis, value=mean))  %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer"))


data.frame(PONS_Demographics_cachexia_dx %>%
  rename("First_CCh"="cachexia_onset") %>%
  inner_join(New_Primary_Cancer_Box) %>%
    inner_join(PONS_Demographics_onset) %>%
      mutate(ElapsedCCh=(First_CCh-cancer_onset)) %>%
  inner_join(PONS_Demographics_mets) %>%
  filter(!is.na(ElapsedCCh)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by(Primary_Cancer) %>% 
    summarise(mean=median(ElapsedCCh)) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")))


temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets %>% filter(cancer_metastasis==1)) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))



temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_cachexia_dx %>% inner_join(PONS_Demographics_onset) %>% filter(cachexia_onset>=cancer_onset) %>% select(patid)) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))

PONS_Demographics_cachexia_dx %>% inner_join(PONS_Demographics_onset) %>% filter(cachexia_onset>=cancer_onset) %>% select(patid)

CCh_mets <- temp %>% select(patid, Month_Min, CCh) %>% distinct() %>%
  inner_join(PONS_Demographics_onset) %>% filter(Month_Min>cancer_onset) %>%
  filter(CCh==1) %>% select(patid) %>% distinct()


temp %>% select(patid) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
  inner_join(PONS_Demographics_mets %>% filter(cancer_metastasis==1)) %>%
  inner_join(CCh_mets) %>%
  filter(Primary_Cancer %in% c("Prostate Cancer", "Breast Cancer", "Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer")) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))



# -----------
# Proportion of patients with BMI<30 in the 12 months post metastasis -------------
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))
unique(New_Primary_Cancer_Box$Primary_Cancer)

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")

Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")


New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer") %>%
  select(patid) %>% # 53553 
inner_join(
  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large")
) # 51276
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))


data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")





PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(-c(Primary, died))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()


temp_min <- fread("Diagnosed Population 1.0/MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_min <- temp_min %>% gather(Month, BMI, `1`:`60`) %>% drop_na()


With_BMI_12m <- temp_min %>% mutate(Month=as.numeric(Month)) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, metastasis_onset)  %>% distinct()) %>%
  filter( (Month>=metastasis_onset)&(Month-metastasis_onset<=12) ) %>% select(patid) %>% distinct()


With_BMI_minus30 <- temp_min %>% mutate(Month=as.numeric(Month)) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, metastasis_onset)  %>% distinct()) %>%
  filter( (Month>=metastasis_onset)&(Month-metastasis_onset<=12) ) %>% filter(BMI<30) %>% select(patid) %>% distinct()


data.frame(New_Primary_Cancer_Box %>% inner_join(With_BMI_12m) %>% left_join(With_BMI_minus30 %>% mutate(Minus30="Minus30")) %>%
  group_by(Primary_Cancer, Minus30) %>% summarise(n=sum(weight)) %>% ungroup() %>%
  spread(key=Minus30, value=n) %>% mutate(perc=Minus30/(Minus30+`<NA>`)))

# ---------
# Time from metastasis to cachexia 10% vs other fearon -------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





data.frame(temp %>% filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>% filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))




data.frame(temp %>% filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(First_CCh>=metastasis_onset) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>% filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(First_CCh>=metastasis_onset) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


# ---------
# Time mets to cachexia Dx -------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





data.frame(temp %>% filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>% filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))




data.frame(temp %>%        
             inner_join(New_Primary_Cancer_Box) %>%
             filter(Month_Min>=metastasis_onset) %>%
             filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(First_CCh>=metastasis_onset) %>%
      filter(ElapsedCCh<=12) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>% 
              inner_join(New_Primary_Cancer_Box) %>%
             filter(Month_Min>=metastasis_onset) %>%
             filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(First_CCh>=metastasis_onset) %>%
   filter(ElapsedCCh<=12) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))






# Time to Cachexia Dx

PONS_Demographics_cachexia_dx <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>%  select(patid, cachexia_onset) %>% drop_na()
setDT(PONS_Demographics_cachexia_dx)
PONS_Demographics_cachexia_dx[, cachexia_onset := as.character(cachexia_onset)][, cachexia_onset := substr(cachexia_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% left_join(Months_lookup, by=c("cachexia_onset" = "Month"))
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% select(-cachexia_onset) %>% rename("cachexia_onset"="Exact_Month")






data.frame(New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_cachexia_dx) %>%
  mutate(ElapsedCCh=(cachexia_onset-metastasis_onset)) %>%
    filter(cachexia_onset>=metastasis_onset) %>%
    group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


# ---------
# Weight loss breakdown 2%, 5%, 10%, 15% -------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





temp <- temp %>% mutate(Drop85=ifelse( (Min<(Max*85)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))

  
data.frame(temp %>% 
   inner_join(New_Primary_Cancer_Box) %>%
  filter(Month_Min>=metastasis_onset) %>%
  filter(Month_Min<=(metastasis_onset+12)) %>%
  select(patid, Max, Min, Primary_Cancer) %>% distinct() %>%
    mutate(Drop=(Max-Min)/Max) %>%
    mutate(group=ifelse(Drop>=0.02&Min<20,1,
                        ifelse(Drop>=0.15,2,
                               ifelse(Drop>=0.1,3,
                                      ifelse(Drop>=0.05,4,5))))) %>%
    group_by(patid, Primary_Cancer) %>% summarise(group=min(group)) %>%
  group_by(Primary_Cancer , group) %>% count() %>%
  spread(key=group, value=n))
    


# ---------
# Time from metastasis to cachexia Rx --------------


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month) %>% distinct()


data.frame(New_Primary_Cancer_Box %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient")) %>%
  filter(Month>=metastasis_onset) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  mutate(diff=Month-metastasis_onset) %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(diff)))
 


# ------------


# Anticancer Drugs Use 1 year post mets  --------------


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





denom <- data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))) %>% rename("denom"="n")




CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% left_join(CAN_Drug_Histories %>% select(-disease), by=c("patid"="patient"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()





CAN_Drug_Histories <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories) %>% 
  filter(Month>=metastasis_onset) %>%
  filter(Month<=(metastasis_onset+12)) %>% select(-c(metastasis_onset, Month)) %>% distinct()


CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>%  distinct()

PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 3.0/PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass) %>% distinct()

unique(CAN_Drug_Histories$chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(chemo_class=="Alkylating Agent"|
                                chemo_class=="Immuno/Targeted"|
                                chemo_class=="Hormonal Therapy"|
                                chemo_class=="Biologic"|
                                chemo_class=="Radio"|
                                  chemo_class=="Antimicrotubule Agent"|
                                  chemo_class=="Platinum agent"|
                                  chemo_class=="Antimetabolites"|
                                  chemo_class=="Topoisomerase Inhibitor"|
                                  chemo_class=="Other Antineoplastics"|
                                  chemo_class=="PD1/PDL1"|
                                  chemo_class=="Surgery Inpatient")



denom %>% left_join(
  CAN_Drug_Histories %>% select(-Drugs) %>% distinct() %>% group_by(Primary_Cancer, chemo_class) %>%
  summarise(num=sum(weight))
)  %>% mutate(perc=num/denom) %>% select(-c(denom, num)) %>%
  spread(key=chemo_class, value=perc)



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Exp=1) %>% ungroup()
CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0
temp <-temp %>% left_join(CAN_Drug_Histories)
names(temp)[2] <- "AppetiteStimulantGroup"


# ------------


# Anticancer Lines Use 1 year post mets  --------------


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





denom <- data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))) %>% rename("denom"="n")




CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% left_join(CAN_Drug_Histories %>% select(-disease), by=c("patid"="patient"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()


CAN_Drug_Histories <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories) %>% 
  filter(Month>=metastasis_onset) %>%
  filter(Month<=(metastasis_onset+12)) %>% select(-c(metastasis_onset)) %>% distinct()

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>%  distinct()

PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 3.0/PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass) %>% distinct()

unique(CAN_Drug_Histories$chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(chemo_class=="Alkylating Agent"|
                                chemo_class=="Immuno/Targeted"|
                                chemo_class=="Hormonal Therapy"|
                                chemo_class=="Biologic"|
                                chemo_class=="Radio"|
                                  chemo_class=="Antimicrotubule Agent"|
                                  chemo_class=="Platinum agent"|
                                  chemo_class=="Antimetabolites"|
                                  chemo_class=="Topoisomerase Inhibitor"|
                                  chemo_class=="Other Antineoplastics"|
                                  chemo_class=="PD1/PDL1"|
                                  chemo_class=="Surgery Inpatient")


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-chemo_class) %>%
  arrange(patid, weight, Primary_Cancer, Month) %>%
  group_by(patid, weight, Primary_Cancer, Month) %>% 
  mutate(Drugs=paste0(Drugs, collapse=",")) %>% distinct()


data.frame(CAN_Drug_Histories %>% ungroup() %>%
  select(-Month) %>% distinct %>%
   group_by(patid, weight, Primary_Cancer) %>% count() %>% ungroup() %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(n)))


# ------------


# Cachexia Rx to palliative care - anticancer lines --------------

CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")


Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% select(patid, fst_dt) %>% distinct() %>% group_by(patid) %>%
  summarise(fst_dt=min(fst_dt)) %>% ungroup() %>%
  mutate(fst_dt=substr(as.character(fst_dt), 1L, 7L)) %>%
  inner_join(Months_lookup, by=c("fst_dt" = "Month")) %>%
  select(-fst_dt) %>% rename("fst_dt"="Exact_Month")




New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()



New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CaxPts_ICD10_Z515)




CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month) %>% distinct()


data.frame(New_Primary_Cancer_Box %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient")) %>%
  filter(Month>=metastasis_onset) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  mutate(diff=fst_dt -Month) %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(diff)))


# ---------
# Proportion of patients with BMI<30 in the 12 months post metastasis -------------
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))
unique(New_Primary_Cancer_Box$Primary_Cancer)

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")

Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")


New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer") %>%
  select(patid) %>% # 53553 
inner_join(
  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large")
) # 51276
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))


data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")





PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(-c(Primary, died))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()


temp_min <- fread("Diagnosed Population 1.0/MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_min <- temp_min %>% gather(Month, BMI, `1`:`60`) %>% drop_na()


With_BMI_12m <- temp_min %>% mutate(Month=as.numeric(Month)) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, metastasis_onset)  %>% distinct()) %>%
  filter( (Month>=metastasis_onset)&(Month-metastasis_onset<=12) ) %>% select(patid) %>% distinct()


With_BMI_minus30 <- temp_min %>% mutate(Month=as.numeric(Month)) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, metastasis_onset)  %>% distinct()) %>%
  filter( (Month>=metastasis_onset)&(Month-metastasis_onset<=12) ) %>% filter(BMI<30) %>% select(patid) %>% distinct()


data.frame(New_Primary_Cancer_Box %>% inner_join(With_BMI_12m) %>% left_join(With_BMI_minus30 %>% mutate(Minus30="Minus30")) %>%
  group_by(Primary_Cancer, Minus30) %>% summarise(n=sum(weight)) %>% ungroup() %>%
  spread(key=Minus30, value=n) %>% mutate(perc=Minus30/(Minus30+`<NA>`)))


With_BMI_12m <- With_BMI_12m %>% left_join(With_BMI_minus30 %>% mutate(Minus30="Minus30")) 

# ---------
# Weight loss breakdown 2%, 5%, 10%, 15% BMI<30 vs BMI>30 -------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





temp <- temp %>% mutate(Drop85=ifelse( (Min<(Max*85)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))

  
data.frame(temp %>% 
   inner_join(New_Primary_Cancer_Box) %>%
  filter(Month_Min>=metastasis_onset) %>%
  filter(Month_Min<=(metastasis_onset+12)) %>%
  select(patid, Max, Min, Primary_Cancer) %>% distinct() %>%
    mutate(Drop=(Max-Min)/Max) %>%
    mutate(group=ifelse(Drop>=0.02&Min<20,1,
                        ifelse(Drop>=0.15,2,
                               ifelse(Drop>=0.1,3,
                                      ifelse(Drop>=0.05,4,5))))) %>%
    group_by(patid, Primary_Cancer) %>% summarise(group=min(group)) %>%
      inner_join(With_BMI_12m) %>%
  group_by(Minus30, Primary_Cancer , group) %>% count() %>%
  spread(key=group, value=n))
    

data.frame(temp %>% 
   inner_join(New_Primary_Cancer_Box) %>%
  filter(Month_Min>=metastasis_onset) %>%
  filter(Month_Min<=(metastasis_onset+12)) %>%
  select(patid, Max, Min, Primary_Cancer) %>% distinct() %>%
    mutate(Drop=(Max-Min)/Max) %>%
    mutate(group=ifelse(Drop>=0.02&Min<20,1,
                        ifelse(Drop>=0.15,2,
                               ifelse(Drop>=0.1,3,
                                      ifelse(Drop>=0.05,4,5))))) %>%
    group_by(patid, Primary_Cancer) %>% summarise(group=min(group)) %>%
  group_by( Primary_Cancer , group) %>% count() %>%
  spread(key=group, value=n))
    


# ---------
# Time from metastasis to cachexia Rx BMI<30 vs BMI>30 --------------


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month) %>% distinct()


data.frame(New_Primary_Cancer_Box %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient")) %>%
  filter(Month>=metastasis_onset) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  mutate(diff=Month-metastasis_onset) %>%
    inner_join(With_BMI_12m) %>%
  group_by(Minus30, Primary_Cancer) %>% summarise(mean=mean(diff))) %>%
  spread(key=Minus30, value=mean)
  


# ------------


# Time from metastasis to cachexia 10% vs other fearon BMI<30 vs BMI>30  -------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





data.frame(temp %>% filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>% filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))




data.frame(temp %>% filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(First_CCh>=metastasis_onset) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>%  inner_join(New_Primary_Cancer_Box) %>%
             filter(Month_Min>=metastasis_onset) %>%
                          filter(Month_Min<=(12+metastasis_onset)) %>%
             filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>% 
    select(patid,Month_Min,metastasis_onset,Primary_Cancer) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))



data.frame(temp %>%  inner_join(New_Primary_Cancer_Box) %>%
             filter(Month_Min>=metastasis_onset) %>%
                          filter(Month_Min<=(12+metastasis_onset)) %>%
             filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>% 
    select(patid,Month_Min,metastasis_onset,Primary_Cancer) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
    inner_join(With_BMI_12m ) %>%
    group_by( Minus30, Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh))) %>%
  spread(key=Minus30, value=mean)





data.frame(temp %>%  inner_join(New_Primary_Cancer_Box) %>%
             filter(Month_Min>=metastasis_onset) %>%
                          filter(Month_Min<=(12+metastasis_onset)) %>%
             filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>% 
    select(patid,Month_Min,metastasis_onset,Primary_Cancer) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
    inner_join(With_BMI_12m ) %>%
    group_by( Minus30, Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh))) %>%
  spread(key=Minus30, value=mean)







# ---------
# Time mets to cachexia Dx BMI<30 vs BMI>30  -------------


PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS Measures/PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

PONS_Demographics <- fread("Diagnosed Population 1.0/PONS_Time_Series_Groups.txt", sep="\t")

unique(PONS_Demographics$Status)

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed=as.character(claimed))
temp <- temp %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)


# fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
# fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)


temp <- temp %>% ungroup() %>% filter(Month_Min>=Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6)  ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))
temp <- temp %>% mutate(Drop90_6m=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Pre_CCh=ifelse( (Min<=(Max*0.98)) & (Min>=(Max*0.95)) & (Month_Min>Month_Max) &  (Month_Min-Month_Max<=6) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Pre_Cachexia_Pred <- temp %>% filter(Pre_CCh==1) %>% select(patid) %>% distinct()#  %>% anti_join(New_Cachexia_Pred)



length(unique(temp$patid))
temp %>% filter(Month_Min>=37) %>% select(patid) %>% distinct()

length(unique(New_Cachexia_Pred$patid))/228536

length(unique(New_Cachexia_Pred$patid))
length(unique(New_Pre_Cachexia_Pred$patid))

temp <- temp %>% 
  left_join(temp %>% select(patid, Min) %>% distinct() %>% group_by(patid) %>% summarise(Abs_Min=min(Min)) %>% ungroup())

temp <- temp %>% mutate(Above_Min_20=ifelse(Min> (Abs_Min/0.80), 1,0))

temp <- temp %>% mutate(CCh=ifelse(Drop95==1 | Drop90==1 | Drop2_20==1,1,0))


temp %>% select(patid) %>% distinct() %>%
  anti_join(New_Cachexia_Pred) %>% anti_join(New_Pre_Cachexia_Pred)




temp <- data.frame(
  temp %>%
  left_join(
    temp %>% filter(CCh==1) %>% select(patid, Month_Min, Min) %>% distinct() %>%
  arrange(patid, Month_Min) %>% group_by(patid) %>%
  mutate(Max_BMI_Flag_CCh = cummax(Min)) %>% ungroup()  %>% select(-Min)
  ) %>% arrange(patid, Month_Min) %>% group_by(patid) %>%
  fill(Max_BMI_Flag_CCh, .direction = "down") %>%
  arrange(patid, Month_Min)
  )


temp <- temp %>% mutate(Recovered=ifelse(Min>Max_BMI_Flag_CCh,1,0))


temp <- temp %>% arrange(patid, Month_Min, Month_Max) %>%
  group_by(patid) %>% 
  mutate(CUM_CCh=cumsum(CCh)) %>% mutate(CUM_CCh=ifelse(CUM_CCh>0,1,0)) %>%
  mutate(Delta=ifelse(Min>lag(Min), 1,
                      ifelse(Min<lag(Min), -1, 0))) %>% ungroup()


temp <- temp %>% 
  left_join(
    temp %>% select(patid, Month_Min, Min) %>% distinct() %>%
      mutate(Month_Min=lead(Month_Min)) %>% rename("Lag"="Min")  %>% drop_na() %>% distinct() 
  ) 


temp <- temp %>% mutate(Lag=ifelse(is.na(Lag), Min, Lag))


temp <- temp %>% mutate(Delta=ifelse(Min>Lag, 1, ifelse(Min<Lag,-1,0)))


# temp <- temp %>% mutate(Delta=ifelse(is.na(Delta),0,Delta)) 

temp <- temp %>% mutate(Recovered=ifelse(is.na(Recovered),0,Recovered)) 



New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)




PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





data.frame(temp %>% filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>% filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
  filter(ElapsedCCh>(-12)) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))




data.frame(temp %>%        
             inner_join(New_Primary_Cancer_Box) %>%
             filter(Month_Min>=metastasis_onset) %>%
             filter(Drop90 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(First_CCh>=metastasis_onset) %>%
      filter(ElapsedCCh<=12) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))


data.frame(temp %>% 
              inner_join(New_Primary_Cancer_Box) %>%
             filter(Month_Min>=metastasis_onset) %>%
             filter(Drop2_20 ==1|Drop95 ==1) %>% 
  group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>%  select(patid,Month_Min) %>% distinct() %>%
  rename("First_CCh"="Month_Min") %>%
  inner_join(New_Primary_Cancer_Box) %>%
      mutate(ElapsedCCh=(First_CCh-metastasis_onset)) %>%
 filter(First_CCh>=metastasis_onset) %>%
   filter(ElapsedCCh<=12) %>%
  group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))






# Time to Cachexia Dx

PONS_Demographics_cachexia_dx <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>%  select(patid, cachexia_onset) %>% drop_na()
setDT(PONS_Demographics_cachexia_dx)
PONS_Demographics_cachexia_dx[, cachexia_onset := as.character(cachexia_onset)][, cachexia_onset := substr(cachexia_onset, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% left_join(Months_lookup, by=c("cachexia_onset" = "Month"))
PONS_Demographics_cachexia_dx <- PONS_Demographics_cachexia_dx %>% select(-cachexia_onset) %>% rename("cachexia_onset"="Exact_Month")






data.frame(New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_cachexia_dx) %>%
  mutate(ElapsedCCh=(cachexia_onset-metastasis_onset)) %>%
    filter(cachexia_onset>=metastasis_onset) %>%
    group_by( Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh)))



data.frame(New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_cachexia_dx) %>%
  mutate(ElapsedCCh=(cachexia_onset-metastasis_onset)) %>%
    filter(cachexia_onset>=metastasis_onset) %>%
    inner_join(With_BMI_12m) %>%
    group_by( Minus30, Primary_Cancer) %>% 
    summarise(mean=mean(ElapsedCCh))) %>%
  spread(key=Minus30, value=mean)
  


# ---------
# Anticancer Drugs Use 1 year post mets BMI<30 vs BMI>30 --------------


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





denom <- data.frame(New_Primary_Cancer_Box %>% 
                      inner_join(With_BMI_12m %>% filter(is.na(Minus30)) %>% select(patid)) %>%
                      group_by(Primary_Cancer) %>% summarise(n=sum(weight))) %>% rename("denom"="n")




CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% left_join(CAN_Drug_Histories %>% select(-disease), by=c("patid"="patient"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()





CAN_Drug_Histories <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories) %>% 
  filter(Month>=metastasis_onset) %>%
  filter(Month<=(metastasis_onset+12)) %>% select(-c(metastasis_onset, Month)) %>% distinct()


CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>%  distinct()

PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 3.0/PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass) %>% distinct()

unique(CAN_Drug_Histories$chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(chemo_class=="Alkylating Agent"|
                                chemo_class=="Immuno/Targeted"|
                                chemo_class=="Hormonal Therapy"|
                                chemo_class=="Biologic"|
                                chemo_class=="Radio"|
                                  chemo_class=="Antimicrotubule Agent"|
                                  chemo_class=="Platinum agent"|
                                  chemo_class=="Antimetabolites"|
                                  chemo_class=="Topoisomerase Inhibitor"|
                                  chemo_class=="Other Antineoplastics"|
                                  chemo_class=="PD1/PDL1"|
                                  chemo_class=="Surgery Inpatient")



denom %>% left_join(
  CAN_Drug_Histories %>% select(-Drugs) %>% distinct() %>% group_by(Primary_Cancer, chemo_class) %>%
  summarise(num=sum(weight))
)  %>% mutate(perc=num/denom) %>% select(-c(denom, num)) %>%
  spread(key=chemo_class, value=perc)


denom %>% left_join(
  CAN_Drug_Histories %>% inner_join(With_BMI_12m %>% filter(is.na(Minus30)) %>% select(patid)) %>%
    select(-Drugs) %>% distinct() %>% group_by(Primary_Cancer, chemo_class) %>%
  summarise(num=sum(weight))
  ) %>%
  mutate(perc=num/denom) %>% select(-c(denom, num)) %>%
  spread(key=chemo_class, value=perc)




# ------------


# Anticancer Lines Use 1 year post mets BMI<30 vs BMI>30  --------------


New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()





denom <- data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))) %>% rename("denom"="n")




CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% left_join(CAN_Drug_Histories %>% select(-disease), by=c("patid"="patient"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()


CAN_Drug_Histories <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories) %>% 
  filter(Month>=metastasis_onset) %>%
  filter(Month<=(metastasis_onset+12)) %>% select(-c(metastasis_onset)) %>% distinct()

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>%  distinct()

PONS_Ingredients_JN_ChemoClass <- fread("CAN Analysis Results 3.0/PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass) %>% distinct()

unique(CAN_Drug_Histories$chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(chemo_class=="Alkylating Agent"|
                                chemo_class=="Immuno/Targeted"|
                                chemo_class=="Hormonal Therapy"|
                                chemo_class=="Biologic"|
                                chemo_class=="Radio"|
                                  chemo_class=="Antimicrotubule Agent"|
                                  chemo_class=="Platinum agent"|
                                  chemo_class=="Antimetabolites"|
                                  chemo_class=="Topoisomerase Inhibitor"|
                                  chemo_class=="Other Antineoplastics"|
                                  chemo_class=="PD1/PDL1"|
                                  chemo_class=="Surgery Inpatient")


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-chemo_class) %>%
  arrange(patid, weight, Primary_Cancer, Month) %>%
  group_by(patid, weight, Primary_Cancer, Month) %>% 
  mutate(Drugs=paste0(Drugs, collapse=",")) %>% distinct()


data.frame(CAN_Drug_Histories %>% ungroup() %>%
  select(-Month) %>% distinct %>%
   group_by(patid, weight, Primary_Cancer) %>% count() %>% ungroup() %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(n)))



data.frame(CAN_Drug_Histories %>% ungroup() %>%
  select(-Month) %>% distinct %>%
   group_by(patid, weight, Primary_Cancer) %>% count() %>% ungroup() %>%
    inner_join(With_BMI_12m) %>%
  group_by(Minus30, Primary_Cancer) %>% summarise(mean=mean(n))) %>%
  spread(key=Minus30, value=mean)


# ------------


# Cachexia Rx to palliative care - anticancer lines  BMI<30 vs BMI>30 --------------

CaxPts_ICD10_Z515 <- fread("Diagnosed Population 1.0/CaxPts_ICD10_Z515.txt", sep=",")


Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

CaxPts_ICD10_Z515 <- CaxPts_ICD10_Z515 %>% select(patid, fst_dt) %>% distinct() %>% group_by(patid) %>%
  summarise(fst_dt=min(fst_dt)) %>% ungroup() %>%
  mutate(fst_dt=substr(as.character(fst_dt), 1L, 7L)) %>%
  inner_join(Months_lookup, by=c("fst_dt" = "Month")) %>%
  select(-fst_dt) %>% rename("fst_dt"="Exact_Month")




New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()



New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CaxPts_ICD10_Z515)




CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CAN Analysis Results 2.2/CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Treat)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month) %>% distinct()


data.frame(New_Primary_Cancer_Box %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient")) %>%
  filter(Month>=metastasis_onset) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  mutate(diff=fst_dt -Month) %>%
  group_by(Primary_Cancer) %>% summarise(mean=mean(diff)))



data.frame(New_Primary_Cancer_Box %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient")) %>%
  filter(Month>=metastasis_onset) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  mutate(diff=fst_dt -Month) %>%
    inner_join(With_BMI_12m) %>%
  group_by(Minus30,Primary_Cancer) %>% summarise(mean=mean(diff))) %>%
  spread(key=Minus30, value=mean)


# ---------
# Cachexia Rx Rate 12 months after metastasis Extra Q8 Nag ----
New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))
unique(New_Primary_Cancer_Box$Primary_Cancer)

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")

Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")


New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer") %>%
  select(patid) %>% # 53553 
inner_join(
  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large")
) # 51276
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))


data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")





PONS_Demographics <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(-c(Primary, died))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()


temp_min <- fread("Diagnosed Population 1.0/MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


temp_min <- temp_min %>% gather(Month, BMI, `1`:`60`) %>% drop_na()


With_BMI_12m <- temp_min %>% mutate(Month=as.numeric(Month)) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, metastasis_onset)  %>% distinct()) %>%
  filter( (Month>=metastasis_onset)&(Month-metastasis_onset<=12) ) %>% select(patid) %>% distinct()


With_BMI_minus30 <- temp_min %>% mutate(Month=as.numeric(Month)) %>%
  inner_join(New_Primary_Cancer_Box %>% select(patid, metastasis_onset)  %>% distinct()) %>%
  filter( (Month>=metastasis_onset)&(Month-metastasis_onset<=12) ) %>% filter(BMI<30) %>% select(patid) %>% distinct()


data.frame(New_Primary_Cancer_Box %>% inner_join(With_BMI_12m) %>% left_join(With_BMI_minus30 %>% mutate(Minus30="Minus30")) %>%
  group_by(Primary_Cancer, Minus30) %>% summarise(n=sum(weight)) %>% ungroup() %>%
  spread(key=Minus30, value=n) %>% mutate(perc=Minus30/(Minus30+`<NA>`)))


With_BMI_12m <- With_BMI_12m %>% left_join(With_BMI_minus30 %>% mutate(Minus30="Minus30")) 
















New_Primary_Cancer_Box <- fread("Diagnosed Population 1.0/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Primary_Cancer=ifelse(Primary_Cancer=="Respiratory Cancer", "Head_Neck Cancer",
                                                                                  ifelse(Primary_Cancer=="Head Cancer", "Head_Neck Cancer", Primary_Cancer)))

PONS_Dossiers <- fread("Diagnosed Population 1.0/PONS Dossiers.txt")
Codes_Intestinal_Colon <- fread("Diagnosed Population 1.0/Codes_Intestinal_Colon.csv")
 
Start_colon <-  PONS_Dossiers %>% inner_join(Codes_Intestinal_Colon) %>%
    group_by(patid) %>% filter(earliest==min(earliest)) %>%
    select(patid, site) %>% distinct() %>% ungroup() %>%
    filter(site=="large") %>% select(patid) %>% mutate(Primary="Colon")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Start_colon) %>%
  mutate(Primary=ifelse(Primary_Cancer=="Intestinal Cancer"&is.na(Primary), "Small Intestine",
                        ifelse(Primary=="Colon", "Colon", "Other"))) %>%
  mutate(Primary_Cancer=ifelse(Primary_Cancer=="Intestinal Cancer" & Primary=="Colon", "Colon Cancer",
                               ifelse(Primary_Cancer=="Intestinal Cancer", "Intestinal Cancer", Primary_Cancer)))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)



PONS_Demographics_mets <- fread("Diagnosed Population 1.0/PONS Demographics.txt")
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(patid, weight, cancer_metastasis)
setDT(PONS_Demographics_mets)
PONS_Demographics_mets[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Diagnosed Population 1.0/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_mets <- PONS_Demographics_mets %>% left_join(Months_lookup, by=c("cancer_metastasis" = "Month"))
PONS_Demographics_mets <- PONS_Demographics_mets %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_mets)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()



With_BMI_12m <- With_BMI_12m %>% left_join(With_BMI_minus30 %>% mutate(Minus30="Minus30")) %>% mutate(Minus30=ifelse(is.na(Minus30), "Above", "Minus30"))



denom <- data.frame(New_Primary_Cancer_Box %>% inner_join(With_BMI_12m) %>% group_by(Minus30, Primary_Cancer) %>% summarise(n=sum(weight))) %>% rename("denom"="n")


CAN_Drug_Histories <- fread("CAN Analysis Results 3.0/CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% left_join(CAN_Drug_Histories %>% select(-disease), by=c("patid"="patient"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()


CAN_Drug_Histories <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories) %>% 
  filter(Month>=metastasis_onset) %>%
  filter(Month<=(metastasis_onset+12)) %>% select(-c(metastasis_onset)) %>% distinct()

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>%  distinct()


PONS_Ingredients <- fread("CAN Analysis Results 3.0/PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

PONS_Ingredients <- PONS_Ingredients %>% filter(generic_name %in% c("Nutrition Therapy", "Dronabinol", "Oxandrolone", "Megestrol", "Cyproheptadine", "Medroxyprogesterone"))

string_nutrition  <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_nutrition,Drugs)) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients %>% select(molecule, generic_name), by=c("Drugs"="molecule"))

CAN_Drug_Histories <- CAN_Drug_Histories  %>% drop_na()



num <- CAN_Drug_Histories %>% select(patid, weight, Primary_Cancer) %>% distinct() %>%
  inner_join(With_BMI_12m) %>% group_by(Minus30, Primary_Cancer) %>% 
  summarise(n=sum(weight))



denom %>% left_join(num) %>% mutate(perc=n/denom)




num <- CAN_Drug_Histories %>% select(patid, weight, Primary_Cancer, generic_name) %>% distinct() %>%
  inner_join(With_BMI_12m) %>% group_by(Minus30, Primary_Cancer, generic_name) %>% 
  summarise(n=sum(weight))


denom %>% left_join(num) %>% mutate(perc=n/denom) %>%
  select(Minus30, Primary_Cancer, generic_name, perc) %>%
  spread(key=generic_name, value=perc)


# ---------

