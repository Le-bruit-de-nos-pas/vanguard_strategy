
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
library(survminer)
library(survival)
library(ggsurvfit)
library(ggrepel)
#library(lessR)

options(scipen = 999)

# Summary Demographics -------------------------------------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer != "-" & Primary_Cancer != "Unspecified Cancer", ]

sum(New_Primary_Cancer_Box$weight)  # 22602908

length(unique(New_Primary_Cancer_Box$patid)) # 741085

# Primary Cancers

sizing_df <- arrange(New_Primary_Cancer_Box[, .(n = .N), by = Primary_Cancer], -n)


PONS_Demographics <- fread("PONS Demographics.txt")

length(unique(PONS_Demographics$patid))

PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_onset)) %>% filter(cancer_onset<="2020-07-31")

PONS_Demographics <- PONS_Demographics[ , .(patid, age, gender, cancer_metastasis, cachexia_onset)]

New_Primary_Cancer_Box <- merge(New_Primary_Cancer_Box, PONS_Demographics, by=c("patid"), all=FALSE)

length(unique(New_Primary_Cancer_Box$patid)) # 672664


sizing_df <- arrange(New_Primary_Cancer_Box[, .(n = .N), by = Primary_Cancer], -n)

arrange(sizing_df, Primary_Cancer)

# Age

age_df <- New_Primary_Cancer_Box[, .(age = round(mean(age),2)), by = Primary_Cancer]

sizing_df <- merge(sizing_df, age_df, by=c("Primary_Cancer"), all.x=TRUE)

# Gender 

perc_male <- New_Primary_Cancer_Box[, .(n = .N), by = .(Primary_Cancer, gender)][
  , dcast(.SD, Primary_Cancer ~ gender, value.var = "n")][
  , .(Primary_Cancer, perc_male = round(M / (M + F),2))][, .(Primary_Cancer, perc_male)]

sizing_df <- merge(sizing_df, perc_male, by=c("Primary_Cancer"), all.x=TRUE)


# % Death

perc_death <- New_Primary_Cancer_Box[, .(n = .N), by = .(Primary_Cancer, died)][
  , dcast(.SD, Primary_Cancer ~ died, value.var = "n")][
  , .(Primary_Cancer, perc_death = round(Y / (Y + N),2))][, .(Primary_Cancer, perc_death)]

sizing_df <- merge(sizing_df, perc_death, by=c("Primary_Cancer"), all.x=TRUE)


# % Metastatic

New_Primary_Cancer_Box[, cancer_metastasis := ifelse(is.na(cancer_metastasis), "N", "Y")]

perc_metastatic <- New_Primary_Cancer_Box[, .(n = .N), by = .(Primary_Cancer, cancer_metastasis )][
  , dcast(.SD, Primary_Cancer ~ cancer_metastasis , value.var = "n")][
  , .(Primary_Cancer, perc_metastatic = round(Y / (Y + N),2))][, .(Primary_Cancer, perc_metastatic)]

sizing_df <- merge(sizing_df, perc_metastatic, by=c("Primary_Cancer"), all.x=TRUE)


# % Cachexia Dx

New_Primary_Cancer_Box[, cachexia_onset := ifelse(is.na(cachexia_onset), "N", "Y")]

perc_cachexia_dx <- New_Primary_Cancer_Box[, .(n = .N), by = .(Primary_Cancer, cachexia_onset )][
  , dcast(.SD, Primary_Cancer ~ cachexia_onset , value.var = "n")][
  , .(Primary_Cancer, perc_cachexia_dx = round(Y / (Y + N),5))][, .(Primary_Cancer, perc_cachexia_dx)]

sizing_df <- merge(sizing_df, perc_cachexia_dx, by=c("Primary_Cancer"), all.x=TRUE)

sizing_df

arrange(
  New_Primary_Cancer_Box[cachexia_onset=="Y", .(n = .N), by = .(Primary_Cancer, cachexia_onset )], Primary_Cancer 
  )

arrange(
  New_Primary_Cancer_Box[gender=="M", .(n = .N), by = .(Primary_Cancer, gender )], Primary_Cancer 
  )

# -----------------------------------------------------------------------------------------------------------

# Cachexia population estimate: all possible drops  --------------------------------------------------------

# Target cancers

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

fwrite(Pats_to_track_BMI, "Pats_to_track_BMI.txt")


data.frame(
  PONS_Measures %>% select(patid) %>% distinct() %>%
  left_join(
    Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis )
    ) %>% 
    group_by(diagnosis, cancer_metastasis ) %>% count()
  )


At_risk_population <- PONS_Measures %>% select(patid) %>% distinct() %>%
  left_join(
    Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis)
    ) 


fwrite(At_risk_population, "At_risk_population.txt")



# Convert BMI records to wide format, compare each BMI with each subsequent record

temp <- PONS_Measures %>% select(patid, claimed, value)

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )


temp <- temp %>% mutate(claimed=as.character(claimed)) %>% mutate(claimed=str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(
  Months_lookup, by=c("claimed"="Month")
  ) %>% select(patid, value, Exact_Month) %>% distinct()

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=max(value))
temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n=min(value))

temp_max <- temp_max %>% ungroup() %>% spread(key=Exact_Month, value=n)
temp_min <- temp_min %>% ungroup() %>% spread(key=Exact_Month, value=n)

fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t") ; fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T) ; temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max" ; names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min" ; names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min) %>% ungroup() %>% filter(Month_Min>Month_Max)

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6) ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12) ,1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

fwrite(temp, "All_drops.txt")


# Pick one option

New_Cachexia_Pred <- temp %>% filter(Drop90==1 | Drop95==1 | Drop2_20==1) %>% select(patid, Drop95, Drop90, Drop2_20) %>% distinct()

Drop95 <- New_Cachexia_Pred %>% filter(Drop95==1) %>% select(patid) %>% distinct()
Drop90 <- New_Cachexia_Pred %>% filter(Drop90==1) %>% select(patid) %>% distinct()
Drop2_20 <- New_Cachexia_Pred %>% filter(Drop2_20==1) %>% select(patid) %>% distinct()

fwrite(Drop95, "Drop95.txt")
fwrite(Drop90, "Drop90.txt")
fwrite(Drop2_20, "Drop2_20.txt")

New_Cachexia_Pred <- temp %>% filter(Drop90==1 | Drop95==1 | Drop2_20==1) %>% select(patid) %>% distinct()


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


fwrite(CachexiaPats_ALL_NEW, "CachexiaPats_ALL_NEW.txt")
CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")


# Exclude BMI>30 
# 
# PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
# 
# Pats_BMI_30 <- PONS_Measures %>% filter(test=="BMI") %>% select(patid, value) %>% distinct() %>% 
#   group_by(patid) %>%  filter(value==min(value))  %>% ungroup() %>% 
#   filter(value>=30) %>% select(patid) %>% distinct()
# 
# fwrite(Pats_BMI_30, "Pats_BMI_30.txt")
# Pats_BMI_30 <- fread("Pats_BMI_30.txt")
# 
# data.frame(
#   CachexiaPats_ALL_NEW %>% anti_join(Pats_BMI_30) %>% group_by(diagnosis, cancer_metastasis ) %>% count()
#   )


data.frame(
  CachexiaPats_ALL_NEW %>% group_by(diagnosis, cancer_metastasis ) %>% count()
  )


# ----------------------------------------------------------------------------------

# Min, Max, Last BMI per patient: Dx, Pred, none -----------------------------------

# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1 | Drop95==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")

Min_Max <- PONS_Measures %>% select(patid, min, max) %>% distinct()

last <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==min(value)) %>% slice(1) %>% select(patid, value)


Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=mean(min, na.rm=T), max=mean(max, na.rm=T))

#   group   min   max
# 1 Dx     19.5  25.3
# 2 None   28.4  30.6
# 3 Pred   26.2  32.2

Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=sd(min, na.rm=T), max=sd(max, na.rm=T))

#   group   min   max
# 1 Dx     4.68  5.76
# 2 None   6.12  6.67
# 3 Pred   6.19  7.72


Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=median(min, na.rm=T), max=median(max, na.rm=T))

#   group   min   max
# 1 Dx     18.7  24.4
# 2 None   27.4  29.4
# 3 Pred   25.4  30.9


Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=IQR(min, na.rm=T), max=IQR(max, na.rm=T))

#  group   min   max
# 1 Dx     5.43  6.8 
# 2 None   7.26  7.9 
# 3 Pred   7.75  9.21

temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) 

kruskal.test(min ~ group, data = temp)
# 
# 	Kruskal-Wallis rank sum test
# 
# data:  min by group
# Kruskal-Wallis chi-squared = 18985, df = 2, p-value < 0.00000000000000022

pairwise.wilcox.test(temp$min, temp$group, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  temp$min and temp$group 
# 
#      Dx                  None               
# None <0.0000000000000002 -                  
# Pred <0.0000000000000002 <0.0000000000000002
# 
# P value adjustment method: bonferroni 

	
temp %>%  select(group, min) %>%
  filter(min<50 & min>10) %>%
   mutate(group=factor(group, levels=c("None" ,"Pred", "Dx"))) %>%
  ggplot(aes(group, min, colour=group, fill=group )) +
  #geom_jitter(width=0.1, height = 0.6, alpha=0.1, show.legend = FALSE, size = 0.1 ) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE,  outlier.alpha = NULL,show.legend = FALSE) +
  theme_minimal() +
  ylim(0,50) +
  xlab("\n  Cachexia Status") + ylab("Lowest BMI reached \n  \n") +
  scale_fill_manual(values=c("honeydew4", "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c("honeydew4", "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




temp %>% group_by(cancer_metastasis) %>% summarise(n=median(min, na.rm=T))

#   cancer_metastasis     n
# 1                 0  26.5
# 2                 1  25.4

temp %>% group_by(cancer_metastasis) %>% summarise(n=mean(min, na.rm=T))

#   cancer_metastasis     n
# 1                 0  27.4
# 2                 1  26.2

temp %>% group_by(cancer_metastasis) %>% summarise(n=sd(min, na.rm=T))

#   cancer_metastasis     n
# 1                 0  6.40
# 2                 1  6.15

wilcox.test(temp$min[temp$cancer_metastasis==0], temp$min[temp$cancer_metastasis==1])

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  temp$min[temp$cancer_metastasis == 0] and temp$min[temp$cancer_metastasis == 1]
# W = 10551832708, p-value < 0.00000000000000022
# alternative hypothesis: true location shift is not equal to 0


temp %>%  select(cancer_metastasis, min) %>%
  filter(min<50 & min>10) %>%
  mutate(cancer_metastasis=ifelse(cancer_metastasis==0, "None", "Metastatic")) %>%
   mutate(cancer_metastasis=factor(cancer_metastasis, levels=c("None" ,"Metastatic"))) %>%
  ggplot(aes(cancer_metastasis, min, colour=cancer_metastasis, fill=cancer_metastasis )) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE, show.legend = FALSE) +
  theme_minimal() +
  ylim(0,50) +
  xlab("\n  Metastasis Status") + ylab("Lowest BMI reached \n  \n") +
  scale_fill_manual(values=c( "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c( "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))







temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) 
temp <- temp %>% mutate(drop=100*(min-max)/max)



temp %>% group_by(group) %>% summarise(drop=mean(drop, na.rm=T))

#   group   drop
# 1 Dx    -22.0 
# 2 None   -6.82
# 3 Pred  -17.8 

temp %>% group_by(group) %>% summarise(drop=sd(drop, na.rm=T))

#   group  drop
# 1 Dx    12.1 
# 2 None   5.54
# 3 Pred   9.47

kruskal.test(drop ~ group, data = temp)



# 	Kruskal-Wallis rank sum test
# 
# data:  drop by group
# Kruskal-Wallis chi-squared = 114113, df = 2, p-value < 0.00000000000000022

pairwise.wilcox.test(temp$drop, temp$group, p.adjust.method = "bonferroni")

#	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

# data:  temp$min and temp$group 
# 
#      Dx                   None                
# None < 0.0000000000000002 -                   
# Pred 0.0000000000000002             < 0.0000000000000002
# 
# P value adjustment method: bonferroni 

	
temp %>%  select(group, drop) %>%
  filter(drop>(-50)) %>%
  mutate(group=factor(group, levels=c("None" ,"Pred", "Dx"))) %>%
  ggplot(aes(group, drop, colour=group, fill=group )) +
  #geom_jitter(width=0.2, height = 0.6, alpha=0.3, show.legend = FALSE, size = 0.1 ) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE, show.legend = FALSE) +
  theme_minimal() +
  #ylim(0,75) +
  xlab("\n  Cachexia Status") + ylab("Highest BMI drop experienced \n  \n") +
  scale_fill_manual(values=c("honeydew4", "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c("honeydew4", "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




temp %>% group_by(cancer_metastasis) %>% summarise(n=mean(drop, na.rm=T))

#   cancer_metastasis     n
# 1                 0 -13.0
# 2                 1 -15.3

temp %>% group_by(cancer_metastasis) %>% summarise(n=sd(drop, na.rm=T))

#   cancer_metastasis     n
# 1                 0  9.50
# 2                 1 10.4 

wilcox.test(temp$drop[temp$cancer_metastasis==0], temp$drop[temp$cancer_metastasis==1])



temp %>%  select(cancer_metastasis, drop) %>%
  filter(drop>(-50)) %>%
  mutate(cancer_metastasis=ifelse(cancer_metastasis==0, "None", "Metastatic")) %>%
   mutate(cancer_metastasis=factor(cancer_metastasis, levels=c("None" ,"Metastatic"))) %>%
  ggplot(aes(cancer_metastasis, drop, colour=cancer_metastasis, fill=cancer_metastasis )) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE, show.legend = FALSE) +
  theme_minimal() +
  xlab("\n  Metastasis Status") + ylab("Highest BMI drop experienced \n  \n") +
  scale_fill_manual(values=c( "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c( "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



	
	
data.frame(
  Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(last)  %>% 
  group_by(diagnosis, group) %>% summarise(last=mean(value, na.rm=T)) %>% spread(key=group, value=last)
  )

temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(last) 

temp %>% group_by(group) %>% summarise(last=mean(value, na.rm=T))

# 1 Dx     21.1
# 2 None   29.6
# 3 Pred   28.6

temp %>% group_by(group) %>% summarise(last=sd(value, na.rm=T))

# #   group  last
# 1 Dx     4.98
# 2 None   6.46
# 3 Pred   7.03

Drop2_20 <- fread("Drop2_20.txt") ; Drop2_20$DropG <- "Drop2_20"
Drop90 <- fread("Drop90.txt") ; Drop90$DropG <- "Drop90"
Drop95 <- fread("Drop95.txt") ; Drop95$DropG <- "Drop95"

DropG <- Drop2_20 %>% bind_rows(Drop90) %>% bind_rows(Drop95)


temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) 

temp <- temp %>% left_join(DropG) %>% drop_na()

kruskal.test(min ~ DropG, data = temp)

# 	Kruskal-Wallis rank sum test
# 
# data:  min by DropG
# Kruskal-Wallis chi-squared = 53682, df = 2, p-value < 0.00000000000000022

pairwise.wilcox.test(temp$min, temp$DropG, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  temp$min and temp$DropG 
# 
#        Drop2_20            Drop90             
# Drop90 <0.0000000000000002 -                  
# Drop95 <0.0000000000000002 <0.0000000000000002
# 
# P value adjustment method: bonferroni 


temp %>% group_by(DropG) %>% summarise(n=mean(min, na.rm=T))

#   DropG        n
# 1 Drop2_20  1.72
# 2 Drop90    6.17
# 3 Drop95    6.24

temp %>% group_by(DropG) %>% summarise(n=sd(min, na.rm=T))

#   DropG        n
# 1 Drop2_20  1.72
# 2 Drop90    6.17
# 3 Drop95    6.24


temp %>%  select(DropG, min) %>%
  filter(min<50 & min>10) %>%
  mutate(DropG=ifelse(DropG=="Drop2_20", ">2% BMI<20",
                      ifelse(DropG=="Drop90", ">10% 12m", ">5% 6m"))) %>%
   mutate(DropG=factor(DropG, levels=c(">2% BMI<20" ,">10% 12m", ">5% 6m"))) %>%
  ggplot(aes(DropG, min, colour=DropG, fill=DropG )) +
  #geom_jitter(width=0.1, height = 0.6, alpha=0.1, show.legend = FALSE, size = 0.1 ) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE,  outlier.alpha = NULL,show.legend = FALSE) +
  theme_minimal() +
  ylim(0,50) +
  xlab("\n Observed-Cachexia Status") + ylab("Lowest BMI reached \n  \n") +
  scale_fill_manual(values=c("honeydew4", "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c("honeydew4", "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


temp <- temp %>% mutate(drop=100*(min-max)/max)


temp %>% group_by(DropG) %>% summarise(drop=mean(drop, na.rm=T))

#   DropG     drop
# 1 Drop2_20 -23.1
# 2 Drop90   -22.3
# 3 Drop95   -18.2

temp %>% group_by(DropG) %>% summarise(drop=sd(drop, na.rm=T))

#   DropG     drop
# 1 Drop2_20 12.2 
# 2 Drop90    9.26
# 3 Drop95    9.62

kruskal.test(drop ~ DropG, data = temp)

# 	Kruskal-Wallis rank sum test
# 
# data:  drop by DropG
# Kruskal-Wallis chi-squared = 18580, df = 2, p-value < 0.00000000000000022

pairwise.wilcox.test(temp$drop, temp$DropG, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  temp$drop and temp$DropG 
# 
#        Drop2_20            Drop90             
# Drop90 0.0052              -                  
# Drop95 <0.0000000000000002 <0.0000000000000002
# 
# P value adjustment method: bonferroni 




temp %>%  select(DropG, drop) %>%
  filter(drop>(-60)) %>%
  mutate(DropG=ifelse(DropG=="Drop2_20", ">2% BMI<20",
                      ifelse(DropG=="Drop90", ">10% 12m", ">5% 6m"))) %>%
   mutate(DropG=factor(DropG, levels=c(">2% BMI<20" ,">10% 12m", ">5% 6m"))) %>%
  ggplot(aes(DropG, drop, colour=DropG, fill=DropG )) +
  #geom_jitter(width=0.2, height = 0.6, alpha=0.3, show.legend = FALSE, size = 0.1 ) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE, show.legend = FALSE) +
  theme_minimal() +
  #ylim(0,75) +
  xlab("\n  Observed-Cachexia Status") + ylab("Highest BMI drop experienced \n  \n") +
  scale_fill_manual(values=c("honeydew4", "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c("honeydew4", "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# ---------------------------------------

# Over time relative to cancer Dx ----------------

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1 | Drop95==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")

PONS_Measures <- PONS_Measures %>% select(patid, claimed, value) %>% distinct()

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed)) %>% mutate(claimed=str_sub(claimed, 1L, 7L))

PONS_Measures <- PONS_Measures %>% left_join(
  Months_lookup, by=c("claimed"="Month")
  ) %>% select(patid, value , Exact_Month) %>% distinct() 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_onset=as.character(cancer_onset)) %>% mutate(cancer_onset=str_sub(cancer_onset, 1L, 7L))
PONS_Demographics <- PONS_Demographics %>% left_join(
  Months_lookup, by=c("cancer_onset"="Month")
  ) %>% select(-cancer_onset) %>% distinct() %>% rename("cancer_onset"="Exact_Month")

PONS_Measures <- PONS_Demographics %>% inner_join(PONS_Measures) %>% mutate(Elapsed=Exact_Month-cancer_onset) %>% select(-c(cancer_onset, Exact_Month))

Groups <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia)

Groups <- Groups %>% inner_join(PONS_Measures)

Groups <- Groups %>% group_by(patid, group, Elapsed) %>% summarise(value=mean(value))

Groups %>%
  filter(Elapsed<=24 & Elapsed >= (-24)) %>%
  ggplot(aes(Elapsed, value, fill=group, colour=group)) +
  geom_smooth()
 
# -----------
# Survival ---------------------------------------
# Target cancers

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

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("None", "Pred", "Dx"), legend.title="Cachexia Status",  
           palette=c("honeydew4", "midnightblue", "firebrick"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia Status", 
           risk.table.height=0.2)


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



sfit <- survfit(Surv(Survived, status)~group, data=temp)
sfit



summary(sfit)

ggsurvplot(sfit)

temp %>% group_by(group) %>% count() %>% rename("n1"="n") %>%
  left_join(temp %>% filter(status==1) %>% group_by(group) %>% count()
) %>% mutate(perc=n/n1)



ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           #legend.labs=c("Dx Metastatic", "Dx no mets",  "None Metastatic", "None no mets", "Pred Metastatic", "Pred no mets"), legend.title="Cachexia Status",  
           palette=c("#929baa", "#515967", "#17b7ba", "#1753ba", "coral2", "firebrick"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia & Metastasis Status", 
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



# ------------------------------------

# Probability of death as a function of minimum BMI or max drop ------------
# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1 | Drop95==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")

Min_Max <- PONS_Measures %>% select(patid, min, max) %>% distinct()

last <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==min(value)) %>% slice(1) %>% select(patid, value)

temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) 
temp <- temp %>% mutate(drop=100*(min-max)/max)



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, died)  %>% mutate(died=ifelse(died=="Y",1,0)) 
 
temp <- temp %>% left_join(PONS_Demographics)

temp %>%
  filter(min>10 & min <50) %>%
  mutate(group=factor(group, levels=c("None" ,"Pred", "Dx"))) %>%
  ggplot(aes(x=min, y=died, colour=group, fill=group)) + 
  ylim(0,1) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial)) +
  scale_fill_manual(values=c("honeydew4", "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c("honeydew4", "midnightblue", "firebrick"  )) +
  theme_minimal() +
  xlab("\n Lowest BMI reached") + ylab("Probability of having died \n")

temp %>%
  filter(min>10 & min <40) %>%
  ggplot(aes(x=min, y=died)) + 
  ylim(0,1) +
  stat_smooth(method="gam", se=TRUE, method.args = list(family=binomial)) +
  theme_minimal() +
  xlab("\n Lowest BMI reached") + ylab("Probability of having died \n")


temp %>%
  filter(drop>(-0)) %>%
  mutate(group=factor(group, levels=c("None" ,"Pred", "Dx"))) %>%
  ggplot(aes(x=drop, y=died, colour=group, fill=group)) + 
  ylim(0,1) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial)) +
  scale_fill_manual(values=c("honeydew4", "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c("honeydew4", "midnightblue", "firebrick"  )) +
  theme_minimal() +
  xlab("\n Highest % BMI drop") + ylab("Probability of having died \n")



temp %>%
  filter(drop>(-40)) %>%
  ggplot(aes(x=abs(drop), y=died)) + 
  ylim(0,1) +
  stat_smooth(method="gam", se=TRUE, method.args = list(family=binomial)) +
  theme_minimal() +
  xlab("\n Highest % BMI drop") + ylab("Probability of having died \n")


# ------------
# Overall prevalence ----------------------------------------------------------------------


Cachexia_Prevalence <- fread("Cachexia_Prevalence.txt")

Cachexia_Prevalence <- fread("Cachexia_Prevalence_v2.csv")

Cachexia_Prevalence <- Cachexia_Prevalence[ , c(1, 4, 7)]

Cachexia_Prevalence <- gather(Cachexia_Prevalence, group, prevalence, `% non-Mets Cachexia`:`% Mets Cachexia`)

Cachexia_Prevalence <- Cachexia_Prevalence %>% arrange(`Primary Cancer`, group, prevalence)


Cachexia_Prevalence$prevalence <- as.numeric(str_replace_all(Cachexia_Prevalence$prevalence, "%", ""))
       

Cachexia_Prevalence %>%
  arrange(-prevalence) %>% select(`Primary Cancer`) %>% distinct() %>%
  left_join(Cachexia_Prevalence) %>% spread(key=group, value=prevalence) %>%
  arrange(-`% Mets Cachexia`)
  
Cachexia_Prevalence %>%
  arrange(-prevalence) %>% select(`Primary Cancer`) %>% distinct() %>%
  left_join(Cachexia_Prevalence) %>%
  mutate(`Primary Cancer`=factor(`Primary Cancer`, levels=c("Pancreatic", "Gastroesophageal", "Respiratory", "Lung", "Liver", "Head", "Myeloma", "Bone", "Intestinal", "Leukemia", "Urinary", 
                                                               "Brain", "Lymphoma", "Reproductive", "Kidney", "Other", "Breast", "Prostate", "Salivary", "Skin", "Thyroid"))) %>%
  ggplot(aes(fill=group, y=prevalence/100, x=`Primary Cancer`)) + 
  geom_bar(position='dodge', stat='identity', alpha=0.8) +
    geom_text(aes(label=paste0(prevalence, "%")), vjust=-0, size=3) +
   theme_minimal() + 
  ylim(0,1) + scale_y_continuous(labels = scales::percent) +
  labs(x='Primary Cancer', y='Estimated \n Cachexia Prevalence \n') +
  scale_fill_manual('Metastasis status', values=c('firebrick', 'midnightblue')) +
    scale_colour_manual('Metastasis status', values=c('firebrick', 'midnightblue')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



Summary_Dems <- fread("Summary_Dems.txt")

Summary_Dems <- Summary_Dems[, c(1, 2, 8)]

Summary_Dems$`# non-Metastatic` <- Summary_Dems$n - Summary_Dems$`# Metastatic`

Summary_Dems <- Summary_Dems[ , c(1, 3, 4)]

Summary_Dems <- gather(Summary_Dems, group, size, `# Metastatic`:`# non-Metastatic`)

Cachexia_Prevalence <- Cachexia_Prevalence %>% mutate(group=ifelse(group=="% Mets Cachexia", "Metastatic", "non-Metastatic"))

Summary_Dems <- Summary_Dems %>% mutate(group=ifelse(group=="# Metastatic", "Metastatic", "non-Metastatic"))

Summary_Dems <- Summary_Dems %>% inner_join(Cachexia_Prevalence)

Summary_Dems %>% 
  ggplot(aes(size, prevalence/100, colour=group, fill=group, size=size)) +
  geom_point(alpha=0.8) +
  scale_y_continuous(labels = scales::percent) +
  geom_text_repel(aes(label = `Primary Cancer`), 
                  colour = "black", 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 25))+
  theme_minimal()+ 
  scale_fill_manual('Metastasis status', values=c('firebrick', 'midnightblue')) +
  scale_colour_manual('Metastasis status', values=c('firebrick', 'midnightblue')) +
  xlab("\n Sample size")+
  ylab("Estimated \n Cachexia Prevalence \n")


Cachexia_Prevalence <- fread("Cachexia_Prevalence_v2.csv")

cachexia <- Cachexia_Prevalence$`# non-Mets Cachexia`
names(cachexia) <- Cachexia_Prevalence$`Primary Cancer`
patients <- Cachexia_Prevalence$`# non-Mets ≥10 BMIs`
Prop_test(n_succ=cachexia, n_tot=patients)

pairwise_prop_test(n_succ=cachexia, n_tot=patients)

# <<< 21-sample test for equality of proportions without continuity correction 
# 
# 
# --- Description
# 
#                Bone   Brain   Breast   Gastroesophageal    Head   Intestinal   Kidney   Leukemia   Liver    Lung   Lymphoma   Myeloma   Other   Pancreatic   Prostate   Reproductive   Respiratory   Salivary    Skin   Thyroid   Urinary
#
# n_              434    1885    22666               1283    1382         6971     3521       5286    1637    5369       5478      1949    2028          990      16848           6877           625        284    4030      4733      4584
# n_total         700    2823    38034               1683    2161        10650     5773       7624    2171    6952       8592      2658    3468         1335      29989          11476           952        501    7676      8928      7107
# proportion    0.620   0.668    0.596              0.762   0.640        0.655    0.610      0.693   0.754   0.772      0.638     0.733   0.585        0.742      0.562          0.599         0.657      0.567   0.525     0.530     0.645
# 
# --- Inference
# 
# Chi-square statistic: 2638.766 
# Degrees of freedom: 20 
# Hypothesis test of equal population proportions: p-value = 0.000



cachexia <- Cachexia_Prevalence$`# Mets Cachexia`
names(cachexia) <- Cachexia_Prevalence$`Primary Cancer`
patients <- Cachexia_Prevalence$`# Mets ≥10 BMIs`
prop.test(cachexia, patients)

 
# 	21-sample test for equality of proportions without continuity correction
# 
# data:  cachexia out of patients
# X-squared = 3444.1, df = 20, p-value < 0.00000000000000022
# alternative hypothesis: two.sided
# sample estimates:
#    prop 1    prop 2    prop 3    prop 4    prop 5    prop 6    prop 7    prop 8    prop 9   prop 10   prop 11   prop 12   prop 13   prop 14   prop 15   prop 16   prop 17   prop 18   prop 19   prop 20   prop 21 
# 0.7749115 0.7218130 0.6881336 0.9298246 0.8061021 0.7847288 0.7368146 0.7515264 0.8388675 0.8583487 0.7368273 0.7985939 0.6973893 0.9361124 0.6706560 0.7522361 0.8531627 0.6819085 0.6530973 0.5982709 0.7496337 


# -----------------------------------------------------------------
# Time from cancer onset to cachexia Dx or observed ------------------
# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

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
Cachexia_Dx <- Cachexia_Dx %>% left_join(Pats_to_track_BMI %>% select(patid, cachexia_onset)) %>% select(patid, cachexia_onset)


Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")
temp <- temp %>% filter(Drop95==1 |Drop90==1|Drop2_20==1) %>% group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>% slice(1) %>% ungroup()
temp <- temp %>% select(patid, Month_Min)
Cachexia_Pred <- Cachexia_Pred %>% left_join(temp) %>% select(patid, Month_Min) 
Cachexia_Pred$group <- "Pred"




PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )


Cachexia_Dx <- Cachexia_Dx %>% mutate(cachexia_onset=as.character(cachexia_onset)) %>% mutate(cachexia_onset=str_sub(cachexia_onset, 1L, 7L))

Cachexia_Dx <- Cachexia_Dx %>% left_join(
  Months_lookup, by=c("cachexia_onset"="Month")
  ) %>% select(patid, Exact_Month) %>% distinct() %>% rename("Month_Min"="Exact_Month")

Cachexia_Dx$group <- "Dx"

Cachexia <- Cachexia_Dx %>% bind_rows(Cachexia_Pred)

Cachexia <- Cachexia %>% left_join(PONS_Demographics)  %>% left_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer))

Cachexia <- Cachexia %>% mutate(cancer_onset=as.character(cancer_onset)) %>% mutate(cancer_onset=str_sub(cancer_onset, 1L, 7L))

Cachexia <- Cachexia %>% left_join(
  Months_lookup, by=c("cancer_onset"="Month")
  ) %>% select(-cancer_onset) %>% distinct() %>% rename("cancer_onset"="Exact_Month")

Cachexia$Elapsed <- Cachexia$Month_Min - Cachexia$cancer_onset

data.frame(Cachexia %>% group_by(group, Primary_Cancer) %>% summarise(mean=mean(Elapsed)))

Cachexia %>% group_by(group) %>% count() %>% rename("n2"="n") %>%
  left_join(
Cachexia %>% filter(Elapsed<=0) %>% group_by(group) %>% count()
) %>% mutate(perc=n/n2)


data.frame(Cachexia %>% group_by(Primary_Cancer) %>% count() %>% rename("n2"="n") %>%
  left_join(
Cachexia %>% filter(Elapsed<=0) %>% group_by(Primary_Cancer) %>% count()
) %>% mutate(perc=n/n2))


data.frame(Cachexia %>% group_by(Primary_Cancer) %>% count() %>% rename("n2"="n") %>%
  left_join(
Cachexia %>% filter(Elapsed<=0) %>% group_by(Primary_Cancer) %>% count()
) %>% mutate(perc=n/n2))


Cachexia %>% 
  ggplot(aes(fill=group, colour=group, x=Elapsed)) + 
  geom_density(alpha=0.5)  + 
   theme_minimal() + 
  facet_wrap(~Primary_Cancer, scales="free_y") +
  labs(y='Patient density \n ', 
       x='\n No. Months \n From Cancer Onset to Cachexia Development (Pred) and Diagnosis (Dx) \n') +
  scale_fill_manual('Cachexia Criterion', values=c('firebrick', 'midnightblue')) +
  scale_colour_manual('Cachexia Criterion', values=c('firebrick', 'midnightblue')) 





data.frame(Cachexia %>% group_by(group, Primary_Cancer) %>% summarise(mean=mean(Elapsed)) %>%
  spread(key = group, value=mean) %>% arrange(-Dx))

Cachexia %>% group_by(group, Primary_Cancer) %>% summarise(mean=mean(Elapsed)) %>%
  ggplot(aes(fill=group, colour=group, y=Primary_Cancer, x=mean)) + 
  geom_bar(position='dodge', stat='identity', alpha=0.8, width = 0.6) +
   geom_text(aes(label=round(mean,2)), vjust=-0, hjust=-0.2) +
   theme_minimal() + 
  labs(y='Primary Cancer', x='\n Average No. Months \n From Cancer Onset to Cachexia Development (Pred) and Diagnosis (Dx) \n') +
  scale_fill_manual('Cachexia Criterion', values=c('firebrick', 'midnightblue')) +
  scale_colour_manual('Cachexia Criterion', values=c('firebrick', 'midnightblue')) 



for(i in unique(Cachexia$Primary_Cancer)){
  
  res <- wilcox.test(Cachexia$Elapsed[Cachexia$group=="Dx" & Cachexia$Primary_Cancer==i], 
            Cachexia$Elapsed[Cachexia$group=="Pred" & Cachexia$Primary_Cancer==i])
  
  print(i)

  print(res$p.value)
}

# [1] "Breast Cancer"
# [1] 0.00000000000000000000000000000000000000000000000000000000000000000000000878343
# [1] "Kidney Cancer"
# [1] 0.0000000000007638822
# [1] "Leukemia Cancer"
# [1] 0.00000000000000000000000005279014
# [1] "Lung Cancer"
# [1] 0.000000000000000000000000000000000000000000000000000004684298
# [1] "Lymphoma Cancer"
# [1] 0.0000000000000002299202
# [1] "Intestinal Cancer"
# [1] 0.00000000000000000000000000000000000000000000001162311
# [1] "Other Cancer"
# [1] 0.0000000001473312
# [1] "Pancreatic Cancer"
# [1] 0.00000000000000000000000000000000000000000000000007394577
# [1] "Reproductive Cancer"
# [1] 0.00000000000000000000000000000005093118
# [1] "Prostate Cancer"
# [1] 0.00000000000000000000000000000000000000000000000000000001954107
# [1] "Urinary Cancer"
# [1] 0.0000000000000000000004920591
# [1] "Liver Cancer"
# [1] 0.0000000000000000000000000009657342
# [1] "Head Cancer"
# [1] 0.000000000000000000000000006024441
# [1] "Respiratory Cancer"
# [1] 0.0000001004207
# [1] "Bone Cancer"
# [1] 0.0000000114737
# [1] "Gastroesophageal Cancer"
# [1] 0.000000000000000000000004406646
# [1] "Myeloma Cancer"
# [1] 0.000000000001230275
# [1] "Skin Cancer"
# [1] 0.0000000003517654
# [1] "Brain Cancer"
# [1] 0.000000000006630812
# [1] "Salivary Cancer"
# [1] 0.04343678
# [1] "Thyroid Cancer"
# [1] 0.003591105




# -----------------------------------------------------------
# Random forest cachexia Dx or cachexia pred vs no cachexia ------------------------------

# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")

Min_Max <- PONS_Measures %>% select(patid, min, max) %>% distinct()
Min_Max$Diff <- Min_Max$max - Min_Max$min


Cachexia <- Cachexia_Dx %>% select(patid, cancer_metastasis, group) %>% 
  bind_rows(Cachexia_Pred %>% select(patid, cancer_metastasis, group)) %>%
    bind_rows(No_Cachexia %>% select(patid, cancer_metastasis, group))

Cachexia <- Cachexia %>% left_join(Min_Max %>% select(patid, Diff))

Cachexia <- Cachexia %>% left_join(PONS_Demographics %>% select(patid, age))






PONS_Measures <- fread("PONS Measures.txt", sep="\t")

PONS_Measures <- PONS_Measures %>% filter(test=="Albumin"|test=="Hemoglobin"|test=="C-Reactive Protein")

PONS_Measures <- PONS_Measures %>% select(patid, test, value) %>% distinct()

PONS_Measures <- Cachexia %>% select(patid) %>% left_join(PONS_Measures)

Albumin <- PONS_Measures %>% filter(test=="Albumin") %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% select(-test) %>% rename("Albumin"="value")
Hemoglobin <- PONS_Measures %>% filter(test=="Hemoglobin") %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% select(-test) %>% rename("Hemoglobin"="value")
CRP <- PONS_Measures %>% filter(test=="C-Reactive Protein") %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% select(-test) %>% rename("CRP"="value")


Cachexia_v2 <- Cachexia %>% left_join(Albumin) %>% left_join(Hemoglobin) %>% left_join(CRP) %>% drop_na()










# Dx Codes
PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")

PONS_Comorbidity_Inventories <- Cachexia %>% select(patid) %>% left_join(PONS_Comorbidity_Inventories)

names(PONS_Comorbidity_Inventories)[3] <- "ICD"

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(ICD!="R64")

PONS_Comorbidity_Inventories$ICD <- substr(PONS_Comorbidity_Inventories$ICD,1,3)

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories  %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("R", ICD))

length(unique(PONS_Comorbidity_Inventories$ICD))

sort(unique(PONS_Comorbidity_Inventories$ICD))

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(ICD!="ACE" & ICD!="YE8")

PONS_Comorbidity_Inventories <- Cachexia %>% select(patid) %>% left_join(PONS_Comorbidity_Inventories)

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories[,c(1,3)]

PONS_Comorbidity_Inventories$value <- 1

length(unique(PONS_Comorbidity_Inventories$patid)) 

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% spread(key=ICD, value=value)

PONS_Comorbidity_Inventories[is.na(PONS_Comorbidity_Inventories)] <- 0

Cachexia_v2 <- Cachexia_v2 %>% left_join(PONS_Comorbidity_Inventories)

sum(is.na(Cachexia_v2))

Cachexia_v2[is.na(Cachexia_v2)] <- 0

Cachexia_v2 %>% group_by(group) %>% count() 



#Cachexia_v3 <- Cachexia_v2 %>% filter(group!="Pred")
Cachexia_v3 <- Cachexia_v2 %>% filter(group!="Dx")

dim(Cachexia_v3)
Cachexia_v3 <- Cachexia_v3 %>% group_by(group) %>% sample_n(2000) %>% ungroup()

names(Cachexia_v3)

Cachexia_v3 <- Cachexia_v3 %>% select(-`<NA>`)

Cachexia_v3 <- Cachexia_v3 %>% select(-patid)

Cachexia_v3 <- Cachexia_v3 %>% mutate(group = ifelse(group=="Pred", 1, 0))


library("randomForest")

modelAll_1_randomForest <- randomForest(group ~ ., data = Cachexia_v3)

summary(modelAll_1_randomForest)

RF_IMP <- modelAll_1_randomForest$importance

RF_IMP <- data.frame(RF_IMP)

RF_IMP %>% arrange(-IncNodePurity)

# -------------------------------------------
# Individual comorbidity prevalence --------------------------------------

# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")


Cachexia <- Cachexia_Dx %>% select(patid, cancer_metastasis, group) %>% 
  bind_rows(Cachexia_Pred %>% select(patid, cancer_metastasis, group)) %>%
    bind_rows(No_Cachexia %>% select(patid, cancer_metastasis, group))


# Dx Codes
PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")

PONS_Comorbidity_Inventories <- Cachexia %>% select(patid) %>% left_join(PONS_Comorbidity_Inventories)

names(PONS_Comorbidity_Inventories)[3] <- "ICD"

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-weight)

All_pats <- Cachexia %>% select(patid, group) %>% distinct()

All_pats %>% group_by(group) %>% count()

# 1 Dx     14559
# 2 None  100284
# 3 Pred  171973

Cachexia <- Cachexia %>% left_join(PONS_Comorbidity_Inventories)

Cachexia %>% filter(ICD=="R62") %>% group_by(group) %>% count()

num <-  c( 4457, 35793, 1702)
names(num) <- c("Dx", "Pred", "None")
den <- c( 14559, 171973,100284 )
prop.test(num, den)
pairwise.prop.test(num, den)



# --------------------------------------------


# Malnutrition, Weight Loss, Failure to Thrive Dx among other populations --------------------
# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")


Cachexia <- Cachexia_Dx %>% select(patid, cancer_metastasis, group) %>% 
  bind_rows(Cachexia_Pred %>% select(patid, cancer_metastasis, group)) %>%
    bind_rows(No_Cachexia %>% select(patid, cancer_metastasis, group))


# Dx Codes
PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")

PONS_Comorbidity_Inventories <- Cachexia %>% select(patid) %>% left_join(PONS_Comorbidity_Inventories)

names(PONS_Comorbidity_Inventories)[3] <- "ICD"

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-weight)

All_pats <- Cachexia %>% select(patid, group) %>% distinct()

All_pats %>% group_by(group) %>% count()

# 1 Dx     14559
# 2 None  158533
# 3 Pred  113724

Cachexia <- Cachexia %>% left_join(PONS_Comorbidity_Inventories)

# weight loss (R63.0 , R63.4, R63.6, R64), malnutrition (E40-E46 / D50-D64), failure to thrive (R62.7) 

WeightLoss <- Cachexia %>% filter(grepl("R63", ICD)|grepl("R64", ICD)) %>% select(patid, group) %>% distinct() %>% mutate(Comorb="WeightLoss")

malnutrition <- Cachexia %>% filter(grepl("E40", ICD)|grepl("E41", ICD)|grepl("E42", ICD)|grepl("E43", ICD)|grepl("E44", ICD)|grepl("E45", ICD)|grepl("E46", ICD)|
                                      grepl("D50", ICD)|grepl("D51", ICD)|grepl("D52", ICD)|grepl("D53", ICD)|
                                      grepl("D55", ICD)|grepl("D56", ICD)|grepl("D57", ICD)|grepl("D58", ICD)|grepl("D59", ICD)|
                                      grepl("D60", ICD)|grepl("D61", ICD)|grepl("D62", ICD)|grepl("D63", ICD)|grepl("D64", ICD)) %>% select(patid, group) %>% distinct() %>% mutate(Comorb="malnutrition")

failureToThrive <- Cachexia %>% filter(grepl("R62", ICD)) %>% select(patid, group) %>% distinct() %>% mutate(Comorb="failureToThrive")

comorbs_extra <- WeightLoss %>% bind_rows(malnutrition) %>% bind_rows(failureToThrive)



All_pats %>% left_join(comorbs_extra) %>% group_by(group, Comorb) %>% count()

All_pats %>% left_join(comorbs_extra) %>% left_join(Summary_vals_pats) %>% 
  group_by(group, Comorb) %>% summarise(n=mean(max, na.rm=T))



PONS_Demographics <- fread("PONS Demographics.txt")
 
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, died, cancer_metastasis, cancer_onset, death_date)  %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics$cancer_onset <- as.Date(PONS_Demographics$cancer_onset)

PONS_Demographics$death_date  <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- PONS_Demographics %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))


All_pats %>% left_join(comorbs_extra)  %>%
  left_join(PONS_Demographics %>% select(patid, Survived)) %>%
  group_by(group, Comorb) %>% summarise(n=mean(Survived, na.rm=T))


temp <- All_pats %>% left_join(comorbs_extra)  %>%
  inner_join(PONS_Demographics  %>% select(patid, died, Survived)) %>%
   mutate(status=ifelse(died=="Y",1,0)) %>% select(-died)  

temp[is.na(temp)] <- "none"

temp <- temp %>% mutate(Comorb=ifelse(Comorb=="WeightLoss", "Any Weight Loss Dx",
                              ifelse(Comorb=="malnutrition", "Any Malnutrition Dx",
                                     ifelse(Comorb=="failureToThrive", "Any Failure To Thrive Dx", "None of the above"))))


temp$group <- paste(temp$group, paste("-", temp$Comorb))

unique(temp$group)

temp$group <- factor(temp$group, levels = c("None - None of the above","None - Any Malnutrition Dx","None - Any Weight Loss Dx", "None - Any Failure To Thrive Dx", 
                                            "Pred - None of the above", "Pred - Any Malnutrition Dx", "Pred - Any Weight Loss Dx", "Pred - Any Failure To Thrive Dx",
                                            "Dx - Any Malnutrition Dx", "Dx - Any Weight Loss Dx", "Dx - Any Failure To Thrive Dx"))


sfit <- survfit(Surv(Survived, status)~group, data=temp)
sfit


summary(sfit)

ggsurvplot(sfit)

temp %>% group_by(group) %>% count() %>% rename("n1"="n") %>%
  left_join(temp %>% filter(status==1) %>% group_by(group) %>% count()
) %>% mutate(perc=n/n1)

#    group                              n1     n   perc
#  1 None - None of the above        78326  2804 0.0358
#  2 None - Any Malnutrition Dx      69705 11288 0.162 
#  3 None - Any Weight Loss Dx       26210  4696 0.179 
#  4 None - Any Failure To Thrive Dx  2693  1510 0.561 
#  5 Pred - None of the above        31555  2541 0.0805
#  6 Pred - Any Malnutrition Dx      73359 24344 0.332 
#  7 Pred - Any Weight Loss Dx       37685 13333 0.354 
#  8 Pred - Any Failure To Thrive Dx  6773  4371 0.645 
#  9 Dx - Any Malnutrition Dx        13642  9036 0.662 
# 10 Dx - Any Weight Loss Dx         14559  9550 0.656 
# 11 Dx - Any Failure To Thrive Dx    4457  3176 0.713



ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,  
           palette=c("gray", "#e6cfaa","#fec96f","#c1a91f","#00a1f2","#006f94","#055679","#004966","#da0000","#810013","#420000"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia % Diagnosis Status ", 
           risk.table.height=0.2)


fit <- coxph(Surv(Survived, status)~group, data=temp)

fit


# ----------------------
# Malnutrition, Weight Loss, Failure to Thrive Dx among other populations V2 --------------------
# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")


Cachexia <- Cachexia_Dx %>% select(patid, cancer_metastasis, group) %>% 
  bind_rows(Cachexia_Pred %>% select(patid, cancer_metastasis, group)) %>%
    bind_rows(No_Cachexia %>% select(patid, cancer_metastasis, group))


# Dx Codes
PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")

PONS_Comorbidity_Inventories <- Cachexia %>% select(patid) %>% left_join(PONS_Comorbidity_Inventories)

names(PONS_Comorbidity_Inventories)[3] <- "ICD"

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-weight)

All_pats <- Cachexia %>% select(patid, group) %>% distinct()

All_pats %>% group_by(group) %>% count()

# 1 Dx     14559
# 2 None  158533
# 3 Pred  113724

Cachexia <- Cachexia %>% left_join(PONS_Comorbidity_Inventories)

# weight loss (R63.0 , R63.4, R63.6, R64), malnutrition (E40-E46 / D50-D64), failure to thrive (R62.7) 

WeightLoss <- Cachexia %>% filter(grepl("R63", ICD)|grepl("R64", ICD)) %>% select(patid, group) %>% distinct() %>% mutate(Comorb="WeightLoss")

malnutrition <- Cachexia %>% filter(grepl("E40", ICD)|grepl("E41", ICD)|grepl("E42", ICD)|grepl("E43", ICD)|grepl("E44", ICD)|grepl("E45", ICD)|grepl("E46", ICD)|
                                      grepl("D50", ICD)|grepl("D51", ICD)|grepl("D52", ICD)|grepl("D53", ICD)|
                                      grepl("D55", ICD)|grepl("D56", ICD)|grepl("D57", ICD)|grepl("D58", ICD)|grepl("D59", ICD)|
                                      grepl("D60", ICD)|grepl("D61", ICD)|grepl("D62", ICD)|grepl("D63", ICD)|grepl("D64", ICD)) %>% select(patid, group) %>% distinct() %>% mutate(Comorb="malnutrition")

failureToThrive <- Cachexia %>% filter(grepl("R62", ICD)) %>% select(patid, group) %>% distinct() %>% mutate(Comorb="failureToThrive")

comorbs_extra <- WeightLoss %>% bind_rows(malnutrition) %>% bind_rows(failureToThrive)



All_pats %>% left_join(comorbs_extra) %>% group_by(group, Comorb) %>% count()
comorbs_extra %>% group_by(group, Comorb) %>% count()


All_pats %>% left_join(comorbs_extra) %>% filter(Comorb=="WeightLoss") %>% select(patid) %>% distinct() %>% count() # 78454

All_pats %>% left_join(comorbs_extra) %>% filter(Comorb=="malnutrition") %>% select(patid) %>% distinct() %>% count() # 156706

All_pats %>% left_join(comorbs_extra) %>% filter(Comorb=="failureToThrive") %>% select(patid) %>% distinct() %>% count() # 13923


All_pats %>% left_join(comorbs_extra) %>% group_by(Comorb, group) %>% count() %>% mutate(n2=ifelse(Comorb=="WeightLoss", n/78454,
                                                                           ifelse(Comorb=="malnutrition", n/156706, n/13923)))




PONS_Demographics <- fread("PONS Demographics.txt")
 
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, died, cancer_metastasis, cancer_onset, death_date)  %>% 
  mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics$cancer_onset <- as.Date(PONS_Demographics$cancer_onset)

PONS_Demographics$death_date  <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- PONS_Demographics %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))


temp <- All_pats %>% left_join(comorbs_extra)  %>%
  inner_join(PONS_Demographics  %>% select(patid, died, Survived)) %>%
   mutate(status=ifelse(died=="Y",1,0)) %>% select(-died)  

temp[is.na(temp)] <- "none"



temp2 <- All_pats %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>%
  bind_rows(WeightLoss %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="weightLoss")  %>% select(1,4,2,3)) %>%
  bind_rows(WeightLoss %>% filter(group!="Dx") %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="weightLoss_noDx")  %>% select(1,4,2,3)) %>%
  bind_rows(malnutrition %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="malnutrition")  %>% select(1,4,2,3)) %>%
  bind_rows(malnutrition %>% filter(group!="Dx") %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="malnutrition_nODx")  %>% select(1,4,2,3)) %>%
  bind_rows(failureToThrive %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="failureToThrive")  %>% select(1,4,2,3)) %>%
  bind_rows(failureToThrive %>% filter(group!="Dx") %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="failureToThrive_noDx") %>% select(1,4,2,3))

# WeightLoss %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="weightLoss")  %>% select(1,4,2,3)
# WeightLoss %>% filter(group!="Dx") %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="weightLoss_noDx")  %>% select(1,4,2,3)
# 
# malnutrition %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="malnutrition")  %>% select(1,4,2,3)
# malnutrition %>% filter(group!="Dx") %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="malnutrition_nODx")  %>% select(1,4,2,3)
# 
# failureToThrive %>% select(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="failureToThrive")  %>% select(1,4,2,3)
# failureToThrive %>% filter(group!="Dx") %>% elect(patid) %>% inner_join(temp %>% select(patid, Survived, status) %>% distinct()) %>% mutate(group="failureToThrive_noDx") %>% select(1,4,2,3)


unique(temp2$group)

temp2$group <- factor(temp2$group, levels = c("None","Pred","Dx", "malnutrition",  "malnutrition_nODx",
                                            "weightLoss", "weightLoss_noDx", "failureToThrive", "failureToThrive_noDx"))


temp2 %>% group_by(group) %>% count() %>% rename("n1"="n") %>%
  left_join(temp2 %>% filter(status==1) %>% group_by(group) %>% count()
) %>% mutate(perc=n/n1)

#   group                    n1     n   perc
# 1 None                 158533 14995 0.0946
# 2 Pred                 113724 28410 0.250 
# 3 Dx                    14559  9550 0.656 
# 4 malnutrition         156706 44668 0.285 
# 5 malnutrition_nODx    143064 35632 0.249 
# 6 weightLoss            78454 27579 0.352 
# 7 weightLoss_noDx       63895 18029 0.282 
# 8 failureToThrive       13923  9057 0.651 
# 9 failureToThrive_noDx   9466  5881 0.621 

sfit <- survfit(Surv(Survived, status)~group, data=temp2)
sfit


summary(sfit)

ggsurvplot(sfit)



ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,  
           palette=c("honeydew4", "midnightblue", "firebrick",  "#e6cfaa","#fec96f", "#00a1f2","#004966", "#E6ABFF", "#683B7A"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia % Diagnosis Status ", 
           risk.table.height=0.2)


fit <- coxph(Surv(Survived, status)~group, data=temp2)

fit

temp2 %>% inner_join(Summary_vals_pats %>% mutate(drop=100*(min-max)/max) %>% select(patid, drop)) %>%
  group_by(group) %>% summarise(n=mean(drop))



temp2 <- temp2 %>% filter(!grepl("noDx", group))

temp2 <- temp2 %>% mutate(group=as.character(group))


temp2$group <- factor(temp2$group, levels = c("None","Pred","Dx", "malnutrition","weightLoss",  "failureToThrive"))


sfit <- survfit(Surv(Survived, status)~group, data=temp2)
sfit


summary(sfit)

ggsurvplot(sfit)



ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,  
           palette=c("honeydew4", "midnightblue", "firebrick", "#fec96f", "#00a1f2", "#E6ABFF"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia % Diagnosis Status ", 
           risk.table.height=0.2)



# -----------------------
# Time-dependent BMI evolution ----------

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1 | Drop95==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")

PONS_Measures <- PONS_Measures %>% select(patid, claimed, value) %>% distinct()

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed)) %>% mutate(claimed=str_sub(claimed, 1L, 7L))

PONS_Measures <- PONS_Measures %>% left_join(
  Months_lookup, by=c("claimed"="Month")
  ) %>% select(patid, value , Exact_Month) %>% distinct() 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_onset=as.character(cancer_onset)) %>% mutate(cancer_onset=str_sub(cancer_onset, 1L, 7L))
PONS_Demographics <- PONS_Demographics %>% left_join(
  Months_lookup, by=c("cancer_onset"="Month")
  ) %>% select(-cancer_onset) %>% distinct() %>% rename("cancer_onset"="Exact_Month")



PONS_Measures <- PONS_Demographics %>% inner_join(PONS_Measures) %>% mutate(Elapsed=Exact_Month-cancer_onset)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=as.character(death_date)) %>% mutate(death_date=str_sub(death_date, 1L, 7L))
PONS_Demographics <- PONS_Demographics %>% left_join(
  Months_lookup, by=c("death_date"="Month")
  ) %>% select(patid,  Exact_Month) %>% distinct() 
PONS_Demographics <- PONS_Demographics %>% drop_na() %>% mutate(status=2)

PONS_Measures <- PONS_Measures %>% left_join(PONS_Demographics)  %>%  
  mutate(status=ifelse(is.na(status),0,status))

Groups <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia)

Groups <- Groups %>% inner_join(PONS_Measures)

Groups <- Groups %>% filter(Elapsed>=0) %>% 
  select(-c(weight, diagnosis, cancer_metastasis, group, Exact_Month )) %>% 
  group_by(patid, Elapsed , status ) %>% summarise(value=mean(value)) %>% ungroup()


df_start <- Groups %>% group_by(patid) %>% filter(Elapsed==max(Elapsed)) %>% select(-value) %>%
  left_join(Groups %>% group_by(patid) %>% filter(Elapsed==min(Elapsed)) %>% select(-c(Elapsed, status)))

df_long <- Groups 


df_long <- df_long %>% anti_join(df_start %>% filter(Elapsed==0) %>% select(patid) %>% distinct())
df_start <- df_start %>% anti_join(df_start %>% filter(Elapsed==0) %>% select(patid) %>% distinct())



df_start_2 <- tmerge(data1 = df_start, data2 = df_start, id = patid, death = event(Elapsed, status)) #set range

df_start_2 <- tmerge(df_start_2, df_long, id = patid, value = tdc(Elapsed, value))


df_start <- df_start %>%  mutate(status=ifelse(status==2,1,status))
df_start_2 <- df_start_2 %>% mutate(death=ifelse(death==2,1,death)) %>% mutate(status=ifelse(status==2,1,status))

df_start <- df_start %>%  mutate(value=exp(value))
df_start_2 <- df_start_2 %>% mutate(value=exp(value))


fit1 <- coxph(Surv(Elapsed, status == 1) ~ log(value) , df_start, cluster = patid)

fit2 <- coxph(Surv(tstart, tstop, death == 1) ~ log(value) , df_start_2, cluster = patid)


fit2 

library(survminer)

ggcoxfunctional(fit2,  data = df_start_2, point.col = "blue", point.alpha = 0.5)


rbind("baseline fit" = coef(fit1), 
      "time dependent" = coef(fit2))



data.frame(predict(fit2, df_start_2)) %>%
  bind_cols(df_start %>% select(value)) %>%
    sample_n(10000) %>%
  ggplot(aes(value, predict.fit2..df_start_2. )) +
  geom_smooth()



ggplot(Predict(fit2))

plot(survfit(fit2)$cumhaz)

require(rms)
dd <- datadist(df_start_2); options(datadist='dd')
f <- cph(Surv(Elapsed, status ) ~ value, data=df_start_2)

ggplot(Predict(f))

library(contsurvplot)


cox <- coxph(Surv(Elapsed, status) ~ value, data = df_start_2, x=TRUE )

plot_surv_at_t(time="Elapsed",
               status="death",
               variable="value",
               data=df_start_2,
               model=cox,
               t=59) 
        
cox_fit2 <- survfit(fit2)

autoplot(cox_fit2)

aa_fit <- aareg(Surv(Elapsed, status) ~ log(value) , data = df_start_2)
autoplot(aa_fit)

# ------------------

# Probability of having died as a function of how long elapsed from onset to cachexia --------------------

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

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
Cachexia_Dx <- Cachexia_Dx %>% left_join(Pats_to_track_BMI %>% select(patid, cachexia_onset)) %>% select(patid, cachexia_onset)


Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")
temp <- temp %>% filter(Drop95==1 |Drop90==1|Drop2_20==1) %>% group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>% slice(1) %>% ungroup()
temp <- temp %>% select(patid, Month_Min)
Cachexia_Pred <- Cachexia_Pred %>% left_join(temp) %>% select(patid, Month_Min) 
Cachexia_Pred$group <- "Pred"




PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )


Cachexia_Dx <- Cachexia_Dx %>% mutate(cachexia_onset=as.character(cachexia_onset)) %>% mutate(cachexia_onset=str_sub(cachexia_onset, 1L, 7L))

Cachexia_Dx <- Cachexia_Dx %>% left_join(
  Months_lookup, by=c("cachexia_onset"="Month")
  ) %>% select(patid, Exact_Month) %>% distinct() %>% rename("Month_Min"="Exact_Month")

Cachexia_Dx$group <- "Dx"

Cachexia <- Cachexia_Dx %>% bind_rows(Cachexia_Pred)

Cachexia <- Cachexia %>% left_join(PONS_Demographics)  %>% left_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer))

Cachexia <- Cachexia %>% mutate(cancer_onset=as.character(cancer_onset)) %>% mutate(cancer_onset=str_sub(cancer_onset, 1L, 7L))

Cachexia <- Cachexia %>% left_join(
  Months_lookup, by=c("cancer_onset"="Month")
  ) %>% select(-cancer_onset) %>% distinct() %>% rename("cancer_onset"="Exact_Month")

Cachexia$Elapsed <- Cachexia$Month_Min - Cachexia$cancer_onset
Cachexia <- Cachexia %>% select(patid, Elapsed, Primary_Cancer)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, died) %>% 
  mutate(died=ifelse(died=="Y",1,0))
         
Cachexia <- Cachexia %>% inner_join(PONS_Demographics) 

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>%  select(patid, death_date) %>% 
  mutate(death_date=as.character(death_date)) %>% mutate(death_date=str_sub(death_date, 1L, 7L))

PONS_Demographics <- PONS_Demographics %>% left_join(
  Months_lookup, by=c("death_date"="Month")
  ) %>% select(-death_date) %>% distinct() %>% rename("death_date"="Exact_Month")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date=ifelse(is.na(death_date), 60, death_date)) 


Cachexia <- Cachexia %>% left_join(
  PONS_Measures %>% select(patid, min, max) %>% distinct() %>% mutate(drop=100*(min-max)/max) %>% distinct()
  ) %>% left_join(
    PONS_Measures %>% select(patid, claimed) %>% distinct() %>% group_by(patid) %>% count()
    ) %>% left_join(
      PONS_Demographics %>% rename("visib"="death_date")
    )


Cachexia$Elapsed <- 1+ Cachexia$Elapsed
head(Cachexia)
summary(glm(died ~ Elapsed + min + drop + n, data=Cachexia))


Cachexia %>%
  filter(Elapsed<=24 & Elapsed >= (-24)) %>%
  ggplot(aes(x=Elapsed, y=died)) + 
  ylim(0,1) +
  stat_smooth(method="gam", se=TRUE, method.args = list(family=binomial)) +
  theme_minimal() +
  xlab("\n Number of Months from \n Cancer Onset to Cachexia observation ") + 
  ylab("Probability of having died \n")


# -----------------------
# BMI drops / min as a function of drugs used ------------------------
# Target cancers

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


data.frame(At_risk_population %>% group_by(diagnosis) %>% count())

temp <- fread("All_drops.txt")


New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1 | Drop95==1) %>% select(patid) %>% distinct()


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

Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")

No_Cachexia <- At_risk_population %>% distinct() %>% anti_join(Cachexia_Dx) %>% anti_join(Cachexia_Pred)  %>% mutate(group="None")

Min_Max <- PONS_Measures %>% select(patid, min, max) %>% distinct()

last <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==min(value)) %>% slice(1) %>% select(patid, value)

temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) 

temp <- temp %>% mutate(drop=100*(min-max)/max)

length(unique(temp$patid)) == dim(temp)[1]

temp <- temp %>% select(-weight, group, max)



CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(temp %>% select(patid), by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories  %>% filter(Treat!="-") %>% select(-c(disease, Month, weight)) %>% distinct()
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()

PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass  %>% filter(indication=="Cancer") %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, chemo_class) %>% distinct() %>% mutate(exp=1) %>% spread(key=chemo_class, value=exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")

names(temp)[1] <- "patient"
temp$diagnosis <- str_replace_all(temp$diagnosis, " Cancer", "")

#temp <- temp  %>% mutate(exp=1) %>% spread(key=diagnosis, value=exp) %>% select(-group)
temp <- temp  %>% select(-diagnosis)
temp[is.na(temp)] <- 0

temp <- temp %>% left_join(CAN_Drug_Histories) %>% select(-c(max, '<NA>'))

temp[is.na(temp)] <- 0

temp_2 <- temp %>% select(-patient)
temp_2 <- temp_2 %>% group_by(group) %>% sample_n(10000)
temp_2 <- temp_2 %>% rename("Immuno"="Immuno/Targeted")
temp_2 <- temp_2 %>% rename("PD1"="PD1/PDL1")
temp_2 <- temp_2 %>% select(-c(drop, Death))
temp_2 <- temp_2 %>%  select(-group) %>% ungroup()

library("randomForest")
library(xgboost)
library(caret)


modelAll_1_randomForest <- randomForest(min ~ ., data = temp_2)

summary(modelAll_1_randomForest)

RF_IMP <- data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)

temp_2 %>% group_by(PD1) %>% summarise(mean=mean(min))





shap.score.rank <- function(xgb_model = xgb_mod, shap_approx = TRUE, 
                            X_train = mydata$train_mm){
  require(xgboost)
  require(data.table)
  shap_contrib <- predict(xgb_model, X_train,
                          predcontrib = TRUE, approxcontrib = shap_approx)
  shap_contrib <- as.data.table(shap_contrib)
  shap_contrib[,BIAS:=NULL]
  cat('make SHAP score by decreasing order\n\n')
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = (mean_shap_score)))
}

# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}


# prep shap data
shap.prep <- function(shap  = shap_result, X_train = mydata$train_mm, top_n){
  require(ggforce)
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
  require(data.table)
  shap_score_sub <- as.data.table(shap$shap_score)
  shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  
  # feature values: the values in the original dataset
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue; 
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2) 
}

plot.shap.summary <- function(data_long){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 1, alpha = 0.5,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="gold1", high="blue4", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") 
  return(plot1)
}






var_importance <- function(shap_result, top_n=10)
{
  var_importance=tibble(var=names(shap_result$mean_shap_score), importance=shap_result$mean_shap_score)
  
  var_importance=var_importance[1:top_n,]
  
  ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
}


names(temp_2)

model_hd = xgboost(data = as.matrix(temp_2[,-2]),
                   nround = 500,
                   #objective = "binary:logistic",
                   label=as.matrix(temp_2[,2]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(temp_2[,-2]),
                              shap_approx = F)


var_importance(shap_result, top_n=15)



shap_long_hd = shap.prep(X_train = as.matrix(temp_2[,-2]) , top_n = 15)

plot.shap.summary(data_long = shap_long_hd)



library(jtools)
library(mgcv)



temp_2 <- temp %>% select(-patient)
temp_2 <- temp_2 %>% group_by(group) %>% sample_n(10000)
temp_2 <- temp_2 %>% rename("Immuno"="Immuno/Targeted")
temp_2 <- temp_2 %>% rename("PD1"="PD1/PDL1")
temp_2 <- temp_2 %>% select(-c(drop, Death))
temp_2 <- temp_2 %>%   ungroup() %>% select(-c( cancer_metastasis, group))
temp_2 <- temp_2 %>% select(-c(Hospital_Inpatient))
temp_2 <- temp_2 %>% filter(min!="0")

fit <- lm(min  ~ PD1 , data = temp_2)
summ(fit, confint = TRUE)
names(temp_2)

#  [1] "min"                     "Alkylating_Agent"        "Antimetabolites"        
#  [4] "Antimicrotubule_Agent"   "Biologic"                "Chemoprotective"        
#  [7] "Hormonal_Therapy"        "Immuno"                  "Other_Antineoplastics"  
# [10] "PD1"                     "Platinum_agent"          "Radio"                  
# [13] "Surgery_Inpatient"       "Topoisomerase_Inhibitor"

plot_summs(fit, plot.distributions = F, inner_ci_level = .9)

temp_2 %>% ggplot(aes(as.factor(Hormonal_Therapy), min, colour=as.factor(Hormonal_Therapy), fill=as.factor(Hormonal_Therapy))) +
  geom_violin() +
  ylim(10,50)

plot_drop_conf_int <- fread("plot_drop_conf_int.csv")

plot_drop_conf_int <- plot_drop_conf_int %>% filter(type=="min")

paste(plot_drop_conf_int %>% arrange(-est) %>% select(var) %>% distinct() )


plot_drop_conf_int %>% arrange(-est) %>% 
  mutate(var=factor(var, levels= c("Hormonal", "Alkylating", "Radio", "Immuno", "Surgery",
                                   "Antimicrotubule", "Biologic", "Topoisomerase", "Other_antineo",
                                   "Antimetabolites", "Platinum", "PD1"))) %>%
  ggplot(aes(x=var, y=est)) + 
    geom_point(alpha=1) + 
    geom_errorbar(width=.5, size=1.5, aes(ymin=low, ymax=high), colour="darkred", alpha=0.5) +
  coord_flip() +
   labs(x="Anticancer drug class used \n", y="\n Contribution towards \n Minimum BMI reached \n [estimated coeficiente, 95% C.I.]") + 
  theme_minimal()



plot_drop_conf_int <- fread("plot_drop_conf_int.csv")

plot_drop_conf_int <- plot_drop_conf_int %>% filter(type=="drop")

paste(plot_drop_conf_int %>% arrange(-est) %>% select(var) %>% distinct() )


plot_drop_conf_int %>% arrange(-est) %>% 
  mutate(var=factor(var, levels= c("Hormonal", "Alkylating", "Radio", "Immuno", "Surgery",
                                   "Antimicrotubule", "Biologic", "Topoisomerase", "Other_antineo",
                                   "Antimetabolites", "Platinum", "PD1"))) %>%
  ggplot(aes(x=var, y=est)) + 
    geom_point(alpha=1) + 
    geom_errorbar(width=.5, size=1.5, aes(ymin=low, ymax=high), colour="midnightblue", alpha=0.5) +
  coord_flip() +
   labs(x="Anticancer drug class used \n", y="\n Contribution towards \n % BMI drop experienced \n [estimated coeficiente, 95% C.I.]") + 
  theme_minimal()


# --------------

