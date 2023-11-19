
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
# library(lessR)

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

New_Cachexia_Pred <- temp %>% filter(Drop90==1 |  Drop95==1 | Drop2_20==1) %>% select(patid) %>% distinct()

# New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()


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

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")

Pats_BMI_30 <- PONS_Measures %>% filter(test=="BMI") %>% select(patid, value) %>% distinct() %>% 
  group_by(patid) %>%  filter(value==min(value))  %>% ungroup() %>% 
  filter(value>=30) %>% select(patid) %>% distinct()

fwrite(Pats_BMI_30, "Pats_BMI_30.txt")
Pats_BMI_30 <- fread("Pats_BMI_30.txt")

data.frame(
  CachexiaPats_ALL_NEW %>% anti_join(Pats_BMI_30) %>% group_by(diagnosis, cancer_metastasis ) %>% count()
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

last <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==min(value)) %>% slice(1) %>% select(patid, value)


Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=mean(min, na.rm=T), max=mean(max, na.rm=T))

#   group   min   max
# 1 Dx     19.5  25.3
# 2 None   28.4  31.0
# 3 Pred   25.2  32.4

Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=sd(min, na.rm=T), max=sd(max, na.rm=T))

#   group   min   max
# 1 Dx     4.68  5.76
# 2 None   6.00  6.66
# 3 Pred   6.14  8.25

Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=median(min, na.rm=T), max=median(max, na.rm=T))

#   group   min   max
# 1 Dx     18.7  24.4
# 2 None   27.3  29.8
# 3 Pred   24.4  31.3


Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) %>% 
  group_by(group) %>% summarise(min=IQR(min, na.rm=T), max=IQR(max, na.rm=T))

#  group   min   max
# 1 Dx     5.43  6.8 
# 2 None   7.11  7.93
# 3 Pred   7.9  10.0 

temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) 

kruskal.test(min ~ group, data = temp)

# 	Kruskal-Wallis rank sum test
# 
# data:  min by group
# Kruskal-Wallis chi-squared = 29866, df = 2, p-value < 0.00000000000000022

pairwise.wilcox.test(temp$min, temp$group, p.adjust.method = "bonferroni")

	# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

# data:  temp$min and temp$group 
# 
#      Dx                  None               
# None <0.0000000000000002 -                  
# Pred <0.0000000000000002 <0.0000000000000002
# 
# P value adjustment method: bonferroni 

	
temp %>%  select(group, min) %>%
  filter(min<75 & min>10) %>%
   mutate(group=factor(group, levels=c("None" ,"Pred", "Dx"))) %>%
  ggplot(aes(group, min, colour=group, fill=group )) +
  #geom_jitter(width=0.2, height = 0.6, alpha=0.3, show.legend = FALSE, size = 0.1 ) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE, show.legend = FALSE) +
  theme_minimal() +
  ylim(0,75) +
  xlab("\n  Cachexia Status") + ylab("Lowest BMI reached \n  \n") +
  scale_fill_manual(values=c("honeydew4", "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c("honeydew4", "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




temp %>% group_by(cancer_metastasis) %>% summarise(n=median(min, na.rm=T))

wilcox.test(temp$min[temp$cancer_metastasis==0], temp$min[temp$cancer_metastasis==1])



temp %>%  select(cancer_metastasis, min) %>%
  filter(min<75 & min>10) %>%
  mutate(cancer_metastasis=ifelse(cancer_metastasis==0, "None", "Metastatic")) %>%
   mutate(cancer_metastasis=factor(cancer_metastasis, levels=c("None" ,"Metastatic"))) %>%
  ggplot(aes(cancer_metastasis, min, colour=cancer_metastasis, fill=cancer_metastasis )) +
  geom_violin(alpha=0.7, show.legend = FALSE) +
  geom_boxplot(alpha=0.8, notch = TRUE, notchwidth = 0.5, varwidth = FALSE, show.legend = FALSE) +
  theme_minimal() +
  ylim(0,75) +
  xlab("\n  Metastasis Status") + ylab("Lowest BMI reached \n  \n") +
  scale_fill_manual(values=c( "midnightblue", "firebrick" )) +
  scale_colour_manual(values=c( "midnightblue", "firebrick"  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))







temp <- Cachexia_Dx %>% bind_rows(Cachexia_Pred) %>% bind_rows(No_Cachexia) %>% left_join(Min_Max) 
temp <- temp %>% mutate(drop=100*(min-max)/max)



temp %>% group_by(group) %>% summarise(drop=mean(drop, na.rm=T))

#   group   drop
# 1 Dx    -22.0 
# 2 None   -8.31
# 3 Pred  -21.4 

temp %>% group_by(group) %>% summarise(drop=sd(drop, na.rm=T))

#   group  drop
# 1 Dx    12.1 
# 2 None   5.52
# 3 Pred   9.40





# kruskal.test(drop ~ group, data = temp)

# 	Kruskal-Wallis rank sum test
# 
# data:  drop by group
# Kruskal-Wallis chi-squared = 142055, df = 2, p-value < 0.00000000000000022

pairwise.wilcox.test(temp$drop, temp$group, p.adjust.method = "bonferroni")

#	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

# data:  temp$min and temp$group 
# 
#      Dx                   None                
# None < 0.0000000000000002 -                   
# Pred 0.0000029            < 0.0000000000000002
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

#   group  last
# 1 Dx     21.1
# 2 None   29.7
# 3 Pred   27.9

temp %>% group_by(group) %>% summarise(last=sd(value, na.rm=T))

#   group  last
# 1 Dx     4.98
# 2 None   6.40
# 3 Pred   7.30

# ---------------------------------------

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
Cachexia_Pred %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1547.984
No_Cachexia %>% inner_join(PONS_Demographics) %>% summarise(n=mean(Survived)) # 1720.274


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

# Call: survfit(formula = Surv(Survived, status) ~ 1, data = temp)
# 
#  time n.risk n.event survival  std.err lower 95% CI upper 95% CI
#    12 272240   15784    0.945 0.000426        0.944        0.946


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
#               coef exp(coef) se(coef)     z                   p
# groupPred  1.06126   2.89001  0.01010 105.1 <0.0000000000000002
# groupDx    2.32542  10.23093  0.01315 176.9 <0.0000000000000002
# 
# Likelihood ratio test=28268  on 2 df, p=< 0.00000000000000022
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
           legend.labs=c("Dx Metastatic", "Dx no mets",  "None Metastatic", "None no mets", "Pred Metastatic", "Pred no mets"), legend.title="Cachexia Status",  
           palette=c("deepskyblue", "deepskyblue4", "deeppink", "deeppink4", "coral2", "firebrick"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia & Metastasis Status", 
           risk.table.height=0.2)


fit <- coxph(Surv(Survived, status)~group, data=temp)

fit






# ------------------------------------

# Overall prevalence ----------------------------------------------------------------------


Cachexia_Prevalence <- fread("Cachexia_Prevalence.txt")

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


Cachexia_Prevalence <- fread("Cachexia_Prevalence.txt")

cachexia <- Cachexia_Prevalence$`# non-Mets Cachexia`
names(cachexia) <- Cachexia_Prevalence$`Primary Cancer`
patients <- Cachexia_Prevalence$`# non-Mets ≥10 BMIs`
Prop_test(n_succ=cachexia, n_tot=patients)

# <<< 21-sample test for equality of proportions without continuity correction 
# 
# 
# --- Description
# 
#                Bone   Brain   Breast   Gastroesophageal    Head   Intestinal   Kidney   Leukemia   Liver    Lung   Lymphoma   Myeloma   Other   Pancreatic   Prostate   Reproductive   Respiratory   Salivary    Skin   Thyroid   Urinary
# n_              211    1077    11047                799     807         3822     1514       3113    1052    3588       2941      1158     978          660       7658           2879           363        124    1914      1963      2423
# n_total         700    2823    38034               1683    2161        10650     5773       7624    2171    6952       8592      2658    3468         1335      29989          11476           952        501    7676      8928      7107
# proportion    0.301   0.382    0.290              0.475   0.373        0.359    0.262      0.408   0.485   0.516      0.342     0.436   0.282        0.494      0.255          0.251         0.381      0.248   0.249     0.220     0.341
# 
# --- Inference
# 
# Chi-square statistic: 4203.627 
# Degrees of freedom: 20 
# Hypothesis test of equal population proportions: p-value = 0.000



cachexia <- Cachexia_Prevalence$`# Mets Cachexia`
names(cachexia) <- Cachexia_Prevalence$`Primary Cancer`
patients <- Cachexia_Prevalence$`# Mets ≥10 BMIs`
prop.test(cachexia, patients)

 
# <<< 21-sample test for equality of proportions without continuity correction 
# 
# 
# --- Description
# 
#                Bone   Brain   Breast   Gastroesophageal    Head   Intestinal   Kidney   Leukemia   Liver    Lung   Lymphoma   Myeloma   Other   Pancreatic   Prostate   Reproductive   Respiratory   Salivary    Skin   Thyroid   Urinary
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
Cachexia_Dx <- Cachexia_Dx %>% left_join(Pats_to_track_BMI %>% select(patid, cachexia_onset)) %>% select(patid, cachexia_onset)


Cachexia_Pred <- New_Cachexia_Pred  %>% left_join(Pats_to_track_BMI)  %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset) %>% distinct() %>% mutate(group="Pred")
temp <- temp %>% filter(Drop90==1|Drop2_20==1) %>% group_by(patid) %>% filter(Month_Min==min(Month_Min)) %>% slice(1) %>% ungroup()
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


data.frame(Cachexia %>% group_by(group, Primary_Cancer) %>% count() %>% rename("n2"="n") %>%
  left_join(
Cachexia %>% filter(Elapsed<=0) %>% group_by(group, Primary_Cancer) %>% count()
) %>% mutate(perc=n/n2))



Cachexia %>% 
  ggplot(aes(fill=group, colour=group, x=Elapsed)) + 
  geom_density(alpha=0.5)  + 
   theme_minimal() + 
 # facet_wrap(~Primary_Cancer, scales="free_y") +
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

Cachexia %>% filter(ICD=="R18") %>% group_by(group) %>% count()

num <-  c( 3876, 10827, 6082)
names(num) <- c("Dx", "Pred", "None")
den <- c( 14559, 113724, 158533)
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