library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
options(scipen = 999)

# Population Sizing -------------------------
PONS_Demographics <- fread("PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, diagnosis, died, cancer_metastasis, cachexia_onset, has_oncology)
PONS_Demographics %>%  summarise(n=sum(weight)) # 43195153

PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics %>%  summarise(n=sum(weight)) # 42839416


PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 

sum(PONS_Demographics$weight) # 43346626

PONS_Demographics %>% filter(diagnosis == "-") %>% summarise(n=sum(weight)) # 36033347

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )

PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

# PONS_Demographics <- PONS_Demographics %>% filter(died=="N")
# PONS_Demographics <- PONS_Demographics %>% ungroup() %>% filter(diagnosis != "-" & diagnosis != "Skin Cancer" & diagnosis != "Unspecified Cancer") 

data.frame(PONS_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))) %>% arrange(diagnosis) %>% arrange(-n)



PONS_Demographics <- PONS_Demographics %>% filter(died=="N")
PONS_Demographics %>% ungroup() %>% filter(diagnosis != "-" & diagnosis != "Skin Cancer" & diagnosis != "Unspecified Cancer") %>% summarise(n=sum(weight)) # 42839416
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)

data.frame(PONS_Demographics %>% group_by(diagnosis, cancer_metastasis) %>% summarise(n=sum(weight))) %>% 
  arrange(diagnosis) %>% spread(key=cancer_metastasis, value=n)



data.frame(PONS_Demographics %>% group_by(diagnosis, cachexia_onset) %>% summarise(n=sum(weight))) %>% 
  arrange(diagnosis) %>% spread(key=cachexia_onset, value=n)


data.frame(PONS_Demographics %>% group_by(diagnosis, cancer_metastasis, cachexia_onset) %>% summarise(n=sum(weight))) %>% 
  arrange(diagnosis) %>% spread(key=cachexia_onset, value=n) %>% filter(cancer_metastasis==1)

data.frame(PONS_Demographics %>% group_by(diagnosis, cancer_metastasis, cachexia_onset) %>% summarise(n=sum(weight))) %>% 
  arrange(diagnosis) %>% spread(key=cachexia_onset, value=n) %>% filter(cancer_metastasis==0)


data.frame(PONS_Demographics %>% group_by(diagnosis, cancer_metastasis, has_oncology) %>% summarise(n=sum(weight))) %>% 
  arrange(diagnosis) %>% spread(key=has_oncology, value=n)  %>% filter(cancer_metastasis==0)


data.frame(PONS_Demographics %>% group_by(diagnosis, has_oncology) %>% summarise(n=sum(weight))) %>% 
  arrange(diagnosis) %>% spread(key=has_oncology, value=n)  

# ------------
# Population Sizing V2 NEW BOXES -------------------------

PONS_Demographics <- fread("PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset, has_oncology)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box %>% summarise(n=sum(weight)) # 31772455
New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))


PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% select(patid)) %>%  summarise(n=sum(weight)) 
# 31772455

PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box) 

PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics %>%  summarise(n=sum(weight)) # 31772455

PONS_Demographics %>% filter(Primary_Cancer == "-") %>% summarise(n=sum(weight)) # 8787566
PONS_Demographics %>% filter(Primary_Cancer=="Skin Cancer") %>% summarise(n=sum(weight)) # 926934.8
PONS_Demographics %>% filter(Primary_Cancer=="Unspecified Cancer") %>% summarise(n=sum(weight)) # 381981.3
PONS_Demographics %>% filter(Primary_Cancer != "-"&Primary_Cancer!="Skin Cancer"&Primary_Cancer!="Unspecified Cancer") %>% summarise(n=sum(weight)) # 21675973
#PONS_Demographics <- PONS_Demographics %>% filter(Primary_Cancer != "-"&Primary_Cancer!="Skin Cancer"&Primary_Cancer!="Unspecified Cancer")
PONS_Demographics %>% filter(died=="N") %>%  summarise(n=sum(weight)) # 17902818

PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 

data.frame(PONS_Demographics  %>% filter(died=="N") %>% group_by(Primary_Cancer,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(Primary_Cancer, -cancer_metastasis))

# -----------
# Create Summary table for BMI processing (Cancer Type vs metastasis pop) ---------

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)

PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )

PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

data.frame(PONS_Demographics %>% group_by(diagnosis) %>% summarise(TOTAL_POP=sum(weight))) %>%
  left_join(PONS_Demographics %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight))) %>%
  arrange(diagnosis, -cancer_metastasis)

PONS_Demographics <- PONS_Demographics %>% filter(died=="N")

data.frame(PONS_Demographics %>% group_by(diagnosis) %>% summarise(TOTAL_POP=sum(weight))) %>%
  left_join(PONS_Demographics %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight))) %>%
  arrange(diagnosis, -cancer_metastasis)


# -----------


# --------------
# Measures ----------
PONS_Measures <- fread("PONS Measures.txt")

unique(PONS_Measures$unit) # "kg/m2" "cm"    "kg" 
unique(PONS_Measures$description) # "kg/m2" "cm"    "kg" 

PONS_Measures <- PONS_Measures %>% select(-c(unit, source, description, metric, vague_value, vague_date))

fwrite(PONS_Measures, "PONS_Measures_short.txt", sep="\t")

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")

# PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

head(PONS_Measures)

PONS_Measures %>% select(patid, weight, test) %>% distinct() %>% group_by(test) %>% count()


PONS_Measures %>% select(patid, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight))

plot <- PONS_Measures %>% filter(test=="BMI") %>% distinct() %>% 
  select(patid) %>% group_by(patid) %>% count() %>% ungroup() %>%
  filter(n<100) %>%
  ggplot(aes(n)) +
  geom_density(size=2, colour="deepskyblue4", fill="deepskyblue4", alpha=0.7)

plot + theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Number of Unique BMI records")+
  ylab("Patient/Cohort proportion \n")

PONS_Measures <- PONS_Measures %>% filter(test=="BMI")


# Population

PONS_Demographics <- fread("PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, died, cancer_metastasis, cachexia_onset, has_oncology)
PONS_Demographics <- PONS_Demographics %>% filter(died=="N")

PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 

sum(PONS_Demographics$weight) # 43346626

PONS_Demographics %>% filter(diagnosis != "-") %>% summarise(n=sum(weight)) # 36033347

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )

PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

data.frame(PONS_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight))) %>% arrange(diagnosis)

Prostate_Pats <- PONS_Demographics %>% filter(diagnosis=="Prostate Cancer") %>% select(patid)
rm(PONS_Demographics)

PONS_Measures <- Prostate_Pats %>% inner_join(PONS_Measures)
PONS_Measures$claimed <- PONS_Measures$claimed

temp <- PONS_Measures %>% group_by(patid) %>% filter(claimed==min(claimed)|claimed==max(claimed)) %>% 
  mutate(claimed=ifelse(claimed==min(claimed),"Min","Max")) %>% group_by(patid, claimed) %>% summarise(n=mean(value))

temp <- temp %>% ungroup() %>% group_by(patid) %>% count() %>% filter(n>=2) %>% select(patid) %>% left_join(temp)

temp <- temp %>% spread(key=claimed, value=n)

temp <- temp %>% mutate(Difference=Max-Min)

temp %>% ggplot(aes(Difference))+
  geom_density()
  

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")

# -----------

# BMI reductions ------------------


# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
 arrange(diagnosis, -cancer_metastasis)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis,cachexia_onset)

Pats_to_track_BMI %>% ungroup() %>% filter(diagnosis!="-"&diagnosis!="Skin Cancer"&diagnosis!="Unspecified Cancer") %>% filter(cachexia_onset==1) %>% summarise(n=sum(weight)) # 529051.3



PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690714 (25960778 records, 857091 samples)
length(unique(PONS_Measures$patid))


data.frame(PONS_Measures %>% select(patid, weight) %>% distinct() %>% inner_join(Pats_to_track_BMI) %>%
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
  arrange(diagnosis, -cancer_metastasis))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)
 

PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) %>% select(patid) %>% distinct()
PONS_Measures %>% ungroup() %>% filter(value<1.35*median&value>0.65*median) %>% select(patid) %>% distinct()
PONS_Measures %>% ungroup() %>% filter(value<1.25*median&value>0.75*median) %>% select(patid) %>% distinct()



# *** Replace back with median +/- 1.5 |  0.5
# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% mutate(TimeElapsed=1+as.numeric(claimed)-as.numeric(lag(claimed)))
# 
# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% mutate(Difference= (100 * (value-lag(value)) / lag(value ))) %>%
#   mutate(DifferencePerDay= (100 * (value-lag(value)) / lag(value )) / TimeElapsed )
# 
# PONS_Measures %>% ungroup() %>% 
#   mutate(ToRemove = ifelse( Difference>20&TimeElapsed>30.5,"YES","NO") ) %>% filter(is.na(ToRemove)|ToRemove=="NO") %>%
#   select(patid) %>% distinct()
#   
# 
# temp <- PONS_Measures %>% ungroup() %>% 
#   mutate(ToRemove = ifelse( Difference>20&TimeElapsed>30.5,"YES","NO") ) %>% 
#   filter(is.na(ToRemove)|ToRemove=="NO") %>%
#   filter(value<1.5*median&value>0.5*median)
#   
# length(unique(temp$patid))
# ***

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690470 (25931366 records, 852569 samples) (-244 pats, haven't lost patients, simply 29412 records)


data.frame(
  PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=2) %>% 
    select(patid) %>%
    left_join(PONS_Measures) %>% select(patid, weight) %>% distinct() %>% 
    inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)


Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()


PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=2) %>% select(patid) %>% 
  inner_join(Min_Max_Dates) %>% select(patid) %>% distinct() # 841731


PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=2) %>% select(patid) %>% distinct() # 841731
#837259

PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=5) %>% select(patid) %>% distinct() # 841731
#645740

PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% distinct() # 841731
#515914

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))


PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

data.frame(PONS_Measures %>% ungroup() %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 


data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate) %>%
  select(patid, weight) %>% distinct() %>%  
  bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
    distinct() %>%
  inner_join(Pats_to_track_BMI) %>%
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
  arrange(diagnosis, -cancer_metastasis))

PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))  

data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))




PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 

PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) %>%
  select(patid, weight) %>% distinct() %>% 
  inner_join(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid,weight) %>% distinct()) %>%
  distinct()  %>% summarise(n=sum(weight)) 

data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) %>%
             select(patid, weight) %>% distinct() %>% 
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 

PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366) %>%
  select(patid, weight) %>% distinct() %>% 
  inner_join(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid,weight) %>% distinct()) %>%
  distinct()  %>% summarise(n=sum(weight)) 

data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) ) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))



data.frame(PONS_Measures %>% ungroup() %>% filter( min<max*0.98 & min<20 & mindate>maxdate ) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))




data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) | (min<max*0.98 & min<20 & mindate>maxdate) ) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))






PONS_Measures %>% ungroup() %>% filter(min<max*0.98 & min<20 & mindate>maxdate) %>%
  select(patid, weight) %>% distinct() %>% 
  inner_join(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid,weight) %>% distinct()) %>%
  distinct()  %>% summarise(n=sum(weight)) 


# Still Alive + Active (not remission)
PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis")) %>% select(patid)
  
data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate ) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


Pats_to_track_BMI %>% ungroup()  %>% filter(cachexia_onset==1) %>% summarise(n=sum(weight)) # 679957

data.frame(Pats_to_track_BMI %>% ungroup()  %>% filter(cachexia_onset==1) %>%
             anti_join(PONS_Measures %>% ungroup() %>% filter( (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) | (min<max*0.98 & min<20 & mindate>maxdate)) %>%
                         select(patid, weight) %>% distinct() %>%  
                         inner_join(Pats_to_track_BMI) %>%
                         select(patid) %>% distinct()) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis, cancer_metastasis) %>%
             summarise(n=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))



Cachexia_pats <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%
  bind_rows(Pats_to_track_BMI %>% ungroup()  %>% filter(cachexia_onset==1) %>% select(patid, weight)) %>% distinct() 

Cachexia_pats$Cachexia <- "Cachexia"

fwrite(Cachexia_pats, "Cachexia_pats.txt", sep="\t")












# ------
# BMI reductions V2 NEW BOXES ------------------


# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
# PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
# PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
# PONS_Demographics %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
#  arrange(diagnosis, -cancer_metastasis)
#***
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics %>% select(-diagnosis))
names(PONS_Demographics)[4] <- "diagnosis"
#***
#*
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis,cachexia_onset)

Pats_to_track_BMI %>% ungroup() %>% filter(diagnosis!="-"&diagnosis!="Skin Cancer"&diagnosis!="Unspecified Cancer") %>% filter(cachexia_onset==1) %>% summarise(n=sum(weight)) # 529051.3



PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690714 (852576 samples)
length(unique(PONS_Measures$patid))


data.frame(PONS_Measures %>% select(patid, weight) %>% distinct() %>% inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)


# PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) %>% select(patid) %>% distinct()
# PONS_Measures %>% ungroup() %>% filter(value<1.35*median&value>0.65*median) %>% select(patid) %>% distinct()
# PONS_Measures %>% ungroup() %>% filter(value<1.25*median&value>0.75*median) %>% select(patid) %>% distinct()


# *** Replace back with median +/- 1.5 |  0.5
# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% mutate(TimeElapsed=1+as.numeric(claimed)-as.numeric(lag(claimed)))
# 
# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% mutate(Difference= (100 * (value-lag(value)) / lag(value ))) %>%
#   mutate(DifferencePerDay= (100 * (value-lag(value)) / lag(value )) / TimeElapsed )
# 
# PONS_Measures %>% ungroup() %>% 
#   mutate(ToRemove = ifelse( Difference>20&TimeElapsed>30.5,"YES","NO") ) %>% filter(is.na(ToRemove)|ToRemove=="NO") %>%
#   select(patid) %>% distinct()
#   
# 
# temp <- PONS_Measures %>% ungroup() %>% 
#   mutate(ToRemove = ifelse( Difference>20&TimeElapsed>30.5,"YES","NO") ) %>% 
#   filter(is.na(ToRemove)|ToRemove=="NO") %>%
#   filter(value<1.5*median&value>0.5*median)
#   
# length(unique(temp$patid))
# ***

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690470 (25931366 records, 852569 samples) (-244 pats, haven't lost patients, simply 29412 records)


data.frame(
  PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=2) %>% 
    select(patid) %>%
    left_join(PONS_Measures) %>% select(patid, weight) %>% distinct() %>% 
    inner_join(Pats_to_track_BMI) %>%
    group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
    arrange(diagnosis, -cancer_metastasis))


Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)


Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()


PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=2) %>% select(patid) %>% 
  inner_join(Min_Max_Dates) %>% select(patid) %>% distinct()

PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=2) %>% select(patid) %>% distinct() 
PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=5) %>% select(patid) %>% distinct()
PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis")) %>% select(patid)


data.frame(PONS_Measures %>% ungroup() %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 


data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))

PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))  

data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))




PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 

PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) %>%
  select(patid, weight) %>% distinct() %>% 
  inner_join(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid,weight) %>% distinct()) %>%
  distinct()  %>% summarise(n=sum(weight)) 

data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) %>%
             select(patid, weight) %>% distinct() %>% 
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 

PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366) %>%
  select(patid, weight) %>% distinct() %>% 
  inner_join(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid,weight) %>% distinct()) %>%
  distinct()  %>% summarise(n=sum(weight)) 

data.frame(PONS_Measures %>% ungroup() %>% filter(min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) ) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))



data.frame(PONS_Measures %>% ungroup() %>% filter( min<max*0.98 & min<20 & mindate>maxdate ) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))




data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) | (min<max*0.98 & min<20 & mindate>maxdate) ) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))






PONS_Measures %>% ungroup() %>% filter(min<max*0.98 & min<20 & mindate>maxdate) %>%
  select(patid, weight) %>% distinct() %>% 
  inner_join(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid,weight) %>% distinct()) %>%
  distinct()  %>% summarise(n=sum(weight)) 


# Still Alive + Active (not remission)
PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis")) %>% select(patid)

data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate ) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


Pats_to_track_BMI %>% ungroup()  %>% filter(cachexia_onset==1) %>% summarise(n=sum(weight)) # 679957

data.frame(Pats_to_track_BMI %>% ungroup()  %>% filter(cachexia_onset==1) %>%
             anti_join(PONS_Measures %>% ungroup() %>% filter( (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) | (min<max*0.98 & min<20 & mindate>maxdate)) %>%
                         select(patid, weight) %>% distinct() %>%  
                         inner_join(Pats_to_track_BMI) %>%
                         select(patid) %>% distinct()) %>%
             filter(died=="N") %>%
             inner_join(PONS_Demographics) %>%
             group_by(diagnosis, cancer_metastasis) %>%
             summarise(n=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))



Cachexia_pats <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%
  bind_rows(Pats_to_track_BMI %>% ungroup()  %>% filter(cachexia_onset==1) %>% select(patid, weight)) %>% distinct() 

Cachexia_pats$Cachexia <- "Cachexia"

fwrite(Cachexia_pats, "Cachexia_pats.txt", sep="\t")



# ----------
# Incidence of each cancer type year-over-year ---------------------------------

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)

sum(PONS_Demographics$weight) # 42839416

PONS_Demographics %>% filter(diagnosis != "-") %>% summarise(n=sum(weight)) # 34051850

PONS_Demographics <- PONS_Demographics %>% filter(diagnosis != "-")

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )

PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(-age)
PONS_Demographics <- PONS_Demographics %>% ungroup()
names(PONS_Demographics)[3] <- "condition"





PONS_Dossiers <- fread("PONS Dossiers.txt")
length(unique(PONS_Dossiers$patid))
PONS_Dossiers$earliest <- as.Date(PONS_Dossiers$earliest)
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, earliest)

PONS_Dossiers <- PONS_Dossiers %>% inner_join(PONS_Demographics) %>% group_by(patid) %>% filter(earliest==min(earliest))
length(unique(PONS_Dossiers$patid))
PONS_Dossiers <- PONS_Dossiers %>% group_by(patid) %>% slice(1) %>% select(-diagnosis) %>% ungroup()


PONS_Dossiers %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight)) # 2912743
PONS_Dossiers %>% filter(earliest>="2019-08-01"&earliest<="2020-07-31") %>% summarise(n=sum(weight)) # 3154587
PONS_Dossiers %>% filter(earliest>="2018-08-01"&earliest<="2019-07-31") %>% summarise(n=sum(weight)) # 4194344
PONS_Dossiers %>% filter(earliest>="2017-08-01"&earliest<="2018-07-31") %>% summarise(n=sum(weight)) # 5553143
PONS_Dossiers %>% filter(earliest>="2016-08-01"&earliest<="2017-07-31") %>% summarise(n=sum(weight)) # 18237033


PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight)) # 1904498
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2019-08-01"&earliest<="2020-07-31") %>% summarise(n=sum(weight)) # 1974692
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2018-08-01"&earliest<="2019-07-31") %>% summarise(n=sum(weight)) # 2479683
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2017-08-01"&earliest<="2018-07-31") %>% summarise(n=sum(weight)) # 3103006
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2016-08-01"&earliest<="2017-07-31") %>% summarise(n=sum(weight)) # 12032842

# 1914132+1981954+2487059+3113598+12068937 = 21565680 Pats [excluding benign & excluding <18 y/o & excluding unspecified cases]


data.frame(PONS_Dossiers %>% group_by(condition) %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight))) # 3119190



# ----------------

# IGNORE ! Remove unspecific codes, re-alocate patients ----------------------------

PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, age, gender, code, earliest)

Filtered_Codes_ToKeep <- fread("Filtered_Codes_ToKeep.csv")

PONS_Dossiers <- PONS_Dossiers %>% inner_join(Filtered_Codes_ToKeep)
PONS_Dossiers <- PONS_Dossiers %>% filter(age>=18)
PONS_Dossiers <- PONS_Dossiers %>% select(-c(diagnosis, code, gender, age))

PONS_Dossiers %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 30623449 (instead of 35746025)

PONS_Dossiers <- PONS_Dossiers %>% distinct()

PONS_Dossiers$earliest <- as.Date(PONS_Dossiers$earliest)

PONS_Dossiers <- PONS_Dossiers %>% arrange(patid, earliest)

PONS_Dossiers %>% group_by(patid) %>% filter(earliest==min(earliest)) %>% 
  filter(condition=="Cachexia"|condition=="Unspecified Cancer") %>% ungroup()

PONS_Dossiers <- PONS_Dossiers %>%
  anti_join(PONS_Dossiers %>% group_by(patid) %>% filter(earliest==min(earliest)) %>% 
              filter(condition=="Cachexia"|condition=="Unspecified Cancer") %>% ungroup()) %>%
  group_by(patid) %>% filter(earliest==min(earliest))

length(unique(PONS_Dossiers$patid))

Single_Dx_pats <- PONS_Dossiers %>% group_by(patid) %>% count() %>% filter(n==1)
names(Single_Dx_pats)[2] <- "Rank"

PONS_Dossiers <- PONS_Dossiers %>% left_join(Single_Dx_pats)

Rank_Cancer_Dx <- fread("Rank_Cancer_Dx.csv")
names(Rank_Cancer_Dx)[1] <- "condition"

PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% left_join(Rank_Cancer_Dx) %>%
  mutate(Rank=ifelse(is.na(Rank),Rank_Dxs, Rank)) %>% select(-Rank_Dxs) %>%
  group_by(patid) %>% filter(Rank==min(Rank))

length(unique(PONS_Dossiers$patid)) # 879268

names(PONS_Dossiers)[3] <- "Primary_Cancer"

PONS_Dossiers <- PONS_Dossiers %>% select(-Rank)

fwrite(PONS_Dossiers, "Primary_Cancer_per_Pat.txt", sep="\t")

Primary_Cancer_per_Pat <- fread("Primary_Cancer_per_Pat.txt", sep="\t")

sum(Primary_Cancer_per_Pat$weight) # 26692179

data.frame(PONS_Dossiers %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight)) %>% arrange(-n))


# ---------

# IGNORE ! Incidence of each cancer type year-over-year NEW PRIMARY DX ----------------


Primary_Cancer_per_Pat <- fread("Primary_Cancer_per_Pat.txt", sep="\t")
Primary_Cancer_per_Pat$earliest <- as.Date(Primary_Cancer_per_Pat$earliest)

Primary_Cancer_per_Pat %>% filter(Primary_Cancer!="Skin Cancer") %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight)) # 2825834
Primary_Cancer_per_Pat %>% filter(earliest>="2019-08-01"&earliest<="2020-07-31") %>% summarise(n=sum(weight)) # 2922103
Primary_Cancer_per_Pat %>% filter(earliest>="2018-08-01"&earliest<="2019-07-31") %>% summarise(n=sum(weight)) # 3640494
Primary_Cancer_per_Pat %>% filter(earliest>="2017-08-01"&earliest<="2018-07-31") %>% summarise(n=sum(weight)) # 4399878
Primary_Cancer_per_Pat %>% filter(earliest>="2016-08-01"&earliest<="2017-07-31") %>% summarise(n=sum(weight)) # 12903869

# 12903869+4399878+3640494+2922103+2825834 = 26692178

# ------------


# IGNORE ! Check if the algorythm gives the same figures as Mark's ------------

# Mark's Primary Cancer Dist
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
sum(PONS_Demographics$weight) # 42990612
PONS_Demographics %>% filter(diagnosis != "-") %>% summarise(n=sum(weight)) # 35746025
PONS_Demographics <- PONS_Demographics %>% filter(diagnosis != "-")
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(-age)
PONS_Demographics <- PONS_Demographics %>% ungroup()
names(PONS_Demographics)[3] <- "condition"

data.frame(PONS_Demographics %>% group_by(condition) %>% summarise(n=sum(weight)) %>% arrange(-n))


sum(PONS_Demographics$weight) # 35746025


PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, age, gender, code, earliest)
PONS_Dossiers <- PONS_Dossiers %>% filter(age>=18)
PONS_Dossiers <- PONS_Dossiers %>% select(-c(diagnosis, code, gender, age))
PONS_Dossiers %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 42408378

PONS_Dossiers <- PONS_Dossiers %>% distinct()
PONS_Dossiers$earliest <- as.Date(PONS_Dossiers$earliest)
PONS_Dossiers <- PONS_Dossiers %>% arrange(patid, earliest)

PONS_Dossiers <- PONS_Dossiers %>% group_by(patid) %>% filter(earliest==min(earliest))

length(unique(PONS_Dossiers$patid))

Single_Dx_pats <- PONS_Dossiers %>% group_by(patid) %>% count() %>% filter(n==1)
names(Single_Dx_pats)[2] <- "Rank"
PONS_Dossiers <- PONS_Dossiers %>% left_join(Single_Dx_pats)

Rank_Cancer_Dx <- fread("Rank_Cancer_Dx.csv")
names(Rank_Cancer_Dx)[1] <- "condition"

PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% left_join(Rank_Cancer_Dx) %>%
  mutate(Rank=ifelse(is.na(Rank),Rank_Dxs, Rank)) %>% select(-Rank_Dxs) %>%
  group_by(patid) %>% filter(Rank==min(Rank))

length(unique(PONS_Dossiers$patid)) # 879268

names(PONS_Dossiers)[3] <- "Primary_Cancer"
PONS_Dossiers <- PONS_Dossiers %>% select(-Rank)

data.frame(PONS_Dossiers %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight)) %>% arrange(-n))

sum(PONS_Dossiers$weight)

data.frame(PONS_Dossiers %>% filter(!grepl("Benign", Primary_Cancer)) %>% group_by(Primary_Cancer) %>%
             summarise(n=sum(weight)) %>% arrange(-n))


# ------------
# Mortality --------------------------------

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, death_date, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)

sum(PONS_Demographics$weight) # 42990612

PONS_Demographics$death_date <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2050-12-31")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

# PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
# 
# PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
# PONS_Demographics <- PONS_Demographics %>% select(-age)
# PONS_Demographics <- PONS_Demographics %>% ungroup()
# names(PONS_Demographics)[4] <- "condition"


final <- fread("New_Primary_Cancer_Box.txt", sep="\t")

PONS_Demographics <- PONS_Demographics %>% inner_join(final)



PONS_Demographics %>% filter(Primary_Cancer!="-") %>% left_join(temp) %>% filter(death_date >="2020-08-01"&death_date <="2021-07-31") %>% group_by(Status) %>% summarise(n=sum(weight)) # 950719.1

PONS_Demographics %>% filter(Primary_Cancer!="-") %>%  filter(death_date >="2019-08-01"&death_date <="2020-07-31") %>% summarise(n=sum(weight)) # 1001938
PONS_Demographics %>% filter(Primary_Cancer!="-") %>% filter(death_date >="2018-08-01"&death_date <="2019-07-31") %>% summarise(n=sum(weight)) # 985952.2
PONS_Demographics %>% filter(Primary_Cancer!="-") %>% filter(death_date >="2017-08-01"&death_date <="2018-07-31") %>% summarise(n=sum(weight)) # 958642.8
PONS_Demographics %>% filter(Primary_Cancer!="-") %>% filter(death_date >="2016-08-01"&death_date <="2017-07-31") %>% summarise(n=sum(weight)) # 0

temp <- fread("PONS_Time_Series_Groups.txt", sep="\t")
temp <- temp %>% filter(Exact_Month==48) %>% select(patid, Status)

data.frame(PONS_Demographics %>%  filter(Primary_Cancer!="-") %>%
             group_by(Primary_Cancer) %>% filter(death_date>="2020-08-01"&death_date<="2021-07-31") %>% summarise(n=sum(weight)))


# --------------
# BMI Distributions ------------------------------------------------------------------------------------
# Get patient primary cancer
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, diagnosis, cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
sum(PONS_Demographics$weight) # 42990612
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(-age)
PONS_Demographics <- PONS_Demographics %>% ungroup()
names(PONS_Demographics)[3] <- "condition"

# BMI records
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25775556
PONS_Measures <- PONS_Measures %>% select(-test)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)



# All records
PONS_Measures %>% select(value) %>% summarise(n=mean(value)) # 29.43935
PONS_Measures %>% select(value) %>% summarise(n=median(value)) # 28.3388

plot <- PONS_Measures %>% select(value) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="deepskyblue4", fill="deepskyblue4", alpha=0.7)

plot + xlim(10,60)+ theme(panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank())+
  xlab("\n BMI")+
  ylab("Proportion of all records \n")


# Max record per patient
PONS_Measures %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=mean(value)) # 30.6
PONS_Measures %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=median(value)) # 29.3

plot <- PONS_Measures %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% select(value) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="deeppink4", fill="deeppink4", alpha=0.7)

plot + xlim(10,60)+ theme(panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank())+
  xlab("\n Max BMI")+
  ylab("Proportion of all records \n")



# Min record per patient
PONS_Measures %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=mean(value)) # 27.2
PONS_Measures %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=median(value)) # 26.4

plot <- PONS_Measures %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% select(value) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="deeppink2", fill="deeppink2", alpha=0.7)

plot + xlim(10,60)+ theme(panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank())+
  xlab("\n Min BMI")+
  ylab("Proportion of all records \n")




# Last record per patient
PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=mean(value)) # 28.6
PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=median(value)) # 28.3388

plot <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% select(value) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="deepskyblue3", fill="deepskyblue3", alpha=0.7)

plot + xlim(10,60)+ theme(panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank())+
  xlab("\n Last BMI")+
  ylab("Proportion of all records \n")




# Last record per patient
PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=mean(value)) # 28.6
PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% ungroup() %>% select(value) %>% summarise(n=median(value)) # 28.3388

plot <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% select(value) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="deepskyblue3", fill="deepskyblue3", alpha=0.7)

plot + xlim(10,60)+ theme(panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank())+
  xlab("\n Last BMI")+
  ylab("Proportion of all records \n")



# Last record per patient
data.frame(PONS_Measures %>% inner_join(PONS_Demographics) %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% ungroup() %>% group_by(condition) %>% select(condition, value) %>% summarise(n=mean(value))) # 28.6
data.frame(PONS_Measures %>% inner_join(PONS_Demographics) %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% ungroup() %>% group_by(condition) %>% select(condition, value) %>% summarise(n=median(value))) # 28.3388

plot <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1) %>% select(value) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="deepskyblue3", fill="deepskyblue3", alpha=0.7)

plot + xlim(10,60)+ theme(panel.grid.major=element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank())+
  xlab("\n Last BMI")+
  ylab("Proportion of all records \n")

# Last BMI Cachexia Dx vs Pred vs None 
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, diagnosis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
sum(PONS_Demographics$weight) # 42990612
PONS_Demographics <- PONS_Demographics %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),"None","Dxed")) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(-age)
PONS_Demographics <- PONS_Demographics %>% ungroup()
names(PONS_Demographics)[3] <- "condition"

PONS_Demographics <- PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") 

Cachexia_pats <- fread("Cachexia_pats.txt", sep="\t")

PONS_Demographics <- PONS_Demographics %>% left_join(Cachexia_pats)

PONS_Demographics <- PONS_Demographics %>% mutate(Cachexia_Final=ifelse(cachexia_onset=="Dxed","Dxed", NA))
PONS_Demographics <- PONS_Demographics %>% mutate(Cachexia_Final=ifelse(is.na(Cachexia_Final)&Cachexia=="Cachexia","Pred", Cachexia_Final))


# BMI records
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25775556
PONS_Measures <- PONS_Measures %>% select(-test)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

# Last record per patient
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1)

PONS_Demographics %>% inner_join(PONS_Measures) %>% group_by(Cachexia_Final) %>% summarise(n=weighted.mean(value, weight))


PONS_Demographics %>% inner_join(PONS_Measures) %>% group_by(Cachexia_Final) %>% summarise(n=weighted.median(value, weight))




PONS_Demographics %>% inner_join(PONS_Measures) %>% group_by(Cachexia_Final) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="midnightblue", fill="midnightblue", alpha=0.7) +
  facet_wrap(~Cachexia_Final) + 
  xlim(10,50) + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Last BMI")+
  ylab("Proportion of all records \n")
  #theme_economist(dkpanel = TRUE)+scale_colour_economist()
  



# Death vs Alive
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, diagnosis, death_date)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
sum(PONS_Demographics$weight) # 42990612
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=ifelse(is.na(death_date),"None","Died")) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(-age)
PONS_Demographics <- PONS_Demographics %>% ungroup()
names(PONS_Demographics)[3] <- "condition"

PONS_Demographics <- PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") 


# BMI records
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25775556
PONS_Measures <- PONS_Measures %>% select(-test)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

# Last record per patient
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% slice(1)

PONS_Demographics %>% inner_join(PONS_Measures) %>% group_by(death_date) %>% summarise(n=weighted.mean(value, weight))



PONS_Demographics %>% inner_join(PONS_Measures) %>% group_by(death_date) %>% summarise(n=weighted.median(value, weight))



PONS_Demographics %>% inner_join(PONS_Measures) %>% group_by(death_date) %>%
  ggplot(aes(value)) +
  geom_density(size=2, colour="firebrick", fill="firebrick", alpha=0.7) +
  facet_wrap(~death_date) + 
  xlim(10,50) + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Last BMI")+
  ylab("Proportion of all records \n")+
  theme_economist(dkpanel = TRUE)+scale_colour_economist()





# ------------
# BMI reductions in identified cachexia patients -----------------

# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

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



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


# Patients that fit our criteria

PONS_Measures_selected_cachexia <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%  
  # bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
  distinct() %>% inner_join(PONS_Measures)




# PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(difference=min-max)
# 
# temp <- PONS_Measures_selected_cachexia %>% select(patid, weight, difference) %>%
#   group_by(patid) %>% filter(difference==min(difference)) %>% slice(1) %>% arrange(difference)
# 
# weighted.mean(temp$difference, temp$weight) # -5.587503
# weighted.median(temp$difference, temp$weight) # -4.4


PONS_Measures_selected_cachexia %>% mutate(PercentMax = 100*value/max)

temp %>% select(difference) %>%
  ggplot(aes(difference))+
  geom_density(size=2, colour="brown4", fill="brown4", alpha=0.7) +
  xlim(-20,0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n MAX BMI Drop")+
  ylab("Proportion of patients \n")


temp %>% mutate(Group=ifelse(abs(difference)>15,"-15",
                             ifelse(abs(difference)>10,"-10",
                                    ifelse(abs(difference)>5,"-5","0-5")))) %>%
  ungroup %>% group_by(Group) %>% summarise(n=sum(weight))


PONS_Measures_selected_cachexia %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))


temp <- temp %>% inner_join(Pats_to_track_BMI %>% select(-cancer_metastasis, -cachexia_onset))
data.frame(temp %>% ungroup() %>% group_by(diagnosis) %>% summarise(n=mean(difference))) %>% arrange(n)





sum(Pats_to_track_BMI$weight) # 42990612
Pats_to_track_BMI <- Pats_to_track_BMI %>% ungroup() %>% filter(diagnosis!="-"&diagnosis!="Skin Cancer")
sum(Pats_to_track_BMI$weight) # 23423393

PONS_Measures_selected_cachexia %>% inner_join(Pats_to_track_BMI) %>% select(patid, weight) %>% 
  distinct() %>% summarise(n=sum(weight)) # 2544494

PONS_Measures_selected_cachexia %>% filter(max>30) %>% inner_join(Pats_to_track_BMI) %>% select(patid, weight) %>% 
  distinct() %>% summarise(n=sum(weight)) # 1020948

PONS_Measures_selected_cachexia %>% filter(max>40) %>% inner_join(Pats_to_track_BMI) %>% select(patid, weight) %>% 
  distinct() %>% summarise(n=sum(weight)) # 240918



# ---------

# Populaiton +18, Alive, Active -----------
# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
# PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
# PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
# PONS_Demographics %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
#  arrange(diagnosis, -cancer_metastasis)
#***
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics %>% select(-diagnosis))
names(PONS_Demographics)[4] <- "diagnosis"
#***
#*
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis,cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(died=="N")



PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis")) %>% select(patid)

Pats_to_track_BMI <- Pats_to_track_BMI %>% inner_join(PONS_Demographics)
sum(Pats_to_track_BMI$weight)

data.frame(Pats_to_track_BMI %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)) %>% ungroup() %>% summarise(n=sum(POP_Mets))

# ------------
# How many on year 4 and year 5 alive ? -------------------
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, death_date, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

PONS_Demographics <- PONS_Demographics %>% filter(diagnosis!="-" & diagnosis!="Skin Cancer" & diagnosis!="Unspecified Cancer")
PONS_Demographics$death_date <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))


# How many in total
sum(PONS_Demographics$weight) # 21494720

# How many alive in total at the end
PONS_Demographics %>% ungroup() %>% filter(died=="N") %>% summarise(n=sum(weight)) # 17784650 # 60  
# (17802670 plus the ones that ever died 3692049 = 21494719)


# -------------
# How many died within 1 year os last Dx ? -----------------
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, death_date, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
sum(PONS_Demographics$weight) # 42990612

PONS_Demographics$death_date <- as.Date(PONS_Demographics$death_date)
missingDeathDay <- ymd("2050-12-31")
PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(-age)
PONS_Demographics <- PONS_Demographics %>% ungroup()
names(PONS_Demographics)[4] <- "death_date"
names(PONS_Demographics)[5] <- "condition"

PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>% filter(death_date >="2020-08-01"&death_date <="2021-07-31") %>% summarise(n=sum(weight)) # 889856
PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>% filter(death_date >="2019-08-01"&death_date <="2020-07-31") %>% summarise(n=sum(weight)) # 945186
PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>% filter(death_date >="2018-08-01"&death_date <="2019-07-31") %>% summarise(n=sum(weight)) # 936585
PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>% filter(death_date >="2017-08-01"&death_date <="2018-07-31") %>% summarise(n=sum(weight)) # 920422
PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>% filter(death_date >="2016-08-01"&death_date <="2017-07-31") %>% summarise(n=sum(weight)) # 0

# 889856+945186+936585+920422 = 3692049 died

data.frame(PONS_Demographics %>%  filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
             group_by(condition) %>% filter(death_date>="2020-08-01"&death_date<="2021-07-31") %>% summarise(n=sum(weight)))

# Get time last seen for each patient (excluding history, benign )

PONS_Dossiers <- fread("PONS Dossiers.txt")
length(unique(PONS_Dossiers$patid))
PONS_Dossiers$latest  <- as.Date(PONS_Dossiers$latest )
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, latest)
PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis!="Benign Diagnosis"&diagnosis!="Cancer History"&diagnosis!="Benign History")
PONS_Dossiers <- PONS_Dossiers %>% inner_join(PONS_Demographics %>% select(patid)) %>% group_by(patid) %>% filter(latest==max(latest)) %>% slice(1)
PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% select(-c(weight, diagnosis, condition)) 
PONS_Dossiers


temp <- PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer")

temp <- temp %>% mutate(difference=death_date-latest)

PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  filter(death_date>latest) %>% filter(death_date<latest+365) %>%
  filter(death_date >="2020-08-01"&death_date <="2021-07-31")  %>% select(patid, weight) %>%  summarise(n=sum(weight)) # 702890



PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  filter(death_date>latest) %>% filter(death_date<latest+365) %>%
  filter(death_date >="2020-08-01"&death_date <="2021-07-31")  %>%
  select(patid, weight, condition) %>% group_by(condition) %>%  summarise(n=sum(weight)) 


PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  filter(death_date>latest) %>% filter(death_date<latest+365) %>%
  filter(death_date >="2019-08-01"&death_date <="2020-07-31")  %>% select(patid, weight) %>%  summarise(n=sum(weight)) # 809984


PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  filter(death_date>latest) %>% filter(death_date<latest+365) %>%
  filter(death_date >="2018-08-01"&death_date <="2019-07-31")  %>% select(patid, weight) %>%  summarise(n=sum(weight)) # 828287


PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  filter(death_date>latest) %>% filter(death_date<latest+365) %>%
  filter(death_date >="2017-08-01"&death_date <="2018-07-31")  %>% select(patid, weight) %>%  summarise(n=sum(weight)) # 879901


PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  filter(death_date>latest) %>% filter(death_date<latest+365) %>%
  filter(death_date >="2016-08-01"&death_date <="2017-07-31")  %>% select(patid, weight) %>%  summarise(n=sum(weight)) # 0


# ---------

# Create Mortality Check Flag for Mark  -----------------
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics$death_date <- as.Date(PONS_Demographics$death_date)

PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers$latest <- as.Date(PONS_Dossiers$latest)

PONS_Dossiers <- PONS_Dossiers %>% left_join(PONS_Demographics)

PONS_Dossiers <- PONS_Dossiers %>% mutate(TimeDeath_ToLatest=death_date-latest) %>% 
  mutate(Flag_ToCheck=ifelse(TimeDeath_ToLatest<0,1,0))

fwrite(PONS_Dossiers, "PONS_Dossiers_2_Mortality_Check.txt", sep="\t")



# -----------
# Time from last Dx (exc. Benign or History) to month 60 (or to death?) --------

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, death_date, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
sum(PONS_Demographics$weight) # 42990612

PONS_Demographics$death_date <- as.Date(PONS_Demographics$death_date)
missingDeathDay <- ymd("2021-07-31")
PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(-age)
PONS_Demographics <- PONS_Demographics %>% ungroup()
names(PONS_Demographics)[4] <- "death_orFinish_date"
names(PONS_Demographics)[5] <- "condition"
unique(PONS_Demographics$condition)



PONS_Dossiers_temp <- fread("PONS Dossiers.txt")
PONS_Dossiers <- fread("PONS Dossiers.txt")
length(unique(PONS_Dossiers$patid))
PONS_Dossiers$latest  <- as.Date(PONS_Dossiers$latest )
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, latest)
PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis!="Benign Diagnosis"&diagnosis!="Cancer History"&diagnosis!="Benign History")
PONS_Dossiers <- PONS_Dossiers %>% inner_join(PONS_Demographics %>% select(patid)) %>% group_by(patid) %>% filter(latest==max(latest)) %>% slice(1)
PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% select(-c(weight, diagnosis, condition)) 
PONS_Dossiers

PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Dossiers) %>% filter(!is.na(latest)) 

PONS_Demographics <- PONS_Demographics %>% mutate(lapsedTime=as.numeric(death_orFinish_date-latest)/30.5)

PONS_Demographics %>%  filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  select(lapsedTime) %>%
  ggplot(aes(lapsedTime))+
  geom_density(size=1, colour="firebrick", fill="firebrick", alpha=0.7)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n No. Months From Last Dx to End of Follow-up (or Death)")+
  ylab("Patient/Cohort proportion \n")+
  xlim(0,60)


PONS_Demographics %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 37922112

PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>% 
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 21565679


PONS_Demographics %>% filter(condition!="-"&condition!="Skin Cancer"&condition!="Unspecified Cancer") %>%
  mutate(Group=ifelse(lapsedTime<=0, "<0",
                      ifelse(lapsedTime<12,"< 1 Year",
                             ifelse(lapsedTime<24,"< 2 Year",
                                    ifelse(lapsedTime<36, "< 3 Year",
                                           ifelse(lapsedTime<48, "< 4 Year",
                                                  ifelse(lapsedTime<=60, "<5 Year"))))))) %>%
  select(patid, weight, Group) %>% distinct() %>% group_by(Group) %>% summarise(n=sum(weight))


# ------------
# Cancer timeline history long table ------------------------
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

# Mortality
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, death_date, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

PONS_Demographics <- PONS_Demographics %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, death_date)
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

PONS_Demographics %>% drop_na()
PONS_Demographics$Death_Date <- PONS_Demographics$death_date
Death_Date <- PONS_Demographics

PONS_Demographics <- PONS_Demographics %>% select(patid, weight) %>% mutate(Repeat=60) 

PONS_Demographics <- expandRows(PONS_Demographics, "Repeat")


PONS_Demographics$Exact_Month <- ave(PONS_Demographics$patid, PONS_Demographics$patid,FUN = seq_along)
PONS_Demographics$Exact_Month <- as.numeric(PONS_Demographics$Exact_Month)

Death_Date <- Death_Date %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)
Death_Date <- Death_Date %>% drop_na()
Death_Date$Death <- "Death"

PONS_Demographics <- PONS_Demographics %>% left_join(Death_Date)

# First Cancer
PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers$earliest  <- as.Date(PONS_Dossiers$earliest )
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, earliest)
PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis!="Benign Diagnosis"&diagnosis!="Cancer History"&diagnosis!="Benign History")
PONS_Dossiers <- PONS_Dossiers %>%  group_by(patid) %>% filter(earliest==min(earliest)) %>% slice(1)
PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% select(-c(weight, diagnosis, condition)) 
PONS_Dossiers <- PONS_Dossiers %>% mutate(earliest=format(as.Date(earliest), "%Y-%m"))
PONS_Dossiers <- PONS_Dossiers %>% left_join(Months_lookup, by=c("earliest"="Month")) %>% select(patid, Exact_Month)
PONS_Dossiers$Earliest <- "Earliest"

PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Dossiers)

PONS_Demographics <- PONS_Demographics %>% mutate(Death=ifelse(is.na(Death),"0",Death))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Death=ifelse(cumsum(Death =="Death")>=1,"Death",Death))

PONS_Demographics <- PONS_Demographics %>% mutate(Earliest=ifelse(is.na(Earliest),"0",Earliest))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Earliest=ifelse(cumsum(Earliest =="Earliest")>=1,"Earliest",Earliest))

PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% mutate(Naive=ifelse(cumsum(Earliest=="Earliest")>=1,"0","Naive"))


PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% mutate(Earliest=ifelse(cumsum(Death=="Death")>=1,"0",Earliest))

# Metastasis
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, cancer_metastasis)
PONS_Demographics_temp$cancer_metastasis <- as.Date(PONS_Demographics_temp$cancer_metastasis)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=format(as.Date(cancer_metastasis), "%Y-%m"))
PONS_Demographics_temp <- PONS_Demographics_temp %>% left_join(Months_lookup, by=c("cancer_metastasis"="Month")) %>% select(patid, Exact_Month)
PONS_Demographics_temp$Metastasis <- "Metastasis"
PONS_Demographics_temp <- PONS_Demographics_temp %>% drop_na()

PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Demographics_temp)

PONS_Demographics <- PONS_Demographics %>% mutate(Metastasis=ifelse(is.na(Metastasis),"0",Metastasis))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Metastasis=ifelse(cumsum(Metastasis =="Metastasis")>=1,"Metastasis",Metastasis))

PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Metastasis=ifelse(cumsum(Death =="Death")>=1,"0",Metastasis))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Earliest=ifelse(cumsum(Metastasis =="Metastasis")>=1,"0",Earliest))


# Latest (for remission)


PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers$latest  <- as.Date(PONS_Dossiers$latest )
PONS_Dossiers <- PONS_Dossiers %>% select(patid, latest, diagnosis)
PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis!="Benign Diagnosis"&diagnosis!="Cancer History"&diagnosis!="Benign History")
PONS_Dossiers <- PONS_Dossiers %>% group_by(patid) %>% filter(latest==max(latest)) %>% slice(1)
PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% select(-c(diagnosis)) 
PONS_Dossiers$latest <- as.Date(PONS_Dossiers$latest)
PONS_Dossiers <- PONS_Dossiers %>% mutate(latest=format(as.Date(latest), "%Y-%m"))
PONS_Dossiers <- PONS_Dossiers %>% left_join(Months_lookup, by=c("latest"="Month")) %>% select(patid, Exact_Month)
PONS_Dossiers$Latest <- "Latest"
PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Dossiers)
PONS_Demographics <- PONS_Demographics %>% mutate(Latest=ifelse(is.na(Latest),"0",Latest))


PONS_Demographics$Latest2 <- PONS_Demographics$Latest
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Latest2=ifelse(cumsum(Latest =="Latest")<1,"Latest",Latest))

PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Remission=ifelse(cumsum(Latest2 =="0")>12&cumsum(Death =="Death")<1,"Remission","0"))

fwrite(PONS_Demographics, "PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")


data.frame(PONS_Demographics %>% ungroup() %>% filter(Naive=="Naive") %>% group_by(Exact_Month) %>% summarise(Naive=sum(weight))) %>%
  full_join(data.frame(PONS_Demographics %>% ungroup() %>% filter(Death=="Death") %>% group_by(Exact_Month) %>% summarise(Death=sum(weight)))) %>%
  full_join(data.frame(PONS_Demographics %>% ungroup() %>% filter(Earliest=="Earliest") %>% group_by(Exact_Month) %>% summarise(Earliest=sum(weight)))) %>%
  full_join(data.frame(PONS_Demographics %>% ungroup() %>% filter(Metastasis=="Metastasis") %>% group_by(Exact_Month) %>% summarise(Metastasis=sum(weight)))) 


PONS_Demographics <- PONS_Demographics %>% mutate(Status= ifelse(Death=="Death", "Death",
                                                                 ifelse(Remission=="Remission","Remission", 
                                                                        ifelse(Metastasis=="Metastasis","Metastasis", 
                                                                               ifelse(Earliest=="Earliest","Earliest", 
                                                                                      ifelse(Naive=="Naive","Naive","none"))))))


fwrite(PONS_Demographics, "PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")

PONS_Demographics %>% filter(Exact_Month==60) %>% filter(Status=="Earliest"|Status=="Metastasis") %>% ungroup() %>% summarise(n=sum(weight)) # 11959409
PONS_Demographics %>% filter(Exact_Month==60) %>% filter(Status=="Remission") %>% ungroup() %>% summarise(n=sum(weight)) # 5843261

PONS_Demographics %>% filter(Exact_Month==48) %>% filter(Status=="Earliest"|Status=="Metastasis") %>% ungroup() %>% summarise(n=sum(weight)) # 12104175
PONS_Demographics %>% filter(Exact_Month==48) %>% filter(Status=="Remission") %>% ungroup() %>% summarise(n=sum(weight)) # 5680475


unique(PONS_Demographics$Status)

PONS_Demographics %>% ungroup() %>% filter(Exact_Month>=49) %>% filter(Status=="Remission") %>% select(patid, weight) %>% distinct() %>%
  anti_join(PONS_Demographics %>% ungroup() %>% filter(Exact_Month<49) %>% filter(Status=="Remission") %>% select(patid, weight) %>% distinct()) %>%
  ungroup %>% summarise(n=sum(weight)) # 2273822


# per Primary Cancer

PONS_Time_Series_Groups <- fread("PONS_Time_Series_Groups.txt", sep="\t")
PONS_Time_Series_Groups <- PONS_Time_Series_Groups %>% filter(Exact_Month==60)
PONS_Time_Series_Groups <- PONS_Time_Series_Groups %>% select(patid, weight, Status)

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)

PONS_Time_Series_Groups <- PONS_Time_Series_Groups %>% left_join(PONS_Demographics) %>% drop_na()

PONS_Time_Series_Groups <- PONS_Time_Series_Groups %>% group_by(diagnosis, Status) %>% summarise(n=sum(weight))

fwrite(PONS_Time_Series_Groups, "PONS_Time_Series_Summary_Groups_per_Primary.txt", sep="\t")

sum(PONS_Time_Series_Groups$n)


# --------
# Cancer timeline history long table  V2 NEW BOXES -----------

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

# Mortality
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, death_date, diagnosis)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

#PONS_Demographics <- PONS_Demographics %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")
PONS_Demographics <- PONS_Demographics %>% filter(diagnosis!="-")

PONS_Demographics <- PONS_Demographics %>% select(patid, weight, death_date)
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

PONS_Demographics %>% drop_na()
PONS_Demographics$Death_Date <- PONS_Demographics$death_date
Death_Date <- PONS_Demographics

PONS_Demographics <- PONS_Demographics %>% select(patid, weight) %>% mutate(Repeat=60) 

PONS_Demographics <- expandRows(PONS_Demographics, "Repeat")


PONS_Demographics$Exact_Month <- ave(PONS_Demographics$patid, PONS_Demographics$patid,FUN = seq_along)
PONS_Demographics$Exact_Month <- as.numeric(PONS_Demographics$Exact_Month)

Death_Date <- Death_Date %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)
Death_Date <- Death_Date %>% drop_na()
Death_Date$Death <- "Death"

PONS_Demographics <- PONS_Demographics %>% left_join(Death_Date)

# First Cancer
PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers$earliest  <- as.Date(PONS_Dossiers$earliest )
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, earliest)
PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis!="Benign Diagnosis"&diagnosis!="Cancer History"&diagnosis!="Benign History")
PONS_Dossiers <- PONS_Dossiers %>%  group_by(patid) %>% filter(earliest==min(earliest)) %>% slice(1)
PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% select(-c(weight, diagnosis, condition)) 
PONS_Dossiers <- PONS_Dossiers %>% mutate(earliest=format(as.Date(earliest), "%Y-%m"))
PONS_Dossiers <- PONS_Dossiers %>% left_join(Months_lookup, by=c("earliest"="Month")) %>% select(patid, Exact_Month)
PONS_Dossiers$Earliest <- "Earliest"

PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Dossiers)

PONS_Demographics <- PONS_Demographics %>% mutate(Death=ifelse(is.na(Death),"0",Death))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Death=ifelse(cumsum(Death =="Death")>=1,"Death",Death))

PONS_Demographics <- PONS_Demographics %>% mutate(Earliest=ifelse(is.na(Earliest),"0",Earliest))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Earliest=ifelse(cumsum(Earliest =="Earliest")>=1,"Earliest",Earliest))

PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% mutate(Naive=ifelse(cumsum(Earliest=="Earliest")>=1,"0","Naive"))


PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% mutate(Earliest=ifelse(cumsum(Death=="Death")>=1,"0",Earliest))

# Metastasis
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, cancer_metastasis)
PONS_Demographics_temp$cancer_metastasis <- as.Date(PONS_Demographics_temp$cancer_metastasis)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=format(as.Date(cancer_metastasis), "%Y-%m"))
PONS_Demographics_temp <- PONS_Demographics_temp %>% left_join(Months_lookup, by=c("cancer_metastasis"="Month")) %>% select(patid, Exact_Month)
PONS_Demographics_temp$Metastasis <- "Metastasis"
PONS_Demographics_temp <- PONS_Demographics_temp %>% drop_na()

PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Demographics_temp)

PONS_Demographics <- PONS_Demographics %>% mutate(Metastasis=ifelse(is.na(Metastasis),"0",Metastasis))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Metastasis=ifelse(cumsum(Metastasis =="Metastasis")>=1,"Metastasis",Metastasis))

PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Metastasis=ifelse(cumsum(Death =="Death")>=1,"0",Metastasis))
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Earliest=ifelse(cumsum(Metastasis =="Metastasis")>=1,"0",Earliest))


# Latest (for remission)


PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers$latest  <- as.Date(PONS_Dossiers$latest )
PONS_Dossiers <- PONS_Dossiers %>% select(patid, latest, diagnosis)
PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis!="Benign Diagnosis"&diagnosis!="Cancer History"&diagnosis!="Benign History")
PONS_Dossiers <- PONS_Dossiers %>% group_by(patid) %>% filter(latest==max(latest)) %>% slice(1)
PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% select(-c(diagnosis)) 
PONS_Dossiers$latest <- as.Date(PONS_Dossiers$latest)
PONS_Dossiers <- PONS_Dossiers %>% mutate(latest=format(as.Date(latest), "%Y-%m"))
PONS_Dossiers <- PONS_Dossiers %>% left_join(Months_lookup, by=c("latest"="Month")) %>% select(patid, Exact_Month)
PONS_Dossiers$Latest <- "Latest"
PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Dossiers)
PONS_Demographics <- PONS_Demographics %>% mutate(Latest=ifelse(is.na(Latest),"0",Latest))


PONS_Demographics$Latest2 <- PONS_Demographics$Latest
PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Latest2=ifelse(cumsum(Latest =="Latest")<1,"Latest",Latest))

PONS_Demographics <- PONS_Demographics  %>% group_by(patid) %>% mutate(Remission=ifelse(cumsum(Latest2 =="0")>12&cumsum(Death =="Death")<1,"Remission","0"))

fwrite(PONS_Demographics, "PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")




PONS_Demographics <- PONS_Demographics %>% mutate(Status= ifelse(Death=="Death", "Death",
                                                                 ifelse(Remission=="Remission","Remission", 
                                                                        ifelse(Metastasis=="Metastasis","Metastasis", 
                                                                               ifelse(Earliest=="Earliest","Earliest", 
                                                                                      ifelse(Naive=="Naive","Naive","none"))))))


fwrite(PONS_Demographics, "PONS_Time_Series_Groups.txt", sep="\t")


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box %>% summarise(n=sum(weight)) # 31772455
New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-") 
New_Primary_Cancer_Box  %>% summarise(n=sum(weight)) # 22984889

PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% select(patid)) %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))
# 22984859

data.frame(PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% select(patid)) %>% ungroup() %>% filter(Naive=="Naive") %>% group_by(Exact_Month) %>% summarise(Naive=sum(weight))) %>%
  full_join(data.frame(PONS_Demographics %>% ungroup() %>% filter(Death=="Death") %>% group_by(Exact_Month) %>% summarise(Death=sum(weight)))) %>%
  full_join(data.frame(PONS_Demographics %>% ungroup() %>% filter(Earliest=="Earliest") %>% group_by(Exact_Month) %>% summarise(Earliest=sum(weight)))) %>%
  full_join(data.frame(PONS_Demographics %>% ungroup() %>% filter(Metastasis=="Metastasis") %>% group_by(Exact_Month) %>% summarise(Metastasis=sum(weight)))) 


PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% filter(Primary_Cancer != "Unspecified Cancer") %>% select(patid)) %>%  filter(Exact_Month==60) %>% filter(Status=="Earliest"|Status=="Metastasis") %>% ungroup() %>% summarise(n=sum(weight)) # 12678364
PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% filter(Primary_Cancer != "Unspecified Cancer") %>% select(patid)) %>% filter(Exact_Month==60) %>% filter(Status=="Remission") %>% ungroup() %>% summarise(n=sum(weight)) # 6409273

PONS_Demographics %>% inner_join(New_Primary_Cancer_Box   %>% filter(Primary_Cancer != "Unspecified Cancer") %>% select(patid)) %>%  filter(Exact_Month==48) %>% filter(Status=="Earliest"|Status=="Metastasis") %>% ungroup() %>% summarise(n=sum(weight)) # 14121158
PONS_Demographics %>% inner_join(New_Primary_Cancer_Box  %>% filter(Primary_Cancer != "Unspecified Cancer") %>% select(patid))  %>% filter(Exact_Month==48) %>% filter(Status=="Remission") %>% ungroup() %>% summarise(n=sum(weight)) # 4081341


unique(PONS_Demographics$Status)

PONS_Demographics %>% ungroup() %>% inner_join(New_Primary_Cancer_Box %>% select(patid)) %>%   filter(Exact_Month>=49) %>% filter(Status=="Remission") %>% select(patid, weight) %>% distinct() %>%
  anti_join(PONS_Demographics %>% ungroup() %>%  inner_join(New_Primary_Cancer_Box %>% select(patid)) %>%  filter(Exact_Month<49) %>% filter(Status=="Remission") %>% select(patid, weight) %>% distinct()) %>%
  ungroup %>% summarise(n=sum(weight)) # 2273822




PONS_Demographics %>% inner_join(New_Primary_Cancer_Box) %>% filter(Primary_Cancer != "Unspecified Cancer") %>%
  filter(Exact_Month==60) %>% group_by(Status)  %>% summarise(n=sum(weight))


# --------------

# Above 65 years old -------------


PONS_Demographics_AGE <- fread("PONS Demographics.txt")
PONS_Demographics_AGE <- PONS_Demographics_AGE %>% select(patid, weight, age)
PONS_Demographics_AGE <- PONS_Demographics_AGE %>% mutate(age=ifelse(age>65,">65","<65"))

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics_AGE <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_AGE)


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis")) %>% select(patid)


data.frame(PONS_Demographics_AGE %>% inner_join(PONS_Demographics) %>% filter(died=="N") %>% group_by(Primary_Cancer,age) %>%
             summarise(n=sum(weight)) %>% spread(key=age, value=n))


# Metastatic only 
PONS_Demographics_AGE <- fread("PONS Demographics.txt")
PONS_Demographics_AGE <- PONS_Demographics_AGE %>% select(patid, weight, age, cancer_metastasis)
PONS_Demographics_AGE <- PONS_Demographics_AGE %>% mutate(age=ifelse(age>65,">65","<65"))
PONS_Demographics_AGE <- PONS_Demographics_AGE %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
PONS_Demographics_AGE <- PONS_Demographics_AGE %>% filter(cancer_metastasis==1)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics_AGE <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_AGE)


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis")) %>% select(patid)


data.frame(PONS_Demographics_AGE %>% inner_join(PONS_Demographics) %>% filter(died=="N") %>% group_by(Primary_Cancer,age) %>%
             summarise(n=sum(weight)) %>% spread(key=age, value=n))


# --------
# Survival Curves -----------

# Metastatic
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, diagnosis, cancer_onset, cancer_metastasis, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(age>=18)

PONS_Demographics_temp <- separate_rows(PONS_Demographics_temp, diagnosis, sep = ",", convert=T )
PONS_Demographics_temp <- PONS_Demographics_temp %>% group_by(patid) %>% slice(1)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")

PONS_Demographics_temp %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #21494720

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date   )

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

data.frame(PONS_Demographics_temp %>% filter(cancer_metastasis==1) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))

PONS_Demographics_temp %>% filter(cancer_metastasis==1) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 49.4
PONS_Demographics_temp %>% filter(cancer_metastasis==1) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 59.5



data.frame(PONS_Demographics_temp %>% filter(cancer_metastasis==0) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))


PONS_Demographics_temp %>% filter(cancer_metastasis==0) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 56.2
PONS_Demographics_temp %>% filter(cancer_metastasis==0) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 59.5



# Cachexia
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, diagnosis, cancer_onset, cachexia_onset, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(age>=18)

PONS_Demographics_temp <- separate_rows(PONS_Demographics_temp, diagnosis, sep = ",", convert=T )
PONS_Demographics_temp <- PONS_Demographics_temp %>% group_by(patid) %>% slice(1)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")

PONS_Demographics_temp %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #21494720

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date   )

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

data.frame(PONS_Demographics_temp %>% filter(cachexia_onset==1) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))

PONS_Demographics_temp %>% filter(cachexia_onset==1) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 33.9
PONS_Demographics_temp %>% filter(cachexia_onset==1) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 31.5



data.frame(PONS_Demographics_temp %>% filter(cachexia_onset==0) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))


PONS_Demographics_temp %>% filter(cachexia_onset==0) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 53.9
PONS_Demographics_temp %>% filter(cachexia_onset==0) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 59.5




# Cachexia +  Metastatic
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, diagnosis, cancer_onset, cachexia_onset, cancer_metastasis, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(age>=18)

PONS_Demographics_temp <- separate_rows(PONS_Demographics_temp, diagnosis, sep = ",", convert=T )
PONS_Demographics_temp <- PONS_Demographics_temp %>% group_by(patid) %>% slice(1)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")

PONS_Demographics_temp %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #21494720

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date)

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

data.frame(PONS_Demographics_temp %>% filter(cachexia_onset==1) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))

PONS_Demographics_temp %>% filter(cachexia_onset==1) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 33.4
PONS_Demographics_temp %>% filter(cachexia_onset==1) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 30.5



data.frame(PONS_Demographics_temp %>% filter(cachexia_onset==0) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))


PONS_Demographics_temp %>% filter(cachexia_onset==0) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 53.7
PONS_Demographics_temp %>% filter(cachexia_onset==0) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 59.5



temp <- data.frame(PONS_Demographics_temp %>% 
             group_by(cachexia_onset, cancer_metastasis, Survived) %>% summarise(n=sum(weight))) %>%
  filter(Survived>=0)

fwrite(temp, "Survival_Curves_CachexiaMets.csv")

data.frame(PONS_Demographics_temp %>% 
             group_by(cachexia_onset, cancer_metastasis) %>% summarise(weighted.mean(Survived, weight))) 
             
PONS_Demographics_temp %>% 
             group_by(cachexia_onset, cancer_metastasis)  %>% summarise(weighted.mean(age, weight))
             

PONS_Demographics_temp %>% 
             group_by(cachexia_onset, cancer_metastasis) %>% summarise(n=sum(weight))
# Per Primary Cancer Type
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, diagnosis, cancer_onset, cancer_metastasis, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(age>=18)

PONS_Demographics_temp <- separate_rows(PONS_Demographics_temp, diagnosis, sep = ",", convert=T )
PONS_Demographics_temp <- PONS_Demographics_temp %>% group_by(patid) %>% slice(1)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")

PONS_Demographics_temp %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #21494720

# PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date   )

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

temp <- data.frame(PONS_Demographics_temp %>% 
                     group_by(diagnosis, Survived) %>% summarise(n=sum(weight))) %>%
  filter(Survived>=0) %>%
  spread(key=diagnosis, value=n)

fwrite(temp, "Survival_Curves_PrimaryCancers.csv")

PONS_Demographics_temp %>%  ungroup() %>% group_by(diagnosis) %>% summarise(weighted.mean(Survived, weight)) %>% arrange(-`weighted.mean(Survived, weight)`) # 49.1

PONS_Demographics_temp %>%  ungroup() %>% group_by(diagnosis) %>% summarise(weighted.median(Survived, weight)) # 59.5


# Cachexia Pred


Cachexia_pats <- fread("Cachexia_pats.txt", sep="\t")


PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, diagnosis, cancer_onset, cachexia_onset, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(age>=18)

PONS_Demographics_temp <- separate_rows(PONS_Demographics_temp, diagnosis, sep = ",", convert=T )
PONS_Demographics_temp <- PONS_Demographics_temp %>% group_by(patid) %>% slice(1)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")

PONS_Demographics_temp %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #21565679

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date   )

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

data.frame(PONS_Demographics_temp %>% filter(cachexia_onset==1) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))

PONS_Demographics_temp %>% filter(cachexia_onset==1) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 33.4

PONS_Demographics_temp %>% filter(cachexia_onset==1) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 30.5

PONS_Demographics_temp  %>%  inner_join(Cachexia_pats %>% select(patid)) %>%  select(patid)) ungroup() %>% summarise(weighted.mean(Survived, weight)) # 30.5


data.frame(PONS_Demographics_temp %>% inner_join(Cachexia_pats %>% select(patid)) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))




data.frame(PONS_Demographics_temp %>% filter(cachexia_onset==0) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))


PONS_Demographics_temp %>% filter(cachexia_onset==0) %>% ungroup() %>% summarise(weighted.mean(Survived, weight)) # 53.7
PONS_Demographics_temp %>% filter(cachexia_onset==0) %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 59.5


# --------
# Survival Curves: Breast , Prostate , Lung, Intestinal, Pancreatic   Cancer ---------------------------------------------------------------

PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_onset, cancer_metastasis, cachexia_onset, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(age>=18)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer" | 
                                                              Primary_Cancer=="Prostate Cancer" | 
                                                              Primary_Cancer=="Lung Cancer" | 
                                                              Primary_Cancer=="Intestinal Cancer" | 
                                                              Primary_Cancer=="Pancreatic Cancer")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(PONS_Demographics_temp %>% select(-c(weight))) %>% select(-died)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
New_Primary_Cancer_Box$cancer_onset <- as.Date(New_Primary_Cancer_Box$cancer_onset)
New_Primary_Cancer_Box$death_date    <- as.Date(New_Primary_Cancer_Box$death_date)

missingDeathDay <- ymd("2050-12-31")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

Breast_New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer")
Prostate_New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Prostate Cancer")
Lung_New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Lung Cancer")
Intestinal_New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer")
Pancreatic_New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Pancreatic Cancer")


Breast_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(Survived))


Breast_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(age))
Breast_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=sum(weight))


data.frame(Breast_New_Primary_Cancer_Box %>%  filter(cachexia_onset==1&cancer_metastasis==1) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))



Prostate_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(Survived))


Prostate_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(age))
Prostate_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=sum(weight))


data.frame(Prostate_New_Primary_Cancer_Box %>%  filter(cachexia_onset==0&cancer_metastasis==0) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))



Lung_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(Survived))


Lung_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(age))
Lung_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=sum(weight))


data.frame(Lung_New_Primary_Cancer_Box %>%  filter(cachexia_onset==1&cancer_metastasis==1) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))



Intestinal_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(Survived))


Intestinal_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(age))
Intestinal_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=sum(weight))


data.frame(Intestinal_New_Primary_Cancer_Box %>%  filter(cachexia_onset==0&cancer_metastasis==0) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))


Pancreatic_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(Survived))

Pancreatic_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=mean(age))
Pancreatic_New_Primary_Cancer_Box %>% group_by(cachexia_onset,cancer_metastasis  ) %>% summarise(n=sum(weight))


data.frame(Pancreatic_New_Primary_Cancer_Box %>%  filter(cachexia_onset==1&cancer_metastasis==1) %>%
             group_by(Survived) %>% summarise(n=sum(weight)))

# ------------------------------------------------------------------------------------------------------
# Events by cachexia / Cachexia development by events -----------
PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")

Cachexia_pats <- fread("Cachexia_pats.txt")

PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Death") %>%
  select(patid,weight) %>% left_join(Cachexia_pats %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))



PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Remission") %>%
  select(patid,weight) %>% left_join(Cachexia_pats  %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))


PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Earliest") %>%
  select(patid,weight) %>% left_join(Cachexia_pats  %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))



PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Metastasis") %>%
  select(patid,weight) %>% left_join(Cachexia_pats  %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))


Cachexia_pats  %>% select(-weight) %>% left_join(
  PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% 
    select(patid,weight, Status) %>% mutate(Metastasis="Metastasis")) %>%
  mutate(Metastasis=ifelse(is.na(Metastasis),"0",Metastasis)) %>%
  group_by(Metastasis) %>% summarise(n=sum(weight))



PONS_Demographics  %>% ungroup() %>% filter(Exact_Month==60) %>% 
  select(patid,weight,Status) %>% left_join(Cachexia_pats %>% select(-weight)) %>%
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia, Status) %>% summarise(n=sum(weight))



# ----------
# Events by cachexia / Cachexia development by events v2 NEW BOXES ----------------
# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
  arrange(diagnosis, -cancer_metastasis)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis,cachexia_onset)

Pats_to_track_BMI %>% ungroup() %>% filter(diagnosis!="-"&diagnosis!="Skin Cancer"&diagnosis!="Unspecified Cancer") %>% filter(cachexia_onset==1) %>% summarise(n=sum(weight)) # 529051.3


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690714 (25960778 records, 857091 samples)
length(unique(PONS_Measures$patid))

data.frame(PONS_Measures %>% select(patid, weight) %>% distinct() %>% inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 

PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690470 (25931366 records, 852569 samples) (-244 pats, haven't lost patients, simply 29412 records)

data.frame(
  PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=2) %>% 
    select(patid) %>%
    left_join(PONS_Measures) %>% select(patid, weight) %>% distinct() %>% 
    inner_join(Pats_to_track_BMI) %>%
    group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
    arrange(diagnosis, -cancer_metastasis))


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


Pats_10Plus_BMIs <- PONS_Measures %>% select(patid) %>% distinct()


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight)) # 22984889 (22602908 without unspecified)

PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% select(patid))

PONS_Demographics <- PONS_Demographics %>% inner_join(Pats_10Plus_BMIs)

Cachexia_pats <- fread("Cachexia_pats.txt")

PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Death") %>%
  select(patid,weight) %>% left_join(Cachexia_pats %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))


PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Remission") %>%
  select(patid,weight) %>% left_join(Cachexia_pats  %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))


PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Earliest") %>%
  select(patid,weight) %>% left_join(Cachexia_pats  %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))



PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% filter(Status=="Metastasis") %>%
  select(patid,weight) %>% left_join(Cachexia_pats  %>% select(-weight)) %>% 
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia) %>% summarise(n=sum(weight))


Cachexia_pats <- Cachexia_pats %>% select(-weight) %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))

Cachexia_pats   %>% left_join(
  PONS_Demographics %>% ungroup() %>% filter(Exact_Month==60) %>% 
    select(patid, Status) %>% mutate(Metastasis="Metastasis")) %>%
  mutate(Metastasis=ifelse(is.na(Metastasis),"0",Metastasis)) %>%
  group_by(Metastasis) %>% summarise(n=sum(weight))



PONS_Demographics  %>% ungroup() %>% filter(Exact_Month==60) %>% 
  select(patid,weight,Status) %>% left_join(Cachexia_pats %>% select(-weight)) %>%
  mutate(Cachexia=ifelse(is.na(Cachexia),"0",Cachexia)) %>% 
  group_by(Cachexia, Status) %>% summarise(n=sum(weight))


# -------------
# BMI evolution over time before/after Dx ----------

Cachexia_pats <- fread("Cachexia_pats.txt", sep="\t")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

Cachexia_pats <- Cachexia_pats %>% left_join(PONS_Demographics) %>% select(-Cachexia)
Cachexia_pats <- Cachexia_pats %>% drop_na()
Cachexia_pats$cancer_onset <- as.Date(Cachexia_pats$cancer_onset)

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

Cachexia_pats <- Cachexia_pats %>% left_join(PONS_Measures) %>% drop_na()

Cachexia_pats <- Cachexia_pats %>% mutate(ElapsedTime=as.numeric(claimed-cancer_onset)/30.5)



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Cachexia_pats <- Cachexia_pats %>% left_join(PONS_Demographics)

temp <- Cachexia_pats %>% filter(diagnosis=="Pancreatic Cancer") %>% select(ElapsedTime, value)

# 
# temp %>% ggplot(aes(ElapsedTime, value)) +
#   geom_point(colour="midnightblue", size=.1) +
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   xlab("\n Elpased Time (months) from BMI record to 1st Cancer Dx")+
#   ylab("Locally weighted moving polynomial regression fit \n")

temp %>% ggplot(aes(ElapsedTime, value)) +
  geom_smooth(colour="midnightblue", fill="midnightblue") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Locally weighted moving polynomial regression fit \n")





# -----------

# BMI over time, declines individual pats, cohorts --------------


# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

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



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


# Patients that fit our criteria

PONS_Measures_selected_cachexia <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%  
  distinct() %>% inner_join(PONS_Measures)


PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% inner_join(Pats_to_track_BMI)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% left_join(PONS_Demographics)   %>% drop_na()
PONS_Measures_selected_cachexia$cancer_onset <- as.Date(PONS_Measures_selected_cachexia$cancer_onset)
PONS_Measures_selected_cachexia$claimed <- as.Date(PONS_Measures_selected_cachexia$claimed)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(ElapsedTime=as.numeric(claimed-cancer_onset)/30.5)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(PercentMax=100*value/max)

list <- unique(PONS_Measures_selected_cachexia$diagnosis)


PONS_Measures_selected_cachexia %>% filter(patid=="PT591778008") %>%  select(ElapsedTime, value) %>%
  ggplot(aes(ElapsedTime, value)) +
  geom_smooth(colour="midnightblue", fill="midnightblue", se=F) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  #ylim(18,25)+
  #xlim(-48,48)+
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Exact BMI (kg/m2) \n")+
  labs(title="PT591778008")+
  theme_economist() +
  geom_vline(xintercept=0, size=1.5, color="red")



PONS_Measures_selected_cachexia %>% filter(diagnosis=="Leukemia Cancer")  %>% select(ElapsedTime, PercentMax) %>%
  group_by(ElapsedTime) %>% summarise(PercentMax=mean(PercentMax)) %>%
  ggplot(aes(ElapsedTime, PercentMax)) +
  geom_smooth(colour="deepskyblue4", fill="deepskyblue4") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(-48,48)+
  theme_fivethirtyeight() +
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Percentage of MAX \n")+
  labs(title="Leukemia")+
  geom_vline(xintercept=0, size=1.5, color="black")





temp <- PONS_Measures_selected_cachexia %>% select(patid, value, diagnosis, ElapsedTime, max) %>%
  mutate(TimeGroup=ifelse(ElapsedTime<0,"Before","After")) %>%
  group_by(patid, TimeGroup) %>% mutate(MIN=min(value)) %>%
  mutate(MAX_Diff = 100*(MIN-max)/max) %>%
  select(patid, diagnosis, TimeGroup, MAX_Diff) %>%
  distinct()

data.frame(temp %>% ungroup() %>% group_by(diagnosis, TimeGroup) %>% 
             summarise(n=mean(MAX_Diff))) %>%
  spread(key=TimeGroup, value=n)

# ----------

# BMI over time until death --------------

# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

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



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


# Patients that fit our criteria

PONS_Measures_selected_cachexia <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%  
  distinct() %>% inner_join(PONS_Measures)


PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% inner_join(Pats_to_track_BMI)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% left_join(PONS_Demographics)   %>% drop_na()
PONS_Measures_selected_cachexia$death_date <- as.Date(PONS_Measures_selected_cachexia$death_date)
PONS_Measures_selected_cachexia$claimed <- as.Date(PONS_Measures_selected_cachexia$claimed)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(ElapsedTime=as.numeric(claimed-death_date)/30.5)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(PercentMax=100*value/max)

PONS_Measures_selected_cachexia %>% filter(patid=="PT207046606") %>%  select(ElapsedTime, value) %>%
  ggplot(aes(ElapsedTime, value)) +
  geom_smooth(colour="firebrick", fill="firebrick", se=F) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  #ylim(18,25)+
  #xlim(-48,48)+
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Exact BMI (kg/m2) \n")+
  labs(title="PT207046606")+
  theme_fivethirtyeight() +
  geom_vline(xintercept=0, size=1.5, color="black")

unique(PONS_Measures_selected_cachexia$diagnosis)


PONS_Measures_selected_cachexia %>% filter(ElapsedTime<=0)  %>% filter(diagnosis=="Breast Cancer")  %>% select(ElapsedTime, PercentMax) %>%
  group_by(ElapsedTime) %>% summarise(PercentMax=mean(PercentMax)) %>%
  ggplot(aes(ElapsedTime, PercentMax)) +
  geom_smooth(colour="deepskyblue4", fill="deepskyblue4") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(-48,0)+
  theme_wsj() +
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Percentage of MAX \n")+
  labs(title="Breast")+
  geom_vline(xintercept=0, size=1.5, color="black")



temp <- PONS_Measures_selected_cachexia %>% select(patid, value, diagnosis, ElapsedTime, max) %>%
  mutate(TimeGroup=ifelse(ElapsedTime<0,"Before","After")) %>%
  group_by(patid, TimeGroup) %>% mutate(MIN=min(value)) %>%
  mutate(MAX_Diff = 100*(MIN-max)/max) %>%
  select(patid, diagnosis, TimeGroup, MAX_Diff) %>%
  distinct()

data.frame(temp %>% ungroup() %>% group_by(diagnosis, TimeGroup) %>% 
             summarise(n=mean(MAX_Diff))) %>%
  spread(key=TimeGroup, value=n)



# ---------------
# Proportion of cachexia pats meeting the criteria before cancer onset ? --------





PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

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



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


# Patients that fit our criteria

PONS_Measures_selected_cachexia <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%  
  distinct() %>% inner_join(PONS_Measures)


PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% inner_join(Pats_to_track_BMI)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% left_join(PONS_Demographics)   %>% drop_na()
PONS_Measures_selected_cachexia$cancer_onset <- as.Date(PONS_Measures_selected_cachexia$cancer_onset)
PONS_Measures_selected_cachexia$claimed <- as.Date(PONS_Measures_selected_cachexia$claimed)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(ElapsedTime=as.numeric(claimed-cancer_onset)/30.5)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(PercentMax=100*value/max)

data.frame(
  PONS_Measures_selected_cachexia %>% select(patid, weight, diagnosis) %>% distinct() %>% 
    group_by(diagnosis) %>% summarise(n=sum(weight)) %>%
  left_join(
    PONS_Measures_selected_cachexia %>% 
      filter(claimed>=cancer_onset & ( 
        (min<max*0.98 & min<20 & mindate>maxdate) | 
          (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)|
          (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183))) %>%
              select(patid, weight, diagnosis) %>% distinct() %>% 
      group_by(diagnosis) %>% summarise(n2=sum(weight))) %>%
  mutate(proportion=n2/n)
  )


# ------------




# New Measures file ------------------
PONS_Measures <- fread("PONS Measures.txt")

PONS_Measures <- PONS_Measures %>% select(-c(unit, source, description, metric, vague_value, vague_date))

fwrite(PONS_Measures, "PONS_Measures_short.txt", sep="\t")

PONS_Measures <- fread("PONS Measures.txt")
PONS_Measures <- PONS_Measures %>% filter(test=="Cancer Stage")

fwrite(PONS_Measures, "PONS_Measures_Stages.txt", sep="\t")
# ----------------

# Stages ---------------

PONS_Measures_Stages <- fread("PONS_Measures_Stages.txt")

unique(PONS_Measures_Stages$description)

unique(PONS_Measures_Stages$source)

PONS_Measures_Stages <- PONS_Measures_Stages %>% filter(description=="STAGE"|description=="Oncology Stage")

PONS_Measures_Stages %>% select(patid) %>% distinct() #100522


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)

Pats_to_track_BMI %>% select(patid) %>% distinct()  #1427427

data.frame(Pats_to_track_BMI %>%  select(patid, weight, diagnosis) %>% group_by(diagnosis) %>% summarise(Total=sum(weight)))  %>%
  left_join(PONS_Measures_Stages %>% select(patid) %>% distinct() %>% 
              inner_join(Pats_to_track_BMI) %>% select(patid, weight, diagnosis) %>% group_by(diagnosis) %>% summarise(WithStage=sum(weight))) %>%
  mutate(proportion=WithStage/Total)

temp <- 
  data.frame(PONS_Measures_Stages %>% select(patid,value) %>% group_by(patid) %>% 
               filter(value==max(value)) %>% slice(1) %>%
               mutate(value = str_sub(value, 1L, 1L)) %>%
               mutate(value=as.numeric(value)) %>%
               inner_join(Pats_to_track_BMI))
             

data.frame(temp %>% group_by(diagnosis, value) %>% summarise(Total=sum(weight))) %>%
  spread(key=value, value=Total)
   


# data.frame(temp %>% group_by(value) %>% summarise(Total=sum(weight))) %>%
#   spread(key=value, value=Total)
# 
# 
# data.frame(temp %>% filter(cancer_metastasis==1) %>% group_by(value) %>% summarise(Total=sum(weight))) %>%
#   spread(key=value, value=Total)
# 
# data.frame(temp %>% filter(cancer_metastasis==0) %>% group_by(value) %>% summarise(Total=sum(weight))) %>%
#   spread(key=value, value=Total)
# 
# 
# data.frame(temp %>% filter(cancer_metastasis==1) %>% group_by(value) %>% summarise(Total=sum(weight))) %>%
#   spread(key=value, value=Total)

data.frame(temp %>% mutate(value=ifelse(cancer_metastasis==1,4,value)) %>% group_by(diagnosis, value) %>% summarise(Total=sum(weight))) %>%
  spread(key=value, value=Total)

data.frame(PONS_Measures_Stages %>% select(patid,value) %>% group_by(patid) %>% 
             filter(value==max(value)) %>% slice(1) %>%
             mutate(value = str_sub(value, 1L, 1L)) %>%
             mutate(value=as.numeric(value)) %>%
             inner_join(Pats_to_track_BMI) %>%
             mutate(value=ifelse(cancer_metastasis==1,4,value)) %>% 
             group_by(diagnosis) %>%
             summarise(n=weighted.mean(value, weight)))


PONS_Measures_Stages %>% filter(vague_value==0) %>% select(patid) %>% distinct() #44752

data.frame(PONS_Measures_Stages %>% filter(vague_value==0) %>% select(patid,value) %>% group_by(patid) %>% 
  filter(value==max(value)) %>% slice(1) %>%
  mutate(value = str_sub(value, 1L, 1L)) %>%
  mutate(value=as.numeric(value)) %>%
  inner_join(Pats_to_track_BMI) %>%
  group_by(diagnosis) %>%
  summarise(n=weighted.mean(value, weight)))  
  

PONS_Measures_Stages %>% filter(vague_value==1) %>% select(patid) %>% distinct() #71136

data.frame(PONS_Measures_Stages %>% filter(vague_value==1) %>% select(patid,value) %>% group_by(patid) %>% 
             filter(value==max(value)) %>% slice(1) %>%
             mutate(value = str_sub(value, 1L, 1L)) %>%
             mutate(value=as.numeric(value)) %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis) %>%
             summarise(n=weighted.mean(value, weight)))  


# -------


# Inventories / Comorbidities ---------------------

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)
Pats_to_track_BMI %>% select(patid) %>% distinct()

Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer") 

PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[3] <- "ICD"
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% inner_join(Pats_to_track_BMI)

PONS_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 21494720
ICD10_all <- PONS_Comorbidity_Inventories %>% group_by(ICD) %>% summarise(n=sum(weight))


ICD10_mets <- PONS_Comorbidity_Inventories %>% filter(cancer_metastasis==1) %>% group_by(ICD) %>% summarise(n=sum(weight)) # 8564782
ICD10_No_mets <- PONS_Comorbidity_Inventories %>% filter(cancer_metastasis==0) %>% group_by(ICD) %>% summarise(n=sum(weight)) # 12929938
ICD10_mets <- ICD10_mets %>% inner_join(ICD10_No_mets, by=c("ICD")) %>% drop_na() %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("G", ICD)|grepl("H", ICD)|
                                                                                               grepl("I", ICD)|grepl("J", ICD)|grepl("K", ICD)|grepl("L", ICD)|grepl("M", ICD)|
                                                                                               grepl("N", ICD)|grepl("R", ICD))
ICD10_mets <- ICD10_mets %>% mutate(n.x=100*n.x/8564782) %>% mutate(n.y=100*n.y/12929938)

fwrite(ICD10_mets, "Inventories_MetastasisVSnoMetastasis.csv")



ICD10_cachexia <- PONS_Comorbidity_Inventories %>% filter(cachexia_onset==1) %>% group_by(ICD) %>% summarise(n=sum(weight)) # 518580.7
ICD10_No_cachexia <- PONS_Comorbidity_Inventories %>% filter(cachexia_onset==0) %>% group_by(ICD) %>% summarise(n=sum(weight)) # 20976139
ICD10_cachexia <- ICD10_cachexia %>% inner_join(ICD10_No_cachexia, by=c("ICD")) %>% drop_na() %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("G", ICD)|grepl("H", ICD)|
                                                                                                           grepl("I", ICD)|grepl("J", ICD)|grepl("K", ICD)|grepl("L", ICD)|grepl("M", ICD)|
                                                                                                           grepl("N", ICD)|grepl("R", ICD))
ICD10_cachexia <- ICD10_cachexia %>% mutate(n.x=100*n.x/518580.7) %>% mutate(n.y=100*n.y/20976139)

ICD10_cachexia <- ICD10_cachexia %>% mutate(foldchange=n.x/n.y)
fwrite(ICD10_cachexia, "Inventories_CachexiaVSnoCachexia.csv")

# -----------
# Cachexia onset to death ? --------
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

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



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


# Patients that fit our criteria

PONS_Measures_selected_cachexia <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%  
  distinct() %>% inner_join(PONS_Measures)


PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% inner_join(Pats_to_track_BMI)
PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% select(-c(cancer_metastasis, cachexia_onset))

fwrite(PONS_Measures_selected_cachexia, "PONS_Measures_selected_cachexia.txt", sep="\t")
PONS_Measures_selected_cachexia <- fread("PONS_Measures_selected_cachexia.txt")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset, death_date)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% left_join(PONS_Demographics)  
PONS_Measures_selected_cachexia$death_date <- as.Date(PONS_Measures_selected_cachexia$death_date)
PONS_Measures_selected_cachexia$claimed <- as.Date(PONS_Measures_selected_cachexia$claimed)
PONS_Measures_selected_cachexia$cancer_onset <- as.Date(PONS_Measures_selected_cachexia$cancer_onset)
PONS_Measures_selected_cachexia$mindate <- as.Date(PONS_Measures_selected_cachexia$mindate)
PONS_Measures_selected_cachexia$maxdate <- as.Date(PONS_Measures_selected_cachexia$maxdate)


missingDeathDay <- ymd("2050-12-31")
PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))
PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% filter(!is.na(cancer_onset))

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>%
  filter(mindate<=death_date) %>%
  filter(mindate>cancer_onset) %>%
  filter( 
    (min<max*0.98 & min<20 & mindate>maxdate) | 
      (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)|
      (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  group_by(patid) %>% filter(mindate==min(mindate)) %>% slice(1) %>%
  select(patid, weight, mindate, death_date)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% mutate(Survived = as.numeric(death_date)-as.numeric(mindate)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))


PONS_Measures_selected_cachexia %>% ungroup() %>% summarise(n=weighted.mean(Survived, weight)) # 43.5

PONS_Measures_selected_cachexia %>% ungroup() %>% group_by(Survived) %>% summarise(n=sum(weight))


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)
PONS_Demographics <- PONS_Demographics  %>% select(patid, diagnosis)


data.frame(PONS_Measures_selected_cachexia %>% ungroup() %>% left_join(PONS_Demographics) %>%
             filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer") %>%
             summarise(n=weighted.mean(Survived, weight))) # 41.11136

data.frame(PONS_Measures_selected_cachexia %>% ungroup() %>% left_join(PONS_Demographics) %>%
             filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer") %>%
             group_by(diagnosis) %>% summarise(n=weighted.mean(Survived, weight))) # 47.71788


data.frame(PONS_Measures_selected_cachexia %>% group_by(Survived) %>% summarise(n=sum(weight)))




PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, diagnosis, cachexia_onset, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(age>=18)

PONS_Demographics_temp <- separate_rows(PONS_Demographics_temp, diagnosis, sep = ",", convert=T )
PONS_Demographics_temp <- PONS_Demographics_temp %>% group_by(patid) %>% slice(1)
PONS_Demographics_temp <- PONS_Demographics_temp %>% filter(diagnosis!="-"&diagnosis!="Unspecified Cancer"&diagnosis!="Skin Cancer")

PONS_Demographics_temp %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #21565679

PONS_Demographics_temp$cachexia_onset <- as.Date(PONS_Demographics_temp$cachexia_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date   )

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% drop_na() %>%
  mutate(Survived = as.numeric(death_date)-as.numeric(cachexia_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

data.frame(PONS_Demographics_temp %>% group_by(Survived) %>% summarise(n=sum(weight)))

data.frame(PONS_Demographics_temp %>%   ungroup() %>% group_by(diagnosis, Survived) %>% summarise(n=sum(weight)) %>%
             spread(key=diagnosis, value=n))

PONS_Demographics_temp %>%   ungroup() %>% group_by(diagnosis) %>% 
  summarise(weighted.mean(Survived, weight)) # 24.1


names(PONS_Measures_selected_cachexia)[3] <- "cachexia_onset"

PONS_Measures_selected_cachexia %>% left_join(PONS_Demographics) %>% select(patid, weight, diagnosis, cachexia_onset, Survived) %>%
  bind_rows(PONS_Demographics_temp %>% select(patid, weight, diagnosis, cachexia_onset, Survived)
  ) %>% group_by(patid) %>% filter(cachexia_onset==min(cachexia_onset)) %>% slice(1) %>%
  ungroup() %>% summarise(n=weighted.mean(Survived, weight)) # 48

# -------
# BMI difference Min to Max death vs alive  --------------

# Population to track
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, cancer_metastasis,cachexia_onset)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

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



data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
             select(patid, weight) %>% distinct() %>%  
             bind_rows(Pats_to_track_BMI %>% filter(cachexia_onset==1) %>% select(patid)) %>%
             distinct() %>%
             inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


# Patients that fit our criteria

PONS_Measures_selected_cachexia <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate) | (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, weight) %>% distinct() %>%  
  distinct() %>% inner_join(PONS_Measures)


PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% inner_join(Pats_to_track_BMI)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)

PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% left_join(PONS_Demographics)   



PONS_Measures_selected_cachexia <- PONS_Measures_selected_cachexia %>% select(patid,weight,min,max,death_date) %>% distinct() %>%
  mutate(death_date=ifelse(is.na(death_date),0,1))


PONS_Measures_selected_cachexia %>%
  mutate(difference=(min-max)*100/max) %>% group_by(death_date) %>% summarise(n=weighted.mean(difference, weight))


PONS_Measures_selected_cachexia  %>% group_by(death_date) %>% summarise(n=weighted.mean(max, weight))
PONS_Measures_selected_cachexia  %>% group_by(death_date) %>% summarise(n=weighted.mean(min, weight))



PONS_Measures_selected_cachexia %>%
  mutate(difference=(min-max)*100/max) %>% 
  ggplot(aes(difference)) +
  geom_density(size=2, colour="midnightblue", fill="midnightblue", alpha=0.7) +
  facet_wrap(~death_date) + 
  xlim(-50,0) + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Min-to-Max BMI Difference")+
  ylab("Proportion of all records \n")
  #theme_economist(dkpanel = TRUE)+scale_colour_economist()



# -----------
# Re-select Lung Cancer Patients ---------------------

PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, age, gender, code, earliest)
PONS_Dossiers <- PONS_Dossiers %>% filter(age>=18)
PONS_Dossiers <- PONS_Dossiers %>% filter(condition=="Lung Cancer") %>% group_by(patid) %>% filter(earliest==min(earliest))
unique(PONS_Dossiers$diagnosis)
#[1] "Cancer History"    "Cancer Metastasis" "Cancer Diagnosis"  "Cancer Suspicion" 
PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis=="Cancer Metastasis"|diagnosis=="Cancer Diagnosis") %>% select(patid, weight, earliest) %>% distinct()

PONS_Dossiers %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # Ever 2764126

PONS_Dossiers %>% ungroup() %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight)) # 427855
PONS_Dossiers %>% ungroup() %>% filter(earliest>="2019-08-01"&earliest<="2020-07-31") %>% summarise(n=sum(weight)) # 445625
PONS_Dossiers %>% ungroup() %>% filter(earliest>="2018-08-01"&earliest<="2019-07-31") %>% summarise(n=sum(weight)) # 503713
PONS_Dossiers %>% ungroup() %>% filter(earliest>="2017-08-01"&earliest<="2018-07-31") %>% summarise(n=sum(weight)) # 525132
PONS_Dossiers %>% ungroup() %>% filter(earliest>="2016-08-01"&earliest<="2017-07-31") %>% summarise(n=sum(weight)) # 861801


# Excluding Metastasis codes:
# > PONS_Dossiers %>% ungroup() %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight)) # 427855
# # A tibble: 1 × 1
# n
# <dbl>
#   1 308912.
# > PONS_Dossiers %>% ungroup() %>% filter(earliest>="2019-08-01"&earliest<="2020-07-31") %>% summarise(n=sum(weight)) # 445625
# # A tibble: 1 × 1
# n
# <dbl>
#   1 313908.
# > PONS_Dossiers %>% ungroup() %>% filter(earliest>="2018-08-01"&earliest<="2019-07-31") %>% summarise(n=sum(weight)) # 503713
# # A tibble: 1 × 1
# n
# <dbl>
#   1 362371.
# > PONS_Dossiers %>% ungroup() %>% filter(earliest>="2017-08-01"&earliest<="2018-07-31") %>% summarise(n=sum(weight)) # 525132
# # A tibble: 1 × 1
# n
# <dbl>
#   1 374540.
# > PONS_Dossiers %>% ungroup() %>% filter(earliest>="2016-08-01"&earliest<="2017-07-31") %>% summarise(n=sum(weight)) # 861801
# # A tibble: 1 × 1
# n
# <dbl>
#   1 665009.

# to remove those with history before
PONS_Dossiers_to_remove <- fread("PONS Dossiers.txt")
PONS_Dossiers_to_remove <- PONS_Dossiers_to_remove %>% select(patid, weight, diagnosis, condition, age, gender, code, earliest,latest)
PONS_Dossiers_to_remove <- PONS_Dossiers_to_remove %>% filter(age>=18)
PONS_Dossiers_to_remove <- PONS_Dossiers_to_remove %>% filter(condition=="Lung Cancer")
#[1] "Cancer History"    "Cancer Metastasis" "Cancer Diagnosis"  "Cancer Suspicion" 
PONS_Dossiers_to_remove <- PONS_Dossiers_to_remove %>% filter(diagnosis=="Cancer History"|diagnosis=="Cancer Suspicion") %>% select(patid, weight, earliest) %>% distinct()
PONS_Dossiers_to_remove <- PONS_Dossiers_to_remove %>% group_by(patid) %>% filter(earliest==min(earliest)) %>% slice(1)
PONS_Dossiers_to_remove  %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # Ever 1314960
names(PONS_Dossiers_to_remove)[3] <- "Earliest-Hist_Susp"
PONS_Dossiers_to_remove$`Earliest-Hist_Susp`<-as.Date(PONS_Dossiers_to_remove$`Earliest-Hist_Susp`)

PONS_Dossiers <- PONS_Dossiers %>% left_join(PONS_Dossiers_to_remove) 

PONS_Dossiers %>% ungroup() %>% filter((is.na(`Earliest-Hist_Susp`)&earliest>="2020-08-01"&earliest<="2021-07-31")|
                                         (earliest>="2020-08-01"&earliest<="2021-07-31"&earliest<=`Earliest-Hist_Susp`)) %>%
  summarise(n=sum(weight)) # 427855 did not move


# -----------
# How many patients have more than 1 code at first time  ---------------------

PONS_Dossiers <- fread("PONS Dossiers.txt")
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, age, gender, code, earliest)
PONS_Dossiers <- PONS_Dossiers %>% filter(age>=18)

PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis=="Cancer Metastasis"|diagnosis=="Cancer Diagnosis") %>%
  group_by(patid) %>% filter(earliest==min(earliest))


PONS_Dossiers %>% group_by(patid) %>% count() %>% filter(n>1)

1,127,290 (>1 ~ 65,301) (0.05792742 have +1)


# ---------------
# Incidence New SKin / Lung patients  ----------------------------

PONS_Dossiers <- fread("PONS Dossiers.txt")
unique(PONS_Dossiers$diagnosis)

PONS_Dossiers <- PONS_Dossiers %>% filter(diagnosis=="Cancer Diagnosis"|diagnosis=="Cancer Metastasis")
Filtered_Codes_ToKeep <- fread("PONS Diagnosis Codes 2.0 - with skin flag.txt")
unique(Filtered_Codes_ToKeep$`include in skin?`)
Filtered_Codes_ToKeep <- Filtered_Codes_ToKeep %>% filter(is.na(`include in skin?`)|`include in skin?`=="1")
unique(Filtered_Codes_ToKeep$`include in skin?`)
Filtered_Codes_ToKeep <- Filtered_Codes_ToKeep %>% select(code, condition)

PONS_Dossiers <- PONS_Dossiers %>% inner_join(Filtered_Codes_ToKeep)
PONS_Dossiers <- PONS_Dossiers %>% filter(age>=18)
PONS_Dossiers <- PONS_Dossiers %>% select(-c(diagnosis, code, gender, age))

PONS_Dossiers %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 24704206 

PONS_Dossiers <- PONS_Dossiers %>% distinct()

PONS_Dossiers$earliest <- as.Date(PONS_Dossiers$earliest)

PONS_Dossiers <- PONS_Dossiers %>% arrange(patid, earliest)

PONS_Dossiers <- PONS_Dossiers %>%
  filter(condition!="Unspecified Cancer")  %>%
  group_by(patid) %>% filter(earliest==min(earliest)) %>% filter(frequency==max(frequency))


Single_Dx_pats <- PONS_Dossiers %>% group_by(patid) %>% count() %>% filter(n==1)
names(Single_Dx_pats)[2] <- "Rank"

PONS_Dossiers <- PONS_Dossiers %>% left_join(Single_Dx_pats)

Rank_Cancer_Dx <- fread("Rank_Cancer_Dx.csv")
names(Rank_Cancer_Dx)[1] <- "condition"

PONS_Dossiers <- PONS_Dossiers %>% ungroup() %>% left_join(Rank_Cancer_Dx) %>%
  mutate(Rank=ifelse(is.na(Rank),Rank_Dxs, Rank)) %>% select(-Rank_Dxs) %>%
  group_by(patid) %>% filter(Rank==min(Rank))

PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, condition)
PONS_Dossiers %>% ungroup() %>% filter(condition=="Skin Cancer") %>% summarise(n=sum(weight)) # 929,093

New_skin_patients <- PONS_Dossiers %>% ungroup() %>% filter(condition=="Skin Cancer") 

fwrite(New_skin_patients, "New_skin_patients.txt", sep="\t")
New_skin_patients <- fread("New_skin_patients.txt", sep="\t")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, diagnosis, died, cancer_metastasis, cachexia_onset, has_oncology)
PONS_Demographics %>%  summarise(n=sum(weight)) # 43195153
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics %>%  summarise(n=sum(weight)) # 42839416
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
sum(PONS_Demographics$weight) # 42839416
PONS_Demographics %>% filter(diagnosis != "-") %>% summarise(n=sum(weight)) # 34051850
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, died)

PONS_Demographics %>% left_join(New_skin_patients)

temp <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
temp <- temp %>% group_by(patid) %>% slice(1) %>% select(patid, diagnosis)
names(temp)[2] <- "Primary_Cancer"

PONS_Demographics <- PONS_Demographics %>% left_join(temp) %>% left_join(New_skin_patients) 

Skin_Cancers <- PONS_Demographics %>% filter(condition=="Skin Cancer"&Primary_Cancer=="Skin Cancer")

temp <- PONS_Demographics %>% filter(Primary_Cancer!="Skin Cancer") %>% bind_rows(Skin_Cancers)

temp <- temp %>% select(patid, weight, died, Primary_Cancer)

temp2 <- PONS_Demographics %>% ungroup() %>% filter(is.na(condition)&grepl("Skin Cancer",Primary_Cancer)&
                                                      grepl("Lung Cancer",diagnosis)) 

temp2$Primary_Cancer <- "Lung Cancer"
temp2 <- temp2 %>% select(patid, weight, died, Primary_Cancer)

final <- temp %>% bind_rows(temp2)

sum(final$weight)

data.frame(final %>% filter(died=="N") %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight)) %>% arrange(-n))



fwrite(final, "New_Primary_Cancer_Box.txt", sep="\t")
final <- fread("New_Primary_Cancer_Box.txt", sep="\t")
length(unique(final$patid))


PONS_Dossiers <- fread("PONS Dossiers.txt")
length(unique(PONS_Dossiers$patid))
PONS_Dossiers$earliest <- as.Date(PONS_Dossiers$earliest)
PONS_Dossiers <- PONS_Dossiers %>% select(patid, weight, diagnosis, condition, earliest)
PONS_Dossiers <- PONS_Dossiers %>% select(patid, condition, earliest)

PONS_Dossiers <- PONS_Dossiers %>% inner_join(final, by=c("patid"="patid", "condition"="Primary_Cancer")) %>% group_by(patid) %>% filter(earliest==min(earliest))
PONS_Dossiers <- PONS_Dossiers %>% group_by(patid) %>% slice(1)  %>% ungroup()

PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer") %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight)) # 1949119
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2019-08-01"&earliest<="2020-07-31") %>% summarise(n=sum(weight)) # 2015160
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2018-08-01"&earliest<="2019-07-31") %>% summarise(n=sum(weight)) # 2518505
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2017-08-01"&earliest<="2018-07-31") %>% summarise(n=sum(weight)) # 3133924
PONS_Dossiers %>% filter(condition!="-"&condition!="Unspecified Cancer"&condition!="Skin Cancer") %>% filter(earliest>="2016-08-01"&earliest<="2017-07-31") %>% summarise(n=sum(weight)) # 12059265

sum(PONS_Dossiers$weight) #22984859
data.frame(PONS_Dossiers %>% filter(died=="N") %>% group_by(condition)  %>% summarise(n=sum(weight)) %>% arrange(-n))


data.frame(PONS_Dossiers %>% group_by(condition) %>% filter(earliest>="2020-08-01"&earliest<="2021-07-31") %>% summarise(n=sum(weight))) # 3119190



# ------
# Comorbidities 1 digit all vs cachexia dx vs cachexia pred ae- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight)) # 22984889 (22602908 without unspecified)

# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)
Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_Dx$weight)

# Cachexia Pred
Cachexia_pats <- fread("Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pats <- Cachexia_pats %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_pats$weight)

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60) %>% filter(Status=="Earliest"|Status=="Metastasis") %>% ungroup() %>% select(patid)


# All Pats
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight) %>% anti_join(Cachexia_Dx %>% select(patid))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight) %>% anti_join(Cachexia_pats %>% select(patid))
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight)) # 19733199
All_Pats <- New_Primary_Cancer_Box
# Cachecia Dx
Cachexia_Dx <- Cachexia_Dx %>% inner_join(PONS_Demographics)
sum(Cachexia_Dx$weight) # 133584.1
# Cachexia Pred
Cachexia_pats <- Cachexia_pats %>% inner_join(PONS_Demographics)
sum(Cachexia_pats$weight) # 1259277


PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[3] <- "ICD"
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-weight)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% inner_join(All_Pats %>% full_join(Cachexia_Dx) %>% full_join(Cachexia_pats))
PONS_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 20992446

PONS_Comorbidity_Inventories$ICD <- substr(PONS_Comorbidity_Inventories$ICD,1,2)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()





All_PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories  %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("G", ICD)|grepl("H", ICD)|
                                                                               grepl("I", ICD)|grepl("J", ICD)|grepl("K", ICD)|grepl("L", ICD)|grepl("M", ICD)|
                                                                               grepl("N", ICD)|grepl("R", ICD)) %>%
  group_by(ICD) %>% summarise(n=sum(weight)) %>% mutate(All_Proportion=n/20992446) %>% filter(All_Proportion>0.02) %>% select(-n)

PONS_Comorbidity_Inventories %>% group_by(patid) %>% count() %>% ungroup() %>% summarise(n2=mean(n)) # 45.0





Dx_PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% inner_join(Cachexia_Dx) %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("G", ICD)|grepl("H", ICD)|
                                                                                                         grepl("I", ICD)|grepl("J", ICD)|grepl("K", ICD)|grepl("L", ICD)|grepl("M", ICD)|
                                                                                                         grepl("N", ICD)|grepl("R", ICD)) %>%
  group_by(ICD) %>% summarise(n=sum(weight)) %>% mutate(Dx_Proportion=n/133584.1) %>% filter(Dx_Proportion>0.02) %>% select(-n)

PONS_Comorbidity_Inventories %>% inner_join(Cachexia_Dx) %>%  group_by(patid) %>% count()  %>% ungroup() %>% summarise(n2=mean(n)) #65.1






Pr_PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% inner_join(Cachexia_pats) %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("G", ICD)|grepl("H", ICD)|
                                                                                                           grepl("I", ICD)|grepl("J", ICD)|grepl("K", ICD)|grepl("L", ICD)|grepl("M", ICD)|
                                                                                                           grepl("N", ICD)|grepl("R", ICD)) %>%
  group_by(ICD) %>% summarise(n=sum(weight)) %>% mutate(Pr_Proportion=n/1259277) %>% filter(Pr_Proportion>0.02) %>% select(-n)


PONS_Comorbidity_Inventories %>% inner_join(Cachexia_pats) %>%  group_by(patid) %>% count()  %>% ungroup() %>% summarise(n2=mean(n)) # 51.0





temp <- All_PONS_Comorbidity_Inventories %>% left_join(Dx_PONS_Comorbidity_Inventories) %>% left_join(Pr_PONS_Comorbidity_Inventories) 

temp[is.na(temp)] <- 0

fwrite(temp, "Comorbidites_proportion_All_vs_Cachexia.csv")

temp <- fread("Comorbidites_proportion_All_vs_Cachexia.csv")
temp <- temp %>% filter(!grepl("D",ICD)&!grepl("L",ICD)&!grepl("N",ICD))


# rownames(temp) <- temp$ICD
# temp2 <- temp %>% select(-ICD)
# rownames(temp2) <- temp$ICD
# rownames(temp2) 
# temp2 <- as.matrix(temp2)
# rownames(temp2) <- temp$ICD
# 
# my_colors <- colorRampPalette(c("cornsilk", "deepskyblue4"))  
# heatmap(temp2, Rowv = NA, Colv = NA, col = my_colors(10000000))    


ggplot(melt(temp), aes(ICD,variable)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))+
  scale_fill_gradient(low = "lightblue", high = "darkred")



# ------------------

# Predict Dx ICD10s Cacheixa VS no Cachexia --------------------
# All Pats +18: Cachexia Dx vs no Dx
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age, gender, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))
Pats_to_track <- PONS_Demographics

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer!="-",c(1)] 
Pats_to_track <- New_Primary_Cancer_Box %>% inner_join(Pats_to_track)


# MAX BMI drop
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% inner_join(Pats_to_track %>% select(patid))
Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))
PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()
PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))
PONS_Measures <- PONS_Measures %>% mutate(difference=(min-max)*100/max)  %>% select(patid, difference) %>% distinct()
Pats_to_track <- Pats_to_track %>% left_join(PONS_Measures) %>% drop_na()









# Dx Codes
PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[3] <- "ICD"
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(ICD!="R64")
PONS_Comorbidity_Inventories$ICD <- substr(PONS_Comorbidity_Inventories$ICD,1,2)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()
# PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories  %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("G", ICD)|grepl("H", ICD)|
#                                                                                grepl("I", ICD)|grepl("J", ICD)|grepl("K", ICD)|grepl("L", ICD)|grepl("M", ICD)|
#                                                                                grepl("N", ICD)|grepl("R", ICD))
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories  %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("R", ICD))
length(unique(PONS_Comorbidity_Inventories$ICD))
PONS_Comorbidity_Inventories <- Pats_to_track %>% select(patid) %>% left_join(PONS_Comorbidity_Inventories)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories[,c(1,3)]
PONS_Comorbidity_Inventories$value <- 1
length(unique(PONS_Comorbidity_Inventories$patid)) 
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% spread(key=ICD, value=value)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-c(47,46,45)) %>% select(-c(ND, FZ))
PONS_Comorbidity_Inventories[is.na(PONS_Comorbidity_Inventories)] <- 0
Pats_to_track <- Pats_to_track %>% left_join(PONS_Comorbidity_Inventories)
sum(is.na(Pats_to_track))
Pats_to_track[is.na(Pats_to_track)] <- 0






fwrite(temp, "temp_all_ICDs_QoL.txt", sep="\t")
# Fit a model
temp <- Pats_to_track
temp %>% group_by(cachexia_onset) %>% count() 
temp <- temp %>% group_by(cachexia_onset) %>% sample_n(9000) %>% ungroup()
temp$cachexia_onset <- as.factor(temp$cachexia_onset)
temp <- temp %>% select(-patid)
temp <- temp %>% mutate(gender = ifelse(gender=="M", 1, 0))


modelAll_1_glm <- glm(cachexia_onset ~ ., data = temp, family = binomial)
summary(modelAll_1_glm)
modelAll_1_glm_summary <- summary(modelAll_1_glm)

fwrite(modelAll_1_glm_summary, "modelAll_1_glm_summary.csv", sep="\t")

library("randomForest")
modelAll_1_randomForest <- randomForest(cachexia_onset ~ ., data = temp)
summary(modelAll_1_randomForest)


modelAll_1_randomForest$importance


library("gbm")
modelAll_1_gradientBoost <- gbm(cachexia_onset==1 ~ ., data = temp, n.trees = 15000, distribution = "bernoulli")
summary(modelAll_1_gradientBoost)

# -------------

# Predict Dx ICD10s BMI reductions VS no reduction -------------------

# All Pats +18: Cachexia Dx vs no Dx
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age, gender, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% filter(is.na(cachexia_onset)) %>% select(-cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) 
Pats_to_track <- PONS_Demographics

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer!="-",c(1)] 
Pats_to_track <- New_Primary_Cancer_Box %>% inner_join(Pats_to_track)


# MAX BMI drop
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% inner_join(Pats_to_track %>% select(patid))
Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))
PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()
PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))
PONS_Measures <- PONS_Measures %>% mutate(difference=(min-max)*100/max)  %>% select(patid, difference) %>% distinct()
Pats_to_track <- Pats_to_track %>% left_join(PONS_Measures) %>% drop_na()





# Filter for those that meet criteria 
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- Pats_to_track %>% select(patid) %>% inner_join(PONS_Measures)

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

Pats_to_track <- PONS_Measures %>% select(patid) %>% distinct() %>% inner_join(Pats_to_track) %>% mutate(Plus10BMI="Plus10BMI") 

PONS_Measures <- data.frame(PONS_Measures %>% ungroup() %>% filter( (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)|
                                                                      (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183) |
                                                                      (min<max*0.98 & min<20 & mindate>maxdate) ) %>%
                              select(patid) %>% distinct()) %>% mutate(CachexiaPred="CachexiaPred")

Pats_to_track <- Pats_to_track %>% left_join(PONS_Measures)
Pats_to_track <- Pats_to_track %>% mutate(CachexiaPred=ifelse(is.na(CachexiaPred),0,1))




# Dx Codes
PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[3] <- "ICD"
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(ICD!="R64")
PONS_Comorbidity_Inventories$ICD <- substr(PONS_Comorbidity_Inventories$ICD,1,2)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()
# PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories  %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("G", ICD)|grepl("H", ICD)|
#                                                                                grepl("I", ICD)|grepl("J", ICD)|grepl("K", ICD)|grepl("L", ICD)|grepl("M", ICD)|
#                                                                                grepl("N", ICD)|grepl("R", ICD))
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories  %>% filter(grepl("D", ICD)|grepl("E", ICD)|grepl("F", ICD)|grepl("R", ICD))
length(unique(PONS_Comorbidity_Inventories$ICD))
PONS_Comorbidity_Inventories <- Pats_to_track %>% select(patid) %>% left_join(PONS_Comorbidity_Inventories)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories[,c(1,3)]
PONS_Comorbidity_Inventories$value <- 1
length(unique(PONS_Comorbidity_Inventories$patid)) 
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% distinct()
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% spread(key=ICD, value=value)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-c(44,45)) %>% select(-c(ND, ED))
PONS_Comorbidity_Inventories[is.na(PONS_Comorbidity_Inventories)] <- 0
Pats_to_track <- Pats_to_track %>% left_join(PONS_Comorbidity_Inventories)
sum(is.na(Pats_to_track))
Pats_to_track[is.na(Pats_to_track)] <- 0






fwrite(temp, "temp_all_ICDs_QoL.txt", sep="\t")
fwrite(temp, "temp_all_ICDs_QoL_BMIs.txt", sep="\t")

# Fit a model
temp <- Pats_to_track

temp <- fread("temp_all_ICDs_QoL_BMIs.txt", sep="\t")
temp %>% group_by(CachexiaPred) %>% count() 
temp <- temp %>% group_by(CachexiaPred) %>% sample_n(20000) %>% ungroup()
temp$CachexiaPred <- as.factor(temp$CachexiaPred)
temp <- temp %>% select(-c(patid, Plus10BMI))
temp <- temp %>% mutate(gender = ifelse(gender=="M", 1, 0))
unique(temp$CachexiaPred)

modelAll_1_glm <- glm(CachexiaPred ~ ., data = temp, family = binomial)
summary(modelAll_1_glm)
modelAll_1_glm_summary <- summary(modelAll_1_glm)



library("randomForest")
modelAll_1_randomForest <- randomForest(CachexiaPred ~ ., data = temp)
summary(modelAll_1_randomForest)



library("gbm")
modelAll_1_gradientBoost <- gbm(CachexiaPred==1 ~ ., data = temp, n.trees = 15000, distribution = "bernoulli")
summary(modelAll_1_gradientBoost)


# ---------------

# Currently Active/metastatic Cachexia patients - time since cancer/cachexia onset ? ------------
Cachexia_pats <- fread("Cachexia_pats.txt", sep="\t")
Cachexia_pats <- Cachexia_pats %>% select(patid)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
Cachexia_pats <- New_Primary_Cancer_Box %>% inner_join(Cachexia_pats)
Cachexia_pats


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60)
PONS_Demographics <- PONS_Demographics %>% filter(Status=="Earliest"|Status=="Metastasis") %>% select(patid, weight)

Cachexia_pats <- Cachexia_pats %>% inner_join(PONS_Demographics)

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

Cachexia_pats <- Cachexia_pats %>% left_join(PONS_Demographics)
Cachexia_pats$cancer_onset <- as.Date(Cachexia_pats$cancer_onset)

Cachexia_pats <- Cachexia_pats %>% mutate(Elapsed= (as.numeric(as.Date("2021-07-31"))-as.numeric(cancer_onset))/30.5)

Cachexia_pats %>% 
  ggplot(aes(Elapsed)) +
  geom_density(size=2, colour="firebrick", fill="firebrick", alpha=0.7) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n No. months since cancer onset")+
  ylab("Proportion of all cachexia patients \n")


Cachexia_pats %>% mutate(Elapsed=round(Elapsed/12)) %>% group_by(Elapsed) %>% summarise(n=sum(weight))


# Cachexia_pats

# Time from cachexia to Jul 21
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
Cachexia_pats <- Cachexia_pats %>% left_join(PONS_Demographics)

rm(PONS_Demographics)

# select pats.... first.

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690714 (852576 samples)
length(unique(PONS_Measures$patid))

data.frame(PONS_Measures %>% select(patid, weight) %>% distinct() %>% inner_join(Pats_to_track_BMI) %>%
             group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 
PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690470 (25931366 records, 852569 samples) (-244 pats, haven't lost patients, simply 29412 records)
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

First_CachexiaPred <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate ) | 
                                                                (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| 
                                                                (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, mindate) %>% group_by(patid) %>% filter(mindate==min(mindate)) %>% slice(1)

rm(PONS_Measures)

Cachexia_pats <- Cachexia_pats %>% left_join(First_CachexiaPred) %>% mutate(FistCachexia=
                                                                              ifelse(is.na(cachexia_onset),as.Date(mindate), 
                                                                                     as.Date(cachexia_onset))) 


Cachexia_pats %>% select(patid, weight, FistCachexia) %>% drop_na() %>%
  mutate(Elapsed=(18839-FistCachexia)/30.5) %>%
  ggplot(aes(Elapsed)) +
  geom_density(size=2, colour="firebrick", fill="firebrick", alpha=0.7) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n No. months since cachexia onset")+
  ylab("Proportion of all cachexia patients \n")



Cachexia_pats %>% select(patid, weight, FistCachexia) %>% drop_na() %>%
  mutate(Elapsed=(18839-FistCachexia)/30.5) %>%
  mutate(Elapsed=round(Elapsed/12)) %>% group_by(Elapsed) %>% summarise(n=sum(weight))



Cachexia_pats %>% select(patid, weight, FistCachexia) %>% drop_na() %>%
  mutate(Elapsed=(18839-FistCachexia)/30.5) %>%
  mutate(Year=ifelse(Elapsed<=12,1,
                     ifelse(Elapsed<=24,2,
                            ifelse(Elapsed<=36,3,
                                   ifelse(Elapsed<=48,4,5))))) %>% group_by(Year) %>% summarise(n=sum(weight))



# --------------
# BMI drops in Cacheixa Dx vs Cachexia Pred vs Non cachexia -----------------

# All Pats 
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight)) # 22984889 (22602908 without unspecified)

# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)
Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_Dx$weight)

# Cachexia Pred
Cachexia_pats <- fread("Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pats <- Cachexia_pats %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_pats$weight)
Cachexia_pred <- Cachexia_pats %>% anti_join(Cachexia_Dx)

sum(Cachexia_Dx$weight)
sum(Cachexia_pred$weight)


Cachexia_Dx <- Cachexia_Dx %>% mutate(Group="Dx")
Cachexia_pred <- Cachexia_pred %>% mutate(Group="Pred")

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(New_Primary_Cancer_Box)

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(!(value >= 1.5*median | value <= 0.5*median))

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

PONS_Measures <- PONS_Measures %>% select(patid,weight,min,max) %>% distinct()
PONS_Measures <- PONS_Measures %>% mutate(difference=(min-max)*100/max) 

PONS_Measures %>% left_join(Cachexia_Dx %>% full_join(Cachexia_pred)) %>% 
  group_by(Group) %>% summarise(n=weighted.mean(difference, weight))



PONS_Measures %>% left_join(Cachexia_Dx %>% full_join(Cachexia_pred)) %>% 
  mutate(absdiff=min-max) %>%
  group_by(Group) %>% summarise(n=weighted.mean(absdiff, weight))



PONS_Measures %>% left_join(Cachexia_Dx %>% full_join(Cachexia_pred)) %>%
  mutate(absdiff=min-max) %>%
  ggplot(aes(absdiff)) +
  geom_density(size=2, colour="deepskyblue4", fill="deepskyblue4", alpha=0.7) +
  facet_wrap(~Group) + 
  #xlim(-60,-5) + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n % Min-to-Max BMI Difference")+
  ylab("Proportion of all patients \n")


# ------------


# BMI bucket among all cachexia patients ---------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight)) # 22984889 (22602908 without unspecified)

# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, diagnosis, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)
Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_Dx$weight)

# Cachexia Pred
Cachexia_pats <- fread("Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pats <- Cachexia_pats %>% inner_join(New_Primary_Cancer_Box %>% select(patid, weight))
sum(Cachexia_pats$weight)
Cachexia_pred <- Cachexia_pats %>% anti_join(Cachexia_Dx)

sum(Cachexia_Dx$weight)
sum(Cachexia_pred$weight)

Cachexia_Dx <- Cachexia_Dx %>% mutate(Group="Dx")
Cachexia_pred <- Cachexia_pred %>% mutate(Group="Pred")


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(New_Primary_Cancer_Box)

PONS_Measures <- PONS_Measures %>% select(patid, weight, value, claimed)

# 2 versions (last vs min)
# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==max(value)) %>% slice(1)
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(value==min(value))  %>% slice(1)

PONS_Measures <- PONS_Measures %>% left_join(Cachexia_Dx %>% full_join(Cachexia_pred))

PONS_Measures %>% mutate(valuebucket=ifelse(value<20,"<20",
                                            ifelse(value>=20&value<25,"20-25",
                                                   ifelse(value>=25&value<27,"25-27",
                                                          ifelse(value>=27&value<30,"27-30",">30"))))) %>%
  group_by(Group, valuebucket) %>% summarise(n=sum(weight)) %>%
  spread(key=valuebucket, value=n)


# -------------


# BMI distribution for patients with 1 record, 2 records, 2-9 records, 10+ records ----------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight))

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(New_Primary_Cancer_Box) %>% select(-died, Primary_Cancer)
PONS_Measures <- PONS_Measures %>% select(patid, value) %>% group_by(patid) %>% mutate(records=n())

data.frame(PONS_Measures %>% ungroup() %>% group_by(records) %>% summarise(n=mean(value))) %>%
  filter(records<50) %>%
  ggplot(aes(records, n))+
  geom_point(size=2, alpha=0.6)+
  ylim(25,40)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Nº. BMI records")+
  ylab("Average BMI (kg/m2) \n")


PONS_Measures %>% ungroup()  %>% mutate(records=ifelse(records==1,"1",
                                                       ifelse(records>=2&records<10,"2-10",">10"))) %>%
  group_by(patid) %>% mutate(meanBMI=mean(value)) %>%
  select(patid,meanBMI, records) %>% distinct() %>% ungroup() %>%
  group_by(records) %>% summarise(n=mean(meanBMI))


# -----
# BMI bucket cachexia patients per primary cancer -------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")
New_Primary_Cancer_Box  %>%  summarise(n=sum(weight)) # 22984889 (22602908 without unspecified)

# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)
Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box )
sum(Cachexia_Dx$weight)

# Cachexia Pred
Cachexia_pats <- fread("Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pats <- Cachexia_pats %>% inner_join(New_Primary_Cancer_Box)
sum(Cachexia_pats$weight)
Cachexia_pred <- Cachexia_pats %>% anti_join(Cachexia_Dx)

sum(Cachexia_Dx$weight)
sum(Cachexia_pred$weight)

Cachexia_Dx <- Cachexia_Dx %>% mutate(Group="Dx")
Cachexia_pred <- Cachexia_pred %>% mutate(Group="Pred")


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Cachexia_Dx %>% full_join(Cachexia_pred) %>% select(patid))

PONS_Measures <- PONS_Measures %>% select(patid, value, claimed)

# 2 versions (last vs min)
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==max(value)) %>% slice(1)
# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(value==min(value))  %>% slice(1)


PONS_Measures <- PONS_Measures %>% left_join(Cachexia_Dx %>% full_join(Cachexia_pred))


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60)
PONS_Demographics <- PONS_Demographics %>% filter(Status=="Earliest"|Status=="Metastasis") %>% select(patid, weight)



data.frame(PONS_Measures %>% inner_join(PONS_Demographics) %>% mutate(valuebucket=ifelse(value<20,"<20",
                                                                                         ifelse(value>=20&value<25,"20-25",
                                                                                                ifelse(value>=25&value<27,"25-27",
                                                                                                       ifelse(value>=27&value<30,"27-30",">30"))))) %>%
             group_by(Primary_Cancer, valuebucket) %>% summarise(n=sum(weight)) %>%
             spread(key=valuebucket  , value=n))


# -------------
# Time to death / severity BMI drops before vs only after Dx -----------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer")

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- New_Primary_Cancer_Box %>% select(-c(died,Primary_Cancer)) %>% 
  inner_join(PONS_Measures %>% select(-c(weight, test)))

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value))
PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)
PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)
PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value<1.5*median&value>0.5*median) 
PONS_Measures %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 25690470 (25931366 records, 852569 samples) (-244 pats, haven't lost patients, simply 29412 records)
Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% summarise(mean=mean(value), median=median(value), min=min(value), max=max(value))
PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value==min) %>% mutate(mindate=claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value==max) %>% mutate(maxdate=claimed) %>% select(patid, claimed, maxdate),
            by="patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)
PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% filter(n>=10) %>% select(patid) %>% inner_join(PONS_Measures)

First_CachexiaPred <- PONS_Measures %>% ungroup() %>% filter( (min<max*0.98 & min<20 & mindate>maxdate ) | 
                                                                (min<max*0.90 & mindate>maxdate & abs(mindate-maxdate)<=366)| 
                                                                (min<max*0.95 & mindate>maxdate & abs(mindate-maxdate)<=183)) %>%
  select(patid, mindate) %>% group_by(patid) %>% filter(mindate==min(mindate)) %>% slice(1)

# Date of first cancer Dx
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_onset, death_date)

First_CachexiaPred <- First_CachexiaPred %>% left_join(PONS_Demographics_temp)
First_CachexiaPred$cancer_onset <- as.Date(First_CachexiaPred$cancer_onset)
First_CachexiaPred$death_date <- as.Date(First_CachexiaPred$death_date)

First_CachexiaPred <- First_CachexiaPred %>% mutate(DropBefore=ifelse(mindate<cancer_onset, "YES", "NO"))

missingDeathDay <- ymd("2050-12-31")
First_CachexiaPred <- First_CachexiaPred %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

First_CachexiaPred <- First_CachexiaPred %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

First_CachexiaPred %>% filter(DropBefore=="YES") %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 46.6
First_CachexiaPred %>% filter(DropBefore=="NO") %>% ungroup() %>% summarise(weighted.median(Survived, weight)) # 46.5

PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))


First_CachexiaPred %>% filter(DropBefore=="YES") %>% left_join(PONS_Demographics_temp) %>%
  ungroup() %>% group_by(cancer_metastasis) %>% summarise(weighted.median(Survived, weight)) 



First_CachexiaPred %>% filter(DropBefore=="NO") %>% left_join(PONS_Demographics_temp) %>%
  ungroup() %>% group_by(cancer_metastasis) %>% summarise(weighted.median(Survived, weight)) 



First_CachexiaPred %>% left_join(PONS_Demographics_temp) %>%
  group_by(cancer_metastasis, DropBefore) %>% count()








data.frame(First_CachexiaPred %>% filter(DropBefore=="YES") %>%
             group_by(Survived) %>% summarise(n=sum(weight)))


data.frame(First_CachexiaPred %>% filter(DropBefore=="NO") %>%
             group_by(Survived) %>% summarise(n=sum(weight)))



PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(First_CachexiaPred %>% select(patid))
PONS_Measures <- PONS_Measures %>% select(patid, value, claimed)
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==max(value)) %>% slice(1)

First_CachexiaPred %>% left_join(PONS_Measures) %>%
  group_by(DropBefore) %>% summarise(n=mean(value))



# ------------







# Cachexia population estimate comparing all possible values WIDE -----------------------------------------------

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
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

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")

PONS_Measures <- PONS_Demographics %>% filter(Exact_Month==60 & (Status=="Earliest" | Status=="Metastasis" | Status=="Death")) %>% select(patid) %>% inner_join(PONS_Measures)

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)
PONS_Measures <- PONS_Measures %>% select(-weight)





# Convert BMI records to wide format

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
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


fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep="\t")
fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep="\t")

temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)


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

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6) & (Month_Min>=49),1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12) & (Month_Min>=49),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) & (Month_Min>=49),1,0 ))


New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()
New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()



New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)  %>% ungroup() %>%
  summarise(n=sum(POP_Mets)) # 1524333


data.frame(New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
             full_join(Pats_to_track_BMI %>% filter(cachexia_onset>="2020-08-01")) %>%
             distinct() %>% 
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)  %>% ungroup()) %>%
  summarise(n=sum(POP_Mets))  # 1603718


CachexiaPats_ALL_NEW <- New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
             full_join(Pats_to_track_BMI %>% filter(cachexia_onset>="2020-08-01")) %>%
             distinct() %>% select(patid) %>% distinct()

fwrite(CachexiaPats_ALL_NEW, "CachexiaPats_ALL_NEW.txt")

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")





# WHO HAS BMI>30 ?
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- New_Cachexia_Pred %>% left_join(PONS_Measures) %>% select(patid, claimed, value)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% group_by(patid) %>%  filter(value==min(value))

Pats_BMI_30 <- PONS_Measures %>% ungroup() %>% filter(value>=30) %>% select(patid) %>% distinct()


data.frame(New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
             full_join(Pats_to_track_BMI %>% filter(cachexia_onset>="2020-08-01")) %>%
             distinct() %>%  anti_join(Pats_BMI_30) %>%
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)  ) 




# WHO HAS BEEN TREATED LAST2 YEARS
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>% left_join(CAN_Drug_Histories, by=c("patid"="patient"))
sum(CAN_Drug_Histories$weight)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, Primary_Cancer, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=37)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[5] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15"|
                                drug_class=="Surgery Inpatient") %>% select(patid) %>% distinct()
# 
# Treat_Last_2Years <- CAN_Drug_Histories



# WHO HAS BEEN ACTIVE LAST 2 YEARS? 

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")

Active_Last3y <- PONS_Demographics %>% filter(Exact_Month>=25) %>% filter(Status=="Earliest"|Status=="Metastasis") %>% select(patid) %>% distinct()
NaiveFirst3y <- PONS_Demographics %>% filter(Exact_Month==24) %>% filter(Status=="Naive") %>% select(patid) %>% distinct()

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

Pats_to_track_BMI <- Active_Last3y %>% inner_join(NaiveFirst3y) %>% inner_join(Pats_to_track_BMI)  

sum(Pats_to_track_BMI$weight) # 6591459


data.frame(Pats_to_track_BMI %>%  ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))





temp2 <- temp %>% mutate(percent=100*(Max-Min)/Max) %>% select(Min, percent, Drop90)

mean(temp2$percent)  # 16.38256
median(temp2$percent)  # 14.02157


temp2 %>%
  ggplot(aes(percent)) +
  geom_density(size=1.5, fill="darkblue",  colour="darkblue") +
  xlim(10,40) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Percentage BMI drop (%)")+
  ylab("Patient/Cohort proportion \n")

temp3 <- temp2 %>% sample_n(100000)

temp3 %>%
  mutate(percent=percent*(-1)) %>% 
  mutate(Drop90=as.factor(Drop90)) %>%
  ggplot(aes(Min, percent, colour=Drop90)) +
  geom_jitter(size=0.2, alpha=0.4) +
  scale_colour_manual(values = c("azure2","firebrick")) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  ylim(-50,+25) + xlim(0,60) +
  xlab("\n Monthly Min BMI")+
  ylab("% Month-over-Month BMI Change \n")
# -------------------------------------------------------------------

# Where are cachexia aptients now? Death vs Active bs Metastatic ---------------------

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")

PONS_Time_Series_Groups <- fread("PONS_Time_Series_Groups.txt", sep="\t")
PONS_Time_Series_Groups <- PONS_Time_Series_Groups %>% filter(Exact_Month==60) %>% select(patid, weight, Status)


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")


CachexiaPats_ALL_NEW %>% left_join(PONS_Time_Series_Groups) %>% summarise(n=sum(weight)) # 1603718

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CachexiaPats_ALL_NEW %>% left_join(PONS_Measures) %>% select(patid, claimed, value)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==max(value))
Pats_BMI_30 <- PONS_Measures %>% ungroup() %>% filter(value>=30) %>% select(patid) %>% distinct()


data.frame(CachexiaPats_ALL_NEW %>% anti_join(Pats_BMI_30) %>% left_join(PONS_Time_Series_Groups) %>% left_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)) %>% 
  group_by(Primary_Cancer, Status) %>% summarise(n=sum(weight))) %>%
  spread(key=Status, value=n)# 1603718

# -------------------------------------------------------
# Cachexia population estimate comparing all possible values WIDE AFTER 1st Dx ----------------------
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1)) %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(died=="N" & diagnosis!="-") 


temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)
temp <- temp %>% left_join(PONS_Demographics) %>% drop_na()
temp <- temp %>% mutate(cancer_onset=str_sub(cancer_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% left_join(Months_lookup, by=c("cancer_onset"="Month")) %>% select(-cancer_onset)


temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6) & (Month_Min>=Exact_Month),1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12) & (Month_Min>=Exact_Month),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) & (Month_Min>=Exact_Month),1,0))

New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop90==1 | Drop2_20 ==1) %>% select(patid) %>% distinct()


New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>% summarise(POP_Mets=sum(weight)) # 1756503

data.frame(New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))

temp %>% filter(Drop95==1) %>% group_by(patid) %>% filter(Min==min(Min)) %>% slice(1) %>% ungroup() %>% summarise(n=median(Min))
temp %>% filter(Drop90==1) %>% group_by(patid) %>% filter(Min==min(Min)) %>% slice(1) %>% ungroup() %>% summarise(n=median(Min))
temp %>% filter(Drop95==1|Drop90==1) %>% group_by(patid) %>% filter(Min==min(Min)) %>% slice(1) %>% ungroup() %>% summarise(n=median(Min))


temp2 <-  temp %>% select(Max, Min, Drop95, Drop90) %>% sample_n(100000)

temp2 %>%
  mutate(Any=ifelse(Drop90==1|Drop95==1,1,0)) %>%
  mutate(Any=as.factor(Any)) %>%
  ggplot(aes(Max, Min, colour=Any)) +
  geom_jitter(size=0.2, alpha=0.3) +
  scale_colour_manual(values = c("azure2","darkred")) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  ylim(10,50) + xlim(10,50) +
  geom_abline(slope=1, intercept = 0)+
  xlab("\n Monthly MAX BMI")+
  ylab("Monthly MIN BMI \n")


switchMat_95drop <- temp %>% select(Max, Min, Drop95, Drop90) %>% mutate(Max=round(Max), Min=round(Min)) %>%
  filter(Drop95==1) %>% group_by(Max, Min) %>% count() %>%
  filter(Max<60&Max>15&Min>15&Min<60) %>%
  spread(key=Min, value=n)

fwrite(switchMat_95drop, "switchMat_95drop.csv")

temp %>% select(Max, Min, Drop95, Drop90) %>% mutate(Max=round(Max), Min=round(Min)) %>%
  filter(Drop95==1) %>% group_by(Max, Min) %>% count() %>%
  filter(Max<50&Max>15&Min>15&Min<50) %>%
  ggplot(aes(Max, Min, colour=n))+
  geom_point(aes(size=n), alpha=0.7, show.legend = F)+
  scale_size(range = c(1,15)) +
  xlim(20,40)+ylim(20,40) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



temp %>% select(patid, Max, Min, Drop95, Drop90) %>%
  filter(Drop95==1) %>%
  mutate(Difference=)
  group_by(patid) %>% filter(Min==min(Min)) %>% slice(1) %>% ungroup() %>% 
  ggplot(aes(Min))+
  geom_density()+
  xlim(15,50)


temp %>% select(patid, Max, Min, Drop95, Drop90) %>%
  filter(Drop90==1) %>%
  group_by(patid) %>% filter(Min==min(Min)) %>% slice(1) %>% ungroup() %>% 
  ggplot(aes(Min))+
  geom_density()+
  xlim(15,50)






switchMat_90drop <- temp %>% select(Max, Min, Drop95, Drop90) %>% mutate(Max=round(Max), Min=round(Min)) %>%
  filter(Drop90==1) %>% group_by(Max, Min) %>% count() %>%
  filter(Max<60&Max>15&Min>15&Min<60) %>%
  spread(key=Min, value=n)

fwrite(switchMat_90drop, "switchMat_90drop.csv")


temp %>% select(Max, Min, Drop95, Drop90) %>% mutate(Max=round(Max), Min=round(Min)) %>%
  filter(Drop90==1) %>% group_by(Max, Min) %>% count() %>%
  filter(Max<50&Max>15&Min>15&Min<50) %>%
  ggplot(aes(Max, Min, colour=n))+
  geom_point(aes(size=n), alpha=0.7, show.legend = F)+
  scale_size(range = c(1,15)) +
  xlim(20,40)+ylim(20,40) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())





temp2 <- temp %>% mutate(percent=100*(Max-Min)/Max) %>% select(Min, percent, Drop90)

mean(temp2$percent)  # 16.38256
median(temp2$percent)  # 14.02157


temp %>%
  mutate(percent=100*(Max-Min)/Max) %>%
  filter(Drop95==1) %>%
  ggplot(aes(Max-Min)) +
  geom_density(size=1.5, fill="darkblue",  colour="darkblue") +
  xlim(0,15) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Absolute BMI drop (Using 5% 6months cutoff)")+
  ylab("Patient/Cohort proportion \n")



temp %>%
  mutate(percent=100*(Max-Min)/Max) %>%
  filter(Drop90==1) %>%
  ggplot(aes(Max-Min)) +
  geom_density(size=1.5, fill="darkblue",  colour="darkblue") +
  xlim(0,15) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Absolute BMI drop (Using 10% 12months cutoff)")+
  ylab("Patient/Cohort proportion \n")
# ----------------------------
# Lab tests - initial exploratory --------------------------------------------------------

CachexiaPats_AllrecordsORdx40perc <- fread("CachexiaPats_AllrecordsORdx40perc.txt")
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid) %>% distinct()
Cachexia_Dx <- Cachexia_Dx %>% inner_join(CachexiaPats_AllrecordsORdx40perc)


PONS_Measures <- fread("PONS Measures.txt", sep="\t")

unique(PONS_Measures$test)
# [1] "BMI"                "Body Height"        "Body Weight"        "Hemoglobin"        
# [5] "Albumin"            "C-Reactive Protein" "Cancer Stage"

PONS_Measures <- PONS_Measures %>% filter(test=="Albumin")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)


# All Cancer
PONS_Measures %>%
  anti_join(CachexiaPats_AllrecordsORdx40perc) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  summarise(n=mean(value))  # 4.09

PONS_Measures %>%
    anti_join(CachexiaPats_AllrecordsORdx40perc) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkblue",  colour="darkblue") +
  xlim(1,5.5) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Albumin Level, All patients (g/dL)")+
  ylab("Patient/Cohort proportion \n")



# Cachexia Pred

PONS_Measures %>%
  inner_join(CachexiaPats_AllrecordsORdx40perc) %>%
  anti_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  summarise(n=mean(value)) # 4.04

PONS_Measures %>%
  inner_join(CachexiaPats_AllrecordsORdx40perc) %>%
    anti_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkblue",  colour="darkblue") +
  xlim(1,5.5) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Albumin Level, Cachexia Pred patients (g/dL)")+
  ylab("Patient/Cohort proportion \n")


# Cachexia Dx

PONS_Measures %>%
  inner_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  summarise(n=mean(value)) # 3.8

PONS_Measures %>%
  inner_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkblue",  colour="darkblue") +
  xlim(1,5.5) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Albumin Level, Cachexia Dx patients (g/dL)")+
  ylab("Patient/Cohort proportion \n")




CachexiaPats_AllrecordsORdx40perc <- fread("CachexiaPats_AllrecordsORdx40perc.txt")
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid) %>% distinct()
Cachexia_Dx <- Cachexia_Dx %>% inner_join(CachexiaPats_AllrecordsORdx40perc)


PONS_Measures <- fread("PONS Measures.txt", sep="\t")

unique(PONS_Measures$test)
# [1] "BMI"                "Body Height"        "Body Weight"        "Hemoglobin"        
# [5] "Albumin"            "C-Reactive Protein" "Cancer Stage"

PONS_Measures <- PONS_Measures %>% filter(test=="Hemoglobin")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)


# All Cancer
PONS_Measures %>%
  anti_join(CachexiaPats_AllrecordsORdx40perc) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  summarise(n=mean(value))  # 12.0

PONS_Measures %>%
    anti_join(CachexiaPats_AllrecordsORdx40perc) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkred",  colour="darkred") +
 xlim(2,20) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Hemoglobin Level, All patients (g/dL)")+
  ylab("Patient/Cohort proportion \n")



# Cachexia Pred

PONS_Measures %>%
  inner_join(CachexiaPats_AllrecordsORdx40perc) %>%
  anti_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  summarise(n=mean(value)) # 10

PONS_Measures %>%
  inner_join(CachexiaPats_AllrecordsORdx40perc) %>%
    anti_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkred",  colour="darkred") +
 xlim(2,20) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Hemoglobin Level, Cachexia Pred patients (g/dL)")+
  ylab("Patient/Cohort proportion \n")


# Cachexia Dx

PONS_Measures %>%
  inner_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  summarise(n=mean(value)) # 9

PONS_Measures %>%
  inner_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  #filter(claimed==max(claimed)) %>%
  filter(value==min(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkred",  colour="darkred") +
 xlim(2,20) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Hemoglobin Level, Cachexia Dx patients (g/dL)")+
  ylab("Patient/Cohort proportion \n")





CachexiaPats_AllrecordsORdx40perc <- fread("CachexiaPats_AllrecordsORdx40perc.txt")
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid) %>% distinct()
Cachexia_Dx <- Cachexia_Dx %>% inner_join(CachexiaPats_AllrecordsORdx40perc)


PONS_Measures <- fread("PONS Measures.txt", sep="\t")

unique(PONS_Measures$test)
# [1] "BMI"                "Body Height"        "Body Weight"        "Hemoglobin"        
# [5] "Albumin"            "C-Reactive Protein" "Cancer Stage"

PONS_Measures <- PONS_Measures %>% filter(test=="C-Reactive Protein")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)


# All Cancer
PONS_Measures %>%
  anti_join(CachexiaPats_AllrecordsORdx40perc) %>%
  group_by(patid) %>%
  filter(value==max(value)) %>%
  ungroup() %>%
  summarise(n=mean(value))  # 37.3

PONS_Measures %>%
    anti_join(CachexiaPats_AllrecordsORdx40perc) %>%
  group_by(patid) %>%
  filter(value==max(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkslategray",  colour="darkslategray") +
 xlim(0,200) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n CRP Level, All patients (mg/L)")+
  ylab("Patient/Cohort proportion \n")



# Cachexia Pred

PONS_Measures %>%
  inner_join(CachexiaPats_AllrecordsORdx40perc) %>%
  anti_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==max(value)) %>%
  ungroup() %>%
  summarise(n=mean(value)) # 54

PONS_Measures %>%
  inner_join(CachexiaPats_AllrecordsORdx40perc) %>%
    anti_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==max(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkslategray",  colour="darkslategray") +
  xlim(0,200) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n CRP Level, Cachexia Pred patients (mg/L)")+
  ylab("Patient/Cohort proportion \n")


# Cachexia Dx

PONS_Measures %>%
  inner_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==max(value)) %>%
  ungroup() %>%
  summarise(n=mean(value)) # 66

PONS_Measures %>%
  inner_join(Cachexia_Dx) %>%
  group_by(patid) %>%
  filter(value==max(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) + 
  geom_density(size=1.5, fill="darkslategray",  colour="darkslategray") +
  xlim(0,200) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n CRP Level, Cachexia Dx patients (mg/L)")+
  ylab("Patient/Cohort proportion \n")
# ---------------------------------------------
# Predict  Cacheixa VS no Cachexia + ICD10 + Labs NEW ---------------------------------------
# For model, from before with age gender mets bmi and icd10s already 
# Add Albumin + CRP + Hemoglobuin 

temp <- fread("temp_all_ICDs_QoL.txt", sep="\t")



CachexiaPats_AllrecordsORdx40perc <- fread("CachexiaPats_AllrecordsORdx40perc.txt")
CachexiaPats_AllrecordsORdx40perc$CachexiPred <- 1

temp <- temp %>% left_join(CachexiaPats_AllrecordsORdx40perc) %>% mutate(CachexiPred=ifelse(is.na(CachexiPred),0,CachexiPred))

PONS_Measures <- fread("PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="Albumin")
PONS_Measures <- PONS_Measures %>% select(patid, value)
PONS_Measures <- temp %>% select(patid) %>% left_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% ungroup()
PONS_Measures <- PONS_Measures %>% select(patid, value)
names(PONS_Measures)[2] <- "Albumin"
temp <- temp %>% left_join(PONS_Measures) %>% drop_na()


PONS_Measures <- fread("PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="Hemoglobin")
PONS_Measures <- PONS_Measures %>% select(patid, value)
PONS_Measures <- temp %>% select(patid) %>% left_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1) %>% ungroup()
PONS_Measures <- PONS_Measures %>% select(patid, value)
names(PONS_Measures)[2] <- "Hemoglobin"
temp <- temp %>% left_join(PONS_Measures) %>% drop_na()


PONS_Measures <- fread("PONS Measures.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="C-Reactive Protein")
PONS_Measures <- PONS_Measures %>% select(patid, value)
PONS_Measures <- temp %>% select(patid) %>% left_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% ungroup()
PONS_Measures <- PONS_Measures %>% select(patid, value)
names(PONS_Measures)[2] <- "CRP"
temp <- temp %>% left_join(PONS_Measures) %>% drop_na()

temp  %>% group_by(CachexiPred) %>% summarise(n=mean(CRP)) 
temp  %>% group_by(CachexiPred) %>% summarise(n=mean(Hemoglobin)) 
temp  %>% group_by(CachexiPred) %>% summarise(n=mean(Albumin)) 




temp %>% group_by(CachexiPred) %>% count() 
temp <- temp %>% group_by(CachexiPred) %>% sample_n(14618) %>% ungroup()
temp$CachexiPred <- as.factor(temp$CachexiPred)
temp <- temp %>% select(-patid)
temp <- temp %>% mutate(gender = ifelse(gender=="M", 1, 0))

modelAll_1_glm <- glm(CachexiPred ~ ., data = temp, family = binomial)
summary(modelAll_1_glm)


library("randomForest")
modelAll_1_randomForest <- randomForest(CachexiPred ~ ., data = temp)
summary(modelAll_1_randomForest)
RF_IMP <- modelAll_1_randomForest$importance
RF_IMP <- data.frame(RF_IMP)
RF_IMP %>% arrange(-MeanDecreaseGini)



library("gbm")
modelAll_1_gradientBoost <- gbm(CachexiPred==1 ~ ., data = temp, n.trees = 15000, distribution = "bernoulli")
summary(modelAll_1_gradientBoost)
# --------------------------
# Payer Mix Insurances -----------------------------------

PONS_Demographics <- fread("PONS Demographics.txt")

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")


data.frame(New_Primary_Cancer_Box %>% left_join(PONS_Demographics %>% select(patid, plan)) %>%
  group_by(Primary_Cancer, plan) %>% summarise(n=sum(weight)) %>%
  filter(Primary_Cancer!="-") %>%
  ungroup() %>%
  spread(key=plan, value=n))


data.frame(New_Primary_Cancer_Box %>% left_join(PONS_Demographics %>% select(patid, product )) %>%
  group_by(Primary_Cancer, product ) %>% summarise(n=sum(weight)) %>%
  filter(Primary_Cancer!="-") %>%
  ungroup() %>%
  spread(key=product , value=n))

# EPO	Exclusive provider organization
# GPO	Group purchasing organization
# HMO	Health maintenance organization
# IND	Indemnity
# IPP	Individual program plan
# OTH	Other
# POS	Point of service
# PPO	Preferred provider organization
# SPN	State policy network
# ---------------------------------------------------------------
# Drug Class penetrance ever and over time --------------------------------------------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)


CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
length(unique(CAN_Drug_Histories$patid)) # 293498
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[3] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)
length(unique(CAN_Drug_Histories$patid))
CancerDrug_Experienced <- CAN_Drug_Histories %>% filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15"|
                                drug_class=="Surgery Inpatient") %>% select(patid) %>% distinct() 


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>% inner_join(CAN_Drug_Histories) 
sum(CAN_Drug_Histories$weight)  # 9861087
data.frame(CAN_Drug_Histories %>% select(patid, weight, Primary_Cancer) %>% distinct() %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, Primary_Cancer, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)

names(CAN_Drug_Histories)[5] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)

data.frame(CAN_Drug_Histories %>% select(patid, weight, drug_class) %>% distinct() %>% 
  group_by(drug_class) %>% summarise(n=sum(weight)))

fwrite(CancerDrug_Experienced, "CancerDrug_Experienced.txt", sep="\t")

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset) %>% filter(!is.na(cachexia_onset))
CachexiaDx <- PONS_Demographics %>% select(patid)
CachexiaPred <- CachexiaPats_ALL_NEW %>% anti_join(CachexiaDx)

CAN_Drug_Histories %>% inner_join(CachexiaDx) %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 350361

data.frame(CAN_Drug_Histories %>% select(patid, weight, drug_class) %>% distinct() %>% 
             inner_join(CachexiaDx) %>%
  group_by(drug_class) %>% summarise(n=sum(weight)))

CAN_Drug_Histories %>% inner_join(CachexiaPred) %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 856140

data.frame(CAN_Drug_Histories %>% select(patid, weight, drug_class) %>% distinct() %>% 
             inner_join(CachexiaPred) %>%
  group_by(drug_class) %>% summarise(n=sum(weight)))



# Drug Class penetrance ever LUNG -------------------------------------------------------


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Lung Cancer") %>% select(patid)

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
New_Primary_Cancer_Box <- CancerDrug_Experienced %>% inner_join(New_Primary_Cancer_Box)

PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid) 

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(Cachexia_dx)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)


CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))

sum(CAN_Drug_Histories$weight) # 832818.5

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
length(unique(CAN_Drug_Histories$patid)) 
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)

data.frame(CAN_Drug_Histories %>% select(patid, weight, drug_class) %>% distinct() %>% 
  group_by(drug_class) %>% summarise(n=sum(weight)))


# -------------------------------------------------------------------------------------
# Lines of Therapy: Anticancer drugs only --------------------------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
New_Primary_Cancer_Box <- CancerDrug_Experienced %>% inner_join(New_Primary_Cancer_Box)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
names(CAN_Drug_Histories)[1] <- "patient"

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs,Drugs))

CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, weight, Month, Drugs) %>%
  group_by(patient, weight, Month) %>% mutate(treat_new = paste(Drugs, collapse=",")) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% ungroup() %>% select(patient, weight, Month, treat_new) %>% distinct()
noLines <- CAN_Drug_Histories


noLines <- noLines %>% spread(key=Month, value=treat_new)

fwrite(noLines, "Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")



# temp <- separate_rows(noLines, treat_new, sep = ",", convert=T)
# names(temp)[4] <- "molecule"
# temp$molecule <- as.character(temp$molecule)
# temp <- temp %>% left_join(PONS_Ingredients)
# 
# data.frame(temp %>% select(patient, weight, drug_class) %>% distinct() %>% 
#   group_by(drug_class) %>% summarise(n=sum(weight)))


temp <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
temp <- temp %>% filter(!is.na(Drugs)) %>% select(patient, weight, Drugs) %>% distinct()

temp %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 2.30

data.frame(temp %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(total=sum(weight)))


names(New_Primary_Cancer_Box)[1] <- "patient"


data.frame(temp %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  left_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(weight))) %>%
  mutate(n=ifelse(n>=10,10,n)) %>% ungroup() %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(total)) %>%
  ungroup() %>%
  spread(key=Primary_Cancer, value=total)


data.frame(data.frame(temp %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  left_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(weight))) %>%
  mutate(n=ifelse(n>=10,10,n)) %>% ungroup() %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(total)) %>%
  ungroup() %>% group_by(Primary_Cancer) %>% summarise(n=weighted.mean(n, total)))


noLines <- fread("Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

temp <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
temp <- temp %>% filter(!is.na(Drugs)) %>% select(patient, weight, Drugs) %>% distinct()

temp %>% inner_join(PONS_Demographics) %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 4.09
temp %>% anti_join(PONS_Demographics) %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 3.26
 

# -----------------------------------------------------------------------------------------------
# BMI over time: Before after Dx +- GDF15 ----------------------------------------------------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CancerDrug_Experienced) %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) 
sum(CAN_Drug_Histories$weight)  # 9861087
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(drug_class=="Nutrition"|drug_class=="Appetite Stimulant"|drug_class=="Cannabinoid"|drug_class=="Progestin"|drug_class=="Androgen"|drug_class=="Corticosteroid") 
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Nutrition="Nutrition")


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories) %>% mutate(Nutrition=ifelse(is.na(Nutrition),"No",Nutrition))


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(PONS_Demographics) 
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% drop_na()
New_Primary_Cancer_Box$cancer_onset <- as.Date(New_Primary_Cancer_Box$cancer_onset)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(PONS_Measures) %>% drop_na()
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(ElapsedTime=as.numeric(claimed-cancer_onset)/30.5)

data.frame(New_Primary_Cancer_Box %>% group_by(Nutrition, Primary_Cancer, patid) %>% filter(value==max(value)|value==min(value)) %>%
  ungroup() %>%
  select(Nutrition, Primary_Cancer, patid, value) %>% distinct() %>%
  group_by(patid) %>% mutate(diff=min(value)-max(value)) %>%
  select(patid, Primary_Cancer, Nutrition, diff) %>% distinct() %>% ungroup() %>%
  group_by(Primary_Cancer, Nutrition) %>% summarise(n=mean(diff)) %>%
  spread(key=Nutrition, value=n))


# New_Primary_Cancer_Box %>%
#   ggplot(aes(ElapsedTime, value, colour=GDF15, fill=GDF15)) +
#   geom_smooth() +
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   facet_wrap(~GDF15) +
#   xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
#   ylab("Locally weighted moving polynomial regression fit \n") +
#   ggsci::scale_colour_jco()+
#   ggsci::scale_fill_jco()


# ----------------------------------------------------------------------------
# Age of Cachexia Dx vs Cachexia Pred vs no cachexia ---------------------------------------------------------------

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia
Cachexia_Pred <- CachexiaPats_ALL_NEW %>% anti_join(PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid))
Cachexia_Pred <- Cachexia_Pred %>% left_join(PONS_Demographics %>% select(patid, weight))
Cachexia_Pred$Cacheixa <- "Pred"

temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T) # the ones with reads
All_NON_Cachexia <- temp_max %>% select(patid) %>% distinct() %>% anti_join(CachexiaPats_ALL_NEW)
All_NON_Cachexia <- All_NON_Cachexia %>% left_join(PONS_Demographics %>% select(patid, weight))
All_NON_Cachexia$Cacheixa <- "None"


PONS_Demographics <- fread("PONS Demographics.txt") # age, weights, and cachexia Dx
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, cachexia_onset)
PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, weight)
Cachexia_Dx$Cacheixa <- "Dx"


temp <- Cachexia_Pred %>% full_join(Cachexia_Dx) %>% full_join(All_NON_Cachexia)

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") %>% select(patid)


temp <- temp %>% filter(Cacheixa!="Dx") %>% full_join(temp %>% filter(Cacheixa=="Dx") %>% inner_join(Pats_to_track_BMI))


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age)

temp %>% left_join(PONS_Demographics) %>% mutate(age=ifelse(age<55,"<55",
                                                            ifelse(age>=55&age<65,"55-65",
                                                                   ifelse(age>=65&age<75,"65-75",
                                                                          ifelse(age>=75&age<85,"75-85",">85"))))) %>%
  group_by(Cacheixa, age) %>% summarise(n=sum(weight)) %>%
  spread(key=age, value=n)


temp %>% left_join(PONS_Demographics) %>% group_by(Cacheixa) %>% summarise(n=weighted.mean(age, weight))



temp <- temp %>% left_join(PONS_Demographics) %>% filter(Cacheixa!="None")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age)
PONS_Demographics <-PONS_Demographics %>% anti_join(temp %>% select(patid))
PONS_Demographics <- PONS_Demographics %>% mutate(Cacheixa="None") %>% select(1,2,4,3)

temp %>% full_join(PONS_Demographics)  %>% group_by(Cacheixa) %>% summarise(n=weighted.mean(age, weight))
 
temp %>% full_join(PONS_Demographics) %>% mutate(age=ifelse(age<55,"<55",
                                                            ifelse(age>=55&age<65,"55-65",
                                                                   ifelse(age>=65&age<75,"65-75",
                                                                          ifelse(age>=75&age<85,"75-85",">85"))))) %>%
  group_by(Cacheixa, age) %>% summarise(n=sum(weight)) %>%
  spread(key=age, value=n)



# -------------------------------------------------------------------------------------------------
# BMI over time before/after Nutrition support -------------------------------------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Nutrition <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Nutrition"|
                                                                     PONS_Ingredients$drug_class == "Appetite Stimulant"|
                                                                     PONS_Ingredients$drug_class == "Cannabinoid"|
                                                                     PONS_Ingredients$drug_class == "Progestin"|
                                                                     PONS_Ingredients$drug_class == "Androgen"], collapse = "|"),")\\b")

CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) 
sum(CAN_Drug_Histories$weight) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Drugs=ifelse(grepl(string_Nutrition, Drugs),1,0))
names(CAN_Drug_Histories)[4] <- "Nutrition"
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Min_Nutrition <- CAN_Drug_Histories %>% filter(Nutrition==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month)
names(Min_Nutrition)[2] <- "First_Nutrition"

CAN_Drug_Histories <- Min_Nutrition %>% select(patid) %>% left_join(CAN_Drug_Histories)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- Min_Nutrition %>% select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)
PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed))
PONS_Measures <- PONS_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))
PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()
PONS_Measures <- PONS_Measures %>% group_by(patid, Exact_Month) %>% summarise(n=mean(value))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Measures, by=c("patid"="patid", "Month"="Exact_Month"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(Min_Nutrition)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Month=Month-First_Nutrition)

CAN_Drug_Histories <- CAN_Drug_Histories %>% drop_na()

CAN_Drug_Histories %>%
  #group_by(Month, Nutrition) %>% summarise(n=mean(n)) %>%
  filter(Month>(-24)&Month<24) %>%
  ggplot(aes(Month, n)) +
  geom_smooth(fill="darkslategray4", colour="darkslategray", size=2, alpha=0.5) +
    theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Elapsed Time (months) to/from Nutritional Support Initiation") +
  ylab("Average Monthly BMI (kg/m2) \n")



# ----------------------------------------------------------------------------------------------------
# BMI over time before/after GDF15 support ---------------------------------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_GDF15 <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"], collapse = "|"),")\\b")

CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) 
sum(CAN_Drug_Histories$weight) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Drugs=ifelse(grepl(string_GDF15, Drugs),1,0))
names(CAN_Drug_Histories)[4] <- "GDF15"
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Min_GDF15 <- CAN_Drug_Histories %>% filter(GDF15==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month)
names(Min_GDF15)[2] <- "First_GDF15"

CAN_Drug_Histories <- Min_GDF15 %>% select(patid) %>% left_join(CAN_Drug_Histories)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- Min_GDF15 %>% select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)
PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed))
PONS_Measures <- PONS_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))
PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()
PONS_Measures <- PONS_Measures %>% group_by(patid, Exact_Month) %>% summarise(n=mean(value))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Measures, by=c("patid"="patid", "Month"="Exact_Month"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(Min_GDF15)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Month=Month-First_GDF15)

CAN_Drug_Histories <- CAN_Drug_Histories %>% drop_na()

CAN_Drug_Histories %>%
  #group_by(Month, Nutrition) %>% summarise(n=mean(n)) %>%
  mutate(Month=Month+5) %>%
  filter(Month>(-24)&Month<24) %>%
    ggplot(aes(Month, n)) +
  geom_smooth(fill="firebrick", colour="darkred", size=2, alpha=0.5) +
    theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Elapsed Time (months) to/from GDF15 Initiation") +
  ylab("Average Monthly BMI (kg/m2) \n")
# ----------------------------------------------------------------------------
# Cacheixa Dx vs Cachexia Pred - VEctor _ All Cachexia patients with any evidence ---------------------------------
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 
Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)
Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)



temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6) ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))


New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct()


CachexiaPats_ALL_NEW <- New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
             full_join(Pats_to_track_BMI %>% filter(!is.na(cachexia_onset))) %>%
             distinct() %>% select(patid) %>% distinct()

fwrite(CachexiaPats_ALL_NEW, "CachexiaPats_Ever_AllDrops.txt")

CachexiaPats_ALL_NEW
No_Cachexia <- temp_max %>% select(patid) %>% distinct() %>% anti_join(CachexiaPats_ALL_NEW)

PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid) %>% inner_join(CachexiaPats_ALL_NEW)
Cachexia_pred <- CachexiaPats_ALL_NEW %>% anti_join(Cachexia_Dx)
No_Cachexia



Cachexia_Dx %>% mutate(Cachexia="Dx") %>%
  full_join(Cachexia_pred %>% mutate(Cachexia="Pred")) %>%
  full_join(No_Cachexia %>% mutate(Cachexia="No")) %>%
  left_join(PONS_Demographics %>% select(patid, age, weight)) %>%
  mutate(age=ifelse(age<55,"<55",
                                                            ifelse(age>=55&age<65,"55-65",
                                                                   ifelse(age>=65&age<75,"65-75",
                                                                          ifelse(age>=75&age<85,"75-85",">85"))))) %>%
  group_by(Cachexia, age) %>% summarise(n=sum(weight)) %>%
  spread(key=age, value=n)


# ------------
# Age by primary cancer and metastasis or cachexia Dx state ---------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer") 

PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, age, cancer_metastasis)  %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

data.frame(New_Primary_Cancer_Box %>% left_join(PONS_Demographics_temp) %>%
  group_by(Primary_Cancer, cancer_metastasis) %>%
  summarise(n=weighted.mean(age, weight)) %>%
  spread(key=cancer_metastasis, value=n))



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer") 

PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, age, cachexia_onset, cancer_metastasis)  %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) %>%
   mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

data.frame(New_Primary_Cancer_Box %>% left_join(PONS_Demographics_temp) %>%
  group_by(Primary_Cancer, cachexia_onset, cancer_metastasis) %>%
    filter(cachexia_onset==1) %>%
  summarise(n=weighted.mean(age, weight)) %>%
  spread(key=cancer_metastasis, value=n))

# -----------------------------------------------------------------------------
# Time from Diagnoses to Treatment to Cachexia ----------------------------------------------------------
CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset, cachexia_onset)

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% inner_join(Pats_to_track_BMI %>% select(-c(weight, died, cancer_metastasis)))


# temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)
# 
# temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
# names(temp_max)[2] <- "Month_Max"
# names(temp_max)[3] <- "Max"
# temp_max$Month_Max <- as.numeric(temp_max$Month_Max)
# temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
# names(temp_min)[2] <- "Month_Min"
# names(temp_min)[3] <- "Min"
# temp_min$Month_Min <- as.numeric(temp_min$Month_Min)
# temp <- temp_max %>% left_join(temp_min)
# temp <- temp %>% ungroup() %>% filter(Month_Min>Month_Max)
# temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))
# temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
# temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20),1,0 ))
# 
# New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% group_by(patid) %>%
#   filter(Month_Min==min(Month_Min)) %>% slice(1) %>% select(patid, Month_Min) %>% distinct()
# names(New_Cachexia_Pred)[2] <- "First_Cachexia_Pred"
# 
# CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(New_Cachexia_Pred)

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% drop_na() %>% mutate(cancer_onset =as.character(cancer_onset ))
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% mutate(cancer_onset =str_sub(cancer_onset , 1L, 7L))

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% mutate(cachexia_onset  =as.character(cachexia_onset  ))
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% mutate(cachexia_onset  =str_sub(cachexia_onset  , 1L, 7L))

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(Months_lookup, by=c("cancer_onset"="Month")) 
names(CachexiaPats_ALL_NEW)[5] <- "First_Cancer_Onset"
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% select(-cancer_onset)

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(Months_lookup, by=c("cachexia_onset"="Month")) 
names(CachexiaPats_ALL_NEW)[5] <- "First_Cachexia_Onset"
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% select(-cachexia_onset )

# CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% mutate(First_Cachexia=ifelse(is.na(First_Cachexia_Pred),First_Cachexia_Onset,
#                                                       ifelse(is.na(First_Cachexia_Onset), First_Cachexia_Pred, NA))) %>%
#   mutate(First_Cachexia=ifelse(is.na(First_Cachexia),First_Cachexia_Onset, First_Cachexia)) %>%
#   select(patid, First_Cancer_Onset, First_Cachexia)
  
mean(CachexiaPats_ALL_NEW$First_Cachexia)
mean(CachexiaPats_ALL_NEW$First_Cancer_Onset)


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

CAN_Drug_Histories <- CachexiaPats_ALL_NEW %>% select(patid) %>% left_join(CAN_Drug_Histories, by=c("patid"="patient"))
sum(CAN_Drug_Histories$weight) # 1603718

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[3] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)
length(unique(CAN_Drug_Histories$patid))

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15"|
                                drug_class=="Surgery Inpatient") %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  slice(1) %>% select(patid, Month) %>% distinct()

names(CAN_Drug_Histories)[2] <- "First_Rx"

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(CAN_Drug_Histories) 

mean(CachexiaPats_ALL_NEW$First_Cancer_Onset)
mean(CachexiaPats_ALL_NEW$First_Rx, na.rm = T)
mean(CachexiaPats_ALL_NEW$First_Cachexia)

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% 
  mutate(Dx_to_Rx=First_Rx-First_Cancer_Onset) %>% 
  mutate(Dx_to_Cachexia=First_Cachexia_Onset -First_Cancer_Onset) 

data.frame(CachexiaPats_ALL_NEW %>% group_by(diagnosis) %>%  summarise(n=mean(Dx_to_Rx, na.rm=T)))
data.frame(CachexiaPats_ALL_NEW %>%  group_by(diagnosis) %>% summarise(n=mean(Dx_to_Cachexia, na.rm=T)))

mean(CachexiaPats_ALL_NEW$Dx_to_Rx, na.rm=T)
mean(CachexiaPats_ALL_NEW$Dx_to_Cachexia, na.rm=T)

CachexiaPats_ALL_NEW %>% select(patid, First_Cachexia) %>% mutate(Elapsed=60-First_Cachexia) %>%
  mutate(Elapsed=-1*Elapsed) %>%
  ggplot(aes(Elapsed)) + 
  geom_density(colour="darkslategrey", fill="darkslategrey", size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Time Since Cachexia Diagnosis") + ylab("Proportion \n")



# Time Since First Cachexia Dx

PONS_Dossiers <- fread("PONS Dossiers.txt")

PONS_Dossiers %>% filter(condition=="Cachexia") %>% inner_join(CachexiaPats_ALL_NEW %>% select(patid)) %>%
  select(patid, earliest) %>% mutate(earliest=as.Date(earliest)) %>%
  mutate(Elapsed=as.numeric(as.Date("2021-07-31")-earliest)/30.5) %>%
  mutate(Elapsed=-1*Elapsed) %>%
  ggplot(aes(Elapsed)) + 
  geom_density(colour="darkslategrey", fill="darkslategrey", size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Time Since Cachexia Diagnosis") + ylab("Proportion \n")


# First to Last Cachexia Predicted

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")


temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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
temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12),1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20),1,0 ))

New_Cachexia_Pred <- temp %>% filter(Drop95==1 | Drop90==1 | Drop2_20==1) %>% group_by(patid) %>%
  filter(Month_Min==min(Month_Min)|Month_Min==max(Month_Min)) %>%  select(patid, Month_Min) %>% distinct() %>%
  mutate(Month=row_number()) %>% spread(key=Month, value=Month_Min) 

New_Cachexia_Pred[is.na(New_Cachexia_Pred)] <- 0

mean(New_Cachexia_Pred$`2`-New_Cachexia_Pred$`1`)

New_Cachexia_Pred %>% mutate(TimeONCachexia=`2`-`1`) %>%
  filter(TimeONCachexia>0) %>% ungroup() %>%
  #filter(TimeONCachexia<(30)) %>%
  ggplot(aes(TimeONCachexia)) +
  geom_density(colour="darkslategrey", fill="darkslategrey", size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Time ON Cachexia") + ylab("Proportion \n")


New_Cachexia_Pred %>% mutate(TimeONCachexia=`2`-`1`) %>%
  filter(TimeONCachexia>0) %>% ungroup() %>% mutate(TimeONCachexia=ifelse(TimeONCachexia<=12,1,
                                                                          ifelse(TimeONCachexia<=24,2,
                                                                                 ifelse(TimeONCachexia<=36,3,
                                                                                        ifelse(TimeONCachexia<=48,4,
                                                                                          ifelse(TimeONCachexia<=60,5,6)))))) %>% 
  group_by(TimeONCachexia) %>% count()



# ---------------------------------------------------------------------------
# Dxed Last Year vs Treated Last Year with Cancer drugs -------------------------------------------------
CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

CAN_Drug_Histories <- Pats_to_track_BMI %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
length(unique(CAN_Drug_Histories$patid))

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[3] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)
length(unique(CAN_Drug_Histories$patid))


Rx_LastYear <- CAN_Drug_Histories %>% filter(Month>=49) %>% filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15"|
                                drug_class=="Surgery Inpatient") %>% select(patid) %>% distinct()



PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
Dx_LastYear <- PONS_Demographics %>% group_by(patid) %>% filter(Exact_Month==48) %>% filter(Status=="Naive") %>% select(patid)

Rx_LastYear %>% inner_join(Pats_to_track_BMI %>% select(patid, weight, diagnosis)) %>% ungroup() %>% summarise(n=sum(weight)) # 4151912
Dx_LastYear %>% inner_join(Pats_to_track_BMI %>% select(patid, weight, diagnosis)) %>% ungroup() %>% summarise(n=sum(weight)) # 1835857
Dx_LastYear %>% inner_join(Pats_to_track_BMI %>% select(patid, weight, diagnosis)) %>% ungroup() %>% inner_join(Rx_LastYear) %>% summarise(n=sum(weight)) # 1835857


Rx_Ever <- CAN_Drug_Histories %>%  filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15"|
                                drug_class=="Surgery Inpatient") %>% select(patid) %>% distinct()


Rx_Ever %>% inner_join(Pats_to_track_BMI %>% select(patid, weight, diagnosis)) %>% ungroup() %>% summarise(n=sum(weight)) # 9861087

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% filter(!is.na(cancer_metastasis))
Mets_Ever <- PONS_Demographics %>% select(patid) %>% inner_join(Pats_to_track_BMI)

Mets_Ever %>% inner_join(Pats_to_track_BMI %>% select(patid, weight, diagnosis)) %>% ungroup() %>% summarise(n=sum(weight)) # 9026280

Mets_Ever %>% inner_join(Pats_to_track_BMI %>% select(patid, weight, diagnosis)) %>% ungroup() %>% inner_join(Rx_Ever) %>% summarise(n=sum(weight)) # 9026280





# -----------------


PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- PONS_Demographics %>% select(patid, Exact_Month, Status)

Dx_LastYear <- PONS_Demographics %>% group_by(patid) %>% filter(Exact_Month==48) %>% filter(Status=="Naive") %>% select(patid)
Dx_Last2Years <- PONS_Demographics %>% group_by(patid) %>% filter(Exact_Month==36) %>% filter(Status=="Naive") %>% select(patid)
Dx_Last2Years <- Dx_Last2Years %>% anti_join(Dx_LastYear)
Dx_Last3Years <- PONS_Demographics %>% group_by(patid) %>% filter(Exact_Month==24) %>% filter(Status=="Naive") %>% select(patid)
Dx_Last3Years <- Dx_Last3Years %>% anti_join(Dx_LastYear)  %>% anti_join(Dx_Last2Years)
Dx_Last4Years <- PONS_Demographics %>% group_by(patid) %>% filter(Exact_Month==12) %>% filter(Status=="Naive") %>% select(patid)
Dx_Last4Years <- Dx_Last4Years %>% anti_join(Dx_LastYear)  %>% anti_join(Dx_Last2Years) %>% anti_join(Dx_Last3Years)
Dx_Last5Years <- PONS_Demographics %>% anti_join(Dx_LastYear) %>% anti_join(Dx_Last2Years) %>% anti_join(Dx_Last3Years) %>% anti_join(Dx_Last4Years) 
Dx_Last5Years <- Dx_Last5Years %>% select(patid) %>% distinct()



# Diagnosed each year vs cachexia predicted ---------------------

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)
names(PONS_Demographics)[4] <- "diagnosis"
Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)
Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis!="-") 

sum(Pats_to_track_BMI$weight) # 22984889

data.frame(Pats_to_track_BMI %>%  ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))


Pats_to_track_BMI %>% inner_join(Dx_LastYear) %>% summarise(n=sum(weight)) # Total Dx 1835857
data.frame(Pats_to_track_BMI %>% inner_join(Dx_LastYear) %>% ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)) # Final Cachexia = 203759


Pats_to_track_BMI %>% inner_join(Dx_Last2Years) %>% summarise(n=sum(weight)) # Total Dx 2054067
data.frame(Pats_to_track_BMI %>% inner_join(Dx_Last2Years) %>% ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)) # Final Cachexia = 238381

Pats_to_track_BMI %>% inner_join(Dx_Last3Years) %>% summarise(n=sum(weight)) # Total Dx 2777753
data.frame(Pats_to_track_BMI %>% inner_join(Dx_Last3Years) %>% ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)) # Final Cachexia = 328409

Pats_to_track_BMI %>% inner_join(Dx_Last4Years) %>% summarise(n=sum(weight)) # Total Dx 3718205
data.frame(Pats_to_track_BMI %>% inner_join(Dx_Last4Years) %>% ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)) # Final Cachexia = 440075


Pats_to_track_BMI %>% inner_join(Dx_Last5Years) %>% summarise(n=sum(weight)) # Total Dx 12599007
data.frame(Pats_to_track_BMI %>% inner_join(Dx_Last5Years) %>% ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)) # Final Cachexia = 1410374






# ---------------------------------------------------
# Ever treated (+/- with cancer drugs) among those "integrated" -------------------------

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-") 
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, has_integrated)
New_Primary_Cancer_Box <- PONS_Demographics %>% filter(has_integrated=="N") %>% inner_join(New_Primary_Cancer_Box)


CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
length(unique(CAN_Drug_Histories$patid)) # 338758


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
length(unique(CAN_Drug_Histories$patid)) # 293498


CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[3] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)
length(unique(CAN_Drug_Histories$patid))

CAN_Drug_Histories %>% filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15"|
                                drug_class=="Surgery Inpatient") %>% select(patid) %>% distinct() #147489

# ---------------------------------------------------------------

# Excel for Nancy ------------------------------------------------------------

 <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, gender)

CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics) %>% summarise(n=sum(weight))

# Gender / Age buckets

CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight))

CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics) %>%
  mutate(age=ifelse(age<55,"<55",
                                                            ifelse(age>=55&age<65,"55-65",
                                                                   ifelse(age>=65&age<75,"65-75",
                                                                          ifelse(age>=75&age<85,"75-85",">85"))))) %>%
  group_by(age) %>% summarise(n=sum(weight))


# BMI buckets
PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CachexiaPats_ALL_NEW %>% left_join(PONS_Measures) %>% select(patid, claimed, value)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% group_by(patid) %>% filter(value==min(value)) %>% slice(1)

PONS_Measures %>% mutate(value=ifelse(value<20,"<20",
                                      ifelse(value>=20&value<27,"20-27",
                                             ifelse(value>=27&value<30,"27-30",">30")))) %>%
  ungroup() %>% left_join(PONS_Demographics) %>% group_by(value) %>%
  summarise(n=sum(weight))


# BMI drops

temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp <- temp %>% inner_join(CachexiaPats_ALL_NEW)

length(unique(temp$patid)) # 50578

temp <- temp %>% filter(Month_Min>=55)


temp <- temp %>% mutate(Less2=ifelse( (Min<(Max)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))
temp <- temp %>% mutate(More2=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))
temp <- temp %>% mutate(More5=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))
temp <- temp %>% mutate(More10=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6),1,0 ))
temp <- temp %>% mutate(More15=ifelse( (Min<(Max*0.85)) & (Month_Min>Month_Max)  & (Month_Min-Month_Max<=6),1,0 ))

temp <- temp %>% mutate(Drop=ifelse(More15==1,"More15",
                            ifelse(More10==1,"More10",
                                   ifelse(More5==1,"More5",
                                          ifelse(More2==1,"More2",
                                                 ifelse(Less2==1,"Less2","None")))))) %>%
  select(patid, Drop) %>% distinct()


temp %>% mutate(rank=ifelse(Drop=="More15",1,
                            ifelse(Drop=="More10",2,
                                   ifelse(Drop=="More5",3,
                                          ifelse(Drop=="More2",4,5))))) %>%
  select(patid, rank) %>% group_by(patid) %>% filter(rank==min(rank)) %>% slice(1) %>%
  left_join(PONS_Demographics %>% select(patid, weight)) %>% ungroup() %>% group_by(rank) %>%
  summarise(n=sum(weight))


# % With Radiation / Surgery alst 3 months

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CancerDrug_Experienced %>% inner_join(CachexiaPats_ALL_NEW)


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
CAN_Drug_Histories <- CancerDrug_Experienced %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) 
sum(CAN_Drug_Histories$weight)  # 939321.4
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month58:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month58:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, drug_class) %>% distinct() %>% ungroup()

CAN_Drug_Histories %>% filter(drug_class=="Surgery Inpatient") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))

CAN_Drug_Histories %>% filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))


# Diagnoses / Comorbidities

PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[3] <- "ICD"

PONS_Comorbidity_Inventories <- CachexiaPats_ALL_NEW %>% inner_join(PONS_Comorbidity_Inventories)
length(unique(PONS_Comorbidity_Inventories$patid))

PONS_Comorbidity_Inventories %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 1603718

PONS_Comorbidity_Inventories %>% filter(grepl("R64", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 126953.2
PONS_Comorbidity_Inventories %>% filter(grepl("I1", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 1298802
PONS_Comorbidity_Inventories %>% filter(grepl("E11", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 649405.7
PONS_Comorbidity_Inventories %>% filter(grepl("E66", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 697955.1
PONS_Comorbidity_Inventories %>% filter(grepl("N0", ICD)|grepl("N1", ICD)|grepl("N2", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 969277
PONS_Comorbidity_Inventories %>% filter(grepl("K7", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 502440.1
PONS_Comorbidity_Inventories %>% filter(grepl("I6", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 472331.2
PONS_Comorbidity_Inventories %>% filter(grepl("I2", ICD)|grepl("I3", ICD)|grepl("I4", ICD)|grepl("I5", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 1141223
PONS_Comorbidity_Inventories %>% filter(grepl("I2", ICD)|grepl("I3", ICD)|grepl("I4", ICD)|grepl("I5", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 1141223
PONS_Comorbidity_Inventories %>% filter(grepl("D5", ICD)|grepl("D6", ICD)|grepl("R1", ICD)|grepl("R4", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 1141223
PONS_Comorbidity_Inventories %>% filter(grepl("M62", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 558905.6
PONS_Comorbidity_Inventories %>% filter(grepl("E4", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 296075.6
PONS_Comorbidity_Inventories %>% filter(grepl("R19", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 663659
PONS_Comorbidity_Inventories %>% filter(grepl("F32", ICD)|grepl("F33", ICD)|grepl("F34", ICD)|grepl("F39", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 562574.9
PONS_Comorbidity_Inventories %>% filter(grepl("R54", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 48491.28
PONS_Comorbidity_Inventories %>% filter(grepl("R52", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 255956.8
PONS_Comorbidity_Inventories %>% filter(grepl("R53", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 1002842
PONS_Comorbidity_Inventories %>% filter(grepl("R11", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 614609.1
PONS_Comorbidity_Inventories %>% filter(grepl("R63", ICD)) %>% select(patid) %>% distinct() %>% left_join(PONS_Demographics %>% select(patid, weight)) %>% summarise(n=sum(weight)) # 491458.9



# Treatments


CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CancerDrug_Experienced %>% inner_join(CachexiaPats_ALL_NEW)


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
CAN_Drug_Histories <- CancerDrug_Experienced %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) 
sum(CAN_Drug_Histories$weight)  # 939321.4
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, drug_class) %>% distinct() %>% ungroup()

CAN_Drug_Histories %>% filter(drug_class=="Nutrition"|drug_class=="Appetite Stimulant"|drug_class=="Cannabinoid"|drug_class=="Progestin"|drug_class=="Androgen") %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))

 
 PONS_Ingredients <- fread("PONS Ingredients.txt")

 
 
 # ---------
# Compare Cancer Drug-experienced vs not experienced ------------------------------------------
   
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))


CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CAN_Drug_Histories %>% select(patient) %>% inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
NO_CancerDrug_Experienced <- CAN_Drug_Histories %>% select(patient) %>% anti_join(CancerDrug_Experienced)
CancerDrug_Experienced$Group <- "CancerDrugs"
NO_CancerDrug_Experienced$Group <- "NoCancerDrugs"

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, plan, product, has_idn, has_notes, has_integrated, has_confinement, has_lab_results, has_tests, has_oncology)
names(PONS_Demographics)[1] <- "patient"


PONS_Demographics <- CancerDrug_Experienced %>% bind_rows(NO_CancerDrug_Experienced) %>% left_join(PONS_Demographics)


PONS_Demographics %>% group_by(Group, plan) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)


PONS_Demographics %>% group_by(Group, product) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL) %>%
  ungroup() %>% select(Group, product, Pen) %>% spread(key=Group, value=Pen)



PONS_Demographics %>% group_by(Group, has_idn) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)



PONS_Demographics %>% group_by(Group, has_notes) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)



PONS_Demographics %>% group_by(Group, has_integrated) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)


PONS_Demographics %>% group_by(Group, has_confinement) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)



PONS_Demographics %>% group_by(Group, has_lab_results) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)



PONS_Demographics %>% group_by(Group, has_tests) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)


PONS_Demographics %>% group_by(Group, has_oncology) %>% summarise(n=sum(weight)) %>% ungroup() %>% group_by(Group) %>% mutate(TOTAL=sum(n)) %>% mutate(Pen=n/TOTAL)


# -----------------------
# Persistency GDF15 -------------------------------------------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_GDF15        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_GDF15,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 3.898653


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_GDF15_Class.csv", sep=",")


# Persistency Radiotherapy -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Radiotherapy        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Radiotherapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Radiotherapy,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 2.622397


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_string_Radiotherapy_Class.csv", sep=",")


# Persistency Chemotherapy  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Chemotherapy        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Chemotherapy,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 15.24451


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_string_Chemotherapy_Class.csv", sep=",")


# Persistency Chemotherapy  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Chemotherapy        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Chemotherapy,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 15.24451


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_string_Chemotherapy_Class.csv", sep=",")



# Persistency Death  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Death        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Death"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Death,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 24.04097


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Death_Class.csv", sep=",")



# Persistency Appetite Stimulant  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Appetite_Stimulant        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Appetite Stimulant"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Appetite_Stimulant,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 4.868118


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Appetite_Stimulant_Class.csv", sep=",")



# Persistency Nutrition  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Nutrition        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Nutrition"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Nutrition,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 2.425715


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Nutrition_Class.csv", sep=",")

# Persistency Antiemetic  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Antiemetic        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Antiemetic"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Antiemetic,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 6.305689


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Antiemetic_Class.csv", sep=",")
# Persistency Chemoprotective  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Chemoprotective        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemoprotective"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Chemoprotective,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 5.52936


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Chemoprotective_Class.csv", sep=",")
# Persistency Biologic Therapy  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Biologic_Therapy        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Biologic Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Biologic_Therapy,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 8.531532


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Biologic_Therapy_Class.csv", sep=",")

# Persistency Cannabinoid  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Cannabinoid        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Cannabinoid"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cannabinoid,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 2.901135


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Cannabinoid_Class.csv", sep=",")

# Persistency Progestin  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Progestin        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Progestin"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Progestin,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 5.460656


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Progestin_Class.csv", sep=",")

# Persistency Androgen  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Androgen        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Androgen"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Androgen,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 17.00625


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Androgen_Class.csv", sep=",")


# Persistency Corticosteroid  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Corticosteroid        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Corticosteroid"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Corticosteroid,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 6.72938


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Corticosteroid_Class.csv", sep=",")



# Persistency Hospital Inpatient  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Hospital_Inpatient        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Hospital Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Hospital_Inpatient,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 2.086956


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Hospital_Inpatient_Class.csv", sep=",")

# Persistency Surgery Inpatient  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Surgery_Inpatient        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Surgery_Inpatient,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 1.213733


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(
  CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(min=min(Month)) 
) %>% ungroup() %>% mutate(Visibility=60-min+1) %>%
  group_by(Visibility, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=Visibility, value=total)

temp[is.na(temp)] <- 0

fwrite(temp, "Total_Persistency_Surgery_Inpatient_Class.csv", sep=",")
# ------------


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_GDF15        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"], collapse = "|"),")\\b")
string_Appetite_Stimulant        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Appetite Stimulant"], collapse = "|"),")\\b")

First_GDF15 <- CAN_Drug_Histories %>% filter(grepl(string_GDF15,Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month)
First_AppetiteStimulant <- CAN_Drug_Histories %>% filter(grepl(string_Appetite_Stimulant,Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, weight, Month)

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
names(PONS_Demographics)[1] <- "patient"

First_GDF15 <- First_GDF15 %>% left_join(PONS_Demographics) %>% drop_na()
First_AppetiteStimulant <- First_AppetiteStimulant %>% left_join(PONS_Demographics) %>% drop_na()

First_GDF15 <- First_GDF15 %>% mutate(cachexia_onset=as.character(cachexia_onset))
First_GDF15 <- First_GDF15 %>% mutate(cachexia_onset=str_sub(cachexia_onset, 1L, 7L))

First_AppetiteStimulant <- First_AppetiteStimulant %>% mutate(cachexia_onset=as.character(cachexia_onset))
First_AppetiteStimulant <- First_AppetiteStimulant %>% mutate(cachexia_onset=str_sub(cachexia_onset, 1L, 7L))


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

First_GDF15 %>% left_join(Months_lookup, by=c("cachexia_onset"="Month")) %>% mutate(Diff=Exact_Month-Month) %>% ungroup() %>% summarise(n=weighted.mean(Diff, weight))
First_AppetiteStimulant %>% left_join(Months_lookup, by=c("cachexia_onset"="Month")) %>% mutate(Diff=Exact_Month-Month) %>% ungroup() %>% summarise(n=weighted.mean(Diff, weight))

# Persistency Chemotherapy Per Primary Cancer  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Chemotherapy        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Chemotherapy,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 15.24451


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  group_by(diagnosis, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=diagnosis, value=total)

temp[is.na(temp)] <- 0


fwrite(temp, "Total_Persistency_string_Chemotherapy_Class_PrimaryCancer.csv", sep=",")




# Persistency GDF15 Per Primary Cancer  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_GDF15        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_GDF15,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 15.24451


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  group_by(diagnosis, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=diagnosis, value=total)

temp[is.na(temp)] <- 0


fwrite(temp, "Total_Persistency_string_GDF15_Class_PrimaryCancer.csv", sep=",")





# Persistency Appetite Stimulant Per Primary Cancer  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_AppetiteStimulant        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Appetite Stimulant"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_AppetiteStimulant,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 15.24451


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
) %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  group_by(diagnosis, n) %>% summarise(total=sum(weight)) %>%
  ungroup() %>%
  spread(key=diagnosis, value=total)

temp[is.na(temp)] <- 0


fwrite(temp, "Total_Persistency_string_AppetiteStimulant_Class_PrimaryCancer.csv", sep=",")



# -------------------------
# Function competing risks event incidence ---------------------------



"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}

# ----------------------------------------------------------

# Persistency Chemotherapy Cachexia vs No Cachexia  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Chemotherapy        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Chemotherapy,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()


 fittrial

# ----------------------------------------
# Persistency GFD15 Cachexia vs No Cachexia  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_GDF15        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_GDF15,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

# -------------------------------------


# Persistency Appetite Stimulants Cachexia vs No Cachexia  -------------------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Appetite_Stimulant        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Appetite Stimulant"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Appetite_Stimulant,Treat))

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()



# -------------------------
# Persistency Chemotherapy by Primary Cancer type -------------------------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Chemotherapy        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Chemotherapy,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1

trial <- trial %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) 

names(trial)[4] <- "dis"

fittrial <- CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[1:22,]

trial_transformed <- transpose(data.frame(fittrial$est)[1:22,])
names(trial_transformed)[1] <- "Bone"
names(trial_transformed)[2] <- "Brain"
names(trial_transformed)[3] <- "Breast"
names(trial_transformed)[4] <- "Gastro"
names(trial_transformed)[5] <- "Head"
names(trial_transformed)[6] <- "Intestinal"
names(trial_transformed)[7] <- "Kidney"
names(trial_transformed)[8] <- "Leukemia"
names(trial_transformed)[9] <- "Liver"
names(trial_transformed)[10] <- "Lung"
names(trial_transformed)[11] <- "Lymphoma"
names(trial_transformed)[12] <- "Myeloma"
names(trial_transformed)[13] <- "Other"
names(trial_transformed)[14] <- "Pancreatic"
names(trial_transformed)[15] <- "Prostate"
names(trial_transformed)[16] <- "Reproductive"
names(trial_transformed)[17] <- "Respiratory"
names(trial_transformed)[18] <- "Salivary"
names(trial_transformed)[19] <- "Skin"
names(trial_transformed)[20] <- "Thyroid"
names(trial_transformed)[21] <- "Unspecified"
names(trial_transformed)[22] <- "Urinary"

trial2 <- trial_transformed %>% gather(Group, Prop, Bone:Urinary) 

trial2$Prop <- 1- trial2$Prop


vector <- trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==max(Follow_up_months)) %>%
  arrange(Prop) %>% select(Group) %>% distinct() %>% ungroup()

vector <- as.vector(paste0(vector$Group, collapse = ", "))


trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==max(Follow_up_months)) %>%
  arrange(Prop) %>%
  select(Group) %>%
  left_join(trial2) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(Group=as.factor(Group)) %>%
  mutate(Group=fct_relevel(Group,"Pancreatic","Liver","Bone","Gastro","Lung","Brain","Myeloma","Leukemia","Other","Respiratory","Kidney","Prostate","Reproductive", "Intestinal", "Head", "Salivary", "Lymphoma", "Skin", "Urinary", "Breast", "Thyroid", "Unspecified")) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_smooth(size=1, se = F) +
  ylim(0,100)+
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  scale_colour_viridis_d(option = "A")+
  theme_minimal()


# ---------------------------------------------------------------------------
# Persistency GDF15 by Primary Cancer type --------------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_GDF15        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_GDF15,Treat)) 

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1

trial <- trial %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) 

names(trial)[4] <- "dis"

fittrial <- CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)



trial_transformed <- transpose(data.frame(fittrial$est)[1:22,])
names(trial_transformed)[1] <- "Bone"
names(trial_transformed)[2] <- "Brain"
names(trial_transformed)[3] <- "Breast"
names(trial_transformed)[4] <- "Gastro"
names(trial_transformed)[5] <- "Head"
names(trial_transformed)[6] <- "Intestinal"
names(trial_transformed)[7] <- "Kidney"
names(trial_transformed)[8] <- "Leukemia"
names(trial_transformed)[9] <- "Liver"
names(trial_transformed)[10] <- "Lung"
names(trial_transformed)[11] <- "Lymphoma"
names(trial_transformed)[12] <- "Myeloma"
names(trial_transformed)[13] <- "Other"
names(trial_transformed)[14] <- "Pancreatic"
names(trial_transformed)[15] <- "Prostate"
names(trial_transformed)[16] <- "Reproductive"
names(trial_transformed)[17] <- "Respiratory"
names(trial_transformed)[18] <- "Salivary"
names(trial_transformed)[19] <- "Skin"
names(trial_transformed)[20] <- "Thyroid"
names(trial_transformed)[21] <- "Unspecified"
names(trial_transformed)[22] <- "Urinary"

trial2 <- trial_transformed %>% gather(Group, Prop, Bone:Urinary) 

trial2$Prop <- 1- trial2$Prop


vector <- trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==15) %>%
  arrange(Prop) %>% select(Group) %>% distinct() %>% ungroup()

vector <- as.vector(paste0(vector$Group, collapse = ", "))


trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==max(Follow_up_months)) %>%
  arrange(Prop) %>%
  select(Group) %>%
  left_join(trial2) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(Group=as.factor(Group)) %>%
  mutate(Group=fct_relevel(Group,"Liver", "Pancreatic", "Brain", "Bone", "Gastro", "Skin", "Prostate", "Lung", "Myeloma", "Other", "Urinary", "Kidney", "Leukemia", "Respiratory", "Lymphoma", "Head", "Reproductive", "Intestinal", "Unspecified", "Breast", "Thyroid", "Salivary")) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_line(size=1) +
  ylim(0,100)+
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  scale_colour_viridis_d(option = "A")+
  theme_minimal()


# ---------------------------------------------------------------------------------------
# Persistency Appetite Stimulant by Primary Cancer type --------------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

string_Appetite_Stimulant        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Appetite Stimulant"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Appetite_Stimulant,Treat)) 

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1

trial <- trial %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) 

names(trial)[4] <- "dis"

fittrial <- CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)



trial_transformed <- transpose(data.frame(fittrial$est)[23:44,])
names(trial_transformed)[1] <- "Bone"
names(trial_transformed)[2] <- "Brain"
names(trial_transformed)[3] <- "Breast"
names(trial_transformed)[4] <- "Gastro"
names(trial_transformed)[5] <- "Head"
names(trial_transformed)[6] <- "Intestinal"
names(trial_transformed)[7] <- "Kidney"
names(trial_transformed)[8] <- "Leukemia"
names(trial_transformed)[9] <- "Liver"
names(trial_transformed)[10] <- "Lung"
names(trial_transformed)[11] <- "Lymphoma"
names(trial_transformed)[12] <- "Myeloma"
names(trial_transformed)[13] <- "Other"
names(trial_transformed)[14] <- "Pancreatic"
names(trial_transformed)[15] <- "Prostate"
names(trial_transformed)[16] <- "Reproductive"
names(trial_transformed)[17] <- "Respiratory"
names(trial_transformed)[18] <- "Salivary"
names(trial_transformed)[19] <- "Skin"
names(trial_transformed)[20] <- "Thyroid"
names(trial_transformed)[21] <- "Unspecified"
names(trial_transformed)[22] <- "Urinary"

trial2 <- trial_transformed %>% gather(Group, Prop, Bone:Urinary) 

trial2$Prop <- 1- trial2$Prop


vector <- trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==30) %>%
  arrange(Prop) %>% select(Group) %>% distinct() %>% ungroup()

vector <- as.vector(paste0(vector$Group, collapse = ", "))


trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==max(Follow_up_months)) %>%
  arrange(Prop) %>%
  select(Group) %>%
  left_join(trial2) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(Group=as.factor(Group)) %>%
  mutate(Group=fct_relevel(Group,"Lymphoma", "Brain", "Leukemia", "Breast", "Thyroid", "Reproductive", "Kidney", "Head", "Prostate", "Respiratory", "Myeloma", "Skin", "Intestinal", "Lung", "Pancreatic", "Bone", "Gastro", "Liver", "Other", "Salivary", "Unspecified", "Urinary")) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_line(size=1) +
  ylim(0,100)+
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  scale_colour_viridis_d(option = "D")+
  theme_minimal()


# ---------------
# Persistency Platinum drugs  Per Primary Cancer ------------------------------------------------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}

# Platinum

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")



PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Platinum        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Platinum agent"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Platinum,Treat)) 

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1

trial <- trial %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) 

names(trial)[4] <- "dis"

fittrial <- CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)



trial_transformed <- transpose(data.frame(fittrial$est)[23:44,])
names(trial_transformed)[1] <- "Bone"
names(trial_transformed)[2] <- "Brain"
names(trial_transformed)[3] <- "Breast"
names(trial_transformed)[4] <- "Gastro"
names(trial_transformed)[5] <- "Head"
names(trial_transformed)[6] <- "Intestinal"
names(trial_transformed)[7] <- "Kidney"
names(trial_transformed)[8] <- "Leukemia"
names(trial_transformed)[9] <- "Liver"
names(trial_transformed)[10] <- "Lung"
names(trial_transformed)[11] <- "Lymphoma"
names(trial_transformed)[12] <- "Myeloma"
names(trial_transformed)[13] <- "Other"
names(trial_transformed)[14] <- "Pancreatic"
names(trial_transformed)[15] <- "Prostate"
names(trial_transformed)[16] <- "Reproductive"
names(trial_transformed)[17] <- "Respiratory"
names(trial_transformed)[18] <- "Salivary"
names(trial_transformed)[19] <- "Skin"
names(trial_transformed)[20] <- "Thyroid"
names(trial_transformed)[21] <- "Unspecified"
names(trial_transformed)[22] <- "Urinary"

trial2 <- trial_transformed %>% gather(Group, Prop, Bone:Urinary) 

trial2$Prop <- 1- trial2$Prop


vector <- trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==15) %>%
  arrange(Prop) %>% select(Group) %>% distinct() %>% ungroup()

vector <- as.vector(paste0(vector$Group, collapse = ", "))


trial2 %>% 
  filter(Group!="Unspecified") %>%
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==max(Follow_up_months)) %>%
  arrange(Prop) %>%
  select(Group) %>%
  left_join(trial2) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(Group=as.factor(Group)) %>%
  mutate(Group=fct_relevel(Group,"Liver", "Bone", "Pancreatic", "Skin", "Brain", "Gastro", "Prostate", "Lung", "Leukemia", "Lymphoma", "Other", "Kidney", "Urinary", "Respiratory", "Head", "Reproductive", "Breast", "Intestinal", "Thyroid", "Myeloma", "Salivary", "Unspecified")) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_line(size=1) +
  ylim(0,100)+
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  scale_colour_viridis_d(option = "D")+
  theme_minimal()

data.frame(trial2 %>%  drop_na() %>% group_by(Group) %>%  filter(Prop==min(Prop)) %>% slice(1))


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")



PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Platinum        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Platinum agent"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Platinum,Treat)) 

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1

trial <- trial %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) 

names(trial)[4] <- "dis"

fittrial <- CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)



trial_transformed <- transpose(data.frame(fittrial$est)[23:44,])
names(trial_transformed)[1] <- "Bone"
names(trial_transformed)[2] <- "Brain"
names(trial_transformed)[3] <- "Breast"
names(trial_transformed)[4] <- "Gastro"
names(trial_transformed)[5] <- "Head"
names(trial_transformed)[6] <- "Intestinal"
names(trial_transformed)[7] <- "Kidney"
names(trial_transformed)[8] <- "Leukemia"
names(trial_transformed)[9] <- "Liver"
names(trial_transformed)[10] <- "Lung"
names(trial_transformed)[11] <- "Lymphoma"
names(trial_transformed)[12] <- "Myeloma"
names(trial_transformed)[13] <- "Other"
names(trial_transformed)[14] <- "Pancreatic"
names(trial_transformed)[15] <- "Prostate"
names(trial_transformed)[16] <- "Reproductive"
names(trial_transformed)[17] <- "Respiratory"
names(trial_transformed)[18] <- "Salivary"
names(trial_transformed)[19] <- "Skin"
names(trial_transformed)[20] <- "Thyroid"
names(trial_transformed)[21] <- "Unspecified"
names(trial_transformed)[22] <- "Urinary"

trial2 <- trial_transformed %>% gather(Group, Prop, Bone:Urinary) 

trial2$Prop <- 1- trial2$Prop


vector <- trial2 %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==15) %>%
  arrange(Prop) %>% select(Group) %>% distinct() %>% ungroup()

vector <- as.vector(paste0(vector$Group, collapse = ", "))


trial2 %>% 
  filter(Group!="Unspecified") %>%
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  filter(Follow_up_months==max(Follow_up_months)) %>%
  arrange(Prop) %>%
  select(Group) %>%
  left_join(trial2) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(Group=as.factor(Group)) %>%
  mutate(Group=fct_relevel(Group,"Liver", "Bone", "Pancreatic", "Skin", "Brain", "Gastro", "Prostate", "Lung", "Leukemia", "Lymphoma", "Other", "Kidney", "Urinary", "Respiratory", "Head", "Reproductive", "Breast", "Intestinal", "Thyroid", "Myeloma", "Salivary", "Unspecified")) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_line(size=1) +
  ylim(0,100)+
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  scale_colour_viridis_d(option = "D")+
  theme_minimal()

data.frame(trial2 %>%  drop_na() %>% group_by(Group) %>%  filter(Prop==min(Prop)) %>% slice(1))


# ---------------------------------------------------------------------------
# Estimate which drugs are more associated with larger BMI drops ------------------------




# BREAST CANCER - LINEAR MODEL/REGRESSION -------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% select(patient,value)

MAX <- PONS_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
names(MAX)[2] <- "MAX" 
MIN <- PONS_Measures %>% group_by(patient) %>% filter(value==min(value)) %>% slice(1)
names(MIN)[2] <- "MIN" 

PONS_Measures <- MAX %>% left_join(MIN) %>% mutate(MaxDrop=100*(MIN-MAX)/MAX)
PONS_Measures <- PONS_Measures %>% select(patient, MaxDrop)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs, Drugs))


CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=Drugs, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- PONS_Measures %>% inner_join(CAN_Drug_Histories)
temp <- CAN_Drug_Histories %>% ungroup() %>% select(-patient) 

temp$MaxDrop <- abs(temp$MaxDrop)
summaryLM <- lm(MaxDrop ~ ., data = temp)
summary(summaryLM)



# PROSTATE CANCER - LINEAR MODEL/REGRESSION ----------------------------------------------------------



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Prostate Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% select(patient,value)

MAX <- PONS_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
names(MAX)[2] <- "MAX" 
MIN <- PONS_Measures %>% group_by(patient) %>% filter(value==min(value)) %>% slice(1)
names(MIN)[2] <- "MIN" 

PONS_Measures <- MAX %>% left_join(MIN) %>% mutate(MaxDrop=100*(MIN-MAX)/MAX)
PONS_Measures <- PONS_Measures %>% select(patient, MaxDrop)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs, Drugs))

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=Drugs, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- PONS_Measures %>% inner_join(CAN_Drug_Histories)
temp <- CAN_Drug_Histories %>% ungroup() %>% select(-patient) 

temp$MaxDrop <- abs(temp$MaxDrop)
summaryLM <- lm(MaxDrop ~ ., data = temp)
summary(summaryLM)





# LUNG CANCER - LINEAR MODEL/REGRESSION  -----------------------------------------------


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Lung Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% select(patient,value)

MAX <- PONS_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
names(MAX)[2] <- "MAX" 
MIN <- PONS_Measures %>% group_by(patient) %>% filter(value==min(value)) %>% slice(1)
names(MIN)[2] <- "MIN" 

PONS_Measures <- MAX %>% left_join(MIN) %>% mutate(MaxDrop=100*(MIN-MAX)/MAX)
PONS_Measures <- PONS_Measures %>% select(patient, MaxDrop)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs, Drugs))

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=Drugs, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- PONS_Measures %>% inner_join(CAN_Drug_Histories)
temp <- CAN_Drug_Histories %>% ungroup() %>% select(-patient) 

temp$MaxDrop <- abs(temp$MaxDrop)
summaryLM <- lm(MaxDrop ~ ., data = temp)
summary(summaryLM)


# --------------------------------------------------------------
# --------------------
# BREAST CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% select(patient,value)

MAX <- PONS_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
names(MAX)[2] <- "MAX" 
MIN <- PONS_Measures %>% group_by(patient) %>% filter(value==min(value)) %>% slice(1)
names(MIN)[2] <- "MIN" 

PONS_Measures <- MAX %>% left_join(MIN) %>% mutate(MaxDrop=100*(MIN-MAX)/MAX)
PONS_Measures <- PONS_Measures %>% select(patient, MaxDrop)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs, Drugs))

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(Drugs) %>% count() %>% filter(n>50) %>% select(Drugs) %>%
  left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=Drugs, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- PONS_Measures %>% inner_join(CAN_Drug_Histories)
temp <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

temp$MaxDrop <- abs(temp$MaxDrop)


suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))




model_bayes<-  stan_glm(MaxDrop~., data=temp, seed=111, iter=5000, chains=10)


print(model_bayes, digits = 3)




mcmc_dens(model_bayes, pars = c("`62`"))+
  vline_at(8.837      , col="red")

describe_posterior(model_bayes)

  
post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)

     
hdi(model_bayes)



eti(model_bayes)



# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)

# p−value=1−pd
1- purrr::map_dbl(post, p_direction)

    
    
plot <- fread("temp.txt", sep="|")
data.frame(plot[,1:5])

plot$CI_low <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,1])
plot$CI_high <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,2])

plot <- plot %>% select(1,2,9,10)

plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot

plot2 <- plot2[-15,]

plot2$Parameter <- c("Baseline Reduction", "Anastrozole", "Azacitidine", "Capecitabine", "Docetaxel", "Doxorubicin", "Eribulin", "Etoposide", "Everolimus", "Fulvestrant", "Gemcitabine", "Lenalidomide", "Methotrexate", "Paclitaxel", "Palbociclib", "Pemetrexed", "Ado-Trastuzumab", "Atezolizumab", "Bevacizumab", "Nivolumab", "Pembrolizumab", "Rituximab", "External", "Systemic Radiotherapy", "Major Surgery")
  
plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="firebrick", size=3, alpha=0.7) +
  geom_point(colour="midnightblue", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Breast Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()
    
    
    
    
    

# PROSTATE CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Prostate Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% select(patient,value)

MAX <- PONS_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
names(MAX)[2] <- "MAX" 
MIN <- PONS_Measures %>% group_by(patient) %>% filter(value==min(value)) %>% slice(1)
names(MIN)[2] <- "MIN" 

PONS_Measures <- MAX %>% left_join(MIN) %>% mutate(MaxDrop=100*(MIN-MAX)/MAX)
PONS_Measures <- PONS_Measures %>% select(patient, MaxDrop)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs, Drugs))

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(Drugs) %>% count() %>% filter(n>50) %>% select(Drugs) %>%
  left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=Drugs, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- PONS_Measures %>% inner_join(CAN_Drug_Histories)
temp <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

temp$MaxDrop <- abs(temp$MaxDrop)


suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))




model_bayes<-  stan_glm(MaxDrop~., data=temp, seed=111, iter=5000, chains=10)




print(model_bayes, digits = 3)



mcmc_dens(model_bayes, pars = c("`62`"))+
  vline_at(5.328          , col="red")

describe_posterior(model_bayes)



  
post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)


     
hdi(model_bayes)


eti(model_bayes)




# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)

    
# p−value=1−pd
1- purrr::map_dbl(post, p_direction)

    
    
plot <- fread("temp.txt", sep="|")
data.frame(plot[,1:5])

plot$CI_low <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,1])
plot$CI_high <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,2])

plot <- plot %>% select(1,2,9,10)

plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot

plot2 <- plot2[-15,]

plot2$Parameter <- c("Baseline Reduction", "Abiraterone", "Azacitidine", "Bicalutamide", "Bortezomib", "Cabazitaxel", "Degarelix", "Docetaxel", "Goserelin", "Leuprolide", "Nintedanib", "Octreotide", "Paclitaxel", "Temozolomide", "Bevacizumab", "Nivolumab", "Pembrolizumab", "Rituximab", "Systemic Radiotherapy", "Cisplatin")
  
plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="firebrick", size=3, alpha=0.7) +
  geom_point(colour="midnightblue", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Prostate Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()

    
    
    
    
    
    

# LUNG CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Lung Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% select(patient,value)

MAX <- PONS_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
names(MAX)[2] <- "MAX" 
MIN <- PONS_Measures %>% group_by(patient) %>% filter(value==min(value)) %>% slice(1)
names(MIN)[2] <- "MIN" 

PONS_Measures <- MAX %>% left_join(MIN) %>% mutate(MaxDrop=100*(MIN-MAX)/MAX)
PONS_Measures <- PONS_Measures %>% select(patient, MaxDrop)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs, Drugs))

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(Drugs) %>% count() %>% filter(n>50) %>% select(Drugs) %>%
  left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=Drugs, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- PONS_Measures %>% inner_join(CAN_Drug_Histories)
temp <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

temp$MaxDrop <- abs(temp$MaxDrop)


suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))




model_bayes<-  stan_glm(MaxDrop~., data=temp, seed=111, iter=5000, chains=10)


print(model_bayes, digits = 3)



mcmc_dens(model_bayes, pars = c("`218`"))+
  vline_at(3.400                , col="red")



describe_posterior(model_bayes)

  
post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)

hdi(model_bayes)


eti(model_bayes)



# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)

    
# p−value=1−pd
1- purrr::map_dbl(post, p_direction)

    
plot <- data.frame(describe_posterior(model_bayes))[,1:5]

plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot
plot2$Parameter <- c("Baseline Reduction", "Etoposide", "Octreotide", "Topotecan", "Bevacizumab", "Cetuximab", "Nivolumab", "Pembrolizumab", "External Radiotherapy", "Systemic Radiotherapy")
  
plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="firebrick", size=3, alpha=0.7) +
  geom_point(colour="midnightblue", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Lung Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()

# INTESTINAL CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- CAN_Drug_Histories %>% select(patient) %>% inner_join(PONS_Measures, by=c("patient"="patid"))
PONS_Measures <- PONS_Measures %>% select(patient,value)

MAX <- PONS_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)
names(MAX)[2] <- "MAX" 
MIN <- PONS_Measures %>% group_by(patient) %>% filter(value==min(value)) %>% slice(1)
names(MIN)[2] <- "MIN" 

PONS_Measures <- MAX %>% left_join(MIN) %>% mutate(MaxDrop=100*(MIN-MAX)/MAX)
PONS_Measures <- PONS_Measures %>% select(patient, MaxDrop)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
unique(PONS_Ingredients$drug_class)

string_CancerDrugs        <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "GDF15"|
                                                                              PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                              PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                              PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                              PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_CancerDrugs, Drugs))

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(Drugs) %>% count() %>% filter(n>50) %>% select(Drugs) %>%
  left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=Drugs, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- PONS_Measures %>% inner_join(CAN_Drug_Histories)
temp <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

temp$MaxDrop <- abs(temp$MaxDrop)


suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))




model_bayes<-  stan_glm(MaxDrop~., data=temp, seed=111, iter=5000, chains=10)




print(model_bayes, digits = 3)


  


describe_posterior(model_bayes)


  
post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)

     
hdi(model_bayes)



eti(model_bayes)



# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)
  

    
# p−value=1−pd
1- purrr::map_dbl(post, p_direction)


plot <- data.frame(describe_posterior(model_bayes))[,1:5]

plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot
plot2$Parameter <- c("Baseline Reduction", "Aflibercept", "Octreotide", "Bevacizumab", "Pembrolizumab", "Major Surgery" )
  
plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="firebrick", size=3, alpha=0.7) +
  geom_point(colour="midnightblue", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Intestinal Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()

# -------------------

# -------------------

# Drug Penetrance Ever using the drug class classification ---------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep = "\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer != "-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight)


CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
sum(CAN_Drug_Histories$weight)  # 9861087

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass)

CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>% summarise(n=sum(weight)) # 9861087


data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% 
  group_by(chemo_class) %>% summarise(n=sum(weight)/9861087)) %>% arrange(-n)



data.frame(CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))


data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer))  %>% 
  select(patid, weight, Primary_Cancer, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% 
  filter(chemo_class=="Platinum agent") %>%
  group_by(Primary_Cancer) %>% summarise(denom=sum(weight))) %>% arrange(-denom) %>%
  left_join(
    data.frame(CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))
  ) %>% mutate(percent = denom/n) %>% arrange(-percent) %>% select(Primary_Cancer, percent)



data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, Primary_Cancer))  %>% 
  select(patid, weight, Primary_Cancer, chemo_class) %>% 
             mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% 
  filter(chemo_class=="PD1/PDL1") %>%
  group_by(Primary_Cancer) %>% summarise(denom=sum(weight))) %>% arrange(-denom) %>%
  left_join(
    data.frame(CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer) %>% summarise(n=sum(weight)))
  ) %>% mutate(percent = denom/n) %>% arrange(-percent) %>% select(Primary_Cancer, percent)



CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, cachexia_onset) %>% filter(!is.na(cachexia_onset))
CachexiaDx <- PONS_Demographics %>% select(patid)
CachexiaPred <- CachexiaPats_ALL_NEW %>% anti_join(CachexiaDx)

PONS_Demographics <- fread("PONS Demographics.txt")
CachexiaDx %>% left_join(PONS_Demographics) %>% summarise(n=sum(weight)) # 676318.6
CachexiaPred %>% left_join(PONS_Demographics) %>% summarise(n=sum(weight, na.rm=T)) # 1476765


CAN_Drug_Histories %>% inner_join(CachexiaDx) %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 350361

data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% distinct() %>% 
             inner_join(CachexiaDx) %>%
  mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% 
  group_by(chemo_class) %>% summarise(n=sum(weight)/350361)) %>% arrange(-n)


CAN_Drug_Histories %>% inner_join(CachexiaPred) %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 856140


data.frame(CAN_Drug_Histories %>% select(patid, weight, chemo_class) %>% distinct() %>% 
             inner_join(CachexiaPred) %>%
  mutate(chemo_class=ifelse(chemo_class=="Biologic Therapy","Biologic", chemo_class)) %>%
             distinct() %>% 
  group_by(chemo_class) %>% summarise(n=sum(weight)/856140)) %>% arrange(-n)



# -----------------------------------------------------------------
# Persistency Platinum Cachexia vs No Cachexia LUNG CANCER  -------------------------------------------------------------------



"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Platinum        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Platinum agent"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Platinum,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))




# --------------------------------------------------------------

# Persistency PD1 PDL1 Cachexia vs No Cachexia LUNG CANCER  -------------------------------------------------------------------



"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_PD1        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "PD1/PDL1"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_PD1,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))



# -----------------------------------------------------------------------------

# Persistency Surgery Cachexia vs No Cachexia LUNG CANCER  -------------------------------------------------------------------



"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Surgery       <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Surgery,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))


# --------------------------------------------------------------

# Persistency Radio Cachexia vs No Cachexia LUNG CANCER ------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Radio       <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Radio"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Radio,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))



# ----------------------------------------------------------------

# Persistency Radio Cachexia vs No Cachexia LUNG CANCER ------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Antiemetic       <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Antiemetic"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Antiemetic,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))




# ----------------------------------------------------------------

# Persistency Steroid Cachexia vs No Cachexia LUNG CANCER ------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Corticosteroid       <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Corticosteroid"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Corticosteroid,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))


 # -----------------------------------------
 
 
 
 
 # Persistency Hormonal Cachexia vs No Cachexia LUNG CANCER ------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Hormonal        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Hormonal,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  # geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))

 # -----------------------------------------
 
  # Persistency Alkylating Cachexia vs No Cachexia LUNG CANCER ------------------------------

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  
  if(missing(level))
  { 
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  
  invisible(out)
}



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis=="Lung Cancer") 

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_Alkylating        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Alkylating Agent"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Alkylating,Treat)) 


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date)
PONS_Demographics <- PONS_Demographics %>% drop_na()
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
PONS_Demographics <- PONS_Demographics %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patid, Exact_Month)

names(PONS_Demographics)[1] <- "patient"
names(PONS_Demographics)[2] <- "Death_Date"


trial <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CAN_Drug_Histories %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(PONS_Demographics) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

trial <- trial %>% left_join(PONS_Demographics) %>% mutate(Cachexia=ifelse(is.na(cachexia_onset),"Cachexia","None")) %>%
  select(-cachexia_onset)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)

data.frame(fittrial$est)[3:4,]
transpose(data.frame(fittrial$ci)[,121:240])

trial_transformed <- transpose(data.frame(fittrial$est)[3:4,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "Cachexia"
names(trial_transformed)[2] <- "None"

trial <- trial_transformed %>% gather(Group, Prop, Cachexia:None) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,121:240]))

trial$Prop <- 1- trial$Prop
trial$V1 <- 1- trial$V1
trial$V2 <- 1- trial$V2

names(trial)[3] <- "conf.high"
names(trial)[4] <- "conf.low"


trial %>% 
  group_by(Group) %>%
  mutate(Follow_up_months=row_number()) %>%
  mutate(Prop=round(100*Prop)) %>%
  mutate(conf.high=round(100*conf.high)) %>%
  mutate(conf.low=round(100*conf.low)) %>%
  ggplot(mapping = aes(x = Follow_up_months, y = Prop, colour=Group)) +
  geom_step(aes(color = Group), show.legend = FALSE, size=0.1) +
  #geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.6) +
  geom_line() +
  ylim(0,100) +
  labs(x = "\n Number Months ON Therapy", y = "Proportion (%) Remaining Patients \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme_minimal()

trial %>% group_by(Group) %>% drop_na() %>% filter(Prop==min(Prop))


 # -----------------------------------------
 
 
 
# Number of flows cachexia vs no cachexia ---------------------------
noLines <- fread("Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")

noLines <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)

noLines <- noLines %>% mutate(Drugs = ifelse(Drugs=="","-",Drugs))

noLines$Month <- as.character(noLines$Month)
noLines$Month <- parse_number(noLines$Month)

noLines <- noLines %>% select(patient) %>% distinct() %>% left_join(noLines)

noLines <- noLines %>% group_by(patient) %>% mutate(flow = ifelse(Drugs != lag(Drugs),1,0)) %>% 
  mutate(flow = ifelse(is.na(flow),0,flow))

totalFlows <- noLines %>% group_by(patient) %>% summarise(n=sum(flow))


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

totalFlows %>% inner_join(PONS_Demographics) %>% summarise(flows=mean(n)) # 5.30
totalFlows %>% anti_join(PONS_Demographics) %>% summarise(flows=mean(n)) # 4.15



New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CancerDrug_Experienced %>% inner_join(New_Primary_Cancer_Box)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CancerDrug_Experienced %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
names(CAN_Drug_Histories)[1] <- "patient"

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
length(unique(CAN_Drug_Histories$patient))


CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient) %>% distinct() %>% left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% mutate(flow = ifelse(Drugs != lag(Drugs),1,0)) %>% 
  mutate(flow = ifelse(is.na(flow),0,flow))

totalFlows_all <- CAN_Drug_Histories %>% group_by(patient) %>% summarise(n=sum(flow))


totalFlows_all %>% inner_join(PONS_Demographics) %>% summarise(flows=mean(n)) # 12.9
totalFlows_all %>% anti_join(PONS_Demographics) %>% summarise(flows=mean(n)) # 10.3

# ------------------------------------------------------------------------------
# Timelines Remission, Naive, Treated -----------------------------------------------------

noLines <- fread("Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")




# Mortality
PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- noLines %>% select(patient) %>% left_join(PONS_Demographics %>% select(patid, weight, death_date), by=c("patient"="patid"))
PONS_Demographics <- PONS_Demographics %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

PONS_Demographics$Death_Date <- PONS_Demographics$death_date
Death_Date <- PONS_Demographics

PONS_Demographics <- PONS_Demographics %>% select(patient, weight) %>% mutate(Repeat=60) 
PONS_Demographics <- expandRows(PONS_Demographics, "Repeat")


PONS_Demographics$Exact_Month <- ave(PONS_Demographics$patient , PONS_Demographics$patient ,FUN = seq_along)
PONS_Demographics$Exact_Month <- as.numeric(PONS_Demographics$Exact_Month)
unique(PONS_Demographics$Exact_Month)

Death_Date <- Death_Date %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% select(patient, Exact_Month)
Death_Date <- Death_Date %>% drop_na()
Death_Date$Death <- "Death"

PONS_Demographics <- PONS_Demographics %>% left_join(Death_Date)

PONS_Demographics <- PONS_Demographics %>% mutate(Death=ifelse(is.na(Death),"0",Death))
PONS_Demographics <- PONS_Demographics  %>% group_by(patient) %>% mutate(Death=ifelse(cumsum(Death =="Death")>=1,"Death",Death))




# First Rx
FirstRx <- gather(noLines, Month, Drugs, month1:month60) 
FirstRx$Month <- as.character(FirstRx$Month)
FirstRx$Month <- parse_number(FirstRx$Month)
FirstRx <- FirstRx %>% filter(Drugs!="") %>% group_by(patient) %>% filter(Month==min(Month)) %>% mutate(FirstRx="FirstRx")
FirstRx <- FirstRx[,-4]

PONS_Demographics <- PONS_Demographics %>% left_join(FirstRx, by=c("patient"="patient", "weight"="weight", "Exact_Month"="Month"))

PONS_Demographics <- PONS_Demographics %>% mutate(FirstRx=ifelse(is.na(FirstRx),"0",FirstRx))

PONS_Demographics <- PONS_Demographics  %>% group_by(patient) %>% mutate(Naive=ifelse(cumsum(FirstRx =="FirstRx")>=1,"0","Naive"))




# Current Rx
CurrentRx <- gather(noLines, Month, Drugs, month1:month60) 
CurrentRx$Month <- as.character(CurrentRx$Month)
CurrentRx$Month <- parse_number(CurrentRx$Month)
CurrentRx <- CurrentRx %>% filter(Drugs!="") %>% group_by(patient)  %>% mutate(CurrentRx="CurrentRx")
CurrentRx <- CurrentRx[,-4]

PONS_Demographics <- PONS_Demographics %>% left_join(CurrentRx, by=c("patient"="patient", "weight"="weight", "Exact_Month"="Month"))

PONS_Demographics <- PONS_Demographics %>% mutate(CurrentRx=ifelse(is.na(CurrentRx),"-",CurrentRx))

PONS_Demographics <- PONS_Demographics  %>% group_by(patient) %>% 
  mutate(Remission=ifelse(cumsum(FirstRx =="FirstRx")>=1 & 
                            ( (CurrentRx == "-" & lead(CurrentRx,1) == "-" & lead(CurrentRx,2) == "-") |
                            (lag(CurrentRx,1) == "-" & CurrentRx == "-" & lead(CurrentRx,1) == "-") | 
                            (lag(CurrentRx,1) == "-"  & lag(CurrentRx,2) == "-"  & CurrentRx == "-") ),"Remission","0"))

PONS_Demographics <- PONS_Demographics %>% mutate(Remission=ifelse(is.na(Remission),"0",Remission))


PONS_Demographics <- PONS_Demographics %>% mutate(Status= ifelse(Death=="Death", "Death",
                                                                 ifelse(Remission=="Remission","Remission", 
                                                                        ifelse(CurrentRx=="CurrentRx","CurrentRx", 
                                                                                      ifelse(Naive=="Naive","Naive","none")))))

PONS_Demographics %>% filter(Exact_Month==60) %>% group_by(Status) %>% summarise(n=sum(weight))


PONS_Demographics <- PONS_Demographics  %>% group_by(patient) %>% mutate(TotalTreat=cumsum(CurrentRx=="CurrentRx"))

PONS_Demographics <- PONS_Demographics %>% left_join(PONS_Demographics  %>% filter(CurrentRx=="CurrentRx") %>% group_by(patient) %>% count())
names(PONS_Demographics)[11] <-"SumTreat"

PONS_Demographics <- PONS_Demographics %>% mutate(Status2 = ifelse( Status =="Remission" & TotalTreat<SumTreat, "none", Status))

PONS_Demographics %>% filter(Exact_Month==60) %>% group_by(Status2) %>% summarise(n=sum(weight))


fwrite(PONS_Demographics, "temp_timeline_Rxs_Remissions.txt", sep="\t")

# ----------------------------------------------------------------------------------------------------------
# Current status by primary cancer and cachexia status ----------------------

temp_timeline_Rxs_Remissions <- fread("temp_timeline_Rxs_Remissions.txt")
temp_timeline_Rxs_Remissions <- temp_timeline_Rxs_Remissions %>% filter(Exact_Month==54) %>% select(patient, weight, Status2)

temp_timeline_Rxs_Remissions %>% group_by(Status2) %>% summarise(n=sum(weight))


# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)

# Cachexia Pred
Cachexia_pats <- fread("Cachexia_pats.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pred <- Cachexia_pats %>% anti_join(Cachexia_Dx)

names(Cachexia_Dx)[1] <- "patient"
names(Cachexia_pred)[1] <- "patient"


temp_timeline_Rxs_Remissions %>% inner_join(Cachexia_Dx) %>% group_by(Status2) %>% summarise(n=sum(weight))



temp_timeline_Rxs_Remissions %>% anti_join(Cachexia_Dx) %>% group_by(Status2) %>% summarise(n=sum(weight))



Cachexia_Dx$Group <- "CachexiaDx"

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid , Primary_Cancer)
names(New_Primary_Cancer_Box)[1] <- "patient"

data.frame(temp_timeline_Rxs_Remissions %>% inner_join(New_Primary_Cancer_Box) %>%
             left_join(Cachexia_Dx) %>%
  group_by(Primary_Cancer, Group, Status2) %>% summarise(n=sum(weight)) %>%
    ungroup() %>% spread(key=Status2, value=n))





PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age)
names(PONS_Demographics)[1] <- "patient"


data.frame(temp_timeline_Rxs_Remissions %>% inner_join(New_Primary_Cancer_Box) %>%
             left_join(Cachexia_Dx) %>%
  left_join(PONS_Demographics) %>%
  group_by(Primary_Cancer, Group, Status2) %>% summarise(n=mean(age)) %>%
   ungroup() %>% spread(key=Status2, value=n))

data.frame(temp_timeline_Rxs_Remissions %>% inner_join(New_Primary_Cancer_Box) %>%
             left_join(Cachexia_Dx) %>%
  left_join(PONS_Demographics) %>%
  group_by(Primary_Cancer, Group) %>% summarise(n=mean(age)) %>%
   ungroup() %>% spread(key=Group, value=n))



temp_timeline_Rxs_Remissions <- fread("temp_timeline_Rxs_Remissions.txt")

data.frame(temp_timeline_Rxs_Remissions %>% select(patient, weight, SumTreat) %>% distinct() %>%
  inner_join(New_Primary_Cancer_Box) %>%
             left_join(Cachexia_Dx) %>%
  group_by(Primary_Cancer, Group) %>%
  summarise(n=weighted.mean(SumTreat, weight)) %>%
  ungroup() %>% spread(key=Group, value=n))

# ---------------------------------------------------------------
# Drug Class penetrance ever V3 Jan9  --------------------------------------------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)


# PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
# PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
# PONS_Ingredients <- PONS_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
# PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
# 
# PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)
# 
# PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%
#    mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) 
# 
# 
# PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(-c(drug_id)))
# 
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(chemo_class = ifelse(generic_name == "Acitretin" , "Immuno/Targeted" ,
                                                               ifelse(generic_name == "Bromocriptine" , "Other Antineoplastics" ,
                                                                      ifelse(generic_name == "Cabergoline" , "Hormonal Therapy" ,
                                                                             ifelse(generic_name == "Desmopressin" , "Hormonal Therapy" ,
                                                                                    ifelse(generic_name == "Diethylstilbestrol" , "Hormonal Therapy" ,
                                                                                           ifelse(generic_name == "Plicamycin" , "Other Antineoplastics" ,
                                                                                                  ifelse(generic_name == "Denosumab" , "Biologic" ,
                                                                                                         ifelse(generic_name == "Emapalumab" , "Biologic" ,
                                                                                                                ifelse(generic_name == "Thyrotropin" , "Hormonal Therapy" , chemo_class))))))))))


# fwrite(PONS_Ingredients_JN_ChemoClass, "PONS_Ingredients_JN_ChemoClass.csv", sep=",")

PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.csv", sep=",")

# 753744 -> 672052 any -> 337699


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
length(unique(CAN_Drug_Histories$patid)) # 672052
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, Drugs) %>% distinct()

names(CAN_Drug_Histories)[2] <- "molecule"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class))

length(unique(CAN_Drug_Histories$patid)) # 672052

CancerDrug_Experienced <- CAN_Drug_Histories %>% filter(chemo_class=="Alkylating Agent"|
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
                                  chemo_class=="Surgery Inpatient") %>% select(patid) %>% distinct() 




CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>% summarise(n=sum(weight)) # 10406757


df <- CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>% select(patid, Primary_Cancer, weight) %>% distinct() %>%
  group_by(Primary_Cancer) %>% summarise(Total=sum(weight)) %>%
  left_join(
    CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>%   left_join(CAN_Drug_Histories) %>%
      select(patid, Primary_Cancer, chemo_class, weight) %>% distinct() %>%
      group_by(Primary_Cancer, chemo_class) %>% summarise(n=sum(weight))
  ) %>% mutate(perc=n/Total) %>% select(-c(Total, n)) %>% distinct() %>%
  spread(key=chemo_class, value=perc)

fwrite(df, "df2.csv")

data.frame(CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>%
  left_join(CAN_Drug_Histories) %>%
    select(patid, weight, chemo_class) %>% distinct() %>%
  group_by(chemo_class) %>% summarise(n=sum(weight)/10406757)  %>% arrange(-n))




Antidepressant_Antipsychotic_exp <- data.frame(CancerDrug_Experienced %>% left_join(New_Primary_Cancer_Box ) %>%
  left_join(CAN_Drug_Histories) %>%
    select(patid, weight, chemo_class) %>% distinct()) %>%
    filter(chemo_class == "Antidepressant" | chemo_class == "Antipsychotic")

fwrite(Antidepressant_Antipsychotic_exp, "Antidepressant_Antipsychotic_exp.txt" , sep="\t")

PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt" , sep="\t")

PONS_Comorbidity_Inventories <- Antidepressant_Antipsychotic_exp %>% select(patid) %>% distinct() %>%
  left_join(PONS_Comorbidity_Inventories)

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(grepl("F2", diagnosis)|grepl("F3", diagnosis)|grepl("F4", diagnosis))


Antidepressant_Antipsychotic_exp %>% select(patid) %>% distinct()  %>% # 65643
  inner_join(PONS_Comorbidity_Inventories %>% select(patid) %>% distinct()) # 52566

Antidepressant_Antipsychotic_exp_to_track <- Antidepressant_Antipsychotic_exp %>% select(patid) %>% distinct()  %>% # 65643
  anti_join(PONS_Comorbidity_Inventories %>% select(patid) %>% distinct()) # 52566

Antidepressant_Antipsychotic_exp_to_track


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- Antidepressant_Antipsychotic_exp_to_track %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Antidepressant <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Antidepressant"], collapse = "|"),")\\b")
string_Antipsychotic <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Antipsychotic"], collapse = "|"),")\\b")


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Drugs=ifelse(grepl(string_Antidepressant, Drugs),1,0))
names(CAN_Drug_Histories)[4] <- "Antidepressant"
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Min_Antidepressant <- CAN_Drug_Histories %>% filter(Antidepressant==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month)
names(Min_Antidepressant)[2] <- "First_Antidepressant"

CAN_Drug_Histories <- Min_Antidepressant %>% select(patid) %>% left_join(CAN_Drug_Histories)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- Min_Antidepressant %>% select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)
PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed))
PONS_Measures <- PONS_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))
PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()
PONS_Measures <- PONS_Measures %>% group_by(patid, Exact_Month) %>% summarise(n=mean(value))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Measures, by=c("patid"="patid", "Month"="Exact_Month"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(Min_Antidepressant)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Month=Month-First_Antidepressant)

CAN_Drug_Histories <- CAN_Drug_Histories %>% drop_na()


CAN_Drug_Histories %>% mutate(period=ifelse(Month<=0,"Before","After")) %>%
  group_by(patid,period) %>% summarise(N=mean(n)) %>%
  spread(key=period, value=N) %>%
  drop_na() %>%
  mutate(Diff=After-Before) %>%
  ungroup() %>% summarise(n=mean(Diff))

CAN_Drug_Histories %>%
  group_by(Month, Antidepressant) %>% summarise(n=mean(n)) %>%
  #mutate(Month=Month+10) %>%
  filter(Month>(-24)&Month<24) %>%
  ggplot(aes(Month, n)) +
  ylim(20,30) +
  geom_smooth(fill="firebrick", colour="darkred", size=2, alpha=0.5) +
    theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Elapsed Time (months) to/from Antidepressant Initiation") +
  ylab("Average Monthly BMI (kg/m2) \n")











CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- Antidepressant_Antipsychotic_exp_to_track %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Antidepressant <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Antidepressant"], collapse = "|"),")\\b")
string_Antipsychotic <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Antipsychotic"], collapse = "|"),")\\b")


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Drugs=ifelse(grepl(string_Antipsychotic, Drugs),1,0))
names(CAN_Drug_Histories)[4] <- "Antipsychotic"
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Min_Antipsychotic <- CAN_Drug_Histories %>% filter(Antipsychotic==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month)
names(Min_Antipsychotic)[2] <- "First_Antipsychotic"

CAN_Drug_Histories <- Min_Antipsychotic %>% select(patid) %>% left_join(CAN_Drug_Histories)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- Min_Antipsychotic %>% select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)
PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed))
PONS_Measures <- PONS_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))
PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()
PONS_Measures <- PONS_Measures %>% group_by(patid, Exact_Month) %>% summarise(n=mean(value))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Measures, by=c("patid"="patid", "Month"="Exact_Month"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(Min_Antipsychotic)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Month=Month-First_Antipsychotic)

CAN_Drug_Histories <- CAN_Drug_Histories %>% drop_na()


CAN_Drug_Histories %>% mutate(period=ifelse(Month<=0,"Before","After")) %>%
  group_by(patid,period) %>% summarise(N=mean(n)) %>%
  spread(key=period, value=N) %>%
  drop_na() %>%
  mutate(Diff=After-Before) %>%
  ungroup() %>% summarise(n=mean(Diff))

CAN_Drug_Histories %>%
  group_by(Month, Antipsychotic) %>% summarise(n=mean(n)) %>%
  # mutate(Month=Month+10) %>%
  filter(Month>(-24)&Month<24) %>%
  ggplot(aes(Month, n)) +
ylim(20,30) +
  geom_smooth(fill="firebrick", colour="darkred", size=2, alpha=0.5) +
    theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())+
  xlab("\n Elapsed Time (months) to/from Antipsychotic Initiation") +
  ylab("Average Monthly BMI (kg/m2) \n")


# ---------------------------------------------
# -----------------
# No Lines Therapy Cachexia vs No Cachexia ------------
noLines <- fread("Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")
CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
names(noLines)[1] <- "patient"
names(CancerDrug_Experienced)[1] <- "patient"

noLines <- noLines %>% inner_join(CancerDrug_Experienced)


PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)
names(Cachexia_Dx)[1] <- "patient"


CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")
names(CachexiaPats_ALL_NEW)[1] <- "patient"



temp <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
temp <- temp %>% filter(!is.na(Drugs)) %>% filter(Drugs!="") %>% select(patient, weight, Drugs) %>% distinct()

temp  %>% anti_join(Cachexia_Dx) %>% anti_join(CachexiaPats_ALL_NEW) %>%  group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 2.25
temp  %>% anti_join(Cachexia_Dx)  %>% inner_join(CachexiaPats_ALL_NEW) %>%  group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 2.51
temp  %>% anti_join(CachexiaPats_ALL_NEW)  %>% inner_join(Cachexia_Dx) %>%  group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 3.08


data.frame(temp %>% anti_join(Cachexia_Dx) %>% anti_join(CachexiaPats_ALL_NEW)  %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
             mutate(n=ifelse(n>=10,10,n)) %>%
  group_by(n) %>% summarise(total=sum(weight))) %>% select(2)

data.frame(temp %>%anti_join(Cachexia_Dx)  %>% inner_join(CachexiaPats_ALL_NEW)   %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
             mutate(n=ifelse(n>=10,10,n)) %>%
  group_by(n) %>% summarise(total=sum(weight))) %>% select(2)

data.frame(temp %>% anti_join(CachexiaPats_ALL_NEW)  %>% inner_join(Cachexia_Dx)  %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
             mutate(n=ifelse(n>=10,10,n)) %>%
  group_by(n) %>% summarise(total=sum(weight))) %>% select(2)



names(New_Primary_Cancer_Box)[1] <- "patient"


data.frame(temp %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  left_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(weight))) %>%
  mutate(n=ifelse(n>=10,10,n)) %>% ungroup() %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(total)) %>%
  ungroup() %>%
  spread(key=Primary_Cancer, value=total)



data.frame(data.frame(temp %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  left_join(New_Primary_Cancer_Box) %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(weight))) %>%
  mutate(n=ifelse(n>=10,10,n)) %>% ungroup() %>%
  group_by(Primary_Cancer, n) %>% summarise(total=sum(total)) %>%
  ungroup() %>% group_by(Primary_Cancer) %>% summarise(n=weighted.mean(n, total)))


noLines <- fread("Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
PONS_Demographics <- PONS_Demographics %>% drop_na()
names(PONS_Demographics)[1] <- "patient"

temp <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
temp <- temp %>% filter(!is.na(Drugs)) %>% select(patient, weight, Drugs) %>% distinct()

temp %>% inner_join(PONS_Demographics) %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 4.09
temp %>% anti_join(PONS_Demographics) %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(nean=mean(n)) # 3.26
 

# -----------------
# Age Boxplot across cancer types cachexia vs no cachexia -------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer") 

PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, age, cachexia_onset, cancer_metastasis)  %>%
  mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) %>%
   mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")

data.frame(New_Primary_Cancer_Box %>% left_join(PONS_Demographics_temp) %>%
             inner_join(CancerDrug_Experienced) %>%
  group_by(Primary_Cancer, cachexia_onset) %>%
  summarise(n=weighted.mean(age, weight))) 
  ungroup() %>%
  ggplot(aes())
  
  
data.frame(New_Primary_Cancer_Box %>% left_join(PONS_Demographics_temp) %>%
             inner_join(CancerDrug_Experienced)) %>%
  select(Primary_Cancer , age, cachexia_onset) %>%
  mutate(Primary_Cancer=str_replace(Primary_Cancer, " Cancer", "")) %>%
  mutate(Primary_Cancer=factor(Primary_Cancer, levels=c("Prostate", "Urinary", "Myeloma", "Lung", "Leukemia", "Skin", "Breast", "Lymphoma", "Kidney","Thyroid", "Pancreatic", "Salivary", "Gastroesophageal", "Respiratory", "Intestinal", "Head", "Liver", "Reproductive", "Other", "Unspecified", "Bone", "Brain"))) %>%
  ggplot(aes(Primary_Cancer, age, colour=as.factor(cachexia_onset), fill=as.factor(cachexia_onset))) +
  geom_boxplot(outlier.colour = "white", size=1, alpha=0.7, notch = TRUE, width=0.4 , show.legend = TRUE) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)) +
  ggsci::scale_fill_nejm()+
  ggsci::scale_colour_nejm()+
  xlab("")+ylab("")

PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, cachexia_onset, cancer_onset) %>% inner_join(New_Primary_Cancer_Box)

PONS_Demographics_temp <- PONS_Demographics_temp %>% drop_na()

# 467.2368
PONS_Demographics_temp %>% mutate(Elapsed= as.numeric(as.Date(cachexia_onset) - as.Date(cancer_onset))/30.5) %>%
  ggplot(aes(Elapsed)) + 
  geom_density(colour="deepskyblue4", fill="deepskyblue4", alpha= 0.8) +
  xlim(-12,60) +
  theme(panel.grid.major=element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank()) +
  xlab("\n No. Elapsed Months from Cancer Dx to Cachexia Dx")+ylab("Proportion \n")

data.frame(PONS_Demographics_temp %>% mutate(Elapsed= as.numeric(as.Date(cachexia_onset) - as.Date(cancer_onset))/30.5)  %>%
  group_by(Primary_Cancer) %>% summarise(n=mean(Elapsed)) %>% arrange(-n))


# --------------------------------
# Survival curves metastasis vs non-metastasis, cachexia Dx vs Cachexia Pred vs no cachexia --------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer") 
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight)

# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% filter(age>60&age<=70) %>% select(patid)

# Cachexia Pred
Cachexia_pats <- fread("CachexiaPats_Ever_AllDrops.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pred <- Cachexia_pats %>% inner_join(PONS_Demographics %>% filter(age>60&age<=70) %>% select(patid)) %>% anti_join(Cachexia_Dx)

names(Cachexia_Dx)[1] <- "patient"
names(Cachexia_pred)[1] <- "patient"

Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box, by=c("patient"="patid"))
Cachexia_pred <- Cachexia_pred %>% inner_join(New_Primary_Cancer_Box, by=c("patient"="patid"))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics %>% filter(age>60&age<=70) %>% select(patid)) %>% 
  anti_join(Cachexia_Dx, by=c("patid"="patient")) %>% anti_join(Cachexia_pred,by=c("patid"="patient"))



PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_metastasis, cancer_onset, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date)

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_temp) %>% summarise(n=mean(Survived)) # 57.24514
Cachexia_pred %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% summarise(n=mean(Survived)) # 52.84376
Cachexia_Dx %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% summarise(n=mean(Survived)) # 36.62144



data.frame(New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_temp) %>% 
             filter(cancer_metastasis==1) %>%
              summarise(n=mean(Survived)))

data.frame(Cachexia_pred %>% select(patient) %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% 
             filter(cancer_metastasis==1) %>%
              summarise(n=mean(Survived)))

data.frame(Cachexia_Dx  %>% select(patient) %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% 
             filter(cancer_metastasis==1) %>%
              summarise(n=mean(Survived)))



data.frame(New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_temp) %>% 
             filter(cancer_metastasis==0) %>%
              group_by(Survived) %>% summarise(n=sum(weight)))

data.frame(Cachexia_pred %>% select(patient) %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% 
             filter(cancer_metastasis==0) %>%
              group_by(Survived) %>% summarise(n=sum(weight)))

data.frame(Cachexia_Dx  %>% select(patient) %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% 
             filter(cancer_metastasis==0) %>%
              group_by(Survived) %>% summarise(n=sum(weight)))



# -------------------------
# Survival curves metastasis vs non-metastasis,Cachexia Pred  --------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer") 
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight, Primary_Cancer)

# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)

# Cachexia Pred
Cachexia_pats <- fread("CachexiaPats_Ever_AllDrops.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pred <- Cachexia_pats %>% anti_join(Cachexia_Dx)

names(Cachexia_Dx)[1] <- "patient"
names(Cachexia_pred)[1] <- "patient"

Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box, by=c("patient"="patid"))
Cachexia_pred <- Cachexia_pred %>% inner_join(New_Primary_Cancer_Box, by=c("patient"="patid"))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% anti_join(Cachexia_Dx, by=c("patid"="patient")) %>% anti_join(Cachexia_pred,by=c("patid"="patient"))



PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_metastasis, cancer_onset, death_date, age)
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date)

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

New_Primary_Cancer_Box %>% inner_join(PONS_Demographics_temp) %>% summarise(n=mean(Survived)) # 55.92183
Cachexia_pred %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% summarise(n=mean(Survived)) # 50.71558
Cachexia_Dx %>% inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% summarise(n=mean(Survived)) # 35.88322


# No Cachexia NO Mets
data.frame(New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Pancreatic Cancer") %>% inner_join(PONS_Demographics_temp) %>% 
             filter(cancer_metastasis==0) %>%
              group_by(Survived) %>% summarise(n=sum(weight)))


# Cachexia NO Mets
data.frame(Cachexia_pred %>% filter(Primary_Cancer=="Pancreatic Cancer") %>% select(patient) %>%  inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% 
             filter(cancer_metastasis==0) %>%
              group_by(Survived) %>% summarise(n=sum(weight)))


# No Cachexia + Mets
data.frame(New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Pancreatic Cancer") %>%  inner_join(PONS_Demographics_temp) %>% 
             filter(cancer_metastasis==1) %>%
              group_by(Survived) %>% summarise(n=sum(weight)))

# Cachexia + Mets
data.frame(Cachexia_pred %>%filter(Primary_Cancer=="Pancreatic Cancer") %>%  select(patient) %>%  inner_join(PONS_Demographics_temp, by=c("patient"="patid")) %>% 
             filter(cancer_metastasis==1) %>%
              group_by(Survived) %>% summarise(n=sum(weight)))

# --------------------------------------------
# Cachexia Penetrance in kids ------------------------------------------


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(age<18)
PONS_Demographics <- PONS_Demographics %>% filter(diagnosis!="-")
PONS_Demographics %>%  summarise(n=sum(weight)) #261252.5     #6849
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cachexia_onset, age)
PONS_Demographics <- PONS_Demographics %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

unique(PONS_Demographics$age)

PONS_Demographics <- PONS_Demographics %>% mutate(age=ifelse(age==1,age,
                                        ifelse(age==2,age,
                                               ifelse(age>2&age<=12,12,17))))

data.frame(PONS_Demographics %>% group_by(diagnosis, age) %>% 
             mutate(age=as.factor(age)) %>% summarise(n=sum(weight)) %>%
             spread(key=age, value=n))

                 diagnosis     X1      X2      X12      X17
1              Bone Cancer  39.11  126.05  3950.10  7217.06
2             Brain Cancer 606.83  760.89 19865.59 17206.39
3            Breast Cancer  83.95   65.02  3714.70  2180.02
4  Gastroesophageal Cancer  39.11   31.18   438.53   394.79
5              Head Cancer  83.95   94.87  3112.40  2310.43
6        Intestinal Cancer  39.11  191.07  3085.38  2219.71
7            Kidney Cancer  83.95  254.76  7618.39  3347.00
8          Leukemia Cancer 665.87  943.17 29800.06 16705.33
9             Liver Cancer 251.85  399.86  4424.21  1600.65
10             Lung Cancer 201.28  188.41  3812.69  1909.07
11         Lymphoma Cancer 290.96  413.32 14030.37 14796.94
12          Myeloma Cancer     NA   32.51   836.99   577.73
13            Other Cancer 854.96 1650.62 25607.17 15237.72
14       Pancreatic Cancer     NA   94.87   389.08   329.46
15         Prostate Cancer     NA   31.18  2277.31  1553.69
16     Reproductive Cancer 123.06  189.74  3286.77  2995.32
17      Respiratory Cancer     NA   32.51  1116.44   703.33
18         Salivary Cancer     NA      NA   435.46   361.78
19             Skin Cancer  78.22   62.36  6828.17  7658.25
20          Thyroid Cancer  39.11   31.18  1683.73  2771.90
21      Unspecified Cancer 212.74  250.77  6009.95  4856.95
22          Urinary Cancer  83.95   31.18  1333.05  1008.92


sum(PONS_Demographics$weight) #261252.5
sum(PONS_Demographics$weight[PONS_Demographics$cachexia_onset==1]) #1736.99 (0.006648702 cachexia)



PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

data.frame(PONS_Measures %>% select(patid) %>% distinct() %>%
  left_join(PONS_Demographics)  %>%
  group_by(diagnosis, age) %>% 
             mutate(age=as.factor(age)) %>% summarise(n=sum(weight)) %>%
             spread(key=age, value=n))

data.frame(PONS_Measures %>% select(patid) %>% distinct() %>%
  left_join(PONS_Demographics)  %>%
  group_by(age) %>% count()  %>% arrange(-n))

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
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

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6) ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12) ,1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter( Drop95==1|Drop90==1|Drop2_20==1) %>% select(patid) %>% distinct()
New_Cachexia_Pred$Group <- "Cachexia"


data.frame(PONS_Demographics %>% left_join(New_Cachexia_Pred) %>% mutate(Group=ifelse(is.na(Group),"none", Group)) %>%
             filter(Group=="Cachexia") %>%
             group_by(diagnosis, age) %>% 
             mutate(age=as.factor(age)) %>% summarise(n=sum(weight)) %>%
             spread(key=age, value=n))

# ---------------------------

# BIM drop vs survival -------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)

# Death/Survival
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_onset, death_date)

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date)

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

New_Primary_Cancer_Box <- PONS_Demographics_temp %>% inner_join(New_Primary_Cancer_Box)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% mutate(Exact_Month=ifelse(is.na(Exact_Month),60,Exact_Month))





temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp %>% select(patid, Month_Min, Diff) %>% inner_join(New_Primary_Cancer_Box %>% select(patid, Exact_Month)) %>%
  mutate(DropToDeath=Exact_Month-Month_Min) %>%
  filter(Exact_Month != 60) %>%
  filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>%
   ggplot(aes(100*abs(Diff), DropToDeath)) +
  geom_smooth(fill="deepskyblue4", colour="darkorange")+
  theme_minimal() +
  xlab("\n MAX Abs. BMI Drop") + ylab("No. Months Survived \n")


# temp <- temp %>% select(patid, Diff)

temp %>% inner_join(New_Primary_Cancer_Box) %>%
   filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>%
  ggplot(aes(100*abs(Diff), Survived)) +
  geom_smooth(fill="firebrick")+
  theme_minimal() +
  xlab("\n MAX Abs. BMI Drop") + ylab("No. Months Survived \n")


tomodel <- temp %>% inner_join(New_Primary_Cancer_Box) %>% 
  filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>%
  select(Diff, Survived) %>% mutate(Diff=(100*abs(Diff)))

summary(lm(Survived~Diff, data=tomodel))



tomodel <- temp %>% select(patid, Month_Min, Diff) %>% inner_join(New_Primary_Cancer_Box %>% select(patid, Exact_Month)) %>%
  mutate(DropToDeath=Exact_Month-Month_Min) %>%
  filter(Exact_Month != 60) %>%
  filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>%
    select(Diff, DropToDeath) %>% mutate(Diff=(100*abs(Diff)))

summary(lm(DropToDeath~Diff, data=tomodel))


# -----------------------------------------------
# NEW CLASSES - BREAST CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))



temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp <- temp %>% select(patid, Diff) %>% mutate(Diff=100*abs(Diff))



CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1


PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)) %>%
  select(patient, Exp, chemo_class) %>% distinct()

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

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- temp %>% rename("patient"="patid") %>% inner_join(CAN_Drug_Histories)

temp2 <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))


model_bayes<-  stan_glm(Diff ~., data=temp2, seed=111, iter=5000, chains=10)


print(model_bayes, digits = 3)




mcmc_dens(model_bayes, pars = c("`PD1/PDL1`"))+
  vline_at(3.558      , col="red")

# stan_glm
#  family:       gaussian [identity]
#  formula:      Diff ~ .
#  observations: 31044
#  predictors:   13

#                           Median MAD_SD
# (Intercept)               10.783  0.135
# `Alkylating Agent`        -1.736  0.273
# Antimetabolites            2.293  0.205
# `Antimicrotubule Agent`    1.178  0.213
# Biologic                   0.873  0.185
# `Hormonal Therapy`        -0.084  0.129
# `Immuno/Targeted`          2.445  0.208
# `Other Antineoplastics`    0.659  0.201
# `PD1/PDL1`                 3.558  0.489
# `Platinum agent`          -0.103  0.250
# Radio                      0.106  0.106
# `Surgery Inpatient`        1.169  0.196
# `Topoisomerase Inhibitor`  1.439  0.251
# 
# Auxiliary parameter(s):
#       Median MAD_SD
# sigma 9.203  0.036 
# 

  
describe_posterior(model_bayes)

  Parameter                 | Median |         95% CI |     pd |          ROPE | % in ROPE |  Rhat |      ESS
-----------------------------------------------------------------------------------------------------------
(Intercept)               |  10.78 | [10.51, 11.05] |   100% | [-0.10, 0.10] |        0% | 1.000 | 41044.00
`Alkylating Agent`        |  -1.74 | [-2.26, -1.21] |   100% | [-0.10, 0.10] |        0% | 1.000 | 19558.00
Antimetabolites           |   2.29 | [ 1.89,  2.69] |   100% | [-0.10, 0.10] |        0% | 1.000 | 43977.00
`Antimicrotubule Agent`   |   1.18 | [ 0.76,  1.60] |   100% | [-0.10, 0.10] |        0% | 1.000 | 23299.00
Biologic                  |   0.87 | [ 0.51,  1.23] |   100% | [-0.10, 0.10] |        0% | 1.000 | 35570.00
`Hormonal Therapy`        |  -0.08 | [-0.34,  0.17] | 73.98% | [-0.10, 0.10] |    49.55% | 1.000 | 45557.00
`Immuno/Targeted`         |   2.44 | [ 2.03,  2.85] |   100% | [-0.10, 0.10] |        0% | 1.000 | 44294.00
`Other Antineoplastics`   |   0.66 | [ 0.27,  1.05] | 99.96% | [-0.10, 0.10] |        0% | 1.000 | 46738.00
`PD1/PDL1`                |   3.56 | [ 2.59,  4.52] |   100% | [-0.10, 0.10] |        0% | 1.000 | 44549.00
`Platinum agent`          |  -0.10 | [-0.59,  0.38] | 66.02% | [-0.10, 0.10] |    30.17% | 1.000 | 30764.00
Radio                     |   0.11 | [-0.11,  0.32] | 83.73% | [-0.10, 0.10] |    47.29% | 1.000 | 50094.00
`Surgery Inpatient`       |   1.17 | [ 0.78,  1.55] |   100% | [-0.10, 0.10] |        0% | 1.000 | 48474.00
`Topoisomerase Inhibitor` |   1.44 | [ 0.94,  1.92] |   100% | [-0.10, 0.10] |        0% | 1.000 | 27967.00


post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)

             (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  10.7852                   -1.7411                    2.2741                    1.1745 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                   0.8846                   -0.0898                    2.4303                    0.6679 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                   3.5452                   -0.1004                    0.1228                    1.2023 
`Topoisomerase Inhibitor` 
                   1.4490 
                   
hdi(model_bayes)

Highest Density Interval 

Parameter                 |        95% HDI
------------------------------------------
(Intercept)               | [10.51, 11.05]
`Alkylating Agent`        | [-2.27, -1.21]
Antimetabolites           | [ 1.88,  2.68]
`Antimicrotubule Agent`   | [ 0.75,  1.59]
Biologic                  | [ 0.51,  1.23]
`Hormonal Therapy`        | [-0.34,  0.17]
`Immuno/Targeted`         | [ 2.04,  2.85]
`Other Antineoplastics`   | [ 0.27,  1.05]
`PD1/PDL1`                | [ 2.61,  4.54]
`Platinum agent`          | [-0.59,  0.37]
Radio                     | [-0.10,  0.33]
`Surgery Inpatient`       | [ 0.79,  1.56]
`Topoisomerase Inhibitor` | [ 0.96,  1.94]

eti(model_bayes)

Equal-Tailed Interval

Parameter                 |        95% ETI | Effects |   Component
------------------------------------------------------------------
(Intercept)               | [10.51, 11.05] |   fixed | conditional
`Alkylating Agent`        | [-2.26, -1.21] |   fixed | conditional
Antimetabolites           | [ 1.89,  2.69] |   fixed | conditional
`Antimicrotubule Agent`   | [ 0.76,  1.60] |   fixed | conditional
Biologic                  | [ 0.51,  1.23] |   fixed | conditional
`Hormonal Therapy`        | [-0.34,  0.17] |   fixed | conditional
`Immuno/Targeted`         | [ 2.03,  2.85] |   fixed | conditional
`Other Antineoplastics`   | [ 0.27,  1.05] |   fixed | conditional
`PD1/PDL1`                | [ 2.59,  4.52] |   fixed | conditional
`Platinum agent`          | [-0.59,  0.38] |   fixed | conditional
Radio                     | [-0.11,  0.32] |   fixed | conditional
`Surgery Inpatient`       | [ 0.78,  1.55] |   fixed | conditional
`Topoisomerase Inhibitor` | [ 0.94,  1.92] |   fixed | conditional

# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)

                (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  1.00000                   1.00000                   1.00000                   1.00000 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                  1.00000                   0.73980                   1.00000                   0.99956 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                  1.00000                   0.66020                   0.83732                   1.00000 
`Topoisomerase Inhibitor` 
                  1.00000 
                  
# p−value=1−pd
1- purrr::map_dbl(post, p_direction)

              (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  0.00000                   0.00000                   0.00000                   0.00000 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                  0.00000                   0.26020                   0.00000                   0.00044 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                  0.00000                   0.33980                   0.16268                   0.00000 
`Topoisomerase Inhibitor` 
                  0.00000 
    
    
plot <- fread("temp.txt", sep="|")
data.frame(plot[,1:5])

plot$CI_low <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,1])
plot$CI_high <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,2])

plot <- plot %>% select(1,2,9,10)

#plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot

plot2 <- plot2 %>% arrange(-Median)


plot2$Parameter <- c("Baseline Reduction", "Anastrozole", "Azacitidine", "Capecitabine", "Docetaxel", "Doxorubicin", "Eribulin", "Etoposide", "Everolimus", "Fulvestrant", "Gemcitabine", "Lenalidomide", "Methotrexate", "Paclitaxel", "Palbociclib", "Pemetrexed", "Ado-Trastuzumab", "Atezolizumab", "Bevacizumab", "Nivolumab", "Pembrolizumab", "Rituximab", "External", "Systemic Radiotherapy", "Major Surgery")
  
plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="deepskyblue4", size=3, alpha=0.7) +
  geom_point(colour="darkgoldenrod1", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Breast Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()
    






# NEW CLASSES - PROSTATE CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Prostate Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))



temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp <- temp %>% select(patid, Diff) %>% mutate(Diff=100*abs(Diff))



CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1


PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)) %>%
  select(patient, Exp, chemo_class) %>% distinct()

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

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- temp %>% rename("patient"="patid") %>% inner_join(CAN_Drug_Histories)

temp2 <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))


model_bayes<-  stan_glm(Diff ~., data=temp2, seed=111, iter=5000, chains=10)


print(model_bayes, digits = 3)

stan_glm
 family:       gaussian [identity]
 formula:      Diff ~ .
 observations: 16899
 predictors:   13
------
                          Median MAD_SD
(Intercept)               10.525  0.167
`Alkylating Agent`         2.766  0.647
Antimetabolites            1.044  0.345
`Antimicrotubule Agent`    3.006  0.346
Biologic                   1.773  0.281
`Hormonal Therapy`         1.363  0.154
`Immuno/Targeted`          1.314  0.338
`Other Antineoplastics`    1.060  0.245
`PD1/PDL1`                 0.769  0.557
`Platinum agent`           3.999  0.492
Radio                     -1.053  0.147
`Surgery Inpatient`        0.067  0.191
`Topoisomerase Inhibitor` -0.341  0.663

Auxiliary parameter(s):
      Median MAD_SD
sigma 9.177  0.050 

------


mcmc_dens(model_bayes, pars = c("`PD1/PDL1`"))+
  vline_at(3.558      , col="red")

  
describe_posterior(model_bayes)

Parameter                 | Median |         95% CI |      pd |          ROPE | % in ROPE |  Rhat |      ESS
------------------------------------------------------------------------------------------------------------
(Intercept)               |  10.53 | [10.19, 10.86] |    100% | [-0.10, 0.10] |        0% | 1.000 | 25513.00
`Alkylating Agent`        |   2.77 | [ 1.47,  4.05] |    100% | [-0.10, 0.10] |        0% | 1.000 | 31143.00
Antimetabolites           |   1.04 | [ 0.37,  1.72] |  99.87% | [-0.10, 0.10] |        0% | 1.000 | 31313.00
`Antimicrotubule Agent`   |   3.01 | [ 2.32,  3.68] |    100% | [-0.10, 0.10] |        0% | 1.000 | 30170.00
Biologic                  |   1.77 | [ 1.22,  2.33] |    100% | [-0.10, 0.10] |        0% | 1.000 | 31224.00
`Hormonal Therapy`        |   1.36 | [ 1.06,  1.67] |    100% | [-0.10, 0.10] |        0% | 1.000 | 29449.00
`Immuno/Targeted`         |   1.31 | [ 0.65,  1.97] | 100.00% | [-0.10, 0.10] |        0% | 1.000 | 31746.00
`Other Antineoplastics`   |   1.06 | [ 0.57,  1.54] |    100% | [-0.10, 0.10] |        0% | 1.000 | 33703.00
`PD1/PDL1`                |   0.77 | [-0.30,  1.84] |  91.98% | [-0.10, 0.10] |     5.79% | 1.000 | 32435.00
`Platinum agent`          |   4.00 | [ 3.04,  4.97] |    100% | [-0.10, 0.10] |        0% | 1.000 | 27935.00
Radio                     |  -1.05 | [-1.35, -0.76] |    100% | [-0.10, 0.10] |        0% | 1.000 | 31270.00
`Surgery Inpatient`       |   0.07 | [-0.31,  0.44] |  63.61% | [-0.10, 0.10] |    39.64% | 1.000 | 29924.00
`Topoisomerase Inhibitor` |  -0.34 | [-1.63,  0.94] |  69.85% | [-0.10, 0.10] |    11.07% | 1.000 | 31352.00

post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)

              (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  10.5302                    2.7461                    1.0709                    3.0114 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                   1.7858                    1.3616                    1.3084                    1.0651 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                   0.7603                    3.9562                   -1.0522                    0.0748 
`Topoisomerase Inhibitor` 
                  -0.2750 


hdi(model_bayes)

Parameter                 |        95% HDI
------------------------------------------
(Intercept)               | [10.19, 10.85]
`Alkylating Agent`        | [ 1.47,  4.04]
Antimetabolites           | [ 0.35,  1.70]
`Antimicrotubule Agent`   | [ 2.32,  3.69]
Biologic                  | [ 1.22,  2.33]
`Hormonal Therapy`        | [ 1.06,  1.67]
`Immuno/Targeted`         | [ 0.65,  1.97]
`Other Antineoplastics`   | [ 0.58,  1.54]
`PD1/PDL1`                | [-0.29,  1.85]
`Platinum agent`          | [ 3.02,  4.95]
Radio                     | [-1.34, -0.76]
`Surgery Inpatient`       | [-0.31,  0.43]
`Topoisomerase Inhibitor` | [-1.65,  0.91]

eti(model_bayes)

Equal-Tailed Interval

Parameter                 |        95% ETI | Effects |   Component
------------------------------------------------------------------
(Intercept)               | [10.19, 10.86] |   fixed | conditional
`Alkylating Agent`        | [ 1.47,  4.05] |   fixed | conditional
Antimetabolites           | [ 0.37,  1.72] |   fixed | conditional
`Antimicrotubule Agent`   | [ 2.32,  3.68] |   fixed | conditional
Biologic                  | [ 1.22,  2.33] |   fixed | conditional
`Hormonal Therapy`        | [ 1.06,  1.67] |   fixed | conditional
`Immuno/Targeted`         | [ 0.65,  1.97] |   fixed | conditional
`Other Antineoplastics`   | [ 0.57,  1.54] |   fixed | conditional
`PD1/PDL1`                | [-0.30,  1.84] |   fixed | conditional
`Platinum agent`          | [ 3.04,  4.97] |   fixed | conditional
Radio                     | [-1.35, -0.76] |   fixed | conditional
`Surgery Inpatient`       | [-0.31,  0.44] |   fixed | conditional
`Topoisomerase Inhibitor` | [-1.63,  0.94] |   fixed | conditional

# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)

              (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  1.00000                   1.00000                   0.99868                   1.00000 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                  1.00000                   1.00000                   0.99996                   1.00000 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                  0.91984                   1.00000                   1.00000                   0.63612 
`Topoisomerase Inhibitor` 
                  0.69848 
                  
# p−value=1−pd
1- purrr::map_dbl(post, p_direction)

              (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  0.00000                   0.00000                   0.00132                   0.00000 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                  0.00000                   0.00000                   0.00004                   0.00000 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                  0.08016                   0.00000                   0.00000                   0.36388 
`Topoisomerase Inhibitor` 
                  0.30152 
    
    
plot <- fread("temp.txt", sep="|")
data.frame(plot[,1:5])

plot$CI_low <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,1])
plot$CI_high <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,2])

plot <- plot %>% select(1,2,9,10)

#plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot

plot2 <- plot2 %>% arrange(-Median)



plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="deepskyblue4", size=3, alpha=0.7) +
  geom_point(colour="darkgoldenrod1", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Prostate Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()
    





# NEW CLASSES - INTESTINAL CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))



temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp <- temp %>% select(patid, Diff) %>% mutate(Diff=100*abs(Diff))



CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1


PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)) %>%
  select(patient, Exp, chemo_class) %>% distinct()

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

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- temp %>% rename("patient"="patid") %>% inner_join(CAN_Drug_Histories)

temp2 <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))


model_bayes<-  stan_glm(Diff ~., data=temp2, seed=111, iter=5000, chains=10)


print(model_bayes, digits = 3)

stan_glm
 family:       gaussian [identity]
 formula:      Diff ~ .
 observations: 8311
 predictors:   13
------
                          Median MAD_SD
(Intercept)               12.967  0.260
`Alkylating Agent`         0.232  0.993
Antimetabolites            0.169  0.331
`Antimicrotubule Agent`    1.702  0.708
Biologic                   1.609  0.344
`Hormonal Therapy`         0.272  0.427
`Immuno/Targeted`          0.851  0.423
`Other Antineoplastics`    0.718  0.297
`PD1/PDL1`                 3.170  0.665
`Platinum agent`           0.374  0.325
Radio                      0.749  0.253
`Surgery Inpatient`        0.834  0.244
`Topoisomerase Inhibitor`  1.619  0.402

Auxiliary parameter(s):
      Median MAD_SD
sigma 10.629  0.081

------


mcmc_dens(model_bayes, pars = c("`PD1/PDL1`"))+
  vline_at(3.558      , col="red")

  
describe_posterior(model_bayes)

Parameter                 | Median |         95% CI |     pd |          ROPE | % in ROPE |  Rhat |      ESS
-----------------------------------------------------------------------------------------------------------
(Intercept)               |  12.97 | [12.45, 13.48] |   100% | [-0.10, 0.10] |        0% | 1.000 | 33671.00
`Alkylating Agent`        |   0.23 | [-1.74,  2.18] | 59.29% | [-0.10, 0.10] |     8.35% | 1.000 | 37840.00
Antimetabolites           |   0.17 | [-0.48,  0.81] | 69.36% | [-0.10, 0.10] |    21.52% | 1.000 | 30513.00
`Antimicrotubule Agent`   |   1.70 | [ 0.34,  3.08] | 99.21% | [-0.10, 0.10] |        0% | 1.000 | 36287.00
Biologic                  |   1.61 | [ 0.93,  2.28] |   100% | [-0.10, 0.10] |        0% | 1.000 | 32638.00
`Hormonal Therapy`        |   0.27 | [-0.57,  1.12] | 74.02% | [-0.10, 0.10] |    15.85% | 1.000 | 41312.00
`Immuno/Targeted`         |   0.85 | [ 0.00,  1.69] | 97.56% | [-0.10, 0.10] |     1.56% | 1.000 | 39421.00
`Other Antineoplastics`   |   0.72 | [ 0.13,  1.31] | 99.12% | [-0.10, 0.10] |        0% | 1.000 | 36281.00
`PD1/PDL1`                |   3.17 | [ 1.90,  4.46] |   100% | [-0.10, 0.10] |        0% | 1.000 | 40852.00
`Platinum agent`          |   0.37 | [-0.26,  1.01] | 87.34% | [-0.10, 0.10] |    13.50% | 1.000 | 31030.00
Radio                     |   0.75 | [ 0.25,  1.25] | 99.82% | [-0.10, 0.10] |        0% | 1.000 | 38421.00
`Surgery Inpatient`       |   0.83 | [ 0.36,  1.31] | 99.95% | [-0.10, 0.10] |        0% | 1.000 | 37643.00
`Topoisomerase Inhibitor` |   1.62 | [ 0.82,  2.43] |   100% | [-0.10, 0.10] |        0% | 1.000 | 31570.00
post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)

              (Intercept)        `Alkylating Agent` 
                  10.5302                    2.7461 
          Antimetabolites   `Antimicrotubule Agent` 
                   1.0709                    3.0114 
                 Biologic        `Hormonal Therapy` 
                   1.7858                    1.3616 
        `Immuno/Targeted`   `Other Antineoplastics` 
                   1.3084                    1.0651 
               `PD1/PDL1`          `Platinum agent` 
                   0.7603                    3.9562 
                    Radio       `Surgery Inpatient` 
                  -1.0522                    0.0748 
`Topoisomerase Inhibitor` 
                  -0.2750 


hdi(model_bayes)

Parameter                 |        95% HDI
------------------------------------------
(Intercept)               | [12.45, 13.47]
`Alkylating Agent`        | [-1.72,  2.20]
Antimetabolites           | [-0.46,  0.82]
`Antimicrotubule Agent`   | [ 0.34,  3.08]
Biologic                  | [ 0.95,  2.30]
`Hormonal Therapy`        | [-0.57,  1.12]
`Immuno/Targeted`         | [-0.01,  1.67]
`Other Antineoplastics`   | [ 0.12,  1.30]
`PD1/PDL1`                | [ 1.86,  4.42]
`Platinum agent`          | [-0.27,  1.00]
Radio                     | [ 0.25,  1.24]
`Surgery Inpatient`       | [ 0.35,  1.30]
`Topoisomerase Inhibitor` | [ 0.82,  2.42]

eti(model_bayes)

Equal-Tailed Interval

Parameter                 |        95% ETI | Effects |   Component
------------------------------------------------------------------
(Intercept)               | [12.45, 13.48] |   fixed | conditional
`Alkylating Agent`        | [-1.74,  2.18] |   fixed | conditional
Antimetabolites           | [-0.48,  0.81] |   fixed | conditional
`Antimicrotubule Agent`   | [ 0.34,  3.08] |   fixed | conditional
Biologic                  | [ 0.93,  2.28] |   fixed | conditional
`Hormonal Therapy`        | [-0.57,  1.12] |   fixed | conditional
`Immuno/Targeted`         | [ 0.00,  1.69] |   fixed | conditional
`Other Antineoplastics`   | [ 0.13,  1.31] |   fixed | conditional
`PD1/PDL1`                | [ 1.90,  4.46] |   fixed | conditional
`Platinum agent`          | [-0.26,  1.01] |   fixed | conditional
Radio                     | [ 0.25,  1.25] |   fixed | conditional
`Surgery Inpatient`       | [ 0.36,  1.31] |   fixed | conditional
`Topoisomerase Inhibitor` | [ 0.82,  2.43] |   fixed | conditional

# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)

              (Intercept)        `Alkylating Agent` 
                  1.00000                   1.00000 
          Antimetabolites   `Antimicrotubule Agent` 
                  0.99868                   1.00000 
                 Biologic        `Hormonal Therapy` 
                  1.00000                   1.00000 
        `Immuno/Targeted`   `Other Antineoplastics` 
                  0.99996                   1.00000 
               `PD1/PDL1`          `Platinum agent` 
                  0.91984                   1.00000 
                    Radio       `Surgery Inpatient` 
                  1.00000                   0.63612 
`Topoisomerase Inhibitor` 
                  0.69848 
                  
# p−value=1−pd
1- purrr::map_dbl(post, p_direction)

                  0.00000                   0.00000 
          Antimetabolites   `Antimicrotubule Agent` 
                  0.00132                   0.00000 
                 Biologic        `Hormonal Therapy` 
                  0.00000                   0.00000 
        `Immuno/Targeted`   `Other Antineoplastics` 
                  0.00004                   0.00000 
               `PD1/PDL1`          `Platinum agent` 
                  0.08016                   0.00000 
                    Radio       `Surgery Inpatient` 
                  0.00000                   0.36388 
`Topoisomerase Inhibitor` 
                  0.30152 
    
    
plot <- fread("temp.txt", sep="|")
data.frame(plot[,1:5])

plot$CI_low <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,1])
plot$CI_high <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,2])

plot <- plot %>% select(1,2,9,10)

#plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot

plot2 <- plot2 %>% arrange(-Median)



plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="deepskyblue4", size=3, alpha=0.7) +
  geom_point(colour="darkgoldenrod1", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Intestinal Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()
    







# NEW CLASSES - LUNG CANCER - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Lung Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))



temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp <- temp %>% select(patid, Diff) %>% mutate(Diff=100*abs(Diff))



CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1


PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)) %>%
  select(patient, Exp, chemo_class) %>% distinct()

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

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- temp %>% rename("patient"="patid") %>% inner_join(CAN_Drug_Histories)

temp2 <- CAN_Drug_Histories %>% ungroup() %>% select(-patient)

suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))


model_bayes<-  stan_glm(Diff ~., data=temp2, seed=111, iter=5000, chains=10)


print(model_bayes, digits = 3)

stan_glm
 family:       gaussian [identity]
 formula:      Diff ~ .
 observations: 10780
 predictors:   13
------
                          Median MAD_SD
(Intercept)               13.982  0.222
`Alkylating Agent`         0.054  0.801
Antimetabolites            1.081  0.293
`Antimicrotubule Agent`    1.382  0.319
Biologic                   1.018  0.325
`Hormonal Therapy`         0.195  0.485
`Immuno/Targeted`         -0.039  0.335
`Other Antineoplastics`    0.573  0.290
`PD1/PDL1`                 1.315  0.244
`Platinum agent`          -0.540  0.333
Radio                      1.367  0.220
`Surgery Inpatient`       -0.507  0.230
`Topoisomerase Inhibitor`  1.663  0.380

Auxiliary parameter(s):
      Median MAD_SD
sigma 10.531  0.071

------


mcmc_dens(model_bayes, pars = c("`PD1/PDL1`"))+
  vline_at(3.558      , col="red")

  
describe_posterior(model_bayes)

Parameter                 | Median |         95% CI |     pd |          ROPE | % in ROPE |  Rhat |      ESS
-----------------------------------------------------------------------------------------------------------
(Intercept)               |  13.98 | [13.55, 14.41] |   100% | [-0.10, 0.10] |        0% | 1.000 | 33632.00
`Alkylating Agent`        |   0.05 | [-1.50,  1.62] | 52.74% | [-0.10, 0.10] |    10.78% | 1.000 | 33133.00
Antimetabolites           |   1.08 | [ 0.50,  1.64] | 99.99% | [-0.10, 0.10] |        0% | 1.000 | 24986.00
`Antimicrotubule Agent`   |   1.38 | [ 0.76,  2.00] |   100% | [-0.10, 0.10] |        0% | 1.000 | 24257.00
Biologic                  |   1.02 | [ 0.38,  1.65] | 99.90% | [-0.10, 0.10] |        0% | 1.000 | 32919.00
`Hormonal Therapy`        |   0.19 | [-0.77,  1.15] | 65.68% | [-0.10, 0.10] |    16.05% | 1.000 | 36933.00
`Immuno/Targeted`         |  -0.04 | [-0.70,  0.61] | 54.67% | [-0.10, 0.10] |    24.45% | 1.000 | 34709.00
`Other Antineoplastics`   |   0.57 | [ 0.00,  1.15] | 97.51% | [-0.10, 0.10] |     2.88% | 1.000 | 35741.00
`PD1/PDL1`                |   1.32 | [ 0.84,  1.79] |   100% | [-0.10, 0.10] |        0% | 1.000 | 34697.00
`Platinum agent`          |  -0.54 | [-1.18,  0.11] | 94.80% | [-0.10, 0.10] |     7.17% | 1.000 | 20741.00
Radio                     |   1.37 | [ 0.94,  1.79] |   100% | [-0.10, 0.10] |        0% | 1.000 | 36879.00
`Surgery Inpatient`       |  -0.51 | [-0.96, -0.05] | 98.53% | [-0.10, 0.10] |     1.52% | 1.000 | 34991.00
`Topoisomerase Inhibitor` |   1.66 | [ 0.91,  2.41] |   100% | [-0.10, 0.10] |        0% | 1.000 | 23437.00



post <- get_parameters(model_bayes)

print(purrr::map_dbl(post,map_estimate),digits = 3)

             (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                13.981954                  0.000168                  1.109741                  1.385826 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                 1.013318                  0.217666                 -0.039431                  0.573444 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                 1.327205                 -0.543127                  1.342806                 -0.528538 
`Topoisomerase Inhibitor` 
                 1.684264 

hdi(model_bayes)

Parameter                 |        95% HDI
------------------------------------------
(Intercept)               | [13.55, 14.41]
`Alkylating Agent`        | [-1.49,  1.63]
Antimetabolites           | [ 0.51,  1.64]
`Antimicrotubule Agent`   | [ 0.76,  2.00]
Biologic                  | [ 0.38,  1.65]
`Hormonal Therapy`        | [-0.75,  1.17]
`Immuno/Targeted`         | [-0.71,  0.59]
`Other Antineoplastics`   | [ 0.00,  1.15]
`PD1/PDL1`                | [ 0.84,  1.79]
`Platinum agent`          | [-1.17,  0.12]
Radio                     | [ 0.94,  1.79]
`Surgery Inpatient`       | [-0.95, -0.04]
`Topoisomerase Inhibitor` | [ 0.92,  2.41]


eti(model_bayes)

Equal-Tailed Interval

Parameter                 |        95% ETI | Effects |   Component
------------------------------------------------------------------
(Intercept)               | [13.55, 14.41] |   fixed | conditional
`Alkylating Agent`        | [-1.50,  1.62] |   fixed | conditional
Antimetabolites           | [ 0.50,  1.64] |   fixed | conditional
`Antimicrotubule Agent`   | [ 0.76,  2.00] |   fixed | conditional
Biologic                  | [ 0.38,  1.65] |   fixed | conditional
`Hormonal Therapy`        | [-0.77,  1.15] |   fixed | conditional
`Immuno/Targeted`         | [-0.70,  0.61] |   fixed | conditional
`Other Antineoplastics`   | [ 0.00,  1.15] |   fixed | conditional
`PD1/PDL1`                | [ 0.84,  1.79] |   fixed | conditional
`Platinum agent`          | [-1.18,  0.11] |   fixed | conditional
Radio                     | [ 0.94,  1.79] |   fixed | conditional
`Surgery Inpatient`       | [-0.96, -0.05] |   fixed | conditional
`Topoisomerase Inhibitor` | [ 0.91,  2.41] |   fixed | conditional

# pd statistic in the above table, 
  # high value means that the associated effect is concentrated on the same side as the median
  
  map_dbl(post, p_direction)

              (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  1.00000                   0.52736                   0.99992                   1.00000 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                  0.99904                   0.65680                   0.54672                   0.97512 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                  1.00000                   0.94800                   1.00000                   0.98532 
`Topoisomerase Inhibitor` 
                  1.00000 
                  
# p−value=1−pd

 1- purrr::map_dbl(post, p_direction)
              (Intercept)        `Alkylating Agent`           Antimetabolites   `Antimicrotubule Agent` 
                  0.00000                   0.47264                   0.00008                   0.00000 
                 Biologic        `Hormonal Therapy`         `Immuno/Targeted`   `Other Antineoplastics` 
                  0.00096                   0.34320                   0.45328                   0.02488 
               `PD1/PDL1`          `Platinum agent`                     Radio       `Surgery Inpatient` 
                  0.00000                   0.05200                   0.00000                   0.01468 
`Topoisomerase Inhibitor` 
                  0.00000
    
    
plot <- fread("temp.txt", sep="|")
data.frame(plot[,1:5])

plot$CI_low <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,1])
plot$CI_high <- parse_number(str_split_fixed( plot$`95% CI`, ",", 2)[,2])

plot <- plot %>% select(1,2,9,10)

#plot <- plot %>% filter(CI_low>0&CI_high>0) 
plot2 <- plot

plot2 <- plot2 %>% arrange(-Median)



plot2 %>%
ggplot(aes(y=fct_reorder(Parameter, Median), x=Median, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="deepskyblue4", size=3, alpha=0.7) +
  geom_point(colour="darkgoldenrod1", size=5, alpha=0.7) + 
  labs(title='Effect Size on BMI Reduction (Lung Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()
    

# ---------------
# Excel nancy class penetrance ---------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep = "\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer != "-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight)


CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by = c("patid" = "patient"))
sum(CAN_Drug_Histories$weight)  # 9861087

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia

CAN_Drug_Histories <- CachexiaPats_ALL_NEW %>% inner_join(CAN_Drug_Histories)

CAN_Drug_Histories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 939321.4

PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())

unique(PONS_Ingredients_JN_ChemoClass$chemo_class)

string_Oncology        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$drug_id[
  PONS_Ingredients_JN_ChemoClass$chemo_class == "Hormonal Therapy"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Immuno/Targeted"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Biologic"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Alkylating Agent"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Other Antineoplastics"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimetabolites"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimicrotubule Agent"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Topoisomerase Inhibitor"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimetabolites"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimicrotubule Agent"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Topoisomerase Inhibitor"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "PD1/PDL1"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Radio"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Platinum agent"|
    PONS_Ingredients_JN_ChemoClass$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Oncology, Drugs))
CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patid) %>% filter(Month==max(Month))
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)


PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(drug_id, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class))

data.frame(CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Drugs" ="drug_id")) %>%
  select(patid, weight, drug_class) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(weight)/939321.4))

# ------------------------------------------------
# Who is diagnosing Cachexia ? -----------------------------------------------

PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cachexia_onset)
PONS_Demographics_temp <- PONS_Demographics_temp %>% drop_na()
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight)


PONS_Events <- fread("PONS Events.txt")
PONS_Events <- PONS_Events %>% inner_join(PONS_Demographics_temp)
PONS_Events <- PONS_Events %>% filter(grepl("R64", code))

PONS_Specialty_Codes <- fread("PONS Specialty Codes.txt")
PONS_Specialty_Codes <- PONS_Specialty_Codes %>% select(code, specialty)

PONS_Event_Claims_Providers <- fread("PONS Event Claims Providers.txt")
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers %>% select(prov, specialty_classification)

length(unique(PONS_Events$patid))

PONS_Events %>% drop_na() %>% filter(prov!="") %>% group_by(patid) %>%
  mutate(claimed=as.Date(claimed)) %>% filter(claimed==min(claimed)) %>% slice(1) %>%
  left_join(PONS_Event_Claims_Providers) %>% 
  filter(specialty_classification != "Facility") %>%
  ungroup() %>% group_by(specialty_classification) %>% summarise(n=sum(weight)/479957)


 1 Anesthesiologist         0.00775 
 2 Anesthetist Assistant    0.00205 
 3 Hematologist             0.00448 
 4 Institutional Care       0.00879 
 5 Internal Medicine        0.389   
 6 Nutrition Specialist     0.00128 
 7 Oncologist               0.0857  
 8 Other Physician          0.134   
 9 Other Provider           0.0741  
10 Pain Specialist          0.0154  
11 Palliative Medicine      0.00612 
12 Pharmacist               0.0115  
13 Primary Care             0.118   
14 Radiologist              0.0141  
15 Surgeon                  0.0353  
16 Surgical Assistant       0.000586
17 Unknown                  0.0917

# -------------------------------------
# BMI evolution before/after start ----------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Nutrition <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Appetite Stimulant"], collapse = "|"),")\\b")


CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CancerDrug_Experienced %>% select(patid) %>% inner_join(CAN_Drug_Histories)


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Nutrition_Pats <- CAN_Drug_Histories %>% filter(grepl(string_Nutrition, Drugs)) %>% select(patid) %>% distinct()


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- Cannabinoid_Pats %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Drugs=ifelse(grepl(string_Nutrition, Drugs),1,0))
names(CAN_Drug_Histories)[4] <- "Nutrition"
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Min_Nutrition <- CAN_Drug_Histories %>% filter(Nutrition==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month)
names(Min_Nutrition)[2] <- "First_Nutrition"

CAN_Drug_Histories <- Min_Nutrition %>% select(patid) %>% left_join(CAN_Drug_Histories)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)


PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- Min_Nutrition %>% select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value)
PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed))
PONS_Measures <- PONS_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))
PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(patid, value, Exact_Month) %>% distinct()
PONS_Measures <- PONS_Measures %>% group_by(patid, Exact_Month) %>% summarise(n=mean(value))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Measures, by=c("patid"="patid", "Month"="Exact_Month"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(Min_Nutrition)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Month=Month-First_Nutrition)

CAN_Drug_Histories <- CAN_Drug_Histories %>% drop_na()


CAN_Drug_Histories %>% mutate(period=ifelse(Month<=0,"Before","After")) %>%
  group_by(patid,period) %>% summarise(N=mean(n)) %>%
  spread(key=period, value=N) %>%
  drop_na() %>%
  mutate(Diff=After-Before) %>%
  ungroup() %>% summarise(n=mean(Diff))

CAN_Drug_Histories %>%
  group_by(Month, Nutrition) %>% summarise(n=mean(n)) %>%
  #mutate(Month=Month+10) %>%
  #filter(Month>(-24)&Month<24) %>%
  ggplot(aes(Month, n)) +
  ylim(20,30) +
  geom_smooth(fill="firebrick", colour="darkred", size=2, alpha=0.5) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n Elapsed Time (months) to/from Nutrition Initiation") +
  ylab("Average Monthly BMI (kg/m2) \n")



# --------------------------------
# NEW CLASSES - UNIVARIATE - BAYESIAN MODEL/REGRESSION ---------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Intestinal Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))


temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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
temp <- temp %>% select(patid, Diff) %>% mutate(Diff=100*abs(Diff))


CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs, weight)
CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, Drugs) %>% count() %>% filter(n>=3) %>%
  select(patient, Drugs) %>% distinct()
CAN_Drug_Histories$Exp <- 1


PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% mutate(drug_id=row_number())
names(PONS_Ingredients_JN_ChemoClass)[1] <- "Drugs"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(Drugs, chemo_class)) %>%
  select(patient, Exp, chemo_class) %>% distinct()

CAN_Drug_Histories <- CAN_Drug_Histories %>% ungroup() %>%  select(patient, Exp, chemo_class) %>% distinct()
  
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

CAN_Drug_Histories <- CAN_Drug_Histories %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

CAN_Drug_Histories <- temp %>% rename("patient"="patid") %>% inner_join(CAN_Drug_Histories)

temp2 <- CAN_Drug_Histories %>% ungroup() %>% select(-patient) %>% select(1,13)

suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))

model_bayes<-  stan_glm(Diff ~., data=temp2, seed=111, iter=5000, chains=10)
print(model_bayes, digits = 3)
describe_posterior(model_bayes)



plot <- fread("BreastUni.txt")
plot <- plot %>% select(1,2,4)

plot$CI_low <- parse_number(str_split_fixed( plot$CI, ",", 2)[,1])
plot$CI_high <- parse_number(str_split_fixed( plot$CI, ",", 2)[,2])

plot <- plot %>% select(1,2,4,5) %>% filter(Breast != "`Surgery Inpatient`")

 
plot2 <- plot




plot2 %>%
ggplot(aes(y=fct_reorder(Breast , coeff), x=coeff, xmin=CI_low, xmax=CI_high)) +
  geom_errorbarh(height=.1, colour="brown3", size=3, alpha=0.7) +
  geom_point(colour="darkslategray", size=5, alpha=0.7) + 
  labs(title='Univariate Bayesian Regression - Effect Size on BMI Reduction (Breast Cancer)', x='Incremental % Reduction', y = 'Therapy') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()





# ------------------------------------------------------
# DANU Utilizations --------------------------------
PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid, weight)
names(Cachexia_Dx)[1] <- "patient"
Cachexia_Dx$Group <- "Dx"


CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")
names(CachexiaPats_ALL_NEW)[1] <- "patient"
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% anti_join(Cachexia_Dx)

PONS_Demographics <- fread("PONS Demographics.txt")
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics %>% select(patid, weight), by=c("patient"="patid"))
CachexiaPats_ALL_NEW$Group <- "Pred"


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer != "-" & Primary_Cancer != "Unspecified Cancer")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight)
names(New_Primary_Cancer_Box)[1] <- "patient"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% anti_join(Cachexia_Dx) %>% anti_join(CachexiaPats_ALL_NEW)
New_Primary_Cancer_Box$Group <- "none"

Groups <- Cachexia_Dx %>% bind_rows(CachexiaPats_ALL_NEW) %>% bind_rows(New_Primary_Cancer_Box)


PONS_Utilizations <- fread("PONS Utilizations.txt")
PONS_Utilizations <- PONS_Utilizations %>% inner_join(Groups, by=c("patid"="patient")) %>% select(-weight.y)

# CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
# PONS_Utilizations %>% inner_join(CancerDrug_Experienced)



for (i in names(PONS_Utilizations[,3:25])){
  print(i)
  print(PONS_Utilizations  %>% group_by(Group) %>% summarise(n=mean(get(i))))
}



PONS_Drug_Utilizations <- fread("PONS Drug Utilizations.txt")
PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% select(patid, drug_ahfs_class)
PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% inner_join(Groups, by=c("patid"="patient")) 
PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% mutate(Exp=1) %>%  select(patid, drug_ahfs_class, Group, Exp) %>% distinct()
unique(PONS_Drug_Utilizations$drug_ahfs_class)

PONS_Drug_Utilizations$drug_ahfs_class <- gsub('[^0-9a-zA-Z\\s]','' ,  PONS_Drug_Utilizations$drug_ahfs_class)

# LookUp <- PONS_Drug_Utilizations %>% select(drug_ahfs_class) %>% distinct() %>% mutate(VarNo=row_number())
# 
# PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% left_join(LookUp)
# PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% select(patid, Group, Exp, VarNo) %>% distinct() 
# PONS_Drug_Utilizations$VarNo <- as.character(PONS_Drug_Utilizations$VarNo)

PONS_Drug_Utilizations$drug_ahfs_class <- gsub("1", "First", PONS_Drug_Utilizations$drug_ahfs_class)
PONS_Drug_Utilizations$drug_ahfs_class <- gsub("2", "Second", PONS_Drug_Utilizations$drug_ahfs_class)
PONS_Drug_Utilizations$drug_ahfs_class <- gsub("3", "Third", PONS_Drug_Utilizations$drug_ahfs_class)
PONS_Drug_Utilizations$drug_ahfs_class <- gsub("4", "Forth", PONS_Drug_Utilizations$drug_ahfs_class)
PONS_Drug_Utilizations$drug_ahfs_class <- gsub("5", "Fifth", PONS_Drug_Utilizations$drug_ahfs_class)

PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% select(patid, drug_ahfs_class, Group, Exp) %>% distinct()
PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% ungroup() %>% spread(key=drug_ahfs_class , value=Exp)

PONS_Drug_Utilizations[is.na(PONS_Drug_Utilizations)] <- 0



temp <- PONS_Drug_Utilizations %>% filter(Group=="none"|Group=="Pred") %>% group_by(Group) %>% sample_n(20000)

temp$Group <- as.factor(temp$Group)

temp$Group <- relevel(temp$Group,"none")
temp <- temp %>% select(-patid)
temp <- temp %>% ungroup()

library("randomForest")
modelAll_1_randomForest <- randomForest(Group  ~  .  , data = temp)
summary(modelAll_1_randomForest)
modelAll_1_randomForest$importance



temp2 <- PONS_Drug_Utilizations %>% filter(Group=="Pred"|Group=="Dx") %>% group_by(Group) %>% sample_n(20000)

temp2$Group <- as.factor(temp2$Group)

temp2$Group <- relevel(temp2$Group,"Pred")
temp2 <- temp2 %>% select(-patid)
temp2 <- temp2 %>% ungroup()

library("randomForest")
modelAll_1_randomForest2 <- randomForest(Group  ~  .  , data = temp2)
summary(modelAll_1_randomForest2)
modelAll_1_randomForest2$importance

# ------------------------------------------------------
# MEgestrol Pats - Persistency Megestrol vs Anticancer drugs -------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Megestrol <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$generic_name == "Megestrol"], collapse = "|"),")\\b")
# 313

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CancerDrug_Experienced %>% select(patid) %>% inner_join(CAN_Drug_Histories)


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Megestrol_Pats <- CAN_Drug_Histories %>% filter(grepl(string_Megestrol, Drugs)) %>% select(patid) %>% distinct()

Megestrol_Pats




New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(Megestrol_Pats, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Megestrol,Treat)) 

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 3.711574


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>%
  group_by(n) %>% summarise(total=sum(weight)) %>%
  ungroup() 

Persistency_Megestrol <- temp
data.frame(Persistency_Megestrol)


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
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

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) # 13.4902


temp <- CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories %>% select(patient) %>% group_by(patient) %>% count()
)  %>%
  group_by(n) %>% summarise(total=sum(weight)) %>%
  ungroup() 

Persistency_Anticancer <- temp

data.frame(Persistency_Anticancer)


# -------------------------------------------------------
# Number patient-months per type, metastasis, cachexia status ----------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=49)

sum(CAN_Drug_Histories$weight)

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
unique(PONS_Ingredients$drug_class)

string_Cancer <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemotherapy"|
                                                                  PONS_Ingredients$drug_class == "Biologic Therapy"|
                                                                  PONS_Ingredients$drug_class == "Radiotherapy"|
                                                                  PONS_Ingredients$drug_class == "GDF15"|
                                                                  PONS_Ingredients$drug_class == "Surgery Inpatient"], collapse = "|"),")\\b")


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

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

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, weight) %>% count()
CAN_Drug_Histories %>% ungroup() %>% summarise(n=sum(weight*n)) # 27094965 patient months

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4151912

# BY primary cancer
data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))
                                 

# BY metastasis state
PONS_Demographics <- fread("PONS Demographics.txt", stringsAsFactors = F)
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

data.frame(CAN_Drug_Histories %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(cancer_metastasis , diagnosis) %>% summarise(n=sum(weight*n)) %>% 
    spread(key=cancer_metastasis, value=n))

# BY cachexia dx/pred 
CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")

CachexiaPats_ALL_NEW %>% left_join(New_Primary_Cancer_Box) %>% summarise(n=sum(weight))

CAN_Drug_Histories %>% inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) # 4151912
# 629202

data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(CachexiaPats_ALL_NEW, by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))


PONS_Demographics <- fread("PONS Demographics.txt", stringsAsFactors = F)
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset) %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1))

CAN_Drug_Histories %>% ungroup() %>% inner_join(PONS_Demographics %>% filter(cachexia_onset == 1), by=c("patient"="patid")) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 91319

data.frame(CAN_Drug_Histories %>% left_join(New_Primary_Cancer_Box %>% select(patid, diagnosis), by=c("patient"="patid")) %>%
             inner_join(PONS_Demographics %>% filter(cachexia_onset == 1), by=c("patient"="patid")) %>%
  ungroup() %>% group_by(diagnosis) %>% summarise(n=sum(weight*n)))

 
# ------------------------------------------------
# BMI evolution over time before/after Dx - NEW Cachexia ----------

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt", sep="\t")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics) 
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% drop_na()
CachexiaPats_ALL_NEW$cancer_onset <- as.Date(CachexiaPats_ALL_NEW$cancer_onset)

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- CachexiaPats_ALL_NEW %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value, cancer_onset)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% mutate(ElapsedTime=as.numeric(claimed-cancer_onset)/30.5)

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Measures <- PONS_Measures %>% left_join(PONS_Demographics)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)

PONS_Measures <- PONS_Measures %>% inner_join(New_Primary_Cancer_Box)
PONS_Measures <- PONS_Measures %>% select(patid, value, ElapsedTime, cancer_metastasis, Primary_Cancer) 

PONS_Measures <- PONS_Measures %>% distinct()
PONS_Measures$ElapsedTime <- round(PONS_Measures$ElapsedTime)
PONS_Measures <- PONS_Measures %>% distinct()
PONS_Measures <- PONS_Measures %>% group_by(patid, Primary_Cancer, cancer_metastasis, ElapsedTime) %>% summarise(n=mean(value))
PONS_Measures <- PONS_Measures %>% ungroup()


PONS_Measures %>% mutate(cancer_metastasis=as.factor(cancer_metastasis)) %>%
  ggplot(aes(ElapsedTime, n, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth(size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Locally weighted moving polynomial regression fit \n")

PONS_Measures %>% mutate(cancer_metastasis=as.factor(cancer_metastasis)) %>%
  mutate(Primary_Cancer=as.factor(Primary_Cancer)) %>%
  filter(Primary_Cancer=="Breast Cancer") %>%
  ggplot(aes(ElapsedTime, n, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth(size=2, se = F) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  #xlim(-60,60) +
  coord_cartesian(ylim=c(25, 32)) + 
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Locally weighted moving polynomial regression fit \n")


# temp1 <- PONS_Measures[PONS_Measures$cancer_metastasis==0,]
# temp1$ElapsedTime <- temp1$ElapsedTime+60
# summary(gam(n ~ s(ElapsedTime), data = temp1))

# temp2 <- PONS_Measures[PONS_Measures$cancer_metastasis==1,]
# temp2$ElapsedTime <- temp2$ElapsedTime+60
# summary(gam(n ~ s(ElapsedTime), data = temp2))




# ---------------------------------------------------------------------------



# BMI evolution over time before/after Rc Class Init - NEW Cachexia ----------

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt", sep="\t")

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% left_join(PONS_Demographics) 
CachexiaPats_ALL_NEW <- CachexiaPats_ALL_NEW %>% drop_na()
CachexiaPats_ALL_NEW$cancer_onset <- as.Date(CachexiaPats_ALL_NEW$cancer_onset)

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- CachexiaPats_ALL_NEW %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(patid, claimed, value, cancer_onset)
PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% mutate(ElapsedTime=as.numeric(claimed-cancer_onset)/30.5)

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

PONS_Measures <- PONS_Measures %>% left_join(PONS_Demographics)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)

PONS_Measures <- PONS_Measures %>% inner_join(New_Primary_Cancer_Box)
PONS_Measures <- PONS_Measures %>% select(-c(cancer_onset, ElapsedTime))
PONS_Measures <- PONS_Measures %>% distinct()


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
CAN_Drug_Histories <- PONS_Measures %>% select(patid) %>% distinct() %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T )
names(CAN_Drug_Histories)[4] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, Month, molecule) %>% group_by(patid, molecule) %>% filter(Month==min(Month))
CAN_Drug_Histories <- CAN_Drug_Histories %>% ungroup()

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)
unique(PONS_Ingredients_JN_ChemoClass$chemo_class)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% filter(chemo_class=="Platinum agent"|
                                            chemo_class=="PD1/PDL1"|
                                            chemo_class=="Immuno/Targeted"|
                                            chemo_class=="Biologic"|
                                            chemo_class=="Hormonal Therapy"|
                                            chemo_class=="Alkylating Agent"|
                                            chemo_class=="Other Antineoplastics"|
                                            chemo_class=="Antimetabolites"|
                                            chemo_class=="Antimicrotubule Agent"|
                                            chemo_class=="Topoisomerase Inhibitor"|
                                            chemo_class=="Radio"|
                                            chemo_class=="Surgery Inpatient")

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Ingredients_JN_ChemoClass) %>%
  select(patid, Month, chemo_class) %>% group_by(patid, chemo_class) %>% filter(Month==min(Month)) %>% ungroup()


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

PONS_Measures <- PONS_Measures %>% mutate(claimed=as.character(claimed))
PONS_Measures <- PONS_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))
PONS_Measures <- PONS_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% rename("BMIrecord"="Exact_Month") %>% select(-claimed)
PONS_Measures <- PONS_Measures %>% distinct()

PONS_Measures <- PONS_Measures %>% inner_join(CAN_Drug_Histories) %>% mutate(ElapsedTime=BMIrecord-Month)
PONS_Measures <- PONS_Measures %>% select(-c(BMIrecord, Month)) %>% distinct() %>% ungroup()

PONS_Measures <- PONS_Measures %>% group_by(patid, cancer_metastasis, Primary_Cancer, chemo_class, ElapsedTime) %>%
  summarise(n=mean(value))
PONS_Measures <- PONS_Measures %>% ungroup()

PONS_Measures <- PONS_Measures %>% mutate(cancer_metastasis=as.factor(cancer_metastasis)) %>%
  mutate(Primary_Cancer=as.factor(Primary_Cancer)) %>%
  mutate(chemo_class=as.factor(chemo_class))

  PONS_Measures %>%
  mutate(ElapsedTime=ElapsedTime+10)  %>%
  filter(Primary_Cancer=="Pancreatic Cancer") %>%
  ggplot(aes(ElapsedTime, n, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth(size=2) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  facet_wrap(~chemo_class, scales="free") +
  xlim(-30,30) +
  xlab("\n Elapsed Time (months) from BMI record to 1st Cancer Dx")+
  ylab("Locally weighted moving polynomial regression fit \n")


# --------------------------------------------------------

  # BIM drop vs survival Per metastasis status & primary cancer & therapy exp  -------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)

# Death/Survival
PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid, weight, cancer_onset, death_date)

PONS_Demographics_temp$cancer_onset <- as.Date(PONS_Demographics_temp$cancer_onset)
PONS_Demographics_temp$death_date    <- as.Date(PONS_Demographics_temp$death_date)

missingDeathDay <- ymd("2050-12-31")
PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_temp <- PONS_Demographics_temp %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived= round(Survived / 30.5,0)) %>%
  mutate(Survived=ifelse(Survived>=60,60,Survived))

New_Primary_Cancer_Box <- PONS_Demographics_temp %>% inner_join(New_Primary_Cancer_Box)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(death_date=format(as.Date(death_date), "%Y-%m"))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(Months_lookup, by=c("death_date"="Month")) %>% mutate(Exact_Month=ifelse(is.na(Exact_Month),60,Exact_Month))





temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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


temp <- temp %>% inner_join(New_Primary_Cancer_Box) %>% select(patid, Primary_Cancer , Diff, Survived) %>%
     filter(Diff<0) %>%
   filter(Diff>-0.3) %>%
     filter(Diff<(-0.05)) %>% 
   mutate(Diff=(100*abs(Diff)))




CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))
CAN_Drug_Histories <- temp %>% select(patid) %>% distinct() %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-") %>% select(patid, Treat) %>% distinct()
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T )
names(CAN_Drug_Histories)[2] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% ungroup()

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)
unique(PONS_Ingredients_JN_ChemoClass$chemo_class)
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% filter(chemo_class=="Platinum agent"|
                                            chemo_class=="PD1/PDL1"|
                                            chemo_class=="Immuno/Targeted"|
                                            chemo_class=="Biologic"|
                                            chemo_class=="Hormonal Therapy"|
                                            chemo_class=="Alkylating Agent"|
                                            chemo_class=="Other Antineoplastics"|
                                            chemo_class=="Antimetabolites"|
                                            chemo_class=="Antimicrotubule Agent"|
                                            chemo_class=="Topoisomerase Inhibitor"|
                                            chemo_class=="Radio"|
                                            chemo_class=="Surgery Inpatient")

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Ingredients_JN_ChemoClass) %>%
  select(patid, chemo_class) %>% distinct() %>% ungroup()

temp <- temp %>% inner_join(CAN_Drug_Histories)

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

temp <- temp %>% left_join(PONS_Demographics)


temp %>% 
  mutate(chemo_class=as.factor(chemo_class)) %>%
  mutate(cancer_metastasis=as.factor(cancer_metastasis)) %>%
  ggplot(aes(Diff, Survived, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth()+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  theme_minimal() +
  facet_wrap(~chemo_class) +
  geom_hline(yintercept=50, colour="palevioletred3") +
  geom_hline(yintercept=40, colour="palevioletred3") +
  xlab("\n MAX Abs. BMI Drop") + ylab("No. Months Survived \n")



temp %>% 
  mutate(chemo_class=as.factor(chemo_class)) %>%
  mutate(cancer_metastasis=as.factor(cancer_metastasis)) %>%
  ggplot(aes(Diff, Survived, colour=cancer_metastasis, fill=cancer_metastasis)) +
  geom_smooth()+
  scale_colour_manual(values = c("steelblue4", "tomato3")) +
  scale_fill_manual(values = c("steelblue4", "tomato3")) +
  theme_minimal() +
  facet_wrap(~Primary_Cancer) +
  geom_hline(yintercept=50, colour="palevioletred3") +
  geom_hline(yintercept=40, colour="palevioletred3") +
  xlab("\n MAX Abs. BMI Drop") + ylab("No. Months Survived \n")




summary(lm(Survived~Diff, data=tomodel))


# -------------------------------------------------------------------------------------------
# Megestrol pats - better recovery ? -----------------------------------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Megestrol <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$generic_name == "Megestrol"], collapse = "|"),")\\b")

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CancerDrug_Experienced %>% select(patid) %>% inner_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Megestrol_Pats <- CAN_Drug_Histories %>% filter(grepl(string_Megestrol, Drugs)) %>% select(patid) %>% distinct()



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% select(patid)) %>% inner_join(CancerDrug_Experienced %>% select(patid))
PONS_Demographics <- PONS_Demographics %>% filter(age>=18)
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)

Megestrol_Pats <- Cachexia_Dx %>% inner_join(Megestrol_Pats)
Cachexia_Dx <- Cachexia_Dx %>% anti_join(Megestrol_Pats)
Megestrol_Pats$Group <- "Megestrol"
Cachexia_Dx$Group <- "NO_Megestrol"

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60) %>% select(patid, Status, weight)
  
Megestrol_Pats %>% full_join(Cachexia_Dx) %>% left_join(PONS_Demographics) %>%
  group_by(Group, Status) %>% summarise(n=sum(weight))

# 1 Megestrol    Death       35476.
# 2 Megestrol    Earliest     2136.
# 3 Megestrol    Metastasis   7453.
# 4 Megestrol    Remission    1772.
# 5 NO_Megestrol Death      213536.
# 6 NO_Megestrol Earliest    17130.
# 7 NO_Megestrol Metastasis  58892.
# 8 NO_Megestrol Remission   13966.
# 
# > 213536/(213536+17130+58892+13966)
# [1] 0.7035226
# > 35476/(35476+2136+7453+1772)
# [1] 0.7574354
# > 1772/(35476+2136+7453+1772)
# [1] 0.03783334
# > 13966/(213536+17130+58892+13966)
# [1] 0.04601284




temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)
names(temp_max)[2] <- "Month_Max"
names(temp_max)[3] <- "Max"
temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)
names(temp_min)[2] <- "Month_Min"
names(temp_min)[3] <- "Min"
temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)

temp <- Cachexia_Dx %>% full_join(Megestrol_Pats) %>% inner_join(temp)

Megestrol_Pats$Group <- "Megestrol"
Cachexia_Dx$Group <- "NO_Megestrol"


ProstatePats <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Prostate Cancer") %>% select(patid)


Megestrol_Pats %>% full_join(Cachexia_Dx) %>% left_join(temp) %>% ungroup() %>% filter(Month_Min>Month_Max) %>% 
  mutate(Diff=(Min-Max)/Max) %>% group_by(patid) %>% filter(Diff==min(Diff)) %>% slice(1) %>% ungroup() %>% rename("BiggestDrop"="Diff") %>% select(patid, BiggestDrop, Group) %>%
  left_join(Megestrol_Pats %>% full_join(Cachexia_Dx) %>% left_join(temp) %>% ungroup() %>% filter(Month_Min<Month_Max) %>%
              mutate(Diff=(Max-Min)/Min) %>% group_by(patid) %>% filter(Diff==max(Diff)) %>% slice(1) %>% ungroup() %>% rename("BiggestRecover"="Diff") %>% select(patid, BiggestRecover, Group)) %>%
  inner_join(ProstatePats) %>%
  group_by(Group) %>% summarise(n=mean(BiggestDrop))

  Group             n
  <chr>         <dbl>
1 Megestrol    -0.228
2 NO_Megestrol -0.211

Megestrol_Pats %>% full_join(Cachexia_Dx) %>% left_join(temp) %>% ungroup() %>% filter(Month_Min>Month_Max) %>% 
  mutate(Diff=(Min-Max)/Max) %>% group_by(patid) %>% filter(Diff==min(Diff)) %>% slice(1) %>% ungroup() %>% rename("BiggestDrop"="Diff") %>% select(patid, BiggestDrop, Group) %>%
  left_join(Megestrol_Pats %>% full_join(Cachexia_Dx) %>% left_join(temp) %>% ungroup() %>% filter(Month_Min<Month_Max) %>%
              mutate(Diff=(Max-Min)/Min) %>% group_by(patid) %>% filter(Diff==max(Diff)) %>% slice(1) %>% ungroup() %>% rename("BiggestRecover"="Diff") %>% select(patid, BiggestRecover, Group)) %>%
    inner_join(ProstatePats) %>%
  group_by(Group) %>% summarise(n=mean(BiggestRecover))

  Group            n
  <chr>        <dbl>
1 Megestrol    0.173
2 NO_Megestrol 0.183

temp %>% group_by(patid) %>% filter(Max==max(Max)) %>% slice(1) %>% select(patid, Max) %>% rename("GlobalMax"="Max") %>% ungroup() %>%
  left_join(temp %>% group_by(patid) %>% filter(Min==max(Max)) %>% slice(1) %>% select(patid, Max) %>% rename("GlobalMax"="Max") %>% ungroup())


# ----------------------------------------------------------
# Appetite Stimulants Experienced in Target population Q1 ---------------------------------------

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia  # 52420

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CachexiaPats_ALL_NEW %>% left_join(CAN_Drug_Histories %>% select(patient, weight), by=c("patid"="patient")) %>% summarise(n=sum(weight))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CancerDrug_Experienced %>% inner_join(CachexiaPats_ALL_NEW)  # 30719

CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
names(CAN_Drug_Histories)[1] <- "patient"

sum(CAN_Drug_Histories$weight) # 939321.4


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% filter(drug_class!="Corticosteroid") %>% filter(drug_group=="Appetite Stimulant") %>%
  select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")


string_Appetite <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

Appetite_Exp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% select(patient, weight) %>% distinct()
sum(Appetite_Exp$weight) # 137187.8   # 0.1460499  4454

First_Appetite_Exp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, weight, Month)

# Nutrition Therapy, Cyproheptadine, Hydrazine, Dronabinol, Nabilone, 
# Medroxyprogesterone, Megestrol, Fluoxymesterone, Nandrolone, Oxandrolone, Oxymetholone, Testosterone



temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep="\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep="\t", header = T)

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

temp <- First_Appetite_Exp %>% select(patient) %>% left_join(temp, by=c("patient"="patid"))

names(First_Appetite_Exp)[3] <- "FirstAppetite"

temp <- temp %>% inner_join(First_Appetite_Exp) %>% group_by(patient) %>% filter(Month_Min<=FirstAppetite)

FirstStartDrop <- temp %>% mutate(PercentDrop=100*(Min-Max)/Max) %>% group_by(patient) %>% filter(PercentDrop==min(PercentDrop)) %>% slice(1)

FirstStartDrop <- FirstStartDrop %>% select(patient, weight, PercentDrop) %>% ungroup()

FirstStartDrop %>% summarise(n=weighted.mean(PercentDrop, weight)) -15%

FirstStartDrop %>% filter(PercentDrop>(-50) & PercentDrop <=0) %>%
  ggplot(aes(PercentDrop)) +
  geom_density(size=1, fill="steelblue4", colour="steelblue4", alpha=0.6) +
  xlab(" \n MAX BMI/Weight  Percentage (%) Drop Observed prior to Appetite Stimulant Initiation") + 
  ylab(" Patient Density \n") +
  theme_minimal()


# ------------------------------------------------------
# Appetite Stimulants Experienced in Target population Q2 ---------------------------------------

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia  # 52420

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CachexiaPats_ALL_NEW %>% left_join(CAN_Drug_Histories %>% select(patient, weight), by=c("patid"="patient")) %>% summarise(n=sum(weight))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CancerDrug_Experienced %>% inner_join(CachexiaPats_ALL_NEW)  # 30719

CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
names(CAN_Drug_Histories)[1] <- "patient"

sum(CAN_Drug_Histories$weight) # 939321.4


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% filter(drug_class!="Corticosteroid") %>% filter(drug_group=="Appetite Stimulant") %>%
  select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")

string_Appetite <- paste0("\\b(",paste0(PONS_Ingredients$molecule, collapse = "|"),")\\b")

Appetite_Exp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% select(patient, weight) %>% distinct()
sum(Appetite_Exp$weight) # 137187.8   # 0.1460499  4454

First_Appetite_Exp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, weight, Month)

# Nutrition Therapy, Cyproheptadine, Hydrazine, Dronabinol, Nabilone, 
# Medroxyprogesterone, Megestrol, Fluoxymesterone, Nandrolone, Oxandrolone, Oxymetholone, Testosterone

temp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% group_by(patient) %>% 
  filter(Month==min(Month)) %>% select(patient, weight, Drugs)

temp <- separate_rows(temp, Drugs, sep = ",", convert=T)

temp <- temp %>% filter(grepl(string_Appetite, Drugs))
 
temp %>% group_by(patient) %>% count() %>% ungroup() %>% summarise(n2=mean(n)) # 1.01

temp %>% group_by(patient) %>% count() %>% ungroup() %>% 
    ggplot(aes(n)) +
  geom_density(size=1, fill="steelblue4", colour="steelblue4", alpha=0.6) +
  xlab(" \n No. Different Appetite Stimulant Molecules at Initiation") + 
  ylab(" Patient Density \n") +
  xlim(0,5) +
  theme_minimal()



temp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs)) %>% group_by(patient) %>% 
   select(patient, weight, Drugs) %>% distinct()

temp <- separate_rows(temp, Drugs, sep = ",", convert=T)

temp <- temp %>% filter(grepl(string_Appetite, Drugs))
 
temp %>% select(patient, Drugs) %>% distinct() %>% group_by(patient) %>% count() %>%
  ungroup() %>% summarise(n2=mean(n)) # 1.12

temp %>% select(patient, Drugs) %>% distinct() %>% group_by(patient) %>% count() %>%
  ungroup()  %>% 
    ggplot(aes(n)) +
  geom_density(size=1, fill="steelblue4", colour="steelblue4", alpha=0.6) +
  xlab(" \n No. Different Appetite Stimulant Molecules Tried Throughout") + 
  ylab(" Patient Density \n") +
  xlim(0,5) +
  theme_minimal()


temp <- CAN_Drug_Histories %>% filter(grepl(string_Appetite, Drugs))
temp <- separate_rows(temp, Drugs, sep = ",", convert=T)
temp <- temp %>% filter(grepl(string_Appetite, Drugs))

temp %>% group_by(patient, weight, Drugs) %>% count() %>%
  ungroup() %>% group_by(Drugs) %>% summarise(mean=weighted.mean(n))

# -------------------------------------------------
# Appetite Stimulants Experienced in Target population Q3 ---------------------------------------

CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt") # All cachexia  # 52420

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CachexiaPats_ALL_NEW %>% left_join(CAN_Drug_Histories %>% select(patient, weight), by=c("patid"="patient")) %>% summarise(n=sum(weight))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CancerDrug_Experienced <- CancerDrug_Experienced %>% inner_join(CachexiaPats_ALL_NEW)  # 30719

CAN_Drug_Histories <- CancerDrug_Experienced %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
names(CAN_Drug_Histories)[1] <- "patient"

sum(CAN_Drug_Histories$weight) # 939321.4


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% filter(drug_class!="Corticosteroid") %>% filter(drug_group=="Appetite Stimulant") %>%
  select(molecule, drug_class)
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

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

temp <- temp %>% left_join(PONS_Ingredients %>% select(molecule, generic_name), by=c("Drugs"="molecule")) %>%
  select(patient, generic_name) %>% distinct()

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- temp %>% select(patient) %>% left_join(CAN_Drug_Histories %>% select(-disease))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct()
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Drugs) %>% distinct()

PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.txt", sep="\t")
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



noLines <- fread("Drug_Histories_Cancer_Drugs_Only.txt", sep="\t")
noLines <- temp %>% select(patient) %>% left_join(noLines)

noLines <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
noLines <- noLines %>% filter(!is.na(Drugs)) %>% filter(Drugs!="")  %>% select(patient, weight, Drugs) %>% distinct()

temp <- temp %>% left_join(noLines %>% group_by(patient, weight) %>% count())
names(temp)[16] <- "NoLines"

temp %>% ungroup() %>% summarise(n=weighted.mean(NoLines, weight))
temp %>% ungroup() %>% group_by(AppetiteStimulantGroup)  %>% summarise(n=weighted.mean(NoLines, weight))

1 Cyproheptadine          2.87
2 Dronabinol              4.09
3 Medroxyprogesterone     2.70
4 Megestrol               3.59
5 Nutrition Therapy       2.88
6 Oxandrolone             8.25
7 Testosterone            2.48



PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis)
names(PONS_Demographics)[1] <- "patient"
PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))
temp <- temp %>% left_join(PONS_Demographics)

temp %>% group_by(AppetiteStimulantGroup, cancer_metastasis) %>% count() %>% ungroup() %>%
  spread(key=cancer_metastasis, value=n)


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
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






CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")
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



# ----------------------------------------------

# Who initiated it ?----------------

CAN_Doses <- fread("CAN Doses.txt")

CAN_Doses <- temp %>% select(patient) %>% distinct() %>% left_join(CAN_Doses, by=c("patient"="pat_id"))
CAN_Doses <- temp %>% ungroup() %>% select(AppetiteStimulantGroup) %>% distinct() %>% left_join(CAN_Doses, by=c("AppetiteStimulantGroup"="generic_name"))

CAN_Doses <- CAN_Doses %>% select(AppetiteStimulantGroup, patient, from_dt, prov, specialty)
CAN_Doses$from_dt <- as.Date(CAN_Doses$from_dt)

CAN_Doses <- CAN_Doses %>% group_by(patient) %>% filter(from_dt==min(from_dt))


PONS_Event_Claims_Providers <- fread("PONS Event Claims Providers.txt")
PONS_Event_Claims_Providers <- PONS_Event_Claims_Providers %>% select(prov, specialty_classification)

data.frame(CAN_Doses %>% left_join(PONS_Event_Claims_Providers) %>%
  group_by(AppetiteStimulantGroup, specialty_classification) %>% count() %>%
  ungroup() %>% spread(key=specialty_classification, value=n))


# -----------------------
# Cachexia Onsets ---------------------------------

# Drugs

CAN_Cachexia_Onset_Drug <- fread("CAN Cachexia Onset Drug.txt")

CAN_Cachexia_Onset_Drug %>% filter(during_patient_population==max(during_patient_population))
# Replacement preparations  21233.78


data.frame(CAN_Cachexia_Onset_Drug %>%  arrange(-during_patient_population) %>% slice(1:100)  %>% select(drug, during_patient_population))

data.frame(CAN_Cachexia_Onset_Drug %>% mutate(difference=after_patient_population-before_patient_population) %>%
  arrange(-difference) )




# Procedures

CAN_Cachexia_Onset_Procedure <- fread("CAN Cachexia Onset Procedure.txt")

CAN_Cachexia_Onset_Procedure %>% filter(during_patient_population==max(during_patient_population))
# Emergency department visit  53524.28                  


temp <- data.frame(CAN_Cachexia_Onset_Procedure %>%  arrange(-during_patient_population) %>% slice(1:100) %>% 
  select(description, during_patient_population)  %>% 
             mutate(description=str_replace_all(description, " ", "_")))

temp <- data.frame(CAN_Cachexia_Onset_Procedure %>% mutate(difference=after_patient_population-before_patient_population) %>%
  arrange(-difference) %>% slice(1:100) %>%
             select(description, difference) %>% 
             mutate(description=str_replace_all(description, " ", "_")))


fwrite(temp, "temp.csv")

# Diagnoses 

CAN_Cachexia_Onset_Diagnosis <- fread("CAN Cachexia Onset Diagnosis.txt")

CAN_Cachexia_Onset_Diagnosis %>% filter(during_patient_population==max(during_patient_population))
# Cachexia     149853.7                   

data.frame(CAN_Cachexia_Onset_Diagnosis %>%  arrange(-during_patient_population) %>% slice(1:100) %>%
             select(description, during_patient_population) %>% 
             mutate(description=str_replace_all(description, " ", "_")))

data.frame(CAN_Cachexia_Onset_Diagnosis %>% mutate(difference=after_patient_population-before_patient_population) %>%
  arrange(-difference) %>%  slice(1:100) %>%
             select(description, difference) %>% 
             mutate(description=str_replace_all(description, " ", "_")))


# -------------------------------------------------
# Sizing Core5, Active vs Remisison, Mets vs Non-Mets - Core 5 vs Liquids vs other --------------------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box %>% summarise(n=sum(weight))
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!= "-" & Primary_Cancer!= "Unspecified Cancer")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(died == "N")
sum(New_Primary_Cancer_Box$weight) # 18752636
names(New_Primary_Cancer_Box)[1] <- "patient"

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60 & Status != "Death" & Status != "Naive" ) %>% select(patid, Status)
names(PONS_Demographics)[1] <- "patient"

unique(New_Primary_Cancer_Box$Primary_Cancer)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics) 


New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(Core5 = ifelse(Primary_Cancer == "Breast Cancer"|
                                                   Primary_Cancer == "Prostate Cancer"|
                                                   Primary_Cancer == "Lung Cancer"|
                                                   Primary_Cancer == "Prostate Cancer"|
                                                   Primary_Cancer == "Pancreatic Cancer", "CORE5",
                                                 ifelse(Primary_Cancer == "Lymphoma Cancer" |
                                                          Primary_Cancer == "Leukemia Cancer" |
                                                          Primary_Cancer == "Myeloma Cancer" |
                                                          Primary_Cancer == "Head Cancer" |
                                                          Primary_Cancer == "Gastroesophageal Cancer" |
                                                          Primary_Cancer == "Salivary Cancer", "LIQUIDS", "OTHER")))
   
New_Primary_Cancer_Box %>% select(Primary_Cancer, Core5) %>% distinct() %>% arrange(Core5)

  
New_Primary_Cancer_Box %>%  filter(Status == "Remission") %>% group_by(Core5) %>% summarise(n=sum(weight))
New_Primary_Cancer_Box %>%  filter(Status != "Remission") %>% group_by(Core5) %>% summarise(n=sum(weight))

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis = ifelse(is.na(cancer_metastasis),0,1))
names(PONS_Demographics)[1] <- "patient"

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(PONS_Demographics)

New_Primary_Cancer_Box %>%  filter(Status != "Remission") %>% group_by(cancer_metastasis, Core5) %>% summarise(n=sum(weight))


# ---------------------------------
# MEgestrol utcomes -------------------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer, weight)


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class, generic_name)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
string_Megestrol <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$generic_name == "Megestrol"], collapse = "|"),")\\b")
# 313

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CancerDrug_Experienced %>% select(patid) %>% inner_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

Megestrol_Pats <- CAN_Drug_Histories %>% filter(grepl(string_Megestrol, Drugs)) %>% select(patid, Month) %>% distinct()

Megestrol_Pats

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% select(patid)) %>% inner_join(CancerDrug_Experienced %>% select(patid))
PONS_Demographics <- PONS_Demographics %>% select(patid, cachexia_onset)
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) 
Cachexia_Dx <- Cachexia_Dx %>% mutate(cachexia_onset=format(as.Date(cachexia_onset), "%Y-%m"))


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

Cachexia_Dx <- Cachexia_Dx %>% left_join(Months_lookup, by=c("cachexia_onset"="Month")) %>% select(patid, Exact_Month)
names(Cachexia_Dx)[2] <- "FirstCachexiaDx"

Megestrol_Pats <- Megestrol_Pats %>% inner_join(Cachexia_Dx)

Megestrol_Pats <- Megestrol_Pats %>% left_join(Megestrol_Pats %>% group_by(patid) %>% filter(Month==min(Month)) %>% rename("FirstMegestrol"="Month") %>% select(1,2))
Megestrol_Pats <- Megestrol_Pats %>% left_join(Megestrol_Pats %>% group_by(patid) %>% count() %>% rename("TotalNoMegestrol"="n") %>% select(1,2))

Megestrol_Pats <- Megestrol_Pats %>% select(1,3,4,5) %>% distinct()

Megestrol_Pats <- Megestrol_Pats %>% mutate(MegestroLBefore = ifelse(FirstCachexiaDx > FirstMegestrol, "YES", "NO"))

PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, died)

Megestrol_Pats %>% left_join(PONS_Demographics) %>% group_by(MegestroLBefore, died) %>% summarise(n=sum(weight))

# 18536/(18536+5384)   0.7749164
# 16940/(16940+5976)   0.7392215

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

New_Primary_Cancer_Box %>% filter(Primary_Cancer == "Lung Cancer") %>% select(patid) %>%
  inner_join(Megestrol_Pats) %>% left_join(PONS_Demographics) %>% group_by(MegestroLBefore, died) %>% summarise(n=sum(weight))

# 4496/(4496+905) # 0.8324384
# 4145/(4145+1222) # 0.7723123

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")
unique(PONS_Demographics$Status)
PONS_Demographics <- PONS_Demographics %>% filter(Exact_Month==60) %>% select(patid, Status ,weight)

Megestrol_Pats %>% left_join(PONS_Demographics) %>% group_by(MegestroLBefore, Status) %>% summarise(n=sum(weight))

#   MegestroLBefore Status          n
#   <chr>           <chr>       <dbl>
# 1 NO              Death      16940.
# 2 NO              Earliest    1231.
# 3 NO              Metastasis  3769.
# 4 NO              Remission    976.
# 5 YES             Death      18536.
# 6 YES             Earliest     904.
# 7 YES             Metastasis  3683.
# 8 YES             Remission    797.
# 
# > 797/(797+3683+904+18536)
# [1] 0.0333194
# > 976/(976+3769+1231+16940)
# [1] 0.04259033

Megestrol_Pats %>% left_join(PONS_Demographics) %>% filter(died=="N") %>% summarise(n=weighted.mean(TotalNoMegestrol, weight))


# ----------------------
# OLANZAPINE ------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-"&Primary_Cancer!="Unspecified Cancer") 
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, weight)

# Cachecia Dx
PONS_Demographics <- fread("PONS Demographics.txt")
Cachexia_Dx <- PONS_Demographics %>% filter(!is.na(cachexia_onset)) %>% select(patid)

# Cachexia Pred
Cachexia_pats <- fread("CachexiaPats_Ever_AllDrops.txt")
Cachexia_pats <- Cachexia_pats %>% select(patid)
Cachexia_pred <- Cachexia_pats %>% inner_join(PONS_Demographics %>% select(patid)) %>% anti_join(Cachexia_Dx)

names(Cachexia_Dx)[1] <- "patient"
names(Cachexia_pred)[1] <- "patient"

Cachexia_Dx <- Cachexia_Dx %>% inner_join(New_Primary_Cancer_Box, by=c("patient"="patid"))
Cachexia_pred <- Cachexia_pred %>% inner_join(New_Primary_Cancer_Box, by=c("patient"="patid"))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics %>%  select(patid)) %>% 
  anti_join(Cachexia_Dx, by=c("patid"="patient")) %>% anti_join(Cachexia_pred,by=c("patid"="patient"))

names(New_Primary_Cancer_Box)[1] <- "patient"


CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.txt", sep="\t")

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl("13", Drugs)) %>% select(patient) %>% distinct() %>% mutate(OLAN="OLAN")

Cachexia_Dx %>% left_join(CAN_Drug_Histories) %>% group_by(OLAN) %>% summarise(n=sum(weight))
Cachexia_pred %>% left_join(CAN_Drug_Histories) %>% group_by(OLAN) %>% summarise(n=sum(weight))
New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories) %>% group_by(OLAN) %>% summarise(n=sum(weight))

# --------------
# Cachexia Penetrance in kids update Lili  ------------------------------------------


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(age<18)
PONS_Demographics <- PONS_Demographics %>% filter(diagnosis!="-")
sum(PONS_Demographics$weight)
max(PONS_Demographics$cancer_onset)

PONS_Demographics %>% mutate(cancer_onset=as.Date(cancer_onset)) %>%
  filter(cancer_onset>="2020-08-01") %>%
  filter(diagnosis!="Other Cancer" & diagnosis!="Unspecified Cancer") %>%
  inner_join(CAN_Drug_Histories) %>%
  summarise(n=sum(weight)) #29978.45    


PONS_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight)) %>% arrange(-n)

PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cachexia_onset, age)
PONS_Demographics <- PONS_Demographics %>% mutate(cachexia_onset=ifelse(is.na(cachexia_onset),0,1)) 
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T )
PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% slice(1)

unique(PONS_Demographics$age)

PONS_Demographics <- PONS_Demographics %>% mutate(age=ifelse(age==1,age,
                                        ifelse(age==2,age,
                                               ifelse(age>2&age<=12,12,17))))

data.frame(PONS_Demographics %>% group_by(diagnosis, age) %>% 
             mutate(age=as.factor(age)) %>% summarise(n=sum(weight)) %>%
             spread(key=age, value=n))

                 diagnosis     X1      X2      X12      X17
1              Bone Cancer  39.11  126.05  3950.10  7217.06
2             Brain Cancer 606.83  760.89 19865.59 17206.39
3            Breast Cancer  83.95   65.02  3714.70  2180.02
4  Gastroesophageal Cancer  39.11   31.18   438.53   394.79
5              Head Cancer  83.95   94.87  3112.40  2310.43
6        Intestinal Cancer  39.11  191.07  3085.38  2219.71
7            Kidney Cancer  83.95  254.76  7618.39  3347.00
8          Leukemia Cancer 665.87  943.17 29800.06 16705.33
9             Liver Cancer 251.85  399.86  4424.21  1600.65
10             Lung Cancer 201.28  188.41  3812.69  1909.07
11         Lymphoma Cancer 290.96  413.32 14030.37 14796.94
12          Myeloma Cancer     NA   32.51   836.99   577.73
13            Other Cancer 854.96 1650.62 25607.17 15237.72
14       Pancreatic Cancer     NA   94.87   389.08   329.46
15         Prostate Cancer     NA   31.18  2277.31  1553.69
16     Reproductive Cancer 123.06  189.74  3286.77  2995.32
17      Respiratory Cancer     NA   32.51  1116.44   703.33
18         Salivary Cancer     NA      NA   435.46   361.78
19             Skin Cancer  78.22   62.36  6828.17  7658.25
20          Thyroid Cancer  39.11   31.18  1683.73  2771.90
21      Unspecified Cancer 212.74  250.77  6009.95  4856.95
22          Urinary Cancer  83.95   31.18  1333.05  1008.92


sum(PONS_Demographics$weight) #261252.5
sum(PONS_Demographics$weight[PONS_Demographics$cachexia_onset==1]) #1736.99 (0.006648702 cachexia)



PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")
PONS_Measures <- PONS_Measures %>% filter(test=="BMI")
PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(PONS_Demographics %>% select(patid, weight))

data.frame(PONS_Measures %>% select(patid) %>% distinct() %>%
  left_join(PONS_Demographics)  %>%
  group_by(diagnosis, age) %>% 
             mutate(age=as.factor(age)) %>% summarise(n=sum(weight)) %>%
             spread(key=age, value=n))

data.frame(PONS_Measures %>% select(patid) %>% distinct() %>%
  left_join(PONS_Demographics)  %>%
  group_by(age) %>% count()  %>% arrange(-n))

temp <- PONS_Measures
temp <- temp %>% select(patid, claimed, value)

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
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

temp <- temp %>% mutate(Drop95=ifelse( (Min<(Max*0.95)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=6) ,1,0 ))
temp <- temp %>% mutate(Drop90=ifelse( (Min<(Max*0.90)) & (Month_Min>Month_Max) & (Month_Min-Month_Max<=12) ,1,0 ))
temp <- temp %>% mutate(Drop2_20=ifelse( (Min<(Max*0.98)) & (Month_Min>Month_Max) & (Min<20) ,1,0 ))

New_Cachexia_Pred <- temp %>% filter( Drop95==1|Drop90==1|Drop2_20==1) %>% select(patid) %>% distinct()
New_Cachexia_Pred$Group <- "Cachexia"


data.frame(PONS_Demographics %>% left_join(New_Cachexia_Pred) %>% mutate(Group=ifelse(is.na(Group),"none", Group)) %>%
             filter(Group=="Cachexia") %>%
             group_by(diagnosis, age) %>% 
             mutate(age=as.factor(age)) %>% summarise(n=sum(weight)) %>%
             spread(key=age, value=n))



CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- PONS_Demographics %>% select(patid) %>% left_join(CAN_Drug_Histories, by=c("patid"="patient"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, weight, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=49)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid) %>% distinct()



PONS_Demographics %>%   filter(diagnosis!="Other Cancer" & diagnosis!="Unspecified Cancer") %>%
 inner_join(CAN_Drug_Histories) %>% ungroup() %>%  summarise(n=sum(weight)) #118438     

# ----------

# Average duration metastasis vs no mestastasis ---------------------
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
names(New_Primary_Cancer_Box)[4] <- "diagnosis"
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(diagnosis!="-")

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box %>% select(patid), by=c("patient"="patid"))

CancerDrug_Experienced <- fread("CancerDrug_Experienced.txt", sep="\t")
CAN_Drug_Histories <- CAN_Drug_Histories %>%  inner_join(CancerDrug_Experienced, by=c("patient"="patid"))

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)
CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)
PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)
unique(PONS_Ingredients$drug_class)

string_Chemoprotective       <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Chemoprotective"], collapse = "|"),")\\b")
string_Appetite       <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Appetite Stimulant"], collapse = "|"),")\\b")
string_Antiemetics       <- paste0("\\b(",paste0(PONS_Ingredients$molecule[PONS_Ingredients$drug_class == "Antiemetic"], collapse = "|"),")\\b")

CAN_Drug_Histories_2 <- CAN_Drug_Histories  %>% filter(grepl(string_Antiemetics,Treat))

CAN_Drug_Histories_2  %>% select(patient, weight) %>% distinct() %>% left_join(
  CAN_Drug_Histories_2 %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% summarise(n=weighted.mean(n, weight)) 


# Chemoprotective -> 5.750753
# Appetite -> 3.226509
# Antiemetic -> 5.827695


PONS_Demographics_temp <- fread("PONS Demographics.txt")
PONS_Demographics_temp <- PONS_Demographics_temp %>% select(patid,  cancer_metastasis)  %>%
   mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))

names(PONS_Demographics_temp)[1] <- "patient"


CAN_Drug_Histories_2 <- CAN_Drug_Histories  %>% inner_join(PONS_Demographics_temp) %>% filter(grepl(string_Chemoprotective,Treat))

CAN_Drug_Histories_2  %>% select(patient, weight, cancer_metastasis) %>% distinct() %>% left_join(
  CAN_Drug_Histories_2 %>% select(patient, cancer_metastasis) %>% group_by(patient, cancer_metastasis) %>% count()
)  %>% ungroup() %>% group_by(cancer_metastasis) %>% summarise(n=weighted.mean(n, weight)) 

# Chemoprotective Mets -> 5.59    Chemoprotective non-Mets ->  # 6.29
# Appetite -> 3.40     Appetite non-Mets ->   2.90
# Antiemetic -> 6.60     Antiemetic non-Mets ->  4.63

# ---------