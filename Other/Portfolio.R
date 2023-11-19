library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)



Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- fread("Mkt_Comorbidity_Groups_OBE_adjusted_Jun22.txt", sep="\t")
Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22  %>% filter(grepl("Obesity", diagnosis))
sum(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$weight) # 121764029

DANU_Measures_Additional <- fread("DANU Measures Additional.txt", sep="\t")
head(DANU_Measures_Additional)
DANU_Measures_Additional <- DANU_Measures_Additional %>% select(-c(weight, source, vague_value, vague_date, metric, description))
DANU_Measures_Additional <- DANU_Measures_Additional %>% inner_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(patid) %>% distinct())

df_count <- DANU_Measures_Additional %>% select(patid, test, claimed) %>% distinct() %>% group_by(patid, test) %>% count()

DANU_Measures <- fread("DANU Measures.txt", sep="\t")
head(DANU_Measures)
DANU_Measures <- DANU_Measures %>% select(-c(weight, source, vague_value, vague_date, metric, description))
DANU_Measures <- DANU_Measures %>% inner_join(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% select(patid) %>% distinct())

df_count_2 <- DANU_Measures %>% select(patid, test, claimed) %>% distinct() %>% group_by(patid, test) %>% count()

df_count <- df_count %>% bind_rows(df_count_2)

data.frame(df_count %>% ungroup() %>% group_by(test) %>% summarise(mean=mean(n)))


missing <- Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>%  select(patid) %>% mutate(bind=1) %>%
  left_join(
DANU_Measures %>% select(test) %>% distinct() %>% 
  bind_rows(DANU_Measures_Additional  %>% select(test) %>% distinct()) %>% mutate(bind=1)
) %>% select(-bind) %>%
  anti_join(
      DANU_Measures %>% select(patid, test) %>% distinct() %>%
  bind_rows(DANU_Measures_Additional  %>% select(patid, test) %>% distinct())
  )
  

  
unique(df_count$test)

df_count %>% bind_rows(missing %>% mutate(n=0)) %>%
  ungroup() %>%
  ggplot(aes(n, y = ..scaled.., colour=test, fill=test)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~test, scales = "free") +
  xlim(0,25) +
  theme_minimal() +
  xlab("\n No. of distinct record dates") + 
  ylab("Patient proportion \n")

df_count %>% ungroup() %>% filter(test=="Diastolic Blood Pressure"|test=="Systolic Blood Pressure") %>% 
  select(patid) %>% distinct()

length(unique(Mkt_Comorbidity_Groups_OBE_adjusted_Jun22$patid))

Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% 
  inner_join(df_count %>% ungroup() %>% filter(test=="ACR Ratio") %>% select(patid) %>% distinct()) %>%
  filter(CKD==0)


Mkt_Comorbidity_Groups_OBE_adjusted_Jun22 %>% group_by(Comorb) %>% summarise(n=sum(weight))



DANU_Measures_Additional %>% filter(test=="Diastolic Blood Pressure"|test=="Systolic Blood Pressure") %>% 
  group_by(patid, test) %>% filter(claimed==max(claimed)) %>% filter(value==max(value)) %>%    ungroup() %>% 
  filter( (test=="Systolic Blood Pressure"&value>120)|(test=="Diastolic Blood Pressure"&value>80) ) %>%
  ungroup() %>% select(patid) %>% distinct()


DANU_Measures %>% filter(test=="ACR Ratio") %>%
  group_by(patid) %>% filter(claimed==max(claimed)) %>% filter(value==max(value)) %>%
  ungroup() %>%
  ggplot(aes(value)) +
  geom_density() +
  xlim(0,100)
