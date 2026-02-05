
library(data.table)
library(tidyverse)
options(scipen = 999)


# AVERAGE SUPPLY DAYS PER PAT PER MOLECULE JAPAN -------------------------------------------------

DANU_Japan_Demographics <- fread("DANU Japan Demographics.txt")
unique(DANU_Japan_Demographics$diagnosis)
DANU_Japan_Demographics <- DANU_Japan_Demographics %>% filter(diagnosis!= "-") %>% select(patid, diagnosis)

OBE_Japan_Doses <- fread("OBE Japan Doses.txt")
OBE_Japan_Doses %>% select(pat_id) %>% distinct()

DIA_Japan_Doses <- fread("DIA Japan Doses_v2.txt")
DIA_Japan_Doses %>% select(pat_id) %>% distinct()

DIA_Japan_Doses %>% select(pat_id) %>% distinct() %>%
  inner_join(OBE_Japan_Doses %>% select(pat_id) %>% distinct())

Doses <- DIA_Japan_Doses %>% bind_rows(OBE_Japan_Doses)

Doses <- Doses %>% left_join(DANU_Japan_Demographics, by=c("pat_id"="patid"))
Doses <- Doses %>% filter(grepl("GLP1",drug_class))
unique(Doses$drug_class)
unique(Doses$status) # exc G e X

# Doses <- Doses %>% filter(status!="X"&status!="G")

range(Doses$from_dt)

Doses <- Doses %>% mutate(Year = ifelse(from_dt <= "2017-06-30", "Year1",
                                                                ifelse(from_dt <= "2018-06-30", "Year2",
                                                                       ifelse(from_dt <= "2019-06-30", "Year3",
                                                                              ifelse(from_dt <= "2020-06-30", "Year4",
                                                                                     ifelse(from_dt <= "2021-06-30", "Year5",claimed)))))) 


data.frame(Doses %>% group_by(pat_id, generic_name, Year, weight) %>% summarise(n=sum(dayssup)) %>%
  ungroup() %>% group_by(generic_name, Year) %>% summarise(mean=weighted.mean(n, weight)))


data.frame(Doses %>% filter(Year=="Year5") %>%  group_by(pat_id, generic_name, Year, weight) %>% summarise(n=sum(dayssup)) %>%
  ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, weight)))

data.frame(Doses %>% filter(Year=="Year5") %>%  filter(grepl("Obesity", diagnosis)) %>%  group_by(pat_id, generic_name, Year, weight) %>% summarise(n=sum(dayssup)) %>%
  ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, weight)))

data.frame(Doses %>%  filter(Year=="Year5") %>% filter(grepl("Diabetes", diagnosis)) %>%  group_by(pat_id, generic_name, Year, weight) %>% summarise(n=sum(dayssup)) %>%
  ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, weight)))


#  -------------------------------------------------


# AVERAGE SUPPLY DAYS PER PAT PER MOLECULE UNITED STATES -------------------------------------------------


DANU_Demographics <- fread("DANU Demographics.txt")
unique(DANU_Demographics$diagnosis)
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis!= "-") %>% select(patid, diagnosis)

OBE_Doses <- fread("OBE Doses.txt")
OBE_Doses %>% select(pat_id) %>% distinct()

DIA_Doses <- fread("DIA Doses.txt")
DIA_Doses %>% select(pat_id) %>% distinct()

DIA_Doses %>% select(pat_id) %>% distinct() %>%
  inner_join(OBE_Doses %>% select(pat_id) %>% distinct())

Doses <- DIA_Doses %>% bind_rows(OBE_Doses)

Doses <- Doses %>% left_join(DANU_Demographics, by=c("pat_id"="patid"))
Doses <- Doses %>% filter(grepl("GLP1",drug_class))
unique(Doses$drug_class)


unique(Doses$status) # 
unique(Doses$paid) # 


#Doses <- Doses %>% filter(status!="X"&status!="G")

range(Doses$from_dt)


Doses <- Doses %>% mutate(Year = ifelse(from_dt <= "2017-06-30", "Year1",
                                                                ifelse(from_dt <= "2018-06-30", "Year2",
                                                                       ifelse(from_dt <= "2019-06-30", "Year3",
                                                                              ifelse(from_dt <= "2020-06-30", "Year4",
                                                                                     ifelse(from_dt <= "2021-06-30", "Year5",claimed)))))) 


data.frame(Doses %>% filter(Year=="Year5") %>%  group_by(pat_id, generic_name, Year, weight) %>% summarise(n=sum(dayssup)) %>%
  ungroup() %>% group_by(generic_name, Year) %>% summarise(mean=weighted.mean(n, weight)))



data.frame(Doses %>%    filter(grepl("Obesity", diagnosis)) %>% group_by(pat_id, generic_name, Year, weight) %>% summarise(n=sum(dayssup)) %>%
  ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, weight)))


data.frame(Doses %>%   filter(grepl("Diabetes", diagnosis)) %>%  group_by(pat_id, generic_name, Year, weight) %>% summarise(n=sum(dayssup)) %>%
  ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, weight)))





# --------------------------------------------------------------------------



# CONVERT STANDARD UNITS TO PATIENTS DIABETES --------------------------------------------------------------------------



GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_SU <- data.table(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_MTHLY_Diabetes_SU_pats <- GLOB_MTHLY_Diabetes_SU
unique(GLOB_MTHLY_Diabetes_SU_pats$MOLECULE)
unique(GLOB_MTHLY_Diabetes_SU_pats$`INT-PRODUCT`)

GLOB_MTHLY_Diabetes_SU %>% filter(COUNTRY=="US"& DATE=="2022") %>% summarise(n=sum(n))


GLOB_MTHLY_Diabetes_SU_pats <- GLOB_MTHLY_Diabetes_SU_pats %>% mutate(n=ifelse(MOLECULE == "DULAGLUTIDE", n/296,
                                                                               ifelse(MOLECULE == "EXENATIDE" , n/266,
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/292,
                                                                                             ifelse(MOLECULE == "LIXISENATIDE", n/281,
                                                                                                    ifelse(`INT-PRODUCT` == "RYBELSUS", n/198,
                                                                                                           ifelse(`INT-PRODUCT` == "OZEMPIC", n/274, 
                                                                                                                  ifelse( MOLECULE == "ALBIGLUTIDE", n/296, n/274 ))))))))



Country_Pop_2021 <- fread("Country_Pop_2021.csv")
Country_Pop_2021 %>% select(Country) %>% arrange(Country)
GLOB_MTHLY_Diabetes_SU_pats %>% select(COUNTRY) %>% arrange(COUNTRY)


Country_Pop_2021 %>% select(Country) %>% distinct() %>% arrange(Country)
names(Country_Pop_2021)[1] <- "COUNTRY"
names(Country_Pop_2021)[2] <- "POP_2021"

GLOB_MTHLY_Diabetes_SU_pats %>% select(COUNTRY) %>% distinct() %>% arrange(COUNTRY)


data.frame(GLOB_MTHLY_Diabetes_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2017`)^(1/6)-1 ),1 ))

data.frame(GLOB_MTHLY_Diabetes_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>% ungroup() %>%
  spread(key=DATE, value=n ) )





GLP1_Share_DIA <- fread("GLP1_Share_DIA.csv", header = T)

library(ggrepel)
library(hrbrthemes)
library(viridis)

data.frame(GLOB_MTHLY_Diabetes_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2017`)^(1/6)-1 ),1 )) %>%
  select(COUNTRY, `2022`) %>%
  left_join(GLP1_Share_DIA %>% select(COUNTRY, `2022`) %>% rename("GLP1_Share"="2022")) %>%
  mutate(`2022`=`2022`/100) %>%
  ggplot(aes(`2022`, GLP1_Share, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = GLP1_Share) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,10)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Outreach / Country Population Penetrance")+
  ylab("% GLP1 Share of Diabetes Market \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)


data.frame(GLOB_MTHLY_Diabetes_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2017`)^(1/6)-1 ),1 )) %>%
  select(COUNTRY, `2022`) %>%
  left_join(GLP1_Share_DIA %>% select(COUNTRY, `2022`) %>% rename("GLP1_Share"="2022")) %>%
  mutate(`2022`=`2022`/100) %>%
  left_join(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, DATE) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>%
  filter(DATE=="2022") %>% ungroup()  %>%
  rename("VOLUME"="n") %>% select(-DATE)) %>%
  ggplot(aes(`2022`, GLP1_Share, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = VOLUME) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,10)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Outreach / Country Population Penetrance")+
  ylab("% GLP1 Share of Diabetes Market \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)




# Growth rate vs current size

data.frame(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, DATE) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>%
  filter(DATE=="2018"|DATE=="2022") %>% ungroup() %>%
  spread(key=DATE, value=n)) %>%
    mutate(CAGR= round(100*((X2022/X2018)^(1/4)-1 ),1 )) %>%
  select(COUNTRY, CAGR, X2022) %>%
  left_join(GLP1_Share_DIA %>% select(COUNTRY, `2022`)) %>% 
    mutate(CAGR=CAGR/100) %>%
    ggplot(aes(`2022`, CAGR, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = X2022) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,20)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Diabetes Market Share")+
  ylab("Compound Annual Growth Rate (CAGR 2018-2022) \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent, limits =c(0,0.5)) +
    scale_x_continuous(labels = scales::percent)


GLOB_MTHLY_Diabetes_SU %>% select(MOLECULE, `INT-PRODUCT`) %>%
  distinct()

data.frame(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, `INT-PRODUCT`, DATE) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, `INT-PRODUCT` , DATE, n) %>%
  filter(DATE=="2022") %>% ungroup()) %>%
spread(key=`INT.PRODUCT`, value=n)
  


# --------------------------------------------------------------------------


# CONVERT STANDARD UNITS TO PATIENTS OBESITY --------------------------------------------------------------------------

GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Obesity_SU <- data.table(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup() )

GLOB_MTHLY_Obesity_SU_pats <- GLOB_MTHLY_Obesity_SU
unique(GLOB_MTHLY_Obesity_SU_pats$MOLECULE)

# GLOB_MTHLY_Obesity_SU_pats %>% filter(`INT-PRODUCT`=="SAXENDA") %>% select(COUNTRY) %>% distinct()



GLOB_MTHLY_Obesity_SU_pats <- GLOB_MTHLY_Obesity_SU_pats %>% mutate(n=ifelse( MOLECULE == "DULAGLUTIDE", n/299,
                                                                              ifelse(  MOLECULE == "EXENATIDE" , n/265, 
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/275,
                                                                                              ifelse(  MOLECULE == "LIXISENATIDE", n/274,
                                                                                                       ifelse(   `INT-PRODUCT` == "RYBELSUS", n/197,
                                                                                                                ifelse(  MOLECULE == "SEMAGLUTIDE", n/270, 
                                                                                                                        ifelse(MOLECULE == "ALBIGLUTIDE", n/252, n/270 ))))))))







Country_Pop_2021 <- fread("Country_Pop_2021.csv")
Country_Pop_2021 %>% select(Country) %>% arrange(Country)
GLOB_MTHLY_Obesity_SU_pats %>% select(COUNTRY) %>% arrange(COUNTRY)




Country_Pop_2021 %>% select(Country) %>% distinct() %>% arrange(Country)
names(Country_Pop_2021)[1] <- "COUNTRY"
names(Country_Pop_2021)[2] <- "POP_2021"

GLOB_MTHLY_Obesity_SU_pats %>% select(COUNTRY) %>% distinct() %>% arrange(COUNTRY)


data.frame(GLOB_MTHLY_Obesity_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp )



GLP1_Share_OBE <- fread("GLP1_Share_OBE.csv", header = T)


data.frame(GLOB_MTHLY_Obesity_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2017`)^(1/6)-1 ),1 )) %>%
  select(COUNTRY, `2022`) %>%
  left_join(GLP1_Share_OBE %>% select(COUNTRY, `2022`) %>% rename("GLP1_Share"="2022")) %>%
  mutate(`2022`=`2022`/100) %>%
  ggplot(aes(`2022`, GLP1_Share, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = GLP1_Share) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,10)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Outreach / Country Population Penetrance")+
  ylab("% GLP1 Share of Obesity Market \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)



data.frame(GLOB_MTHLY_Obesity_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2017`)^(1/6)-1 ),1 )) %>%
  select(COUNTRY, `2022`) %>%
  left_join(GLP1_Share_OBE %>% select(COUNTRY, `2022`) %>% rename("GLP1_Share"="2022")) %>%
  mutate(`2022`=`2022`/100) %>%
  left_join(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, DATE) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>%
  filter(DATE=="2022") %>% ungroup()  %>%
  rename("VOLUME"="n") %>% select(-DATE)) %>%
  ggplot(aes(`2022`, GLP1_Share, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = VOLUME) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,20)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Outreach / Country Population Penetrance")+
  ylab("% GLP1 Share of Obesity Market \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)



# -------------------------------
# CONVERT STANDARD UNITS TO PATIENTS BOTH DIABETES + OBESITY --------------------------------------------------------------------------


GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Both_SU_pats <- GLOB_MTHLY_Obesity_SU_pats %>% rename("n2"="n") %>% full_join(GLOB_MTHLY_Diabetes_SU_pats)
GLOB_MTHLY_Both_SU_pats[is.na(GLOB_MTHLY_Both_SU_pats)] <- 0

GLOB_MTHLY_Both_SU_pats <- GLOB_MTHLY_Both_SU_pats %>% mutate(Total=n2+n) %>% select(-c(n,n2))



data.frame(GLOB_MTHLY_Diabetes_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  select(COUNTRY, `2022`) %>%
  bind_cols(data.frame(GLOB_MTHLY_Obesity_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  select(COUNTRY, `2022`))








data.frame(GLOB_MTHLY_Both_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(Total))) %>%
    mutate(PatsProp=100*n/(POP_2021*1000)) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) 

data.frame(GLOB_MTHLY_Obesity_SU_pats %>% filter(DATE=="2022") %>% rename("n2"="n") %>% 
  full_join(GLOB_MTHLY_Diabetes_SU_pats %>% filter(DATE=="2022")) %>%
  mutate(n2=ifelse(is.na(n2),0,n2)) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(COUNTRY) %>%
  mutate(n2=sum(n2)) %>%
  mutate(n=sum(n)) %>%
  select(COUNTRY, n2, n) %>% distinct() %>%
  mutate(Total=n2+n) %>%
    mutate(n2=n2/Total) %>%
    mutate(n=n/Total)) 


# Growth rate vs pop penetrance DIA + OBE both combined
  
data.frame(
  GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, DATE) %>% 
    summarise(n=sum(n)) %>% select(COUNTRY, DATE, n) %>%
    filter(DATE=="2018"|DATE=="2022") %>% ungroup()
  ) %>%
  left_join(
    GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, DATE) %>%
      summarise(n2=sum(n)) %>% select(COUNTRY, DATE, n2) %>%
      filter(DATE=="2018"|DATE=="2022") %>% ungroup()
    ) %>% 
  mutate(Total=n+n2) %>% select(-c(n,n2)) %>% select(COUNTRY, DATE, Total) %>% spread(key=DATE, value=Total) %>%
  mutate(CAGR= round(100*((`2022`/`2018`)^(1/4)-1 ),1 )) %>%
  left_join(
    data.frame(
      GLOB_MTHLY_Both_SU_pats %>% left_join(Country_Pop_2021) %>% 
        group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(Total)
                                                        )
      ) %>%
      mutate(PatsProp=100*n/(POP_2021*1000)) %>%
      select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
      mutate(PatsProp=round(PatsProp,2)) %>%
      spread(key=DATE, value=PatsProp )  %>%
      select(COUNTRY, `2022`) %>%
      rename("Pen"="2022")) %>% 
  mutate(Pen=Pen/100) %>%
  mutate(CAGR=CAGR/100) %>%
  ungroup() %>%
  ggplot(aes(Pen, CAGR, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = `2022`) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,20)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n % Outreach / Country Population Penetrance")+
  ylab("Compound Annual Growth Rate (CAGR 2018-2022) \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.75)) +
  scale_x_continuous(labels = scales::percent)


# Regression GDP vs Population Penetrance

GDP_per_capita <- fread("GDP_per_capita.csv")

temp <- GDP_per_capita %>% left_join(
  data.frame(GLOB_MTHLY_Diabetes_SU_pats %>% left_join(Country_Pop_2021) %>% 
    group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
    mutate(PatsProp=100*n/(POP_2021*1000))) %>%
    select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
    mutate(PatsProp=round(PatsProp,2)) %>%
    filter(DATE=="2022")
  ) %>% drop_na()

temp$GDP <- temp$GDP / 10000
summary(lm(PatsProp ~ GDP, data=temp))
cor(temp$PatsProp, temp$GDP)

temp %>%
  mutate(PatsProp=PatsProp/100) %>%
  left_join(
    GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, DATE) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>%
  filter(DATE=="2022") %>% ungroup()  %>%
  rename("VOLUME"="n") %>% select(-DATE)
  ) %>%
  ggplot(aes(GDP, PatsProp, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = VOLUME) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,40)) +
  #ylim(0,1.60) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n GDP per capita (x 10,000) ") +
  ylab("% Outreach / Country Population Penetrance \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) 




temp <- GDP_per_capita %>% left_join(
  data.frame(GLOB_MTHLY_Obesity_SU_pats %>% left_join(Country_Pop_2021) %>% 
    group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
    mutate(PatsProp=100*n/(POP_2021*1000))) %>%
    select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
    mutate(PatsProp=round(PatsProp,2)) %>%
    filter(DATE=="2022")
  ) %>% drop_na()

temp$GDP <- temp$GDP / 10000
summary(lm(PatsProp ~ GDP, data=temp))
cor(temp$PatsProp, temp$GDP)


temp %>%
  mutate(PatsProp=PatsProp/100) %>%
  left_join(
    GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, DATE) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>%
  filter(DATE=="2022") %>% ungroup()  %>%
  rename("VOLUME"="n") %>% select(-DATE)
  ) %>%
  ggplot(aes(GDP, PatsProp, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = VOLUME) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,40)) +
  #ylim(0,1.60) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n GDP per capita (x 10,000) ") +
  ylab("% Outreach / Country Population Penetrance \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) 

# -------------------------------------------------
# CONVERT OPTUM PATIENTS TO IQVIA PATIENTS -----------------------------

DIA_Doses <- fread("DIA Doses.txt")
DIA_Doses <- DIA_Doses %>% filter(paid=="P")
DIA_Doses %>% select(pat_id) %>% distinct()
DIA_Doses %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 30750104

DIA_Doses <- DIA_Doses %>% select(-c(drug_id, prov, prov_type, specialty, taxonomy1, taxonomy2)) %>% mutate(from_dt=as.Date(from_dt))

max(DIA_Doses$from_dt)

DIA_Doses <- DIA_Doses %>% filter(from_dt>="2020-07-01")

DIA_Doses <- DIA_Doses %>% filter(drug_group!="Insulin")

DIA_Doses %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 19369621

DIA_Doses <- DIA_Doses %>% mutate(drug_group=ifelse(grepl("GLP",drug_group),"GLP1", drug_group ))

DIA_Doses %>% group_by(drug_group) %>% summarise(TOTAL=sum(weight*dayssup))

DIA_Doses %>% select(pat_id, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(PATS=sum(weight))

DIA_Doses %>% group_by(drug_group) %>% summarise(TOTAL=sum(weight*dayssup)) %>%
  left_join(DIA_Doses %>% select(pat_id, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(PATS=sum(weight))) %>%
  mutate(Days_per_pat=TOTAL/PATS)
 

DIA_Doses %>% group_by(drug_class) %>% summarise(TOTAL=sum(weight*dayssup)) %>%
  left_join(DIA_Doses %>% select(pat_id, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(PATS=sum(weight))) %>%
  mutate(Days_per_pat=TOTAL/PATS)


GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(COUNTRY=="US") %>% 
  select(ATC3, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)


unique(GLOB_MTHLY_Diabetes_SU$`ATC3 (SHORT NAME)`)
unique(GLOB_MTHLY_Diabetes_SU$MOLECULE)

data.frame(GLOB_MTHLY_Diabetes_SU %>% group_by(`ATC3 (SHORT NAME)`, MOLECULE) %>% summarise(n=sum(as.numeric(`VOLUME (FACTORED)`), na.rm=T)))

names(GLOB_MTHLY_Diabetes_SU)[6] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(DATE=="2019")

data.frame(GLOB_MTHLY_Diabetes_SU %>%  filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% group_by(`INT-PRODUCT`) %>% summarise(n=sum(VOLUME_FACTORED)))


# --------------------------

# CONVERT OPTUM PATIENTS TO IQVIA PATIENTS -----------------------------


OBE_Doses <- fread("OBE Doses.txt")
Amphet_to_remove <- fread("Amphet_to_remove.txt")
names(Amphet_to_remove)[1] <- "pat_id"

OBE_Doses <- OBE_Doses %>% anti_join(Amphet_to_remove)
OBE_Doses <- OBE_Doses %>% filter(paid=="P")
OBE_Doses %>% select(pat_id) %>% distinct()
OBE_Doses %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3803510

OBE_Doses <- OBE_Doses %>% select(-c(drug_id, prov, prov_type, specialty, taxonomy1, taxonomy2)) %>% mutate(from_dt=as.Date(from_dt))

max(OBE_Doses$from_dt)

OBE_Doses <- OBE_Doses %>% filter(from_dt>="2020-07-01")

OBE_Doses <- OBE_Doses %>% filter(drug_group!="Insulin")
OBE_Doses <- OBE_Doses %>% filter(drug_class!="Surgery")

unique(OBE_Doses$drug_class)

OBE_Doses %>% select(generic_name, drug_class) %>% distinct() %>% arrange(drug_class)

OBE_Doses %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1509103

OBE_Doses <- OBE_Doses %>% mutate(drug_class=ifelse(grepl("GLP",drug_class),"GLP1", drug_class ))

OBE_Doses %>% group_by(drug_class) %>% summarise(TOTAL=sum(weight*dayssup))

OBE_Doses %>% select(pat_id, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(PATS=sum(weight))

OBE_Doses %>% group_by(drug_class) %>% summarise(TOTAL=sum(weight*dayssup)) %>%
  left_join(OBE_Doses %>% select(pat_id, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(PATS=sum(weight))) %>%
  mutate(Days_per_pat=TOTAL/PATS)



OBE_Doses %>% group_by(drug_class) %>% summarise(TOTAL=sum(weight*dayssup)) %>%
  left_join(OBE_Doses %>% select(pat_id, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(PATS=sum(weight))) %>%
  mutate(Days_per_pat=TOTAL/PATS)

GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(COUNTRY=="US") %>% 
  select(ATC3, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)


unique(GLOB_MTHLY_Obesity_SU$`ATC3 (SHORT NAME)`)
unique(GLOB_MTHLY_Obesity_SU$MOLECULE)

data.frame(GLOB_MTHLY_Obesity_SU %>% group_by(`ATC3 (SHORT NAME)`, MOLECULE) %>% summarise(n=sum(as.numeric(`VOLUME (FACTORED)`), na.rm=T)))

names(GLOB_MTHLY_Obesity_SU)[6] <- "VOLUME_FACTORED"

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))


GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(DATE=="2021")

GLOB_MTHLY_Obesity_SU %>% group_by(MOLECULE) %>% summarise(n=sum(VOLUME_FACTORED))

data.frame(GLOB_MTHLY_Obesity_SU %>%  filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% group_by(`INT-PRODUCT`) %>% summarise(n=sum(VOLUME_FACTORED)))




# --------------------------
# Prices per GLP1 molecule over time ------------------------
GLOB_MTHLY_Diabetes_DOL <- fread("GLOB MTHLY Diabetes DOL.csv")
GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  filter(`ATC3 (SHORT NAME)`=="GLP-1")

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>% select(MOLECULE, `INT-PRODUCT`, COUNTRY, DATE, `VOLUME (FACTORED)`)
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% select(MOLECULE, `INT-PRODUCT`,  COUNTRY, DATE, `VOLUME (FACTORED)`)

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>% 
  mutate(`VOLUME (FACTORED)`=str_replace_all(`VOLUME (FACTORED)`, ",", ""))  %>%
  mutate(`VOLUME (FACTORED)` = as.numeric(`VOLUME (FACTORED)`))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% 
  mutate(`VOLUME (FACTORED)`=str_replace_all(`VOLUME (FACTORED)`, ",", ""))  %>%
  mutate(`VOLUME (FACTORED)` = as.numeric(`VOLUME (FACTORED)`))

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
names(GLOB_MTHLY_Diabetes_DOL)[5] <- "DOL"
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
names(GLOB_MTHLY_Diabetes_SU)[5] <- "SUs"

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>% group_by(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(n=sum(DOL))
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(n=sum(SUs))

names(GLOB_MTHLY_Diabetes_DOL)[5] <- "DOL"
names(GLOB_MTHLY_Diabetes_SU)[5] <- "SUs"



GLOB_MTHLY_Diabetes_DOL %>% full_join(GLOB_MTHLY_Diabetes_SU) %>% mutate(PRICE=DOL/SUs) %>% 
   ungroup() %>%
   select(COUNTRY, `INT-PRODUCT`, DATE, PRICE) %>%
   filter(COUNTRY=="GERMANY") %>%
   spread(key=DATE, value=PRICE)

data.frame(GLOB_MTHLY_Diabetes_DOL %>% full_join(GLOB_MTHLY_Diabetes_SU) %>% mutate(PRICE=DOL/SUs) %>% 
  ungroup() %>%
  select(COUNTRY, `INT-PRODUCT`, DATE, PRICE) %>%
  filter(DATE=="2022") %>%
  spread(key=`INT-PRODUCT`, value=PRICE))

OZEMPIC; RYBELSUS; VICTOZA; TRULICITY
BYDUREON ; LYXUMIA ; TANZEUM ; 

# ----------------------------
# Total Standard Units per country in 2022 -------------------------------

GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  filter(`ATC3 (SHORT NAME)`=="GLP-1")

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%   filter(grepl("2022", DATE))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  filter(grepl("2022", DATE))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% 
  mutate(`VOLUME (FACTORED)`=str_replace_all(`VOLUME (FACTORED)`, ",", ""))  %>%
  mutate(`VOLUME (FACTORED)` = as.numeric(`VOLUME (FACTORED)`))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% 
  mutate(`VOLUME (FACTORED)`=str_replace_all(`VOLUME (FACTORED)`, ",", ""))  %>%
  mutate(`VOLUME (FACTORED)` = as.numeric(`VOLUME (FACTORED)`))



data.frame(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY) %>% summarise(n=sum(`VOLUME (FACTORED)`)) %>% rename("DIA"="n") %>%
  full_join(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY) %>% summarise(n=sum(`VOLUME (FACTORED)`)) %>% rename("OBE"="n")) %>%
  mutate(TOTAL=DIA+OBE))





# -----------------------------------
# Number of patients on glp1 % all diabetes per country  -----------------------------

DIA_Doses <- fread("DIA Doses.txt")
DIA_Doses <- DIA_Doses %>% filter(paid=="P")
DIA_Doses %>% select(pat_id) %>% distinct()
DIA_Doses %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 30750104

DIA_Doses <- DIA_Doses %>% select(-c(drug_id, prov, prov_type, specialty, taxonomy1, taxonomy2)) %>% mutate(from_dt=as.Date(from_dt))

max(DIA_Doses$from_dt)

DIA_Doses <- DIA_Doses %>% filter(from_dt>="2020-07-01")

DIA_Doses <- DIA_Doses %>% filter(drug_group!="Insulin")

DIA_Doses %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 19369621

DIA_Doses <- DIA_Doses %>% mutate(drug_group=ifelse(grepl("GLP",drug_group),"GLP1", drug_group ))

DIA_Doses %>% group_by(drug_group) %>% summarise(TOTAL=sum(weight*dayssup))

DIA_Doses %>% select(pat_id, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(PATS=sum(weight))

DIA_Doses %>% group_by(drug_group) %>% summarise(TOTAL=sum(weight*dayssup)) %>%
  left_join(DIA_Doses %>% select(pat_id, weight, drug_group) %>% distinct() %>% group_by(drug_group) %>% summarise(PATS=sum(weight))) %>%
  mutate(Days_per_pat=TOTAL/PATS)
 


DIA_Doses %>% group_by(drug_class) %>% summarise(TOTAL=sum(weight*dayssup)) %>%
  left_join(DIA_Doses %>% select(pat_id, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(PATS=sum(weight))) %>%
  mutate(Days_per_pat=TOTAL/PATS)




GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% 
  select(ATC3, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`, COUNTRY)


unique(GLOB_MTHLY_Diabetes_SU$`ATC3 (SHORT NAME)`)
unique(GLOB_MTHLY_Diabetes_SU$MOLECULE)

data.frame(GLOB_MTHLY_Diabetes_SU %>% group_by(`ATC3 (SHORT NAME)`, MOLECULE) %>% summarise(n=sum(as.numeric(`VOLUME (FACTORED)`), na.rm=T)))

names(GLOB_MTHLY_Diabetes_SU)[6] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(DATE=="2022")

data.frame(GLOB_MTHLY_Diabetes_SU  %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`) %>% summarise(n=sum(VOLUME_FACTORED)) %>%
             spread(key=`ATC3 (SHORT NAME)`, value=n))

data.frame(GLOB_MTHLY_Diabetes_SU  %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% group_by(COUNTRY, `INT-PRODUCT`) %>% summarise(n=sum(VOLUME_FACTORED)) %>%
             spread(key= `INT-PRODUCT`, value=n))

# ---------------------------
# Plot % DIA ON GLP1 vs CAGR --------------------------------------

GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_SU <- data.table(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

library(ggrepel)
library(hrbrthemes)
library(viridis)

# Growth rate vs current size

Percentage_Unique_Pats_ON_GLP1_Global  <- fread("Percentage_Unique_Pats_ON_GLP1_Global.csv")

data.frame(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, DATE) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>%
  filter(DATE=="2018"|DATE=="2022") %>% ungroup() %>%
  spread(key=DATE, value=n)) %>%
    mutate(CAGR= round(100*((X2022/X2018)^(1/4)-1 ),1 )) %>%
  select(COUNTRY, CAGR, X2022) %>%
  left_join(Percentage_Unique_Pats_ON_GLP1_Global %>% select(COUNTRY, GLP1_PERC)) %>% 
    mutate(CAGR=CAGR/100) %>%
    ggplot(aes(GLP1_PERC, CAGR, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = X2022) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,20)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Diabetes Patients ON GLP1")+
  ylab("Compound Annual Growth Rate \n GLP1 Patient-Days (CAGR 2018-2022) \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent, limits =c(0,0.5)) +
    scale_x_continuous(labels = scales::percent)


# -------------------------------
# Number of patients on glp1 % all obesity per country  -----------------------------


GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% 
  select(ATC3, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`, COUNTRY)


unique(GLOB_MTHLY_Obesity_SU$`ATC3 (SHORT NAME)`)
unique(GLOB_MTHLY_Obesity_SU$MOLECULE)

data.frame(GLOB_MTHLY_Obesity_SU %>% group_by(`ATC3 (SHORT NAME)`, MOLECULE) %>% summarise(n=sum(as.numeric(`VOLUME (FACTORED)`), na.rm=T)))

names(GLOB_MTHLY_Obesity_SU)[6] <- "VOLUME_FACTORED"

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))


GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(DATE=="2018")

data.frame(GLOB_MTHLY_Obesity_SU  %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`) %>% summarise(n=sum(VOLUME_FACTORED)) %>%
             spread(key=`ATC3 (SHORT NAME)`, value=n))


data.frame(GLOB_MTHLY_Obesity_SU  %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% group_by(COUNTRY, `INT-PRODUCT`) %>% summarise(n=sum(VOLUME_FACTORED)) %>%
             spread(key= `INT-PRODUCT`, value=n))


# ------------------
# Plot % OBE ON GLP1 vs % POP COUNTRY --------------------------------------

GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Obesity_SU <- data.table(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup() )

GLOB_MTHLY_Obesity_SU_pats <- GLOB_MTHLY_Obesity_SU
unique(GLOB_MTHLY_Obesity_SU_pats$MOLECULE)



GLOB_MTHLY_Obesity_SU_pats <- GLOB_MTHLY_Obesity_SU_pats %>% mutate(n=ifelse( MOLECULE == "DULAGLUTIDE", n/299,
                                                                              ifelse(  MOLECULE == "EXENATIDE" , n/265, 
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/275,
                                                                                              ifelse(  MOLECULE == "LIXISENATIDE", n/274,
                                                                                                       ifelse(   `INT-PRODUCT` == "RYBELSUS", n/197,
                                                                                                                ifelse(  MOLECULE == "SEMAGLUTIDE", n/270, 
                                                                                                                        ifelse(MOLECULE == "ALBIGLUTIDE", n/252, n/270 ))))))))







Country_Pop_2021 <- fread("Country_Pop_2021.csv")
Country_Pop_2021 %>% select(Country) %>% arrange(Country)
GLOB_MTHLY_Obesity_SU_pats %>% select(COUNTRY) %>% arrange(COUNTRY)




Country_Pop_2021 %>% select(Country) %>% distinct() %>% arrange(Country)
names(Country_Pop_2021)[1] <- "COUNTRY"
names(Country_Pop_2021)[2] <- "POP_2021"

GLOB_MTHLY_Obesity_SU_pats %>% select(COUNTRY) %>% distinct() %>% arrange(COUNTRY)


data.frame(GLOB_MTHLY_Obesity_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp )



Percentage_Unique_Pats_ON_GLP1_Global_obesity  <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Obesity.csv")


data.frame(GLOB_MTHLY_Obesity_SU_pats %>% left_join(Country_Pop_2021) %>% 
  group_by(COUNTRY, DATE, POP_2021) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2021*1000))) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,2)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2017`)^(1/6)-1 ),1 )) %>%
  select(COUNTRY, `2022`) %>%
  mutate(`2022`=`2022`/100) %>%
  left_join(Percentage_Unique_Pats_ON_GLP1_Global_obesity %>% select(COUNTRY, GLP1_PERC) ) %>%
  ggplot(aes(`2022`, GLP1_PERC, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = GLP1_PERC) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,10)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Outreach / Country Population Penetrance")+
  ylab("% Obesity-treated patients ON GLP1\n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)




# -------------------------
# US TRx vs NBRx ----------------------------------------

US_MTHLY_Diabetes_TRx <- fread("US MTHLY Diabetes TRx.csv")
US_MTHLY_Diabetes_TRx <- US_MTHLY_Diabetes_TRx %>% filter(COUNTRY=="US" & grepl("GLP", ATC3)) %>% select(MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)
US_MTHLY_Diabetes_NBRx <- fread("US MTHLY Diabetes NBRx.csv")
US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% filter(COUNTRY=="US" & grepl("GLP", ATC3)) %>% select(MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(US_MTHLY_Diabetes_TRx)[4] <- "TRx"
names(US_MTHLY_Diabetes_NBRx)[4] <- "NBRx"

US_MTHLY_Diabetes_TRx <- US_MTHLY_Diabetes_TRx %>% mutate(TRx=str_replace_all(TRx, ",", ""))  %>% mutate(TRx = as.numeric(TRx))
US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% mutate(NBRx=str_replace_all(NBRx, ",", ""))  %>% mutate(NBRx = as.numeric(NBRx))

US_MTHLY_Diabetes_TRx <- US_MTHLY_Diabetes_TRx %>% group_by(MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(TRx=sum(as.numeric(TRx)))
US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% group_by(MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(NBRx=sum(as.numeric(NBRx)))

temp <- US_MTHLY_Diabetes_TRx %>% full_join(US_MTHLY_Diabetes_NBRx)

Dates <- US_MTHLY_Diabetes_NBRx %>% ungroup() %>% select(DATE) %>% distinct() %>% 
  mutate(YEAR=str_sub(DATE, 7L, 10L), MONTH=str_sub(DATE, 4L, 5L), DAY=str_sub(DATE, 1L, 2L)) %>%
  arrange(YEAR, MONTH, DAY)  %>% mutate(DATE_IND=row_number()) %>%
  select(DATE, DATE_IND)

temp %>% left_join(Dates) %>%
  gather(Type, value, TRx:NBRx) %>%
  ggplot(aes(DATE_IND, value, colour=Type)) +
  geom_line() +
  theme_minimal() +
  xlab(" \n Date") + ylab("NBRx & TRx \n") +
  facet_wrap(~`INT-PRODUCT`, scales = "free_y")


temp %>% left_join(Dates) %>%
  mutate(RATIO=NBRx/TRx) %>%
   ggplot(aes(DATE_IND, RATIO)) +
  geom_line() +
  theme_minimal() +
  xlab(" \n Date") + ylab("NBRx & TRx \n") +
  facet_wrap(~`INT-PRODUCT`)





US_MTHLY_Obesity_TRx <- fread("US MTHLY Obesity TRx.csv")
US_MTHLY_Obesity_TRx <- US_MTHLY_Obesity_TRx %>% filter(COUNTRY=="US" & grepl("GLP", ATC3)) %>% select(MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)
US_MTHLY_Obesity_NBRx <- fread("US MTHLY Obesity NBRx.csv")
US_MTHLY_Obesity_NBRx <- US_MTHLY_Obesity_NBRx %>% filter(COUNTRY=="US" & grepl("GLP", ATC3)) %>% select(MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(US_MTHLY_Obesity_TRx)[4] <- "TRx"
names(US_MTHLY_Obesity_NBRx)[4] <- "NBRx"

US_MTHLY_Obesity_TRx <- US_MTHLY_Obesity_TRx %>% mutate(TRx=str_replace_all(TRx, ",", ""))  %>% mutate(TRx = as.numeric(TRx))
US_MTHLY_Obesity_NBRx <- US_MTHLY_Obesity_NBRx %>% mutate(NBRx=str_replace_all(NBRx, ",", ""))  %>% mutate(NBRx = as.numeric(NBRx))

US_MTHLY_Obesity_TRx <- US_MTHLY_Obesity_TRx %>% group_by(MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(TRx=sum(as.numeric(TRx)))
US_MTHLY_Obesity_NBRx <- US_MTHLY_Obesity_NBRx %>% group_by(MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(NBRx=sum(as.numeric(NBRx)))

temp <- US_MTHLY_Obesity_TRx %>% full_join(US_MTHLY_Obesity_NBRx)

Dates <- US_MTHLY_Obesity_NBRx %>% ungroup() %>% select(DATE) %>% distinct() %>% 
  mutate(YEAR=str_sub(DATE, 7L, 10L), MONTH=str_sub(DATE, 4L, 5L), DAY=str_sub(DATE, 1L, 2L)) %>%
  arrange(YEAR, MONTH, DAY)  %>% mutate(DATE_IND=row_number()) %>%
  select(DATE, DATE_IND)

temp %>% left_join(Dates) %>%
  gather(Type, value, TRx:NBRx) %>%
  ggplot(aes(DATE_IND, value, colour=Type)) +
  geom_line() +
  theme_minimal() +
  xlab(" \n Date") + ylab("NBRx & TRx \n") +
  facet_wrap(~`INT-PRODUCT`, scales = "free_y")


temp %>% left_join(Dates) %>%
  mutate(RATIO=NBRx/TRx) %>%
   ggplot(aes(DATE_IND, RATIO)) +
  geom_line() +
  theme_minimal() +
  xlab(" \n Date") + ylab("NBRx & TRx \n") +
  facet_wrap(~`INT-PRODUCT`)



# ----------------
# Specialty responsible for new starts --------------

US_MTHLY_Diabetes_NBRx <- fread("US MTHLY Diabetes NBRx.csv")
US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% filter(COUNTRY=="US" ) %>% select(DATE, ATC3, `VOLUME (FACTORED)`, `SPECIALTY GROUP`)

names(US_MTHLY_Diabetes_NBRx)[3] <- "NBRx"
names(US_MTHLY_Diabetes_NBRx)[4] <- "SPECIALTY"

US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% mutate(NBRx=str_replace_all(NBRx, ",", ""))  %>% mutate(NBRx = as.numeric(NBRx))
US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% group_by(ATC3, DATE, SPECIALTY) %>% summarise(NBRx=sum(as.numeric(NBRx)))

data.frame(US_MTHLY_Diabetes_NBRx %>% ungroup() %>% 
  mutate(DATE=str_sub(DATE, 7L, 10L)) %>% filter(DATE=="2017") %>% select(-DATE) %>%
  group_by(ATC3, SPECIALTY) %>% summarise(NBRx=sum(as.numeric(NBRx))) %>% arrange(ATC3, -NBRx)) %>%
  mutate(SPECIALTY   =str_replace_all(SPECIALTY   , " ", "_"))  %>%
    mutate(ATC3    =str_replace_all(ATC3    , " ", "_"))  %>%
  ungroup() %>% spread(key=SPECIALTY, value=NBRx) %>%
  mutate(TOTAL=ALL_OTHERS+CARDS+ENDOS + `NP/PA` + PCP, CARDIO_SHARE=CARDS/TOTAL)

1148038      

US_MTHLY_Diabetes_NBRx <- fread("US MTHLY Diabetes NBRx.csv")
US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% filter(COUNTRY=="US" ) %>% select(DATE, ATC3, `VOLUME (FACTORED)`, `SPECIALTY`)

names(US_MTHLY_Diabetes_NBRx)[3] <- "NBRx"
names(US_MTHLY_Diabetes_NBRx)[4] <- "SPECIALTY"

US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% mutate(NBRx=str_replace_all(NBRx, ",", ""))  %>% mutate(NBRx = as.numeric(NBRx))
US_MTHLY_Diabetes_NBRx <- US_MTHLY_Diabetes_NBRx %>% group_by(ATC3, DATE, SPECIALTY) %>% summarise(NBRx=sum(as.numeric(NBRx)))

data.frame(US_MTHLY_Diabetes_NBRx %>% ungroup() %>% 
  mutate(DATE=str_sub(DATE, 7L, 10L)) %>% filter(DATE=="2017") %>% select(-DATE) %>%
  group_by(ATC3, SPECIALTY) %>% summarise(NBRx=sum(as.numeric(NBRx))) %>% arrange(ATC3, -NBRx)) %>%
  mutate(SPECIALTY   =str_replace_all(SPECIALTY   , " ", "_"))  %>%
    mutate(ATC3    =str_replace_all(ATC3    , " ", "_"))  %>%
  ungroup() %>% spread(key=SPECIALTY, value=NBRx) %>%
  select(ATC3, NEPHROLOGY)
# --------------------------------
# % DIA ON GLP1 vs % OBE ON GLP1 ------------------------

Percentage_Unique_Pats_ON_GLP1_Global <- fread("Percentage_Unique_Pats_ON_GLP1_Global.csv")

Percentage_Unique_Pats_ON_GLP1_Global_obesity  <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Obesity.csv")


temp <- Percentage_Unique_Pats_ON_GLP1_Global %>% select(COUNTRY, GLP1_PERC) %>% rename("DIA"="GLP1_PERC") %>%
  full_join(Percentage_Unique_Pats_ON_GLP1_Global_obesity %>% select(COUNTRY, GLP1_PERC) %>% rename("OBE"="GLP1_PERC"))


temp %>% 
  mutate(COLOUR=ifelse(COUNTRY=="NORWAY"|COUNTRY=="AUSTRALIA"|COUNTRY=="TURKEY"|COUNTRY=="BRAZIL",1,0)) %>%
  ggplot(aes(DIA, OBE, colour=as.factor(COLOUR))) +
    theme_minimal() +
  geom_point(size=5, show.legend = F, alpha=0.7) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Diabetes Patients ON GLP1")+
  ylab("% Obesity Patients ON GLP1 \n") +
  ggsci::scale_color_nejm() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)



# ---------------------------------
# Pool together quarterly and monthly  GLP1s  QRTLY -------------------------

# QUARTERLY

GLOB_QRTLY_Diabetes_SU <- fread("GLOB QRTLY Diabetes SU.csv")
GLOB_QRTLY_Obesity_SU <- fread("GLOB QRTLY Obesity SU.csv")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_QRTLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_QRTLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED)
GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED)


Diabetes_Q4_Predicted <- GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Diabetes_Q4_Predicted <- Diabetes_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))

Obesity_Q4_Predicted <- GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Obesity_Q4_Predicted <- Obesity_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))


GLOB_QRTLY_Diabetes_SU <- data.table(GLOB_QRTLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_QRTLY_Obesity_SU <- data.table(GLOB_QRTLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% left_join(Diabetes_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% left_join(Obesity_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")





#MONTHLY
GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_MTHLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_SU <- data.table(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_MTHLY_Obesity_SU <- data.table(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



GLOB_Diabetes_SU_Pooled <- GLOB_QRTLY_Diabetes_SU %>%  anti_join(GLOB_MTHLY_Diabetes_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Diabetes_SU) 


GLOB_Obesity_SU_Pooled <- GLOB_QRTLY_Obesity_SU %>%  anti_join(GLOB_MTHLY_Obesity_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Obesity_SU) 




GLOB_Diabetes_SU_Pooled_pats <- GLOB_Diabetes_SU_Pooled %>% mutate(n=ifelse(MOLECULE == "DULAGLUTIDE", n/296,
                                                                               ifelse(MOLECULE == "EXENATIDE" , n/266,
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/292,
                                                                                             ifelse(MOLECULE == "LIXISENATIDE", n/281,
                                                                                                    ifelse(`INT-PRODUCT` == "RYBELSUS", n/198,
                                                                                                           ifelse(`INT-PRODUCT` == "OZEMPIC", n/274, 
                                                                                                                  ifelse( MOLECULE == "ALBIGLUTIDE", n/296, n/274 ))))))))




GLOB_Obesity_SU_Pooled_pats <- GLOB_Obesity_SU_Pooled %>% mutate(n=ifelse( MOLECULE == "DULAGLUTIDE", n/299,
                                                                              ifelse(  MOLECULE == "EXENATIDE" , n/265, 
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/275,
                                                                                              ifelse(  MOLECULE == "LIXISENATIDE", n/274,
                                                                                                       ifelse(   `INT-PRODUCT` == "RYBELSUS", n/197,
                                                                                                                ifelse(  MOLECULE == "SEMAGLUTIDE", n/270, 
                                                                                                                        ifelse(MOLECULE == "ALBIGLUTIDE", n/252, n/270 ))))))))










Country_Pop_2023 <- fread("Country_Pop_2023.csv")
Country_Pop_2023 %>% select(Country) %>% arrange(Country)
names(Country_Pop_2023)[1] <- "COUNTRY"
names(Country_Pop_2023)[2] <- "POP_2023"
Country_Pop_2023 <- Country_Pop_2023 %>% drop_na()


data.frame(GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% inner_join(Country_Pop_2023) %>% 
  group_by(COUNTRY, DATE, POP_2023) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2023)) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,5)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2018`)^(1/4)-1 ),1 ))) %>%
  arrange(-X2022)

data.frame(GLOB_Diabetes_SU_Pooled_pats %>% inner_join(Country_Pop_2023) %>% 
  group_by(COUNTRY, DATE, POP_2023) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>% ungroup() %>%
  spread(key=DATE, value=n ) )



library(ggrepel)
library(hrbrthemes)
library(viridis)


data.frame(GLOB_Diabetes_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% filter(DATE=="2018"|DATE=="2022") %>% 
  left_join(
    GLOB_Obesity_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n2=sum(n)) %>% ungroup() %>% filter(DATE=="2018"|DATE=="2022") 
  ) %>% 
  mutate(Total=n+n2) %>% select(-c(n,n2)) %>% select(COUNTRY, DATE, Total) %>% spread(key=DATE, value=Total) %>%
  mutate(CAGR= round(((`2022`/`2018`)^(1/4)-1 ),1 ))) %>%
  select(COUNTRY, CAGR, X2022) %>%
  inner_join(
  GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY,DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>%
   left_join(
    GLOB_Obesity_SU_Pooled_pats %>% group_by(COUNTRY, DATE) %>% 
  summarise(n2=sum(n)) %>% ungroup()
  ) %>% mutate(Total=n+n2) %>% select(-c(n,n2)) %>% select(COUNTRY, DATE, Total) %>%
  filter(DATE=="2022")  %>%
  inner_join(Country_Pop_2023) %>% 
  mutate(PatsProp=Total/(POP_2023)) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup()
  ) %>%
  ungroup() %>%
  ggplot(aes(PatsProp, CAGR, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = X2022) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,20)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n % Outreach / Country Population Penetrance")+
  ylab("Compound Annual Growth Rate \n (CAGR 2018-2022) \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent, limits =c(0,1.25)) +
  scale_x_continuous(labels = scales::percent)

  
  

data.frame(GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY,DATE) %>% summarise(n=sum(n)) %>% ungroup() %>% 
  left_join( GLOB_Obesity_SU_Pooled_pats %>% group_by(COUNTRY, DATE) %>% summarise(n2=sum(n)) %>% ungroup()) %>%
  filter(DATE=="2022") %>%
  inner_join(Country_Pop_2023) %>% 
  mutate(DiaProp=n/POP_2023, ObeProp=n2/POP_2023) %>%
  select(COUNTRY, DiaProp, ObeProp) %>% ungroup())


data.frame(GLOB_Diabetes_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% filter(DATE=="2022") %>% 
  left_join(
    GLOB_Obesity_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n2=sum(n)) %>% ungroup() %>% filter(DATE=="2022") 
  ) %>% inner_join(Country_Pop_2023 %>% select(COUNTRY))
) 





data.frame(GLOB_Diabetes_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% filter(DATE=="2022") %>% 
  left_join(
    GLOB_Obesity_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n2=sum(n)) %>% ungroup() %>% filter(DATE=="2022") 
  ) %>% 
  mutate(Total=n+n2) %>% select(-c(n,n2)) %>% select(COUNTRY, DATE, Total) %>% spread(key=DATE, value=Total) %>%
  mutate(CAGR= round(((`2022`/`2018`)^(1/4)-1 ),1 ))) %>%
  select(COUNTRY, CAGR, X2022) %>%
  inner_join(
  GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY,DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>%
   left_join(
    GLOB_Obesity_SU_Pooled_pats %>% group_by(COUNTRY, DATE) %>% 
  summarise(n2=sum(n)) %>% ungroup()
  ) %>% mutate(Total=n+n2) %>% select(-c(n,n2)) %>% select(COUNTRY, DATE, Total) %>%
  filter(DATE=="2022")  %>%
  inner_join(Country_Pop_2023) %>% 
  mutate(PatsProp=Total/(POP_2023)) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup()
  ) %>%
  ungroup() %>%
  ggplot(aes(PatsProp, CAGR, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = X2022) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,20)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n % Outreach / Country Population Penetrance")+
  ylab("Compound Annual Growth Rate \n (CAGR 2018-2022) \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent, limits =c(0,1.25)) +
  scale_x_continuous(labels = scales::percent)
# ---------------------------------


# Pool together quarterly and monthly  All Classes  - Convert to Pats QRTLY  -------------------------

# QUARTERLY

GLOB_QRTLY_Diabetes_SU <- fread("GLOB QRTLY Diabetes SU.csv")
GLOB_QRTLY_Obesity_SU <- fread("GLOB QRTLY Obesity SU.csv")

# GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")
# GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%
  select(`ATC3 (SHORT NAME)`, COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%
  select(`ATC3 (SHORT NAME)`, COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_QRTLY_Diabetes_SU)[6] <- "VOLUME_FACTORED"
names(GLOB_QRTLY_Obesity_SU)[6] <- "VOLUME_FACTORED"

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED)
GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED)


Diabetes_Q4_Predicted <- GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Diabetes_Q4_Predicted <- Diabetes_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))

Obesity_Q4_Predicted <- GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Obesity_Q4_Predicted <- Obesity_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))


GLOB_QRTLY_Diabetes_SU <- data.table(GLOB_QRTLY_Diabetes_SU %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_QRTLY_Obesity_SU <- data.table(GLOB_QRTLY_Obesity_SU %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% left_join(Diabetes_Q4_Predicted %>% select(COUNTRY,`ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% left_join(Obesity_Q4_Predicted %>% select(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



#MONTHLY
GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% 
  select(COUNTRY, `ATC3 (SHORT NAME)` , MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% 
  select(COUNTRY, `ATC3 (SHORT NAME)` , MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_SU)[6] <- "VOLUME_FACTORED"
names(GLOB_MTHLY_Obesity_SU)[6] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_SU <- data.table(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY,`ATC3 (SHORT NAME)`, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_MTHLY_Obesity_SU <- data.table(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY,`ATC3 (SHORT NAME)`,  MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



GLOB_Diabetes_SU_Pooled <- GLOB_QRTLY_Diabetes_SU %>%  anti_join(GLOB_MTHLY_Diabetes_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Diabetes_SU) 


GLOB_Obesity_SU_Pooled <- GLOB_QRTLY_Obesity_SU %>%  anti_join(GLOB_MTHLY_Obesity_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Obesity_SU) 


data.frame(GLOB_Diabetes_SU_Pooled %>% filter(DATE=="2022") %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`) %>% summarise(n=sum(n)) %>%
             ungroup() %>% spread(key=`ATC3 (SHORT NAME)`, value=n))



data.frame(GLOB_Diabetes_SU_Pooled %>% filter(DATE=="2022" & `ATC3 (SHORT NAME)`=="GLP-1" ) %>%
             # filter(`INT-PRODUCT`=="BYDUREON"|
             #          `INT-PRODUCT`=="MOUNJARO"|
             #          `INT-PRODUCT`=="OZEMPIC"|
             #          `INT-PRODUCT`=="RYBELSUS"|
             #          `INT-PRODUCT`=="TRULICITY"|
             #          `INT-PRODUCT`=="VICTOZA") %>%
             group_by(COUNTRY, `INT-PRODUCT`) %>% summarise(n=sum(n)) %>%
             ungroup() %>% spread(key=`INT-PRODUCT`, value=n))


data.frame(GLOB_Obesity_SU_Pooled %>% filter(DATE=="2022") %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`) %>% summarise(n=sum(n)) %>%
             ungroup() %>% spread(key=`ATC3 (SHORT NAME)`, value=n))


# ----------------------------------
# % DIA ON GLP1 vs % OBE ON GLP1 QRTLY ------------------------

Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY.csv")

Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY  <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY.csv")


temp <- Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY %>% select(COUNTRY, GLP1_PERC) %>% rename("DIA"="GLP1_PERC") %>%
  full_join(Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY %>% select(COUNTRY, GLP1_PERC) %>% rename("OBE"="GLP1_PERC"))



library(ggrepel)
library(hrbrthemes)
library(viridis)


temp %>% 
  ggplot(aes(DIA, OBE)) +
    theme_minimal() +
  geom_point(size=5, show.legend = F, alpha=0.4, colour="deepskyblue4") +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Diabetes Patients ON GLP1")+
  ylab("% Obesity Patients ON GLP1 \n") +
  ggsci::scale_color_nejm() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)




# --------------
# CAGR vs % Diabetes Patients ON GLP1 QRTLY ---------------------------------

# QUARTERLY

GLOB_QRTLY_Diabetes_SU <- fread("GLOB QRTLY Diabetes SU.csv")
GLOB_QRTLY_Obesity_SU <- fread("GLOB QRTLY Obesity SU.csv")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_QRTLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_QRTLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED)
GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED)


Diabetes_Q4_Predicted <- GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Diabetes_Q4_Predicted <- Diabetes_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))

Obesity_Q4_Predicted <- GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Obesity_Q4_Predicted <- Obesity_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))


GLOB_QRTLY_Diabetes_SU <- data.table(GLOB_QRTLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_QRTLY_Obesity_SU <- data.table(GLOB_QRTLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% left_join(Diabetes_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% left_join(Obesity_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")





#MONTHLY
GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_MTHLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_SU <- data.table(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_MTHLY_Obesity_SU <- data.table(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



GLOB_Diabetes_SU_Pooled <- GLOB_QRTLY_Diabetes_SU %>%  anti_join(GLOB_MTHLY_Diabetes_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Diabetes_SU) 


GLOB_Obesity_SU_Pooled <- GLOB_QRTLY_Obesity_SU %>%  anti_join(GLOB_MTHLY_Obesity_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Obesity_SU) 




GLOB_Diabetes_SU_Pooled_pats <- GLOB_Diabetes_SU_Pooled %>% mutate(n=ifelse(MOLECULE == "DULAGLUTIDE", n/296,
                                                                               ifelse(MOLECULE == "EXENATIDE" , n/266,
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/292,
                                                                                             ifelse(MOLECULE == "LIXISENATIDE", n/281,
                                                                                                    ifelse(`INT-PRODUCT` == "RYBELSUS", n/198,
                                                                                                           ifelse(`INT-PRODUCT` == "OZEMPIC", n/274, 
                                                                                                                  ifelse( MOLECULE == "ALBIGLUTIDE", n/296, n/274 ))))))))




GLOB_Obesity_SU_Pooled_pats <- GLOB_Obesity_SU_Pooled %>% mutate(n=ifelse( MOLECULE == "DULAGLUTIDE", n/299,
                                                                              ifelse(  MOLECULE == "EXENATIDE" , n/265, 
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/275,
                                                                                              ifelse(  MOLECULE == "LIXISENATIDE", n/274,
                                                                                                       ifelse(   `INT-PRODUCT` == "RYBELSUS", n/197,
                                                                                                                ifelse(  MOLECULE == "SEMAGLUTIDE", n/270, 
                                                                                                                        ifelse(MOLECULE == "ALBIGLUTIDE", n/252, n/270 ))))))))










Country_Pop_2023 <- fread("Country_Pop_2023.csv")
Country_Pop_2023 %>% select(Country) %>% arrange(Country)
names(Country_Pop_2023)[1] <- "COUNTRY"
names(Country_Pop_2023)[2] <- "POP_2023"
Country_Pop_2023 <- Country_Pop_2023 %>% drop_na()


data.frame(GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% inner_join(Country_Pop_2023) %>% 
  group_by(COUNTRY, DATE, POP_2023) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=100*n/(POP_2023)) %>%
  select(COUNTRY, DATE, PatsProp) %>% ungroup() %>%
  mutate(PatsProp=round(PatsProp,5)) %>%
  spread(key=DATE, value=PatsProp ) %>%
  mutate(CAGR= round(100*((`2022`/`2018`)^(1/4)-1 ),1 ))) %>%
  arrange(-X2022)

data.frame(GLOB_Diabetes_SU_Pooled_pats %>% inner_join(Country_Pop_2023) %>% 
  group_by(COUNTRY, DATE, POP_2023) %>% summarise(n=sum(n)) %>%
  select(COUNTRY, DATE, n) %>% ungroup() %>%
  spread(key=DATE, value=n ) )



library(ggrepel)
library(hrbrthemes)
library(viridis)


Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY.csv")

Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY  <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY.csv")



data.frame(GLOB_Diabetes_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% filter(DATE=="2018"|DATE=="2022") %>% 
  left_join(
    GLOB_Obesity_SU_Pooled %>% group_by(COUNTRY, DATE) %>% 
  summarise(n2=sum(n)) %>% ungroup() %>% filter(DATE=="2018"|DATE=="2022") 
  ) %>% 
  mutate(Total=n+n2) %>% select(-c(n,n2)) %>% select(COUNTRY, DATE, Total) %>% spread(key=DATE, value=Total) %>%
  mutate(CAGR= round(((`2022`/`2018`)^(1/4)-1 ),1 ))) %>%
  select(COUNTRY, CAGR, X2022) %>%
  inner_join(
  Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY) %>%
  select(COUNTRY, CAGR, GLP1_PERC,X2022) %>% ungroup()  %>%
  ungroup() %>%
  ggplot(aes(GLP1_PERC, CAGR, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = X2022) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,20)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n % Diabetes Patients ON GLP1")+
  ylab("Compound Annual Growth Rate \n (CAGR 2018-2022) \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent, limits =c(0,1.25)) +
  scale_x_continuous(labels = scales::percent)

  

# --------------------------------------
# Plot % DIA|OBE ON GLP1 vs % POP COUNTRY QRTLY --------------------------------------


# QUARTERLY

GLOB_QRTLY_Diabetes_SU <- fread("GLOB QRTLY Diabetes SU.csv")
GLOB_QRTLY_Obesity_SU <- fread("GLOB QRTLY Obesity SU.csv")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_QRTLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_QRTLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED)
GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED)


Diabetes_Q4_Predicted <- GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Diabetes_Q4_Predicted <- Diabetes_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))

Obesity_Q4_Predicted <- GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Obesity_Q4_Predicted <- Obesity_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))


GLOB_QRTLY_Diabetes_SU <- data.table(GLOB_QRTLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_QRTLY_Obesity_SU <- data.table(GLOB_QRTLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% left_join(Diabetes_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% left_join(Obesity_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")





#MONTHLY
GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_MTHLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_SU <- data.table(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_MTHLY_Obesity_SU <- data.table(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



GLOB_Diabetes_SU_Pooled <- GLOB_QRTLY_Diabetes_SU %>%  anti_join(GLOB_MTHLY_Diabetes_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Diabetes_SU) 


GLOB_Obesity_SU_Pooled <- GLOB_QRTLY_Obesity_SU %>%  anti_join(GLOB_MTHLY_Obesity_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Obesity_SU) 




GLOB_Diabetes_SU_Pooled_pats <- GLOB_Diabetes_SU_Pooled %>% mutate(n=ifelse(MOLECULE == "DULAGLUTIDE", n/296,
                                                                               ifelse(MOLECULE == "EXENATIDE" , n/266,
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/292,
                                                                                             ifelse(MOLECULE == "LIXISENATIDE", n/281,
                                                                                                    ifelse(`INT-PRODUCT` == "RYBELSUS", n/198,
                                                                                                           ifelse(`INT-PRODUCT` == "OZEMPIC", n/274, 
                                                                                                                  ifelse( MOLECULE == "ALBIGLUTIDE", n/296, n/274 ))))))))




GLOB_Obesity_SU_Pooled_pats <- GLOB_Obesity_SU_Pooled %>% mutate(n=ifelse( MOLECULE == "DULAGLUTIDE", n/299,
                                                                              ifelse(  MOLECULE == "EXENATIDE" , n/265, 
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/275,
                                                                                              ifelse(  MOLECULE == "LIXISENATIDE", n/274,
                                                                                                       ifelse(   `INT-PRODUCT` == "RYBELSUS", n/197,
                                                                                                                ifelse(  MOLECULE == "SEMAGLUTIDE", n/270, 
                                                                                                                        ifelse(MOLECULE == "ALBIGLUTIDE", n/252, n/270 ))))))))










Country_Pop_2023 <- fread("Country_Pop_2023.csv")
Country_Pop_2023 %>% select(Country) %>% arrange(Country)
names(Country_Pop_2023)[1] <- "COUNTRY"
names(Country_Pop_2023)[2] <- "POP_2023"
Country_Pop_2023 <- Country_Pop_2023 %>% drop_na()

Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY  <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY.csv")

Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY  <- fread("Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY.csv")


library(ggrepel)
library(hrbrthemes)
library(viridis)


data.frame(GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% inner_join(Country_Pop_2023) %>% 
    filter(DATE=="2022") %>%
  group_by(COUNTRY, POP_2023) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=n/(POP_2023)) %>%
  select(COUNTRY, PatsProp) %>% ungroup()) %>%
  left_join(Percentage_Unique_Pats_ON_GLP1_Global_Diabetes_QRTLY %>% select(-DIABETES_POP )) %>%
  ggplot(aes(PatsProp , GLP1_PERC, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = GLP1_PERC) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,10)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Outreach / Country Population Penetrance")+
  ylab("% Diabetes-treated patients ON GLP1\n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)



data.frame(GLOB_Obesity_SU_Pooled_pats %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% inner_join(Country_Pop_2023) %>% 
    filter(DATE=="2022") %>%
  group_by(COUNTRY, POP_2023) %>% summarise(n=sum(n)) %>% 
  mutate(PatsProp=n/(POP_2023)) %>%
  select(COUNTRY, PatsProp) %>% ungroup()) %>%
  left_join(Percentage_Unique_Pats_ON_GLP1_Global_Obesity_QRTLY %>% select(-OBESITY)) %>%
  ggplot(aes(PatsProp , GLP1_PERC, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = GLP1_PERC) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,10)) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
   xlab("\n % Outreach / Country Population Penetrance")+
  ylab("% Obesity-treated patients ON GLP1\n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)
# -------------------------





# Pool together quarterly and monthly  All Classes  - PRICES QRTLY  -------------------------

# QUARTERLY

GLOB_QRTLY_Diabetes_DOL <- fread("GLOB QRTLY Diabetes DOL.csv")
GLOB_QRTLY_Obesity_DOL <- fread("GLOB QRTLY Obesity DOL.csv")

unique(GLOB_QRTLY_Obesity_DOL$MARKET)

GLOB_QRTLY_Diabetes_DOL <- GLOB_QRTLY_Diabetes_DOL %>%
  select(`ATC3 (SHORT NAME)`, COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_QRTLY_Obesity_DOL <- GLOB_QRTLY_Obesity_DOL %>%
  select(`ATC3 (SHORT NAME)`, COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_QRTLY_Diabetes_DOL)[6] <- "VOLUME_FACTORED"
names(GLOB_QRTLY_Obesity_DOL)[6] <- "VOLUME_FACTORED"

GLOB_QRTLY_Diabetes_DOL <- GLOB_QRTLY_Diabetes_DOL %>% 
  mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  

GLOB_QRTLY_Obesity_DOL <- GLOB_QRTLY_Obesity_DOL %>% 
  mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", "")) 


GLOB_QRTLY_Diabetes_DOL <- GLOB_QRTLY_Diabetes_DOL %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))
GLOB_QRTLY_Obesity_DOL <- GLOB_QRTLY_Obesity_DOL %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_QRTLY_Diabetes_DOL$VOLUME_FACTORED <- abs(GLOB_QRTLY_Diabetes_DOL$VOLUME_FACTORED)
GLOB_QRTLY_Obesity_DOL$VOLUME_FACTORED <- abs(GLOB_QRTLY_Obesity_DOL$VOLUME_FACTORED)


Diabetes_Q4_Predicted <- GLOB_QRTLY_Diabetes_DOL %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Diabetes_DOL %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Diabetes_Q4_Predicted <- Diabetes_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))

Obesity_Q4_Predicted <- GLOB_QRTLY_Obesity_DOL %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Obesity_DOL %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Obesity_Q4_Predicted <- Obesity_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))


GLOB_QRTLY_Diabetes_DOL <- GLOB_QRTLY_Diabetes_DOL %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_QRTLY_Obesity_DOL <- GLOB_QRTLY_Obesity_DOL %>%  mutate(DATE=str_sub(DATE, 7L, 10L))


GLOB_QRTLY_Diabetes_DOL <- data.table(GLOB_QRTLY_Diabetes_DOL %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_QRTLY_Obesity_DOL <- data.table(GLOB_QRTLY_Obesity_DOL %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_QRTLY_Diabetes_DOL <- GLOB_QRTLY_Diabetes_DOL %>% left_join(Diabetes_Q4_Predicted %>% select(COUNTRY,`ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Diabetes_DOL <- GLOB_QRTLY_Diabetes_DOL %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_QRTLY_Obesity_DOL <- GLOB_QRTLY_Obesity_DOL %>% left_join(Obesity_Q4_Predicted %>% select(COUNTRY, `ATC3 (SHORT NAME)`, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Obesity_DOL <- GLOB_QRTLY_Obesity_DOL %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



#MONTHLY
GLOB_MTHLY_Diabetes_DOL <- fread("GLOB MTHLY Diabetes DOL.csv")
GLOB_MTHLY_Obesity_DOL <- fread("GLOB MTHLY Obesity DOL.csv")

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>% 
  select(COUNTRY, `ATC3 (SHORT NAME)` , MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_MTHLY_Obesity_DOL <- GLOB_MTHLY_Obesity_DOL %>% 
  select(COUNTRY, `ATC3 (SHORT NAME)` , MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_DOL)[6] <- "VOLUME_FACTORED"
names(GLOB_MTHLY_Obesity_DOL)[6] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_DOL <- GLOB_MTHLY_Obesity_DOL %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_DOL <- GLOB_MTHLY_Obesity_DOL %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_DOL <- data.table(GLOB_MTHLY_Diabetes_DOL %>% group_by(COUNTRY,`ATC3 (SHORT NAME)`, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_MTHLY_Obesity_DOL <- data.table(GLOB_MTHLY_Obesity_DOL %>% group_by(COUNTRY,`ATC3 (SHORT NAME)`,  MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_MTHLY_Diabetes_DOL <- GLOB_MTHLY_Diabetes_DOL %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_MTHLY_Obesity_DOL <- GLOB_MTHLY_Obesity_DOL %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



GLOB_Diabetes_DOL_Pooled <- GLOB_QRTLY_Diabetes_DOL %>%  anti_join(GLOB_MTHLY_Diabetes_DOL %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Diabetes_DOL) 


GLOB_Obesity_DOL_Pooled <- GLOB_QRTLY_Obesity_DOL %>%  anti_join(GLOB_MTHLY_Obesity_DOL %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Obesity_DOL) 


data.frame(GLOB_Diabetes_DOL_Pooled %>% filter(DATE=="2022") %>% group_by(COUNTRY, `ATC3 (SHORT NAME)`) %>% summarise(n=sum(n)) %>%
             ungroup() %>% spread(key=`ATC3 (SHORT NAME)`, value=n))


sum(GLOB_Diabetes_DOL_Pooled$n[GLOB_Diabetes_DOL_Pooled$COUNTRY=="US"&GLOB_Diabetes_DOL_Pooled$DATE=="2022"&GLOB_Diabetes_DOL_Pooled$`ATC3 (SHORT NAME)`=="GLP-1"])



data.frame(GLOB_Diabetes_DOL_Pooled %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% 
             mutate(COUNTRY=ifelse(COUNTRY=="US", "US", "RoW")) %>%
             group_by(DATE, COUNTRY) %>% summarise(n=sum(n)) %>%
             ungroup())


data.frame(GLOB_Obesity_DOL_Pooled %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% 
            mutate(COUNTRY=ifelse(COUNTRY=="US", "US", "RoW")) %>%
             group_by(DATE, COUNTRY) %>% summarise(n=sum(n)) %>%
             ungroup())



GLOB_Diabetes_DOL_Pooled %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% 
             mutate(COUNTRY=ifelse(COUNTRY=="US", "US", "RoW")) %>%
             group_by(DATE, COUNTRY) %>% summarise(n=sum(n)) %>%
  bind_rows(GLOB_Obesity_DOL_Pooled %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% 
             mutate(COUNTRY=ifelse(COUNTRY=="US", "US", "RoW")) %>%
             group_by(DATE, COUNTRY) %>% summarise(n=sum(n))) %>%
  group_by(DATE, COUNTRY) %>% summarise(n=sum(n))



data.frame(GLOB_Diabetes_DOL_Pooled %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% 
            #mutate(COUNTRY=ifelse(COUNTRY=="US", "US", "RoW")) %>%
             filter(DATE=="2022") %>%
             group_by(COUNTRY) %>% summarise(n=sum(n)) %>%
             ungroup() %>%
             arrange(-n))


data.frame(GLOB_Obesity_DOL_Pooled %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% 
            #mutate(COUNTRY=ifelse(COUNTRY=="US", "US", "RoW")) %>%
             filter(DATE=="2022") %>%
             group_by(COUNTRY) %>% summarise(n=sum(n)) %>%
             ungroup() %>%
             arrange(-n))





GLOB_Diabetes_DOL_Pooled <- GLOB_Diabetes_DOL_Pooled %>% group_by(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(n=sum(n))
GLOB_Diabetes_SU_Pooled <- GLOB_Diabetes_SU_Pooled %>% group_by(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE) %>% summarise(n=sum(n))

names(GLOB_Diabetes_DOL_Pooled)[5] <- "DOL"
names(GLOB_Diabetes_SU_Pooled)[5] <- "SUs"

GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
to_keep <- GLOB_MTHLY_Diabetes_SU %>% filter(`ATC3 (SHORT NAME)`=="GLP-1") %>% select(MOLECULE, `INT-PRODUCT`) %>% distinct()

GLOB_Diabetes_DOL_Pooled %>% inner_join(to_keep) %>% 
  full_join(GLOB_Diabetes_SU_Pooled  %>% inner_join(to_keep) ) %>%
  mutate(DOL=ifelse(COUNTRY=="FRANCE"|COUNTRY=="GERMANY", DOL/17, DOL)) %>%
  mutate(PRICE=DOL/SUs) %>% 
   ungroup() %>%
   select(COUNTRY, `INT-PRODUCT`, DATE, PRICE) %>%
   filter(COUNTRY=="SPAIN") %>%
   spread(key=DATE, value=PRICE)

data.frame(GLOB_Diabetes_DOL_Pooled %>% inner_join(to_keep) %>% 
  full_join(GLOB_Diabetes_SU_Pooled  %>% inner_join(to_keep) ) %>%
  mutate(DOL=ifelse(COUNTRY=="FRANCE"|COUNTRY=="GERMANY", DOL/17, DOL)) %>%
    mutate(PRICE=DOL/SUs) %>% 
  ungroup() %>%
  select(COUNTRY, `INT-PRODUCT`, DATE, PRICE) %>%
  filter(DATE=="2022") %>%
  spread(key=`INT-PRODUCT`, value=PRICE))

data.frame(GLOB_Diabetes_DOL_Pooled %>% inner_join(to_keep) %>% 
  full_join(GLOB_Diabetes_SU_Pooled  %>% inner_join(to_keep) ) %>%
    filter(COUNTRY=="US") %>% 
    mutate(PRICE=DOL/SUs) %>% 
    select(-c(DOL, SUs)))
# -----------------------------
# GDP per capita - All countries --------------------------------

# QUARTERLY

GLOB_QRTLY_Diabetes_SU <- fread("GLOB QRTLY Diabetes SU.csv")
GLOB_QRTLY_Obesity_SU <- fread("GLOB QRTLY Obesity SU.csv")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%   filter(`ATC3 (SHORT NAME)`=="GLP-1")

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_QRTLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_QRTLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Diabetes_SU$VOLUME_FACTORED)
GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED <- abs(GLOB_QRTLY_Obesity_SU$VOLUME_FACTORED)


Diabetes_Q4_Predicted <- GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Diabetes_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Diabetes_Q4_Predicted <- Diabetes_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))

Obesity_Q4_Predicted <- GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(2) %>% rename("Q2"="DATE") %>% rename("Q2_VOLUME"="VOLUME_FACTORED") %>% 
  left_join(
GLOB_QRTLY_Obesity_SU %>% filter(grepl("2022", DATE)) %>%
  group_by(COUNTRY, MOLECULE, `INT-PRODUCT`) %>%
  slice(3) %>% rename("Q3"="DATE") %>% rename("Q3_VOLUME"="VOLUME_FACTORED")) %>%
  mutate(Q4="01/12/2022") %>% mutate(Q4_VOLUME= (((Q3_VOLUME-Q2_VOLUME)/Q2_VOLUME) + 1) *Q3_VOLUME  ) %>%
  ungroup() %>% mutate(Q4_VOLUME=ifelse(is.na(Q4_VOLUME), 0, Q4_VOLUME))

Obesity_Q4_Predicted <- Obesity_Q4_Predicted  %>% mutate(Q4_VOLUME=ifelse(Q4_VOLUME=="Inf", Q3_VOLUME, Q4_VOLUME))


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))


GLOB_QRTLY_Diabetes_SU <- data.table(GLOB_QRTLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_QRTLY_Obesity_SU <- data.table(GLOB_QRTLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% left_join(Diabetes_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Diabetes_SU <- GLOB_QRTLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% left_join(Obesity_Q4_Predicted %>% select(COUNTRY, MOLECULE, `INT-PRODUCT`, Q4_VOLUME)) %>%
  mutate(n=ifelse(DATE=="2022", n+Q4_VOLUME, n)) %>% select(-Q4_VOLUME)

GLOB_QRTLY_Obesity_SU <- GLOB_QRTLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")





#MONTHLY
GLOB_MTHLY_Diabetes_SU <- fread("GLOB MTHLY Diabetes SU.csv")
GLOB_MTHLY_Obesity_SU <- fread("GLOB MTHLY Obesity SU.csv")

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(grepl("GLP", ATC3)) %>%
  select(COUNTRY, MOLECULE, `INT-PRODUCT`, DATE, `VOLUME (FACTORED)`)

names(GLOB_MTHLY_Diabetes_SU)[5] <- "VOLUME_FACTORED"
names(GLOB_MTHLY_Obesity_SU)[5] <- "VOLUME_FACTORED"

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% mutate(VOLUME_FACTORED=str_replace_all(VOLUME_FACTORED, ",", ""))  %>%
  mutate(VOLUME_FACTORED = as.numeric(VOLUME_FACTORED))

GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))
GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>%  mutate(DATE=str_sub(DATE, 7L, 10L))

GLOB_MTHLY_Diabetes_SU <- data.table(GLOB_MTHLY_Diabetes_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())

GLOB_MTHLY_Obesity_SU <- data.table(GLOB_MTHLY_Obesity_SU %>% group_by(COUNTRY, MOLECULE , `INT-PRODUCT`, DATE) %>% 
  summarise(n=sum(VOLUME_FACTORED)) %>%
  ungroup())


GLOB_MTHLY_Diabetes_SU <- GLOB_MTHLY_Diabetes_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")

GLOB_MTHLY_Obesity_SU <- GLOB_MTHLY_Obesity_SU %>% filter(DATE=="2018"|DATE=="2019"|DATE=="2020"|DATE=="2021"|DATE=="2022")



GLOB_Diabetes_SU_Pooled <- GLOB_QRTLY_Diabetes_SU %>%  anti_join(GLOB_MTHLY_Diabetes_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Diabetes_SU) 


GLOB_Obesity_SU_Pooled <- GLOB_QRTLY_Obesity_SU %>%  anti_join(GLOB_MTHLY_Obesity_SU %>% select(COUNTRY) %>% distinct()) %>%
  bind_rows(GLOB_MTHLY_Obesity_SU) 




GLOB_Diabetes_SU_Pooled_pats <- GLOB_Diabetes_SU_Pooled %>% mutate(n=ifelse(MOLECULE == "DULAGLUTIDE", n/296,
                                                                               ifelse(MOLECULE == "EXENATIDE" , n/266,
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/292,
                                                                                             ifelse(MOLECULE == "LIXISENATIDE", n/281,
                                                                                                    ifelse(`INT-PRODUCT` == "RYBELSUS", n/198,
                                                                                                           ifelse(`INT-PRODUCT` == "OZEMPIC", n/274, 
                                                                                                                  ifelse( MOLECULE == "ALBIGLUTIDE", n/296, n/274 ))))))))




GLOB_Obesity_SU_Pooled_pats <- GLOB_Obesity_SU_Pooled %>% mutate(n=ifelse( MOLECULE == "DULAGLUTIDE", n/299,
                                                                              ifelse(  MOLECULE == "EXENATIDE" , n/265, 
                                                                                      ifelse(MOLECULE == "LIRAGLUTIDE", n/275,
                                                                                              ifelse(  MOLECULE == "LIXISENATIDE", n/274,
                                                                                                       ifelse(   `INT-PRODUCT` == "RYBELSUS", n/197,
                                                                                                                ifelse(  MOLECULE == "SEMAGLUTIDE", n/270, 
                                                                                                                        ifelse(MOLECULE == "ALBIGLUTIDE", n/252, n/270 ))))))))










Country_Pop_2023 <- fread("Country_Pop_2023.csv")
Country_Pop_2023 %>% select(Country) %>% arrange(Country)
names(Country_Pop_2023)[1] <- "COUNTRY"
names(Country_Pop_2023)[2] <- "POP_2023"
Country_Pop_2023 <- Country_Pop_2023 %>% drop_na()



data.frame(GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY,DATE) %>% summarise(n=sum(n)) %>% ungroup() %>% 
  left_join( GLOB_Obesity_SU_Pooled_pats %>% group_by(COUNTRY, DATE) %>% summarise(n2=sum(n)) %>% ungroup()) %>%
  filter(DATE=="2022") %>%
  inner_join(Country_Pop_2023) %>% 
  mutate(DiaProp=n/POP_2023, ObeProp=n2/POP_2023) %>%
  select(COUNTRY, DiaProp, ObeProp) %>% ungroup())

GDP_per_capita_ALL <- fread("GDP_per_capita_ALL.csv")



temp <- data.frame(GLOB_Diabetes_SU_Pooled_pats %>% group_by(COUNTRY,DATE) %>% summarise(n=sum(n)) %>% ungroup() %>% 
  left_join( GLOB_Obesity_SU_Pooled_pats %>% group_by(COUNTRY, DATE) %>% summarise(n2=sum(n)) %>% ungroup()) %>%
  filter(DATE=="2022") %>%
  inner_join(Country_Pop_2023) %>% 
  mutate(DiaProp=n/POP_2023, ObeProp=n2/POP_2023) %>%
  select(COUNTRY, DiaProp, ObeProp) %>% ungroup()) %>%
  inner_join(GDP_per_capita_ALL) %>%
  mutate(GDP=GDP/10000)

cor(temp$DiaProp, temp$GDP)
cor(temp$ObeProp, temp$GDP)

summary(lm(DiaProp ~ GDP, data=temp))
summary(lm(ObeProp ~ GDP, data=temp))

temp %>% left_join(GLOB_Diabetes_SU_Pooled %>% filter(DATE=="2022") %>%
  group_by(COUNTRY) %>% summarise(n=sum(n))) %>%
  ggplot(aes(GDP, DiaProp, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = n) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,40)) +
  #ylim(0,1.60) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n GDP per capita (x 10,000) ") +
  ylab("% Outreach / Country Population Penetrance \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) 




temp %>% left_join(GLOB_Obesity_SU_Pooled %>% filter(DATE=="2022") %>%
  group_by(COUNTRY) %>% summarise(n=sum(n))) %>%
  ggplot(aes(GDP, ObeProp, colour=COUNTRY)) +
  theme_minimal() +
  geom_point(aes(size = n) , show.legend = F, alpha=0.5) +
  scale_size(range = c(2,40)) +
  #ylim(0,1.60) +
  geom_text_repel(aes(label = COUNTRY), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2, show.legend = F) +
  xlab("\n GDP per capita (x 10,000) ") +
  ylab("% Outreach / Country Population Penetrance \n") +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent) 


# ---------------------------
