
# US Obesity  - DANUGLIPRON -
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
options(scipen = 999)


# Population and BMIs --------
DANU_Demographics <- fread("DANU Demographics.txt")

DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% group_by(diagnosis) %>%
  summarise(pop=sum(weight))

# diagnosis                 pop
# <chr>                   <dbl>
# 1 Diabetes + Obesity  40282960.
# 2 Obesity            106469049.

DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% group_by(diagnosis, obesity_condition) %>%
  summarise(pop=sum(weight))

# diagnosis          obesity_condition           pop
# <chr>              <chr>                     <dbl>
# 1 Diabetes + Obesity General Obesity          1452926.
# 2 Diabetes + Obesity Moderate Obesity         9962752.
# 3 Diabetes + Obesity Morbid Obesity          14005268.
# 4 Diabetes + Obesity Overweight               9261045.
# 5 Diabetes + Obesity Severe Obesity           5489530.
# 6 Diabetes + Obesity Treatment For Obesity     111439.
# 7 Obesity            General Obesity          3017804.
# 8 Obesity            Moderate Obesity        29154848.
# 9 Obesity            Morbid Obesity          16778999.
# 10 Obesity           Overweight              44472651.
# 11 Obesity           Severe Obesity          12754600.
# 12 Obesity           Treatment For Obesity     290147.

DANU_Demographics %>% filter(obesity_condition == "Moderate Obesity"|
                               obesity_condition == "Severe Obesity"|
                               obesity_condition == "Morbid Obesity"|
                               obesity_condition == "Overweight") %>%
  summarise(pop=sum(weight))

# 141879692

DANU_Demographics %>% filter(obesity_condition == "Moderate Obesity"|
                               obesity_condition == "Severe Obesity"|
                               obesity_condition == "Morbid Obesity"|
                               obesity_condition == "Overweight") %>%
  group_by(obesity_condition) %>% summarise(pop=sum(weight))

# obesity_condition       pop
# <chr>                 <dbl>
# 1 Moderate Obesity  39117600.
# 2 Morbid Obesity    30784266.
# 3 Overweight        53733696.
# 4 Severe Obesity    18244130.

OBE_pats <- DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>%select(patid, weight, diagnosis)

DANU_Events <- fread("DANU Events.txt")

OBE_pats <- OBE_pats %>% left_join(DANU_Events, by=c("patid"="patid", "weight"="weight"))
        
OBE_pats <- OBE_pats %>% filter(grepl("BMI", code))       

OBE_pats$code <- as.character(OBE_pats$code)
OBE_pats$code <- parse_number(OBE_pats$code)

OBE_pats %>% group_by(diagnosis) %>% summarise(n=weighted.mean(code, weight_3))
# 
# diagnosis              n
# <chr>              <dbl>
#   1 Diabetes + Obesity  33.6
# 2 Obesity             30.7

OBE_pats %>% group_by(diagnosis) %>% summarise(n=weighted.median(code, weight_3))
# 
# diagnosis              n
# <chr>              <dbl>
#   1 Diabetes + Obesity  31.8
# 2 Obesity             29.2

OBE_pats %>% 
  ggplot(aes(code, colour=diagnosis, fill=diagnosis))+
  geom_histogram(alpha=0.9, show.legend = F)+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")+
  facet_wrap(~diagnosis, ncol = 1)+
  scale_fill_viridis_d(option = "D")+
  scale_colour_viridis_d(option = "D")
  
  




OBE_pats <- OBE_pats %>% group_by(patid) %>% slice(n())


OBE_pats %>% group_by(diagnosis) %>% summarise(n=weighted.mean(code, weight_3))

# diagnosis              n
# <chr>              <dbl>
#   1 Diabetes + Obesity  33.5
# 2 Obesity             31.0

OBE_pats %>% group_by(diagnosis) %>% summarise(n=weighted.median(code, weight_3))

# diagnosis              n
# <chr>              <dbl>
#   1 Diabetes + Obesity  31.8
# 2 Obesity             29.2

OBE_pats %>% 
  ggplot(aes(code, colour=diagnosis, fill=diagnosis))+
  geom_histogram(alpha=0.9, show.legend = F)+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")+
  facet_wrap(~diagnosis, ncol = 1)+
  scale_fill_viridis_d(option = "D")+
  scale_colour_viridis_d(option = "D")


# ------
# Treatment Experienced Obesity -----------
OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(4:63)

OBE_Drug_Histories[OBE_Drug_Histories != "-"] <- 1  # on drug 
OBE_Drug_Histories[OBE_Drug_Histories == "-"] <- 0  # no drug

OBE_Drug_Histories[] <- lapply(OBE_Drug_Histories,as.numeric)

OBE_Drug_Histories$SUM <- rowSums(OBE_Drug_Histories)

OBE_Drug_Historiess_LONG     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Pats_vec <- OBE_Drug_Historiess_LONG %>% select(patient, weight)

OBE_Drug_Histories <- Pats_vec %>% bind_cols(OBE_Drug_Histories)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(SUM != 0)

sum(OBE_Drug_Histories$weight) # 9155116

Treatment_exp_Vector <- OBE_Drug_Histories %>% select(patient, weight)

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Box_Histories     <- fread("OBE Box Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Demographics     <- fread("OBE Demographics.txt", integer64 = "character", stringsAsFactors = F)
OBE_Demographics <- OBE_Drug_Histories %>% select(patient) %>% left_join(OBE_Demographics, by=c("patient"="patid"))

fwrite(OBE_Demographics, "OBE_Demographics.txt", sep="\t")

OBE_Drug_Histories <- Treatment_exp_Vector %>% left_join(OBE_Drug_Histories)
OBE_Box_Histories <- Treatment_exp_Vector %>% left_join(OBE_Box_Histories)
OBE_Demographics <- Treatment_exp_Vector %>% left_join(OBE_Demographics, by=c("patient"="patid", "weight"="weight"))

fwrite(OBE_Drug_Histories, "OBE_Drug_Histories_v2.txt")
fwrite(OBE_Box_Histories, "OBE_Box_Histories_v2.txt")
fwrite(OBE_Demographics, "OBE_Demographics_v2.txt")


# -----
# Flows Matrix ------
OBE_Flows_Matrix <- fread("OBE_Flows_Matrix.txt")

row.names(OBE_Flows_Matrix) <- OBE_Flows_Matrix$`From / To`

OBE_Flows_Matrix <- OBE_Flows_Matrix %>% select(-c(`From / To`))



grid.bubble.plot <- function(df, 
                             axis_labels_size=10, 
                             aspect_ratio=1/1,
                             values_text_size=2,
                             values_text_color="black",
                             x_axis_position="top", # or "bottom",
                             bubble_size_range=c(5, 25),
                             bubble_alpha=0.5,
                             bubble_shape=21,
                             bubble_edge_stroke=0) {
  col_names <- colnames(df)
  row_names <- rownames(df)
  values <- as.vector(as.matrix(df))
  values_x <- as.vector(sapply(col_names, function(i) rep(i, nrow(df))))
  values_y <- as.vector(rep(row_names, dim(df)[2]))
  res_df <- data.frame(values = values, values_x = values_x, values_y)
  res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("Na?ve/Lapsed","Weight Loss","Anorectic","Antiobesity","GLP1 Oral","GLP1 Injectable","Surgery"))) %>%
                         mutate(values_y=fct_relevel(values_y,c("Na?ve/Lapsed","Weight Loss","Anorectic","Antiobesity","GLP1 Oral","GLP1 Injectable","Surgery"))))
  gg <- ggplot(res_df, aes(x=values_x, y=values_y, size = values, fill=factor(values_x))) +
    geom_point(alpha=bubble_alpha, shape=bubble_shape, stroke=bubble_edge_stroke) +
    scale_size(range = bubble_size_range) +
    scale_fill_brewer(palette = "Blues") +
    scale_x_discrete(position = x_axis_position) +
    scale_y_discrete(limits=rev)+
    geom_text(aes(label=values), size=values_text_size, color=values_text_color,) +
    theme(line=element_blank(), 
          panel.background=element_blank(),
          legend.position="none",
          axis.title=element_blank(),
          axis.text=element_text(size=axis_labels_size),
          aspect.ratio=aspect_ratio)
  gg
}

grid.bubble.plot(OBE_Flows_Matrix)


# ----------
# Age and gender --------

OBE_Demographics <- fread("OBE_Demographics_v2.txt")

OBE_Demographics %>% group_by(gender) %>% summarise(n=sum(weight))

gender        n
<chr>     <dbl>
1 F      5377152.
2 M      3777964.


OBE_Demographics %>% group_by(gender) %>% summarise(n=weighted.mean(age, weight))
OBE_Demographics %>% group_by(gender) %>% summarise(n=weighted.median(age, weight))

OBE_Demographics %>% select(patient, weight, age) %>% mutate(age_group = ifelse(age>=18 & age<30,"18_to_29", 
                                                                       ifelse(age>=30 & age<40, "30_to_39", 
                                                                              ifelse(age>=40 & age<50, "40_to_49",
                                                                                     ifelse(age>=50 & age<60, "50_to_59", 
                                                                                            ifelse(age>=60 & age<70,"60_to_69",
                                                                                                   ifelse(age>=70 & age<80, "70_to_79", "+80"))))))) %>%
                                                               group_by(age_group) %>% summarise(n=sum(weight))

age_group        n
<chr>        <dbl>
1 +80         41029.
2 18_to_29  2479708.
3 30_to_39  2283489.
4 40_to_49  1899728.
5 50_to_59  1467072.
6 60_to_69   776512.
7 70_to_79   207577.



OBE_Demographics %>% select(patient, weight, gender, age) %>%
  ggplot(aes(x=age ,fill=gender))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")+
  facet_wrap(~gender, ncol = 1)+
  scale_fill_viridis_d()

# -----
# Age and BMI distribution: All OBESITY vs Treat_Experienced --------------
OBE_Demographics <- fread("OBE Demographics.txt")
OBE_Demographics <- OBE_Demographics %>% filter(diagnosis=="Obesity")

OBE_Demographics %>% select(patid, weight, age) %>%
  ggplot(aes(x=age ))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black", fill="midnightblue")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")

sum(OBE_Demographics$weight) # 106469049
weighted.mean(OBE_Demographics$age, OBE_Demographics$weight) # 49.09858
weighted.median(OBE_Demographics$age, OBE_Demographics$weight) #  47.5



OBE_Demographics_v2 <- fread("OBE_Demographics_v2.txt")

OBE_Demographics_v2 %>% select(patient, weight, age) %>%
  ggplot(aes(x=age ))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black", fill="deepskyblue4")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")

sum(OBE_Demographics_v2$weight) # 9155116
weighted.mean(OBE_Demographics_v2$age, OBE_Demographics_v2$weight) # 40.37536
weighted.median(OBE_Demographics_v2$age, OBE_Demographics_v2$weight) #  38.5

All_OBE <- OBE_Demographics %>% select(patid)
Treat_exp <- OBE_Demographics_v2 %>% select(patient)

# BMI 
DANU_Events <- fread("DANU Events.txt")

All_OBE <- All_OBE %>% left_join(DANU_Events, by=c("patid"="patid"))
Treat_exp <- Treat_exp %>% left_join(DANU_Events, by=c("patient"="patid"))



All_OBE <- All_OBE %>% filter(grepl("BMI", code))       

All_OBE$code <- as.character(All_OBE$code)
All_OBE$code <- parse_number(All_OBE$code)

All_OBE <- All_OBE %>% group_by(patid) %>% slice(n())

All_OBE %>% ungroup() %>% summarise(n=weighted.mean(code, weight)) #30.4

All_OBE %>%  ungroup() %>%  summarise(n=weighted.median(code, weight))#28.8


All_OBE %>% 
  ggplot(aes(code))+
  geom_histogram(alpha=0.9, show.legend = F, colour="black", fill="midnightblue")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")



Treat_exp <- Treat_exp %>% filter(grepl("BMI", code))       

Treat_exp$code <- as.character(Treat_exp$code)
Treat_exp$code <- parse_number(Treat_exp$code)

Treat_exp <- Treat_exp %>% group_by(patient) %>% slice(n())

Treat_exp %>% ungroup() %>% summarise(n=weighted.mean(code, weight)) #31.5

Treat_exp %>%  ungroup() %>%  summarise(n=weighted.median(code, weight))#29.8

Treat_exp %>% 
  ggplot(aes(code))+
  geom_histogram(alpha=0.9, show.legend = F, colour="black", fill="deepskyblue4")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")


# Only ages for those with BMI > 30 

All_OBE <- All_OBE %>% filter(code >30)
Treat_exp <- Treat_exp %>% filter(code >30)

All_OBE <- All_OBE %>% select(patid)
Treat_exp <- Treat_exp %>% select(patient)

All_OBE <- All_OBE %>% left_join(OBE_Demographics) %>% select(patid, weight, age)
Treat_exp <- Treat_exp %>% left_join(OBE_Demographics_v2) %>% select(patient, weight, age)



All_OBE %>% select(patid, weight, age) %>%
  ggplot(aes(x=age ))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black", fill="brown3")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")


All_OBE %>% ungroup() %>% summarise(n=weighted.mean(age, weight)) #48.2

All_OBE %>%  ungroup() %>%  summarise(n=weighted.median(age, weight))#47.5

Treat_exp %>% select(patient, weight, age) %>%
  ggplot(aes(x=age ))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black", fill="cadetblue")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")


Treat_exp %>% ungroup() %>% summarise(n=weighted.mean(age, weight)) #41.3

Treat_exp %>%  ungroup() %>%  summarise(n=weighted.median(age, weight))#39.5


All_OBE <- All_OBE %>% mutate(Age_group = ifelse(age>=18&age<39, "18_39",
                                      ifelse(age>=40&age<59, "40_59", "60+")))

All_OBE %>% group_by(Age_group) %>% summarise(n=sum(weight))

Age_group         n
<chr>         <dbl>
1 18_39     10433769.
2 40_59     11442767.
3 60+        9956539.


Treat_exp <- Treat_exp %>% mutate(Age_group = ifelse(age>=18&age<39, "18_39",
                                                 ifelse(age>=40&age<59, "40_59", "60+")))

Treat_exp %>% group_by(Age_group) %>% summarise(n=sum(weight))

Age_group        n
<chr>        <dbl>
1 18_39     1355481.
2 40_59     1153193.
3 60+        443862.

# ----- 
# Create NEW Projection Weights ----------------

DANU_Demographics <- fread("DANU Demographics.txt")

DANU_Demographics <- DANU_Demographics %>% mutate(weight_2 = ifelse(diagnosis=="Obesity", weight* 1.2420199 , weight))

DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% group_by(diagnosis) %>% summarise(pop=sum(weight_2))

diagnosis                 pop
<chr>                   <dbl>
1 Diabetes + Obesity  40282960.
2 Obesity            132236677.

OBE_pats <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight, diagnosis)

DANU_Events <- fread("DANU Events.txt")

OBE_pats <- OBE_pats %>% left_join(DANU_Events, by=c("patid"="patid", "weight"="weight"))

OBE_pats <- OBE_pats %>% filter(grepl("BMI", code))       

OBE_pats$code <- as.character(OBE_pats$code)
OBE_pats$code <- parse_number(OBE_pats$code)

OBE_pats <- OBE_pats %>% group_by(patid) %>% filter(code==max(code))

OBE_pats <- OBE_pats %>% group_by(patid) %>% select(-c(claimed)) %>% distinct()

OBE_pats <- OBE_pats %>% ungroup() %>% mutate(BMI_group = ifelse(code<25, "<25",
                                                                 ifelse(code>25&code<=27, "25_to_27",
                                                                        ifelse(code>27&code<=30, "27_to_30",
                                                                           ifelse(code>30&code<=40, "30_to_40", ">40")))))



OBE_pats %>% group_by(diagnosis, BMI_group) %>% summarise(n=sum(weight))

# diagnosis          BMI_group         n
# <chr>              <chr>         <dbl>
# 1 Diabetes + Obesity <25         741684.
# 2 Diabetes + Obesity >40        6034986.
# 3 Diabetes + Obesity 25_to_27   2643361.
# 4 Diabetes + Obesity 27_to_30   4787175.
# 5 Diabetes + Obesity 30_to_40  12555377.
# 6 Obesity            <25        2291855.
# 7 Obesity            >40       10988821.
# 8 Obesity            25_to_27  14355613.
# 9 Obesity            27_to_30  19394054.
# 10 Obesity            30_to_40  30487501.


OBE_pats %>% filter(diagnosis=="Obesity") %>% summarise(n=sum(weight))

OBE_PATS_BMI_Weight <- OBE_pats %>% mutate(weight_BMI = ifelse(BMI_group<="<25", NA,
                                        ifelse(BMI_group=="25_to_27"|BMI_group=="27_to_30",weight*2.14675,weight*2.41256))) %>%
  drop_na()

sum(OBE_PATS_BMI_Weight$weight_BMI)

DANU_Demographics <- DANU_Demographics %>% left_join(OBE_PATS_BMI_Weight %>% select(patid, weight_BMI) , by=c("patid"="patid"))

DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis, weight, weight_2, weight_BMI)

sum(DANU_Demographics$weight_BMI[DANU_Demographics$diagnosis=="Obesity"], na.rm=T)
sum(DANU_Demographics$weight_2[DANU_Demographics$diagnosis=="Obesity"], na.rm=T)
sum(DANU_Demographics$weight_BMI[DANU_Demographics$diagnosis=="Diabetes + Obesity"|DANU_Demographics$diagnosis=="Obesity"], na.rm=T)
sum(DANU_Demographics$weight_2[DANU_Demographics$diagnosis=="Diabetes + Obesity"|DANU_Demographics$diagnosis=="Obesity"], na.rm=T)


fwrite(DANU_Demographics, "DANU_Demographics_Weights_V3_BMI.txt")


# -----
# Dosages GLP1 Oral vs Inj -------
# Saxenda dosages over time ----------------------------------------------------------------------
OBE_Medications <- fread("DANU Medications.txt")

OBE_Medications <- OBE_Medications %>% filter(brand_name =="Saxenda") %>% select(drug_id, med_ingredient, med_strength)

OBE_US_Doses <- fread("OBE Doses.txt")

OBE_US_Doses <- OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% left_join(OBE_Medications) %>% mutate(from_dt = as.Date(from_dt))

OBE_US_Doses$doses <- parse_number(OBE_US_Doses$med_strength)

OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  select(generic_name, dayssup, pat_id, from_dt, doses) %>% group_by(pat_id) %>% 
  summarise(n=n()) %>% arrange(-n)

unique(OBE_US_Doses$doses) # 6
weighted.mean(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 6
weighted.median(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 6

OBE_US_Doses_Saxenda <- OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  select(generic_name, dayssup, pat_id, weight, from_dt, doses) 

OBE_US_Doses_Saxenda <- OBE_US_Doses_Saxenda %>% filter(!is.na(doses))

OBE_US_Doses_Saxenda_summary <- OBE_US_Doses_Saxenda %>% group_by(pat_id) %>% arrange(pat_id, from_dt) %>% 
  mutate(index = from_dt-lag(from_dt)) %>% ungroup() %>% mutate(index = as.numeric(index)) %>%
  mutate(index = ifelse(is.na(index), 0, index)) %>% group_by(pat_id) %>%
  mutate(time_progression = cumsum(index)) %>% select(-c(generic_name, dayssup, index))

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% select(pat_id, doses, time_progression) %>%
  ggplot(aes(x=time_progression, y=doses, fill=doses, colour=-doses))+
  geom_jitter(height =0.1, width = 0.1, show.legend = F, alpha=0.1, size=1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))+
  xlab("\nTime since therapy initiation (days)")+
  ylab("Dosage Prescribed\n")+
  scale_y_continuous(breaks = c(0, 6, 12))

length(unique(OBE_US_Doses_Saxenda_summary$pat_id)) #1904

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% slice_head() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1     6 247958.

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=30) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1     6 247958

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=60) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1     6 247958.

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=90) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1     6 247958.

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=120) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1     6 247958.

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=150) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1     6 247958.

OBE_US_Doses_Saxenda_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=180) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
# 
# doses       n
# <dbl>   <dbl>
#   1     6 247958.



# Wegovy dosages over time ----------------------------------------------------------------------
OBE_Medications <- fread("DANU Medications.txt")

OBE_Medications <- OBE_Medications %>% filter(brand_name =="Wegovy") %>% select(drug_id, med_ingredient, med_strength)

OBE_US_Doses <- fread("OBE Doses.txt")

OBE_US_Doses <- OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% left_join(OBE_Medications) %>% mutate(from_dt = as.Date(from_dt))

OBE_US_Doses$doses <- parse_number(OBE_US_Doses$med_strength)

OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  select(generic_name, dayssup, pat_id, from_dt, doses) %>% group_by(pat_id) %>% 
  summarise(n=n()) %>% arrange(-n)

unique(OBE_US_Doses$doses) #  0.50 0.25 2.40 1.70
weighted.mean(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 0.4418643
weighted.median(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 0.25

OBE_US_Doses_Wegovy <- OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  select(generic_name, dayssup, pat_id, weight, from_dt, doses) 

OBE_US_Doses_Wegovy <- OBE_US_Doses_Wegovy %>% filter(!is.na(doses))

OBE_US_Doses_Wegovy_summary <- OBE_US_Doses_Wegovy %>% group_by(pat_id) %>% arrange(pat_id, from_dt) %>% 
  mutate(index = from_dt-lag(from_dt)) %>% ungroup() %>% mutate(index = as.numeric(index)) %>%
  mutate(index = ifelse(is.na(index), 0, index)) %>% group_by(pat_id) %>%
  mutate(time_progression = cumsum(index)) %>% select(-c(generic_name, dayssup, index))

OBE_US_Doses_Wegovy_summary %>% ungroup() %>% select(pat_id, doses, time_progression) %>%
  ggplot(aes(x=time_progression, y=doses, fill=doses, colour=-doses))+
  geom_jitter(height =0.1, width = 0.1, show.legend = F, alpha=0.6, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))+
  xlab("\nTime since therapy initiation (days)")+
  ylab("Dosage Prescribed\n")+
  scale_y_continuous(breaks = c(0.25, 0.5, 1.7, 2.4))

length(unique(OBE_US_Doses_Wegovy_summary$pat_id)) #27




# Ozempic dosages over time ----------------------------------------------------------------------
OBE_Medications <- fread("DANU Medications.txt")

OBE_Medications <- OBE_Medications %>% filter(brand_name =="Ozempic") %>% select(drug_id, med_ingredient, med_strength)

OBE_US_Doses <- fread("OBE Doses.txt")

OBE_US_Doses <- OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% left_join(OBE_Medications) %>% mutate(from_dt = as.Date(from_dt))

OBE_US_Doses$doses <- parse_number(OBE_US_Doses$med_strength)

OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  select(generic_name, dayssup, pat_id, from_dt, doses) %>% group_by(pat_id) %>% 
  summarise(n=n()) %>% arrange(-n)

unique(OBE_US_Doses$doses) #  2.00   NA 1.00 0.25
weighted.mean(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 1.968962
weighted.median(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 1.5

OBE_US_Doses_Ozempic <- OBE_US_Doses %>% filter(drug_group == "GLP1 Injectable") %>% 
  select(generic_name, dayssup, pat_id, weight, from_dt, doses) 

OBE_US_Doses_Ozempic <- OBE_US_Doses_Ozempic %>% filter(!is.na(doses))

OBE_US_Doses_Ozempic_summary <- OBE_US_Doses_Ozempic %>% group_by(pat_id) %>% arrange(pat_id, from_dt) %>% 
  mutate(index = from_dt-lag(from_dt)) %>% ungroup() %>% mutate(index = as.numeric(index)) %>%
  mutate(index = ifelse(is.na(index), 0, index)) %>% group_by(pat_id) %>%
  mutate(time_progression = cumsum(index)) %>% select(-c(generic_name, dayssup, index))

OBE_US_Doses_Ozempic_summary %>% ungroup() %>% select(pat_id, doses, time_progression) %>%
  ggplot(aes(x=time_progression, y=doses, fill=doses, colour=-doses))+
  geom_jitter(height =0.2, width = 0.1, show.legend = F, alpha=0.5, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))+
  xlab("\nTime since therapy initiation (days)")+
  ylab("Dosage Prescribed\n")+
  scale_y_continuous(breaks = c(0.25, 1, 2))

length(unique(OBE_US_Doses_Ozempic_summary$pat_id)) #606


OBE_US_Doses_Ozempic_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% slice_head() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
# doses      n
# <dbl>  <dbl>
#   1  0.25   241.
# 2  1     1491.
# 3  2    78128.

OBE_US_Doses_Ozempic_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=30) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
# doses       n
# <dbl>   <dbl>
#   1  0.25    97.8
# 2  1     2260. 
# 3  2    77504. 

OBE_US_Doses_Ozempic_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=60) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1  0.25    97.8
# 2  1     2467. 
# 3  2    77297.

OBE_US_Doses_Ozempic_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=90) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

# doses       n
# <dbl>   <dbl>
#   1  0.25    97.8
# 2  1     2777. 
# 3  2    76986. 

OBE_US_Doses_Ozempic_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=120) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
# 
# doses       n
# <dbl>   <dbl>
#   1  0.25    97.8
# 2  1     3093. 
# 3  2    76671. 

OBE_US_Doses_Ozempic_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=150) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
# doses       n
# <dbl>   <dbl>
#   1  0.25    97.8
# 2  1     3308. 
# 3  2    76455. 

OBE_US_Doses_Ozempic_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=180) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
# doses       n
# <dbl>   <dbl>
#   1  0.25    97.8
# 2  1     3886. 
# 3  2    75878.

# Rybelsus dosages over time ----------------------------------------------------------------------
OBE_Medications <- fread("DANU Medications.txt")

OBE_Medications <- OBE_Medications %>% filter(brand_name =="Rybelsus") %>% select(drug_id, med_ingredient, med_strength)

OBE_US_Doses <- fread("OBE Doses.txt")

OBE_US_Doses <- OBE_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% left_join(OBE_Medications) %>% mutate(from_dt = as.Date(from_dt))

OBE_US_Doses$doses <- parse_number(OBE_US_Doses$med_strength)

OBE_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% 
  select(generic_name, dayssup, pat_id, from_dt, doses) %>% group_by(pat_id) %>% 
  summarise(n=n()) %>% arrange(-n)

unique(OBE_US_Doses$doses) #  7  3 14
weighted.mean(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 7.865027
weighted.median(OBE_US_Doses$doses, OBE_US_Doses$weight, na.rm=T) # 5

OBE_US_Doses_Rybelsus <- OBE_US_Doses %>% filter(drug_group == "GLP1 Oral") %>% 
  select(generic_name, dayssup, pat_id, weight, from_dt, doses) 

OBE_US_Doses_Rybelsus <- OBE_US_Doses_Rybelsus %>% filter(!is.na(doses))

OBE_US_Doses_Rybelsus_summary <- OBE_US_Doses_Rybelsus %>% group_by(pat_id) %>% arrange(pat_id, from_dt) %>% 
  mutate(index = from_dt-lag(from_dt)) %>% ungroup() %>% mutate(index = as.numeric(index)) %>%
  mutate(index = ifelse(is.na(index), 0, index)) %>% group_by(pat_id) %>%
  mutate(time_progression = cumsum(index)) %>% select(-c(generic_name, dayssup, index))

OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% select(pat_id, doses, time_progression) %>%
  ggplot(aes(x=time_progression, y=doses, fill=doses, colour=-doses))+
  geom_jitter(height =0.3, width = 0.1, show.legend = F, alpha=0.3, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))+
  xlab("\nTime since therapy initiation (days)")+
  ylab("Dosage Prescribed\n")+
  scale_y_continuous(breaks = c(3, 7, 14))

length(unique(OBE_US_Doses_Rybelsus_summary$pat_id)) #266


OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% slice_head() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
doses      n
<dbl>  <dbl>
  1     3 23128.
2     7 10710.
3    14  2368.

OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=30) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
doses      n
<dbl>  <dbl>
  1     3 19072.
2     7 14626.
3    14  2508.

OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=60) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

doses      n
<dbl>  <dbl>
  1     3 15564.
2     7 15885.
3    14  4758.

OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=90) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

doses      n
<dbl>  <dbl>
  1     3 14765.
2     7 15026.
3    14  6415.

OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=120) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
# 
doses      n
<dbl>  <dbl>
  1     3 14456.
2     7 14223.
3    14  7527.

OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=150) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
doses      n
<dbl>  <dbl>
  1     3 14377.
2     7 14201.
3    14  7629.

OBE_US_Doses_Rybelsus_summary %>% ungroup() %>% 
  left_join(OBE_US_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=180) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))
doses      n
<dbl>  <dbl>
  1     3 14279.
2     7 14399.
3    14  7528.


# ----
# How were the patients identified? Rx, Dx or BMI ? --------

#All pats, all weights
DANU_Demographics_v2weights <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% select(patid, diagnosis, weight, weight_2, weight_3)


# Ever treated 
OBE_Drug_Histories_v2 <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_v2 <- OBE_Drug_Histories_v2 %>% select(patient, weight)
OBE_Drug_Histories_v2 <- OBE_Drug_Histories_v2 %>% mutate(Ever_treated="Ever_treated")
OBE_Drug_Histories_v2 <- OBE_Drug_Histories_v2 %>% select(patient, Ever_treated)

DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% left_join(OBE_Drug_Histories_v2, by=c("patid"="patient"))

# Diagnosis
DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, source)
  
DANU_US_Events <- fread("DANU Events.txt")
DANU_US_Events <- DANU_US_Events %>% select(patid, code) %>% distinct()

DANU_US_Events <- DANU_US_Events %>% left_join(DANU_Diagnosis_Codes) %>% filter(!is.na(source))
DANU_US_Events <- DANU_US_Events %>% select(patid, source) %>% distinct()

Dx_pats <- DANU_US_Events %>% filter(source!= "Measurement") %>% select(patid) %>% distinct()

Dx_pats <- Dx_pats %>% mutate(Diagnosis = "Diagnosis")

DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% left_join(Dx_pats)


# BMI
DANU_US_Events <- fread("DANU Events.txt")
BMI_pats <- DANU_US_Events %>% filter(grepl("BMI", code))  %>% select(patid) %>% distinct()
BMI_pats <- BMI_pats %>% mutate(BMI_measure = "BMI")

DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% left_join(BMI_pats)

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, obesity_condition)

DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% left_join(DANU_Demographics)

DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% select(1,2,10,3,4,5,6,7,9,8)
  
DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% mutate(Ever_treated = 
                                                                        ifelse(is.na(Ever_treated)&obesity_condition=="Treatment For Obesity", "Ever_treated",Ever_treated))
  

DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% mutate(Ever_treated = 
                                                                        ifelse(is.na(Ever_treated)&obesity_condition=="Nonspecific Treatment For Obesity", "Ever_treated",Ever_treated))




DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% mutate(Ever_treated = 
                                                                        ifelse(is.na(Ever_treated)&obesity_condition=="Treatment For Diabetes Or Obesity", "Ever_treated",Ever_treated))



DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% mutate(Priority_Ident = ifelse(!is.na(Ever_treated), "Drugs",
                                                                                             ifelse(!is.na(Diagnosis), "Diagnosis",
                                                                                                    ifelse(!is.na(BMI_measure), "BMI", NA))))


DANU_Demographics_v2weights %>% filter(grepl("Obesity", diagnosis)) %>% group_by(Priority_Ident) %>% summarise(n=sum(weight_2))



DANU_Demographics_v2weights %>% filter( diagnosis=="Diabetes + Obesity") %>% summarise(n=sum(weight_2))


data.frame(DANU_Demographics_v2weights %>% group_by(obesity_condition, Priority_Ident) %>% 
             summarise(n=sum(weight_2)))

fwrite(DANU_Demographics_v2weights, "DANU_Demographics_v2Ident_source.txt" )



# ------
# BMI NEW Weights -----
DANU_Demographics_v2weights <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% select(patid, diagnosis, weight_3) %>% filter(!is.na(weight_3))
OBE_pats <- DANU_Demographics_v2weights

DANU_Events <- fread("DANU Events.txt")

OBE_pats <- OBE_pats %>% left_join(DANU_Events, by=c("patid"="patid"))

OBE_pats <- OBE_pats %>% filter(grepl("BMI", code))       

OBE_pats$code <- as.character(OBE_pats$code)
OBE_pats$code <- parse_number(OBE_pats$code)

OBE_pats %>% group_by(diagnosis) %>% summarise(n=weighted.mean(code, weight_3))

diagnosis              n
<chr>              <dbl>
  1 Diabetes + Obesity  33.6
2 Obesity             30.7

# Age and gender --------
DANU_Demographics_v2weights <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% select(patid, diagnosis, gender, age, weight_2) %>% filter(!is.na(weight_2))

DANU_Demographics_v2weights <- DANU_Demographics_v2weights %>% filter(diagnosis == "Obesity")

DANU_Demographics_v2weights %>% group_by(gender) %>% summarise(n=sum(weight_2))

gender        n
<chr>     <dbl>
  1 F      63611715
2 M      61549146


DANU_Demographics_v2weights %>% group_by(gender) %>% summarise(n=weighted.mean(age, weight_2))
DANU_Demographics_v2weights %>% group_by(gender) %>% summarise(n=weighted.median(age, weight_2))

DANU_Demographics_v2weights %>% select(patid, weight_2, age) %>% mutate(age_group = ifelse(age>=18 & age<30,"18_to_29", 
                                                                                           ifelse(age>=30 & age<40, "30_to_39", 
                                                                                                  ifelse(age>=40 & age<50, "40_to_49",
                                                                                                         ifelse(age>=50 & age<60, "50_to_59", 
                                                                                                                ifelse(age>=60 & age<70,"60_to_69",
                                                                                                                       ifelse(age>=70 & age<80, "70_to_79", "+80"))))))) %>%
  group_by(age_group) %>% summarise(n=sum(weight_2))

age_group         n
<chr>         <dbl>
  1 +80        6181871.
2 18_to_29  20137809.
3 30_to_39  23125783.
4 40_to_49  21664771.
5 50_to_59  22003137.
6 60_to_69  19830421.
7 70_to_79  12217069.



OBE_Demographics %>% select(patient, weight, gender, age) %>%
  ggplot(aes(x=age ,fill=gender))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")+
  facet_wrap(~gender, ncol = 1)+
  scale_fill_viridis_d()



# Age and BMI distribution: All OBESITY vs Treat_Experienced --------------
DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis, weight_2) %>% filter(!is.na(weight_2))
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis == "Obesity")

temp <- fread("DANU Demographics.txt")
temp <- temp %>% select(patid, gender, age)

DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% left_join(temp) 

DANU_Demographics %>% select(patid, weight_2, age) %>%
  ggplot(aes(x=age ))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black", fill="midnightblue")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")

sum(DANU_Demographics$weight_2) # 132236677
weighted.mean(DANU_Demographics$age, DANU_Demographics$weight_2) # 49.09858
weighted.median(DANU_Demographics$age, DANU_Demographics$weight_2) #  47.5


DANU_Demographics %>% mutate(Age_group = ifelse(age>=18&age<39, "18_39",
                                                ifelse(age>=40&age<59, "40_59", "60+"))) %>% group_by(Age_group) %>% summarise(n=sum(weight_2))

names(DANU_Demographics)[1] <- "patient"

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
DANU_Demographics <- Treatment_exp_Vector %>% select(patient) %>% left_join(DANU_Demographics)


sum(DANU_Demographics$weight_2) # 132236677
weighted.mean(DANU_Demographics$age, DANU_Demographics$weight_2) # 40.37536
weighted.median(DANU_Demographics$age, DANU_Demographics$weight_2) #  38.5


DANU_Demographics %>% mutate(Age_group = ifelse(age>=18&age<39, "18_39",
                                                ifelse(age>=40&age<59, "40_59", "60+"))) %>% group_by(Age_group) %>% summarise(n=sum(weight_2))








DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, diagnosis, weight_2) %>% filter(!is.na(weight_2))
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis == "Obesity")
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-diagnosis)
All_OBE <- DANU_Demographics


Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient) %>% left_join(DANU_Demographics)
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(-diagnosis)
Treat_exp <- Treatment_exp_Vector

# BMI 
DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"

All_OBE <- All_OBE %>% left_join(DANU_Events)
Treat_exp <- Treat_exp %>% left_join(DANU_Events)

All_OBE <- All_OBE %>% filter(grepl("BMI", code))       

All_OBE$code <- as.character(All_OBE$code)
All_OBE$code <- parse_number(All_OBE$code)

All_OBE <- All_OBE %>% group_by(patient) %>% slice(n())

All_OBE %>% ungroup() %>% summarise(n=weighted.mean(code, weight_2)) #30.4

All_OBE %>%  ungroup() %>%  summarise(n=weighted.median(code, weight_2))#28.8


All_OBE %>% 
  ggplot(aes(code))+
  geom_histogram(alpha=0.9, show.legend = F, colour="black", fill="midnightblue")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")



Treat_exp <- Treat_exp %>% filter(grepl("BMI", code))       

Treat_exp$code <- as.character(Treat_exp$code)
Treat_exp$code <- parse_number(Treat_exp$code)

Treat_exp <- Treat_exp %>% group_by(patient) %>% slice(n())

Treat_exp %>% ungroup() %>% summarise(n=weighted.mean(code, weight_3)) #32.5

Treat_exp %>%  ungroup() %>%  summarise(n=weighted.median(code, weight_3))#30.2

Treat_exp %>% 
  ggplot(aes(code))+
  geom_histogram(alpha=0.9, show.legend = F, colour="black", fill="deepskyblue4")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")


# Only ages for those with BMI > 30 

All_OBE <- All_OBE %>% filter(code >30)
Treat_exp <- Treat_exp %>% filter(code >30)

All_OBE <- All_OBE %>% select(patid)
Treat_exp <- Treat_exp %>% select(patient)

All_OBE <- All_OBE %>% left_join(DANU_Demographics_v2weights) %>% select(patid, weight_3, age)
Treat_exp <- Treat_exp %>% left_join(DANU_Demographics_v2weights, by=c("patient"="patid")) %>% select(patient, weight_3, age)



All_OBE %>% select(patid, weight_3, age) %>%
  ggplot(aes(x=age ))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black", fill="brown3")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")


All_OBE %>% ungroup() %>% summarise(n=weighted.mean(age, weight_3)) #48.2

All_OBE %>%  ungroup() %>%  summarise(n=weighted.median(age, weight_3))#47.5

Treat_exp %>% select(patient, weight_3, age) %>%
  ggplot(aes(x=age ))+
  geom_histogram(alpha=0.7, bins=72, show.legend = F, colour="black", fill="cadetblue")+
  theme(panel.background = element_blank())+
  ylab("Number of Patients\n")+
  xlab("\n Age (Years)")


Treat_exp %>% ungroup() %>% summarise(n=weighted.mean(age, weight_3)) #41.3

Treat_exp %>%  ungroup() %>%  summarise(n=weighted.median(age, weight_3))#39.5


All_OBE <- All_OBE %>% mutate(Age_group = ifelse(age>=18&age<39, "18_39",
                                                 ifelse(age>=40&age<59, "40_59", "60+")))

All_OBE %>% group_by(Age_group) %>% summarise(n=sum(weight_3))

Age_group         n
<chr>         <dbl>
  1 18_39     18707122.
2 40_59     20516195.
3 60+       17851477.

Treat_exp <- Treat_exp %>% mutate(Age_group = ifelse(age>=18&age<39, "18_39",
                                                     ifelse(age>=40&age<59, "40_59", "60+")))

Treat_exp %>% group_by(Age_group) %>% summarise(n=sum(weight_3))

Age_group        n
<chr>        <dbl>
  1 18_39     2430296.
2 40_59     2067606.
3 60+        795817.
# ------
# Evolution of scripts over time --------------------------------------------

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
sum(DANU_Demographics$weight_2)

DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% filter(!is.na(weight_2))

OBE_Doses_BIG <- read.table("OBE Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
OBE_Doses_BIG <- OBE_Doses_BIG %>% select(drug_id, generic_name, drug_group, pat_id, weight, from_dt)

OBE_Doses_BIG <- OBE_Doses_BIG %>% left_join(DANU_Demographics, by=c("pat_id"="patid"))

setDT(OBE_Doses_BIG)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]

OBE_Doses_BIG <- OBE_Doses_BIG %>% filter(from_dt >= "2016-01-01") %>% filter(from_dt <= "2021-04-31")

length(unique(OBE_Doses_BIG$pat_id)) #62350


OBE_Doses_BIG %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight_2))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts \n")


OBE_Doses_BIG %>% select(pat_id, weight_2, Month_Yr) %>% distinct() %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight_2))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Patients \n")


data.frame(OBE_Doses_BIG %>% select(pat_id,weight_2, Month_Yr) %>% group_by(Month_Yr) %>% mutate(script_count = sum(as.numeric(weight_2))) %>% ungroup() %>%
             select(pat_id, weight_2, Month_Yr, script_count) %>% distinct() %>% group_by(Month_Yr) %>% mutate(pat_count = sum(as.numeric(weight_2))) %>% 
             select(Month_Yr,script_count, pat_count) %>% distinct() %>% arrange(Month_Yr) %>% mutate(scripts_pat = script_count/pat_count))%>%
  ggplot(aes(x=Month_Yr, y=scripts_pat))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts per Patient \n")


Month_Yr script_count pat_count scripts_pat
1   2016-01      4917425   3029289    1.623294
2   2016-02      4735761   3017281    1.569546
3   2016-03      5045048   3128781    1.612465
4   2016-04      5022264   3114693    1.612443
5   2016-05      4867581   3033926    1.604384
6   2016-06      4805229   2944879    1.631724
7   2016-07      4745454   2928804    1.620270
8   2016-08      5065369   3059646    1.655541
9   2016-09      5065746   3114662    1.626419
10  2016-10      5063202   3137376    1.613833
11  2016-11      5066044   3126214    1.620505
12  2016-12      5061445   3102147    1.631595
13  2017-01      5238979   3165095    1.655236
14  2017-02      4902215   3064179    1.599846
15  2017-03      5369160   3282941    1.635472
16  2017-04      5161773   3216504    1.604778
17  2017-05      5279074   3225757    1.636538
18  2017-06      5269037   3188924    1.652293
19  2017-07      5027587   3133445    1.604492
20  2017-08      5531249   3267868    1.692617
21  2017-09      5314941   3265337    1.627685
22  2017-10      5395850   3301971    1.634130
23  2017-11      5405257   3264824    1.655604
24  2017-12      5185155   3199876    1.620423
25  2018-01      5426683   3199827    1.695930
26  2018-02      5008234   3068433    1.632180
27  2018-03      5369069   3280807    1.636508
28  2018-04      5327031   3273875    1.627133
29  2018-05      5362078   3245302    1.652259
30  2018-06      5213229   3171824    1.643606
31  2018-07      5142333   3145807    1.634663
32  2018-08      5500064   3247547    1.693605
33  2018-09      5226886   3233923    1.616268
34  2018-10      5472514   3299217    1.658731
35  2018-11      5422468   3269501    1.658500
36  2018-12      5229174   3223708    1.622099
37  2019-01      5534975   3243254    1.706612
38  2019-02      5139639   3108250    1.653547
39  2019-03      5381593   3277496    1.641983
40  2019-04      5522653   3301056    1.672996
41  2019-05      5527269   3279400    1.685452
42  2019-06      5219433   3212134    1.624912
43  2019-07      5441152   3206536    1.696894
44  2019-08      5666390   3310350    1.711720
45  2019-09      5490376   3302388    1.662547
46  2019-10      5699545   3368970    1.691777
47  2019-11      5475155   3312161    1.653046
48  2019-12      5422649   3278194    1.654157
49  2020-01      5888830   3307664    1.780359
50  2020-02      5266519   3183504    1.654315
51  2020-03      5202866   3149828    1.651794
52  2020-04      5282426   3170409    1.666166
53  2020-05      5287526   3155801    1.675494
54  2020-06      5481679   3234893    1.694547
55  2020-07      5673179   3282816    1.728144
56  2020-08      5606287   3302549    1.697563
57  2020-09      5695400   3336690    1.706901
58  2020-10      5723952   3362414    1.702334
59  2020-11      5522801   3333900    1.656559
60  2020-12      5767893   3403420    1.694735
61  2021-01      5734628   3352330    1.710640
62  2021-02      5535133   3343666    1.655408
63  2021-03      6053370   3533783    1.713000
64  2021-04      6162688   3594903    1.714285

# ----
# Stocks Over time ----------

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% filter(!is.na(weight_2))


OBE_Drug_Histories     <- fread("OBE_Drug_Histories_v2.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(DANU_Demographics, by=c("patient"="patid")) %>% select(1,63,3:62)

length(unique(OBE_Drug_Histories$patient)) # 60282
sum(as.numeric(OBE_Drug_Histories$weight_2)) #11370836

DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_class)

OBE_Drug_Histories_time_series <- DANU_Ingredients %>% select(drug_class) %>% distinct()
# Repeat for each month

OBE_Drug_Histories_month60 <- OBE_Drug_Histories %>% select(patient, weight_2, month60)
OBE_Drug_Histories_month60 <- separate_rows(OBE_Drug_Histories_month60, month60, sep = ",", convert=T )
names(OBE_Drug_Histories_month60)[3] <- "molecule"
OBE_Drug_Histories_month60 <- OBE_Drug_Histories_month60 %>% left_join(DANU_Ingredients) %>% filter(!is.na(drug_class))
OBE_Drug_Histories_month60 <- OBE_Drug_Histories_month60 %>% select(patient, weight_2, drug_class)
OBE_Drug_Histories_month60 <- OBE_Drug_Histories_month60 %>% distinct()

OBE_Drug_Histories_time_series_month60 <- data.frame(OBE_Drug_Histories_month60 %>% group_by(drug_class) %>% summarise(sum_weights_month60 = sum(as.numeric(weight_2))))

OBE_Drug_Histories_time_series <- OBE_Drug_Histories_time_series %>% full_join(OBE_Drug_Histories_time_series_month60)

rm(OBE_Drug_Histories_month60, OBE_Drug_Histories_time_series_month60)




OBE_Drug_Histories_time_series <- OBE_Drug_Histories_time_series %>% filter(!is.na(sum_weights_month60))

fwrite(OBE_Drug_Histories_time_series, "OBE_Drug_Histories_time_series_class_NEW_weights.txt", sep = "\t")

# ------
# BMI distribution with NEW weight_3 -------------------------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code))
DANU_Events <- DANU_Events %>% group_by(patid) %>% slice(n())
DANU_Events <- DANU_Events %>% select(patid, code)

DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
# 
# DANU_Demographics <- DANU_Demographics %>% mutate(weight_3 = ifelse(!is.na(weight_3)&diagnosis=="Diabetes + Obesity", weight_3*1.043132,
#                                                                     ifelse(!is.na(weight_3)&diagnosis=="Obesity", weight_3*0.984592, NA)))
# 
# fwrite(DANU_Demographics, "DANU_Demographics_v2weights.txt")

DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")

DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

diagnosis                   n
<chr>                   <dbl>
1 Diabetes + Obesity  47355077.
2 Obesity            125160866.

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% 
  summarise(n=sum(weight_3, na.rm=T)) # 172515943


DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% 
  filter(code>25&code<=27) %>% summarise(n=sum(weight_3, na.rm=T)) # 29719393

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% 
  filter(code>27&code<=30) %>% summarise(n=sum(weight_3, na.rm=T)) # 42381937

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% 
  filter(code>30&code<=40) %>% summarise(n=sum(weight_3, na.rm=T)) # 77302008

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% 
  filter(code>40) %>% summarise(n=sum(weight_3, na.rm=T)) # 23112605



DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% filter(diagnosis=="Obesity") %>%
  summarise(n=sum(weight_3, na.rm=T)) # 125160866

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% filter(diagnosis=="Obesity") %>%
  filter(code>25&code<=27) %>% summarise(n=sum(weight_3, na.rm=T)) # 24868074

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% filter(diagnosis=="Obesity") %>%
  filter(code>27&code<=30) %>% summarise(n=sum(weight_3, na.rm=T)) # 33596110

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% filter(diagnosis=="Obesity") %>%
  filter(code>30&code<=40) %>% summarise(n=sum(weight_3, na.rm=T)) # 54662261

DANU_Events %>% left_join(DANU_Demographics) %>% ungroup() %>% filter(!is.na(diagnosis)) %>% filter(diagnosis=="Obesity") %>%
  filter(code>40) %>% summarise(n=sum(weight_3, na.rm=T)) # 13078166
# ------
# How long have patients been lapsed? -------
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
New_weight <- DANU_Demographics %>% select(patid, weight_2)

OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(New_weight, by=c("patient"="patid")) %>% select(patient, weight_2, month1:month60)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(month60 == "-") 

OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Treat = ifelse(Treat=="-", 0, 1))

OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)

OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>%  filter(grp == max(grp))


OBE_Drug_Histories %>% group_by(patient, weight_2) %>% count() %>% ungroup() %>%
  mutate(Total_lapsed_bucket = ifelse(n == 1, "1", 
                                      ifelse(n >1 & n < 6, "2 to 6",
                                             ifelse(n>=6 & n <12, "6 to 12",
                                                    ifelse(n>=12&n<24, "12 to 24",
                                                           ifelse(n>=24&n<36, "24 to 36",
                                                                  ifelse(n>=36&n<48, "36 to 48",
                                                                         ifelse(n>=48&n<60, "48 to 60", "60")))))))) %>%
  group_by(Total_lapsed_bucket) %>%
  summarise(pats = sum(as.numeric(weight_2)))

Total_lapsed_bucket     pats
<chr>                  <dbl>
  1 1                    281533.
2 12 to 24            1525081.
3 2 to 6               916864.
4 24 to 36            1288836.
5 36 to 48            1220538.
6 48 to 60            1087398.
7 6 to 12              910276.


Lapsed_summary <- OBE_Drug_Histories %>% group_by(patient, weight_2) %>% count() %>% ungroup()

library(spatstat)
weighted.mean(Lapsed_summary$n, as.numeric(Lapsed_summary$weight_2))  #25.45396
weighted.median(Lapsed_summary$n, as.numeric(Lapsed_summary$weight_2)) #22.5

# -----
# Nr of Lines -------------
drgDIA2 <- read("OBE_Drug_Histories_v2.txt")

data <- data.frame(drgDIA2, stringsAsFactors = F)

nrLines <- data[,c(1:3)] 
nrLines$month1 <- (data$month1 != "-")*1

for(i in 2:60){
  cat(i)
  nrLines[,i+3] <- apply(data[,(4:(i+3))], 1, function(x) length(unique(x[x!="-"])))
  names(nrLines)[i+3] <- paste0("month",i)
}

fwrite(nrLines,"OBE_nrLines_Histories.txt")
# ------
# nr of Lines per stock m60 -----------------

OBE_Box_Histories <- fread("OBE Box Histories.txt")
OBE_Box_Histories <- OBE_Box_Histories %>% select(1,2,63)
OBE_Box_Histories <- OBE_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% filter(!is.na(weight_2))
names(DANU_Demographics)[1] <- "patient"


OBE_Box_Histories <- OBE_Box_Histories %>% left_join(DANU_Demographics) %>% select(-disease)

nrLines <- fread("OBE_nrLines_Histories.txt")
nrLines <- nrLines %>% select(2, 63)
names(nrLines)[2] <- "Nr_lines"

OBE_Box_Histories <- OBE_Box_Histories %>% left_join(nrLines)

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

OBE_Box_Histories <- Treatment_exp_Vector %>% inner_join(OBE_Box_Histories)

sum(OBE_Box_Histories$weight_2) # 11370836
OBE_Box_Histories %>% group_by(month60) %>% summarise(n=sum(weight_2))

month60        n
<chr>      <dbl>
  1 a       3589047.
2 g          9827.
3 G         75724.
4 H         23577.
5 o         31622.
6 w          1744.
7 x       7639296.

OBE_Box_Histories %>% group_by(month60) %>% summarise(lines=weighted.mean(Nr_lines, weight_2))

weighted.mean(OBE_Box_Histories$Nr_lines, OBE_Box_Histories$weight_2)


# ------
# Concomitant classes on month 60 ------------------
# Box month60
OBE_Box_Histories <- fread("OBE_Box_Histories_v2.txt")
OBE_Box_Histories <- OBE_Box_Histories %>% select(1,62)
OBE_Box_Histories <- OBE_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))
names(OBE_Box_Histories)[2] <- "Box_m60"


#drugs m60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(1,62)
OBE_Drug_Histories <- OBE_Box_Histories %>% left_join(OBE_Drug_Histories)

# New weight
DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% filter(!is.na(weight_2))
names(DANU_Demographics)[1] <- "patient"


OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(DANU_Demographics)

OBE_Drug_Histories %>% group_by(Box_m60) %>% summarise(n=sum(as.numeric(weight_2)))

# Box_m60        n
# <chr>      <dbl>
#   1 a       3589047.
# 2 g          9827.
# 3 G         75724.
# 4 H         23577.
# 5 o         31622.
# 6 w          1744.
# 7 x       7639296.


OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(combo = ifelse(grepl(",",month60), "Combo", "Mono"))

OBE_Drug_Histories %>% group_by(Box_m60, combo) %>% summarise(n=sum(as.numeric(weight_2)))

 
# Box_m60 combo        n
# <chr>   <chr>    <dbl>
#   1 a       Combo  112876.
# 2 a       Mono  3476171.
# 3 g       Combo     741.
# 4 g       Mono     9086.
# 5 G       Combo    9543.
# 6 G       Mono    66180.
# 7 H       Combo    1145.
# 8 H       Mono    22433.
# 9 o       Combo    2508.
# 10 o       Mono    29113.
# 11 w       Mono     1744.
# 12 x       Mono  7639296.


OBE_Drug_Histories <- separate_rows(OBE_Drug_Histories, month60, sep = ",", convert=T)
names(OBE_Drug_Histories)[3] <- "molecule"

# drugs look up
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_class)
DANU_Ingredients$molecule <- as.numeric(DANU_Ingredients$molecule)
DANU_Ingredients$molecule <- as.character(DANU_Ingredients$molecule)

data.frame(OBE_Drug_Histories %>% left_join(DANU_Ingredients) %>% select(patient, weight_2, Box_m60, drug_class) %>% distinct() %>%
             group_by(Box_m60, drug_class) %>% summarise(n=sum(as.numeric(weight_2))))

# Box_m60      drug_class            n
# 1        a       Anorectic 3589047.0448
# 2        a     Weight Loss     134.8834
# 3        g       Anorectic     741.0636
# 4        g       GLP1 Oral    9826.8366
# 5        G       Anorectic    8688.4633
# 6        G     Antiobesity     579.9115
# 7        G GLP1 Injectable   75723.5065
# 8        G       GLP1 Oral     168.3806
# 9        G     Weight Loss     129.1701
# 10       H       Anorectic     829.9425
# 11       H GLP1 Injectable     136.2496
# 12       H         Surgery   23577.4501
# 13       o       Anorectic    2508.1847
# 14       o     Antiobesity   31621.5410
# 15       w     Weight Loss    1743.9450
# 16       x            <NA> 7639295.7734

# ------
# Flows last 12 months ------------


OBE_Flows_Aux._Long <- fread("OBE_Flows_Aux._Long_v2.txt")
length(unique(OBE_Flows_Aux._Long$patient))

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% filter(!is.na(weight_2))
names(DANU_Demographics)[1] <- "patient"

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% left_join(DANU_Demographics)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% filter(p1>=48)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% filter(flow==1)

OBE_Flows_Aux._Long %>% filter(stops==1) %>% summarise(n=sum(weight_2))

OBE_Flows_Aux._Long %>% select(patient, weight_2) %>% distinct() %>% summarise(n=sum(weight_2))

OBE_Flows_Aux._Long %>% filter(starts==0&stops==0&re_starts==0) %>% summarise(n=sum(weight_2))

OBE_Flows_Aux._Long %>% filter(s1=="x"&s2=="G") %>% select(patient, weight_2) %>% distinct() %>% summarise(n=sum(weight_2)) # 1309067

OBE_Flows_Aux._Long %>% filter(starts==1) %>% group_by(s1, s2) %>% summarise(n=sum(weight_2)) %>%
  spread(key=s2,value=n)

OBE_Flows_Aux._Long %>% filter(s1=="x"&s2!="G"&starts==0&flow==1) %>% summarise(n=sum(weight_2)) #3872574

OBE_Flows_Aux._Long %>% filter(p2==60) %>% filter(s2=="x") %>% summarise(n=sum(weight_2))


# -----------
# Correct weight on Drug and Box Hisotories table ------------------
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
New_weight <- DANU_Demographics %>% select(patid, weight_2)

OBE_Drug_Histories     <- fread("OBE_Drug_Histories_v2.txt", integer64 = "character", stringsAsFactors = F)
OBE_Box_Histories     <- fread("OBE_Box_Histories_v2.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(New_weight, by=c("patient" = "patid")) %>% select(patient, weight_2, month1:month60)
OBE_Box_Histories <- OBE_Box_Histories %>% left_join(New_weight, by=c("patient" = "patid")) %>% select(patient, weight_2, month1:month60)

fwrite(OBE_Drug_Histories, "OBE_Drug_Histories_v2.txt")
fwrite(OBE_Box_Histories, "OBE_Box_Histories_v2.txt")

# ------
# Long flows table -----

OBE_Drug_Histories     <- fread("OBE_Drug_Histories_v2.txt", integer64 = "character", stringsAsFactors = F)
OBE_Box_Histories     <- fread("OBE_Box_Histories_v2.txt", integer64 = "character", stringsAsFactors = F)

# Flows table in long format
flOBE <- OBE_Drug_Histories

flOBE <- melt(flOBE, id = c("patient","weight_2"))
names(flOBE)[c(3,4)] <- c("p1","v1")
flOBE <- flOBE[, p1 := str_extract(p1,"[:digit:]+")]
flOBE$p1 <- as.numeric(flOBE$p1)
flOBE <- data.frame(cbind(flOBE[p1 < 60], flOBE[p1 > 1,.(p2 = p1, v2 = v1)]), stringsAsFactors = F)
flOBE <- flOBE[,c(1:3,5,4,6)]

# Any flow flag and stops flag
flOBE <- setDT(flOBE)[, flow := (v1 != v2)*1]
flOBE <- flOBE[, stops := (flow == 1 & v2 == "-")*1]

# Treatment experience
RxExp <- data.frame(OBE_Drug_Histories, stringsAsFactors = F)
RxExp$month1 <- (RxExp$month1 != "-")*1

for(i in 2:60){
  cat(i)
  RxExp[,i+2] <- (((RxExp[,i+2] != "-")*1 + RxExp[,i+2-1]) > 0)*1
}

RxExp <- setDT(RxExp)
RxExp <- melt(RxExp, id = c("patient","weight_2"))
RxExp <- RxExp[, month := str_extract(variable,"[:digit:]+")]
RxExp$month <- as.numeric(RxExp$month)
names(RxExp)[4] <- "OBE_RxExp"

flOBE <- RxExp[,.(patient,month,OBE_RxExp)][flOBE, on = .(patient, month = p1)]
flOBE <- flOBE[,.(patient, weight_2, p1 = month, p2, v1, v2, p1_RxExp = OBE_RxExp, flow, stops)]

# Starts and re-starts flag
flOBE <- flOBE[, starts := (flow == 1 & v1 == "-" & p1_RxExp == 0)*1]
flOBE <- flOBE[, re_starts := (flow == 1 & v1 == "-" & p1_RxExp == 1)*1]
flOBE <- flOBE[, disease := "OBE US"]
flOBE <- flOBE[,c(12,1:11)]

# Bring Therapy classes (Stocks) to the table
OBE_Box_Histories <- data.frame(OBE_Box_Histories, stringsAsFactors = F)

for(i in 1:60){
  cat(i)
  OBE_Box_Histories[,i+2] <- unlist(lapply(OBE_Box_Histories[,i+2],function(x) str_sub(x, 2L, 2L)))
}

setDT(OBE_Box_Histories) 
OBE_Box_Histories <- melt(OBE_Box_Histories, id = c("patient","weight_2"))
names(OBE_Box_Histories)[c(3,4)] <- c("p","s")
OBE_Box_Histories <- OBE_Box_Histories[, p := str_extract(p,"[:digit:]+")]
OBE_Box_Histories$p <- as.numeric(OBE_Box_Histories$p)

flOBE <- OBE_Box_Histories[,.(patient,p,s)][flOBE, on = .(patient, p = p1)]
names(flOBE)[c(2,3)] <- c("p1","s1")
flOBE <- OBE_Box_Histories[,.(patient,p,s)][flOBE, on = .(patient, p = p2)]
names(flOBE)[c(2,3)] <- c("p2","s2")

flOBE <- flOBE[,.(disease, patient, weight_2, p1, p2, v1, v2, s1, s2, p1_RxExp, flow, stops, starts, re_starts)]
names(flOBE)[c(6,7)] <- c("d1","d2")

fwrite(flOBE,"OBE_Flows_Aux._Long_v2.txt")


data.frame(flOBE %>% filter(p1>=48) %>% group_by(s1,s2) %>% filter(flow==1) %>% summarise(n=sum(weight_2)))






# ------
# Find Rybelsus patients to vizualise ------
OBE_Drug_Histories_v2 <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_v2 <- gather(OBE_Drug_Histories_v2, Month, Treat, month1:month60, factor_key=TRUE)

Rybelsus_m60 <- OBE_Drug_Histories_v2 %>% filter(grepl("47", Treat)&Month=="month60") %>% select(patient)
Rybelsus_m54 <- OBE_Drug_Histories_v2 %>% filter(grepl("47", Treat)&Month=="month64") %>% select(patient)


Pats_to_tract <- Rybelsus_m60 %>% filter(row_number()<=25) %>% bind_rows(Rybelsus_m54  %>% filter(row_number()<=25)) %>% distinct()

n <- Pats_to_tract %>% summarise(n=list(Pats_to_tract))
unlist(n)

# -----
# Number of Dx on different dates for the pats identified by Dx only --------------
DANU_Demographics <- fread("DANU_Demographics_v2Ident_source.txt")

Pats_Ident_Dx <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% filter(Priority_Ident=="Diagnosis") %>% select(patid, weight_2)

DANU_Events <- fread("DANU Events.txt")

Pats_Ident_Dx <- Pats_Ident_Dx %>% left_join(DANU_Events) %>% select(-c(weight))

Pats_Ident_Dx <- Pats_Ident_Dx %>% filter(!grepl("BMI", code)) 

Pats_Ident_Dx <- Pats_Ident_Dx %>% select(patid, weight_2, claimed) %>% distinct()

N_Dx_Pat <- data.frame(Pats_Ident_Dx %>% group_by(patid, weight_2) %>% 
                         count() %>% group_by(n) %>% summarise(Pats_N_dx = sum(weight_2))) 



# Distance between Dx for the pats identified by Dx only --------------

DANU_Demographics <- fread("DANU_Demographics_v2Ident_source.txt")

Pats_Ident_Dx <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% filter(Priority_Ident=="Diagnosis") %>% select(patid, weight_2)

DANU_Events <- fread("DANU Events.txt")

Pats_Ident_Dx <- Pats_Ident_Dx %>% left_join(DANU_Events) %>% select(-c(weight))

Pats_Ident_Dx <- Pats_Ident_Dx %>% filter(!grepl("BMI", code)) 

Pats_Ident_Dx <- Pats_Ident_Dx %>% select(patid, weight_2, claimed) %>% distinct()
Pats_Ident_Dx <- Pats_Ident_Dx %>% mutate(claimed = as.Date(claimed))
Pats_Ident_Dx <- Pats_Ident_Dx %>% mutate(claimed = as.numeric(claimed))

Pats_Ident_Dx <- Pats_Ident_Dx %>% group_by(patid) %>% mutate(distance= (claimed - lag(claimed)))

temp <- Pats_Ident_Dx


temp2 <- temp %>% group_by(distance) %>% summarise(n=sum(weight_2))

temp2 %>% mutate( distance2 = ifelse(distance>=1 & distance<30.5, "1month",
                                     ifelse(distance>=30.5 & distance<61, "2month",
                                            ifelse(distance>=61 & distance<91.5, "3month",
                                                   ifelse(distance>=91.5 & distance<122, "4month",
                                                          ifelse(distance>=122 & distance<152.5, "5month",
                                                                 ifelse(distance>=152.5 & distance<183, "6month",
                                                                        ifelse(distance>=183 & distance<213.5, "7month",
                                                                               ifelse(distance>=213.5 & distance<244, "8month",
                                                                                      ifelse(distance>=244 & distance<274.5, "9month",
                                                                                             ifelse(distance>=274.5 & distance<305, "10month",
                                                                                                    ifelse(distance>=305 & distance<335.5, "11month",
                                                                                                           ifelse(distance>=335.5 & distance<366, "12month", 
                                                                                                                  ifelse(distance>=366 & distance<731, "2years","+2years")))))))))))))) %>%
  group_by(distance2) %>% summarise(pats=sum(n))
# ------- 
# Find Inj GLP1 Pats patients to vizualise ------
OBE_Drug_Histories_v2 <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_v2 <- gather(OBE_Drug_Histories_v2, Month, Treat, month1:month60, factor_key=TRUE)

Inj_m60 <- OBE_Drug_Histories_v2 %>% filter((grepl("48", Treat)|grepl("49", Treat)|grepl("50", Treat)|grepl("51", Treat)|grepl("52", Treat)|grepl("53", Treat))&Month=="month60") %>% select(patient)
Inj_m54 <- OBE_Drug_Histories_v2 %>% filter((grepl("48", Treat)|grepl("49", Treat)|grepl("50", Treat)|grepl("51", Treat)|grepl("52", Treat)|grepl("53", Treat))&Month=="month64") %>% select(patient)


Pats_to_tract <- Inj_m60 %>% filter(row_number()<=25) %>% bind_rows(Inj_m54  %>% filter(row_number()<=25)) %>% distinct()

n <- Pats_to_tract %>% summarise(n=list(Pats_to_tract))
unlist(n)

# 
# -----
# BMI distribution per stock --------------------

DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% arrange(patid, weight, claimed)
DANU_Events <- DANU_Events %>% group_by(patid) %>% slice(n())
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
names(DANU_Events)[1] <- "patient"

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_BMI) %>% filter(!is.na(weight_BMI))
names(DANU_Demographics)[1] <- "patient"

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics)
DANU_Events <- DANU_Events %>% filter(!is.na(weight_BMI))

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

setDT(DANU_Events)[, Month_Yr := format(as.Date(claimed), "%Y-%m") ]

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>%  filter(!is.na(Exact_Month))

# Get stock on that month
OBE_Box_Histories     <- fread("OBE Box Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Box_Histories     <- gather(OBE_Box_Histories, Month, Box, month1:month60, factor_key=TRUE)
OBE_Box_Histories     <- OBE_Box_Histories %>% mutate(Box = str_sub(Box, 2L, 2L))
OBE_Box_Histories$Month <- as.character(OBE_Box_Histories$Month)
OBE_Box_Histories$Month <- parse_number(OBE_Box_Histories$Month)

DANU_Events <- DANU_Events %>% inner_join(OBE_Box_Histories %>% select(patient, Month, Box), 
                                          by = c("patient"="patient", "Exact_Month"="Month")) %>% filter(!is.na(Box))

DANU_Events %>% group_by(Box) %>% summarise(n=sum(as.numeric(weight_BMI)))

# Box            n
# <chr>      <dbl>
# 1 a       4041063.
# 2 g          4978.
# 3 G         51851.
# 4 H         69676.
# 5 o         54283.
# 6 w          1128.
# 7 x     161586179.

DANU_Events %>% group_by(Box) %>% summarise(n=weighted.mean(as.numeric(code), as.numeric(weight_BMI))) %>% arrange(-n)

# Box       n
# <chr> <dbl>
# 1 H      38.9
# 2 g      35.7
# 3 w      34.9
# 4 G      34.6
# 5 o      34.5
# 6 x      30.8
# 7 a      30.5

DANU_Events %>% group_by(Box) %>% summarise(n=weighted.median(as.numeric(code), as.numeric(weight_BMI))) %>% arrange(-n)

# Box       n
# <chr> <dbl>
# 1 H      38.2
# 2 g      36.5
# 3 o      33.8
# 4 G      33.2
# 5 w      30.2
# 6 x      29.2
# 7 a      28.8

length(unique(DANU_Events$patient))
DANU_Events %>% ungroup() %>% select(Box, code) %>% 
  mutate(Box = factor(Box, levels=c("x", "w", "a", "o", "g", "G", "H"))) %>% group_by(Box) %>%
  ggplot(aes(code))+
  geom_density(aes(fill = Box, colour=Box), alpha =0.7, show.legend = F, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~Box, ncol = 8)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  coord_flip()+
  xlab("BMI kg/m2\n")+ ylab("\nProportion of patients")

# -----

# BMIs Reductions Before vs after therapy 3 months ------------------------

# --------
# GLP1 Injectable ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))


DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no GLP1Injectable too zero, and GLP1Injectable to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "GLP1Injectable")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "GLP1Injectable")) %>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "GLP1Injectable")) %>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "GLP1Injectable")) %>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "GLP1Injectable")) %>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "GLP1Injectable")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1Injectable",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON GLP1 Injectable
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 Injectable
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 Injectable
OBE_Drug_Histories_FIRST_GLP1Injectable <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_GLP1Injectable)[3] <- "Month_First_GLP1Injectable"

#filter for the patients that fit the GLP1 Injectable criteria
Patient_first_GLP1Injectable <-  OBE_Drug_Histories_FIRST_GLP1Injectable %>% select(patient)
DANU_Measures_BMI <- Patient_first_GLP1Injectable %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Injectable month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_GLP1Injectable, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before GLP1 Injectable start and months after GLP1 Injectable start
BMI_before_GLP1Injectable <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_GLP1Injectable)-3)
BMI_before_GLP1Injectable <- BMI_before_GLP1Injectable %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_GLP1Injectable)[6] <- "Month_Prior"
names(BMI_before_GLP1Injectable)[7] <- "BMI_Prior"
BMI_before_GLP1Injectable <- BMI_before_GLP1Injectable %>% select(-c(claimed, Month_Yr))


BMI_after_GLP1Injectable <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_GLP1Injectable+3))
BMI_after_GLP1Injectable <- BMI_after_GLP1Injectable %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_GLP1Injectable)[6] <- "Month_After"
names(BMI_after_GLP1Injectable)[7] <- "BMI_After"
BMI_after_GLP1Injectable <- BMI_after_GLP1Injectable %>% select(-c(claimed, Month_Yr))

#join the before and after HbA1cs
BMI_GLP1Injectable_BEFORE_vs_AFTER <- BMI_before_GLP1Injectable %>% full_join(BMI_after_GLP1Injectable)
BMI_GLP1Injectable_BEFORE_vs_AFTER <- BMI_GLP1Injectable_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_Prior) # 38.10369
mean(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_After) # 35.2642

median(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_Prior) # 36.5
median(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_After) # 34

write.csv(BMI_GLP1Injectable_BEFORE_vs_AFTER, "BMI_GLP1Injectable_BEFORE_vs_AFTER_closest_3months.csv")




# GLP1 Oral ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))


DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no GLP1Oral too zero, and GLP1Oral to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "GLP1Oral")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1Oral",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON GLP1 Oral
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 Oral
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 Oral
OBE_Drug_Histories_FIRST_GLP1Oral <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_GLP1Oral)[3] <- "Month_First_GLP1Oral"

#filter for the patients that fit the GLP1 Oral criteria
Patient_first_GLP1Oral <-  OBE_Drug_Histories_FIRST_GLP1Oral %>% select(patient)
DANU_Measures_BMI <- Patient_first_GLP1Oral %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_GLP1Oral, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before GLP1 Oral start and months after GLP1 Oral start
BMI_before_GLP1Oral <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_GLP1Oral)-3)
BMI_before_GLP1Oral <- BMI_before_GLP1Oral %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_GLP1Oral)[6] <- "Month_Prior"
names(BMI_before_GLP1Oral)[7] <- "BMI_Prior"
BMI_before_GLP1Oral <- BMI_before_GLP1Oral %>% select(-c(claimed, Month_Yr))

BMI_after_GLP1Oral <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_GLP1Oral+3))
BMI_after_GLP1Oral <- BMI_after_GLP1Oral %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_GLP1Oral)[6] <- "Month_After"
names(BMI_after_GLP1Oral)[7] <- "BMI_After"
BMI_after_GLP1Oral <- BMI_after_GLP1Oral %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_GLP1Oral_BEFORE_vs_AFTER <- BMI_before_GLP1Oral %>% full_join(BMI_after_GLP1Oral)
BMI_GLP1Oral_BEFORE_vs_AFTER <- BMI_GLP1Oral_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_Prior) # 38.16667
mean(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_After) # 35.66667

median(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_Prior) #  38
median(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_After) # 36

write.csv(BMI_GLP1Oral_BEFORE_vs_AFTER, "BMI_GLP1Oral_BEFORE_vs_AFTER_closest_3months.csv")





# Surgery ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Surgery too zero, and Surgery to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Surgery")) %>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Surgery")) %>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Surgery")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Surgery",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Surgery
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Surgery
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Surgery
OBE_Drug_Histories_FIRST_Surgery <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Surgery)[3] <- "Month_First_Surgery"

#filter for the patients that fit the Surgery criteria
Patient_first_Surgery <-  OBE_Drug_Histories_FIRST_Surgery %>% select(patient)
DANU_Measures_BMI <- Patient_first_Surgery %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Surgery, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Surgery start and months after Surgery start
BMI_before_Surgery <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_Surgery)-3)
BMI_before_Surgery <- BMI_before_Surgery %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Surgery)[6] <- "Month_Prior"
names(BMI_before_Surgery)[7] <- "BMI_Prior"
BMI_before_Surgery <- BMI_before_Surgery %>% select(-c(claimed, Month_Yr))

BMI_after_Surgery <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_Surgery+3))
BMI_after_Surgery <- BMI_after_Surgery %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Surgery)[6] <- "Month_After"
names(BMI_after_Surgery)[7] <- "BMI_After"
BMI_after_Surgery <- BMI_after_Surgery %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Surgery_BEFORE_vs_AFTER <- BMI_before_Surgery %>% full_join(BMI_after_Surgery)
BMI_Surgery_BEFORE_vs_AFTER <- BMI_Surgery_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Surgery_BEFORE_vs_AFTER$BMI_Prior) # 42.68829
mean(BMI_Surgery_BEFORE_vs_AFTER$BMI_After) # 31.61037

median(BMI_Surgery_BEFORE_vs_AFTER$BMI_Prior) #  42.5
median(BMI_Surgery_BEFORE_vs_AFTER$BMI_After) # 30.5

write.csv(BMI_Surgery_BEFORE_vs_AFTER, "BMI_Surgery_BEFORE_vs_AFTER_closest_3months.csv")

# Antiobesity ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Antiobesity too zero, and Antiobesity to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('11',.), ~replace(., grepl('11', .), "Antiobesity")) %>%
  mutate_if(grepl('12',.), ~replace(., grepl('12', .), "Antiobesity")) %>%
  mutate_if(grepl('13',.), ~replace(., grepl('13', .), "Antiobesity")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiobesity",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Antiobesity
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Antiobesity
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Antiobesity
OBE_Drug_Histories_FIRST_Antiobesity <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Antiobesity)[3] <- "Month_First_Antiobesity"

#filter for the patients that fit the Antiobesity criteria
Patient_first_Antiobesity <-  OBE_Drug_Histories_FIRST_Antiobesity %>% select(patient)
DANU_Measures_BMI <- Patient_first_Antiobesity %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Antiobesity, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Antiobesity start and months after Antiobesity start
BMI_before_Antiobesity <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_Antiobesity)-3)
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Antiobesity)[6] <- "Month_Prior"
names(BMI_before_Antiobesity)[7] <- "BMI_Prior"
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% select(-c(claimed, Month_Yr))

BMI_after_Antiobesity <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_Antiobesity+3))
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Antiobesity)[6] <- "Month_After"
names(BMI_after_Antiobesity)[7] <- "BMI_After"
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity %>% full_join(BMI_after_Antiobesity)
BMI_Antiobesity_BEFORE_vs_AFTER <- BMI_Antiobesity_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_Prior) # 36.25033
mean(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_After) # 34.0339

median(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_Prior) #  35
median(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_After) # 33

write.csv(BMI_Antiobesity_BEFORE_vs_AFTER, "BMI_Antiobesity_BEFORE_vs_AFTER_closest_3months.csv")

# Anorectic ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Anorectic too zero, and Anorectic to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "Anorectic")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Anorectic",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Anorectic
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Anorectic
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Anorectic
OBE_Drug_Histories_FIRST_Anorectic <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Anorectic)[3] <- "Month_First_Anorectic"

#filter for the patients that fit the Anorectic criteria
Patient_first_Anorectic <-  OBE_Drug_Histories_FIRST_Anorectic %>% select(patient)
DANU_Measures_BMI <- Patient_first_Anorectic %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Anorectic, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Anorectic start and months after Anorectic start
BMI_before_Anorectic <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_Anorectic)-3)
BMI_before_Anorectic <- BMI_before_Anorectic %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Anorectic)[6] <- "Month_Prior"
names(BMI_before_Anorectic)[7] <- "BMI_Prior"
BMI_before_Anorectic <- BMI_before_Anorectic %>% select(-c(claimed, Month_Yr))

BMI_after_Anorectic <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_Anorectic+3))
BMI_after_Anorectic <- BMI_after_Anorectic %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Anorectic)[6] <- "Month_After"
names(BMI_after_Anorectic)[7] <- "BMI_After"
BMI_after_Anorectic <- BMI_after_Anorectic %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Anorectic_BEFORE_vs_AFTER <- BMI_before_Anorectic %>% full_join(BMI_after_Anorectic)
BMI_Anorectic_BEFORE_vs_AFTER <- BMI_Anorectic_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Anorectic_BEFORE_vs_AFTER$BMI_Prior) # 32.70153
mean(BMI_Anorectic_BEFORE_vs_AFTER$BMI_After) # 30.42125

median(BMI_Anorectic_BEFORE_vs_AFTER$BMI_Prior) #  31.5
median(BMI_Anorectic_BEFORE_vs_AFTER$BMI_After) #  29

write.csv(BMI_Anorectic_BEFORE_vs_AFTER, "BMI_Anorectic_BEFORE_vs_AFTER_closest_3months.csv")



# WeightLoss ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no WeightLoss too zero, and WeightLoss to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "WeightLoss"))

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="WeightLoss",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON WeightLoss
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to WeightLoss
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took WeightLoss
OBE_Drug_Histories_FIRST_WeightLoss <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_WeightLoss)[3] <- "Month_First_WeightLoss"

#filter for the patients that fit the WeightLoss criteria
Patient_first_WeightLoss <-  OBE_Drug_Histories_FIRST_WeightLoss %>% select(patient)
DANU_Measures_BMI <- Patient_first_WeightLoss %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_WeightLoss, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before WeightLoss start and months after WeightLoss start
BMI_before_WeightLoss <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_WeightLoss)-3)
BMI_before_WeightLoss <- BMI_before_WeightLoss %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_WeightLoss)[6] <- "Month_Prior"
names(BMI_before_WeightLoss)[7] <- "BMI_Prior"
BMI_before_WeightLoss <- BMI_before_WeightLoss %>% select(-c(claimed, Month_Yr))

BMI_after_WeightLoss <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_WeightLoss+3))
BMI_after_WeightLoss <- BMI_after_WeightLoss %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_WeightLoss)[6] <- "Month_After"
names(BMI_after_WeightLoss)[7] <- "BMI_After"
BMI_after_WeightLoss <- BMI_after_WeightLoss %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_WeightLoss_BEFORE_vs_AFTER <- BMI_before_WeightLoss %>% full_join(BMI_after_WeightLoss)
BMI_WeightLoss_BEFORE_vs_AFTER <- BMI_WeightLoss_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_Prior) # 36.15
mean(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_After) # 33.9

median(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_Prior) #  34.25
median(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_After) # 31

write.csv(BMI_WeightLoss_BEFORE_vs_AFTER, "BMI_WeightLoss_BEFORE_vs_AFTER_closest_3months.csv")

# Summary evolution over time -------
BMI_Evolution_US_All_3months <- fread("BMI_Evolution_US_All_3months.csv")

BMI_Evolution_US_All_3months %>% group_by(Therapy, Period) %>% summarise(n=mean(BMI))

# Therapy          Period     n
# <chr>            <chr>  <dbl>
# 1 Amphetamines     After   30.7
# 2 Amphetamines     Before  34.4
# 3 GLP1 Injectable  After   35.3
# 4 GLP1 Injectable  Before  38.1
# 5 GLP1 Oral        After   35.7
# 6 GLP1 Oral        Before  38.2
# 7 Lipase Inhibitor After   33.9
# 8 Lipase Inhibitor Before  36.2
# 9 Naltrexone       After   34.0
# 10 Naltrexone       Before  36.3
# 11 Surgery          After   31.6
# 12 Surgery          Before  42.7

BMI_Evolution_US_All_3months %>% group_by(Therapy, Period) %>% summarise(n=median(BMI))

# Therapy          Period     n
# <chr>            <chr>  <dbl>
# 1 Amphetamines     After   29.5
# 2 Amphetamines     Before  32.5
# 3 GLP1 Injectable  After   34  
# 4 GLP1 Injectable  Before  36.5
# 5 GLP1 Oral        After   36  
# 6 GLP1 Oral        Before  38  
# 7 Lipase Inhibitor After   31  
# 8 Lipase Inhibitor Before  34.2
# 9 Naltrexone       After   33  
# 10 Naltrexone       Before  35  
# 11 Surgery          After   30.5
# 12 Surgery          Before  42.5


Pats_to_keep_paired <- BMI_Evolution_US_All_3months %>% group_by(patient, Therapy) %>% filter(Month>=-12 & Month<=12) %>% 
  count() %>% filter(n%%2==0) %>% select(patient, Therapy)

BMI_Evolution_US_All_3months <- Pats_to_keep_paired %>% left_join(BMI_Evolution_US_All_3months)

BMI_Evolution_US_All_3months %>% 
  group_by(Therapy, Month) %>%  
  ggplot(aes(x=Month, y=BMI, fill=Therapy, colour=Therapy))+
  geom_smooth(size=2.5, method = "loess", se=F )+
  ylab("BMI kg/m2\n")+
  xlab("\nMonth")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_viridis_d()+
  ggsci::scale_colour_jama()


# -----
# BMIs Reductions Before vs after therapy 1 month ------------------------

# ----------
# GLP1 Injectable ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))


DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no GLP1 Injectable too zero, and GLP1Injectable to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "GLP1Injectable")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "GLP1Injectable")) %>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "GLP1Injectable")) %>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "GLP1Injectable")) %>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "GLP1Injectable")) %>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "GLP1Injectable")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1Injectable",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON GLP1 Injectable
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 Injectable
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 Injectable
OBE_Drug_Histories_FIRST_GLP1Injectable <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_GLP1Injectable)[3] <- "Month_First_GLP1Injectable"

#filter for the patients that fit the GLP1 Injectable criteria
Patient_first_GLP1Injectable <-  OBE_Drug_Histories_FIRST_GLP1Injectable %>% select(patient)
DANU_Measures_BMI <- Patient_first_GLP1Injectable %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Injectable month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_GLP1Injectable, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before GLP1 Injectable start and months after GLP1 Injectable start
BMI_before_GLP1Injectable <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_GLP1Injectable)-1)
BMI_before_GLP1Injectable <- BMI_before_GLP1Injectable %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_GLP1Injectable)[6] <- "Month_Prior"
names(BMI_before_GLP1Injectable)[7] <- "BMI_Prior"
BMI_before_GLP1Injectable <- BMI_before_GLP1Injectable %>% select(-c(claimed, Month_Yr))


BMI_after_GLP1Injectable <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_GLP1Injectable+1))
BMI_after_GLP1Injectable <- BMI_after_GLP1Injectable %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_GLP1Injectable)[6] <- "Month_After"
names(BMI_after_GLP1Injectable)[7] <- "BMI_After"
BMI_after_GLP1Injectable <- BMI_after_GLP1Injectable %>% select(-c(claimed, Month_Yr))

#join the before and after HbA1cs
BMI_GLP1Injectable_BEFORE_vs_AFTER <- BMI_before_GLP1Injectable %>% full_join(BMI_after_GLP1Injectable)
BMI_GLP1Injectable_BEFORE_vs_AFTER <- BMI_GLP1Injectable_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_Prior) # 37.92112
mean(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_After) # 34.89563

median(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_Prior) # 36.5
median(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_After) # 33.5

write.csv(BMI_GLP1Injectable_BEFORE_vs_AFTER, "BMI_GLP1Injectable_BEFORE_vs_AFTER_closest_1months.csv")




# GLP1 Oral ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))


DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no GLP1Oral too zero, and GLP1Oral to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "GLP1Oral")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1Oral",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON GLP1 Oral
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 Oral
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 Oral
OBE_Drug_Histories_FIRST_GLP1Oral <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_GLP1Oral)[3] <- "Month_First_GLP1Oral"

#filter for the patients that fit the GLP1 Oral criteria
Patient_first_GLP1Oral <-  OBE_Drug_Histories_FIRST_GLP1Oral %>% select(patient)
DANU_Measures_BMI <- Patient_first_GLP1Oral %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_GLP1Oral, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before GLP1 Oral start and months after GLP1 Oral start
BMI_before_GLP1Oral <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_GLP1Oral)-1)
BMI_before_GLP1Oral <- BMI_before_GLP1Oral %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_GLP1Oral)[6] <- "Month_Prior"
names(BMI_before_GLP1Oral)[7] <- "BMI_Prior"
BMI_before_GLP1Oral <- BMI_before_GLP1Oral %>% select(-c(claimed, Month_Yr))

BMI_after_GLP1Oral <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_GLP1Oral+1))
BMI_after_GLP1Oral <- BMI_after_GLP1Oral %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_GLP1Oral)[6] <- "Month_After"
names(BMI_after_GLP1Oral)[7] <- "BMI_After"
BMI_after_GLP1Oral <- BMI_after_GLP1Oral %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_GLP1Oral_BEFORE_vs_AFTER <- BMI_before_GLP1Oral %>% full_join(BMI_after_GLP1Oral)
BMI_GLP1Oral_BEFORE_vs_AFTER <- BMI_GLP1Oral_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_Prior) # 36.9375
mean(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_After) # 34.51562

median(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_Prior) #  36.75
median(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_After) # 34.5

write.csv(BMI_GLP1Oral_BEFORE_vs_AFTER, "BMI_GLP1Oral_BEFORE_vs_AFTER_closest_1months.csv")





# Surgery ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Surgery too zero, and Surgery to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Surgery")) %>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Surgery")) %>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Surgery")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Surgery",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Surgery
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Surgery
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Surgery
OBE_Drug_Histories_FIRST_Surgery <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Surgery)[3] <- "Month_First_Surgery"

#filter for the patients that fit the Surgery criteria
Patient_first_Surgery <-  OBE_Drug_Histories_FIRST_Surgery %>% select(patient)
DANU_Measures_BMI <- Patient_first_Surgery %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Surgery, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Surgery start and months after Surgery start
BMI_before_Surgery <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_Surgery)-1)
BMI_before_Surgery <- BMI_before_Surgery %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Surgery)[6] <- "Month_Prior"
names(BMI_before_Surgery)[7] <- "BMI_Prior"
BMI_before_Surgery <- BMI_before_Surgery %>% select(-c(claimed, Month_Yr))

BMI_after_Surgery <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_Surgery+1))
BMI_after_Surgery <- BMI_after_Surgery %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Surgery)[6] <- "Month_After"
names(BMI_after_Surgery)[7] <- "BMI_After"
BMI_after_Surgery <- BMI_after_Surgery %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Surgery_BEFORE_vs_AFTER <- BMI_before_Surgery %>% full_join(BMI_after_Surgery)
BMI_Surgery_BEFORE_vs_AFTER <- BMI_Surgery_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Surgery_BEFORE_vs_AFTER$BMI_Prior) # 42.81374
mean(BMI_Surgery_BEFORE_vs_AFTER$BMI_After) # 31.83012

median(BMI_Surgery_BEFORE_vs_AFTER$BMI_Prior) #  42.5
median(BMI_Surgery_BEFORE_vs_AFTER$BMI_After) # 31

write.csv(BMI_Surgery_BEFORE_vs_AFTER, "BMI_Surgery_BEFORE_vs_AFTER_closest_1months.csv")

# Antiobesity ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Antiobesity too zero, and Antiobesity to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('11',.), ~replace(., grepl('11', .), "Antiobesity")) %>%
  mutate_if(grepl('12',.), ~replace(., grepl('12', .), "Antiobesity")) %>%
  mutate_if(grepl('13',.), ~replace(., grepl('13', .), "Antiobesity")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiobesity",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Antiobesity
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Antiobesity
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Antiobesity
OBE_Drug_Histories_FIRST_Antiobesity <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Antiobesity)[3] <- "Month_First_Antiobesity"

#filter for the patients that fit the Antiobesity criteria
Patient_first_Antiobesity <-  OBE_Drug_Histories_FIRST_Antiobesity %>% select(patient)
DANU_Measures_BMI <- Patient_first_Antiobesity %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Antiobesity, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Antiobesity start and months after Antiobesity start
BMI_before_Antiobesity <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_Antiobesity)-1)
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Antiobesity)[6] <- "Month_Prior"
names(BMI_before_Antiobesity)[7] <- "BMI_Prior"
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% select(-c(claimed, Month_Yr))

BMI_after_Antiobesity <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_Antiobesity+1))
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Antiobesity)[6] <- "Month_After"
names(BMI_after_Antiobesity)[7] <- "BMI_After"
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity %>% full_join(BMI_after_Antiobesity)
BMI_Antiobesity_BEFORE_vs_AFTER <- BMI_Antiobesity_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_Prior) # 36.34896
mean(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_After) # 33.93461

median(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_Prior) #  35
median(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_After) # 32.5

write.csv(BMI_Antiobesity_BEFORE_vs_AFTER, "BMI_Antiobesity_BEFORE_vs_AFTER_closest_1months.csv")

# Anorectic ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Anorectic too zero, and Anorectic to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "Anorectic")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Anorectic",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Anorectic
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Anorectic
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Anorectic
OBE_Drug_Histories_FIRST_Anorectic <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Anorectic)[3] <- "Month_First_Anorectic"

#filter for the patients that fit the Anorectic criteria
Patient_first_Anorectic <-  OBE_Drug_Histories_FIRST_Anorectic %>% select(patient)
DANU_Measures_BMI <- Patient_first_Anorectic %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Anorectic, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Anorectic start and months after Anorectic start
BMI_before_Anorectic <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_Anorectic)-1)
BMI_before_Anorectic <- BMI_before_Anorectic %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Anorectic)[6] <- "Month_Prior"
names(BMI_before_Anorectic)[7] <- "BMI_Prior"
BMI_before_Anorectic <- BMI_before_Anorectic %>% select(-c(claimed, Month_Yr))

BMI_after_Anorectic <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_Anorectic+1))
BMI_after_Anorectic <- BMI_after_Anorectic %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Anorectic)[6] <- "Month_After"
names(BMI_after_Anorectic)[7] <- "BMI_After"
BMI_after_Anorectic <- BMI_after_Anorectic %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Anorectic_BEFORE_vs_AFTER <- BMI_before_Anorectic %>% full_join(BMI_after_Anorectic)
BMI_Anorectic_BEFORE_vs_AFTER <- BMI_Anorectic_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Anorectic_BEFORE_vs_AFTER$BMI_Prior) # 32.81357
mean(BMI_Anorectic_BEFORE_vs_AFTER$BMI_After) # 30.33303

median(BMI_Anorectic_BEFORE_vs_AFTER$BMI_Prior) #  31.5
median(BMI_Anorectic_BEFORE_vs_AFTER$BMI_After) #  29

write.csv(BMI_Anorectic_BEFORE_vs_AFTER, "BMI_Anorectic_BEFORE_vs_AFTER_closest_1months.csv")



# WeightLoss ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no WeightLoss too zero, and WeightLoss to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "WeightLoss"))

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="WeightLoss",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON WeightLoss
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to WeightLoss
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took WeightLoss
OBE_Drug_Histories_FIRST_WeightLoss <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_WeightLoss)[3] <- "Month_First_WeightLoss"

#filter for the patients that fit the WeightLoss criteria
Patient_first_WeightLoss <-  OBE_Drug_Histories_FIRST_WeightLoss %>% select(patient)
DANU_Measures_BMI <- Patient_first_WeightLoss %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_WeightLoss, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before WeightLoss start and months after WeightLoss start
BMI_before_WeightLoss <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month < (Month_First_WeightLoss)-1)
BMI_before_WeightLoss <- BMI_before_WeightLoss %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_WeightLoss)[6] <- "Month_Prior"
names(BMI_before_WeightLoss)[7] <- "BMI_Prior"
BMI_before_WeightLoss <- BMI_before_WeightLoss %>% select(-c(claimed, Month_Yr))

BMI_after_WeightLoss <- DANU_Measures_BMI %>% group_by(patient) %>% filter(Exact_Month > (Month_First_WeightLoss+1))
BMI_after_WeightLoss <- BMI_after_WeightLoss %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_WeightLoss)[6] <- "Month_After"
names(BMI_after_WeightLoss)[7] <- "BMI_After"
BMI_after_WeightLoss <- BMI_after_WeightLoss %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_WeightLoss_BEFORE_vs_AFTER <- BMI_before_WeightLoss %>% full_join(BMI_after_WeightLoss)
BMI_WeightLoss_BEFORE_vs_AFTER <- BMI_WeightLoss_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_Prior) # 36.3871
mean(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_After) # 33.77419

median(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_Prior) #  35
median(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_After) # 31

write.csv(BMI_WeightLoss_BEFORE_vs_AFTER, "BMI_WeightLoss_BEFORE_vs_AFTER_closest_1months.csv")

# Summary evolution over time -------
BMI_Evolution_US_All_1month <- fread("BMI_Evolution_US_All_1month.csv")

BMI_Evolution_US_All_1month %>% group_by(Therapy, Period) %>% summarise(n=mean(BMI))

# Therapy           Period     n
# <chr>             <chr>  <dbl>
# 1 Amphetamines      After   30.3
# 2 Amphetamines      Before  32.8
# 3 GLP1 Injectable   After   34.9
# 4 GLP1 Injectable   Before  37.9
# 5 GLP1 Oral         After   34.5
# 6 GLP1 Oral         Before  36.9
# 7 Lipase Inhibitors After   33.8
# 8 Lipase Inhibitors Before  36.4
# 9 Naltrexone        After   33.9
# 10 Naltrexone        Before  36.3
# 11 Surgery           After   31.8
# 12 Surgery           Before  42.8

BMI_Evolution_US_All_1month %>% group_by(Therapy, Period) %>% summarise(n=median(BMI))

# Therapy           Period     n
# <chr>             <chr>  <dbl>
#   1 Amphetamines      After   29  
# 2 Amphetamines      Before  31.5
# 3 GLP1 Injectable   After   33.5
# 4 GLP1 Injectable   Before  36.5
# 5 GLP1 Oral         After   34.5
# 6 GLP1 Oral         Before  36.8
# 7 Lipase Inhibitors After   31  
# 8 Lipase Inhibitors Before  35  
# 9 Naltrexone        After   32.5
# 10 Naltrexone        Before  35  
# 11 Surgery           After   31  
# 12 Surgery           Before  42.5


Pats_to_keep_paired <- BMI_Evolution_US_All_1month %>% group_by(patient, Therapy) %>% filter(Month>=-12 & Month<=12) %>% 
  count() %>% filter(n%%2==0) %>% select(patient, Therapy)

BMI_Evolution_US_All_1month <- Pats_to_keep_paired %>% left_join(BMI_Evolution_US_All_1month)

BMI_Evolution_US_All_1month %>% 
  group_by(Therapy, Month) %>%  
  ggplot(aes(x=Month, y=BMI, fill=Therapy, colour=Therapy))+
  geom_smooth(size=2.5, method = "loess", se=F )+
  ylab("BMI kg/m2\n")+
  xlab("\nMonth")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_viridis_d()+
  ggsci::scale_colour_jama()

# ----

# BMIs Reductions Before vs after therapy 1 month to 12m ------------------------

# ------
# GLP1 Injectable ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))


DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no GLP1Injectable too zero, and GLP1Injectable to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "GLP1Injectable")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "GLP1Injectable")) %>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "GLP1Injectable")) %>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "GLP1Injectable")) %>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "GLP1Injectable")) %>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "GLP1Injectable")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1Injectable",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON GLP1 Injectable
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 Injectable
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 Injectable
OBE_Drug_Histories_FIRST_GLP1Injectable <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_GLP1Injectable)[3] <- "Month_First_GLP1Injectable"

#filter for the patients that fit the GLP1 Injectable criteria
Patient_first_GLP1Injectable <-  OBE_Drug_Histories_FIRST_GLP1Injectable %>% select(patient)
DANU_Measures_BMI <- Patient_first_GLP1Injectable %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Injectable month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_GLP1Injectable, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before GLP1 Injectable start and months after GLP1 Injectable start
BMI_before_GLP1Injectable <- DANU_Measures_BMI %>% group_by(patient) %>% filter( (Exact_Month < (Month_First_GLP1Injectable)-1) & (Exact_Month > (Month_First_GLP1Injectable)-13))
BMI_before_GLP1Injectable <- BMI_before_GLP1Injectable %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_GLP1Injectable)[6] <- "Month_Prior"
names(BMI_before_GLP1Injectable)[7] <- "BMI_Prior"
BMI_before_GLP1Injectable <- BMI_before_GLP1Injectable %>% select(-c(claimed, Month_Yr))


BMI_after_GLP1Injectable <- DANU_Measures_BMI %>% group_by(patient) %>% filter((Exact_Month > (Month_First_GLP1Injectable+1))&(Exact_Month < (Month_First_GLP1Injectable)+13))
BMI_after_GLP1Injectable <- BMI_after_GLP1Injectable %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_GLP1Injectable)[6] <- "Month_After"
names(BMI_after_GLP1Injectable)[7] <- "BMI_After"
BMI_after_GLP1Injectable <- BMI_after_GLP1Injectable %>% select(-c(claimed, Month_Yr))

#join the before and after HbA1cs
BMI_GLP1Injectable_BEFORE_vs_AFTER <- BMI_before_GLP1Injectable %>% full_join(BMI_after_GLP1Injectable)
BMI_GLP1Injectable_BEFORE_vs_AFTER <- BMI_GLP1Injectable_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_Prior) # 37.22713
mean(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_After) # 35.1189

median(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_Prior) # 36
median(BMI_GLP1Injectable_BEFORE_vs_AFTER$BMI_After) # 34

write.csv(BMI_GLP1Injectable_BEFORE_vs_AFTER, "BMI_GLP1Injectable_BEFORE_vs_AFTER_closest_1to12months.csv")

BMI_GLP1Injectable_BEFORE_vs_AFTER <- fread("BMI_GLP1Injectable_BEFORE_vs_AFTER_closest_1to12months.csv")
BMI_GLP1Injectable_BEFORE_vs_AFTER <- BMI_GLP1Injectable_BEFORE_vs_AFTER %>% mutate(difference = BMI_After-BMI_Prior)


# GLP1 Oral ---------------
DANU_Events <- fread("DANU Events.txt")

temp <- DANU_Events %>% filter(patid=="PT084499912")
temp <- temp %>% filter(!grepl("BMI",code))
fwrite(temp, "Dxs_PT084499912.txt", sep="\t")


DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))


DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no GLP1Oral too zero, and GLP1Oral to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "GLP1Oral")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1Oral",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON GLP1 Oral
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 Oral
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 Oral
OBE_Drug_Histories_FIRST_GLP1Oral <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_GLP1Oral)[3] <- "Month_First_GLP1Oral"

#filter for the patients that fit the GLP1 Oral criteria
Patient_first_GLP1Oral <-  OBE_Drug_Histories_FIRST_GLP1Oral %>% select(patient)
DANU_Measures_BMI <- Patient_first_GLP1Oral %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_GLP1Oral, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before GLP1 Oral start and months after GLP1 Oral start
BMI_before_GLP1Oral <- DANU_Measures_BMI %>% group_by(patient) %>% filter( (Exact_Month < (Month_First_GLP1Oral)-1) & (Exact_Month > (Month_First_GLP1Oral)-13))
BMI_before_GLP1Oral <- BMI_before_GLP1Oral %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_GLP1Oral)[6] <- "Month_Prior"
names(BMI_before_GLP1Oral)[7] <- "BMI_Prior"
BMI_before_GLP1Oral <- BMI_before_GLP1Oral %>% select(-c(claimed, Month_Yr))


BMI_after_GLP1Oral <- DANU_Measures_BMI %>% group_by(patient) %>% filter((Exact_Month > (Month_First_GLP1Oral+1))&(Exact_Month < (Month_First_GLP1Oral)+13))
BMI_after_GLP1Oral <- BMI_after_GLP1Oral %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_GLP1Oral)[6] <- "Month_After"
names(BMI_after_GLP1Oral)[7] <- "BMI_After"
BMI_after_GLP1Oral <- BMI_after_GLP1Oral %>% select(-c(claimed, Month_Yr))


#join the before and after BMI
BMI_GLP1Oral_BEFORE_vs_AFTER <- BMI_before_GLP1Oral %>% full_join(BMI_after_GLP1Oral)
BMI_GLP1Oral_BEFORE_vs_AFTER <- BMI_GLP1Oral_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_Prior) # 36.35185
mean(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_After) # 35.01852

median(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_Prior) #  36.5
median(BMI_GLP1Oral_BEFORE_vs_AFTER$BMI_After) # 34.5

write.csv(BMI_GLP1Oral_BEFORE_vs_AFTER, "BMI_GLP1Oral_BEFORE_vs_AFTER_closest_1to12months.csv")





# Surgery ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Surgery too zero, and Surgery to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Surgery")) %>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Surgery")) %>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Surgery")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Surgery",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Surgery
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Surgery
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Surgery
OBE_Drug_Histories_FIRST_Surgery <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Surgery)[3] <- "Month_First_Surgery"

#filter for the patients that fit the Surgery criteria
Patient_first_Surgery <-  OBE_Drug_Histories_FIRST_Surgery %>% select(patient)
DANU_Measures_BMI <- Patient_first_Surgery %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Surgery, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Surgery start and months after Surgery start
BMI_before_Surgery <- DANU_Measures_BMI %>% group_by(patient) %>% filter( (Exact_Month < (Month_First_Surgery)-1) & (Exact_Month > (Month_First_Surgery)-13))
BMI_before_Surgery <- BMI_before_Surgery %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Surgery)[6] <- "Month_Prior"
names(BMI_before_Surgery)[7] <- "BMI_Prior"
BMI_before_Surgery <- BMI_before_Surgery %>% select(-c(claimed, Month_Yr))


BMI_after_Surgery <- DANU_Measures_BMI %>% group_by(patient) %>% filter((Exact_Month > (Month_First_Surgery+1))&(Exact_Month < (Month_First_Surgery)+13))
BMI_after_Surgery <- BMI_after_Surgery %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Surgery)[6] <- "Month_After"
names(BMI_after_Surgery)[7] <- "BMI_After"
BMI_after_Surgery <- BMI_after_Surgery %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Surgery_BEFORE_vs_AFTER <- BMI_before_Surgery %>% full_join(BMI_after_Surgery)
BMI_Surgery_BEFORE_vs_AFTER <- BMI_Surgery_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Surgery_BEFORE_vs_AFTER$BMI_Prior) # 42.84952
mean(BMI_Surgery_BEFORE_vs_AFTER$BMI_After) # 32.83885

median(BMI_Surgery_BEFORE_vs_AFTER$BMI_Prior) #  42.5
median(BMI_Surgery_BEFORE_vs_AFTER$BMI_After) #  32

write.csv(BMI_Surgery_BEFORE_vs_AFTER, "BMI_Surgery_BEFORE_vs_AFTER_closest_1to12months.csv")

# Antiobesity ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Antiobesity too zero, and Antiobesity to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('11',.), ~replace(., grepl('11', .), "Antiobesity")) %>%
  mutate_if(grepl('12',.), ~replace(., grepl('12', .), "Antiobesity")) %>%
  mutate_if(grepl('13',.), ~replace(., grepl('13', .), "Antiobesity")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiobesity",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Antiobesity
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Antiobesity
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Antiobesity
OBE_Drug_Histories_FIRST_Antiobesity <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Antiobesity)[3] <- "Month_First_Antiobesity"

#filter for the patients that fit the Antiobesity criteria
Patient_first_Antiobesity <-  OBE_Drug_Histories_FIRST_Antiobesity %>% select(patient)
DANU_Measures_BMI <- Patient_first_Antiobesity %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Antiobesity, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Antiobesity start and months after Antiobesity start
BMI_before_Antiobesity <- DANU_Measures_BMI %>% group_by(patient) %>% filter( (Exact_Month < (Month_First_Antiobesity)-1) & (Exact_Month > (Month_First_Antiobesity)-13))
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Antiobesity)[6] <- "Month_Prior"
names(BMI_before_Antiobesity)[7] <- "BMI_Prior"
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% select(-c(claimed, Month_Yr))


BMI_after_Antiobesity <- DANU_Measures_BMI %>% group_by(patient) %>% filter((Exact_Month > (Month_First_Antiobesity+1))&(Exact_Month < (Month_First_Antiobesity)+13))
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Antiobesity)[6] <- "Month_After"
names(BMI_after_Antiobesity)[7] <- "BMI_After"
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity %>% full_join(BMI_after_Antiobesity)
BMI_Antiobesity_BEFORE_vs_AFTER <- BMI_Antiobesity_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_Prior) # 35.96786
mean(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_After) # 34.52786

median(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_Prior) #  35
median(BMI_Antiobesity_BEFORE_vs_AFTER$BMI_After) # 33.5

write.csv(BMI_Antiobesity_BEFORE_vs_AFTER, "BMI_Antiobesity_BEFORE_vs_AFTER_closest_1to12months.csv")

# Anorectic ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no Anorectic too zero, and Anorectic to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "Anorectic")) %>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "Anorectic")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Anorectic",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON Anorectic
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Anorectic
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took Anorectic
OBE_Drug_Histories_FIRST_Anorectic <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_Anorectic)[3] <- "Month_First_Anorectic"

#filter for the patients that fit the Anorectic criteria
Patient_first_Anorectic <-  OBE_Drug_Histories_FIRST_Anorectic %>% select(patient)
DANU_Measures_BMI <- Patient_first_Anorectic %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_Anorectic, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before Anorectic start and months after Anorectic start
BMI_before_Anorectic <- DANU_Measures_BMI %>% group_by(patient) %>% filter( (Exact_Month < (Month_First_Anorectic)-1) & (Exact_Month > (Month_First_Anorectic)-13))
BMI_before_Anorectic <- BMI_before_Anorectic %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Anorectic)[6] <- "Month_Prior"
names(BMI_before_Anorectic)[7] <- "BMI_Prior"
BMI_before_Anorectic <- BMI_before_Anorectic %>% select(-c(claimed, Month_Yr))


BMI_after_Anorectic <- DANU_Measures_BMI %>% group_by(patient) %>% filter((Exact_Month > (Month_First_Anorectic+1))&(Exact_Month < (Month_First_Anorectic)+13))
BMI_after_Anorectic <- BMI_after_Anorectic %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Anorectic)[6] <- "Month_After"
names(BMI_after_Anorectic)[7] <- "BMI_After"
BMI_after_Anorectic <- BMI_after_Anorectic %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_Anorectic_BEFORE_vs_AFTER <- BMI_before_Anorectic %>% full_join(BMI_after_Anorectic)
BMI_Anorectic_BEFORE_vs_AFTER <- BMI_Anorectic_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_Anorectic_BEFORE_vs_AFTER$BMI_Prior) # 32.46612
mean(BMI_Anorectic_BEFORE_vs_AFTER$BMI_After) # 30.79771

median(BMI_Anorectic_BEFORE_vs_AFTER$BMI_Prior) #  31
median(BMI_Anorectic_BEFORE_vs_AFTER$BMI_After) #  29.5

write.csv(BMI_Anorectic_BEFORE_vs_AFTER, "BMI_Anorectic_BEFORE_vs_AFTER_closest_1to12months.csv")



# WeightLoss ---------------
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, claimed)

# Get exact month
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% mutate(Month_Yr = format(as.Date(claimed), "%Y-%m"))

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>% 
  filter(!is.na(Exact_Month))

# New weights
DANU_Demographics <- fread("DANU_Demographics_v2weights.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_3, diagnosis) %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
DANU_Demographics %>% group_by(diagnosis) %>% summarise(n=sum(weight_3, na.rm=T))

DANU_Events <- DANU_Events %>% left_join(DANU_Demographics) %>% filter(!is.na(weight_3)) %>%
  select(patid, weight_3, diagnosis, claimed, Month_Yr, Exact_Month, code)

# Minimum 3 months away from start

# read table in wide format from months 1 to 60
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(3:62)

# convert no WeightLoss too zero, and WeightLoss to one, then convert everything to numeric 
OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "WeightLoss"))

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="WeightLoss",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Drug_Histories_LONG <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% bind_cols(OBE_Drug_Histories)
rm(OBE_Drug_Histories_LONG)

#convert to long format
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those months ON WeightLoss
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to WeightLoss
OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Treat)

# When each patient first took WeightLoss
OBE_Drug_Histories_FIRST_WeightLoss <- OBE_Drug_Histories
names(OBE_Drug_Histories_FIRST_WeightLoss)[3] <- "Month_First_WeightLoss"

#filter for the patients that fit the WeightLoss criteria
Patient_first_WeightLoss <-  OBE_Drug_Histories_FIRST_WeightLoss %>% select(patient)
DANU_Measures_BMI <- Patient_first_WeightLoss %>% left_join(DANU_Events, by=c("patient"="patid"))

#remove those ptients with no BMI level readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% filter(!is.na(weight_3))

#join the patient first GLP1 Oral month to his HbA1c readings
DANU_Measures_BMI <- DANU_Measures_BMI %>% left_join(OBE_Drug_Histories_FIRST_WeightLoss, by = c("patient" = "patient"))
DANU_Measures_BMI <- DANU_Measures_BMI %>% select(-weight)
names(DANU_Measures_BMI)[2] <- "weight"

# now split into months before WeightLoss start and months after WeightLoss start
BMI_before_WeightLoss <- DANU_Measures_BMI %>% group_by(patient) %>% filter( (Exact_Month < (Month_First_WeightLoss)-1) & (Exact_Month > (Month_First_WeightLoss)-13))
BMI_before_WeightLoss <- BMI_before_WeightLoss %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_WeightLoss)[6] <- "Month_Prior"
names(BMI_before_WeightLoss)[7] <- "BMI_Prior"
BMI_before_WeightLoss <- BMI_before_WeightLoss %>% select(-c(claimed, Month_Yr))


BMI_after_WeightLoss <- DANU_Measures_BMI %>% group_by(patient) %>% filter((Exact_Month > (Month_First_WeightLoss+1))&(Exact_Month < (Month_First_WeightLoss)+13))
BMI_after_WeightLoss <- BMI_after_WeightLoss %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_WeightLoss)[6] <- "Month_After"
names(BMI_after_WeightLoss)[7] <- "BMI_After"
BMI_after_WeightLoss <- BMI_after_WeightLoss %>% select(-c(claimed, Month_Yr))

#join the before and after BMI
BMI_WeightLoss_BEFORE_vs_AFTER <- BMI_before_WeightLoss %>% full_join(BMI_after_WeightLoss)
BMI_WeightLoss_BEFORE_vs_AFTER <- BMI_WeightLoss_BEFORE_vs_AFTER %>% na.omit()

mean(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_Prior) # 36.97826
mean(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_After) # 35.36957

median(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_Prior) #  35
median(BMI_WeightLoss_BEFORE_vs_AFTER$BMI_After) # 33.5

write.csv(BMI_WeightLoss_BEFORE_vs_AFTER, "BMI_WeightLoss_BEFORE_vs_AFTER_closest_1to12months.csv")

# Summary evolution over time -------
BMI_Evolution_US_All_1to12month <- fread("BMI_Evolution_US_All_1to12month.csv")

BMI_Evolution_US_All_1to12month %>% group_by(Therapy, Period) %>% summarise(n=mean(BMI))
Therapy           Period     n
<chr>             <chr>  <dbl>
  1 Amphetamines      After   30.8
2 Amphetamines      Before  32.5
3 GLP1 Injectable   After   35.1
4 GLP1 Injectable   Before  37.2
5 GLP1 Oral         After   35.0
6 GLP1 Oral         Before  36.4
7 Lipase Inhibitors After   35.4
8 Lipase Inhibitors Before  37.0
9 Naltrexone        After   34.5
10 Naltrexone        Before  36.0
11 Surgery           After   32.8
12 Surgery           Before  42.8

BMI_Evolution_US_All_1to12month %>% group_by(Therapy, Period) %>% summarise(n=median(BMI))

Groups:   Therapy [6]
Therapy           Period     n
<chr>             <chr>  <dbl>
  1 Amphetamines      After   29.5
2 Amphetamines      Before  31  
3 GLP1 Injectable   After   34  
4 GLP1 Injectable   Before  36  
5 GLP1 Oral         After   34.5
6 GLP1 Oral         Before  36.5
7 Lipase Inhibitors After   33.5
8 Lipase Inhibitors Before  35  
9 Naltrexone        After   33.5
10 Naltrexone        Before  35  
11 Surgery           After   32  
12 Surgery           Before  42.5


Pats_to_keep_paired <- BMI_Evolution_US_All_1to12month %>% group_by(patient, Therapy) %>% filter(Month>=-12 & Month<=12) %>% 
  count() %>% filter(n%%2==0) %>% select(patient, Therapy)

BMI_Evolution_US_All_1to12month <- Pats_to_keep_paired %>% left_join(BMI_Evolution_US_All_1to12month)

BMI_Evolution_US_All_1to12month %>% 
  group_by(Therapy, Month) %>%  
  ggplot(aes(x=Month, y=BMI, fill=Therapy, colour=Therapy))+
  geom_smooth(size=2.5, method = "loess", se=F )+
  ylab("BMI kg/m2\n")+
  xlab("\nMonth")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_viridis_d()+
  ggsci::scale_colour_jama()


# ---------
# Number patients per physician ----------
OBE_US_Doses <- fread("OBE Doses.txt")

temp_1 <- OBE_US_Doses %>% select(prov, pat_id) %>% group_by(prov, pat_id) %>% distinct() %>% ungroup() %>%
  group_by(prov) %>% count() %>% ungroup() %>% filter(prov != "" & prov != "0") %>%
  group_by(n) %>% count()

sum(temp_1$nn) # 77423 unique physicians 

temp_1 %>% ggplot(aes(n, nn)) + 
  geom_col(fill="midnightblue")+
  xlim(0,25)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  geom_text(aes(label = paste0(round(nn/1000,0),"k")), vjust = -0.5)+
  xlab("\n Number of Patients per Physician")+ylab("Number of Physicians\n")


# -------
# Scripts per Specialty -------------
OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
OBE_US_Doses <- OBE_US_Doses %>% mutate(from_dt = as.Date(from_dt))
OBE_US_Doses <- OBE_US_Doses %>%filter(from_dt >= "2016-05-01" & from_dt <= "2021-04-30") 


PROVCAT <- fread("PROVCAT.txt", sep="|")
PROVCAT$PROVCAT <- sub("^0+", "", PROVCAT$PROVCAT)  
PROVCAT <- PROVCAT %>% mutate(PROVCAT = ifelse(PROVCAT=="","0",PROVCAT))
PROVCAT$PROVCAT <- as.numeric(PROVCAT$PROVCAT)

Specialties_to_keep <- OBE_US_Doses %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(PROVCAT, by=c("specialty"="PROVCAT"))

fwrite(Specialties_to_keep, "Specialties_to_keep.txt", sep="\t")

Specialties_to_keep <- fread("Specialties_to_keep.txt")

data.frame(OBE_US_Doses %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))

# PHYSICIAN     n2
# 1                      PCP 336873
# 2               PSYCHIATRY 279480
# 3                OTHER HCP 147561
# 4        INTERNAL MEDICINE 107304
# 5          OTHER PHYSICIAN  87138
# 6                  SURGERY  11253
# 7  OBSTETRICS & GYNECOLOGY   9404
# 8       EMERGENCY MEDICINE   7466
# 9            ENDOCRINOLOGY   6878
# 10             PULMONOLOGY   6132
# 11          ANESTHESIOLOGY   5098
# 12      GERIATRIC MEDICINE   2161
# 13              CARDIOLOGY   1624
# 14            RHEUMATOLOGY   1502
# 15                 UNKNOWN   1208
# 16                ONCOLOGY   1072
# 17            INFECTIOLOGY   1060
# 18        GASTROENTEROLOGY   1014
# 19              NEPHROLOGY    577
# 20          INTENSIVE CARE    490
# 21              IMMUNOLOGY    488
# 22               RADIOLOGY    468
# 23                 UROLOGY    465
# 24              GENETICIST    166
# 25             DERMATOLOGY    115

OBE_US_Doses %>% filter(drug_class == "Weight Loss") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")

PHYSICIAN                  n2
<chr>                   <int>
  1 PCP                       220
2 INTERNAL MEDICINE         127
3 OTHER HCP                  83
4 ENDOCRINOLOGY              21
5 OTHER PHYSICIAN            15
6 PSYCHIATRY                 14
7 GASTROENTEROLOGY            8
8 OBSTETRICS & GYNECOLOGY     8
9 DERMATOLOGY                 5
10 EMERGENCY MEDICINE          4
11 ANESTHESIOLOGY              2
12 PULMONOLOGY                 2
13 SURGERY                     2

data.frame(OBE_US_Doses %>% filter(drug_class == "Anorectic") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))

PHYSICIAN     n2
1                      PCP 323171
2               PSYCHIATRY 278953
3                OTHER HCP 136148
4        INTERNAL MEDICINE 101181
5          OTHER PHYSICIAN  86205
6  OBSTETRICS & GYNECOLOGY   7766
7       EMERGENCY MEDICINE   7190
8              PULMONOLOGY   6053
9            ENDOCRINOLOGY   4230
10                 SURGERY   3587
11      GERIATRIC MEDICINE   2038
12          ANESTHESIOLOGY   1944
13            RHEUMATOLOGY   1458
14              CARDIOLOGY   1337
15            INFECTIOLOGY   1027
16                ONCOLOGY    911
17        GASTROENTEROLOGY    752
18              NEPHROLOGY    528
19          INTENSIVE CARE    488
20              IMMUNOLOGY    456
21               RADIOLOGY    402
22              GENETICIST    166
23                 UROLOGY    133
24                 UNKNOWN    129
25             DERMATOLOGY    102

data.frame(OBE_US_Doses %>% filter(drug_class == "Antiobesity") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))

PHYSICIAN   n2
1                      PCP 8903
2                OTHER HCP 5190
3        INTERNAL MEDICINE 3489
4            ENDOCRINOLOGY  995
5  OBSTETRICS & GYNECOLOGY  974
6          OTHER PHYSICIAN  493
7               PSYCHIATRY  483
8                  SURGERY  241
9               CARDIOLOGY  158
10        GASTROENTEROLOGY  138
11      EMERGENCY MEDICINE  126
12      GERIATRIC MEDICINE   92
13          ANESTHESIOLOGY   86
14             PULMONOLOGY   45
15              NEPHROLOGY   42
16            RHEUMATOLOGY   37
17               RADIOLOGY   19
18            INFECTIOLOGY   11
19                ONCOLOGY   11
20                 UROLOGY    7
21             DERMATOLOGY    3
22              IMMUNOLOGY    1

OBE_US_Doses %>% filter(drug_class == "GLP1 Oral") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")

PHYSICIAN                  n2
<chr>                   <int>
  1 PCP                       310
2 OTHER HCP                 281
3 INTERNAL MEDICINE         162
4 ENDOCRINOLOGY             147
5 OBSTETRICS & GYNECOLOGY   136
6 SURGERY                    35
7 EMERGENCY MEDICINE         13
8 GERIATRIC MEDICINE         13
9 OTHER PHYSICIAN             7

data.frame(OBE_US_Doses %>% filter(drug_class == "GLP1 Injectable") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))


PHYSICIAN   n2
1                      PCP 4146
2                OTHER HCP 3594
3        INTERNAL MEDICINE 2273
4            ENDOCRINOLOGY 1480
5  OBSTETRICS & GYNECOLOGY  377
6                  SURGERY  275
7          OTHER PHYSICIAN  267
8               CARDIOLOGY  125
9       EMERGENCY MEDICINE   96
10        GASTROENTEROLOGY   86
11             PULMONOLOGY   31
12              IMMUNOLOGY   28
13          ANESTHESIOLOGY   27
14            INFECTIOLOGY   21
15      GERIATRIC MEDICINE   18
16              PSYCHIATRY   16
17            RHEUMATOLOGY    7
18             DERMATOLOGY    4
19              NEPHROLOGY    4
20                ONCOLOGY    4
21               RADIOLOGY    2

data.frame(OBE_US_Doses %>% filter(drug_class == "Surgery") %>% group_by(specialty) %>% summarise(n=n()) %>%
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY"))

PHYSICIAN   n2
1                  SURGERY 7113
2           ANESTHESIOLOGY 3039
3                OTHER HCP 2265
4                  UNKNOWN 1079
5                  UROLOGY  325
6          OTHER PHYSICIAN  151
7                 ONCOLOGY  146
8  OBSTETRICS & GYNECOLOGY  143
9                      PCP  123
10       INTERNAL MEDICINE   72
11               RADIOLOGY   45
12      EMERGENCY MEDICINE   37
13        GASTROENTEROLOGY   30
14              PSYCHIATRY   14
15           ENDOCRINOLOGY    5
16              CARDIOLOGY    4
17              IMMUNOLOGY    3
18              NEPHROLOGY    3
19          INTENSIVE CARE    2
20             DERMATOLOGY    1
21            INFECTIOLOGY    1
22             PULMONOLOGY    1



OBE_US_Doses %>% filter(drug_class == "GLP1 Oral") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")

PHYSICIAN                  n2
<chr>                   <int>
  1 PCP                        53
2 OTHER HCP                  50
3 OBSTETRICS & GYNECOLOGY    32
4 INTERNAL MEDICINE          30
5 ENDOCRINOLOGY              27
6 SURGERY                     4
7 EMERGENCY MEDICINE          3
8 GERIATRIC MEDICINE          3
9 OTHER PHYSICIAN             1



OBE_US_Doses %>% filter(drug_class == "GLP1 Injectable") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty"))%>%
  ungroup() %>% group_by(PHYSICIAN) %>% summarise(n2=sum(n)) %>% arrange(-n2) %>% filter(PHYSICIAN != "FACILITY")

# ----
# Charlson comorbidty index in treatment exp vs no treatment exp ---------------
OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(4:63)

OBE_Drug_Histories[OBE_Drug_Histories != "-"] <- 1  # on drug 
OBE_Drug_Histories[OBE_Drug_Histories == "-"] <- 0  # no drug

OBE_Drug_Histories[] <- lapply(OBE_Drug_Histories,as.numeric)

OBE_Drug_Histories$SUM <- rowSums(OBE_Drug_Histories)

OBE_Drug_Historiess_LONG     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Pats_vec <- OBE_Drug_Historiess_LONG %>% select(patient, weight)

OBE_Drug_Histories <- Pats_vec %>% bind_cols(OBE_Drug_Histories)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(SUM != 0)

sum(OBE_Drug_Histories$weight) # 9155116

Treatment_exp_Vector <- OBE_Drug_Histories %>% select(patient, weight)

Treatment_exp_Vector <- Treatment_exp_Vector %>% mutate(Treat_exp = "Experienced")

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Treatment_exp_Vector <- OBE_Drug_Histories %>% select(patient, weight) %>% left_join(Treatment_exp_Vector) %>%
  mutate(Treat_exp=ifelse(is.na(Treat_exp), "None", Treat_exp))

# Charlson
OBE_Disorder_Histories <- fread("OBE Disorder Histories.txt")
OBE_Disorder_Histories <- OBE_Disorder_Histories %>% select(patient, month60)

OBE_Disorder_Histories <- Treatment_exp_Vector %>% left_join(OBE_Disorder_Histories)

OBE_Disorder_Histories$month60 <- as.character(OBE_Disorder_Histories$month60)
OBE_Disorder_Histories$score <- parse_number(OBE_Disorder_Histories$month60)

OBE_Disorder_Histories %>% group_by(Treat_exp) %>% summarise(n=weighted.mean(score, weight))

# Treat_exp       n
# <chr>       <dbl>
# 1 Experienced  1.25
# 2 None         2.07

OBE_Disorder_Histories %>% ggplot(aes(score, colour=Treat_exp, fill=Treat_exp)) +
  geom_density() + 
  facet_wrap(~Treat_exp, ncol = 1)+
  theme(panel.grid.major=element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())+
  ggsci::scale_fill_nejm()+
  ggsci::scale_colour_nejm()+
  xlab("\nLast observed Charlson Comorbidity Index")+ ylab("Proportion of patients \n")


OBE_Disorder_Histories <- OBE_Disorder_Histories %>% mutate(comorbs = str_extract(month60, "[a-z]+"))


OBE_Disorder_Histories <- OBE_Disorder_Histories %>% mutate(comorbs = ifelse(is.na(comorbs), "None", comorbs))

OBE_Disorder_Histories

OBE_Disorder_Histories_sep <- separate_rows(OBE_Disorder_Histories, comorbs, sep = "", convert=F )

OBE_Disorder_Histories_sep <- OBE_Disorder_Histories_sep %>% filter(comorbs != "")

OBE_Disorder_Histories_sep <- OBE_Disorder_Histories %>% select(patient, weight, Treat_exp, month60, score) %>% left_join(OBE_Disorder_Histories_sep) 
 
OBE_Disorder_Histories %>% group_by(Treat_exp) %>% summarise(n=sum(weight))

# Treat_exp           n
# <chr>           <dbl>
# 1 Experienced  9155116.
# 2 None        97313933.


OBE_Disorder_Histories_sep %>% group_by(Treat_exp, comorbs) %>% 
  summarise(n=sum(weight)) %>% spread(key=Treat_exp, value=n) %>%
  mutate(Experienced = Experienced*100/9155116.) %>%
  mutate(None = None*100/97313933.)


# comorbs Experienced   None
# <chr>         <dbl>  <dbl>
# 1 a             1.40   3.27 
# 2 c             4.31   7.35 
# 3 d             0.334  0.257
# 4 f             2.59   5.21 
# 5 k             2.63   5.52 
# 6 l            10.5    9.55 
# 7 p            29.5   25.4  
# 8 r             4.09   3.57 
# 9 s             4.21   7.68 
# 10 u             2.78   2.16 
# 11 v             4.47   9.44 
# 12 z             1.33   1.85 
# 13 NA           54.7   53.5


# -----------
# All Obesity Events - Physicians -------------------------------
DANU_Events <- fread("DANU Events.txt")
DANU_Event_Claims_Providers <- fread("DANU Event Claims Providers.txt")

Unique_Physicians_OBE <- fread("Unique_Physicians_OBE.txt")
DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")



# Physicians All Events Obe Treat Exp
Treatment_exp_Vector %>% filter(Treat_exp=="Experienced") %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

# SUMMARY_SPECIALTY       n
# <chr>               <int>
#   1 NA                 513050
# 2 GP                 131132
# 3 INTERNAL MEDICINE   61837
# 4 SURGERY             61367
# 5 OTHER HCP           61316
# 6 OTHER PHYSICIAN     55991
# 7 FACILITY            41605
# 8 UNKNOWN             37903
# 9 CARDIOLOGY           9879
# 10 EMERGENCY MEDICINE   7645
# 11 ENDOCRINOLOGY        7396
# 12 PATHOLOGY            7030
# 13 GASTRO/HEPATO        5563
# 14 HEMATO/ONCOL         4667
# 15 RADIOLOGY            3953
# 16 PSYCHIATRY           3650
# 17 NEUROLOGY            2748
# 18 NEPHROLOGIST         1039
# 19 MEDICAL ONCOLOGIST    661

# Physicians All Events Obe Naive
Treatment_exp_Vector %>% filter(Treat_exp=="None") %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

# SUMMARY_SPECIALTY        n
# <chr>                <int>
#   1 NA                 4847073
# 2 GP                  757010
# 3 INTERNAL MEDICINE   454749
# 4 OTHER PHYSICIAN     331613
# 5 OTHER HCP           245596
# 6 FACILITY            202425
# 7 UNKNOWN             170304
# 8 SURGERY             120837
# 9 CARDIOLOGY           87766
# 10 EMERGENCY MEDICINE   38145
# 11 GASTRO/HEPATO        32309
# 12 ENDOCRINOLOGY        31904
# 13 HEMATO/ONCOL         18087
# 14 PATHOLOGY            17680
# 15 NEUROLOGY            15270
# 16 PSYCHIATRY           11061
# 17 NEPHROLOGIST          9455
# 18 RADIOLOGY             8261
# 19 MEDICAL ONCOLOGIST    5953


# Physicians First OBE Dx Treat-exp
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, diagnosis)

Treatment_exp_Vector %>% filter(Treat_exp=="Experienced") %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(patient, code, prov) %>% left_join(DANU_Diagnosis_Codes) %>%
  filter(diagnosis == "Obesity") %>% group_by(patient) %>% slice(1) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

# SUMMARY_SPECIALTY      n
# <chr>              <int>
# 1 NA                 27995
# 2 GP                 11186
# 3 OTHER PHYSICIAN     4764
# 4 INTERNAL MEDICINE   4758
# 5 OTHER HCP           3333
# 6 SURGERY             1704
# 7 UNKNOWN             1465
# 8 FACILITY            1396
# 9 CARDIOLOGY           522
# 10 EMERGENCY MEDICINE   503
# 11 ENDOCRINOLOGY        472
# 12 GASTRO/HEPATO        368
# 13 NEUROLOGY            207
# 14 PSYCHIATRY           205
# 15 PATHOLOGY            171
# 16 RADIOLOGY             98
# 17 HEMATO/ONCOL          82
# 18 NEPHROLOGIST          69
# 19 MEDICAL ONCOLOGIST    40



# Physicians First OBE Dx Naive
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, diagnosis)

Treatment_exp_Vector %>% filter(Treat_exp=="None") %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(patient, code, prov) %>% left_join(DANU_Diagnosis_Codes) %>%
  filter(diagnosis == "Obesity") %>% group_by(patient) %>% slice(1) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)
# 
# SUMMARY_SPECIALTY       n
# <chr>               <int>
# 1 NA                 389756
# 2 GP                  97743
# 3 INTERNAL MEDICINE   53795
# 4 OTHER PHYSICIAN     38544
# 5 OTHER HCP           26759
# 6 UNKNOWN             15184
# 7 FACILITY            12718
# 8 SURGERY              9510
# 9 CARDIOLOGY           8038
# 10 EMERGENCY MEDICINE   4051
# 11 GASTRO/HEPATO        4004
# 12 ENDOCRINOLOGY        2674
# 13 NEUROLOGY            1514
# 14 PATHOLOGY            1192
# 15 HEMATO/ONCOL         1066
# 16 NEPHROLOGIST          896
# 17 PSYCHIATRY            759
# 18 RADIOLOGY             602
# 19 MEDICAL ONCOLOGIST    443
# ------------------




# Number of events, sex and age: Treat-exp vs Naive ---------------------
OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(4:63)

OBE_Drug_Histories[OBE_Drug_Histories != "-"] <- 1  # on drug 
OBE_Drug_Histories[OBE_Drug_Histories == "-"] <- 0  # no drug

OBE_Drug_Histories[] <- lapply(OBE_Drug_Histories,as.numeric)

OBE_Drug_Histories$SUM <- rowSums(OBE_Drug_Histories)

OBE_Drug_Historiess_LONG     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Pats_vec <- OBE_Drug_Historiess_LONG %>% select(patient, weight)

OBE_Drug_Histories <- Pats_vec %>% bind_cols(OBE_Drug_Histories)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(SUM != 0)

sum(OBE_Drug_Histories$weight) # 9155116

Treatment_exp_Vector <- OBE_Drug_Histories %>% select(patient, weight)

Treatment_exp_Vector <- Treatment_exp_Vector %>% mutate(Treat_exp = "Experienced")

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Treatment_exp_Vector <- OBE_Drug_Histories %>% select(patient, weight) %>% left_join(Treatment_exp_Vector) %>%
  mutate(Treat_exp=ifelse(is.na(Treat_exp), "None", Treat_exp))

DANU_Events <- fread("DANU Events.txt")

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(grepl("Obesity",diagnosis)) %>% filter(grepl("Diagnosis", source))
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code)
DANU_Events <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% group_by(patid) %>% count()

Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(DANU_Events, by=c("patient"="patid"))

Treatment_exp_Vector %>% filter(Treat_exp=="Experienced") %>% summarise(n=weighted.mean(n, weight, na.rm=T)) #10.74444
Treatment_exp_Vector %>% filter(Treat_exp=="None") %>% summarise(n=weighted.mean(n, weight, na.rm=T)) #6.15583


Treatment_exp_Vector %>% ggplot(aes(n, colour=Treat_exp, fill=Treat_exp)) +
  geom_density() + 
  facet_wrap(~Treat_exp, ncol = 1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_fill_nejm()+
  ggsci::scale_colour_nejm()+
  xlim(0,20)+
  xlab("\nNumber of Dxs/Events")+ ylab("Proportion of patients \n")


Treatment_exp_Vector <- Treatment_exp_Vector %>% select(-n)

DANU_Demographics <- fread("DANU Demographics.txt")

DANU_Demographics <- DANU_Demographics %>% select(patid, gender, age)

Treatment_exp_Vector <- Treatment_exp_Vector %>% left_join(DANU_Demographics, by=c("patient"="patid"))

Treatment_exp_Vector %>% group_by(Treat_exp) %>% summarise(n=weighted.mean(age, weight))

Treat_exp       n
<chr>       <dbl>
  1 Experienced  40.4
2 None         49.9




Treatment_exp_Vector %>%  ggplot(aes(age, colour=Treat_exp, fill=Treat_exp)) +
  geom_density() + 
  facet_wrap(~Treat_exp, ncol = 1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_fill_nejm()+
  ggsci::scale_colour_nejm()+
  xlab("\nAge")+ ylab("Proportion of patients \n")



Treatment_exp_Vector %>% group_by(Treat_exp, gender) %>% summarise(n=sum(weight))

 
# ------------
# Develop a classification method for GLP1 Experience or Amphetamines ---------------


DANU_Ingredients <- fread("DANU Ingredients.txt")

# Cumulative drug class experience every month 
OBE_Flows_Aux._Long <- fread("OBE_Flows_Aux._Long_v2.txt", integer64 = "character", stringsAsFactors = F)
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(-c(disease, starts, stops, re_starts))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(-c(s1, s2, p1_RxExp, flow, weight))


OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1_OralExp = ifelse(grepl("47",d1)|grepl("47",d2),1,0))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = cumsum(p1_OralExp))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = ifelse(p1_OralExp==0,0,1))



OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1_InjExp = ifelse(grepl("48",d1)|grepl("49",d1)|grepl("50",d1)|
                                                                           grepl("51",d1)|grepl("52",d1)|grepl("53",d1)|grepl("48",d2)|
                                                                           grepl("49",d2)|grepl("50",d2)|grepl("51",d2)|grepl("52",d2)|grepl("53",d2),1,0))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = cumsum(p1_InjExp))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = ifelse(p1_InjExp==0,0,1))


OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1_WeightLossExp = ifelse(grepl("(^|\\D)(3{1})(\\D|$)",d1)|grepl("(^|\\D)(3{1})(\\D|$)",d2),1,0))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_WeightLossExp = cumsum(p1_WeightLossExp))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_WeightLossExp = ifelse(p1_WeightLossExp==0,0,1))


OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1_AnorecticExp = ifelse(grepl("(^|\\D)(4{1})(\\D|$)",d1)|grepl("(^|\\D)(4{1})(\\D|$)",d2)|
                                                                                 grepl("(^|\\D)(5{1})(\\D|$)",d1)|grepl("(^|\\D)(5{1})(\\D|$)",d2)|
                                                                                 grepl("(^|\\D)(6{1})(\\D|$)",d1)|grepl("(^|\\D)(6{1})(\\D|$)",d2)|
                                                                                 grepl("(^|\\D)(7{1})(\\D|$)",d1)|grepl("(^|\\D)(7{1})(\\D|$)",d2)|
                                                                                 grepl("(^|\\D)(8{1})(\\D|$)",d1)|grepl("(^|\\D)(8{1})(\\D|$)",d2)|
                                                                                 grepl("(^|\\D)(9{1})(\\D|$)",d1)|grepl("(^|\\D)(9{1})(\\D|$)",d2)|
                                                                                 grepl("10",d1)|grepl("10",d2),1,0))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AnorecticExp = cumsum(p1_AnorecticExp))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AnorecticExp = ifelse(p1_AnorecticExp==0,0,1))



OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1_AntiobesityExp = ifelse(grepl("11",d1)|grepl("11",d2)|
                                                                                   grepl("12",d1)|grepl("12",d2)|
                                                                                   grepl("13",d1)|grepl("13",d2),1,0))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiobesityExp = cumsum(p1_AntiobesityExp))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiobesityExp = ifelse(p1_AntiobesityExp==0,0,1))




OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(-c(d1, d2))

fwrite(OBE_Flows_Aux._Long, "Cum_Class_Experience_EveryMonth.txt", sep="\t")






# Get MAX HbA1c & MAX BMI & GENDER & MAX AGE & Comorbidity status per patient
# Demographics
DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
names(DANU_Demographics)[1] <- "patient"

DANU_Demographics <- DANU_Demographics %>% select(patient, age, gender, diagnosis)






#MAX HbA1c & BMI
DANU_Measures <- fread("DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
names(DANU_Measures)[1] <- "patient"
DANU_Measures <-DANU_Measures %>% select(patient, test, value)

MAX_HbA1c <- DANU_Measures %>% filter(test=="HbA1c Level") %>%
  group_by(patient) %>% filter(value==max(value)) %>% slice(1) %>% ungroup()
names(MAX_HbA1c)[3] <- "MAX_HbA1c"
MAX_HbA1c <- MAX_HbA1c[,c(1,3)]

MAX_BMI <- DANU_Measures %>% filter(test=="BMI") %>%
  group_by(patient) %>% filter(value==max(value)) %>% slice(1) %>% ungroup()
names(MAX_BMI)[3] <- "MAX_BMI"
MAX_BMI <- MAX_BMI[,c(1,3)]

DANU_Demographics <- DANU_Demographics %>% left_join(MAX_HbA1c) %>% left_join(MAX_BMI)





#MAX AST & ALT
# From NASH DANU Meaures 3.1
DANU_Measures <- fread("DANU Measures.txt", integer64 = "character", stringsAsFactors = F)

DANU_Measures <- DANU_Measures %>% filter(test=="AST Level"|test=="ALT Level")
DANU_Measures <- DANU_Measures %>% select(patid, test, value)
names(DANU_Measures)[1] <- "patient"

MAX_AST <- DANU_Measures %>% filter(test=="AST Level") %>%
  group_by(patient) %>% filter(value==max(value)) %>% slice(1) %>% ungroup()
names(MAX_AST)[3] <- "MAX_AST"
MAX_AST <- MAX_AST[,c(1,3)]

MAX_ALT <- DANU_Measures %>% filter(test=="ALT Level") %>%
  group_by(patient) %>% filter(value==max(value)) %>% slice(1) %>% ungroup()
names(MAX_ALT)[3] <- "MAX_ALT"
MAX_ALT <- MAX_ALT[,c(1,3)]

DANU_Demographics <- DANU_Demographics %>% left_join(MAX_AST) %>% left_join(MAX_ALT)

fwrite(DANU_Demographics, "MAX_Labs_Demographics.txt", sep="\t")









# Number of lines and number of drugs and number of flows 

# Flows
Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")

OBE_Flows_Aux._Long <- fread("OBE_Flows_Aux._Long_v2.txt", integer64 = "character", stringsAsFactors = F)
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(-c(disease, starts, stops, re_starts))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(-c(s1, s2, p1_RxExp, weight))
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% group_by(patient) %>% mutate(cumflow = cumsum(flow))

Cummulative_Flows <- OBE_Flows_Aux._Long  %>% select(patient, p1, p2, cumflow)

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% left_join(Cummulative_Flows)

fwrite(Cum_Class_Experience_EveryMonth, "Cum_Class_Experience_EveryMonth.txt", sep="\t")









# Lines
drgOBE2 <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

data <- data.frame(drgOBE2, stringsAsFactors = F)

nrLines <- data[,c(1:3)] 
nrLines$month1 <- (data$month1 != "-")*1

for(i in 2:60){
  cat(i)
  nrLines[,i+3] <- apply(data[,(4:(i+3))], 1, function(x) length(unique(x[x!="-"])))
  names(nrLines)[i+3] <- paste0("month",i)
}

fwrite(nrLines,"OBE_nrLines_Histories.txt")

nrLines <- gather(nrLines, Month, Lines, month1:month60, factor_key=TRUE)

nrLines <- nrLines %>% select(patient, Month, Lines)

nrLines$Month <- as.character(nrLines$Month)
nrLines$Month <- parse_number(nrLines$Month)

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% left_join(nrLines, by=c("patient"="patient", "p2"="Month"))

fwrite(Cum_Class_Experience_EveryMonth, "Cum_Class_Experience_EveryMonth.txt", sep="\t")










# Nr drugs cumm

OBE_Drug_Histories <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-disease)
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-weight)

OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Drugs != "-")

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-Month)

OBE_Drug_Histories <- OBE_Drug_Histories %>% distinct()
OBE_Drug_Histories <- separate_rows(OBE_Drug_Histories, Drugs, sep = ",", convert=T )

OBE_Drug_Histories <- OBE_Drug_Histories %>% group_by(patient) %>% count()

names(OBE_Drug_Histories)[2] <- "Diff_Drugs_exp"


fwrite(OBE_Drug_Histories, "Number_Drugs_EverExperienced.txt", sep="\t")





# Physicians Experience
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient)

OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
OBE_US_Doses <- OBE_US_Doses %>% mutate(from_dt = as.Date(from_dt))
OBE_US_Doses <- OBE_US_Doses %>%filter(from_dt >= "2016-05-01" & from_dt <= "2021-04-30") 
names(OBE_US_Doses)[4] <- "patient"


OBE_US_Doses <- OBE_Drug_Histories %>% left_join(OBE_US_Doses)

PROVCAT <- fread("PROVCAT.txt", sep="|")
PROVCAT$PROVCAT <- sub("^0+", "", PROVCAT$PROVCAT)  
PROVCAT <- PROVCAT %>% mutate(PROVCAT = ifelse(PROVCAT=="","0",PROVCAT))
PROVCAT$PROVCAT <- as.numeric(PROVCAT$PROVCAT)

Specialties_to_keep <- fread("Specialties_to_keep.txt")

temp <- OBE_US_Doses %>% select(patient, specialty) %>% distinct() %>% 
  left_join(Specialties_to_keep %>% select(specialty, PHYSICIAN), by=c("specialty"="specialty")) %>%
  select(patient, PHYSICIAN) %>% distinct() %>% mutate(Physician_Exp = 1) %>% 
  spread(key=PHYSICIAN, value=Physician_Exp) %>% select(-c(28))

length(unique(temp$patient))

temp[is.na(temp)] <- 0

fwrite(temp, "Physicians_Experience.txt", sep="\t")





# Treatment-experienced
OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(4:63)

OBE_Drug_Histories[OBE_Drug_Histories != "-"] <- 1  # on drug 
OBE_Drug_Histories[OBE_Drug_Histories == "-"] <- 0  # no drug

OBE_Drug_Histories[] <- lapply(OBE_Drug_Histories,as.numeric)

OBE_Drug_Histories$SUM <- rowSums(OBE_Drug_Histories)

OBE_Drug_Histories_LONG     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Pats_vec <- OBE_Drug_Histories_LONG %>% select(patient, weight)

OBE_Drug_Histories <- Pats_vec %>% bind_cols(OBE_Drug_Histories)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(SUM != 0)

sum(OBE_Drug_Histories$weight)

Treatment_exp_Vector <- OBE_Drug_Histories %>% select(patient, weight)

fwrite(Treatment_exp_Vector, "Treatment_exp_Vector.txt")






# Charlson
OBE_Disorder_Histories <- fread("OBE Disorder Histories.txt")
OBE_Disorder_Histories <- OBE_Disorder_Histories %>% select(patient, month60)

OBE_Disorder_Histories <- OBE_Disorder_Histories %>% mutate(comorbs = str_extract(month60, "[a-z]+"))

OBE_Disorder_Histories <- OBE_Disorder_Histories %>% filter(!is.na(comorbs))

OBE_Disorder_Histories

OBE_Disorder_Histories_sep <- separate_rows(OBE_Disorder_Histories, comorbs, sep = "", convert=F )

OBE_Disorder_Histories_sep <- OBE_Disorder_Histories_sep %>% filter(comorbs != "")

OBE_Disorder_Histories_sep <- OBE_Disorder_Histories_sep %>% select(-c(month60))

OBE_Disorder_Histories_sep <- OBE_Disorder_Histories_sep %>% spread(key=comorbs, value=comorbs)

OBE_Disorder_Histories_sep <- OBE_Disorder_Histories_sep %>% 
  mutate(a=ifelse(is.na(a),0,1)) %>%
  mutate(c=ifelse(is.na(c),0,1)) %>%
  mutate(d=ifelse(is.na(d),0,1)) %>%
  mutate(f=ifelse(is.na(f),0,1)) %>%
  mutate(k=ifelse(is.na(k),0,1)) %>%
  mutate(l=ifelse(is.na(l),0,1)) %>%
  mutate(p=ifelse(is.na(p),0,1)) %>%
  mutate(r=ifelse(is.na(r),0,1)) %>%
  mutate(s=ifelse(is.na(s),0,1)) %>%
  mutate(u=ifelse(is.na(u),0,1)) %>%
  mutate(v=ifelse(is.na(v),0,1)) %>%
  mutate(z=ifelse(is.na(z),0,1))

MAX_Labs_Demographics <- fread("MAX_Labs_Demographics.txt")

MAX_Labs_Demographics <- MAX_Labs_Demographics %>% left_join(OBE_Disorder_Histories_sep) %>%
  mutate(a=ifelse(is.na(a),0,a)) %>%
  mutate(c=ifelse(is.na(c),0,c)) %>%
  mutate(d=ifelse(is.na(d),0,d)) %>%
  mutate(f=ifelse(is.na(f),0,f)) %>%
  mutate(k=ifelse(is.na(k),0,k)) %>%
  mutate(l=ifelse(is.na(l),0,l)) %>%
  mutate(p=ifelse(is.na(p),0,p)) %>%
  mutate(r=ifelse(is.na(r),0,r)) %>%
  mutate(s=ifelse(is.na(s),0,s)) %>%
  mutate(u=ifelse(is.na(u),0,u)) %>%
  mutate(v=ifelse(is.na(v),0,v)) %>%
  mutate(z=ifelse(is.na(z),0,z))


fwrite(MAX_Labs_Demographics, "MAX_Labs_Demographics.txt", sep="\t")



# Logistic regression GLP1 Exp vs No GLP1

MAX_Labs_Demographics <- fread("MAX_Labs_Demographics.txt")

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% filter(p2==60) %>% select(-c(p1, p2))

Dems_Labs_TreatExp <- MAX_Labs_Demographics %>% inner_join(Cum_Class_Experience_EveryMonth)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% drop_na()

Number_Drugs_EverExperienced <- fread("Number_Drugs_EverExperienced.txt")

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% left_join(Number_Drugs_EverExperienced) %>% mutate(Diff_Drugs_exp = ifelse(is.na(Diff_Drugs_exp),0,Diff_Drugs_exp))

Physicians_Experience <- fread("Physicians_Experience.txt")

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% left_join(Physicians_Experience)

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

Dems_Labs_TreatExp <- Treatment_exp_Vector %>% inner_join(Dems_Labs_TreatExp)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(Group = ifelse(p1_InjExp==1, "GLP_Exp", "No_GLP1"))

Dems_Labs_TreatExp$Group <- as.factor(Dems_Labs_TreatExp$Group)

Dems_Labs_TreatExp$Group <- relevel(Dems_Labs_TreatExp$Group,"No_GLP1")


Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-c(p1_InjExp ))

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(gender = ifelse(gender=="M", 1, 0))


temp <- Dems_Labs_TreatExp

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-patient)



Dems_Labs_TreatExp %>% group_by(Group) %>% count()

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% group_by(Group) %>% sample_n(444)
Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% ungroup()


# Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(age=scale(age),
#                               MAX_HbA1c =scale(MAX_HbA1c ),
#                               MAX_BMI =scale(MAX_BMI ),
#                               MAX_ALT =scale(MAX_ALT ),
#                               MAX_AST =scale(MAX_AST ),
#                               age=scale(age))




create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}



Dems_Labs_TreatExp <- Dems_Labs_TreatExp[sample(1:nrow(Dems_Labs_TreatExp)), ]




Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(c(age, MAX_BMI, gender, p1_WeightLossExp, p1_AnorecticExp, p1_AntiobesityExp,
                                                      ENDOCRINOLOGY, CARDIOLOGY, PSYCHIATRY, Group, cumflow, Lines, Diff_Drugs_exp))


data_train <- create_train_test(Dems_Labs_TreatExp, 0.8, train = TRUE)
data_test <- create_train_test(Dems_Labs_TreatExp, 0.8, train = FALSE)

Risk_pred_model <- glm( Group ~ ., data = data_train, family = binomial)

summary(Risk_pred_model)



# Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)         0.094430   0.589619   0.160               0.8728    
# age                 0.007561   0.007424   1.018               0.3085    
# MAX_BMI             0.018053   0.009795   1.843               0.0653 .  
# gender             -0.399757   0.201668  -1.982               0.0475 *  
#   p1_WeightLossExp   14.138122 535.411279   0.026               0.9789    
# p1_AnorecticExp    -1.860180   0.198968  -9.349 < 0.0000000000000002 ***
#   p1_AntiobesityExp   0.597133   0.327695   1.822               0.0684 .  
# ENDOCRINOLOGY       2.017318   0.412021   4.896          0.000000977 ***
#   CARDIOLOGY          1.382351   0.774021   1.786               0.0741 .  
# PSYCHIATRY         -0.641787   0.301747  -2.127               0.0334 * 



predict <- predict(Risk_pred_model, data_test, type = 'response')

table_mat <- table(data_test$Group, predict > 0.50)
table_mat

plot(table_mat)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test


precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}



recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
prec # 0.7875

rec <- recall(table_mat)
rec # 0.7325581

f1 <- 2 * ((prec * rec) / (prec + rec))
f1 # 0.7590361



pred <- data.frame(predict(Risk_pred_model, temp, type = 'response')) %>% bind_cols(temp)

names(pred)[1] <- "Probability"




OBE_Flows_Aux._Long     <- fread("OBE_Flows_Aux._Long_v2.txt", integer64 = "character", stringsAsFactors = F)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(patient, s2) %>% filter(s2=="G") %>% distinct()

pred <- pred %>% left_join(OBE_Flows_Aux._Long) %>% mutate(s2=ifelse(is.na(s2),"No_GLP1", "GLP1_Exp"))
pred <- pred %>% select(-c(a,c,d,l,p,r,s,u,v,z))



Odd_ratios_GLP1_noGLP1_TreatExt <- fread("Odd_ratios_GLP1_noGLP1_TreatExt_short.txt", sep="\t")

Odd_ratios_GLP1_noGLP1_TreatExt$Predictor <- as.factor(Odd_ratios_GLP1_noGLP1_TreatExt$Predictor)

Odd_ratios_GLP1_noGLP1_TreatExt <- Odd_ratios_GLP1_noGLP1_TreatExt %>% arrange(`Odd ratio`)

Odd_ratios_GLP1_noGLP1_TreatExt <- Odd_ratios_GLP1_noGLP1_TreatExt %>% filter(`Sig. ?`!="ns")

Odd_ratios_GLP1_noGLP1_TreatExt %>%
  ggplot() +
  geom_segment( aes(x=reorder(Predictor, `Odd ratio`) , xend=reorder(Predictor, `Odd ratio`), y=Low, yend=High), color="brown3", size=1) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=Low), color="deepskyblue4", size=3 ) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=High), color="deeppink4", size=3 ) +
  coord_flip()+
  theme_minimal() +
  scale_y_continuous(trans='log10')+
  theme(legend.position = "none",) +
  geom_hline(yintercept=1, linetype='dashed', color='firebrick', size=1) +
  xlab("Predictor \n") +
  #ylim(0.5,1.5)+
  ylab("\n Odd ratio (Lower-Upper ends)")

















# Logistic regression Anorectic Exp vs No Anorectic

MAX_Labs_Demographics <- fread("MAX_Labs_Demographics.txt")

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% filter(p2==60) %>% select(-c(p1, p2))

Dems_Labs_TreatExp <- MAX_Labs_Demographics %>% inner_join(Cum_Class_Experience_EveryMonth)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% drop_na()

Number_Drugs_EverExperienced <- fread("Number_Drugs_EverExperienced.txt")

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% left_join(Number_Drugs_EverExperienced) %>% mutate(Diff_Drugs_exp = ifelse(is.na(Diff_Drugs_exp),0,Diff_Drugs_exp))

Physicians_Experience <- fread("Physicians_Experience.txt")

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% left_join(Physicians_Experience)

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

Dems_Labs_TreatExp <- Treatment_exp_Vector %>% inner_join(Dems_Labs_TreatExp)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(Group = ifelse(p1_AnorecticExp==1, "Anorectic_Exp", "No_Anorectic"))

Dems_Labs_TreatExp$Group <- as.factor(Dems_Labs_TreatExp$Group)

Dems_Labs_TreatExp$Group <- relevel(Dems_Labs_TreatExp$Group,"No_Anorectic")


Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-c(p1_AnorecticExp ))

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(gender = ifelse(gender=="M", 1, 0))

temp <- Dems_Labs_TreatExp

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-patient)


Dems_Labs_TreatExp %>% group_by(Group) %>% count()

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% group_by(Group) %>% sample_n(1886)
Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% ungroup()


# Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(age=scale(age),
#                               MAX_HbA1c =scale(MAX_HbA1c ),
#                               MAX_BMI =scale(MAX_BMI ),
#                               MAX_ALT =scale(MAX_ALT ),
#                               MAX_AST =scale(MAX_AST ),
#                               age=scale(age))




create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}



Dems_Labs_TreatExp <- Dems_Labs_TreatExp[sample(1:nrow(Dems_Labs_TreatExp)), ]



Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(c(age, MAX_BMI, gender, p1_WeightLossExp, p1_InjExp, p1_AntiobesityExp,
                                                      ENDOCRINOLOGY, CARDIOLOGY, PSYCHIATRY,  a,c,l,u, Group))


data_train <- create_train_test(Dems_Labs_TreatExp, 0.8, train = TRUE)
data_test <- create_train_test(Dems_Labs_TreatExp, 0.8, train = FALSE)

Risk_pred_model <- glm( Group ~ ., data = data_train, family = binomial)

summary(Risk_pred_model)


# Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)        5.698029   0.330343  17.249 < 0.0000000000000002 ***
#   age               -0.037728   0.003897  -9.682 < 0.0000000000000002 ***
#   MAX_BMI           -0.090508   0.005552 -16.301 < 0.0000000000000002 ***
#   gender             0.425462   0.106809   3.983       0.000067934606 ***
#   p1_WeightLossExp  -1.320195   1.237492  -1.067              0.28605    
# p1_InjExp         -2.363721   0.226653 -10.429 < 0.0000000000000002 ***
#   p1_AntiobesityExp -2.047959   0.163438 -12.531 < 0.0000000000000002 ***
#   ENDOCRINOLOGY      0.436187   0.292858   1.489              0.13638    
# CARDIOLOGY         0.904418   0.581826   1.554              0.12008    
# PSYCHIATRY         3.637451   0.370894   9.807 < 0.0000000000000002 ***
#   a                 -0.650698   0.272186  -2.391              0.01682 *  
#   c                 -0.577374   0.187792  -3.075              0.00211 ** 
#   l                 -0.799099   0.123994  -6.445       0.000000000116 ***
#   u                 -1.167125   0.246578  -4.733       0.000002209060 ***

i

predict <- predict(Risk_pred_model, data_test, type = 'response')

table_mat <- table(data_test$Group, predict > 0.50)
table_mat

plot(table_mat)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test


precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}



recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
prec # 0.7565445

rec <- recall(table_mat)
rec # 0.7853261

f1 <- 2 * ((prec * rec) / (prec + rec))
f1 # 0.7706667



pred <- data.frame(predict(Risk_pred_model, temp, type = 'response')) %>% bind_cols(temp)

names(pred)[1] <- "Probability"




OBE_Flows_Aux._Long     <- fread("OBE_Flows_Aux._Long_v2.txt", integer64 = "character", stringsAsFactors = F)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(patient, s2) %>% filter(s2=="a") %>% distinct()

pred <- pred %>% left_join(OBE_Flows_Aux._Long) %>% mutate(s2=ifelse(is.na(s2),"No", "Anorectic"))

pred <- pred %>% select(-c(weight, diagnosis, MAX_HbA1c, MAX_AST, MAX_ALT, d,f,k,p,r,s,v,z))


Odd_ratios_Anorectic_noAnorectic_TreatExt_short <- fread("Odd_ratios_Anorectic_noAnorectic_TreatExt_short.txt", sep="\t")

Odd_ratios_Anorectic_noAnorectic_TreatExt_short$Predictor <- as.factor(Odd_ratios_Anorectic_noAnorectic_TreatExt_short$Predictor)

Odd_ratios_Anorectic_noAnorectic_TreatExt_short <- Odd_ratios_Anorectic_noAnorectic_TreatExt_short %>% arrange(`Odd ratio`)

Odd_ratios_Anorectic_noAnorectic_TreatExt_short <- Odd_ratios_Anorectic_noAnorectic_TreatExt_short %>% filter(`Sig. ?`!="ns")

Odd_ratios_Anorectic_noAnorectic_TreatExt_short %>%
  ggplot() +
  geom_segment( aes(x=reorder(Predictor, `Odd ratio`) , xend=reorder(Predictor, `Odd ratio`), y=Low, yend=High), color="brown3", size=1) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=Low), color="deepskyblue4", size=3 ) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=High), color="deeppink4", size=3 ) +
  coord_flip()+
  scale_y_continuous(trans='log10')+
  theme_minimal() +
  theme(legend.position = "none",) +
  geom_hline(yintercept=1, linetype='dashed', color='firebrick', size=1) +
  xlab("Predictor \n") +
  #ylim(0.5,1.5)+
  ylab("\n Odd ratio (Lower-Upper ends)")

# -----------
# Summary Waterfall Obesity Population, experienced, targets ---------------------------------

# Obesity: T2D vs OBE only
DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637

DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))

sum(DANU_Demographics$weight_2) # 132236677

# BMI MAX Bucket
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Events)

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code))

DANU_Events <- DANU_Events %>% group_by(patid) %>% select(-c(claimed)) %>% distinct()

DANU_Events <- DANU_Events %>% ungroup() %>% mutate(BMI_group = ifelse(code<25, "<25",
                                                                       ifelse(code>=25&code<=27, "25_to_27",
                                                                              ifelse(code>27&code<=30, "27_to_30",
                                                                                     ifelse(code>30&code<=40, "30_to_40", ">40"))))) %>% select(-code)



DANU_Events %>% filter(BMI_group!="<25"&BMI_group!="25_to_27")

DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Events)

sum(DANU_Demographics$weight_2, na.rm=T) # 132236677
names(DANU_Demographics)[1] <- "patient"

DANU_Demographics %>% group_by(BMI_group) %>% summarise(n=sum(weight_2))




# Comorbid ?

# OBE_Disorder_Histories <- fread("OBE Disorder Histories.txt")
# OBE_Disorder_Histories <- OBE_Disorder_Histories %>% select(patient, month60)
# OBE_Disorder_Histories <- OBE_Disorder_Histories %>% mutate(comorbs = str_extract(month60, "[a-z]+")) %>% select(-month60)
# 
# Comorbid_Pats <- OBE_Disorder_Histories %>% filter(grepl("a",comorbs)|
#                                                      grepl("f",comorbs)|
#                                                      grepl("k",comorbs)|
#                                                      grepl("l",comorbs)|
#                                                      grepl("s",comorbs)|
#                                                      grepl("v",comorbs)) %>% select(patient) %>% mutate(Comorbid="Comrbid")
# 


OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")

length(unique(OBE_Comorbidity_Inventories$patid)) # 721893

OBE_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 106172398

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% filter(grepl("I1", diagnosis)|grepl("I2", diagnosis)|grepl("I4", diagnosis)|grepl("I6", diagnosis)|
                                         grepl("E78", diagnosis)|grepl("I70", diagnosis)|grepl("I50", diagnosis)|
                                         grepl("Z95", diagnosis))

data.frame(OBE_Comorbidity_Inventories %>% group_by(diagnosis) %>% count())

OBE_Comorbidity_Inventories %>% mutate(diagnosis = ifelse(grepl("I1", diagnosis),"I1",
                                                          ifelse(grepl("I2", diagnosis), "I2",
                                                                 ifelse(grepl("I4", diagnosis), "I4",
                                                                        ifelse(grepl("I6", diagnosis), "I6",
                                                                               ifelse(grepl("E78", diagnosis), "E78",
                                                                                      ifelse(grepl("G47", diagnosis), "G47",
                                                                                        ifelse(grepl("I70", diagnosis), "I70",
                                                                                             ifelse(grepl("I50", diagnosis),"E50",
                                                                                                    ifelse(grepl("Z95", diagnosis), "Z95", NA)))))))))) %>% 
  group_by(diagnosis) %>% count() %>% mutate(percent=100*n/723778) %>% arrange(n)


# diagnosis      n percent
# <chr>      <int>   <dbl>
#   1 E50        28224    3.90
# 2 I70        32335    4.47
# 3 Z95        33805    4.67
# 4 I6         73992   10.2 
# 5 I2        127580   17.6 
# 6 I4        176473   24.4 
# 8 E78       338230   46.7 
# 9 I1        378260   52.3

Comorbid_Pats <- OBE_Comorbidity_Inventories %>% select(patid) %>% distinct() %>% mutate(Comorbid="Comorbid")
names(Comorbid_Pats)[1] <- "patient"

DANU_Demographics <- DANU_Demographics %>% left_join(Comorbid_Pats)
DANU_Demographics <- DANU_Demographics %>% mutate(Comorbid=ifelse(is.na(Comorbid), "NO", Comorbid))

sum(DANU_Demographics$weight_BMI, na.rm=T) # 132236677

DANU_Demographics %>% filter(BMI_group==">40"|BMI_group=="30_to_40"|(BMI_group=="27_to_30"&Comorbid=="Comorbid")) %>% summarise(n=sum(weight_2)) # 51514418

DANU_Demographics %>% filter(is.na(BMI_group)|BMI_group=="<25"|(BMI_group=="25_to_27"&Comorbid=="NO")) %>% summarise(n=sum(weight_2, na.rm=T)) # 52791756

DANU_Demographics <- DANU_Demographics %>% mutate(ToKepp = ifelse(BMI_group==">40"|BMI_group=="30_to_40"|(BMI_group=="27_to_30"&Comorbid=="Comorbid"),"YES", "Nope"))

DANU_Demographics$weight_BMI <- DANU_Demographics$weight_BMI*0.7665022

sum(DANU_Demographics$weight_BMI, na.rm=T) # 132236677

DANU_Demographics %>% filter(ToKepp=="YES") %>% summarise(n=sum(weight_2, na.rm=T)) # 76699366
DANU_Demographics %>% filter(ToKepp!="YES") %>% summarise(n=sum(weight_2, na.rm=T)) # 55534693

sum(DANU_Demographics$weight_2, na.rm=T) # 132236677




# Treatment experience
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient) %>% mutate(TreatExp="TreatExp")
DANU_Demographics <- DANU_Demographics %>% left_join(Treatment_exp_Vector) %>% mutate(TreatExp=ifelse(is.na(TreatExp),"NO", TreatExp))

# Any Dx (from events, but remove BMI)

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity"&source=="Diagnosis") %>% select(code)
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes)
DANU_Events <- DANU_Events %>% select(patid, code)
names(DANU_Events)[1] <- "patient"
DANU_Events <- DANU_Demographics %>% select(patient) %>% inner_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(!grepl("BMI", code))  
DANU_Events <- DANU_Events %>% select(patient) %>% distinct() %>% mutate(Diagnosis="Yes")
DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Events) %>% mutate(Diagnosis=ifelse(is.na(Diagnosis),"NO", Diagnosis))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="NO") %>% summarise(n=sum(weight_2, na.rm=T)) # 41263051
DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="Yes") %>% summarise(n=sum(weight_2, na.rm=T)) # 79602790
DANU_Demographics %>% filter(TreatExp=="TreatExp") %>% summarise(n=sum(weight_2, na.rm=T)) # 11370836

sum(DANU_Demographics$weight_2, na.rm=T) # 132236677

DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="Yes") %>% summarise(n=sum(weight_2, na.rm=T)) # 79602790

DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="Yes") %>% filter(BMI_group==">40"|BMI_group=="30_to_40"|(BMI_group=="27_to_30"&Comorbid=="Comorbid")) %>%
  summarise(n=sum(weight_2, na.rm=T)) # 36246043

DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="Yes") %>% filter((BMI_group!=">40"&BMI_group!="30_to_40"&!(BMI_group=="27_to_30"&Comorbid=="Comorbid"))) %>%
  summarise(n=sum(weight_2, na.rm=T))  # 11334772


DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="NO") %>% summarise(n=sum(weight_2, na.rm=T)) # 41263051

DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="NO") %>% filter(BMI_group==">40"|BMI_group=="30_to_40"|(BMI_group=="27_to_30"&Comorbid=="Comorbid")) %>%
  summarise(n=sum(weight_2, na.rm=T)) # 23515099

DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="NO") %>% filter((BMI_group!=">40"&BMI_group!="30_to_40"&!(BMI_group=="27_to_30"&Comorbid=="Comorbid"))) %>%
  summarise(n=sum(weight_2, na.rm=T))  # 17536104




DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637

DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2) %>% filter(!is.na(weight_2))

sum(DANU_Demographics$weight_2) # 132236677
names(DANU_Demographics)[1] <- "patient"



# Treatment experience
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient) %>% mutate(TreatExp="TreatExp")

DANU_Demographics <- DANU_Demographics %>% left_join(Treatment_exp_Vector) %>% mutate(TreatExp=ifelse(is.na(TreatExp),"NO", TreatExp))


# Any Dx (from events, but remove BMI)
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code)
names(DANU_Events)[1] <- "patient"
DANU_Events <- DANU_Demographics %>% select(patient) %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(!grepl("BMI", code))  
DANU_Events <- DANU_Events %>% select(patient) %>% distinct() %>% mutate(Diagnosis="Yes")

DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Events) %>% mutate(Diagnosis=ifelse(is.na(Diagnosis),"NO", Diagnosis))

sum(DANU_Demographics$weight_2) # 132236677

DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="NO") %>% summarise(n=sum(weight_2, na.rm=T)) # 40930096

DANU_Demographics %>% filter(TreatExp=="NO"&Diagnosis=="Yes") %>% summarise(n=sum(weight_2, na.rm=T)) # 79935745

DANU_Demographics %>% filter(TreatExp=="TreatExp") %>% summarise(n=sum(weight_2, na.rm=T)) # 11370836

DANU_Demographics <- DANU_Demographics %>% filter(TreatExp=="TreatExp")

sum(DANU_Demographics$weight_2) # 11370836


# Drug usage last year vs only first 48 months
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% inner_join(Treatment_exp_Vector)

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(52:63)

OBE_Drug_Histories <- OBE_Drug_Histories %>%  mutate_if(grepl("-",.), ~replace(., grepl("-", .), "None")) 

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="None",0,1))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

OBE_Drug_Histories_LONG     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% inner_join(Treatment_exp_Vector %>% select(patient))
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% select(patient) %>% bind_cols(OBE_Drug_Histories)

OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% distinct() %>% mutate(TreatLastYear="TreatLastYear")

DANU_Demographics <- DANU_Demographics %>% left_join(OBE_Drug_Histories) %>% mutate(TreatLastYear=ifelse(is.na(TreatLastYear), "No", TreatLastYear))

sum(DANU_Demographics$weight_2) #11370836

DANU_Demographics %>% filter(TreatLastYear=="No") %>% summarise(n=sum(weight_2)) # 5411411
DANU_Demographics %>% filter(TreatLastYear=="TreatLastYear") %>% summarise(n=sum(weight_2)) # 5959425

DANU_Demographics <- DANU_Demographics %>% filter(TreatLastYear=="TreatLastYear")


# GLP1 Last year vs only old drugs
DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")


Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% inner_join(Treatment_exp_Vector)

# select only columns with the months / drugs
OBE_Drug_Histories <-  OBE_Drug_Histories %>%  select(52:63)

OBE_Drug_Histories <- OBE_Drug_Histories %>% 
  mutate_if(grepl(string_InjectableGLP1,.)|grepl(string_OralGLP1,.), ~replace(., grepl(string_InjectableGLP1, .)|grepl(string_OralGLP1,.), "GLP1"))

OBE_Drug_Histories <-  OBE_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1",1,0))

OBE_Drug_Histories[] <-  lapply(OBE_Drug_Histories,as.numeric)

OBE_Drug_Histories_LONG     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories_LONG <- OBE_Drug_Histories_LONG %>% inner_join(Treatment_exp_Vector %>% select(patient))
OBE_Drug_Histories <- OBE_Drug_Histories_LONG %>% select(patient) %>% bind_cols(OBE_Drug_Histories)

OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat == 1)
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient) %>% distinct() %>% mutate(GLP1LastYear="GLP1LastYear")

DANU_Demographics <- DANU_Demographics %>% left_join(OBE_Drug_Histories) %>% mutate(GLP1LastYear=ifelse(is.na(GLP1LastYear), "No", GLP1LastYear))


sum(DANU_Demographics$weight_2) #3404519

DANU_Demographics %>% filter(GLP1LastYear=="No") %>% summarise(n=sum(weight_2)) # 5762121
DANU_Demographics %>% filter(GLP1LastYear=="GLP1LastYear") %>% summarise(n=sum(weight_2)) # 197304.3

# ------
# Circular charts ----------
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- fread("Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity.txt")
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% filter(difference>1|difference< (-1))
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% select(penetance_treat_pop, penetance_untreat_pop, Last)
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity$Last <- gsub('[^[:alnum:] ]','',Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity$Last)


Treated <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% select(penetance_treat_pop, Last) %>% mutate(Group="Treated") 
names(Treated)[1] <- "Penetrance"

Untreated <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% select(penetance_untreat_pop, Last) %>% mutate(Group="Untreated")
names(Untreated)[1] <- "Penetrance"

OBesity_circular <- Treated %>% bind_rows(Untreated)

OBesity_circular <- OBesity_circular %>% mutate(Last=as.factor(Last), Group=as.factor(Group))
OBesity_circular <- data.frame(OBesity_circular)

row_num = length(levels(OBesity_circular$Group))

g = ggplot(OBesity_circular,aes(x=Last,y=as.numeric(Group),fill=Penetrance)) + 
  xlim(c("",as.vector(unique(OBesity_circular$Last)))) + 
  ylim(c(-row_num/1.5,row_num+1))+
  geom_tile()+ ylab("")+
  scale_fill_gradientn(colours=wes_palette("Zissou1", 116, type = "continuous"))+
  annotate(x="",y=1:row_num,label=levels(OBesity_circular$Group),size=4,geom="text", colour="firebrick") 

g + coord_polar(start=-0.15,) + theme_bw() + theme(axis.text = element_text(size = 8)) 









Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- fread("Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity.txt")
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% select(difference, Last)
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% filter(difference>1 | difference<(-1))
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% mutate(Group = str_sub(Last, 2L, 2L))

Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity$Last <- gsub('[^[:alnum:] ]','',Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity$Last)

empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity$Group), ncol(Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity)) )
colnames(to_add) <- colnames(Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity)
to_add$group <- rep(levels(Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity$Group), each=empty_bar)
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- rbind(Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity, to_add)
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity %>% arrange(Group)
Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity$id <- seq(1, nrow(Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity))

label_data <- Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


ggplot(Circular_bar_Chart_Dx_Pen_TreatVSNonTreat_Obesity, aes(x=as.factor(id), y=difference, fill=Group)) +  
  geom_bar(stat="identity", alpha=0.5) +
  theme_minimal() +
  theme(legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()) +
  coord_polar() + 
  scale_fill_viridis_d()+
  geom_text(data=label_data, aes(x=id, y=difference+4, label=Last , hjust=hjust), color="black", fontface="bold",alpha=0.6, size=6, angle= label_data$angle, inherit.aes = FALSE ) 


# ------------
# Percentage Paid Scripts over time ----------


OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% filter(paid != "?")


OBE_US_Doses %>% group_by(drug_class, paid) %>% count()

# drug_class      paid        n
# <chr>           <chr>   <int>
# 1 Anorectic       D      211130
# 2 Anorectic       P     1059754
# 3 Antiobesity     D       11447
# 4 Antiobesity     P       15679
# 5 GLP1 Injectable D        5949
# 6 GLP1 Injectable P       10538
# 7 GLP1 Oral       D         616
# 8 GLP1 Oral       P         797
# 9 Surgery         D          88
# 10 Surgery         P        3198
# 11 Weight Loss     D         344
# 12 Weight Loss     P         334



OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% filter(paid != "?")
OBE_US_Doses <- OBE_US_Doses %>% filter(drug_class == "GLP1 Injectable")
OBE_US_Doses <- OBE_US_Doses %>% select(from_dt, paid)
OBE_US_Doses$Month_Yr <-  format(as.Date(OBE_US_Doses$from_dt), "%Y-%m")
OBE_US_Doses <- OBE_US_Doses %>% select(-from_dt)

temp <- OBE_US_Doses %>% group_by(Month_Yr, paid) %>% count() %>%
  spread(key=paid, value=n) %>%
  mutate(total=D+P) %>% 
  mutate(Percent_paid=100*P/total) 

temp$Month_Yr <- paste0("\'",temp$Month_Yr) 


temp %>% mutate(Index= %>% ggplot(aes(x=Index, y=Percent_paid))+
                  geom_point()+
                  geom_smooth(method = "lm")
                
fwrite(temp,"Percent_PaidDenied_GLP1_OverTime.txt", sep="\t")
                
# ------
# All physicians for all Dx Events Obesity ---------
# FROM NASH FOLDER !!!!!!!
NASH_Event_Claims_Providers <- fread("NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(-c(taxonomy1, taxonomy2, grp_practice, clmid))

Summary_Specialties <- fread("Summary_Specialties.txt")


OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(prov_unique, ptid,fst_dt)
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[1] <- "prov"
OBE_Pats_95ConfLiver_2plusHits_Provider$prov <- as.character(OBE_Pats_95ConfLiver_2plusHits_Provider$prov)


temp <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% inner_join(NASH_Event_Claims_Providers) %>% drop_na() %>%
  left_join(Summary_Specialties) %>% drop_na() %>% ungroup() %>% select(ptid, fst_dt, SUMMARY_SPECIALTY) 

temp %>% group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(n)

# ------
# How events evolve over time and % share of physicians ------------
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient)

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, claimed, code) %>% distinct()
names(DANU_Events)[1] <- "patient"

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code)

DANU_Events <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes)

DANU_Events <- DANU_Events %>% group_by(patient) %>% slice(1)
DANU_Events <- DANU_Events %>% select(patient, claimed)
DANU_Events <- DANU_Events %>% inner_join(OBE_Drug_Histories)
names(DANU_Events)[2] <- "First_OBE_Dx"

First_OBE_Dx <- DANU_Events

# You have the date of first Obeisty Dx for each patient now



OBE_Pats_95ConfLiver_2plusHits_Provider <- fread("OBE_Pats_95ConfLiver_2plusHits_Provider.txt")
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>%  mutate(diag = str_sub(diag, 1L, 1L))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% filter(grepl("A|B|C|D|E|F|G|H|I|J|K|L|M|N", diag))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(-c(taxonomy1, taxonomy2, grp_practice, clmid))
OBE_Pats_95ConfLiver_2plusHits_Provider <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% select(ptid, diag, fst_dt)
names(OBE_Pats_95ConfLiver_2plusHits_Provider)[1] <- "patient"

First_OBE_Dx <- OBE_Pats_95ConfLiver_2plusHits_Provider %>% left_join(First_OBE_Dx)


First_OBE_Dx$fst_dt  <- as.Date(First_OBE_Dx$fst_dt )
First_OBE_Dx$First_OBE_Dx <- as.Date(First_OBE_Dx$First_OBE_Dx)

First_OBE_Dx <- First_OBE_Dx %>% mutate(ElapsedTime = round(as.numeric((fst_dt-First_OBE_Dx)/30.5)))

length(unique(First_OBE_Dx$patient))

First_OBE_Dx <- First_OBE_Dx %>% filter(ElapsedTime>=12 | ElapsedTime<=(-12)) %>% select(patient) %>% distinct() %>% left_join(First_OBE_Dx)

First_OBE_Dx %>% filter(ElapsedTime==0) %>% select(patient) %>% distinct()

First_OBE_Dx <- First_OBE_Dx %>% mutate(diag = str_sub(diag, 1L, 1L))

First_OBE_Dx <- First_OBE_Dx %>% distinct()

temp <- data.frame(First_OBE_Dx %>% filter(ElapsedTime>=(-12)&ElapsedTime<=(12)) %>%
                     group_by(ElapsedTime, diag) %>% count() %>%
                     spread(key=ElapsedTime, value=n))

fwrite(temp, "ICD10_Pen_First_OBE_Dx_12m.txt", sep="\t")




# -----------



# Doses All
OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% filter(paid != "?")
OBE_US_Doses <- OBE_US_Doses %>% select(drug_class, pat_id, from_dt, paid)
OBE_US_Doses$from_dt <- as.Date(OBE_US_Doses$from_dt)
OBE_US_Doses <- OBE_US_Doses %>% arrange(drug_class, pat_id, from_dt)

# Injectable GLP1
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(To_keep)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Injectable %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
# 1 D      3291
# 2 P     10538

# Oral GLP1
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(To_keep)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Oral %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
# 1 D       423
# 2 P       797

# Antiobesity
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
Paid_dates_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Antiobesity)[3] <- "Paid_dates"
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(Paid_dates_Antiobesity)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Antiobesity %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(To_keep)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Antiobesity %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
# 1 D      7926
# 2 P     15679



# Anorectic
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
Paid_dates_Anorectic <- OBE_US_Doses_Anorectic %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Anorectic)[3] <- "Paid_dates"
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(Paid_dates_Anorectic)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Anorectic %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(To_keep)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Anorectic %>% group_by(paid) %>% count()

# paid        n
# <chr>   <int>
#   1 D      189479
# 2 P     1059754


# Weight_Loss
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight_Loss")
Paid_dates_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Weight_Loss)[3] <- "Paid_dates"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(Paid_dates_Weight_Loss)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Weight_Loss %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight_Loss")
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(To_keep)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Weight_Loss %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D        53
# 2 P      3198



# Weight_Loss
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
Paid_dates_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Weight_Loss)[3] <- "Paid_dates"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(Paid_dates_Weight_Loss)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Weight_Loss %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(To_keep)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Weight_Loss %>% group_by(paid) %>% count()


# paid      n
# <chr> <int>
#   1 D       112
# 2 P       334



OBE_US_Doses_GLP1_Injectable # From above, but ove time now

OBE_US_Doses_GLP1_Injectable$Month_Yr <-  format(as.Date(OBE_US_Doses_GLP1_Injectable$from_dt), "%Y-%m")
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% select(-from_dt)

temp <- OBE_US_Doses_GLP1_Injectable %>% group_by(Month_Yr, paid) %>% count() %>%
  spread(key=paid, value=n) %>%
  mutate(total=D+P) %>% 
  mutate(Percent_paid=100*P/total) 

temp$Month_Yr <- paste0("\'",temp$Month_Yr) 


temp %>% ggplot(aes(x=Month_Yr     , y=Percent_paid))+
  geom_point()+
  geom_smooth(method = "lm")

fwrite(temp,"Percent_PaidDenied_GLP1_OverTime.txt", sep="\t")

# % ever treated based on BMI and stock on M60 ------------------

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector$Treat_Exp <- "Treat_Exp"
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(-weight)


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677

# BMI MAX Bucket
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code))
DANU_Events <- DANU_Events %>% group_by(patid) %>% select(-c(claimed)) %>% distinct()
DANU_Events <- DANU_Events %>% ungroup() %>% mutate(BMI_group = ifelse(code<25, "<25",
                                                                       ifelse(code>25&code<=27, "25_to_27",
                                                                              ifelse(code>27&code<=30, "27_to_30",
                                                                                     ifelse(code>30&code<=40, "30_to_40", ">40"))))) %>% select(-code)


DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Events)

sum(DANU_Demographics$weight_2, na.rm=T) # 132236677
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)


DANU_Demographics <- DANU_Demographics %>% left_join(Treatment_exp_Vector)

DANU_Demographics %>% group_by(BMI_group, Treat_Exp) %>%
  summarise(n=sum(weight_2))

OBE_Box_Histories <- fread("OBE Box Histories.txt")
OBE_Box_Histories <- OBE_Box_Histories %>% select(1,2,63)
OBE_Box_Histories <- OBE_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))


DANU_Demographics %>% left_join(OBE_Box_Histories) %>% inner_join(Treatment_exp_Vector %>% select(patient)) %>%
  group_by(BMI_group, month60) %>% summarise(n=sum(weight_2)) %>%
  spread(key=BMI_group, value=n)





# ------------



# All comorbidities ~ BMI bucket -----------

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677

# BMI MAX Bucket
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code))
DANU_Events <- DANU_Events %>% group_by(patid) %>% select(-c(claimed)) %>% distinct()
DANU_Events <- DANU_Events %>% ungroup() %>% mutate(BMI_group = ifelse(code<25, "<25",
                                                                       ifelse(code>25&code<=27, "25_to_27",
                                                                              ifelse(code>27&code<=30, "27_to_30",
                                                                                     ifelse(code>30&code<=40, "30_to_40", ">40"))))) %>% select(-code)


DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Events)

sum(DANU_Demographics$weight_2, na.rm=T) # 132236677
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)


DANU_Demographics %>% filter(!is.na(BMI_group)) %>% group_by(BMI_group) %>% summarise(n=sum(weight_2)) 

#  with BMI

# BMI_group         n
# <chr>         <dbl>
# 1 <25        2846529.
# 2 >40       13648335.
# 3 "25_to_27"  17829957. (20676486 with 25-)
# 4 27_to_30  24087801.
# 5 30_to_40  37866083.




OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
names(OBE_Comorbidity_Inventories)[1] <- "patient"

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% select(-weight) %>% left_join(DANU_Demographics)
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% drop_na()
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% mutate(BMI_group=ifelse(BMI_group=="<25", "25_to_27", BMI_group))


Dx_to_track <- OBE_Comorbidity_Inventories %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% filter(n>962787.1)

temp <- OBE_Comorbidity_Inventories %>% group_by(BMI_group, diagnosis) %>%
  summarise(n=sum(weight_2)) %>% mutate(percent=ifelse(BMI_group=="25_to_27",100*n/20676486,
                                                       ifelse(BMI_group=="27_to_30", 100*n/24087801,
                                                              ifelse(BMI_group=="30_to_40", 100*n/37866083,
                                                                     ifelse(BMI_group==">40", 100*n/13648335., NA))))) %>%
  select(-n) %>%
  #inner_join(Dx_to_track %>% select(diagnosis)) %>%
  spread(key=BMI_group, value=percent)

temp <- temp %>% mutate(difference=`>40`-`25_to_27`) %>% arrange(-difference)

# -------
# Surgery Patients: Stocks / BMI before/after ----------------

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% filter(!is.na(weight_2))
names(DANU_Demographics)[1] <- "patient"

# Where are they now ?

OBE_Flows_Aux._Long <- fread("OBE_Flows_Aux._Long_v2.txt")

Surgery_Pats <- OBE_Flows_Aux._Long %>% filter(s1=="H"|s2=="H") %>% select(patient) %>% distinct()

Surgery_Pats %>% left_join(OBE_Flows_Aux._Long) %>% filter(p2==60) %>% left_join(DANU_Demographics) %>%
  group_by(s2) %>% summarise(n=sum(weight_2))

# s2          n
# <chr>   <dbl>
# 1 a      42430.
# 2 g        484.
# 3 G       3225.
# 4 H      23577.
# 5 o        535.
# 6 x     879157.

# Stock before and stock after

Surgery_Pats %>% left_join(DANU_Demographics) %>% summarise(n=sum(weight_2))  # 949408 / 5638pats

Surgery_Pats %>% left_join(OBE_Flows_Aux._Long) %>% filter(s1!="H"&s2=="H") %>% left_join(DANU_Demographics) %>% 
  group_by(s1,s2) %>% summarise(n=sum(weight_2))

# s1    s2           n
# <chr> <chr>    <dbl>
# 1 a     H       34866.
# 2 G     H         975.
# 3 o     H         980.
# 4 x     H     1100073.

Surgery_Pats %>% left_join(OBE_Flows_Aux._Long) %>% filter(s1=="H"&s2!="H") %>% left_join(DANU_Demographics) %>% 
  group_by(s1,s2) %>% summarise(n=sum(weight_2))

# s1    s2           n
# <chr> <chr>    <dbl>
# 1 H     a       31180.
# 2 H     G         999.
# 3 H     o        1129.
# 4 H     x     1107215.


# BMI before / after surgery

First_Surgery <- Surgery_Pats %>% left_join(OBE_Flows_Aux._Long) %>% filter(s1=="H"|s2=="H") %>% select(patient, p1, p2) %>% 
  group_by(patient) %>% slice(1) %>% select(patient, p2)
names(First_Surgery)[2] <- "First_Surgery"


DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"
DANU_Events <- Surgery_Pats %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(-weight)
DANU_Events$claimed <- format(as.Date(DANU_Events$claimed), "%Y-%m")

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% drop_na() %>% select(-claimed)

DANU_Events <- DANU_Events %>% left_join(First_Surgery) %>% mutate(elapsedTime=Exact_Month-First_Surgery)

Pats_to_keep <- DANU_Events %>% filter(elapsedTime<0) %>% select(patient) %>% distinct() %>%
  inner_join(DANU_Events %>% filter(elapsedTime>0) %>% select(patient) %>% distinct())


Pats_to_keep %>% left_join(DANU_Events) %>% filter(elapsedTime<0) %>% ungroup() %>%
  summarise(n=mean(code)) # 40.74468

Pats_to_keep %>% left_join(DANU_Events) %>% filter(elapsedTime>0) %>% ungroup() %>%
  summarise(n=mean(code)) # 33.89102

Pats_to_keep %>% left_join(DANU_Events) %>% filter(elapsedTime==0) %>% ungroup() %>%
  summarise(n=mean(code)) # 39.56597


Summary_To_Graph <- Pats_to_keep %>% left_join(DANU_Events)  %>%
  group_by(patient, elapsedTime) %>% filter(code==mean(code)) %>%
  slice(1)

Summary_To_Graph %>%
  ggplot(aes(elapsedTime, code)) +
  geom_jitter(size=0.3, alpha=0.3, colour="deepskyblue4")+
  geom_smooth(method="lm", formula= y~poly(x,10), colour="firebrick", fill="firebrick")+
  xlim(-24,24)+
  ylim(25,62)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


# --------
# Percentage Paid Scripts over time per PLAN (Public, Private, Dual) ----------

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, plan)

DANU_Demographics %>% filter(plan=="D") %>% select(patid)




# Doses All  -   DUAL
OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% filter(paid != "?")
OBE_US_Doses <- OBE_US_Doses %>% select(drug_class, pat_id, from_dt, paid)
OBE_US_Doses$from_dt <- as.Date(OBE_US_Doses$from_dt)
OBE_US_Doses <- OBE_US_Doses %>% arrange(drug_class, pat_id, from_dt)
OBE_US_Doses <- OBE_US_Doses %>% inner_join(DANU_Demographics %>% filter(plan=="D") %>% select(patid), by=c("pat_id"="patid"))


# Injectable GLP1
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(To_keep)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Injectable %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D       114
# 2 P       195

# Oral GLP1
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(To_keep)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Oral %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D         2
# 2 P         5

# Antiobesity
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
Paid_dates_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Antiobesity)[3] <- "Paid_dates"
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(Paid_dates_Antiobesity)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Antiobesity %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(To_keep)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Antiobesity %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D       292
# 2 P       369



# Anorectic
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
Paid_dates_Anorectic <- OBE_US_Doses_Anorectic %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Anorectic)[3] <- "Paid_dates"
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(Paid_dates_Anorectic)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Anorectic %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(To_keep)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Anorectic %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D      9521
# 2 P     33418

# Weight_Loss
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
Paid_dates_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Weight_Loss)[3] <- "Paid_dates"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(Paid_dates_Weight_Loss)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Weight_Loss %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(To_keep)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Weight_Loss %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D         8
# 2 P         7

# Surgery
OBE_US_Doses_Surgery <- OBE_US_Doses %>% filter(drug_class=="Surgery")
Paid_dates_Surgery <- OBE_US_Doses_Surgery %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Surgery)[3] <- "Paid_dates"
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% left_join(Paid_dates_Surgery)
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Surgery %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Surgery <- OBE_US_Doses %>% filter(drug_class=="Surgery")
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% left_join(To_keep)
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Surgery %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 P        38






OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% filter(paid != "?")
OBE_US_Doses <- OBE_US_Doses %>% select(drug_class, pat_id, from_dt, paid)
OBE_US_Doses$from_dt <- as.Date(OBE_US_Doses$from_dt)
OBE_US_Doses <- OBE_US_Doses %>% arrange(drug_class, pat_id, from_dt)
OBE_US_Doses <- OBE_US_Doses %>% inner_join(DANU_Demographics %>% filter(plan=="M") %>% select(patid), by=c("pat_id"="patid"))


# Injectable GLP1
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(To_keep)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Injectable %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D       314
# 2 P       571

# Oral GLP1
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(To_keep)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Oral %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D         5
# 2 P        18

# Antiobesity
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
Paid_dates_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Antiobesity)[3] <- "Paid_dates"
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(Paid_dates_Antiobesity)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Antiobesity %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(To_keep)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Antiobesity %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D       408
# 2 P       685



# Anorectic
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
Paid_dates_Anorectic <- OBE_US_Doses_Anorectic %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Anorectic)[3] <- "Paid_dates"
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(Paid_dates_Anorectic)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Anorectic %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(To_keep)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Anorectic %>% group_by(paid) %>% count()

# paid       n
# <chr>  <int>
#   1 D      32901
# 2 P     133402

# Weight_Loss
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
Paid_dates_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Weight_Loss)[3] <- "Paid_dates"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(Paid_dates_Weight_Loss)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Weight_Loss %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(To_keep)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Weight_Loss %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D        31
# 2 P        59
#   

# Surgery
OBE_US_Doses_Surgery <- OBE_US_Doses %>% filter(drug_class=="Surgery")
Paid_dates_Surgery <- OBE_US_Doses_Surgery %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Surgery)[3] <- "Paid_dates"
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% left_join(Paid_dates_Surgery)
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Surgery %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Surgery <- OBE_US_Doses %>% filter(drug_class=="Surgery")
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% left_join(To_keep)
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Surgery %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D        31
# 2 P        59











OBE_US_Doses <- fread("OBE Doses.txt")
OBE_US_Doses <- OBE_US_Doses %>% filter(status != "G")
OBE_US_Doses <- OBE_US_Doses %>% filter(paid != "?")
OBE_US_Doses <- OBE_US_Doses %>% select(drug_class, pat_id, from_dt, paid)
OBE_US_Doses$from_dt <- as.Date(OBE_US_Doses$from_dt)
OBE_US_Doses <- OBE_US_Doses %>% arrange(drug_class, pat_id, from_dt)
OBE_US_Doses <- OBE_US_Doses %>% inner_join(DANU_Demographics %>% filter(plan=="C") %>% select(patid), by=c("pat_id"="patid"))


# Injectable GLP1
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Injectable %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses %>% filter(drug_class=="GLP1 Injectable")
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% left_join(To_keep)
OBE_US_Doses_GLP1_Injectable <- OBE_US_Doses_GLP1_Injectable %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Injectable %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D      2695
# 2 P      9772

# Oral GLP1
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
Paid_dates_GLP1 <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_GLP1)[3] <- "Paid_dates"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(Paid_dates_GLP1)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_GLP1_Oral %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses %>% filter(drug_class=="GLP1 Oral")
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% left_join(To_keep)
OBE_US_Doses_GLP1_Oral <- OBE_US_Doses_GLP1_Oral %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_GLP1_Oral %>% group_by(paid) %>% count()

# paid      n
# <chr> <int>
#   1 D       398
# 2 P       774

# Antiobesity
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
Paid_dates_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Antiobesity)[3] <- "Paid_dates"
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(Paid_dates_Antiobesity)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Antiobesity %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Antiobesity <- OBE_US_Doses %>% filter(drug_class=="Antiobesity")
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% left_join(To_keep)
OBE_US_Doses_Antiobesity <- OBE_US_Doses_Antiobesity %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Antiobesity %>% group_by(paid) %>% count()

paid      n
<chr> <int>
  1 D      7226
2 P     14625


# Anorectic
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
Paid_dates_Anorectic <- OBE_US_Doses_Anorectic %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Anorectic)[3] <- "Paid_dates"
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(Paid_dates_Anorectic)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Anorectic %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Anorectic <- OBE_US_Doses %>% filter(drug_class=="Anorectic")
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% left_join(To_keep)
OBE_US_Doses_Anorectic <- OBE_US_Doses_Anorectic %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Anorectic %>% group_by(paid) %>% count()
# 
paid       n
<chr>  <int>
  1 D     147057
2 P     892934

# Weight_Loss
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
Paid_dates_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Weight_Loss)[3] <- "Paid_dates"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(Paid_dates_Weight_Loss)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Weight_Loss %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Weight_Loss <- OBE_US_Doses %>% filter(drug_class=="Weight Loss")
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% left_join(To_keep)
OBE_US_Doses_Weight_Loss <- OBE_US_Doses_Weight_Loss %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Weight_Loss %>% group_by(paid) %>% count()

paid      n
<chr> <int>
  1 D        73
2 P       268

# Surgery
OBE_US_Doses_Surgery <- OBE_US_Doses %>% filter(drug_class=="Surgery")
Paid_dates_Surgery <- OBE_US_Doses_Surgery %>% filter(paid=="P") %>% select(-paid)
names(Paid_dates_Surgery)[3] <- "Paid_dates"
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% left_join(Paid_dates_Surgery)
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% group_by(pat_id) %>% mutate(ElapsedTime=as.numeric(from_dt-Paid_dates))
To_keep <- OBE_US_Doses_Surgery %>% filter(paid=="D"&ElapsedTime<=0&ElapsedTime>=(-90)) %>% select(-c(Paid_dates, ElapsedTime)) %>% distinct()
To_keep$tokeep <- "Yes"
OBE_US_Doses_Surgery <- OBE_US_Doses %>% filter(drug_class=="Surgery")
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% left_join(To_keep)
OBE_US_Doses_Surgery <- OBE_US_Doses_Surgery %>% filter(!(paid=="D"&is.na(tokeep)))

OBE_US_Doses_Surgery %>% group_by(paid) %>% count()

paid      n
<chr> <int>
  1 D        29
2 P      2554

# -----------
# Geographic location US states patients -------------------

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(race=="African American")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid)

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% inner_join(DANU_Demographics)
DANU_Events <- DANU_Events %>% select(patid, prov)
DANU_Events <- DANU_Events %>% distinct()
DANU_Events <- DANU_Events %>% filter(prov!= "")

claims_provider <- fread("claims_provider.text")
claims_provider <- claims_provider %>% select(V1, V6)
names(claims_provider)[1] <- "prov"
names(claims_provider)[2] <- "state"

DANU_Events <- DANU_Events %>% left_join(claims_provider) %>% drop_na()


DANU_Events <- DANU_Events %>% group_by(patid, state) %>% count() %>% ungroup() 

# DANU_Events <- DANU_Events %>% group_by(patid, state) %>% count() %>% ungroup() %>%
#   group_by(patid) %>% filter(n==max(n)) 

# DANU_Events <- DANU_Events %>% ungroup() %>% group_by(patid) %>% sample_n(1)

length(unique(DANU_Events$patid)) # 104191

# Summary <- DANU_Events %>% ungroup() %>% group_by(state) %>% count() %>% arrange(-n)


# BMI
DANU_Events_BMI <- fread("DANU Events.txt")
DANU_Events_BMI <- DANU_Events %>% select(patid) %>% inner_join(DANU_Events_BMI)
DANU_Events_BMI <- DANU_Events_BMI %>% filter(grepl("BMI", code))
DANU_Events_BMI$code <- as.character(DANU_Events_BMI$code)
DANU_Events_BMI$code <- parse_number(DANU_Events_BMI$code)
DANU_Events_BMI <- DANU_Events_BMI %>% group_by(patid) %>% filter(code==mean(code))
DANU_Events_BMI <- DANU_Events_BMI %>% group_by(patid) %>% slice(n())
DANU_Events_BMI <- DANU_Events_BMI %>% select(patid, code)

DANU_Events <- DANU_Events %>% inner_join(DANU_Events_BMI)

Summary <- DANU_Events %>% ungroup() %>% group_by(state) %>% summarise(n=mean(code))




# us_state_look_up <- us_states %>% select(region) %>% distinct()
# fwrite(us_state_look_up, "us_state_look_up.csv")




us_state_look_up <- fread("us_state_look_up.csv")
us_state_look_up$Population <- parse_number(us_state_look_up$Population)



us_states <- map_data("state")

Summary %>% left_join(us_state_look_up) %>% drop_na() %>% 
  #mutate(percent=100*n/(177085)) %>% 
  #mutate(percent=percent*(percent/(100*PopPercent))) %>%
  #mutate(ratio=n/Population) %>%
  left_join(us_states) %>%
  ggplot(aes(x=long,y=lat,group=group, fill=n )) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())
  
  
# -------
# Geographic location US Divisions patients -------------------

DANU_Demographics <- fread("DANU Demographics.txt")
unique(DANU_Demographics$division)
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, division)
DANU_Demographics <- DANU_Demographics %>% filter(division != "?")


# DANU_Demographics <- DANU_Demographics %>% group_by(division) %>% count() %>% ungroup() 
# 
# length(unique(DANU_Demographics$patid)) # 967188
# 
# Summary <- DANU_Demographics

# BMI
DANU_Events_BMI <- fread("DANU Events.txt")
DANU_Events_BMI <- DANU_Demographics %>% select(patid) %>% inner_join(DANU_Events_BMI, by=c("patid"="patid"))
DANU_Events_BMI <- DANU_Events_BMI %>% filter(grepl("BMI", code))
DANU_Events_BMI$code <- as.character(DANU_Events_BMI$code)
DANU_Events_BMI$code <- parse_number(DANU_Events_BMI$code)
DANU_Events_BMI <- DANU_Events_BMI %>% group_by(patid) %>% filter(code==meax(code))
DANU_Events_BMI <- DANU_Events_BMI %>% group_by(patid) %>% slice(n())
DANU_Events_BMI <- DANU_Events_BMI %>% select(patid, code)

DANU_Demographics <- DANU_Demographics %>% inner_join(DANU_Events_BMI)


DANU_Demographics <- DANU_Demographics %>% group_by(division) %>% summarise(n=mean(code))

Summary <- DANU_Demographics


us_state_look_up <- fread("us_state_look_up.csv")
us_state_look_up$Population <- parse_number(us_state_look_up$Population)

us_states <- map_data("state")

Summary %>% left_join(us_state_look_up) %>% drop_na() %>% 
  #mutate(percent=100*n/(967188)) %>% 
  #mutate(percent=percent*(percent/(100*PopPercent))) %>%
  #mutate(ratio=n/Population) %>%
  left_join(us_states) %>%
  ggplot(aes(x=long,y=lat,group=group, fill=n )) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())


# -------
# NEW Obesity Dx over time ----------------

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
names(DANU_Events)[1] <- "patient"
DANU_Events %>% group_by()
DANU_Events <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Events, by=c("patid"="patient"))
DANU_Events <- DANU_Events %>% filter(!grepl("BMI", code))  


DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(source=="Diagnosis")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code)

DANU_Events <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes)

DANU_Events <- DANU_Events %>% group_by(patid) %>% slice(1)

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

setDT(DANU_Events)[, Month_Yr := format(as.Date(claimed), "%Y-%m") ]

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>%  filter(!is.na(Exact_Month))

temp <- DANU_Events %>% left_join(DANU_Demographics %>% select(patid, weight_2)) %>%
  group_by(Month_Yr) %>% summarise(n=sum(weight_2))


# --------
# BMI over time -----------


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")

# BMI
DANU_Events_BMI <- fread("DANU Events.txt")
DANU_Events_BMI <- DANU_Demographics %>% select(patid) %>% inner_join(DANU_Events_BMI, by=c("patid"="patid"))
DANU_Events_BMI <- DANU_Events_BMI %>% filter(grepl("BMI", code))
DANU_Events_BMI$code <- as.character(DANU_Events_BMI$code)
DANU_Events_BMI$code <- parse_number(DANU_Events_BMI$code)
DANU_Events_BMI <- DANU_Events_BMI %>% select(patid, code, claimed)
DANU_Demographics <- DANU_Demographics %>% select(-c(weight, weight_BMI, diagnosis))
DANU_Demographics <- DANU_Demographics %>% inner_join(DANU_Events_BMI)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

setDT(DANU_Demographics)[, Month_Yr := format(as.Date(claimed), "%Y-%m") ]

DANU_Demographics <- DANU_Demographics %>% left_join(Months_lookup, by = c("Month_Yr" = "Month")) %>%  filter(!is.na(Exact_Month))

temp2 <- DANU_Demographics %>% group_by(Month_Yr) %>% summarise(n=mean(code))

temp2 %>% ggplot(aes(x=Month_Yr, y=n))+
  geom_point(size=3, alpha=0.8, colour="deepskyblue4")+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\n Month")+ylab("Average BMI (kg/m2) \n")
# -----

# GLP1 Brands -----------
OBE_Drug_Histories     <- fread("OBE GLP1 Brand Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)

Pats_1_brand <- OBE_Drug_Histories %>% filter(Treat!="-") %>% select(patient, Treat) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n==1)

Pats_1_brand <- Pats_1_brand %>% left_join(OBE_Drug_Histories) %>% filter(Treat != "-") %>%
  select(-c(n, weight, Month)) %>% distinct()

Pats_1_brand <- Pats_1_brand %>% mutate(Treat=ifelse(Treat=="336", "Victoza",
                                                     ifelse(Treat=="335","Saxenda",
                                                            ifelse(Treat=="327","Rybelsus",
                                                                   ifelse(Treat=="340","Ozempic",
                                                                          ifelse(Treat=="341", "Wegovy", NA)))))) %>% drop_na()


unique(Pats_1_brand$Treat)

# [1] "Victoza"  "Saxenda"  "Ozempic"  "Rybelsus"

OBE_Flows_Aux._Long <- fread("OBE_Flows_Aux._Long_v2.txt")

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2) %>% filter(!is.na(weight_2))
names(DANU_Demographics)[1] <- "patient"

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% left_join(DANU_Demographics)
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(-c(disease, weight))

OBE_Flows_Aux._Long <- Pats_1_brand %>% left_join(OBE_Flows_Aux._Long)

# Victoza

OBE_Flows_Aux._Long %>% filter(Treat=="Victoza") %>% filter(flow==1) %>% filter(s1!="G"&s2=="G") %>%
  group_by(s1) %>% summarise(n=sum(weight_2))

# s1         n
# <chr>  <dbl>
# 1 a      3173.
# 2 H       424.
# 3 o       357.
# 4 x     41164.

OBE_Flows_Aux._Long %>% filter(Treat=="Victoza") %>% filter(flow==1) %>% filter(s1=="G"&s2!="G") %>%
  group_by(s2) %>% summarise(n=sum(weight_2))

# s2         n
# <chr>  <dbl>
# 1 a      4930.
# 2 H       275.
# 3 x     40966.


# Saxenda

OBE_Flows_Aux._Long %>% filter(Treat=="Saxenda") %>% filter(flow==1) %>% filter(s1!="G"&s2=="G") %>%
  group_by(s1) %>% summarise(n=sum(weight_2))

# s1          n
# <chr>   <dbl>
# 1 a      29025.
# 2 H        147.
# 3 o       5005.
# 4 x     249123.

OBE_Flows_Aux._Long %>% filter(Treat=="Saxenda") %>% filter(flow==1) %>% filter(s1=="G"&s2!="G") %>%
  group_by(s2) %>% summarise(n=sum(weight_2))

# s2          n
# <chr>   <dbl>
# 1 a      30779.
# 2 H        419.
# 3 o       6287.
# 4 x     216336.



# Ozempic

OBE_Flows_Aux._Long %>% filter(Treat=="Ozempic") %>% filter(flow==1) %>% filter(s1!="G"&s2=="G") %>%
  group_by(s1) %>% summarise(n=sum(weight_2))

# s1         n
# <chr>  <dbl>
# 1 a      6526.
# 2 o       699.
# 3 x     52960.

OBE_Flows_Aux._Long %>% filter(Treat=="Ozempic") %>% filter(flow==1) %>% filter(s1=="G"&s2!="G") %>%
  group_by(s2) %>% summarise(n=sum(weight_2))

# s2         n
# <chr>  <dbl>
# 1 a      5086.
# 2 x     28053.



# Rybelsus

OBE_Flows_Aux._Long %>% filter(Treat=="Rybelsus") %>% filter(flow==1) %>% filter(s1!="g"&s2=="g") %>%
  group_by(s1) %>% summarise(n=sum(weight_2))

# s1         n
# <chr>  <dbl>
# 1 a      2246.
# 2 x     28171.

OBE_Flows_Aux._Long %>% filter(Treat=="Rybelsus") %>% filter(flow==1) %>% filter(s1=="g"&s2!="g") %>%
  group_by(s2) %>% summarise(n=sum(weight_2))

# s2         n
# <chr>  <dbl>
# 1 a      1582.
# 2 x     19740.
# -------
# Nr comorbidities vs BMI  -----------

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677

# BMI MAX Bucket
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code))
DANU_Events <- DANU_Events %>% group_by(patid) %>% select(-c(claimed)) %>% distinct()
DANU_Events <- DANU_Events %>% ungroup() %>% mutate(BMI_group = ifelse(code<25, "<25",
                                                                       ifelse(code>25&code<=27, "25_to_27",
                                                                              ifelse(code>27&code<=30, "27_to_30",
                                                                                     ifelse(code>30&code<=40, "30_to_40", ">40"))))) 


DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Events)

sum(DANU_Demographics$weight_2, na.rm=T) # 132236677
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)


DANU_Demographics %>% filter(!is.na(BMI_group)) %>% group_by(BMI_group) %>% summarise(n=sum(weight_2)) 

#  with BMI

# BMI_group         n
# <chr>         <dbl>
# 1 <25        2846529.
# 2 >40       13648335.
# 3 "25_to_27"  17829957. (20676486 with 25-)
# 4 27_to_30  24087801.
# 5 30_to_40  37866083.




OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
names(OBE_Comorbidity_Inventories)[1] <- "patient"

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% select(-weight) %>% left_join(DANU_Demographics)
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% drop_na()
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% mutate(BMI_group=ifelse(BMI_group=="<25", "25_to_27", BMI_group))


temp <- OBE_Comorbidity_Inventories %>% filter(grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|grepl("H", diagnosis)|
                                                 grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|grepl("M", diagnosis)|
                                                 grepl("N", diagnosis)) %>% select(patient, diagnosis, weight_2, code) %>% distinct() %>%
  group_by(patient) %>% mutate(UniqueDxs = n()) %>% select(patient, code, UniqueDxs) %>% distinct()

plot <- temp %>% ungroup() %>% filter(code>25) %>% group_by(code) %>% mutate(n=mean(UniqueDxs))  %>% ggplot(aes(x=code, y=n)) 

lm(uniqueDxs~code, data=temp)



plot +  geom_jitter(colour="midnightblue", alpha=0.5, size=3) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n BMI (kg/m2)")+
  ylab("Average Number of Unique/Different Comorbidities \n")

# ----------





# Treat Experienced VS Naive


OBE_US_Doses <- fread("OBE Doses.txt") 
Amphet_pats <- OBE_US_Doses %>% filter(drug_class == "Anorectic") %>% select(pat_id) %>% distinct()
names(Amphet_pats)[1] <- "patient"


Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

sum(Treatment_exp_Vector$weight) # 9155116
Treatment_exp_Vector$Treat_Exp <- "Treat_Exp"

OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) 
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% left_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% mutate(Treat_Exp = ifelse(is.na(Treat_Exp),"Naive",Treat_Exp))

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% inner_join(DANU_Demographics_BMI27, by=c("patid"="patient"))

OBE_Comorbidity_Inventories %>% select(patid, weight_2) %>% distinct() %>% summarise(n=sum(weight_2)) 



OBE_Comorbidity_Inventories %>% select(patid, weight_2, Treat_Exp) %>% distinct() %>% group_by(Treat_Exp) %>% summarise(n=sum(weight_2))

# Treat_Exp         n
# <chr>         <dbl>
# 1 Naive     96456936.
# 2 Treat_Exp  1088270.


OBE_Comorbidity_Inventories <-OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))


Pats_Treat_exp <- OBE_Comorbidity_Inventories %>% filter(Treat_Exp=="Treat_Exp")
Pats_Naive <- OBE_Comorbidity_Inventories %>% filter(Treat_Exp=="Naive")


Pats_Treat_exp <- Pats_Treat_exp %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/8525008) %>% select(-n) 
names(Pats_Treat_exp)[1] <- "Last"
names(Pats_Treat_exp)[2] <- "Penetrance_Experienced"


Pats_Naive <- Pats_Naive %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/94911374) %>% select(-n)
names(Pats_Naive)[1] <- "Last"
names(Pats_Naive)[2] <- "Penetrance_Naive"

Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Pats_Treat_exp %>% full_join(Pats_Naive) %>% drop_na() %>% 
  mutate(Difference=Penetrance_Experienced-Penetrance_Naive) %>%
  mutate(fol_change = Penetrance_Experienced/Penetrance_Naive)


Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% filter(Difference>3|Difference< (-3))
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% select(Penetrance_Experienced, Penetrance_Naive, Last)
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity$Last <- gsub('[^[:alnum:] ]','',Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity$Last)


Pats_Treat_exp <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% select(Penetrance_Experienced, Last) %>% mutate(Group="Experienced") 
names(Pats_Treat_exp)[1] <- "Penetrance"

Pats_Naive <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% select(Penetrance_Naive, Last) %>% mutate(Group="Naive")
names(Pats_Naive)[1] <- "Penetrance"

Obesity_circular <- Pats_Treat_exp %>% bind_rows(Pats_Naive)

Obesity_circular <- Obesity_circular %>% mutate(Last=as.factor(Last), Group=as.factor(Group))
Obesity_circular <- data.frame(Obesity_circular)

row_num = length(levels(Obesity_circular$Group))

g = ggplot(Obesity_circular,aes(x=Last,y=as.numeric(Group),fill=Penetrance)) + 
  xlim(c("",as.vector(unique(Obesity_circular$Last)))) + 
  ylim(c(-row_num/1.5,row_num+1))+
  geom_tile()+ ylab("")+
  scale_fill_gradientn(colours=wes_palette("Zissou1", 116, type = "continuous"))+
  annotate(x="",y=1:row_num,label=levels(Obesity_circular$Group),size=4,geom="text", colour="firebrick") 

g + coord_polar(start=-0.15,) + theme_bw() + theme(axis.text = element_text(size = 8)) 







Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% mutate(Difference=Penetrance_Experienced -Penetrance_Naive )
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% select(Difference, Last)
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% filter(Difference>3 | Difference<(-3))
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% mutate(Group = str_sub(Last, 1L, 1L))

Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity$Last <- gsub('[^[:alnum:] ]','',Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity$Last)

empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity$Group), ncol(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity)) )
colnames(to_add) <- colnames(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity)
to_add$group <- rep(levels(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity$Group), each=empty_bar)
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- rbind(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity, to_add)
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity %>% arrange(Group)
Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity$id <- seq(1, nrow(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity))

label_data <- Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


ggplot(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity, aes(x=as.factor(id), y=Difference, fill=Group)) +  
  geom_bar(stat="identity", alpha=0.5) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  coord_polar() + 
  scale_fill_viridis_d()+
  geom_text(data=label_data, aes(x=id, y=Difference+4, label=Last , hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, inherit.aes = FALSE ) 


fwrite(Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity, "Circular_Bar_Chart_Dx_TreatExpVSNaive_Obesity_NoAMPHETAMINES.txt", sep="\t") 
 

# How many with bariatric surgery every year ? -----------------
OBE_Drug_Histories <- fread("OBE_Drug_Histories_v2.txt")
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(grepl('54',Treat))

OBE_Drug_Histories <- OBE_Drug_Histories %>% mutate(Year=ifelse(Month<13,"Year1",
                                                                ifelse(Month<25,"Year2",
                                                                       ifelse(Month<37,"Year3",
                                                                              ifelse(Month<49,"Year4","Year5")))))


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2)

OBE_Drug_Histories %>% select(-weight) %>% left_join(DANU_Demographics, by=c("patient"="patid")) %>%
  select(patient, Year, weight_2) %>% distinct() %>% group_by(Year) %>%
  summarise(n=sum(weight_2))

# ----------
# Get patient with BMI & drugs ------------
OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

OBE_Disorder_Histories <- Treatment_exp_Vector %>% left_join(OBE_Drug_Histories)



DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- Treatment_exp_Vector %>% select(patient) %>% left_join(DANU_Events, by=c("patient"="patid"))
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code)) 




# -----------
# How many on amphetamines have ICD10s that justify it ? ---------------
OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
length(unique(OBE_Comorbidity_Inventories$patid)) # 721893
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% filter(grepl("F9", diagnosis))
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% select(patid) %>% distinct()

OBE_Comorbidity_Inventories$ICD <- "mental"
names(OBE_Comorbidity_Inventories)[1] <- "patient"

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

OBE_Box_Histories <- fread("OBE Box Histories.txt")
OBE_Box_Histories <- OBE_Box_Histories %>% select(1,2,63)
OBE_Box_Histories <- OBE_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

OBE_Box_Histories <- Treatment_exp_Vector %>% inner_join(OBE_Box_Histories)


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2)

OBE_Box_Histories <- OBE_Box_Histories %>% left_join(DANU_Demographics, by=c("patient"="patid"))

sum(OBE_Box_Histories$weight_2)

OBE_Box_Histories %>% group_by(month60) %>% summarise(n=sum(weight_2))



OBE_Box_Histories <- OBE_Box_Histories %>% left_join(OBE_Comorbidity_Inventories) %>% 
  mutate(ICD=ifelse(is.na(ICD),"0",ICD)) %>%
  mutate(month60 = ifelse(month60=="a"&ICD=="mental","a2",month60)) %>% group_by(month60) %>% summarise(n=sum(weight_2))


# ------------

# -----
# Age & BMI GLP1 exp vs naive --------------

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705


Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")
Cum_Class_Experience_EveryMonth

Cum_Class_Experience_EveryMonth %>% filter(p2==60)
Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))
GLP1_Exp_Pats <- Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient)
Naive_Pats <- OBE_Drug_Histories %>% anti_join(Treatment_exp_Vector)




GLP1_Exp_Pats <- GLP1_Exp_Pats %>% select(patient) %>% distinct()
Naive_Pats <- Naive_Pats %>% select(patient) %>% distinct()

DANU_Demographics
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)
names(DANU_Demographics)[1] <- "patient"

GLP1_Exp_Pats
GLP1_Exp_Pats %>% inner_join(DANU_Demographics)

GLP1_Exp_Pats <- GLP1_Exp_Pats %>% inner_join(DANU_Demographics)
Naive_Pats <- Naive_Pats %>% inner_join(DANU_Demographics)

temp <- fread("DANU Demographics.txt")
temp <- temp %>% select(patid, gender, age)
temp
names(temp)[1] <- "patient"

GLP1_Exp_Pats %>% left_join(temp)
GLP1_Exp_Pats <- GLP1_Exp_Pats %>% left_join(temp)
Naive_Pats <- Naive_Pats %>% left_join(temp)

weighted.mean(GLP1_Exp_Pats$age, GLP1_Exp_Pats$weight_2) # 47.6972
weighted.mean(Naive_Pats$age, Naive_Pats$weight_2) #  50.01593

GLP1_Exp_Pats %>% group_by(gender) %>% summarise(n=sum(weight_2))

# gender       n
# <chr>    <dbl>
# 1 F      257678.
# 2 M       69651.


Naive_Pats %>% group_by(gender) %>% summarise(n=sum(weight_2))

# gender         n
# <chr>      <dbl>
# 1 F      47278571.
# 2 M      48016564




# BMI
DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"
GLP1_Exp_Pats <- GLP1_Exp_Pats %>% left_join(DANU_Events)
Naive_Pats <- Naive_Pats %>% left_join(DANU_Events)
GLP1_Exp_Pats <- GLP1_Exp_Pats %>% filter(grepl("BMI", code))
GLP1_Exp_Pats$code <- as.character(GLP1_Exp_Pats$code)
GLP1_Exp_Pats$code <- parse_number(GLP1_Exp_Pats$code)
GLP1_Exp_Pats <- GLP1_Exp_Pats %>% group_by(patient) %>% slice(n())


GLP1_Exp_Pats %>% ungroup() %>% summarise(n=weighted.mean(code, weight_2)) #36.6

GLP1_Exp_Pats %>%
  ggplot(aes(code))+
  geom_density(alpha=0.9, show.legend = F, colour="black", fill="deepskyblue4")+
  theme(panel.background = element_blank())+
  xlim(20,70)+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")


Naive_Pats <- Naive_Pats %>% filter(grepl("BMI", code))
Naive_Pats$code <- as.character(Naive_Pats$code)
Naive_Pats$code <- parse_number(Naive_Pats$code)
Naive_Pats <- Naive_Pats %>% group_by(patient) %>% slice(n())
Naive_Pats %>% ungroup() %>% summarise(n=weighted.mean(code, weight_2)) #32.2

Naive_Pats %>%
  ggplot(aes(code))+
  geom_density(alpha=0.9, show.legend = F, colour="black", fill="darksalmon")+
  theme(panel.background = element_blank())+
  xlim(20,70)+
  ylab("Number of Patients\n")+
  xlab("\n BMI (kg/m2)")




# Comorbidities
GLP1_Exp_Pats
Naive_Pats


OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories <-OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))


GLP1_Exp_Pats <- GLP1_Exp_Pats %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid"))
Naive_Pats <- Naive_Pats %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) 


GLP1_Exp_Pats <- GLP1_Exp_Pats %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/327329) %>% select(-n)
names(GLP1_Exp_Pats)[1] <- "Last"
names(GLP1_Exp_Pats)[2] <- "Penetrance_p1_InjExp"


Naive_Pats <- Naive_Pats %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/95295135) %>% select(-n)
names(Naive_Pats)[1] <- "Last"
names(Naive_Pats)[2] <- "Penetrance_p1_NO_InjExp"

Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- GLP1_Exp_Pats %>% full_join(Naive_Pats) %>% drop_na() %>% 
  mutate(Difference=Penetrance_p1_InjExp-Penetrance_p1_NO_InjExp) %>%
  mutate(fol_change = Penetrance_p1_InjExp/Penetrance_p1_NO_InjExp)


Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% filter(Difference>2|Difference< (-2))
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% select(Penetrance_p1_InjExp, Penetrance_p1_NO_InjExp, Last)
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes$Last <- gsub('[^[:alnum:] ]','',Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes$Last)


p1_InjExp <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% select(Penetrance_p1_InjExp, Last) %>% mutate(Group="GLP1 Exp") 
names(p1_InjExp)[1] <- "Penetrance"

p1_NO_InjExp <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% select(Penetrance_p1_NO_InjExp, Last) %>% mutate(Group="No GLP1 Exp")
names(p1_NO_InjExp)[1] <- "Penetrance"

Diabetes_circular <- p1_InjExp %>% bind_rows(p1_NO_InjExp)

Diabetes_circular <- Diabetes_circular %>% mutate(Last=as.factor(Last), Group=as.factor(Group))
Diabetes_circular <- data.frame(Diabetes_circular)

Diabetes_circular <- Diabetes_circular %>% mutate(Group=ifelse(Group=="GLP1 Exp","GLP1 exp","Treat naive"))

Diabetes_circular$Group <- as.factor(Diabetes_circular$Group)
row_num = length(levels(Diabetes_circular$Group))

g = ggplot(Diabetes_circular,aes(x=Last,y=as.numeric(Group),fill=Penetrance)) + 
  xlim(c("",as.vector(unique(Diabetes_circular$Last)))) + 
  ylim(c(-row_num/1.5,row_num+1))+
  geom_tile()+ ylab("")+
  scale_fill_gradientn(colours=wes_palette("Zissou1", 116, type = "continuous"))+
  annotate(x="",y=1:row_num,label=levels(Diabetes_circular$Group),size=4,geom="text", colour="firebrick") 

g + coord_polar(start=-0.15,) + theme_bw() + theme(axis.text = element_text(size = 8)) 







Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% mutate(Difference=Penetrance_p1_InjExp-Penetrance_p1_NO_InjExp)
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% select(Difference, Last)
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% filter(Difference>2 | Difference<(-2))
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% mutate(Group = str_sub(Last, 1L, 1L))

Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes$Last <- gsub('[^[:alnum:] ]','',Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes$Last)

empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes$Group), ncol(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes)) )
colnames(to_add) <- colnames(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes)
to_add$group <- rep(levels(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes$Group), each=empty_bar)
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- rbind(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes, to_add)
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes %>% arrange(Group)
Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes$id <- seq(1, nrow(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes))

label_data <- Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


ggplot(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes, aes(x=as.factor(id), y=Difference, fill=Group)) +  
  geom_bar(stat="identity", alpha=0.5) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  coord_polar() + 
  scale_fill_viridis_d()+
  geom_text(data=label_data, aes(x=id, y=Difference+4, label=Last , hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, inherit.aes = FALSE ) 


fwrite(Circular_Bar_Chart_Dx_Pen_GLPqVSNoGLP1_Diabetes, "Circular_Bar_Chart_Dx_Pen_GLP1_VS_TreatNaive_Obesity.csv") 



# Number of events - ANY



DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705


Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")
Cum_Class_Experience_EveryMonth

Cum_Class_Experience_EveryMonth %>% filter(p2==60)
Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))
GLP1_Exp_Pats <- Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient)
Naive_Pats <- OBE_Drug_Histories %>% anti_join(Treatment_exp_Vector)




GLP1_Exp_Pats <- GLP1_Exp_Pats %>% select(patient) %>% distinct()
Naive_Pats <- Naive_Pats %>% select(patient) %>% distinct()



DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"

GLP1_Exp_Pats %>% left_join(DANU_Events) %>% group_by(patient) %>% count() %>% ungroup() %>% summarise(MeanEvents=mean(n)) # 20.4


GLP1_Exp_Pats %>% left_join(DANU_Events) %>% group_by(patient) %>% count() %>% ungroup() %>%
  ggplot(aes(n))+
  geom_density(alpha=0.9, show.legend = F, colour="black", fill="deepskyblue4")+
  theme(panel.background = element_blank())+
  xlim(0,100)+
  ylab("Proportion of Patients\n")+
  xlab("\n Number of recorded medical events (diagnoses/BMI records/procedures)")

Naive_Pats %>% left_join(DANU_Events) %>% group_by(patient) %>% count() %>% ungroup() %>% summarise(MeanEvents=mean(n)) # 10.6



Naive_Pats %>% left_join(DANU_Events) %>% group_by(patient) %>% count() %>% ungroup() %>%
  ggplot(aes(n))+
  geom_density(alpha=0.9, show.legend = F, colour="black", fill="darksalmon")+
  theme(panel.background = element_blank())+
  xlim(0,100)+
  ylab("Proportion of Patients\n")+
  xlab("\n Number of recorded medical events (diagnoses/BMI records/procedures)")







# Physicians 

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705


Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")
Cum_Class_Experience_EveryMonth

Cum_Class_Experience_EveryMonth %>% filter(p2==60)
Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))
GLP1_Exp_Pats <- Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient)
Naive_Pats <- OBE_Drug_Histories %>% anti_join(Treatment_exp_Vector)


GLP1_Exp_Pats <- GLP1_Exp_Pats %>% select(patient) %>% distinct()
Naive_Pats <- Naive_Pats %>% select(patient) %>% distinct()



DANU_Events <- fread("DANU Events.txt")
DANU_Event_Claims_Providers <- fread("DANU Event Claims Providers.txt")

Unique_Physicians_OBE <- fread("Unique_Physicians_OBE.txt")
DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")


temp1 <- GLP1_Exp_Pats  %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

sum(temp1$n)

# SUMMARY_SPECIALTY      n
# <chr>              <int>
# 1 NA                 22154
# 2 GP                  8091
# 3 OTHER HCP           4662
# 4 INTERNAL MEDICINE   4350
# 5 OTHER PHYSICIAN     3280
# 6 UNKNOWN             2989
# 7 FACILITY            2809
# 8 SURGERY             2780
# 9 ENDOCRINOLOGY       1659
# 10 CARDIOLOGY           819
# 11 EMERGENCY MEDICINE   463
# 12 GASTRO/HEPATO        432
# 13 PATHOLOGY            282
# 14 HEMATO/ONCOL         257
# 15 RADIOLOGY            167
# 16 NEUROLOGY            141
# 17 PSYCHIATRY            95
# 18 NEPHROLOGIST          68
# 19 MEDICAL ONCOLOGIST    22

temp2 <- Naive_Pats  %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

# SUMMARY_SPECIALTY        n
# <chr>                <int>
# 1 NA                 4847073
# 2 GP                  757010
# 3 INTERNAL MEDICINE   454749
# 4 OTHER PHYSICIAN     331613
# 5 OTHER HCP           245596
# 6 FACILITY            202425
# 7 UNKNOWN             170304
# 8 SURGERY             120837
# 9 CARDIOLOGY           87766
# 10 EMERGENCY MEDICINE   38145
# 11 GASTRO/HEPATO        32309
# 12 ENDOCRINOLOGY        31904
# 13 HEMATO/ONCOL         18087
# 14 PATHOLOGY            17680
# 15 NEUROLOGY            15270
# 16 PSYCHIATRY           11061
# 17 NEPHROLOGIST          9455
# 18 RADIOLOGY             8261
# 19 MEDICAL ONCOLOGIST    5953

sum(temp2$n)

names(temp2)[2] <- "N_naive"

temp <- temp1 %>% full_join(temp2)

fwrite(temp, "Physicians_GLP1vsNaive.csv")

# -----------
# Compare demographics of the 4 groups of identified pats (drugs, procedures, obesity dxs, bmi dxs, bmi records) --------------------

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)


# Treatment experienced vector
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)
Treatment_exp_Vector %>% inner_join(DANU_Demographics) %>% summarise(n=sum(weight_2)) # 8543570
Treatment_exp_Vector <- Treatment_exp_Vector %>% inner_join(DANU_Demographics) 
Treatment_exp_Vector$Group <- "Treat_exp"

# Procedures
DANU_Dossiers <- fread("DANU Dossiers.txt")

DANU_Dossiers %>%  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% filter(grepl("P=",code)) %>% select(patid) %>% distinct() %>%
  inner_join(DANU_Demographics, by=c("patid"="patient")) %>% summarise(n=sum(weight_2))    # 4565829

Procedures_Pats <- DANU_Dossiers %>%  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% filter(grepl("P=",code)) %>% select(patid) %>% distinct() %>%
  inner_join(DANU_Demographics, by=c("patid"="patient")) %>% distinct()
names(Procedures_Pats)[1] <- "patient"
Procedures_Pats$Group <- "Procedures"



# Obesity Dx Pats
DANU_Events <- fread("DANU Events.txt")

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity"&source=="Diagnosis")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(!grepl("V",code)&!grepl("Z",code))

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code)
DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% distinct() %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% distinct() %>% summarise(n=sum(weight_2)) # 36166190

Obesity_Dx_Pats <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% distinct() 
names(Obesity_Dx_Pats)[1] <- "patient"
Obesity_Dx_Pats$Group <- "Obesity_Dx"


# Obesity BMI Dx codes
DANU_Events <- fread("DANU Events.txt")
DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(!grepl("BMI",code)) %>% filter(grepl("V",code)|grepl("Z",code)) %>% select(code)

DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% anti_join(Obesity_Dx_Pats, by=c("patid"="patient")) %>% 
  distinct() %>% summarise(n=sum(weight_2)) # 16178029

Obesity_BMI_Dx <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% anti_join(Obesity_Dx_Pats, by=c("patid"="patient")) %>% 
  distinct()
names(Obesity_BMI_Dx)[1] <- "patient"
Obesity_BMI_Dx$Group <- "BMI_Dx"


Treatment_exp_Vector %>% bind_rows(Procedures_Pats) %>% bind_rows(Obesity_Dx_Pats) %>% bind_rows(Obesity_BMI_Dx) %>%
  full_join(DANU_Demographics) %>% group_by(Group) %>% summarise(n=sum(weight_2))


Groups_to_compare <- Treatment_exp_Vector %>% bind_rows(Procedures_Pats) %>% bind_rows(Obesity_Dx_Pats) %>% bind_rows(Obesity_BMI_Dx) %>%
  full_join(DANU_Demographics)

Groups_to_compare <- Groups_to_compare %>% mutate(Group=ifelse(is.na(Group),"BMI_record",Group))





temp <- fread("DANU Demographics.txt")
temp <- temp %>% select(patid, gender, age)
temp
names(temp)[1] <- "patient"


Groups_to_compare <- Groups_to_compare %>% left_join(temp)

Groups_to_compare %>% group_by(Group) %>% summarise(n=weighted.mean(age, weight_2))

# Group          n
# <chr>      <dbl>
# 1 BMI_Dx      51.4
# 2 BMI_record  47.8
# 3 Obesity_Dx  51.3
# 4 Procedures  53.6
# 5 Treat_exp   40.7

Groups_to_compare %>% group_by(Group, gender) %>% summarise(n=sum(weight_2))

# Group      gender         n
# <chr>      <chr>      <dbl>
# 1 BMI_Dx     F       7798981.
# 2 BMI_Dx     M       8379048.
# 3 BMI_record F      17132342.
# 4 BMI_record M      21252745.
# 5 Obesity_Dx F      19959512.
# 6 Obesity_Dx M      16206678.
# 7 Procedures F       2387736.
# 8 Procedures M       2178093.
# 9 Treat_exp  F       5052329.
# 10 Treat_exp  M       3491241.



# BMI
DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"
Groups_to_compare <- Groups_to_compare %>% left_join(DANU_Events)
Groups_to_compare <- Groups_to_compare %>% filter(grepl("BMI", code))
Groups_to_compare$code <- as.character(Groups_to_compare$code)
Groups_to_compare$code <- parse_number(Groups_to_compare$code)
Groups_to_compare <- Groups_to_compare %>% group_by(patient) %>% slice(n())

Groups_to_compare %>% group_by(Group) %>% summarise(n=weighted.mean(code, weight_2))

# Group          n
# <chr>      <dbl>
# 1 BMI_Dx      30.6
# 2 BMI_record  30.5
# 3 Obesity_Dx  34.8
# 4 Procedures  32.3
# 5 Treat_exp   33.6



Groups_to_compare$Group <- factor(Groups_to_compare$Group, levels=c('Treat_exp', 'Obesity_Dx', 'Procedures','BMI_Dx','BMI_record'))


Groups_to_compare %>% 
  ggplot(aes(x = code, y = Group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  xlim(20,50) +
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n BMI (Last)") + ylab("Patient Identification Group \n")




DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"
Groups_to_compare <- Groups_to_compare %>% select(patient, weight_2,Group)
Groups_to_compare %>% left_join(DANU_Events) %>% group_by(Group, patient) %>% count() %>% ungroup() %>% group_by(Group) %>% summarise(MeanEvents=mean(n))

# Group      MeanEvents
# <chr>           <dbl>
# 1 BMI_Dx          12.8 
# 2 BMI_record       8.61
# 3 Obesity_Dx      18.7 
# 4 Procedures      16.3 
# 5 Treat_exp       20.2 




Groups_to_compare %>% left_join(DANU_Events) %>% group_by(Group, patient) %>% count() %>% ungroup() %>% 
  ggplot(aes(x = n, y = Group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  xlim(0,50) +
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n No. Events") + ylab("Patient Identification Group \n")

Groups_to_compare <- Groups_to_compare %>% select(patient, weight_2,Group)


# Comorbidities 

OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories <-OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))




Treat_exp <- Groups_to_compare %>% filter(Group=="Treat_exp") %>% select(-Group)
Procedures <- Groups_to_compare %>% filter(Group=="Procedures") %>% select(-Group)
Obesity_Dx <- Groups_to_compare %>% filter(Group=="Obesity_Dx") %>% select(-Group)
BMI_record <- Groups_to_compare %>% filter(Group=="BMI_record") %>% select(-Group)
BMI_Dx <- Groups_to_compare %>% filter(Group=="BMI_Dx") %>% select(-Group)


Treat_exp %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 8543570
Procedures %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 4565829
Obesity_Dx %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 36166190
BMI_record %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 38385087
BMI_Dx %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 16178029



Treat_exp <- Treat_exp %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
Procedures <- Procedures %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
Obesity_Dx <- Obesity_Dx %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_record <- BMI_record %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_Dx <- BMI_Dx %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)



Treat_exp <- Treat_exp %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/8543570) %>% select(-n)
names(Treat_exp)[1] <- "Last"
names(Treat_exp)[2] <- "Penetrance_Treat_exp"


Procedures <- Procedures %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/4565829) %>% select(-n)
names(Procedures)[1] <- "Last"
names(Procedures)[2] <- "Penetrance_Procedures"


Obesity_Dx <- Obesity_Dx %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/36166190) %>% select(-n)
names(Obesity_Dx)[1] <- "Last"
names(Obesity_Dx)[2] <- "Penetrance_Obesity_Dx"


BMI_record <- BMI_record %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/38385087) %>% select(-n)
names(BMI_record)[1] <- "Last"
names(BMI_record)[2] <- "Penetrance_BMI_record"


BMI_Dx <- BMI_Dx %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/16178029) %>% select(-n)
names(BMI_Dx)[1] <- "Last"
names(BMI_Dx)[2] <- "Penetrance_BMI_Dx"

Treat_exp <- Treat_exp %>% filter(Penetrance_Treat_exp>5)
Procedures  <- Procedures %>% filter(Penetrance_Procedures>5)
Obesity_Dx  <- Obesity_Dx %>% filter(Penetrance_Obesity_Dx>5)
BMI_record  <- BMI_record %>% filter(Penetrance_BMI_record>5)
BMI_Dx  <- BMI_Dx %>% filter(Penetrance_BMI_Dx>5)

temp <- Treat_exp %>% full_join(Procedures) %>% full_join(Obesity_Dx) %>% full_join(BMI_Dx) %>% full_join(BMI_record)

temp <- mutate_if(temp, is.numeric, ~replace(., is.na(.), 0))

temp <- temp %>% arrange(Last)
rownames(temp) <- temp$Last
temp2 <- temp %>% select(-Last)

rownames(temp2) 
rownames(temp2) <- rownames(temp)
temp2 <- as.matrix(temp2)


my_colors <- colorRampPalette(c("cornsilk", "deepskyblue4"))  
heatmap(temp2, Rowv = NA, Colv = NA, col = my_colors(1000))    






# Using 1 ICD10 code digit


OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories <-OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% mutate(diagnosis = str_sub(diagnosis, 1L, 1L))
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% distinct()


Treat_exp <- Groups_to_compare %>% filter(Group=="Treat_exp") %>% select(-Group)
Procedures <- Groups_to_compare %>% filter(Group=="Procedures") %>% select(-Group)
Obesity_Dx <- Groups_to_compare %>% filter(Group=="Obesity_Dx") %>% select(-Group)
BMI_record <- Groups_to_compare %>% filter(Group=="BMI_record") %>% select(-Group)
BMI_Dx <- Groups_to_compare %>% filter(Group=="BMI_Dx") %>% select(-Group)


Treat_exp %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 8543570
Procedures %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 4565829
Obesity_Dx %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 36166190
BMI_record %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 38385087
BMI_Dx %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 16178029



Treat_exp <- Treat_exp %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
Procedures <- Procedures %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
Obesity_Dx <- Obesity_Dx %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_record <- BMI_record %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_Dx <- BMI_Dx %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)



Treat_exp <- Treat_exp %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/8543570) %>% select(-n)
names(Treat_exp)[1] <- "Last"
names(Treat_exp)[2] <- "Penetrance_Treat_exp"


Procedures <- Procedures %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/4565829) %>% select(-n)
names(Procedures)[1] <- "Last"
names(Procedures)[2] <- "Penetrance_Procedures"


Obesity_Dx <- Obesity_Dx %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/36166190) %>% select(-n)
names(Obesity_Dx)[1] <- "Last"
names(Obesity_Dx)[2] <- "Penetrance_Obesity_Dx"


BMI_record <- BMI_record %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/38385087) %>% select(-n)
names(BMI_record)[1] <- "Last"
names(BMI_record)[2] <- "Penetrance_BMI_record"


BMI_Dx <- BMI_Dx %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/16178029) %>% select(-n)
names(BMI_Dx)[1] <- "Last"
names(BMI_Dx)[2] <- "Penetrance_BMI_Dx"

Treat_exp <- Treat_exp %>% filter(Penetrance_Treat_exp>5)
Procedures  <- Procedures %>% filter(Penetrance_Procedures>5)
Obesity_Dx  <- Obesity_Dx %>% filter(Penetrance_Obesity_Dx>5)
BMI_record  <- BMI_record %>% filter(Penetrance_BMI_record>5)
BMI_Dx  <- BMI_Dx %>% filter(Penetrance_BMI_Dx>5)

temp <- Treat_exp %>% full_join(Procedures) %>% full_join(Obesity_Dx) %>% full_join(BMI_Dx) %>% full_join(BMI_record)

temp <- mutate_if(temp, is.numeric, ~replace(., is.na(.), 0))




# --------------- 
# Physicians of Obesity Dx vs BMI records only ------------------
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events
names(Plus27_pats)[1] <- "patient"

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)





# Treatment experienced vector
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)
Treatment_exp_Vector %>% inner_join(DANU_Demographics) %>% summarise(n=sum(weight_2)) # 8543570
Treatment_exp_Vector <- Treatment_exp_Vector %>% inner_join(DANU_Demographics) 
Treatment_exp_Vector$Group <- "Treat_exp"

# Procedures
DANU_Dossiers <- fread("DANU Dossiers.txt")

DANU_Dossiers %>%  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% filter(grepl("P=",code)) %>% select(patid) %>% distinct() %>%
  inner_join(DANU_Demographics, by=c("patid"="patient")) %>% summarise(n=sum(weight_2))    # 4565829

Procedures_Pats <- DANU_Dossiers %>%  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% filter(grepl("P=",code)) %>% select(patid) %>% distinct() %>%
  inner_join(DANU_Demographics, by=c("patid"="patient")) %>% distinct()
names(Procedures_Pats)[1] <- "patient"
Procedures_Pats$Group <- "Procedures"



# Obesity Dx Pats
DANU_Events <- fread("DANU Events.txt")

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis=="Obesity"&source=="Diagnosis")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(!grepl("V",code)&!grepl("Z",code))

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code)
DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% distinct() %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% distinct() %>% summarise(n=sum(weight_2)) # 36166190

Obesity_Dx_Pats <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% distinct() 
names(Obesity_Dx_Pats)[1] <- "patient"
Obesity_Dx_Pats$Group <- "Obesity_Dx"


# Obesity BMI Dx codes
DANU_Events <- fread("DANU Events.txt")
DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(!grepl("BMI",code)) %>% filter(grepl("V",code)|grepl("Z",code)) %>% select(code)

DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% anti_join(Obesity_Dx_Pats, by=c("patid"="patient")) %>% 
  distinct() %>% summarise(n=sum(weight_2)) # 16178029

Obesity_BMI_Dx <- DANU_Events %>% inner_join(DANU_Diagnosis_Codes) %>% select(patid) %>% inner_join(DANU_Demographics, by=c("patid"="patient")) %>%
  anti_join(Treatment_exp_Vector, by=c("patid"="patient")) %>% anti_join(Procedures_Pats, by=c("patid"="patient")) %>% anti_join(Obesity_Dx_Pats, by=c("patid"="patient")) %>% 
  distinct()
names(Obesity_BMI_Dx)[1] <- "patient"
Obesity_BMI_Dx$Group <- "BMI_Dx"


Treatment_exp_Vector %>% bind_rows(Procedures_Pats) %>% bind_rows(Obesity_Dx_Pats) %>% bind_rows(Obesity_BMI_Dx) %>%
  full_join(DANU_Demographics) %>% group_by(Group) %>% summarise(n=sum(weight_2))


Groups_to_compare <- Treatment_exp_Vector %>% bind_rows(Procedures_Pats) %>% bind_rows(Obesity_Dx_Pats) %>% bind_rows(Obesity_BMI_Dx) %>%
  full_join(DANU_Demographics)

Groups_to_compare <- Groups_to_compare %>% mutate(Group=ifelse(is.na(Group),"BMI_record",Group))





Obesity_Dx <- Groups_to_compare %>% filter(Group=="Obesity_Dx") %>% select(patient,weight_2) %>% distinct()
BMI_record <- Groups_to_compare %>% filter(Group=="BMI_record") %>% select(patient,weight_2) %>% distinct()



DANU_Events <- fread("DANU Events.txt")
DANU_Event_Claims_Providers <- fread("DANU Event Claims Providers.txt")

Unique_Physicians_OBE <- fread("Unique_Physicians_OBE.txt")
DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")


temp1 <- Obesity_Dx  %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

sum(temp1$n)

# SUMMARY_SPECIALTY        n
# <chr>                <int>
# 1 NA                 1704622
# 2 GP                  272082
# 3 INTERNAL MEDICINE   177547
# 4 OTHER PHYSICIAN     144929
# 5 FACILITY            113798
# 6 OTHER HCP            93152
# 7 UNKNOWN              79180
# 8 SURGERY              61559
# 9 CARDIOLOGY           42148
# 10 EMERGENCY MEDICINE   18396
# 11 GASTRO/HEPATO        14044
# 12 ENDOCRINOLOGY        11661
# 13 PATHOLOGY             9381
# 14 HEMATO/ONCOL          7244
# 15 NEUROLOGY             6451
# 16 PSYCHIATRY            5530
# 17 RADIOLOGY             4257
# 18 NEPHROLOGIST          4154
# 19 MEDICAL ONCOLOGIST    2548

temp2 <- BMI_record  %>% left_join(DANU_Events, by=c("patient"="patid")) %>% select(prov) %>% 
  left_join(DANU_Event_Claims_Providers %>% select(prov, specialty)) %>%
  left_join(Unique_Physicians_OBE) %>% select(-specialty) %>%
  group_by(SUMMARY_SPECIALTY) %>% count() %>% arrange(-n)

# SUMMARY_SPECIALTY        n
# <chr>                <int>
# 1 NA                 1212060
# 2 GP                     245
# 3 ENDOCRINOLOGY          218
# 4 FACILITY               180
# 5 OTHER PHYSICIAN        160
# 6 UNKNOWN                147
# 7 OTHER HCP               99
# 8 INTERNAL MEDICINE       63
# 9 EMERGENCY MEDICINE      14
# 10 NEPHROLOGIST            11
# 11 PATHOLOGY               11
# 12 CARDIOLOGY               9
# 13 HEMATO/ONCOL             4
# 14 RADIOLOGY                4
# 15 SURGERY                  4
# 16 NEUROLOGY                2
# 17 GASTRO/HEPATO            1

sum(temp2$n)

names(temp2)[2] <- "N_naive"

temp <- temp1 %>% full_join(temp2)

fwrite(temp, "Physicians_OBEDxvsBMIrecord.csv")


# ----------
# Compare demographics of the 3 BMI groups of identified pats (27_30, 30_40, +40) --------------------

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)




# BMI groups
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Demographics %>% select(patient) %>% inner_join(DANU_Events, by=c("patient"="patid"))
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patient) %>% filter(code==max(code))
DANU_Events <- DANU_Events %>% group_by(patient) %>% select(-c(claimed)) %>% distinct()
DANU_Events <- DANU_Events %>% ungroup() %>% mutate(BMI_group = ifelse(code>27&code<=30, "27_to_30",
                                                                       ifelse(code>30&code<=40, "30_to_40",
                                                                              ifelse(code>40, ">40", NA)))) 


DANU_Events %>% group_by(BMI_group) %>% count()


DANU_Demographics <- DANU_Events %>% select(patient, BMI_group) %>% inner_join(DANU_Demographics)



temp <- fread("DANU Demographics.txt")
temp <- temp %>% select(patid, gender, age)
temp
names(temp)[1] <- "patient"


DANU_Demographics <- DANU_Demographics %>% left_join(temp)

DANU_Demographics %>% group_by(BMI_group) %>% summarise(n=weighted.mean(age, weight_2))

# BMI_group     n
# <chr>     <dbl>
# 1 >40        45.9
# 2 27_to_30   50.2
# 3 30_to_40   49.5


DANU_Demographics %>% group_by(BMI_group, gender) %>% summarise(n=sum(weight_2))

# BMI_group gender         n
# <chr>     <chr>      <dbl>
# 1 >40       F       8401533.
# 2 >40       M       4845786.
# 3 27_to_30  F      16138578.
# 4 27_to_30  M      19083544.
# 5 30_to_40  F      27790790.
# 6 30_to_40  M      27578474.


DANU_Demographics <- DANU_Demographics %>% select(patient, BMI_group, weight_2)

# BMI
DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% left_join(DANU_Events)
DANU_Demographics <- DANU_Demographics %>% filter(grepl("BMI", code))
DANU_Demographics$code <- as.character(DANU_Demographics$code)
DANU_Demographics$code <- parse_number(DANU_Demographics$code)
DANU_Demographics <- DANU_Demographics %>% group_by(patient) %>% filter(code==max(code)) %>% slice(1)

DANU_Demographics %>% group_by(BMI_group) %>% summarise(n=weighted.mean(code, weight_2))

# BMI_group     n
# <chr>     <dbl>
#   1 >40        46.0
# 2 27_to_30   28.7
# 3 30_to_40   34.0


DANU_Demographics$BMI_group <- factor(DANU_Demographics$BMI_group, levels=c('27_to_30', '30_to_40', '>40'))


DANU_Demographics %>% 
  ggplot(aes(x = code, y = BMI_group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  xlim(20,58) +
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n BMI (Max)") + ylab("BMI Group \n")




DANU_Events <- fread("DANU Events.txt")
names(DANU_Events)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(patient, weight_2,BMI_group)
DANU_Demographics %>% left_join(DANU_Events) %>% group_by(BMI_group , patient) %>% count() %>% ungroup() %>% group_by(BMI_group ) %>% summarise(MeanEvents=mean(n))

# BMI_group MeanEvents
# <fct>          <dbl>
#   1 27_to_30        10.6
# 2 30_to_40        14.2
# 3 >40             23.2




DANU_Demographics %>% left_join(DANU_Events) %>% group_by(BMI_group, patient) %>% count() %>% ungroup() %>% 
  ggplot(aes(x = n, y = BMI_group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  xlim(0,50) +
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n No. Events") + ylab("BMI Group \n")

DANU_Demographics <- DANU_Demographics %>% select(patient, weight_2,BMI_group)


# Comorbidities 

OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories <-OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))


unique(DANU_Demographics$BMI_group)

BMI_27_to_30 <- DANU_Demographics %>% filter(BMI_group=="27_to_30") %>% select(-BMI_group)
BMI_30_to_40 <- DANU_Demographics %>% filter(BMI_group=="30_to_40") %>% select(-BMI_group)
BMI_40 <- DANU_Demographics %>% filter(BMI_group==">40") %>% select(-BMI_group)


BMI_27_to_30 %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 35222122
BMI_30_to_40 %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 55369264
BMI_40 %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 13247319



BMI_27_to_30 <- BMI_27_to_30 %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_30_to_40 <- BMI_30_to_40 %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_40 <- BMI_40 %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)




BMI_27_to_30 <- BMI_27_to_30 %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/35222122) %>% select(-n)
names(BMI_27_to_30)[1] <- "Last"
names(BMI_27_to_30)[2] <- "Penetrance_BMI_27_to_30"


BMI_30_to_40 <- BMI_30_to_40 %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/55369264) %>% select(-n)
names(BMI_30_to_40)[1] <- "Last"
names(BMI_30_to_40)[2] <- "Penetrance_BMI_30_to_40"


BMI_40 <- BMI_40 %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/13247319) %>% select(-n)
names(BMI_40)[1] <- "Last"
names(BMI_40)[2] <- "Penetrance_BMI_40"



BMI_27_to_30 <- BMI_27_to_30 %>% filter(Penetrance_BMI_27_to_30>5)
BMI_30_to_40  <- BMI_30_to_40 %>% filter(Penetrance_BMI_30_to_40>5)
BMI_40  <- BMI_40 %>% filter(Penetrance_BMI_40>5)


temp <- BMI_27_to_30 %>% full_join(BMI_30_to_40) %>% full_join(BMI_40) 

temp <- mutate_if(temp, is.numeric, ~replace(., is.na(.), 0))

temp <- temp %>% arrange(Last)
rownames(temp) <- temp$Last
temp2 <- temp %>% select(-Last)

rownames(temp2) 
rownames(temp2) <- rownames(temp)
temp2 <- as.matrix(temp2)


my_colors <- colorRampPalette(c("cornsilk", "deepskyblue4"))  
heatmap(temp2, Rowv = NA, Colv = NA, col = my_colors(1000))    



# Using 1 ICD10 code digit


OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories <-OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% mutate(diagnosis = str_sub(diagnosis, 1L, 1L))
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% distinct()





BMI_27_to_30 <- DANU_Demographics %>% filter(BMI_group=="27_to_30") %>% select(-BMI_group)
BMI_30_to_40 <- DANU_Demographics %>% filter(BMI_group=="30_to_40") %>% select(-BMI_group)
BMI_40 <- DANU_Demographics %>% filter(BMI_group==">40") %>% select(-BMI_group)


BMI_27_to_30 %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 35222122
BMI_30_to_40 %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 55369264
BMI_40 %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 13247319



BMI_27_to_30 <- BMI_27_to_30 %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_30_to_40 <- BMI_30_to_40 %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
BMI_40 <- BMI_40 %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)




BMI_27_to_30 <- BMI_27_to_30 %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/35222122) %>% select(-n)
names(BMI_27_to_30)[1] <- "Last"
names(BMI_27_to_30)[2] <- "Penetrance_BMI_27_to_30"


BMI_30_to_40 <- BMI_30_to_40 %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/55369264) %>% select(-n)
names(BMI_30_to_40)[1] <- "Last"
names(BMI_30_to_40)[2] <- "Penetrance_BMI_30_to_40"


BMI_40 <- BMI_40 %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/13247319) %>% select(-n)
names(BMI_40)[1] <- "Last"
names(BMI_40)[2] <- "Penetrance_BMI_40"



BMI_27_to_30 <- BMI_27_to_30 %>% filter(Penetrance_BMI_27_to_30>5)
BMI_30_to_40  <- BMI_30_to_40 %>% filter(Penetrance_BMI_30_to_40>5)
BMI_40  <- BMI_40 %>% filter(Penetrance_BMI_40>5)


temp <- BMI_27_to_30 %>% full_join(BMI_30_to_40) %>% full_join(BMI_40) 

temp <- mutate_if(temp, is.numeric, ~replace(., is.na(.), 0))


# -------------
# Nr Lines per stock on month 60, BMI>27 ----------------

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)




OBE_Box_Histories <- fread("OBE Box Histories.txt")
OBE_Box_Histories <- OBE_Box_Histories %>% select(1,2,63)
OBE_Box_Histories <- OBE_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))



OBE_Box_Histories <- OBE_Box_Histories %>% inner_join(DANU_Demographics) %>% select(-disease)

nrLines <- fread("OBE_nrLines_Histories.txt")
nrLines <- nrLines %>% select(2, 63)
names(nrLines)[2] <- "Nr_lines"

OBE_Box_Histories <- OBE_Box_Histories %>% left_join(nrLines)

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

OBE_Box_Histories <- Treatment_exp_Vector %>% inner_join(OBE_Box_Histories)

sum(OBE_Box_Histories$weight_2) # 8543570
OBE_Box_Histories %>% group_by(month60) %>% summarise(n=sum(weight_2))

# month60        n
# <chr>      <dbl>
#   1 a       2561075.
# 2 g          7128.
# 3 G         57008.
# 4 H         23914.
# 5 o         24240.
# 6 w          1806.
# 7 x       5868398.

OBE_Box_Histories %>% group_by(month60) %>% summarise(lines=weighted.mean(Nr_lines, weight_2))
# 
# month60 lines
# <chr>   <dbl>
#   1 a        1.40
# 2 g        1.61
# 3 G        1.86
# 4 H        1.40
# 5 o        1.60
# 6 w        1.83
# 7 x        1.22

weighted.mean(OBE_Box_Histories$Nr_lines, OBE_Box_Histories$weight_2) 1.281123

OBE_Box_Histories %>% mutate(Nr_lines=ifelse(Nr_lines>=3,3,Nr_lines)) %>% group_by(month60,Nr_lines) %>%
  summarise(n=sum(weight_2)) %>%
  spread(key=Nr_lines,value=n)


# --------

# Source of inflows per stock BMI > 27 ------------------
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705
names(DANU_Demographics)[1] <- "patient"
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)

DANU_Demographics_BMI27 <- DANU_Demographics

OBE_Flows_Aux._Long <- fread("OBE_Flows_Aux._Long_v2.txt")
length(unique(OBE_Flows_Aux._Long$patient))

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% inner_join(DANU_Demographics)

OBE_Flows_Aux._Long %>% select(patient, weight_2) %>% distinct() %>% summarise(n=sum(weight_2)) # 8543570

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% filter(p1>=48)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% filter(flow==1)

OBE_Flows_Aux._Long %>% filter(stops==1&s1!="G") %>% summarise(n=sum(weight_2))

OBE_Flows_Aux._Long %>% select(patient, weight_2) %>% distinct() %>% summarise(n=sum(weight_2)) # 3133886

OBE_Flows_Aux._Long %>% filter(starts==0&stops==0&re_starts==0) %>% summarise(n=sum(weight_2)) #432132.3

OBE_Flows_Aux._Long %>% filter(re_starts==1&s2!="G") %>% summarise(n=sum(weight_2)) #1857318
OBE_Flows_Aux._Long %>% filter(starts==1) %>% summarise(n=sum(weight_2)) #1005671

OBE_Flows_Aux._Long %>% filter(starts==1&s2=="G") %>% summarise(n=sum(weight_2)) #1005671


OBE_Flows_Aux._Long %>% filter(s1=="x"&s2=="G") %>% select(patient, weight_2) %>% distinct() %>% summarise(n=sum(weight_2)) # 1309067

OBE_Flows_Aux._Long %>% filter(starts==1) %>% group_by(s1, s2) %>% summarise(n=sum(weight_2)) %>%
  spread(key=s2,value=n)

OBE_Flows_Aux._Long %>% filter(s1=="x"&s2!="G"&starts==0&flow==1) %>% summarise(n=sum(weight_2)) #3872574

OBE_Flows_Aux._Long %>% filter(p2==60) %>% filter(s2=="x") %>% summarise(n=sum(weight_2))

OBE_Flows_Aux._Long %>% filter(s1!="x"&s2!="x"&s1!="G"&s2!="G") %>% summarise(n=sum(weight_2))

# -----------
# Comorbidities in GLP1-experienced patients 1 digit ------------
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705


Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")

Cum_Class_Experience_EveryMonth %>% filter(p2==60)
Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))
GLP1_Exp_Pats <- Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient)
Naive_Pats <- OBE_Drug_Histories %>% anti_join(Treatment_exp_Vector)




GLP1_Exp_Pats <- GLP1_Exp_Pats %>% select(patient) %>% distinct()
Naive_Pats <- Naive_Pats %>% select(patient) %>% distinct()

DANU_Demographics
DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)
names(DANU_Demographics)[1] <- "patient"

GLP1_Exp_Pats
GLP1_Exp_Pats %>% inner_join(DANU_Demographics)

GLP1_Exp_Pats <- GLP1_Exp_Pats %>% inner_join(DANU_Demographics)
Naive_Pats <- Naive_Pats %>% inner_join(DANU_Demographics)






OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
OBE_Comorbidity_Inventories <-OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% mutate(diagnosis = str_sub(diagnosis, 1L, 1L))
OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% distinct()



GLP1_Exp_Pats %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 327329.1
Naive_Pats %>% select(patient, weight_2) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight_2)) # 95295135


GLP1_Exp_Pats <- GLP1_Exp_Pats %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)
Naive_Pats <- Naive_Pats %>% left_join(OBE_Comorbidity_Inventories, by=c("patient"="patid")) %>% select(-weight)


GLP1_Exp_Pats <- GLP1_Exp_Pats %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/327329.1) %>% select(-n)
names(GLP1_Exp_Pats)[1] <- "Last"
names(GLP1_Exp_Pats)[2] <- "Penetrance_GLP1_Exp_Pats"


Naive_Pats <- Naive_Pats %>% group_by(diagnosis) %>% summarise(n=sum(weight_2)) %>% mutate(penetrance=100*n/95295135) %>% select(-n)
names(Naive_Pats)[1] <- "Last"
names(Naive_Pats)[2] <- "Penetrance_Naive_Pats"


GLP1_Exp_Pats <- GLP1_Exp_Pats %>% filter(Penetrance_GLP1_Exp_Pats>5)
Naive_Pats  <- Naive_Pats %>% filter(Penetrance_Naive_Pats>5)


temp <- GLP1_Exp_Pats %>% full_join(Naive_Pats) 

temp <- mutate_if(temp, is.numeric, ~replace(., is.na(.), 0))

# ------------
# GLP1 penetrance ever / last year - BMI>27 BMI<27 ----------
DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637

DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))

sum(DANU_Demographics$weight_2) # 132236677

DANU_Demographics <- DANU_Demographics %>% select(-weight_BMI)

# BMI MAX Bucket
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Events)

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)

DANU_Events <- DANU_Events %>% mutate(BMI_group=ifelse(code>27,">27","<27")) %>% select(-c(code, claimed))



OBE_Flows_Aux._Long <- fread("OBE_Flows_Aux._Long_v2.txt")
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(patient, p1,p2,d1,d2) 

GLP_ever <- OBE_Flows_Aux._Long %>% filter(grepl("47",d1)|grepl("48",d1)|grepl("49",d1)|grepl("50",d1)|grepl("51",d1)|grepl("52",d1)|grepl("53",d1)|
                                             grepl("47",d2)|grepl("48",d2)|grepl("49",d2)|grepl("50",d2)|grepl("51",d2)|grepl("52",d2)|grepl("53",d2)) %>%
  select(patient) %>% distinct()

GLP_ever$Group <- "GLP_ever"



GLP_lastYear <- OBE_Flows_Aux._Long %>% filter(p1>=48) %>% filter(grepl("47",d1)|grepl("48",d1)|grepl("49",d1)|grepl("50",d1)|grepl("51",d1)|grepl("52",d1)|grepl("53",d1)|
                                                                    grepl("47",d2)|grepl("48",d2)|grepl("49",d2)|grepl("50",d2)|grepl("51",d2)|grepl("52",d2)|grepl("53",d2)) %>%
  select(patient) %>% distinct()

GLP_lastYear$Group <- "GLP_lastYear"


DANU_Events %>% inner_join(DANU_Demographics) %>% left_join(GLP_ever, by=c("patid"="patient")) %>%
  group_by(BMI_group, Group) %>% summarise(n=sum(weight_2))

# BMI_group Group            n
# <chr>     <chr>        <dbl>
# 1 <27       GLP_ever    15487.
# 2 <27       NA       25249721.
# 3 >27       GLP_ever   223855.
# 4 >27       NA       70789642.


DANU_Events %>% inner_join(DANU_Demographics) %>% left_join(GLP_lastYear, by=c("patid"="patient")) %>%
  group_by(BMI_group, Group) %>% summarise(n=sum(weight_2))

# BMI_group Group                n
# <chr>     <chr>            <dbl>
# 1 <27       GLP_lastYear     9568.
# 2 <27       NA           25255641.
# 3 >27       GLP_lastYear   114026.
# 4 >27       NA           70899471.

# -----------
# Prevalence of procedures in GLP1-experienced vs Treat-naive ---------
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% mutate(code=ifelse(code<=27, "<27",
                                                  ifelse(code>27,">27", NA))) %>% select(-claimed)

Plus27_pats <- DANU_Events

names(Plus27_pats)[1] <- "patient"


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Demographics$weight_2) # 172519637
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.479105607))
# 138783027
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2, weight_BMI) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677
DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27"), by=c("patid"="patient")) %>% summarise(n=sum(weight_2*1.462239))
# 103838705
DANU_Demographics <- DANU_Demographics %>% inner_join(Plus27_pats %>% filter(code==">27") %>% select(-code), by=c("patid"="patient")) %>% mutate(weight_2=weight_2*1.462239)
sum(DANU_Demographics$weight_2) # 103838705


Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")
Cum_Class_Experience_EveryMonth

Cum_Class_Experience_EveryMonth %>% filter(p2==60)
Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))
GLP1_Exp_Pats <- Cum_Class_Experience_EveryMonth %>% filter(p2==60&(p1_OralExp==1|p1_InjExp==1))

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)

OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
OBE_Drug_Histories <- OBE_Drug_Histories %>% select(patient)
Naive_Pats <- OBE_Drug_Histories %>% anti_join(Treatment_exp_Vector)


GLP1_Exp_Pats <- GLP1_Exp_Pats %>% select(patient) %>% distinct()
Naive_Pats <- Naive_Pats %>% select(patient) %>% distinct()

GLP1_Exp_Pats <- GLP1_Exp_Pats %>% inner_join(DANU_Demographics %>% select(patid, weight_2), by=c("patient"="patid"))
Naive_Pats <- Naive_Pats %>% inner_join(DANU_Demographics %>% select(patid, weight_2), by=c("patient"="patid"))


sum(GLP1_Exp_Pats$weight_2) # 327329.1
sum(Naive_Pats$weight_2) # 95295135


DANU_Dossiers <- fread("DANU Dossiers.txt")
DANU_Dossiers <- DANU_Dossiers %>% filter(grepl("P=",code)) %>% select(patid) %>% distinct()

GLP1_Exp_Pats %>% inner_join(DANU_Dossiers, by=c("patient"="patid")) %>% summarise(n=sum(weight_2)) # 41543.99
Naive_Pats %>% inner_join(DANU_Dossiers, by=c("patient"="patid")) %>% summarise(n=sum(weight_2)) # 4565829

# -------

# Create new files for Hendrik's pipeline ------

OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Box_Histories     <- fread("OBE Box Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Demographics     <- fread("OBE Demographics.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(-weight)
OBE_Box_Histories <- OBE_Box_Histories %>% select(-weight)
OBE_Demographics <- OBE_Demographics %>% select(-weight)


DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity")

sum(DANU_Demographics$weight_2)
DANU_Demographics <- DANU_Demographics %>% select(patid, weight_2)
names(DANU_Demographics)[1] <- "patient"
names(DANU_Demographics)[2] <- "weight"

OBE_Drug_Histories <- OBE_Drug_Histories %>% left_join(DANU_Demographics) 
OBE_Box_Histories <- OBE_Box_Histories %>% left_join(DANU_Demographics) 
OBE_Demographics <- OBE_Demographics %>% left_join(DANU_Demographics) 

fwrite(OBE_Drug_Histories, "OBE_Drug_Histories_v3.txt", sep="\t")
fwrite(OBE_Box_Histories, "OBE_Box_Histories_v3.txt", sep="\t")
fwrite(OBE_Demographics, "OBE_Demographics_v3.txt", sep="\t")


OBE_Drug_Histories <- fread("OBE_Drug_Histories_v3.txt", sep="\t")
OBE_Box_Histories <- fread("OBE_Box_Histories_v3.txt", sep="\t")
OBE_Demographics <- fread("OBE_Demographics_v3.txt", sep="\t")

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(1,2,63,3:62) 
OBE_Box_Histories <- OBE_Box_Histories %>% select(1,2,63,3:62) 
OBE_Demographics <- OBE_Demographics %>% select(1,25,2,,3:24) 


fwrite(OBE_Drug_Histories, "OBE_Drug_Histories_v3.txt", sep="\t")
fwrite(OBE_Box_Histories, "OBE_Box_Histories_v3.txt", sep="\t")
fwrite(OBE_Demographics, "OBE_Demographics_v3.txt", sep="\t")

# ---------

# Treatment-experienced last 12 months -----------

DANU_Demographics <- fread("DANU_Demographics_Weights_V3_BMI.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight_2) %>% filter(!is.na(weight_2))
sum(DANU_Demographics$weight_2) # 132236677


OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OBE_Drug_Histories <- OBE_Drug_Histories %>% select(52:63)

OBE_Drug_Histories[OBE_Drug_Histories != "-"] <- 1  # on drug 
OBE_Drug_Histories[OBE_Drug_Histories == "-"] <- 0  # no drug

OBE_Drug_Histories[] <- lapply(OBE_Drug_Histories,as.numeric)

OBE_Drug_Histories$SUM <- rowSums(OBE_Drug_Histories)

OBE_Drug_Histories_LONG     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

Pats_vec <- OBE_Drug_Histories_LONG %>% select(patient, weight)

OBE_Drug_Histories <- Pats_vec %>% bind_cols(OBE_Drug_Histories)

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(SUM != 0)

sum(OBE_Drug_Histories$weight) # 4798172

Treatment_exp_Vector <- OBE_Drug_Histories %>% select(patient, weight)

fwrite(Treatment_exp_Vector, "Treatment_exp_Vector.txt")


# -----------

# Comorbidities ALL OBESITY pats ----------------------------------------------------
OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% filter(!grepl("A", diagnosis) & !grepl("B", diagnosis) & !grepl("C", diagnosis) &
                                                                       !grepl("O", diagnosis) & !grepl("P", diagnosis) & !grepl("Q", diagnosis) &
                                                                       !grepl("R", diagnosis) & !grepl("S", diagnosis) & !grepl("T", diagnosis) &
                                                                       !grepl("X", diagnosis) & !grepl("Y", diagnosis) & !grepl("Z", diagnosis))

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% mutate(diagnosis=str_sub(diagnosis, 1L, 3L)) %>% distinct()

OBE_Drug_Histories <- fread("OBE Drug Histories.txt")
sum(OBE_Drug_Histories$weight) # 106469049

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% filter(grepl("[A-Z]", diagnosis))

temp <- OBE_Comorbidity_Inventories %>% group_by(diagnosis) %>% summarise(n=sum(weight)) %>% mutate(penetrance=100*n/106469049) %>% select(-n)

fwrite(temp, "Comorb_Penetrance_Obesity106m.csv")

# -------------------------------------------------
# Comorbidity prevalence Obesity BMI>27 ---------------------------------------------------
DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, weight)
OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- fread("DIA Comorbidity Inventories.txt")
Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% full_join(DIA_Comorbidity_Inventories) %>% distinct()
Comorbidity_Inventories <- Comorbidity_Inventories %>% inner_join(DANU_Demographics)
length(unique(Comorbidity_Inventories$patid))

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- Comorbidity_Inventories %>% select(patid) %>% distinct() %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
Comorbidity_Inventories <- DANU_Events %>% filter(code>=27) %>% select(patid) %>% distinct() %>% left_join(Comorbidity_Inventories)

Comorbidity_Inventories %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 84173305
Comorbidity_Inventories <- Comorbidity_Inventories %>% ungroup()




# DIABETES  --  19826523   -- 0.2355441 
Comorbidity_Inventories %>% filter(grepl("E11", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
DIABETES <-  Comorbidity_Inventories %>% filter(grepl("E11", diagnosis)) %>% select(patid, weight)  %>% distinct()




# CVD   --  25853921   -- 0.3071511 
Comorbidity_Inventories %>% filter(grepl("I20", diagnosis)|
                                   grepl("I21", diagnosis)|
                                   grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)|
                                   grepl("I24", diagnosis)|
                                   grepl("I25", diagnosis)|
                                   grepl("I4", diagnosis)|
                                   grepl("I6", diagnosis)|
                                   grepl("G45", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
CVD <- Comorbidity_Inventories %>% filter(grepl("I20", diagnosis)|
                                   grepl("I21", diagnosis)|
                                   grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)|
                                   grepl("I24", diagnosis)|
                                   grepl("I25", diagnosis)|
                                   grepl("I4", diagnosis)|
                                   grepl("I6", diagnosis)|
                                   grepl("G45", diagnosis)) %>% select(patid, weight)  %>% distinct()


# HF   --  27660087   -- 0.1314236 
Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
HF <- Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight)  %>% distinct()


# Sleep Apnea   --  24666610   -- 0.2930455 
Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
SLEEPAPNEA <- Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid, weight)  %>% distinct()




# Osteoarthritis   --  27800684   -- 0.3302791 
Comorbidity_Inventories %>% filter(grepl("M15", diagnosis)|
                                     grepl("M16", diagnosis)|
                                     grepl("M17", diagnosis)|
                                     grepl("M18", diagnosis)|
                                     grepl("M19", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
OSTEOARTHRITIS <- Comorbidity_Inventories %>% filter(grepl("M15", diagnosis)|
                                     grepl("M16", diagnosis)|
                                     grepl("M17", diagnosis)|
                                     grepl("M18", diagnosis)|
                                     grepl("M19", diagnosis)) %>% select(patid, weight)  %>% distinct()




# Dyslipidemia   --  45754552   -- 0.5435756 
Comorbidity_Inventories %>% filter(grepl("E78", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
DISLIPIDEMIA <- Comorbidity_Inventories %>% filter(grepl("E78", diagnosis)) %>% select(patid, weight)  %>% distinct()




# Hypertension   --  44147009   -- 0.5244776 
Comorbidity_Inventories %>% filter(grepl("I10", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
HYPERTENSION <- Comorbidity_Inventories %>% filter(grepl("I10", diagnosis)) %>% select(patid, weight)  %>% distinct()




# PAD   --  9499300   -- 0.1128541 
Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|
                                     grepl("I73", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
PAD <- Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|
                                     grepl("I73", diagnosis)) %>% select(patid, weight)  %>% distinct()



# Cancer   --  12044634   -- 0.1430933 
Comorbidity_Inventories %>% filter(grepl("C", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
CANCER <- Comorbidity_Inventories %>% filter(grepl("C", diagnosis)) %>% select(patid, weight)  %>% distinct()
  

# CKD   --  8475453   -- 0.1006905 
Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
CKD <- Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight)  %>% distinct()
  
# POS   --  2461119   -- 0.02923871 
Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
POS <- Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid, weight)  %>% distinct()
  
# GERD   --  27798897   -- 0.3302579 
Comorbidity_Inventories %>% filter(grepl("K21", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
GERD <- Comorbidity_Inventories %>% filter(grepl("K21", diagnosis)) %>% select(patid, weight)  %>% distinct()
  
# StressIncontinence   --  18453475   -- 0.2192319 
Comorbidity_Inventories %>% filter(grepl("N39", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
StressIncontinence <- Comorbidity_Inventories %>% filter(grepl("N39", diagnosis)) %>% select(patid, weight)  %>% distinct()
  
# NASH   --  1234341   -- 0.01466428 
Comorbidity_Inventories %>% filter(grepl("K75", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
NASH <- Comorbidity_Inventories %>% filter(grepl("K75", diagnosis)) %>% select(patid, weight)  %>% distinct()
  

# PREDIABETES   --  13978711   -- 0.1660706 
Comorbidity_Inventories %>% filter(grepl("R73", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
PREDIABETES <- Comorbidity_Inventories %>% filter(grepl("R73", diagnosis)) %>% select(patid, weight)  %>%  anti_join(DIABETES %>% select(patid)) %>% distinct()

# RA   --  2917008   -- 0.03465479 
Comorbidity_Inventories %>% filter(grepl("M05", diagnosis)|grepl("M06", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
RA <- Comorbidity_Inventories %>% filter(grepl("M05", diagnosis)|grepl("M06", diagnosis)) %>% select(patid, weight)  %>% distinct()
  







PREDIABETES %>% select(patid, weight) %>% inner_join(DISLIPIDEMIA  %>% select(patid, weight)) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(HYPERTENSION  %>% select(patid, weight)) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(OSTEOARTHRITIS  %>% select(patid, weight)) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(StressIncontinence  %>% select(patid, weight)) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(SLEEPAPNEA) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(DIABETES) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(CANCER) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(PAD) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(CKD) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(POS) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(GERD) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(CVD) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(HF) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(NASH) %>% summarise(n=sum(weight)/84173305)
PREDIABETES %>% select(patid, weight) %>% inner_join(RA) %>% summarise(n=sum(weight)/84173305)






DIABETES$disease <- "DIABETES"
CVD$disease <- "CVD"
SLEEPAPNEA$disease <- "SLEEPAPNEA"
OSTEOARTHRITIS$disease <- "OSTEOARTHRITIS"
DISLIPIDEMIA$disease <- "DISLIPIDEMIA"
HYPERTENSION$disease <- "HYPERTENSION"
PAD$disease <- "PAD"
CANCER$disease <- "CANCER"
CKD$disease <- "CKD"
POS$disease <- "POS"
GERD$disease <- "GERD"
StressIncontinence$disease <- "StressIncontinence"
HF$disease <- "HF"
NASH$disease <- "NASH"
RA$disease <- "RA"
PREDIABETES$disease <- "PREDIABETES"


temp <- DIABETES %>% full_join(CVD) %>% full_join(SLEEPAPNEA) %>% full_join(OSTEOARTHRITIS) %>% full_join(DISLIPIDEMIA) %>%
  full_join(HYPERTENSION) %>% full_join(PAD) %>% full_join(CANCER) %>% full_join(CKD) %>% full_join(POS) %>%
  full_join(GERD) %>% full_join(StressIncontinence) %>% full_join(HF) %>% full_join(NASH) %>% full_join(RA) %>%  full_join(PREDIABETES) %>% mutate(ORIGINALDISEASE=disease) %>% mutate(label=1) %>%
  spread(key=disease, value=label)

temp[is.na(temp)] <- 0

temp %>% group_by(patid) %>% count() %>% left_join(temp %>% select(patid, ORIGINALDISEASE) %>% distinct()) %>%
  ungroup() %>% group_by(ORIGINALDISEASE) %>% summarise(n=mean(n)) %>% arrange(-n)


   ORIGINALDISEASE        n
   <chr>              <dbl>
 1 CKD                 7.55
 2 PAD                 7.54
 3 HF                  7.46
 4 NASH                6.94
 5 RA                  6.94
 6 CVD                 6.31
 7 CANCER              6.21
 8 DIABETES            6.15
 9 OSTEOARTHRITIS      5.84
10 StressIncontinence  5.73
11 SLEEPAPNEA          5.61
12 GERD                5.60
13 HYPERTENSION        5.36
14 DISLIPIDEMIA        5.23
15 PREDIABETES         4.90
16 POS                 4.48


# 
# temp %>% select(patid, ORIGINALDISEASE) %>% distinct() %>% ungroup() %>%
#   left_join(Comorbidity_Inventories %>% filter(!grepl("O",diagnosis)&!grepl("P",diagnosis)&!grepl("Q",diagnosis)&
#                                                  !grepl("R",diagnosis)&!grepl("S",diagnosis)&!grepl("T",diagnosis)&!grepl("U",diagnosis)&
#                                                  !grepl("V",diagnosis)&!grepl("W",diagnosis)&!grepl("X",diagnosis)&!grepl("Y",diagnosis)&
#                                                  !grepl("Z",diagnosis)) %>% 
#               group_by(patid) %>% count()) %>% ungroup() %>%
#   group_by(ORIGINALDISEASE) %>% summarise(mean=mean(n, na.rm = T)) %>% arrange(-mean)
#   

# 
   ORIGINALDISEASE     mean
   <chr>              <dbl>
 1 PAD                 50.2
 2 RA                  49.1
 3 HF                  48.9
 4 NASH                48.7
 5 CKD                 48.6
 6 CANCER              42.0
 7 StressIncontinence  41.5
 8 CVD                 41.4
 9 OSTEOARTHRITIS      40.0
10 GERD                39.5
11 DIABETES            39.5
12 SLEEPAPNEA          38.4
13 HYPERTENSION        34.6
14 POS                 34.4
15 DISLIPIDEMIA        34.0
16 PREDIABETES         30.6


DIABETES
CVD
SLEEPAPNEA
OSTEOARTHRITIS
DISLIPIDEMIA
HYPERTENSION
PAD
CANCER
CKD
POS
GERD
StressIncontinence
HF
RA
PREDIABETES



temp <- sum(PREDIABETES$weight) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="R52") %>% 
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="R26") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp)

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="Z73") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="R58") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="R11") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp)

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="R12") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="R19") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="R53"|diagnosis=="R40"|diagnosis=="R42"|diagnosis=="R41") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="F32"|diagnosis=="F33"|diagnosis=="F34"|diagnosis=="F39"|diagnosis=="F41") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="D50"|diagnosis=="D51"|diagnosis=="D52"|diagnosis=="D63"|diagnosis=="D64") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

PREDIABETES %>% left_join(Comorbidity_Inventories) %>% filter(diagnosis=="E50"|diagnosis=="E51"|diagnosis=="E52"|diagnosis=="E53"|diagnosis=="E54"|diagnosis=="E55"|diagnosis=="E56"|diagnosis=="E58"|diagnosis=="E59"|diagnosis=="E60"|diagnosis=="E61"|diagnosis=="E63") %>%  
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)/temp) 

# -------------------------------------------------------------------
# Time from First Dx to First GLP1 ---------------------------------------------------------
Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
OBE_Drug_Histories     <- fread("OBE Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE_Drug_Histories <- OBE_Drug_Histories %>% inner_join(Treatment_exp_Vector)
OBE_Drug_Histories <- gather(OBE_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Drug_Histories$Month <- as.character(OBE_Drug_Histories$Month)
OBE_Drug_Histories$Month <- parse_number(OBE_Drug_Histories$Month)
OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(Treat!="-")

DANU_Ingredients       <- fread("DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients <- DANU_Ingredients %>% select(molecule, drug_group)
string_InjectableGLP1  <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"], collapse = "|"),")\\b")

OBE_Drug_Histories <- OBE_Drug_Histories %>% filter(grepl(string_InjectableGLP1, Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, weight, Month)
names(OBE_Drug_Histories)[3] <- "FirstGLP1"



DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% select(patid, code, claimed)
names(DANU_Events)[1] <- "patient"
DANU_Events <- OBE_Drug_Histories %>% select(patient) %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("D",code)) %>% group_by(patient) %>% filter(claimed==min(claimed)) %>% slice(1)
DANU_Events <- DANU_Events %>% select(patient, claimed)
DANU_Events$claimed <- format(as.Date(DANU_Events$claimed), "%Y-%m")


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

DANU_Events <- DANU_Events %>% left_join(Months_lookup, by=c("claimed"="Month")) %>%
  drop_na() %>% select(-claimed)
names(DANU_Events)[2] <- "FirstDx"

OBE_Drug_Histories %>% inner_join(DANU_Events) %>% ungroup() %>%
  filter(FirstGLP1<FirstDx) %>%
  summarise(n=weighted.mean(FirstDx-FirstGLP1,weight))

# -------------------------------------------------------------
# --------------------------------------------------------------------------------------

# T2DM DANU_Utilizations ------------------------------------------------------------------------------------------------------



# Diabetes: GLP1 Exp vs No Exp (within treatment experienced)

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep = "\t")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% filter(p2 == 60) %>% select( -c(p1, p2))

Cum_Class_Experience_EveryMonth <- Treatment_exp_Vector %>% inner_join(Cum_Class_Experience_EveryMonth)

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% select(patient, weight, p1_OralExp, p1_InjExp)


# DANU Utilizations

DANU_Utilizations <- fread("DANU Utilizations.txt")

DANU_Utilizations <- Cum_Class_Experience_EveryMonth %>% select(patient) %>% inner_join(DANU_Utilizations, by = c("patient" = "patid"))

DANU_Utilizations <- DANU_Utilizations %>% left_join(Cum_Class_Experience_EveryMonth)

names(DANU_Utilizations)

 [1] "patient"                       
 [2] "weight"                        
 [3] "medical_visits"                
 [4] "rx_visits"                     
 [5] "encounter_visits"              
 [6] "medical_providers"             
 [7] "rx_providers"                  
 [8] "encounter_providers"           
 [9] "hospital_days"                 
[10] "hospital_stays"                
[11] "hospital_day_stays"            
[12] "hospital_short_stays"          
[13] "hospital_medium_stays"         
[14] "hospital_long_stays"           
[15] "icu_visits"                    
[16] "surgery_visits"                
[17] "emergency_visits"              
[18] "prescription_count"            
[19] "public_payer_prescriptions"    
[20] "commercial_payer_prescriptions"
[21] "drug_supply_days"              
[22] "drug_formulations"             
[23] "drug_ingredients"              
[24] "drug_ahfs_classes"             
[25] "drug_fdb_classes"              
[26] "p1_OralExp"                    
[27] "p1_InjExp"


# INJECTABLE
DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(medical_visits, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  115.
2         1  123.

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(rx_visits, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  114.
2         1  157

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(encounter_visits, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  203.
2         1  246.

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(medical_providers, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  31.4
2         1  34.1

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(rx_providers, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  12.7
2         1  15.1

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(encounter_providers, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  37.1
2         1  40.7

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(hospital_days, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  1.75
2         1  1.23

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(hospital_stays, weight))

  p1_InjExp     n
      <int> <dbl>
1         0 0.233
2         1 0.185

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(hospital_day_stays, weight))

  p1_InjExp      n
      <int>  <dbl>
1         0 0.0361
2         1 0.0307

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(hospital_short_stays, weight))

  p1_InjExp      n
      <int>  <dbl>
1         0 0.0689
2         1 0.0590

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(hospital_medium_stays, weight))

 p1_InjExp      n
      <int>  <dbl>
1         0 0.0646
2         1 0.0527

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(hospital_long_stays, weight))

  p1_InjExp      n
      <int>  <dbl>
1         0 0.0634
2         1 0.0422

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(icu_visits, weight))

  p1_InjExp      n
      <int>  <dbl>
1         0 0.0710
2         1 0.0558

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(surgery_visits, weight))

  p1_InjExp      n
      <int>  <dbl>
1         0 0.0617
2         1 0.0510

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(emergency_visits, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  4.94
2         1  4.99

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(prescription_count, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  223.
2         1  318.

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(public_payer_prescriptions, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  94.2
2         1 104. 

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(commercial_payer_prescriptions, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  77.3
2         1 137. 

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(drug_supply_days, weight))

  p1_InjExp      n
      <int>  <dbl>
1         0  9643.
2         1 13456.

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(drug_formulations, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  50.9
2         1  65.8

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(drug_ingredients, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  30.0
2         1  37.7

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(drug_ahfs_classes, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  22.3
2         1  27.8

DANU_Utilizations %>% group_by(p1_InjExp) %>% summarise(n = weighted.mean(drug_fdb_classes, weight))

  p1_InjExp     n
      <int> <dbl>
1         0  21.9
2         1  27.9




# ORAL

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(medical_visits, weight))

  p1_OralExp     n
       <int> <dbl>
1          0 116. 
2          1  95.6


DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(rx_visits, weight))

  p1_OralExp     n
       <int> <dbl>
1          0  122.
2          1  133.

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(encounter_visits, weight))

 p1_OralExp     n
       <int> <dbl>
1          0  210.
2          1  201.

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(medical_providers, weight))

 p1_OralExp     n
       <int> <dbl>
1          0  31.9
2          1  28.1

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(rx_providers, weight))

  p1_OralExp     n
       <int> <dbl>
1          0  13.1
2          1  13.6

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(encounter_providers, weight))

  p1_OralExp     n
       <int> <dbl>
1          0  37.7
2          1  34.2

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(hospital_days, weight))

  p1_OralExp     n
       <int> <dbl>
1          0 1.66 
2          1 0.739

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(hospital_stays, weight))

  p1_OralExp     n
       <int> <dbl>
1          0 0.225
2          1 0.139

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(hospital_day_stays, weight))

  p1_OralExp      n
       <int>  <dbl>
1          0 0.0353
2          1 0.0240

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(hospital_short_stays, weight))

 p1_OralExp      n
       <int>  <dbl>
1          0 0.0674
2          1 0.0486

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(hospital_medium_stays, weight))

  p1_OralExp      n
       <int>  <dbl>
1          0 0.0627
2          1 0.0418

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(hospital_long_stays, weight))

  p1_OralExp      n
       <int>  <dbl>
1          0 0.0600
2          1 0.0242

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(icu_visits, weight))

  p1_OralExp      n
       <int>  <dbl>
1          0 0.0685
2          1 0.0446

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(surgery_visits, weight))

  p1_OralExp      n
       <int>  <dbl>
1          0 0.0599
2          1 0.0460

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(emergency_visits, weight))

  p1_OralExp     n
       <int> <dbl>
1          0  4.96
2          1  3.64

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(prescription_count, weight))

 p1_OralExp     n
       <int> <dbl>
1          0  239.
2          1  258.

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(public_payer_prescriptions, weight))

 p1_OralExp     n
       <int> <dbl>
1          0  96.3
2          1  52.4

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(commercial_payer_prescriptions, weight))

 p1_OralExp     n
       <int> <dbl>
1          0  87.4
2          1 143.

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(drug_supply_days, weight))

  p1_OralExp      n
       <int>  <dbl>
1          0 10305.
2          1 11142.

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(drug_formulations, weight))

  p1_OralExp     n
       <int> <dbl>
1          0  53.5
2          1  57.8

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(drug_ingredients, weight))

  p1_OralExp     n
       <int> <dbl>
1          0  31.3
2          1  34.5

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(drug_ahfs_classes, weight))

  p1_OralExp     n
       <int> <dbl>
1          0  23.3
2          1  25.4

DANU_Utilizations %>% group_by(p1_OralExp) %>% summarise(n = weighted.mean(drug_fdb_classes, weight))

 p1_OralExp     n
       <int> <dbl>
1          0  22.9
2          1  25.5

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age, gender)

DANU_Utilizations <- DANU_Utilizations %>% left_join(DANU_Demographics, by = c("patient" = "patid"))

DANU_Utilizations_tempOral <- DANU_Utilizations %>% select(-c(weight, p1_InjExp))

DANU_Utilizations_tempOral$p1_OralExp <- as.factor(DANU_Utilizations_tempOral$p1_OralExp)

DANU_Utilizations_tempOral$p1_OralExp <- relevel(DANU_Utilizations_tempOral$p1_OralExp, "0")

DANU_Utilizations_tempOral <- DANU_Utilizations_tempOral %>% select(-patient)

DANU_Utilizations_tempOral <- DANU_Utilizations_tempOral %>% mutate(gender = ifelse(gender=="M", 1, 0))

DANU_Utilizations_tempOral$gender <- as.factor(DANU_Utilizations_tempOral$gender)

DANU_Utilizations_tempOral$gender <- relevel(DANU_Utilizations_tempOral$gender, "0")

DANU_Utilizations_tempOral[,1] <- scale(DANU_Utilizations_tempOral[,1])
DANU_Utilizations_tempOral[,2] <- scale(DANU_Utilizations_tempOral[,2])
DANU_Utilizations_tempOral[,3] <- scale(DANU_Utilizations_tempOral[,3])
DANU_Utilizations_tempOral[,4] <- scale(DANU_Utilizations_tempOral[,4])
DANU_Utilizations_tempOral[,5] <- scale(DANU_Utilizations_tempOral[,5])
DANU_Utilizations_tempOral[,6] <- scale(DANU_Utilizations_tempOral[,6])
DANU_Utilizations_tempOral[,7] <- scale(DANU_Utilizations_tempOral[,7])
DANU_Utilizations_tempOral[,8] <- scale(DANU_Utilizations_tempOral[,8])
DANU_Utilizations_tempOral[,9] <- scale(DANU_Utilizations_tempOral[,9])
DANU_Utilizations_tempOral[,10] <- scale(DANU_Utilizations_tempOral[,10])
DANU_Utilizations_tempOral[,11] <- scale(DANU_Utilizations_tempOral[,11])
DANU_Utilizations_tempOral[,12] <- scale(DANU_Utilizations_tempOral[,12])
DANU_Utilizations_tempOral[,13] <- scale(DANU_Utilizations_tempOral[,13])
DANU_Utilizations_tempOral[,14] <- scale(DANU_Utilizations_tempOral[,14])
DANU_Utilizations_tempOral[,15] <- scale(DANU_Utilizations_tempOral[,15])
DANU_Utilizations_tempOral[,16] <- scale(DANU_Utilizations_tempOral[,16])
DANU_Utilizations_tempOral[,17] <- scale(DANU_Utilizations_tempOral[,17])
DANU_Utilizations_tempOral[,18] <- scale(DANU_Utilizations_tempOral[,18])
DANU_Utilizations_tempOral[,19] <- scale(DANU_Utilizations_tempOral[,19])
DANU_Utilizations_tempOral[,20] <- scale(DANU_Utilizations_tempOral[,20])
DANU_Utilizations_tempOral[,21] <- scale(DANU_Utilizations_tempOral[,21])
DANU_Utilizations_tempOral[,22] <- scale(DANU_Utilizations_tempOral[,22])
DANU_Utilizations_tempOral[,23] <- scale(DANU_Utilizations_tempOral[,23])
DANU_Utilizations_tempOral[,25] <- scale(DANU_Utilizations_tempOral[,25])
DANU_Utilizations_tempOral[,26] <- scale(DANU_Utilizations_tempOral[,26])



p1_OralExp_pred_model <- glm( p1_OralExp ~  drug_fdb_classes +age, data = DANU_Utilizations_tempOral, family = binomial)
summary(p1_OralExp_pred_model)




Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep = "\t")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% filter(p2 == 60) %>% select( -c(p1, p2))

Cum_Class_Experience_EveryMonth <- Treatment_exp_Vector %>% inner_join(Cum_Class_Experience_EveryMonth)

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% select(patient, weight, p1_OralExp, p1_InjExp)



# --------------------------------------------------------------------------------------

# T2DM Drug_Utilizations ------------------------------------------------------------------------------------------------------


DANU_Drug_Utilizations <- fread("DANU Drug Utilizations.txt")

DANU_Drug_Utilizations <- DANU_Drug_Utilizations %>% inner_join(Cum_Class_Experience_EveryMonth %>% select(-weight), by=c("patid" = "patient"))

names(DANU_Drug_Utilizations)

 [1] "patid"                
 [2] "weight"               
 [3] "drug_ahfs_class"      
 [4] "prescription_count"   
 [5] "rx_visits"            
 [6] "drug_supply_days"     
 [7] "brandname_supply_days"
 [8] "generic_supply_days"  
 [9] "drug_formulations"    
[10] "drug_ingredients"     
[11] "p1_OralExp"           
[12] "p1_InjExp"

DANU_Drug_Utilizations %>% select(p1_InjExp, patid) %>% distinct() %>% group_by(p1_InjExp) %>% count()

  p1_InjExp      n
      <int>  <int>
1         0 179857
2         1  40833


temp <- DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n=sum(prescription_count)) %>%
  mutate(prescriptionsPerPat=ifelse(p1_InjExp==0,n/179857,n/40833)) %>% arrange(drug_ahfs_class)

temp <- temp %>% select(p1_InjExp, drug_ahfs_class, prescriptionsPerPat) %>% distinct()  %>% spread(key=p1_InjExp, value=prescriptionsPerPat) %>%
  mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
  mutate(`1` = ifelse(is.na(`1`),0,`1`)) %>%
  mutate(foldChange = `1`/`0`) %>% 
  arrange(-foldChange)

data.frame(temp %>% 
             mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_")) %>%
             slice(251:388))





DANU_Drug_Utilizations %>% select(p1_OralExp, patid) %>% distinct() %>% group_by(p1_OralExp) %>% count()

  p1_OralExp      n
       <int>  <int>
1          0 218640
2          1   2050


temp <- DANU_Drug_Utilizations %>% group_by(p1_OralExp, drug_ahfs_class) %>% summarise(n=sum(prescription_count)) %>%
  mutate(prescriptionsPerPat=ifelse(p1_OralExp==0,n/218640,n/2050)) %>% arrange(drug_ahfs_class)

temp <- temp %>% select(p1_OralExp, drug_ahfs_class, prescriptionsPerPat) %>% distinct()  %>% spread(key=p1_OralExp, value=prescriptionsPerPat) %>%
  mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
  mutate(`1` = ifelse(is.na(`1`),0,`1`)) %>%
  mutate(foldChange = `1`/`0`) %>% 
  arrange(-foldChange)

data.frame(temp %>% 
             mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_")) %>%
             slice(251:388))











temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n=sum(prescription_count)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(251:370)


temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n = weighted.mean(rx_visits, weight)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(251:370)



temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n = weighted.mean(drug_supply_days, weight)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(251:370)





temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n = weighted.mean(brandname_supply_days, weight)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(251:301)






temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n = weighted.mean(brandname_supply_days, weight)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(251:301)






temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n = weighted.mean(generic_supply_days, weight)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(251:312)






temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n = weighted.mean(drug_formulations, weight)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(250:370)






temp <- data.frame(data.frame(DANU_Drug_Utilizations %>% group_by(p1_InjExp, drug_ahfs_class) %>% summarise(n = weighted.mean(drug_ingredients, weight)) %>%
             spread(key=p1_InjExp, value=n) %>% 
             mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
             mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
  mutate(foldChange = X1/X0) %>%
  arrange(-foldChange))

temp <- temp %>% filter(X0!=0 & X1!=0) %>%
  mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))

temp %>% slice(250:370)

# --------------------------------------------------------------------------------------

# T2DM Specialty_Utilizations ------------------------------------------------------------------------------------------------------


# DANU_Specialty_Utilizations <- fread("DANU Specialty Utilizations.txt")
# 
# DANU_Specialty_Utilizations <- DANU_Specialty_Utilizations %>% inner_join(Cum_Class_Experience_EveryMonth %>% select(-weight), by=c("patid" = "patient"))
# 
# names(DANU_Specialty_Utilizations)
# 
#  [1] "patid"               "weight"              "specialty_priority" 
#  [4] "physician_specialty" "medical_visits"      "rx_visits"          
#  [7] "encounter_visits"    "medical_providers"   "rx_providers"       
# [10] "encounter_providers" "p1_OralExp"          "p1_InjExp"
# 
# 
# 
# 
# temp <- data.frame(data.frame(DANU_Specialty_Utilizations %>% group_by(p1_OralExp, physician_specialty) %>% summarise(n = weighted.mean(medical_visits, weight)) %>%
#              spread(key=p1_OralExp, value=n) %>% 
#              mutate(`0` = ifelse(is.na(`0`),0,`0`)) %>%
#              mutate(`1` = ifelse(is.na(`1`),0,`1`))) %>%
#   mutate(foldChange = X1/X0) %>%
#   arrange(-foldChange))
# 
# temp <- temp %>% filter(X0!=0 & X1!=0) %>%
#   mutate(drug_ahfs_class = str_replace_all(drug_ahfs_class, " ", "_"))
# 
# temp %>% slice(250:370)

# ------------------------------------------------------------------------------------------------------

# OBESITY AND BMIs --------------------------------------------------------------------------------------------


DANU_Utilizations <- fread("DANU Utilizations.txt")

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% select(patid, code, weight) %>% distinct()
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)

DANU_Utilizations <- DANU_Events %>% inner_join(DANU_Utilizations)



DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, age, gender)

DANU_Utilizations <- DANU_Utilizations %>% left_join(DANU_Demographics)


for (i in 4:27) {
  print(
    cor(DANU_Utilizations$code, DANU_Utilizations[,i])
    )
}

range(DANU_Utilizations$code)

DANU_Utilizations <- DANU_Utilizations %>% ungroup()
DANU_Utilizations <- DANU_Utilizations %>% filter(code>25) 

BMI.lm <- lm(code ~ commercial_payer_prescriptions + age, data = DANU_Utilizations)
summary(BMI.lm)


for(i in names(DANU_Utilizations)[-1]){
  print(i)
  print(
    summary(lm(code ~ get(i) +  age, DANU_Utilizations))
    )
}



for(i in names(DANU_Utilizations)[-1]){
  print(i)
  print(
    summary(lm(code ~ get(i) , DANU_Utilizations))
    )
}



DANU_Utilizations %>%
  ggplot(aes(code, drug_ahfs_classes)) + 
  geom_smooth(size=1, colour="firebrick", fill="midnightblue") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n MAX  BMI reached") +
  ylab("No. American Hosp. Form. Service Drug Classes \n")




DANU_Utilizations %>%
  ggplot(aes(code, prescription_count)) + 
  geom_smooth( size=1, colour="firebrick", fill="midnightblue") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n MAX  BMI reached") +
  ylab("No. of Prescriptions \n")




DANU_Utilizations %>%
  ggplot(aes(code, emergency_visits)) + 
  geom_smooth( size=1, colour="firebrick", fill="midnightblue") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n MAX  BMI reached") +
  ylab("No. of Emergency Visits \n")




DANU_Utilizations %>%
  ggplot(aes(code, hospital_day_stays)) + 
  geom_smooth( size=1, colour="firebrick", fill="midnightblue") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n MAX  BMI reached") +
  ylab("No. of Hospital day stays \n")

# -----------------------------------------------------
# DANU utilizations among Obesity comorbidities --------------------------

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity") %>% select(patid, weight)
OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- fread("DIA Comorbidity Inventories.txt")
Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% full_join(DIA_Comorbidity_Inventories) %>% distinct()
Comorbidity_Inventories <- Comorbidity_Inventories %>% inner_join(DANU_Demographics)
length(unique(Comorbidity_Inventories$patid))

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- Comorbidity_Inventories %>% select(patid) %>% distinct() %>% left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1)
Comorbidity_Inventories <- DANU_Events %>% filter(code>=27) %>% select(patid) %>% distinct() %>% left_join(Comorbidity_Inventories)

Comorbidity_Inventories %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 84173305
Comorbidity_Inventories <- Comorbidity_Inventories %>% ungroup()


DIABETES <-  Comorbidity_Inventories %>% filter(grepl("E11", diagnosis)) %>% select(patid, weight)  %>% distinct()

CVD <- Comorbidity_Inventories %>% filter(grepl("I20", diagnosis)|
                                   grepl("I21", diagnosis)|
                                   grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)|
                                   grepl("I24", diagnosis)|
                                   grepl("I25", diagnosis)|
                                   grepl("I4", diagnosis)|
                                   grepl("I6", diagnosis)|
                                   grepl("G45", diagnosis)) %>% select(patid, weight)  %>% distinct()

HF <- Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight)  %>% distinct()

SLEEPAPNEA <- Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid, weight)  %>% distinct()

OSTEOARTHRITIS <- Comorbidity_Inventories %>% filter(grepl("M15", diagnosis)|
                                     grepl("M16", diagnosis)|
                                     grepl("M17", diagnosis)|
                                     grepl("M18", diagnosis)|
                                     grepl("M19", diagnosis)) %>% select(patid, weight)  %>% distinct()

DISLIPIDEMIA <- Comorbidity_Inventories %>% filter(grepl("E78", diagnosis)) %>% select(patid, weight)  %>% distinct()

HYPERTENSION <- Comorbidity_Inventories %>% filter(grepl("I10", diagnosis)) %>% select(patid, weight)  %>% distinct()

PAD <- Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|
                                     grepl("I73", diagnosis)) %>% select(patid, weight)  %>% distinct()

CANCER <- Comorbidity_Inventories %>% filter(grepl("C", diagnosis)) %>% select(patid, weight)  %>% distinct()

CKD <- Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight)  %>% distinct()

POS <- Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid, weight)  %>% distinct()

GERD <- Comorbidity_Inventories %>% filter(grepl("K21", diagnosis)) %>% select(patid, weight)  %>% distinct()

StressIncontinence <- Comorbidity_Inventories %>% filter(grepl("N39", diagnosis)) %>% select(patid, weight)  %>% distinct()

NASH <- Comorbidity_Inventories %>% filter(grepl("K75", diagnosis)) %>% select(patid, weight)  %>% distinct()

PREDIABETES <- Comorbidity_Inventories %>% filter(grepl("R73", diagnosis)) %>% select(patid, weight)  %>%  anti_join(DIABETES %>% select(patid)) %>% distinct()

RA <- Comorbidity_Inventories %>% filter(grepl("M05", diagnosis)|grepl("M06", diagnosis)) %>% select(patid, weight)  %>% distinct()
  

DIABETES$disease <- "DIABETES"
CVD$disease <- "CVD"
SLEEPAPNEA$disease <- "SLEEPAPNEA"
OSTEOARTHRITIS$disease <- "OSTEOARTHRITIS"
DISLIPIDEMIA$disease <- "DISLIPIDEMIA"
HYPERTENSION$disease <- "HYPERTENSION"
PAD$disease <- "PAD"
CANCER$disease <- "CANCER"
CKD$disease <- "CKD"
POS$disease <- "POS"
GERD$disease <- "GERD"
StressIncontinence$disease <- "StressIncontinence"
HF$disease <- "HF"
NASH$disease <- "NASH"
RA$disease <- "RA"
PREDIABETES$disease <- "PREDIABETES"

temp <- DIABETES %>% full_join(CVD) %>% full_join(SLEEPAPNEA) %>% full_join(OSTEOARTHRITIS) %>% full_join(DISLIPIDEMIA) %>%
  full_join(HYPERTENSION) %>% full_join(PAD) %>% full_join(CANCER) %>% full_join(CKD) %>% full_join(POS) %>%
  full_join(GERD) %>% full_join(StressIncontinence) %>% full_join(HF) %>% full_join(NASH) %>% full_join(RA) %>%  full_join(PREDIABETES) 



DANU_Utilizations <- fread("DANU Utilizations.txt")
DANU_Utilizations <- temp %>% select(patid) %>% distinct() %>% inner_join(DANU_Utilizations)

DANU_Utilizations <- temp %>% left_join(DANU_Utilizations)


start <- data.frame(unique(DANU_Utilizations$disease))
names(start)[1] <- "disease"

for (i in names(DANU_Utilizations[,4:26])){
  print(i)
  start <- start %>% bind_cols(DANU_Utilizations %>% group_by(disease) %>% summarise(n=mean(get(i))) )
  print(start)
}

start <- start %>% select(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47)

names(start) <- c("disease","medical_visits","rx_visits","encounter_visits","medical_providers",
                  "rx_providers","encounter_providers","hospital_days","hospital_stays", "hospital_day_stays",
                  "hospital_short_stays","hospital_medium_stays","hospital_long_stays","icu_visits",
                  "surgery_visits","emergency_visits","prescription_count","public_payer_prescriptions",
                  "commercial_payer_prescriptions","drug_supply_days","drug_formulations","drug_ingredients",
                  "drug_ahfs_classes","drug_fdb_classes")

start

fwrite(start, "Obesity_Comorbidities_Utilizations.txt", sep="\t")
# ------------------------------------------------------------------------------------
# BMI buckets and drug penetrance each year forecast ---------------

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight, obesity_onset)

Amphet_to_remove <- fread("Amphet_to_remove.txt")

DANU_Demographics <- DANU_Demographics %>% anti_join(Amphet_to_remove)
DANU_Demographics$obesity_onset <- as.Date(DANU_Demographics$obesity_onset)

DANU_Demographics %>% filter(obesity_onset<="2017-12-31") %>% summarise(n=sum(weight)) # 67666343
DANU_Demographics %>% filter(obesity_onset<="2018-12-31") %>% summarise(n=sum(weight)) # 80657511
DANU_Demographics %>% filter(obesity_onset<="2019-12-31") %>% summarise(n=sum(weight)) # 90915685
DANU_Demographics %>% filter(obesity_onset<="2020-12-31") %>% summarise(n=sum(weight)) # 96737643
DANU_Demographics %>% filter(obesity_onset<="2021-12-31") %>% summarise(n=sum(weight)) # 98646843

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Events)

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

DANU_Events <- DANU_Events %>% group_by(patid) %>% slice(n())

DANU_Events <- DANU_Events %>% mutate(code = ifelse(code<27, "<27", ifelse(code>=27&code<30, "27_to_30", ">30")))

DANU_Demographics <- DANU_Events %>% select(patid, code) %>% inner_join(DANU_Demographics) %>% ungroup() 

OBE_Doses <- fread("OBE Doses.txt")
names(OBE_Doses)[5] <- "patid"

OBE_Doses <- DANU_Demographics %>% select(patid) %>% inner_join(OBE_Doses)
OBE_Doses <- OBE_Doses %>% filter(paid=="P") %>% select(patid, drug_class, from_dt) %>% distinct()

OBE_Doses$from_dt <- as.Date(OBE_Doses$from_dt)

OBE_Doses <- DANU_Demographics %>% left_join(OBE_Doses)

OBE_Doses <- OBE_Doses %>%  mutate(obesity_onset=as.character(obesity_onset)) %>% 
  mutate(obesity_onset=str_sub(obesity_onset, 1L, 4L)) %>%
  mutate(obesity_onset=ifelse(obesity_onset=="2015"|obesity_onset=="2016"|obesity_onset=="2017", "2018", obesity_onset))


OBE_Doses <- OBE_Doses %>%  mutate(obesity_onset=as.character(obesity_onset)) %>% 
  mutate(obesity_onset=str_sub(obesity_onset, 1L, 4L)) %>%
  mutate(obesity_onset=ifelse(obesity_onset=="2015"|obesity_onset=="2016"|obesity_onset=="2017", "2018", obesity_onset))

range(OBE_Doses$from_dt, na.rm=T)

OBE_Doses %>% select(patid, code, weight) %>% distinct() %>% group_by(code) %>% summarise(n=sum(weight))
  

unique(OBE_Doses$drug_class)

OBE_Doses %>% group_by(drug_class) %>% count()

OBE_Doses %>% 
  filter(grepl("GLP1 Oral", drug_class)) %>%
  filter(from_dt<="2018-12-31"&from_dt>="2018-01-01") %>%
  select(patid, code, weight) %>% distinct() %>% group_by(code) %>% summarise(n=sum(weight))
 

# ------------------
# Obesity only 5 comorbidities, age gender GLP1 penetrance  ---------------------------------------------------
DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight)

OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")

Comorbidity_Inventories <- OBE_Comorbidity_Inventories
Comorbidity_Inventories <- Comorbidity_Inventories %>% inner_join(DANU_Demographics)
length(unique(Comorbidity_Inventories$patid))

Comorbidity_Inventories %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 106172398

# HF   --  8562239   
Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
HF <- Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight)  %>% distinct()

# PAD   --  7611787  
Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|
                                     grepl("I73", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
PAD <- Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|
                                     grepl("I73", diagnosis)) %>% select(patid, weight)  %>% distinct()

# CKD   --  5860767
Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
CKD <- Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight)  %>% distinct()

# POS   --  2138051
Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
POS <- Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid, weight)  %>% distinct()

# Sleep Apnea -- 25267754
Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
SLEEPAPNEA <- Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid, weight)  %>% distinct()


OBE_Doses <- fread("OBE Doses.txt")
OBE_Doses <- OBE_Doses %>% select(pat_id, drug_class) %>% distinct()
OBE_Doses <- OBE_Doses %>% filter(grepl("GLP", drug_class))
names(OBE_Doses)[1] <- "patid"

DANU_Demographics <- fread("DANU Demographics.txt")

HF %>% left_join(DANU_Demographics) %>% summarise(n=weighted.mean(age, weight))
PAD %>% left_join(DANU_Demographics) %>% summarise(n=weighted.mean(age, weight))
CKD %>% left_join(DANU_Demographics) %>% summarise(n=weighted.mean(age, weight))
POS %>% left_join(DANU_Demographics) %>% summarise(n=weighted.mean(age, weight))
SLEEPAPNEA %>% left_join(DANU_Demographics) %>% summarise(n=weighted.mean(age, weight))


HF %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight)/8562239)
PAD %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight)/7611787)
CKD %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight)/5860767)
POS %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight)/2138051)
SLEEPAPNEA %>% left_join(DANU_Demographics) %>% group_by(gender) %>% summarise(n=sum(weight)/25267754)



HF %>% inner_join(OBE_Doses %>% filter(grepl("Oral", drug_class)) %>% select(patid) %>% distinct()) %>% summarise(n=sum(weight)/8562239)
PAD %>% inner_join(OBE_Doses %>% filter(grepl("Oral", drug_class)) %>%  select(patid) %>% distinct()) %>% summarise(n=sum(weight)/7611787)
CKD %>% inner_join(OBE_Doses %>% filter(grepl("Oral", drug_class)) %>%  select(patid) %>% distinct()) %>% summarise(n=sum(weight)/5860767)
POS %>% inner_join(OBE_Doses %>% filter(grepl("Oral", drug_class)) %>%  select(patid) %>% distinct()) %>% summarise(n=sum(weight)/2138051)
SLEEPAPNEA %>% inner_join(OBE_Doses %>% filter(grepl("Oral", drug_class)) %>%  select(patid) %>% distinct()) %>% summarise(n=sum(weight)/25267754)


# Obesity only/any vs Diabetes only/any 5 comorbidity penetrance ---------------------------------------------------


DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes"|diagnosis=="Diabetes + Obesity") %>% select(patid, weight, diagnosis)
OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- fread("DIA Comorbidity Inventories.txt")
Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% full_join(DIA_Comorbidity_Inventories) %>% distinct()

names(DANU_Demographics)[3] <- "Type"
Comorbidity_Inventories <- Comorbidity_Inventories %>% inner_join(DANU_Demographics)
length(unique(Comorbidity_Inventories$patid))

DANU_Demographics %>% group_by(Type) %>% summarise(n=sum(weight))
 
# 1 Diabetes             7949715.
# 2 Diabetes + Obesity  40282960.
# 3 Obesity            106469049.


#HF
Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))

# PAD  
Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|grepl("I73", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))

# CKD   
Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))

# POS  
Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))

# Sleep Apnea 
Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))

# --------------------
# DIA HF CKD, SGLT2 GLP1 -------------------

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Diabetes"|diagnosis=="Diabetes + Obesity") %>% select(patid, weight)

DIA_Comorbidity_Inventories <- fread("DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- DIA_Comorbidity_Inventories %>% inner_join(DANU_Demographics)
length(unique(DIA_Comorbidity_Inventories$patid))

DIA_Comorbidity_Inventories %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 48180871

# HF  
DIA_Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
HF <- DIA_Comorbidity_Inventories %>% filter(grepl("I5", diagnosis)) %>% select(patid, weight)  %>% distinct()
HF$HF <- "1"
# CKD  
DIA_Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight)  %>% distinct() %>% summarise(n=sum(weight))
CKD <- DIA_Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight)  %>% distinct()
CKD$CKD <- "1"

DANU_Demographics <- DANU_Demographics %>% left_join(HF) %>% left_join(CKD)
DANU_Demographics[is.na(DANU_Demographics)] <- 0

DANU_Demographics <- DANU_Demographics %>%
   mutate(disease_group=ifelse(HF==1&CKD==1, "HF+CKD",
                               ifelse(HF==1, "HF",
                                      ifelse(CKD==1, "CKD", "none"))))

DANU_Demographics <- DANU_Demographics %>% select(-c(HF, CKD))

DIA_Doses <- fread("DIA Doses.txt")
DIA_Doses <- DIA_Doses %>% select(pat_id, drug_class) %>% distinct() %>% filter(drug_class=="SGLT2"|grepl("GLP", drug_class))
names(DIA_Doses)[1] <- "patid"
DIA_Doses <- DIA_Doses %>% mutate(drug_class=ifelse(drug_class!="SGLT2", "GLP1", drug_class))
DIA_Doses <- DIA_Doses %>% mutate(Exp=1) %>% distinct() %>% spread(key=drug_class, value=Exp)
DIA_Doses[is.na(DIA_Doses)] <- 0

DIA_Doses <- DIA_Doses %>%
   mutate(drug_group=ifelse(GLP1==1&SGLT2==1, "GLP1+SGLT2",
                               ifelse(GLP1==1, "GLP1",
                                      ifelse(SGLT2==1, "SGLT2", "none"))))  %>% select(-c(GLP1, SGLT2))

DANU_Demographics <- DANU_Demographics %>% left_join(DIA_Doses) %>% mutate(drug_group=ifelse(is.na(drug_group), "none", drug_group))

DANU_Demographics %>% group_by(disease_group, drug_group) %>% summarise(n=sum(weight)) %>% ungroup() %>%
  spread(key=disease_group, value=n)

  drug_group      CKD       HF `HF+CKD`      none
  <chr>         <dbl>    <dbl>    <dbl>     <dbl>
1 GLP1        434495.  445768.  396607.  2373624.
2 GLP1+SGLT2  263090.  344740.  212129.  1502558.
3 none       4176046. 5295655. 4048295. 25909466.
4 SGLT2       298078.  408267.  273183.  1850675.
# ---------------------------------

# Add of label from Diabetes ----------------------
DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity") %>% select(patid, weight, obesity_onset)

Amphet_to_remove <- fread("Amphet_to_remove.txt")

DANU_Demographics <- DANU_Demographics %>% anti_join(Amphet_to_remove)
DANU_Demographics$obesity_onset <- as.Date(DANU_Demographics$obesity_onset)

DANU_Demographics %>% filter(obesity_onset<="2017-12-31") %>% summarise(n=sum(weight)) # 67666343
DANU_Demographics %>% filter(obesity_onset<="2018-12-31") %>% summarise(n=sum(weight)) # 80657511
DANU_Demographics %>% filter(obesity_onset<="2019-12-31") %>% summarise(n=sum(weight)) # 90915685
DANU_Demographics %>% filter(obesity_onset<="2020-12-31") %>% summarise(n=sum(weight)) # 96737643
DANU_Demographics %>% filter(obesity_onset<="2021-12-31") %>% summarise(n=sum(weight)) # 98646843

OBE_Doses <- fread("OBE Doses.txt")
names(OBE_Doses)[5] <- "patid"

OBE_Doses <- DANU_Demographics %>% select(patid) %>% inner_join(OBE_Doses)
OBE_Doses <- OBE_Doses %>% filter(paid=="P") %>% select(patid, drug_class, from_dt) %>% distinct()

OBE_Doses$from_dt <- as.Date(OBE_Doses$from_dt)

OBE_Doses <- DANU_Demographics %>% left_join(OBE_Doses)

OBE_Doses <- OBE_Doses %>%  mutate(obesity_onset=as.character(obesity_onset)) %>% 
  mutate(obesity_onset=str_sub(obesity_onset, 1L, 4L)) %>%
  mutate(obesity_onset=ifelse(obesity_onset=="2015"|obesity_onset=="2016"|obesity_onset=="2017", "2018", obesity_onset))


range(OBE_Doses$from_dt, na.rm=T)


unique(OBE_Doses$drug_class)

OBE_Doses %>% group_by(drug_class) %>% count()

OBE_Doses %>% 
  filter(grepl("GLP", drug_class)) %>%
  filter(from_dt<="2019-12-31"&from_dt>="2019-01-01") %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))
 



temp <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t")
temp <- temp %>% filter(p2==60) %>% filter((  (p1_OralExp==1 | p1_InjExp==1) & p1_InsulinExp==0 & p1_SGLT2Exp==0 & p1_DPP4Exp==0 & p1_AntiDiabeticExp==0))
temp <- temp %>% select(-c(cumflow, Lines, p1_AntiDiabeticExp, p1_DPP4Exp, p1_SGLT2Exp, p1,p2, p1_InsulinExp))
temp %>% group_by(p1_InjExp, p1_OralExp, p1_BiguanideExp) %>% count() 
Off_label_DIA <- temp %>% select(patient)
DIA_Doses <- fread("DIA Doses.txt")
names(DIA_Doses)[5] <- "patient"
DIA_Doses <- Off_label_DIA %>% select(patient) %>% inner_join(DIA_Doses)
DIA_Doses <- DIA_Doses %>% filter(paid=="P") %>% select(patient, weight, drug_class, from_dt) %>% distinct() %>%  filter(grepl("GLP", drug_class))
names(DIA_Doses)[1] <- "patid"
DIA_Doses$from_dt <- as.Date(DIA_Doses$from_dt)


range(DIA_Doses$from_dt)

DIA_Doses %>%
  bind_rows(OBE_Doses %>% select(patid, weight, drug_class, from_dt) ) %>%
  #filter(grepl("GLP1", drug_class)) %>%
 filter(!is.na(drug_class)) %>%
  filter(from_dt<="2018-12-31"&from_dt>="2018-01-01") %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight))



DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DIA_Doses %>% select(patid) %>% distinct() %>% bind_rows(OBE_Doses %>% select(patid) %>% distinct())  %>% distinct() %>%  left_join(DANU_Events)
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% slice(n())
DANU_Events <- DANU_Events %>% mutate(code = ifelse(code<27, "<27", ifelse(code>=27&code<30, "27_to_30", ">30")))
DANU_Events <- DANU_Events %>% select(patid, code) %>% ungroup() 


DIA_Doses %>%
  bind_rows(OBE_Doses %>% select(patid, weight, drug_class, from_dt) ) %>%
  inner_join(DANU_Events)  %>%
  filter(grepl("GLP1", drug_class)) %>%
  #filter(!is.na(drug_class)) %>%
  filter(from_dt<="2018-12-31"&from_dt>="2018-01-01") %>%
  select(patid, weight, code) %>% distinct() %>% group_by(code) %>% summarise(n=sum(weight))


 

# -----------------------------
# Obesity & Diabetes comorbidity Apr 27 ---------------------------------------------------


DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes"|diagnosis=="Diabetes + Obesity") %>% select(patid, weight, diagnosis)
OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- fread("DIA Comorbidity Inventories.txt")
Comorbidity_Inventories <- OBE_Comorbidity_Inventories %>% full_join(DIA_Comorbidity_Inventories) %>% distinct()

names(DANU_Demographics)[3] <- "Type"
Comorbidity_Inventories <- Comorbidity_Inventories %>% inner_join(DANU_Demographics)
length(unique(Comorbidity_Inventories$patid))
DANU_Demographics <- DANU_Demographics %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type))
Comorbidity_Inventories <- Comorbidity_Inventories %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type))

DANU_Demographics %>% group_by(Type) %>% summarise(n=sum(weight))
# 1 Diabetes  48232675.
# 2 Obesity  106469049.

First_Diastolic_All <- fread("First_Diastolic_All.txt")
First_Systolic_All <- fread("First_Systolic_All.txt")

#HF
First_Diastolic_All %>% full_join(First_Systolic_All) %>% select(patient) %>% rename("patid"="patient") %>%
  inner_join(DANU_Demographics)  %>% group_by(Type) %>% summarise(n=sum(weight))
#HFpEF
First_Diastolic_All %>% select(patient) %>% rename("patid"="patient") %>%
  inner_join(DANU_Demographics)  %>% group_by(Type) %>% summarise(n=sum(weight))
# PAD  
Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|grepl("I73", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))
# CKD   
Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))
# POS  
Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))
# Sleep Apnea 
Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid, weight, Type)  %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))


Comorbs <- First_Diastolic_All %>% select(patient) %>% rename("patid"="patient")  %>% mutate(group="HFpEF") %>% inner_join(DANU_Demographics) %>%
  bind_rows(Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|grepl("I73", diagnosis)) %>% select(patid)  %>% distinct() %>% mutate(group="PAD")  %>% inner_join(DANU_Demographics)) %>%
  bind_rows(Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid)  %>% distinct() %>% mutate(group="CKD")  %>% inner_join(DANU_Demographics)) %>%
  bind_rows(Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid)  %>% distinct() %>% mutate(group="POS")  %>% inner_join(DANU_Demographics)) %>%
bind_rows(Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid)  %>% distinct() %>% mutate(group="SA")  %>% inner_join(DANU_Demographics))


Comorbs %>% select(patid, weight, Type) %>% distinct() %>% group_by(Type) %>% summarise(n=sum(weight))
 
BMIs <- fread("DANU Events.txt")
BMIs <- BMIs %>% filter(grepl("BMI", code))       
BMIs$code <- as.character(BMIs$code)
BMIs$code <- parse_number(BMIs$code)
BMIs <- BMIs %>% select(patid, code, weight) %>% distinct()
BMIs <- BMIs %>% group_by(patid) %>% filter(code==max(code)) %>% slice(1) %>% select(patid, code) %>% distinct()
BMIs <- BMIs %>% ungroup() %>% mutate(code=ifelse(code>=30, ">30", ifelse(code>=27, "27-30", ">25")))

BMIs %>% inner_join(DANU_Demographics) %>% group_by(code, Type) %>% summarise(n=sum(weight))

#   code  Type                       n
#   <chr> <chr>                  <dbl>
# 1 >25   Diabetes            2267950.
# 2 >25   Diabetes + Obesity  3205508.
# 3 >25   Obesity            16638826.
# 4 >30   Diabetes + Obesity 18878355.
# 5 >30   Obesity            40779460.
# 6 27-30 Diabetes + Obesity  4678719.
# 7 27-30 Obesity            20099558.


BMIs %>% inner_join(DANU_Demographics) %>% filter(Type=="Diabetes + Obesity") %>%
  inner_join(First_Diastolic_All %>% full_join(First_Systolic_All) %>% select(patient) %>% rename("patid"="patient")) %>%
  group_by(code) %>% summarise(n=sum(weight))


BMIs %>% inner_join(DANU_Demographics) %>% filter(Type=="Diabetes + Obesity") %>%
  inner_join(First_Diastolic_All %>% select(patient) %>% rename("patid"="patient")) %>%
  group_by(code) %>% summarise(n=sum(weight))

BMIs %>% inner_join(DANU_Demographics) %>% filter(Type=="Diabetes + Obesity") %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  group_by(code) %>% summarise(n=sum(weight))


BMIs %>% inner_join(DANU_Demographics) %>% filter(Type=="Diabetes + Obesity") %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|grepl("I73", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  group_by(code) %>% summarise(n=sum(weight))


BMIs %>% inner_join(DANU_Demographics) %>% filter(Type=="Diabetes + Obesity") %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  group_by(code) %>% summarise(n=sum(weight))

BMIs %>% inner_join(DANU_Demographics) %>% filter(Type=="Diabetes + Obesity") %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  group_by(code) %>% summarise(n=sum(weight))



HbA1c <- fread("DANU Measures.txt")
HbA1c <- HbA1c %>% filter(test=="HbA1c Level") %>% select(patid, value) %>% distinct()
HbA1c <- HbA1c %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% select(patid, value) %>% distinct()

HbA1c %>% ungroup() %>% 
  ggplot(aes(value)) +
  geom_density()

HbA1c <- HbA1c %>% ungroup() %>% mutate(value=ifelse(value>=7.5, ">7.5", ifelse(value>=6.5, "6.5-7.5", "<6.5")))

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
names(Treatment_exp_Vector)[1] <- "patid"

HbA1c %>% inner_join(Treatment_exp_Vector) %>%
   inner_join(First_Diastolic_All %>% full_join(First_Systolic_All) %>% select(patient) %>% rename("patid"="patient")) %>%
  inner_join(DANU_Demographics) %>% group_by(value) %>% summarise(n=sum(weight))

HbA1c %>% inner_join(Treatment_exp_Vector) %>%
  inner_join(First_Diastolic_All %>% select(patient) %>% rename("patid"="patient")) %>%
  inner_join(DANU_Demographics) %>% group_by(value) %>% summarise(n=sum(weight))

HbA1c %>% inner_join(Treatment_exp_Vector) %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  inner_join(DANU_Demographics) %>% group_by(value) %>% summarise(n=sum(weight))

HbA1c %>% inner_join(Treatment_exp_Vector) %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("I70", diagnosis)|grepl("I73", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  inner_join(DANU_Demographics) %>% group_by(value) %>% summarise(n=sum(weight))

HbA1c %>% inner_join(Treatment_exp_Vector) %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("E28", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  inner_join(DANU_Demographics) %>% group_by(value) %>% summarise(n=sum(weight))

HbA1c %>% inner_join(Treatment_exp_Vector) %>%
  inner_join(Comorbidity_Inventories %>% filter(grepl("G47", diagnosis)) %>% select(patid)  %>% distinct()) %>%
  inner_join(DANU_Demographics) %>% group_by(value) %>% summarise(n=sum(weight))


DIA_Doses <- fread("DIA Doses.txt")
DIA_Doses <- DIA_Doses %>% filter(paid=="P") %>% select(pat_id, drug_group) %>% distinct() %>% rename("patid"="pat_id")



Comorbs %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type)) %>%
  inner_join(Treatment_exp_Vector) %>%
  filter(Type=="Diabetes") %>% select(-Type) %>%
 left_join(DIA_Doses) %>%
  group_by(group, drug_group)  %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=drug_group, value = n)


DANU_Specialty_Utilizations <- fread("DANU Specialty Utilizations.txt")
unique(DANU_Specialty_Utilizations$physician_specialty)


Comorbs %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type)) %>%
  group_by(Type, group) %>% summarise(n=sum(weight)) %>% 
  spread(key=Type, value=n)

Comorbs %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type)) %>%
  inner_join(DANU_Specialty_Utilizations %>% select(patid, physician_specialty) %>% distinct()) %>%
  group_by(Type, group, physician_specialty) %>% summarise(n=sum(weight)) %>%
  spread(key=physician_specialty, value=n)



DANU_Utilizations <- fread("DANU Utilizations.txt")

DANU_Utilizations <- Comorbs %>% left_join(DANU_Utilizations)
DANU_Utilizations <-  DANU_Utilizations %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type))

start <- data.frame(unique(DANU_Utilizations$group))

for (i in names(DANU_Utilizations[Type=="Obesity",5:27])){
  print(i)
  start <- start %>% bind_cols(DANU_Utilizations[Type=="Obesity",] %>% group_by(group) %>% summarise(n=mean(get(i))) )
  print(start)
}

start <- start %>% select(2,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47)

names(start) <- c( "group", "medical_visits","rx_visits","encounter_visits","medical_providers",
                  "rx_providers","encounter_providers","hospital_days","hospital_stays", "hospital_day_stays",
                  "hospital_short_stays","hospital_medium_stays","hospital_long_stays","icu_visits",
                  "surgery_visits","emergency_visits","prescription_count","public_payer_prescriptions",
                  "commercial_payer_prescriptions","drug_supply_days","drug_formulations","drug_ingredients",
                  "drug_ahfs_classes","drug_fdb_classes")


DANU_Utilizations <- fread("DANU Drug Utilizations.txt")
DANU_Utilizations <- DANU_Utilizations %>% select(patid, weight, drug_ahfs_class) %>% distinct()

DANU_Utilizations <- Comorbs %>% inner_join(DANU_Utilizations)
DANU_Utilizations <-  DANU_Utilizations %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type))

temp <- Comorbs %>% mutate(Type=ifelse(grepl("Diabetes", Type), "Diabetes", Type)) %>%
  group_by(Type, group) %>% summarise(n=sum(weight)) %>%
  left_join(DANU_Utilizations %>% group_by(Type, drug_ahfs_class) %>% summarise(or=sum(weight))) %>%
  left_join(DANU_Utilizations %>% group_by(Type, group, drug_ahfs_class) %>% summarise(n2=sum(weight))) %>%
  mutate(Pen=100*n2/n) %>%
  select(-c(n, n2)) %>% mutate(or=ifelse(Type=="Diabetes", 100*or/48232675, 100*or/106469049)) %>%
  spread(key=group, value=Pen) %>%
  filter(or>=2)

fwrite(temp, "temp.csv")

# ------------------------------------------------------