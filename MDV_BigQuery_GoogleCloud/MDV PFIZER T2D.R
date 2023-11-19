# -------------------------------------------------------------------------
  # Exploratory Data Analysis - DIabetes Files ----------------------------

# import lis
library(data.table)
library(tidyverse)
library(lubridate)
library(viridis)
library(ggsci)
library(scales)
library(RColorBrewer)

# keep R from using scientific notation for patient IDs
options(scipen = 999)

# ----------------------- IMPORT TABLES ---------------------------------------------------------------------
T2D_FF1_Data <- read.table("T2D_FF1_Data.csv", 
                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

M_KaCode <- 
  read.table("D:/Paulo_Bastos/PFIZER_JAPEN_T2_DIABETES_OBESITY/pfizer_dmo_20220131/pfizer_dmo_20220131/M_KaCode.txt", 
             header = TRUE, sep="\t",  colClasses = "character", stringsAsFactors = FALSE)



# ############################### FF1 DATA ################################################

# we have 202394 unique IDs T2D for the discharge table
T2D_FF1_Data_unique_IDS <- unique(T2D_FF1_Data$patientid)
length(T2D_FF1_Data_unique_IDS)

# Add DEPARTMENT NAME in ENGLISH
T2D_FF1_Data_w_DEPARTMENT <- T2D_FF1_Data %>% left_join(M_KaCode, by="kacodeuni")

# remove NAs and weights or heights = 0 prior to computing the BMI
T2D_FF1_Data <-
  T2D_FF1_Data %>% 
  filter(!is.na(T2D_FF1_Data$weight) & !is.na(T2D_FF1_Data$height) & T2D_FF1_Data$weight != 0 &  T2D_FF1_Data$height != 0)

# Add DEPARTMENT NAME in ENGLISH
T2D_FF1_Data_w_DEPARTMENT <- T2D_FF1_Data %>% left_join(M_KaCode, by="kacodeuni")

# check how many unique Paients now,  197263
T2D_FF1_Data_w_DEPARTMENT_unique_IDS <- unique(T2D_FF1_Data_w_DEPARTMENT$patientid)
length(T2D_FF1_Data_w_DEPARTMENT_unique_IDS)

str(T2D_FF1_Data_w_DEPARTMENT)
rm(M_KaCode, T2D_FF1_Data, T2D_FF1_Data_w_DEPARTMENT_unique_IDS, T2D_FF1_Data_unique_IDS)

#  Adding a BMI column
T2D_FF1_Data_w_DEPARTMENT$weight <- as.numeric(T2D_FF1_Data_w_DEPARTMENT$weight)
T2D_FF1_Data_w_DEPARTMENT$height <- as.numeric(T2D_FF1_Data_w_DEPARTMENT$height)

T2D_FF1_Data_w_DEPARTMENT <- T2D_FF1_Data_w_DEPARTMENT %>%
  select(patientid, weight, height, kacodeuni, kaname_eng) %>%
  mutate(BMI = weight / (height / 100)^2 )


# Checking how many unique DEPARTMENTS we have
# 58 unique departments
T2D_FF1_Data_w_DEPARTMENT_unique_DEPS <- unique(T2D_FF1_Data_w_DEPARTMENT$kacodeuni)
length(T2D_FF1_Data_w_DEPARTMENT_unique_DEPS)

# Again, removing some on the lower end that make no sense
# BMIs above 10.1 and below 100, there are many wrong values, which makes the max being 78 okayish
# cannot be BMI > 100 because it catches a bunch of 999s weights and heights
# BMIs distribution

T2D_FF1_Data_w_DEPARTMENT <- T2D_FF1_Data_w_DEPARTMENT %>%  
  filter(BMI > 10.1)

T2D_FF1_Data_w_DEPARTMENT <- T2D_FF1_Data_w_DEPARTMENT %>%  
  filter(BMI < 100)

range(T2D_FF1_Data_w_DEPARTMENT$BMI)

# jitter
T2D_FF1_Data_w_DEPARTMENT %>%  
  select(patientid, BMI) %>%
  mutate(typevar= typeof(patientid)) %>%
  ggplot(aes(x=typevar, y=BMI, color=BMI)) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  geom_jitter(width = 0.5, size = 2, alpha = 0.4, show.legend = F)+
  ggtitle("BMIs, T2D-related disorder patients \n (Hospital visits Apr 2008 - Oct 2021)")

# violin
median(T2D_FF1_Data_w_DEPARTMENT$BMI)  # 24.28501
mean(T2D_FF1_Data_w_DEPARTMENT$BMI)  # 24.94257

T2D_FF1_Data_w_DEPARTMENT %>%  
  select(patientid, BMI) %>%
  mutate(typevar= typeof(patientid)) %>%
  ggplot(aes(x=typevar, y=BMI)) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  geom_violin(alpha = 0.8, fill="brown4", show.legend = F, colour = "brown4")+
  ggtitle("BMIs, T2D-related disorder patients \n (Hospital visits Apr 2008 - Oct 2021)")



#### number of visits by medical deparment -----------------------------------
T2D_FF1_Data_w_DEPARTMENT %>%  
  select(patientid, kaname_eng) %>%
  group_by(kaname_eng) %>%
  arrange(kaname_eng) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  ggplot(aes(x=reorder(kaname_eng, n), y=n, fill = n))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ylim(0,140000)+
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of registered visits by Medical Department")+
  scale_fill_viridis_c() 

rm(T2D_FF1_Data_w_DEPARTMENT, h_line, T2D_FF1_Data_w_DEPARTMENT_unique_DEPS)




#######################################################################################################

T2DM_ICD10_all <- read.table("T2DM_ICD10_all.csv", 
                           header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# T2 DM _Disease_Data_ICD10_Buckets
data.frame(
  T2DM_ICD10_all %>%
    select(icd10code) %>%
    group_by(icd10code) %>%
    summarise(n = n()))


T2DM_ICD10_all %>%
  select(icd10code) %>%
  group_by(icd10code) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(icd10code,n), y=n))+
  geom_bar(position="dodge", stat="identity", fill="midnightblue", alpha=0.8)+
  ylim(0,42000000)+
  scale_x_discrete(labels=c("E11" =	"T2 DM",
                            "E110" =	"T2 DM w/ hyperosmolarity",
                            "E111" =	"T2 DM w/ ketoacidosis",
                            "E112"	= "T2 DM w/ kidney complications",
                            "E113"	= "T2 DM w/ ophthalmic complications",
                            "E114"	= "T2 DM w/ neurological complications",
                            "E115"	= "T2 DM w/ circulatory complications",
                            "E116"	= "T2 DM w/ other specified complications",
                            "E117"	= "T2 DM w/ multiple diabetic complications",
                            "E119"	= "T2 DM w/o complications",
                            "E12"	= "Malnutrition-related DM",
                            "E13"	= "Other specified DM",
                            "E130"	= "Other specified DM w/ hyperosmolarity w/o nonketotic hyperglycemic-hyperosmolar coma",
                            "E131"	= "Other specified DM w/ ketoacidosis",
                            "E132"	= "Other specified DM w/ kidney complications",
                            "E133"	= "Other specified DM w/ ophthalmic complications",
                            "E134"	= "Other specified DM w/ neurological complications",
                            "E135"	= "Other specified DM w/ circulatory complications",
                            "E136"	= "Other specified DM w/ other specified complications",
                            "E137"	= "Other specified DM w/ multiple diabetic complications",
                            "E139"	= "Other specified DM w/o complications",
                            "E14"	= "Diabetic complication",
                            "E140"	= "Hyperosmotic nonketotic coma",
                            "E141"	= "Diabetic ketoacidosis",
                            "E142"	= "Diabetic nephropathy",
                            "E143"	= "Diabetic ophthalmopathy",
                            "E144"	= "Diabetic neuropathy",
                            "E145"	= "Diabetic angiopathy",
                            "E146"	= "Unspecified DM w/ other specified complications",
                            "E149"	= "DM w/o diabetic complications"))+
  geom_text(aes(label = n, vjust = -1))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of patient-visits")+
  xlab("")+
  labs(title = "N. of patient-visits, obesity-related disorders")



# -----------------------------------------------------------------------------
# -------------- Patient per nyugaikbn (i.e. outpatient vs inpatient) ---------

T2_DM_nyugaikbn_all <- read.table("T2_DM_nyugaikbn_all.csv", 
                             header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# Obesity_Disease_Data_Out_vs_Inpatients

T2_DM_nyugaikbn_all %>%
  select(nyugaikbn) %>%
  mutate(nyugaikbn = as.factor(nyugaikbn)) %>%
  group_by(nyugaikbn) %>%
  summarise(n = n())  %>%
  ggplot(aes(x=nyugaikbn, y=n, fill=nyugaikbn))+
  geom_bar(position="dodge", stat="identity", alpha=0.8, show.legend = FALSE)+
  ylim(0, 110000000)+
  scale_fill_manual(values=c("firebrick", "midnightblue"))+
  scale_x_discrete(labels=c("1" = "1 - Outpatient", "2" = "2 - Inpatient"))+
  geom_text(aes(label = n, vjust = -1))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of patient-visits")+
  xlab("")+
  labs(title = "Outpatient vs Inpatient care \n (Apr 2008 - Oct 2021)")


# -----------------------------------------------------------------------------
# -------------- Hospital Visits per Month (Obesity-related patients) ---------

# calculate number of visits per month
T2_DM_visits_per_month <- read.table("T2_DM_visits_per_month.csv", 
                                  header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

T2_DM_visits_per_month <- T2_DM_visits_per_month %>%
  select(datamonth) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth) %>%
  summarise(n = n())  %>%
  arrange(datamonth)

T2_DM_visits_per_month_df <- as.data.frame(T2_DM_visits_per_month)
rm(T2_DM_visits_per_month_df)


T2_DM_visits_per_month %>% ggplot(aes(x=datamonth, y=n, color=datamonth)) +
  geom_line(color="firebrick", size = 3, alpha = 0.75)+
  scale_x_date(breaks=date_breaks("6 months"), 
               labels=date_format("%b %y")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("Number of patient-visits \n") +
  ggtitle("Number of obesity-related patient visits to the hospital / month \n (Apr 2008 - Oct 2021)")



####### Number_Visits_per_Patien
install.packages("fs")
library(fs)

# all the paths to all the files
file_paths <- fs::dir_ls()

# empty df, same columns
df <- data.frame(patientid = character(), datamonth=character(), stringsAsFactors=FALSE)

# what I wanna do for each file
# open it
Disease_Data_000000000000 <- 
  read.table("Processed_queries_T2D_sub_DiseaseData_FILTERED-000000000000.csv", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
# pick the first 2 columns
Disease_Data_000000000000 <- 
  Disease_Data_000000000000 %>% select(patientid, datamonth)

# append them to the empty df
df <- bind_rows(df, Disease_Data_000000000000)

# sequence along each file on the folder and append the 2 columns to the df, remove that file, proceed
for (i in seq_along(file_paths))  { 
  Disease_Data <- read.table(file = file_paths[[i]], header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  Disease_Data <- Disease_Data %>% select(patientid, datamonth)
  df <- bind_rows(df, Disease_Data)
  rm(Disease_Data)
}


# final data follow-up 
finaldate <- as.Date("2021-10-01")

# we have 8859852 unique patients with records
# same thing on big query, all good
# total number of visits per patient 
number_visits_per_patient <- df %>%
  select(patientid) %>%
  group_by(patientid) %>%
  summarise(n=n())

follow_up_days <- df %>%
  select(patientid, datamonth) %>%
  mutate(datamonth = as.Date(datamonth)) %>%
  group_by(patientid) %>%
  arrange(patientid, datamonth) %>%
  summarise(min = min(datamonth)) %>%
  mutate(followup = finaldate - min)

rm(df)
rm(file_paths, finaldate, i)

write.csv(follow_up_days, "T2DM_Follow_up_days_patient.csv")
rm(follow_up_days)

write.csv(number_visits_per_patient, "T2DM_N_visits_patient.csv")
rm(number_visits_per_patient)

# add number of visits per patient AND number of follow up days
visits_by_followup <- follow_up_days %>% 
  left_join(number_visits_per_patient, by= "patientid")

# convert follow up to months
# calculate visits per patient per month
visits_by_followup_normalized <- visits_by_followup %>%
  mutate(followup = (followup / 12)) %>%
  mutate(followup = as.numeric(followup)) %>%
  mutate(visitspermonth = (n / followup))

write.csv(visits_by_followup, "T2DM_N_visits_by_follow_up_Days.csv")
write.csv(visits_by_followup_normalized, "T2DM_N_visits_by_follow_up_months.csv")

---------------------------------------------------------------------------

# visits per patient, entire followup
# create skeleton plot
visits_by_followup_normalized_plot <- visits_by_followup %>%
  select(patientid, n) %>%
  mutate(typevar= typeof(patientid)) %>%
  ggplot(aes(x=typevar, y=n, color=n))

# Visits per patient, entire follow-up
visits_by_followup_normalized_plot + 
  #geom_jitter(width = 0.6, size = 2, alpha = 0.3, show.legend = F)+
  scale_y_log10()+
  ylab("Log10 N. visits")+
  geom_violin(alpha = 0.75, fill="firebrick", color="midnightblue", show.legend = F)+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("N. of visits per patient during follow-up")

mean(visits_by_followup$n)  # 11.34088
median(visits_by_followup$n) # 2

rm(visits_by_followup_normalized_plot)

-------------------------------------------------------------------------

# visits per patient, per month
# create skeleton plot
visits_by_followup_normalized_plot <- visits_by_followup_normalized %>%
  select(patientid, visitspermonth) %>%
  mutate(typevar= typeof(patientid)) %>%
  ggplot(aes(x=typevar, y=visitspermonth, color=visitspermonth))

# Visits per patient normalized to N. months of follow-up (cont.)
visits_by_followup_normalized_plot + 
  #geom_jitter(width = 0.6, size = 2, alpha = 0.3, show.legend = F)+
  scale_y_log10()+
  ylab("Log10 N. visits")+
  geom_violin(alpha = 0.75, fill="cyan4", color="cyan4", show.legend = F)+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("N. of visits per patient per month of follow-up")

median(visits_by_followup_normalized$visitspermonth) # 0.032832

rm(visits_by_followup, visits_by_followup_normalized, visits_by_followup_normalized_plot)



# AGE Group analysis Time Series-----------------------------------------------------


labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


N_patients_month_per_age <- read.table("N_patients_month_per_age.csv", 
             header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

N_patients_month_per_age$age <- as.numeric(N_patients_month_per_age$age)
N_patients_month_per_age$f0_ <- as.numeric(N_patients_month_per_age$f0_)

N_patients_month_per_age$AgeGroup <- cut(N_patients_month_per_age$age, 
                                                  breaks = c(seq(0, 100, by = 5), Inf), 
                                                  labels = labs, right = FALSE)



T2_DM_Dat <- N_patients_month_per_age %>%
  #filter(age >= 18) %>%
  select(datamonth, AgeGroup, f0_) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth, AgeGroup) %>%
  arrange(datamonth)

T2_DM_Dat <- as.data.frame(T2_DM_Dat)

# time series skeleton plot, the whole duration
T2_DM_Dat_plot1 <- T2_DM_Dat %>%
  #filter(age >= 18) %>%
  select(datamonth, AgeGroup, f0_) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth, AgeGroup) %>%
  arrange(datamonth) %>%
  summarize(n = sum(f0_)) %>%
  ggplot(aes(x=datamonth, y=n, color=AgeGroup))

# finish up the plot details
T2_DM_Dat_plot1  + geom_line( size = 2, alpha = 0.9)+
  scale_x_date(breaks=date_breaks("6 months"), 
               labels=date_format("%b %y")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("Number of patient'visits \n") +
  ggtitle("Number of T2 DM-related patient-visits to the hospital / month \n (Apr 2008 - Oct 2021)")+
  scale_color_viridis_d()



# same thing, just adults > 18 years old

T2_DM_Dat <- N_patients_month_per_age %>%
  filter(age >= 18) %>%
  select(datamonth, AgeGroup, f0_) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth, AgeGroup) %>%
  arrange(datamonth)

T2_DM_Dat <- as.data.frame(T2_DM_Dat)

# time series skeleton plot, the whole duration
T2_DM_Dat_plot1 <- T2_DM_Dat %>%
  select(datamonth, AgeGroup, f0_) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth, AgeGroup) %>%
  arrange(datamonth) %>%
  summarize(n = sum(f0_)) %>%
  ggplot(aes(x=datamonth, y=n, color=AgeGroup))

# finish up the plot details
T2_DM_Dat_plot1  + geom_line( size = 2, alpha = 0.9)+
  scale_x_date(breaks=date_breaks("6 months"), 
               labels=date_format("%b %y")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("Number of patient'visits \n") +
  ggtitle("Number of T2 DM-related patient-visits to the hospital / month \n (Apr 2008 - Oct 2021)")+
  scale_color_viridis_d()

rm(T2_DM_Dat, T2_DM_Dat_plot1, N_patients_month_per_age)


# - Number of Visits By Age group, TOTAL
T2D_patients_per_age <- read.table("T2D_patients_per_age.csv", 
                                       header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

T2D_patients_per_age$age <- as.numeric(T2D_patients_per_age$age)
T2D_patients_per_age$f0_ <- as.numeric(T2D_patients_per_age$f0_)

T2D_patients_per_age$AgeGroup <- cut(T2D_patients_per_age$age, 
                                         breaks = c(seq(0, 100, by = 5), Inf), 
                                         labels = labs, right = FALSE)


# count how many in each bucket, start the plot
T2D_patients_per_age %>%
  #filter(age >= 18) %>%
  group_by(AgeGroup) %>%
  summarise(n = sum(f0_)) %>%
  ggplot(aes(x = AgeGroup, y = n , fill = AgeGroup))+
  geom_bar(stat="identity") +
  #ylim(0,13000)+
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("Number of TOTAL patient-visits per Age Group")

rm(T2D_patients_per_age)



### ---------------- Number of Unique Patients per Age group ----- #
### ---------------- Age at the time last seen ------------------- #

T2DM_id_age_at_last_month <- read.table("T2DM_id_age_at_last_month.csv", 
                                   header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

T2DM_id_age_at_last_month$age <- as.numeric(T2DM_id_age_at_last_month$age)


T2DM_id_age_at_last_month$AgeGroup <- cut(T2DM_id_age_at_last_month$age, 
                                     breaks = c(seq(0, 100, by = 5), Inf), 
                                     labels = labs, right = FALSE)



# count how many in each bucket, start the plot
T2DM_id_age_at_last_month %>%
  #filter(age >= 18) %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n , fill = AgeGroup))+
  geom_bar(stat="identity") +
  #ylim(0,13000)+
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("Number of Unique Patient per Age Group")

rm(T2DM_id_age_at_last_month)




# ------- DRUG PENETRANCE T2 DM Year - 1------------------------- #

###############
# DULAGLUTIDE #
###############

# import all patients for that period
T2D_ID_datamonth_age_YearMinus1 <- read.table("T2D_ID_datamonth_age_YearMinus1.csv", 
                                                header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Dulaglutide_YearMinus1 <- read.table("Dulaglutide_YearMinus1.csv", 
                               header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus1$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1 %>%
  left_join(Dulaglutide_YearMinus1, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>%
  arrange(datamonth)

rm(Dulaglutide_YearMinus1, T2D_ID_datamonth_age_YearMinus1, T2D_ID_datamonth_age_YearMinus1_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))

# Add age bucket
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode)

T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Dulaglutide / Age Group (Year -1)")


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Dulaglutide / Age Group (Year -1)")






# Caluclate Percentage / Drug penetrance DULAGLUTIDE

How_many_OFF_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_1 %>% left_join(How_many_ON_Year_Minus_1, by = "AgeGroup")

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it

How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with DULAGLUTIDE Prescribed upon Hospital Visit (Year -1)")








#------------------------------------------------------------------------------------------





###################
# IPDRAGLIFLOZINE #
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus1 <- read.table("T2D_ID_datamonth_age_YearMinus1.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Ipragliflozine_YearMinus1 <- read.table("Ipragliflozine_YearMinus1.csv", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus1$patientid))


# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1 %>%
  left_join(Ipragliflozine_YearMinus1, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>% distinct()


# arrange by date
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>%
  arrange(datamonth)

rm(Ipragliflozine_YearMinus1, T2D_ID_datamonth_age_YearMinus1, T2D_ID_datamonth_age_YearMinus1_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

# drug label as factor
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Ipragliflozine / Age Group (Year -1)")


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Ipragliflozine / Age Group (Year -1)")






# Caluclate Percentage / Drug penetrance DULAGLUTIDE
How_many_OFF_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_1 %>% left_join(How_many_ON_Year_Minus_1, by = "AgeGroup")

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it

How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with IPRAGLIFLOZINE Prescribed upon Hospital Visit (Year -1)")


###################
# GLIMEPIRIDE#### #
##################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus1 <- read.table("T2D_ID_datamonth_age_YearMinus1.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Glimepiride_YearMinus1 <- read.table("Glimepiride_YearMinus1.CSV", 
                                        header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus1$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1 %>%
  left_join(Glimepiride_YearMinus1, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>%
  arrange(datamonth)

rm(Glimepiride_YearMinus1, T2D_ID_datamonth_age_YearMinus1, T2D_ID_datamonth_age_YearMinus1_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Glimepiride / Age Group (Year -1)")


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Glimepiride / Age Group (Year -1)")


# Caluclate Percentage / Drug penetrance GLIMEPIRIDE
How_many_OFF_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_1 %>% left_join(How_many_ON_Year_Minus_1, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with GLIMEPIRIDE Prescribed upon Hospital Visit (Year -1)")




###################
# PIOGLITAZONE ####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus1 <- read.table("T2D_ID_datamonth_age_YearMinus1.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Pioglitazone_YearMinus1 <- read.table("Pioglitazone_YearMinus1.CSV", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus1$patientid))


# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1 %>%
  left_join(Pioglitazone_YearMinus1, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>%
  arrange(datamonth)

rm(Pioglitazone_YearMinus1, T2D_ID_datamonth_age_YearMinus1, T2D_ID_datamonth_age_YearMinus1_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Pioglitazone / Age Group (Year -1)")


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Pioglitazone / Age Group (Year -1)")


# Caluclate Percentage / Drug penetrance PIOGLITAZONE
How_many_OFF_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_1 %>% left_join(How_many_ON_Year_Minus_1, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with PIOGLITAZONE Prescribed upon Hospital Visit (Year -1)")


###################
# SITAGLIPTIN #####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus1 <- read.table("T2D_ID_datamonth_age_YearMinus1.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Sitagliptin_YearMinus1 <- read.table("Sitagliptin_YearMinus1.CSV", 
                                      header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus1$patientid))


# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1 %>%
  left_join(Sitagliptin_YearMinus1, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


# remove duplicates
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>% distinct()


# arrange by date
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>%
  arrange(datamonth)

rm(Sitagliptin_YearMinus1, T2D_ID_datamonth_age_YearMinus1, T2D_ID_datamonth_age_YearMinus1_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth)


# age to numeric
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age)


# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

# drug label as factor
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Sitagliptin / Age Group (Year -1)")


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Sitagliptin / Age Group (Year -1)")


# Caluclate Percentage / Drug penetrance SITAGLIPTINE

How_many_OFF_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_1 %>% left_join(How_many_ON_Year_Minus_1, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with SITAGLIPTIN Prescribed upon Hospital Visit (Year -1)")


###################
# SITAGLIPTIN #####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus1 <- read.table("T2D_ID_datamonth_age_YearMinus1.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Metformin_YearMinus1 <- read.table("Metformin_YearMinus1.CSV", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus1$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1 %>%
  left_join(Metformin_YearMinus1, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


# remove duplicates
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>% distinct()


# arrange by date
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label %>%
  arrange(datamonth)

rm(Metformin_YearMinus1, T2D_ID_datamonth_age_YearMinus1, T2D_ID_datamonth_age_YearMinus1_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age)


# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))

# Add age bucket
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month$receiptcode)



T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Metformin / Age Group (Year -1)")


T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Metformin / Age Group (Year -1)")



# Caluclate Percentage / Drug penetrance METFORMIN

How_many_OFF_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_1 <- data.frame(T2D_ID_datamonth_age_YearMinus1_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_1 %>% left_join(How_many_ON_Year_Minus_1, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,16)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with METFORMIN Prescribed upon Hospital Visit (Year -1)")






##################### YEAR   - 2 #####################################################
# ------- DRUG PENETRANCE T2 DM Year  - 2  ------------------------- #

###############
# DULAGLUTIDE #
###############


# import all patients for that period
T2D_ID_datamonth_age_YearMinus2 <- read.table("T2D_ID_datamonth_age_YearMinus2.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Dulaglutide_YearMinus2 <- read.table("Dulaglutide_YearMinus2.csv", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 13,625,414 entries, 2,881,746 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus2$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2 %>%
  left_join(Dulaglutide_YearMinus2, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>% distinct()


# arrange by date
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>%
  arrange(datamonth)

rm(Dulaglutide_YearMinus2, T2D_ID_datamonth_age_YearMinus2, T2D_ID_datamonth_age_YearMinus2_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth)


# age to numeric
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age)


# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)



# drug label as factor
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode)



T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Dulaglutide / Age Group (Year -2)")


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Dulaglutide / Age Group (Year -2)")


# Caluclate Percentage / Drug penetrance DULAGLUTIDE
How_many_OFF_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_2 %>% left_join(How_many_ON_Year_Minus_2, by = "AgeGroup")

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)

# plot it

How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with DULAGLUTIDE Prescribed upon Hospital Visit (Year -2)")



###################
# IPDRAGLIFLOZINE #
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus2 <- read.table("T2D_ID_datamonth_age_YearMinus2.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Ipragliflozine_YearMinus2 <- read.table("Ipragliflozine_YearMinus2.csv", 
                                        header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus2$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2 %>%
  left_join(Ipragliflozine_YearMinus2, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>%
  arrange(datamonth)

rm(Ipragliflozine_YearMinus2, T2D_ID_datamonth_age_YearMinus2, T2D_ID_datamonth_age_YearMinus2_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)



# drug label as factor
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode)



T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Ipragliflozine / Age Group (Year -2)")


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Ipragliflozine / Age Group (Year -2)")




# Caluclate Percentage / Drug penetrance DULAGLUTIDE
How_many_OFF_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_2 %>% left_join(How_many_ON_Year_Minus_2, by = "AgeGroup")

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it

How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with IPRAGLIFLOZINE Prescribed upon Hospital Visit (Year -2)")



###################
# GLIMEPIRIDE#### #
###################


# import all patients for that period
T2D_ID_datamonth_age_YearMinus2 <- read.table("T2D_ID_datamonth_age_YearMinus2.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Glimepiride_YearMinus2 <- read.table("Glimepiride_YearMinus2.CSV", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus2$patientid))



# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2 %>%
  left_join(Glimepiride_YearMinus2, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


# remove duplicates
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>% distinct()



# arrange by date
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>%
  arrange(datamonth)


rm(Glimepiride_YearMinus2, T2D_ID_datamonth_age_YearMinus2, T2D_ID_datamonth_age_YearMinus2_with_DRUG_label)



# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth)


# age to numeric
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age)


# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

# drug label as factor
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode)



T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Glimepiride / Age Group (Year -2)")


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Glimepiride / Age Group (Year -2)")



# Caluclate Percentage / Drug penetrance GLIMEPIRIDE

How_many_OFF_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_2 %>% left_join(How_many_ON_Year_Minus_2, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with GLIMEPIRIDE Prescribed upon Hospital Visit (Year -2)")






###################
# PIOGLITAZONE ####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus2 <- read.table("T2D_ID_datamonth_age_YearMinus2.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Pioglitazone_YearMinus2 <- read.table("Pioglitazone_YearMinus2.CSV", 
                                      header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus2$patientid))


# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2 %>%
  left_join(Pioglitazone_YearMinus2, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>%
  arrange(datamonth)


rm(Pioglitazone_YearMinus2, T2D_ID_datamonth_age_YearMinus2, T2D_ID_datamonth_age_YearMinus2_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)



# drug label as factor
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Pioglitazone / Age Group (Year -2)")


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Pioglitazone / Age Group (Year -2)")


# Caluclate Percentage / Drug penetrance PIOGLITAZONE
How_many_OFF_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_2 %>% left_join(How_many_ON_Year_Minus_2, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with PIOGLITAZONE Prescribed upon Hospital Visit (Year -2)")



###################
# SITAGLIPTIN #####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus2 <- read.table("T2D_ID_datamonth_age_YearMinus2.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Sitagliptin_YearMinus2 <- read.table("Sitagliptin_YearMinus2.CSV", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

length(unique(T2D_ID_datamonth_age_YearMinus2$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2 %>%
  left_join(Sitagliptin_YearMinus2, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>%
  arrange(datamonth)

rm(Sitagliptin_YearMinus2, T2D_ID_datamonth_age_YearMinus2, T2D_ID_datamonth_age_YearMinus2_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Sitagliptin / Age Group (Year -2)")


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Sitagliptin / Age Group (Year -2)")



# Caluclate Percentage / Drug penetrance SITAGLIPTINE
How_many_OFF_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_2 %>% left_join(How_many_ON_Year_Minus_2, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with SITAGLIPTIN Prescribed upon Hospital Visit (Year -2)")


###################
# SITAGLIPTIN #####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus2 <- read.table("T2D_ID_datamonth_age_YearMinus2.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Metformin_YearMinus2 <- read.table("Metformin_YearMinus2.CSV", 
                                   header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus2$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2 %>%
  left_join(Metformin_YearMinus2, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


# remove duplicates
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label %>%
  arrange(datamonth)

rm(Metformin_YearMinus2, T2D_ID_datamonth_age_YearMinus2, T2D_ID_datamonth_age_YearMinus2_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$datamonth)


# age to numeric
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age)


# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))

# Add age bucket
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)



# drug label as factor
T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month$receiptcode)



T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Metformin / Age Group (Year -2)")


T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Metformin / Age Group (Year -2)")






# Caluclate Percentage / Drug penetrance METFORMIN
How_many_OFF_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_2 <- data.frame(T2D_ID_datamonth_age_YearMinus2_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_2 %>% left_join(How_many_ON_Year_Minus_2, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,16)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with METFORMIN Prescribed upon Hospital Visit (Year -2)")


##################### YEAR   - 3 #####################################################
# ------- DRUG PENETRANCE T2 DM Year  - 3  ------------------------- #

###############
# DULAGLUTIDE #
###############

# import all patients for that period
T2D_ID_datamonth_age_YearMinus3 <- read.table("T2D_ID_datamonth_age_YearMinus3.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Dulaglutide_YearMinus3 <- read.table("Dulaglutide_YearMinus3.csv", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,881,746 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus3$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3 %>%
  left_join(Dulaglutide_YearMinus3, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


# remove duplicates
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>% distinct()


# arrange by date
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>%
  arrange(datamonth)

rm(Dulaglutide_YearMinus3, T2D_ID_datamonth_age_YearMinus3, T2D_ID_datamonth_age_YearMinus3_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)



# drug label as factor
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Dulaglutide / Age Group (Year -3)")

T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Dulaglutide / Age Group (Year -3)")


# Caluclate Percentage / Drug penetrance DULAGLUTIDE
How_many_OFF_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_3 %>% left_join(How_many_ON_Year_Minus_3, by = "AgeGroup")

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it

How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with DULAGLUTIDE Prescribed upon Hospital Visit (Year -3)")





###################
# IPDRAGLIFLOZINE #
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus3 <- read.table("T2D_ID_datamonth_age_YearMinus3.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Ipragliflozine_YearMinus3 <- read.table("Ipragliflozine_YearMinus3.csv", 
                                        header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus3$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3 %>%
  left_join(Ipragliflozine_YearMinus3, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>% distinct()


# arrange by date
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>%
  arrange(datamonth)

rm(Ipragliflozine_YearMinus3, T2D_ID_datamonth_age_YearMinus3, T2D_ID_datamonth_age_YearMinus3_with_DRUG_label)

# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth)


# age to numeric
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age)


# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))



# Add age bucket
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Ipragliflozine / Age Group (Year -3)")


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Ipragliflozine / Age Group (Year -3)")




# Caluclate Percentage / Drug penetrance DULAGLUTIDE

How_many_OFF_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))

How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_3 %>% left_join(How_many_ON_Year_Minus_3, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it

How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with IPRAGLIFLOZINE Prescribed upon Hospital Visit (Year -3)")



###################
# GLIMEPIRIDE#### #
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus3 <- read.table("T2D_ID_datamonth_age_YearMinus3.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Glimepiride_YearMinus3 <- read.table("Glimepiride_YearMinus3.CSV", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus3$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3 %>%
  left_join(Glimepiride_YearMinus3, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>% distinct()



# arrange by date
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>%
  arrange(datamonth)

rm(Glimepiride_YearMinus3, T2D_ID_datamonth_age_YearMinus3, T2D_ID_datamonth_age_YearMinus3_with_DRUG_label)



# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))



# Add age bucket
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode)



T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Glimepiride / Age Group (Year -3)")


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Glimepiride / Age Group (Year -3)")




# Caluclate Percentage / Drug penetrance GLIMEPIRIDE

How_many_OFF_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_3 %>% left_join(How_many_ON_Year_Minus_3, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with GLIMEPIRIDE Prescribed upon Hospital Visit (Year -3)")





###################
# PIOGLITAZONE ####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus3 <- read.table("T2D_ID_datamonth_age_YearMinus3.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Pioglitazone_YearMinus3 <- read.table("Pioglitazone_YearMinus3.CSV", 
                                      header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 1
length(unique(T2D_ID_datamonth_age_YearMinus3$patientid))


# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3 %>%
  left_join(Pioglitazone_YearMinus3, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>%
  arrange(datamonth)

rm(Pioglitazone_YearMinus3, T2D_ID_datamonth_age_YearMinus3, T2D_ID_datamonth_age_YearMinus3_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth)


# age to numeric
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age)


# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))


# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)



# drug label as factor
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Pioglitazone / Age Group (Year -3)")


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Pioglitazone / Age Group (Year -3)")






# Caluclate Percentage / Drug penetrance PIOGLITAZONE
How_many_OFF_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_3 %>% left_join(How_many_ON_Year_Minus_3, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with PIOGLITAZONE Prescribed upon Hospital Visit (Year -3)")



###################
# SITAGLIPTIN #####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus3 <- read.table("T2D_ID_datamonth_age_YearMinus3.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Sitagliptin_YearMinus3 <- read.table("Sitagliptin_YearMinus3.CSV", 
                                     header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


length(unique(T2D_ID_datamonth_age_YearMinus3$patientid))



# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3 %>%
  left_join(Sitagliptin_YearMinus3, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


# remove duplicates
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>%
  arrange(datamonth)

rm(Sitagliptin_YearMinus3, T2D_ID_datamonth_age_YearMinus3, T2D_ID_datamonth_age_YearMinus3_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

# drug label as factor
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode)

T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Sitagliptin / Age Group (Year -3)")


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Sitagliptin / Age Group (Year -3)")


# Caluclate Percentage / Drug penetrance SITAGLIPTINE
How_many_OFF_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_3 %>% left_join(How_many_ON_Year_Minus_3, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)

# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with SITAGLIPTIN Prescribed upon Hospital Visit (Year -3)")



###################
# METFORMIN #####
###################

# import all patients for that period
T2D_ID_datamonth_age_YearMinus3 <- read.table("T2D_ID_datamonth_age_YearMinus3.csv", 
                                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


# table with act month, patientid and drug code
# onlyone drug, but wanna check with vs without drug after merging it
Metformin_YearMinus3 <- read.table("Metformin_YearMinus3.CSV", 
                                   header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# 13,625,414 entries, 2,903,943 unique patient
length(unique(T2D_ID_datamonth_age_YearMinus3$patientid))

# left join to add the drug tag (yes or no)
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3 %>%
  left_join(Metformin_YearMinus3, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

# remove duplicates
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>% distinct()

# arrange by date
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label %>%
  arrange(datamonth)

rm(Metformin_YearMinus3, T2D_ID_datamonth_age_YearMinus3, T2D_ID_datamonth_age_YearMinus3_with_DRUG_label)


# change drug tag to ON vs OFF it at the time the patient was seen
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  mutate(receiptcode = ifelse(is.na(receiptcode), "OFF Drug", "ON Drug"))

# datamonth to date type
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth <- as.Date(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$datamonth)

# age to numeric
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age <- as.numeric(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age)

# select maximum row after sorting them
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month <- T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

# age buckets
labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))


# Add age bucket
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$AgeGroup <- 
  cut(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)


# drug label as factor
T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode <- as.factor(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month$receiptcode)


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF Metformin / Age Group (Year -3)")


T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
  filter(receiptcode == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON Metformin / Age Group (Year -3)")


# Caluclate Percentage / Drug penetrance METFORMIN
How_many_OFF_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                          filter(receiptcode == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_3 <- data.frame(T2D_ID_datamonth_age_YearMinus3_with_DRUG_label_by_month %>%
                                         filter(receiptcode == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))

How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_3 %>% left_join(How_many_ON_Year_Minus_3, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)

# plot it
How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,16)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with METFORMIN Prescribed upon Hospital Visit (Year -3)")


