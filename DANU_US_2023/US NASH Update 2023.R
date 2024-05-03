# NASH
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)


# Population ------------------------
NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
unique(NASH_Demographics$diagnosis)

sum(NASH_Demographics$weight) # 1384888
sum(NASH_Demographics$weight[is.na(NASH_Demographics$fibrosis)&is.na(NASH_Demographics$cirrhosis)]) # 
sum(NASH_Demographics$weight[!is.na(NASH_Demographics$fibrosis)&is.na(NASH_Demographics$cirrhosis)]) # 
sum(NASH_Demographics$weight[!is.na(NASH_Demographics$cirrhosis)]) #  


NAFLD_Demographics <- fread("NAFLD Analysis Results 1.1/NAFLD Demographics.txt")
sum(NAFLD_Demographics$weight) # 



NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis",
                                          ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
  select(patid, weight, age, gender, group)  %>% 
  bind_rows(
NAFLD_Demographics %>% select(patid, weight, age, gender) %>% mutate(group="NAFLD")
) %>%
  group_by(group, gender) %>% summarise(n=weighted.mean(age, weight)) %>%
  spread(key=gender, value=n)




NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis",
                                          ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
  select(patid, weight, age, gender, group)  %>% 
  bind_rows(
NAFLD_Demographics %>% select(patid, weight, age, gender) %>% mutate(group="NAFLD")
) %>%
  group_by(group, gender) %>% summarise(n=sum(weight)) %>%
  spread(key=gender, value=n) %>%
  mutate(percF=F/(F+M))



NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis",
                                          ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
  select(patid, weight, age, gender, group)  %>% 
  bind_rows(
NAFLD_Demographics %>% select(patid, weight, age, gender) %>% mutate(group="NAFLD")
) %>% 
  mutate(group=factor(group, levels=c("NAFLD", "NASH-only", "Fibrosis", "Cirrhosis"))) %>%
  ggplot(aes(age, colour=gender, fill=gender)) +
  geom_histogram(position="stack", alpha=0.5) +
  facet_wrap(~group, scales = "free") +
  theme_minimal() +
  ggsci::scale_fill_nejm() +
  ggsci::scale_color_nejm() +
  ylab("No. patient samples \n") + xlab("\n Age (years) group")



NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis",
                                          ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
    select(patid, weight, age, gender, group, diabetes_onset, obesity_onset)  %>% 
  mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset=ifelse(is.na(obesity_onset),0,1)) %>% 
  mutate(OBE=ifelse(diabetes_onset==1&obesity_onset==1, "DIA+OBE", 
                    ifelse(obesity_onset==1, "OBE", NA))) %>%
  group_by(group, OBE) %>% summarise(n=sum(weight))



NAFLD_Demographics %>% 
    select(patid, weight, diabetes_onset, obesity_onset)  %>% 
  mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset=ifelse(is.na(obesity_onset),0,1)) %>% 
  mutate(OBE=ifelse(diabetes_onset==1&obesity_onset==1, "DIA+OBE", 
                    ifelse(obesity_onset==1, "OBE", NA))) %>%
  group_by(OBE) %>% summarise(n=sum(weight))

# -------------------------------
# Treatment-experienced ----------------

NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

sum(NASH_Drug_Histories$weight) # 

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(4:63)
NASH_Drug_Histories[NASH_Drug_Histories != "-"] <- 1  # on drug 
NASH_Drug_Histories[NASH_Drug_Histories == "-"] <- 0  # no drug
NASH_Drug_Histories[] <- lapply(NASH_Drug_Histories,as.numeric)
NASH_Drug_Histories$SUM <- rowSums(NASH_Drug_Histories)

NASH_Drug_Histories_LONG <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Pats_vec <- NASH_Drug_Histories_LONG %>% select(patient, weight)
NASH_Drug_Histories <- Pats_vec %>% bind_cols(NASH_Drug_Histories)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(SUM != 0)

sum(NASH_Drug_Histories$weight) # 

NASH_Treatment_exp_Vector <- NASH_Drug_Histories %>% select(patient, weight)
fwrite(NASH_Treatment_exp_Vector, "NASH Analysis Results 1.1/NASH_Treatment_exp_Vector.txt")



NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

sum(NAFLD_Drug_Histories$weight) # 

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(4:63)
NAFLD_Drug_Histories[NAFLD_Drug_Histories != "-"] <- 1  # on drug 
NAFLD_Drug_Histories[NAFLD_Drug_Histories == "-"] <- 0  # no drug
NAFLD_Drug_Histories[] <- lapply(NAFLD_Drug_Histories,as.numeric)
NAFLD_Drug_Histories$SUM <- rowSums(NAFLD_Drug_Histories)

NAFLD_Drug_Histories_LONG <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Pats_vec <- NAFLD_Drug_Histories_LONG %>% select(patient, weight)
NAFLD_Drug_Histories <- Pats_vec %>% bind_cols(NAFLD_Drug_Histories)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(SUM != 0)

sum(NAFLD_Drug_Histories$weight) # 

NAFLD_Treatment_exp_Vector <- NAFLD_Drug_Histories %>% select(patient, weight)
fwrite(NAFLD_Treatment_exp_Vector, "NAFLD Analysis Results 1.1/NAFLD_Treatment_exp_Vector.txt")

# ----------------------------------

# Stock month 60 ------------------------------

NASH_Treatment_exp_Vector <- fread("NASH Analysis Results 1.1/NASH_Treatment_exp_Vector.txt")
sum(NASH_Treatment_exp_Vector$weight) # 1044641
NASH_Box_Histories     <- fread("NASH Analysis Results 1.1/NASH Box Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Box_Histories <- NASH_Treatment_exp_Vector %>% left_join(NASH_Box_Histories)
NASH_Box_Histories <- NASH_Box_Histories %>% select(patient, weight, month60)
NASH_Box_Histories <- NASH_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

NASH_Box_Histories %>% group_by(month60) %>% summarise(n=sum(weight))


NAFLD_Treatment_exp_Vector <- fread("NAFLD Analysis Results 1.1/NAFLD_Treatment_exp_Vector.txt")
sum(NAFLD_Treatment_exp_Vector$weight) # 9448652
NAFLD_Box_Histories     <- fread("NAFLD Analysis Results 1.1/NAFLD Box Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Box_Histories <- NAFLD_Treatment_exp_Vector %>% left_join(NAFLD_Box_Histories)
NAFLD_Box_Histories <- NAFLD_Box_Histories %>% select(patient, weight, month60)
NAFLD_Box_Histories <- NAFLD_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

NAFLD_Box_Histories %>% group_by(month60) %>% summarise(n=sum(weight))


# --------------------
# Stocks month-over-month --------------

NASH_Treatment_exp_Vector <- fread("NASH Analysis Results 1.1/NASH_Treatment_exp_Vector.txt")
NASH_Drug_Histories     <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Treatment_exp_Vector %>% left_join(NASH_Drug_Histories)

length(unique(NASH_Drug_Histories$patient)) # 
sum(as.numeric(NASH_Drug_Histories$weight)) # 

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% select(molecule, drug_group)
names(NASH_Ingredients)[1] <- "Drugs"

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, Month, Drugs) %>% 
  distinct() %>% filter(Drugs!="-") %>% left_join(NASH_Ingredients) 

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct() 

data.frame(NASH_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  group_by(Month, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  spread(key=Month, value=n)) %>%
  transpose()










NAFLD_Treatment_exp_Vector <- fread("NAFLD Analysis Results 1.1/NAFLD_Treatment_exp_Vector.txt")
NAFLD_Drug_Histories     <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Treatment_exp_Vector %>% left_join(NAFLD_Drug_Histories)

length(unique(NAFLD_Drug_Histories$patient)) # 
sum(as.numeric(NAFLD_Drug_Histories$weight)) # 

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% select(molecule, drug_group)
names(NASH_Ingredients)[1] <- "Drugs"

NAFLD_Drug_Histories <- gather(NAFLD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NAFLD_Drug_Histories <- separate_rows(NAFLD_Drug_Histories, Drugs, sep = ",", convert=T)

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient, weight, Month, Drugs) %>% 
  distinct() %>% filter(Drugs!="-") %>% left_join(NASH_Ingredients) 

NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct() 

data.frame(NAFLD_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  group_by(Month, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  spread(key=Month, value=n)) %>%
  transpose()



# --------------------------------
# Predict NASH in the other populations -------------------------------------

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
length(unique(DANU_Measures$patid))
unique(DANU_Measures$test)

DANU_Measures <- DANU_Measures %>% select(patid, weight, test, claimed, value) %>% distinct()
DANU_Measures <- DANU_Measures %>% filter(test=="ALT Level"|test=="AST Level"|test=="Platelet Count")



NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, weight) %>% mutate(group="NASH")
NAFLD_Demographics <- fread("NAFLD Analysis Results 1.1/NAFLD Demographics.txt")
NAFLD_Demographics <- NAFLD_Demographics %>% select(patid, weight) %>% mutate(group="NAFLD")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis!="-")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% rename("group"="diagnosis")

DANU_Demographics <- DANU_Demographics %>% anti_join(NASH_Demographics %>% select(patid)) %>% anti_join(NAFLD_Demographics %>% select(patid)) 

groups <- DANU_Demographics %>% bind_rows(NASH_Demographics) %>% bind_rows(NAFLD_Demographics) 


groups <- groups %>% inner_join(DANU_Measures)

groups %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))


groups %>% group_by(group, test) %>% summarise(n=weighted.mean(value, weight)) %>%
  spread(key=test, value=n)

library(ggridges)


groups %>% filter(test=="AST Level"&value<=100) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n AST Level (IU)") + ylab("Disease Group \n")


groups %>% filter(test=="ALT Level"&value<=100) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n ALT Level (IU)") + ylab("Disease Group \n")


groups %>% filter(test=="Platelet Count"&value<=500) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Platelet Count") + ylab("Disease Group \n")


groups <- groups %>% group_by(patid, test) %>% mutate(value=ifelse(test=="Platelet Count", min(value), max(value))) %>% slice(1)

groups %>% group_by(group, test) %>% summarise(n=weighted.mean(value, weight)) %>%
  spread(key=test, value=n)




groups %>% filter(test=="AST Level"&value<=100) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n AST Level (IU)") + ylab("Disease Group \n")


groups %>% filter(test=="ALT Level"&value<=100) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n ALT Level (IU)") + ylab("Disease Group \n")


groups %>% filter(test=="Platelet Count"&value<=500) %>%
  ggplot(aes(x = value, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Platelet Count") + ylab("Disease Group \n")


groups <- groups %>% select(-claimed)
groups <- groups %>% spread(key=test, value=value) %>% drop_na()


Negative <- groups %>%  filter(`ALT Level`<20 & `AST Level`<20 & `Platelet Count`>300 & group!="NASH" & group != "NAFLD") %>% 
  ungroup() %>% select(-c(patid, weight, group)) 


Negative <- Negative[1:500,]

names(Negative) <- c("ALT", "AST", "Platelets")

Negative$group <- 0

Positive <- groups %>%  filter(`ALT Level`>50 & `AST Level`>50 & `Platelet Count`<150 & group=="NASH") %>% 
  ungroup() %>% select(-c(patid, weight, group)) 

Positive <- Positive[1:500,]
names(Positive) <- c("ALT", "AST", "Platelets")

Positive$group <- 1

to_train <- Negative %>% bind_rows(Positive)
to_train %>% group_by(group) %>% count()

to_train <- to_train %>% sample_n(1000)

groups %>% group_by(group) %>% summarise(n=sum(weight))


groups2 <- groups %>% ungroup() %>% select(-c(patid,weight)) 
groups2 <- groups2 %>% select(-group)


names(groups2) <- c("ALT", "AST", "Platelets")




normalize <- function(x) {
 return ((x - min(x)) / (max(x) - min(x)))
}


maxmindf <- as.data.frame(lapply(to_train, normalize))



#trainset <- maxmindf[1:300, ]
#testset <- maxmindf[300:1000, ]



library(neuralnet)


nn <- neuralnet(group ~ AST + ALT + Platelets, data=maxmindf, hidden=c(5,2), 
                rep = 10, linear.output=TRUE, threshold=0.01)

nn$result.matrix

plot(nn)

nn.results <- compute(nn, maxmindf)

results <- data.frame(actual = maxmindf$group, prediction = nn.results$net.result)
results


mean( (results$actual-results$prediction)**2)


nn.results_all <- compute(nn, groups2)

groups2$group <- "Unk"

results_all <- data.frame(actual = groups2$group, prediction = nn.results_all$net.result)
results_all

ignore <- (groups  %>% bind_cols(results_all))
ignore <- ignore %>% select(-actual)
ignore <- gather(ignore, Label, score, `ALT Level`:`Platelet Count`, factor_key=TRUE)

ignore %>%
  filter( (Label=="ALT Level"&score<200) | (Label=="AST Level"&score<200) | (Label=="Platelet Count"&score<1000)) %>%
  ggplot(aes(score, prediction, colour=Label, fill=Label)) +
  geom_smooth() +
  facet_wrap(~Label,  scales = "free") +
  theme_minimal() +
  xlab("\n Original Lab Test Result") + ylab("Predicted NASH Score \n") +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama()

ignore %>% group_by(group) %>% summarise(n=mean(prediction))


ignore %>% mutate(prediction=ifelse(prediction>0.9,1,0)) %>%
  group_by(group, prediction, Label) %>% summarise(n=mean(score)) %>%
  spread(key=Label, value=n)


ignore %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(total=sum(weight)) %>%
  left_join(
    ignore %>% filter( (prediction>0.56&group=="Diabetes")|
                         (prediction>0.627&group=="Diabetes + Obesity")|
                         (prediction>0.8&group=="Obesity")|
                         (group=="NASH")|
                         (prediction>0.9&group=="NAFLD")) %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(select=sum(weight))
  ) %>%
  mutate(perc=select/total)


High_Risk_pred_temporary <- ignore %>% filter( (prediction>0.56&group=="Diabetes")|
                         (prediction>0.627&group=="Diabetes + Obesity")|
                         (prediction>0.8&group=="Obesity")|
                         (group=="NASH")|
                         (prediction>0.9&group=="NAFLD")) %>% select(patid, weight, group) %>% distinct()

fwrite(High_Risk_pred_temporary, "NASH Analysis Results 1.1/High_Risk_pred_temporary.txt")

# ----------------------------------------------


# Drug usage 12 months before and after 1st NASH Dx -------------------------------
NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, weight, nash)
min(NASH_Demographics$nash)
NASH_Demographics <- NASH_Demographics %>% mutate(nash=as.character(nash))
NASH_Demographics <- NASH_Demographics %>% mutate(nash=str_sub(nash, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

NASH_Demographics <- NASH_Demographics %>% left_join(Months_lookup, by=c("nash"="Month")) %>% 
  select(patid, Exact_Month) %>% distinct()
names(NASH_Demographics)[1] <- "patient"
NASH_Demographics <- NASH_Demographics %>% drop_na()

NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Demographics <- NASH_Drug_Histories %>% select(patient) %>% inner_join(NASH_Demographics)

NASH_Demographics %>%
  ggplot(aes(Exact_Month)) +
  geom_density(fill="darkslategray4", colour="darkslategray", size=1, alpha=0.5) +
  theme_minimal() +
  xlab("\n Month of 1st NASH  Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

names(NASH_Demographics)[2] <- "First_NASH_Dx"

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
#NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-")
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)



NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)

NASH_Ingredients$molecule <- as.character(NASH_Ingredients$molecule)
names(NASH_Ingredients)[1] <- "Drugs"

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 
NASH_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 1384888

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Demographics) %>% 
  mutate(Lapsed=Month-First_NASH_Dx) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(NASH_Drug_Histories)

NASH_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 670728

NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_group=ifelse(is.na(drug_group),"Lapsed",drug_group)) %>%
  rename("Drug Group"="drug_group") %>%
  mutate(n=n/670728) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=.6) +
  theme_minimal() +
  ggsci::scale_color_jco() +
  ylim(0,65) +
  xlab("\n No. Elapsed Months \n(Before/After 1st NASH Dx)") +
  ylab("Population % \n")


NASH_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()  %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))



NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_group=ifelse(is.na(drug_group),"Lapsed",drug_group)) %>%
  filter(drug_group!="Lapsed") %>%
    mutate(n=ifelse(drug_group=="Anticholesterol", n/308106,
                    ifelse(drug_group=="Antidiabetic", n/254444,
                           ifelse(drug_group=="Antiobesity",n/42817,
                                  ifelse(drug_group=="GLP1 Injectable",n/68360,
                                         ifelse(drug_group=="GLP1 Oral", n/3193,
                                                ifelse(drug_group=="Hepatoprotective", n/16283,
                                                       ifelse(drug_group=="Hospitalization", n/70787, NA)))))))) %>%
  rename("Drug Group"="drug_group") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=0.6) +
  theme_minimal() +
  ggsci::scale_color_jco() +
  ylim(0,75) +
  xlab("\n No. Elapsed Months \n(Before/After 1st NASH Dx)") +
  ylab("Population % (of Class-experienced) \n")



# -----------------------------------
# Drug Usage NASH-only vs Fibrosis vs Cirrhosis -----------------------------------

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
unique(NASH_Demographics$diagnosis)

sum(NASH_Demographics$weight) # 1384888
sum(NASH_Demographics$weight[is.na(NASH_Demographics$fibrosis)&is.na(NASH_Demographics$cirrhosis)]) # 959920.9
sum(NASH_Demographics$weight[!is.na(NASH_Demographics$fibrosis)&is.na(NASH_Demographics$cirrhosis)]) # 106936.7
sum(NASH_Demographics$weight[!is.na(NASH_Demographics$cirrhosis)]) 

NASH_Demographics <- NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis", ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
  select(patid, weight, group)
names(NASH_Demographics)[1] <- "patient"
NASH_Demographics %>% group_by(group) %>% summarise(n=sum(weight))



NASH_Treatment_exp_Vector <- fread("NASH Analysis Results 1.1/NASH_Treatment_exp_Vector.txt")
NASH_Drug_Histories     <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Treatment_exp_Vector %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <- NASH_Demographics %>% inner_join(NASH_Drug_Histories)

length(unique(NASH_Drug_Histories$patient)) # 
sum(as.numeric(NASH_Drug_Histories$weight)) # 

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% select(molecule, drug_group)
names(NASH_Ingredients)[1] <- "Drugs"
NASH_Ingredients$Drugs <- as.numeric(NASH_Ingredients$Drugs)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-Month) %>% filter(Drugs != "-") %>% distinct()
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-disease)
NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, group, drug_group) %>% distinct() 


data.frame(NASH_Drug_Histories %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(total=sum(weight)) %>%
  left_join(NASH_Drug_Histories %>% select(patient, weight, group, drug_group) %>% distinct()  %>% group_by(group, drug_group) %>% 
  summarise(n=sum(weight)))) %>% mutate(perc=n/total) %>%
  select(-c(total, n)) %>%
  spread(key=group, value=perc)

# -------------------------------------------------
# Drug usage NASH vs DIA/OBE ------------------------------

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, weight, diabetes_onset, obesity_onset)

NASH_Demographics <- NASH_Demographics %>% mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset=ifelse(is.na(obesity_onset),0,1)) %>% 
  mutate(group=ifelse(diabetes_onset==1&obesity_onset==1, "DIA+OBE", 
                    ifelse(diabetes_onset==1, "DIA", 
                           ifelse(obesity_onset==1, "OBE", NA)))) 



NASH_Demographics %>% group_by(group) %>% summarise(n=sum(weight))
names(NASH_Demographics)[1] <- "patient"





NASH_Treatment_exp_Vector <- fread("NASH Analysis Results 1.1/NASH_Treatment_exp_Vector.txt")
NASH_Drug_Histories     <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Treatment_exp_Vector %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <- NASH_Demographics %>% inner_join(NASH_Drug_Histories)

length(unique(NASH_Drug_Histories$patient)) # 
sum(as.numeric(NASH_Drug_Histories$weight)) # 

#


NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% filter(drug_group=="Antidiabetic" | grepl("GLP", drug_group))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_class)
names(NASH_Ingredients)[1] <- "Drugs"
NASH_Ingredients$Drugs <- as.numeric(NASH_Ingredients$Drugs)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-Month) %>% filter(Drugs != "-") %>% distinct()
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-disease)
NASH_Drug_Histories <- NASH_Drug_Histories %>% inner_join(NASH_Ingredients) 
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() 
NASH_Drug_Histories <- NASH_Drug_Histories %>% mutate(group=ifelse(grepl("DIA", group), "DIA", group))
NASH_Drug_Histories <- NASH_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="AGI"|drug_class=="Glitazone"|drug_class=="Antidiabetic"|drug_class=="Sulfonylurea"|drug_class=="Glinide", "Antidiabetic", drug_class))

NASH_Drug_Histories %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(total=sum(weight))


unique(NASH_Drug_Histories$drug_class)




data.frame(NASH_Drug_Histories %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(total=sum(weight)) %>%
  left_join(NASH_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct()  %>% group_by(group, drug_class) %>% 
  summarise(n=sum(weight)))) %>% mutate(perc=n/total) %>%
  select(-c(total, n)) %>%
  spread(key=group, value=perc)

# ------------------------------

# NASH stages vs DIA + OBE --------------------------

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")

NAFLD_Demographics <- fread("NAFLD Analysis Results 1.1/NAFLD Demographics.txt")


temp <- NASH_Demographics %>% mutate(disease=ifelse(!is.na(cirrhosis), "Cirrhosis",
                                          ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
  select(patid, weight,disease)  %>% 
  bind_rows(
NAFLD_Demographics %>% select(patid, weight) %>% mutate(disease="NAFLD")
) 




temp2 <- NASH_Demographics %>% 
  mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset=ifelse(is.na(obesity_onset),0,1)) %>% 
  mutate(comorb=ifelse(diabetes_onset==1&obesity_onset==1, "DIA+OBE", 
                    ifelse(diabetes_onset==1, "DIA", 
                           ifelse(obesity_onset==1, "OBE", NA)))) %>%
  select(patid, weight, comorb)  %>% 
  bind_rows(
NAFLD_Demographics %>% 
  mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset=ifelse(is.na(obesity_onset),0,1)) %>% 
  mutate(comorb=ifelse(diabetes_onset==1&obesity_onset==1, "DIA+OBE", 
                    ifelse(diabetes_onset==1, "DIA", 
                           ifelse(obesity_onset==1, "OBE", NA)))) %>%
  select(patid, weight, comorb)
) 


temp %>% left_join(temp2) %>% group_by(disease, comorb) %>% summarise(n=sum(weight)) %>%
  spread(key=comorb, value=n)

# ---------------------
# Apply the "nn" model from before to a sample of random patients ----------------------------

ce18_NASH_labs <- fread("ce18_NASH_labs.txt")

ce18_NASH_labs <- ce18_NASH_labs %>% select(-c(LOINC_CD, LOW_NRML, TST_NBR))

unique(ce18_NASH_labs$TST_DESC)

ce18_NASH_labs <- ce18_NASH_labs %>% filter(TST_DESC=="ALT (SGPT)"|TST_DESC=="AST (SGOT)"|TST_DESC=="AST"|TST_DESC=="ALT"|TST_DESC=="PLATELETS"|TST_DESC=="ALANINE AMINOTRANSFERASE"|
                                              TST_DESC=="ASPARTATE AMINOTRANSFERASE"|TST_DESC=="ALT/SGPT"|TST_DESC=="AST/SGOT"|TST_DESC=="ALANINE TRANSAMINASE"|TST_DESC=="ASPARTATE TRANSAMINASE"|TST_DESC==""|
                                              TST_DESC=="SGPT (ALT)"|TST_DESC=="SGOT (AST)"|TST_DESC=="CBC, PLATELET, NO DIFFERENTIAL"|TST_DESC=="PLATELET"|TST_DESC=="SGPT"|
                                              TST_DESC=="SGOT"|TST_DESC=="PLATELET COMMENT"|TST_DESC=="ALT(GPT)"|TST_DESC=="AST(GOT)"|TST_DESC=="Platelet Ct"|TST_DESC=="ALT(SGPT)"|TST_DESC=="ALT (SGPT) P5P"|TST_DESC=="AST (SGOT) P5P"|
                                              TST_DESC=="AST (SGOT) P5P"|TST_DESC=="ALT-SGPT"|TST_DESC=="PLT CT"|TST_DESC=="PLATELET ESTIMATION"|TST_DESC=="ALT-O"|
                                              TST_DESC=="AST-O"|TST_DESC=="AST(SGOT)"|TST_DESC=="PLATELET COUNT&PLATELET COUNT")

data.frame(ce18_NASH_labs %>% group_by(TST_DESC) %>% count() %>% arrange(n))
ce18_NASH_labs <- ce18_NASH_labs %>% filter(TST_DESC!="")
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("PLATELET", TST_DESC), "PLATELET", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("Platelet ", TST_DESC)|grepl("PLT ", TST_DESC), "PLATELET", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("AST", TST_DESC), "AST", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("ALT", TST_DESC), "ALT", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("SGOT", TST_DESC), "AST", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("SGPT", TST_DESC), "ALT", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("ASPARTATE TRANSAMINAS", TST_DESC), "AST", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("ALANINE TRANSAMINASE", TST_DESC), "ALT", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("ASPARTATE AMINOTRANSFERASE", TST_DESC), "AST", TST_DESC))
ce18_NASH_labs <- ce18_NASH_labs %>% mutate(TST_DESC=ifelse(grepl("ALANINE AMINOTRANSFERASE", TST_DESC), "ALT", TST_DESC))

ce18_NASH_labs %>% select(TST_DESC, RSLT_UNIT_NM) %>% distinct() %>% arrange(TST_DESC, RSLT_UNIT_NM)

ce18_NASH_labs <- ce18_NASH_labs %>% select(-c(RSLT_TXT, RSLT_UNIT_NM, FST_DT)) %>% mutate(RSLT_NBR=ifelse(RSLT_NBR > 1000, RSLT_NBR /1000, RSLT_NBR)) %>% distinct()

ce18_NASH_labs <- ce18_NASH_labs %>% group_by(PTID, TST_DESC) %>% mutate(RSLT_NBR =ifelse(TST_DESC=="PLATELET", min(RSLT_NBR ), max(RSLT_NBR ))) %>% slice(1)
ce18_NASH_labs <- ce18_NASH_labs %>% spread(key=TST_DESC, value=RSLT_NBR)
ce18_NASH_labs <- ce18_NASH_labs %>% drop_na()

length(unique(ce18_NASH_labs$PTID)) # 678
cetopredict <- ce18_NASH_labs %>% select(ALT, AST, PLATELET) %>% ungroup() %>% rename("Platelets"="PLATELET")



ce_nn.results_all <- compute(nn, cetopredict)

cetopredict$group <- "Unk"

results_all <- data.frame(actual = cetopredict$group, prediction = ce_nn.results_all$net.result)
results_all

ignore <- (ce18_NASH_labs  %>% bind_cols(results_all))
ignore <- ignore %>% select(-actual)
ignore <- gather(ignore, Label, score, ALT:PLATELET, factor_key=TRUE)

ignore %>%
  filter( (Label=="ALT"&score<200) | (Label=="AST"&score<200) | (Label=="PLATELET"&score<1000)) %>%
  ggplot(aes(score, prediction, colour=Label, fill=Label)) +
  geom_smooth() +
  facet_wrap(~Label,  scales = "free") +
  theme_minimal() +
  xlab("\n Original Lab Test Result") + ylab("Predicted NASH Score \n") +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama()

mean(ignore$prediction)

ignore %>% mutate(prediction=ifelse(prediction>0.,1,0)) %>%
  group_by(prediction, Label) %>% summarise(n=mean(score)) %>%
  spread(key=Label, value=n)
 


ignore %>% filter(prediction>9) %>% select(PTID) %>% distinct() # 52/678 0.07669617

# -------------------------------------------
# Specialties associated with scripts ----------------------------------

NASH_Specialty_codes <- fread("NASH Analysis Results 1.1/NASH Specialty Codes.txt", colClasses = "character",)
NASH_Specialty_codes <- NASH_Specialty_codes %>% select(code, specialty)
names(NASH_Specialty_codes)[2] <- "SPECIALTY"

NASH_Doses <- fread("NASH Analysis Results 1.1/NASH Doses.txt", colClasses = "character",)
NASH_Doses <- NASH_Doses %>% filter(status != "G" & paid=="P")
NASH_Doses <- NASH_Doses %>% select(-c(drug_id, dayssup, taxonomy1, taxonomy2, status))
NASH_Doses <- NASH_Doses %>% mutate(from_dt = as.Date(from_dt))
NASH_Doses <- NASH_Doses %>%filter(from_dt >= "2017-07-01" & from_dt <= "2022-06-30") 



temp1 <- data.frame(NASH_Doses %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n2=sum(as.numeric(n))) %>% arrange(-n2) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown"  & SPECIALTY != "Other Provider")



temp2 <- data.frame(NASH_Doses %>% filter(drug_group == "Anticholesterol") %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n3=sum(as.numeric(n))) %>% arrange(-n3) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")



temp3 <- data.frame(NASH_Doses %>% filter(drug_group == "Antiobesity") %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n4=sum(as.numeric(n))) %>% arrange(-n4) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")




temp4 <- data.frame(NASH_Doses %>% filter(drug_group == "Hepatoprotective") %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n5=sum(as.numeric(n))) %>% arrange(-n5) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")



temp5 <- data.frame(NASH_Doses %>% filter(drug_group == "Antidiabetic") %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n6=sum(as.numeric(n))) %>% arrange(-n6) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")


temp6 <- data.frame(NASH_Doses %>% filter(drug_group == "GLP1 Oral") %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n7=sum(as.numeric(n))) %>% arrange(-n7) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")


temp7 <- data.frame(NASH_Doses %>% filter(drug_group == "GLP1 Injectable") %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n8=sum(as.numeric(n))) %>% arrange(-n8) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")


temp8 <- data.frame(NASH_Doses %>% filter(drug_group == "Hospitalization") %>% group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n9=sum(as.numeric(n))) %>% arrange(-n9) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")


temp1 %>% left_join(temp2) %>% left_join(temp3) %>% left_join(temp4) %>% left_join(temp5) %>% left_join(temp6) %>% left_join(temp7) %>% left_join(temp8)

# First

 data.frame(NASH_Doses %>%  filter(drug_group == "GLP1 Oral") %>% 
              arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup() %>%
             group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n7=sum(as.numeric(n))) %>% arrange(-n7) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")

 


 
 data.frame(NASH_Doses %>%  filter(drug_group == "GLP1 Injectable") %>%
              arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup() %>%
              group_by(specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
             left_join(NASH_Specialty_codes, by=c("specialty"="code"))) %>%
             ungroup() %>% group_by(SPECIALTY) %>% summarise(n7=sum(as.numeric(n))) %>% arrange(-n7) %>% filter(SPECIALTY != "Facility" & SPECIALTY != "Unknown" & SPECIALTY != "Other Provider")


 
# -----------------------------------
# Dx per Specialty NASH --------------


NASH_Events <- fread("NASH Analysis Results 1.1/NASH Events.txt")

NASH_Event_Claims_Providers <- fread("NASH Analysis Results 1.1/NASH Event Claims Providers.txt")
NASH_Event_Claims_Providers <- NASH_Event_Claims_Providers %>% select(prov, specialty)

data.frame(NASH_Events %>% left_join(NASH_Event_Claims_Providers) %>% ungroup() %>%
                     group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n)) %>%
  mutate(specialty = str_replace_all(specialty, " ", "_"))


NASH_Diagnosis_Codes <- fread("NASH Analysis Results 1.1/NASH Diagnosis Codes.txt")


data.frame(NASH_Events %>% inner_join(NASH_Diagnosis_Codes %>% filter(grepl("K75", encoding)) %>% select(code)) %>% 
  group_by(patid) %>% filter(claimed==min(claimed)) %>% slice(1) %>% ungroup() %>%
  left_join(NASH_Event_Claims_Providers) %>%
                     group_by(specialty) %>% summarise(n=sum(weight)) %>% arrange(-n) %>%
  mutate(specialty = str_replace_all(specialty, " ", "_")))


# -----------------

# Comorbidity Inventories  -----------------------------------

NASH_Comorbidity_Inventories <- fread("NASH Analysis Results 1.1/NASH Comorbidity Inventories.txt")



NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")

NASH_Demographics <- NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis",
                                          ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
  select(patid, weight, group)

NASH_Demographics %>% group_by(group) %>% summarise(n=sum(weight))

data.frame(
  NASH_Comorbidity_Inventories %>% inner_join(NASH_Demographics) %>%
  group_by(group, diagnosis) %>% summarise(n=sum(weight)) %>% arrange(-n) %>%
  spread(key=group, value=n) %>% arrange(-`NASH-only`) %>%
  mutate(Cirrhosis=Cirrhosis/318030., Fibrosis=Fibrosis/106937., `NASH-only`=`NASH-only`/959921.) %>%
  filter(grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|grepl("H", diagnosis)|
                                                 grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|grepl("M", diagnosis)|
                                                 grepl("N", diagnosis))
)

# -----------------------------------
# Time from nash to fibrosis to cirrhosis ----------------------------

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")



NASH_Demographics %>% select(patid, weight, nash, fibrosis, cirrhosis) %>% drop_na() %>%
  mutate(nash=as.Date(nash), fibrosis=as.Date(fibrosis), cirrhosis=as.Date(cirrhosis)) %>%
  filter(cirrhosis>fibrosis & fibrosis>nash) %>%
 mutate(nash_to_fib=fibrosis-nash) %>% # 
  ggplot(aes(nash_to_fib/30.5)) +
  geom_density(size=2) +
  theme_minimal() +
  xlab("\n No. Elapsed Months from observable NASH Dx to Fibrosis Dx") + ylab("Patient density \n")


NASH_Demographics %>% select(patid, weight, nash, fibrosis, cirrhosis) %>% drop_na() %>%
  mutate(nash=as.Date(nash), fibrosis=as.Date(fibrosis), cirrhosis=as.Date(cirrhosis)) %>%
    filter(cirrhosis>fibrosis & fibrosis>nash) %>%
  mutate(fib_to_cirr=cirrhosis-fibrosis) %>% # 374.7049
 ggplot(aes(fib_to_cirr/30.5)) +
  geom_density(size=2) +
  theme_minimal() +
  xlab("\n No. Elapsed Months from observable Fibrosis Dx to Cirrhosis Dx") + ylab("Patient density \n")

# ------------------------------

# NASH Excel to fill -----------------------------------------------------




High_Risk_pred_temporary <- fread("NASH Analysis Results 1.1/High_Risk_pred_temporary.txt")
High_Risk_pred_temporary %>% group_by(group) %>% summarise(n=sum(weight))
names(High_Risk_pred_temporary)[1] <- "patient"


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% select(-weight) %>%
  inner_join(High_Risk_pred_temporary, by=c("patid"="patient")) %>% 
  filter(test=="BMI") %>% select(patid, value, weight, group) %>% distinct() %>% group_by(patid ) %>%  filter(value==max(value)) %>% slice(1) %>% ungroup()

DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(value<=27, "<=27",
                                      ifelse(value<=30,"<=30",
                                             ifelse(value<=35,"<=35", ">35"))))

# DANU_Measures <- DANU_Measures %>% group_by(group, value) %>% summarise(n=sum(weight)) %>%
#   spread(key=value, value=n) %>%
#   mutate(total=`<=27`+`<=30`+`<=35`+`>35`)

DANU_Measures <- DANU_Measures %>% mutate(value=ifelse(group=="Diabetes"&value!="<=27", NA, value)) %>%
  mutate(value=ifelse(group=="Obesity"&value=="<=27", NA, value)) %>%
  mutate(value=ifelse(group=="Diabetes + Obesity"&value=="<=27", NA, value)) %>% drop_na()

DANU_Measures %>% group_by(group, value) %>% summarise(n=sum(weight))
names(DANU_Measures)[1] <- "patient"




NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", integer64 = "character", stringsAsFactors = F)


NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-disease) 
NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- as.numeric(NASH_Drug_Histories$Month)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Month>=49) %>% filter(Drugs!="-") %>% select(-c(Month, weight))

NAFLD_Drug_Histories <- gather(NAFLD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(-disease) 
NAFLD_Drug_Histories$Month <- as.character(NAFLD_Drug_Histories$Month)
NAFLD_Drug_Histories$Month <- parse_number(NAFLD_Drug_Histories$Month)
NAFLD_Drug_Histories$Month <- as.numeric(NAFLD_Drug_Histories$Month)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(Month>=49) %>% filter(Drugs!="-") %>% select(-c(Month, weight))

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-disease) 
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- as.numeric(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Month>=49) %>% filter(Drugs!="-") %>% select(-c(Month, weight))

OBE2_Drug_Histories <- gather(OBE2_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% select(-disease) 
OBE2_Drug_Histories$Month <- as.character(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- parse_number(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- as.numeric(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(Month>=49) %>% filter(Drugs!="-") %>% select(-c(Month, weight))

Treat_exp_LY <- NASH_Drug_Histories %>% select(patient) %>% distinct() %>%
  full_join(NAFLD_Drug_Histories %>% select(patient) %>% distinct()) %>%
  full_join(DIA_Drug_Histories %>% select(patient) %>% distinct()) %>%
  full_join(OBE2_Drug_Histories %>% select(patient) %>% distinct()) %>% distinct()


# High_Risk_pred_temporary %>% group_by(group) %>% summarise(n=sum(weight)) %>%
#   left_join(High_Risk_pred_temporary %>% inner_join(Treat_exp_LY) %>% group_by(group) %>% summarise(n2=sum(weight)) ) %>%
#   mutate(Perc=n2/n)


DANU_Measures %>% group_by(value) %>% summarise(n=sum(weight) / (314860+1091477+1640135+2219984))

DANU_Measures %>% group_by(value) %>% summarise(n=sum(weight)) %>%
  left_join(DANU_Measures %>% inner_join(Treat_exp_LY) %>% group_by(value) %>% summarise(n2=sum(weight)) ) %>%
  mutate(Perc=n2/n)

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_OralGLP1        <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(grepl(string_GLP1, Drugs))
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(grepl(string_GLP1, Drugs))

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% select(molecule, drug_group)
names(NASH_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_OralGLP1_NASH        <- paste0("\\b(",paste0(NASH_Ingredients$Drugs[NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")
string_GLP1_NASH  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[NASH_Ingredients$drug_group == "GLP1 Injectable"|NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(grepl(string_GLP1_NASH, Drugs))
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(grepl(string_GLP1_NASH, Drugs))

GLP1_exp_LY <- NASH_Drug_Histories %>% select(patient) %>% distinct() %>%
  full_join(NAFLD_Drug_Histories %>% select(patient) %>% distinct()) %>%
  full_join(DIA_Drug_Histories %>% select(patient) %>% distinct()) %>%
  full_join(OBE2_Drug_Histories %>% select(patient) %>% distinct()) %>% distinct()


DANU_Measures %>% group_by(value) %>% summarise(n=sum(weight)) %>%
  left_join(DANU_Measures %>% inner_join(GLP1_exp_LY) %>% group_by(value) %>% summarise(n2=sum(weight)) ) %>%
  mutate(Perc=n2/n)


DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs))
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(grepl(string_OralGLP1, Drugs))
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(grepl(string_OralGLP1_NASH, Drugs))
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(grepl(string_OralGLP1_NASH, Drugs))


Oral_GLP1_exp_LY <- NASH_Drug_Histories %>% select(patient) %>% distinct() %>%
  full_join(NAFLD_Drug_Histories %>% select(patient) %>% distinct()) %>%
  full_join(DIA_Drug_Histories %>% select(patient) %>% distinct()) %>%
  full_join(OBE2_Drug_Histories %>% select(patient) %>% distinct()) %>% distinct()


DANU_Measures %>% group_by(value) %>% summarise(n=sum(weight)) %>%
  left_join(DANU_Measures %>% inner_join(Oral_GLP1_exp_LY) %>% group_by(value) %>% summarise(n2=sum(weight)) ) %>%
  mutate(Perc=n2/n)


# -----------------
# Estimate Liver abusa, liver cancer, hepatitis in high risk populations -------------------------


High_Risk_pred_temporary <- fread("NASH Analysis Results 1.1/High_Risk_pred_temporary.txt")

DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- High_Risk_pred_temporary %>% filter(group=="Diabetes + Obesity") %>% select(patid) %>% inner_join(DIA_Comorbidity_Inventories)
DIA_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1582935
DIA_Comorbidity_Inventories %>% filter(diagnosis=="B15"|diagnosis=="B16"|diagnosis=="B17"|diagnosis=="B18"|diagnosis=="B19"|
                                           diagnosis=="C22"|diagnosis=="C78") %>% 
  summarise(n=sum(weight)) # 


DIA_Comorbidity_Inventories <- fread("DIA Analysis Results 1.1/DIA Comorbidity Inventories.txt")
DIA_Comorbidity_Inventories <- High_Risk_pred_temporary %>% filter(group=="Diabetes") %>% select(patid) %>% inner_join(DIA_Comorbidity_Inventories)
DIA_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 129884
DIA_Comorbidity_Inventories %>% filter(diagnosis=="B15"|diagnosis=="B16"|diagnosis=="B17"|diagnosis=="B18"|diagnosis=="B19"|
                                           diagnosis=="C22"|diagnosis=="C78") %>% 
  summarise(n=sum(weight)) # 


OBE2_Comorbidity_Inventories <- fread("OBE2 Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")
OBE2_Comorbidity_Inventories <- High_Risk_pred_temporary %>% filter(group=="Obesity") %>% select(patid) %>% inner_join(OBE2_Comorbidity_Inventories)
OBE2_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3200859
OBE2_Comorbidity_Inventories %>% filter(diagnosis=="B15"|diagnosis=="B16"|diagnosis=="B17"|diagnosis=="B18"|diagnosis=="B19"|
                                           diagnosis=="C22"|diagnosis=="C78") %>% 
  summarise(n=sum(weight)) # 

NAFLD_Comorbidity_Inventories <- fread("NAFLD Analysis Results 1.1/NAFLD Comorbidity Inventories.txt")
NAFLD_Comorbidity_Inventories <- High_Risk_pred_temporary %>% filter(group=="NAFLD") %>% select(patid) %>% inner_join(NAFLD_Comorbidity_Inventories)
NAFLD_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1253871
NAFLD_Comorbidity_Inventories %>% filter(diagnosis=="B15"|diagnosis=="B16"|diagnosis=="B17"|diagnosis=="B18"|diagnosis=="B19"|
                                           diagnosis=="C22"|diagnosis=="C78") %>% 
  summarise(n=sum(weight)) # 







# -------------

# Drug usage 12 months before and after 1st GLP1 -------------------------------

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)
string_GLP1  <- paste0("\\b(",paste0(NASH_Ingredients$molecule[NASH_Ingredients$drug_group == "GLP1 Injectable"|NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-")
NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

GLP1_Pats <- NASH_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>%
  filter(Month==min(Month)) %>% rename("FirstGLP1"="Month")
NASH_Drug_Histories <- GLP1_Pats %>% left_join(NASH_Drug_Histories) %>% select(-disease)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)

names(NASH_Ingredients)[1] <- "Drugs"
NASH_Ingredients$Drugs <- as.numeric(NASH_Ingredients$Drugs)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 

NASH_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 224681

NASH_Drug_Histories <- NASH_Drug_Histories %>%  mutate(Lapsed=Month-FirstGLP1) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(NASH_Drug_Histories)

NASH_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 52619

NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_group=ifelse(is.na(drug_group),"Lapsed",drug_group)) %>%
  rename("Drug Group"="drug_group") %>%
  mutate(n=n/52619) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(linewidth=2, alpha=.6) +
  theme_minimal() +
  ggsci::scale_color_jco() +
  #ylim(0,65) +
  xlab("\n No. Elapsed Months \n(Before/After 1st GLP1)") +
  ylab("Population % \n")


NASH_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()  %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))



NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_group=ifelse(is.na(drug_group),"Lapsed",drug_group)) %>%
  filter(drug_group!="Lapsed") %>%
    mutate(n=ifelse(drug_group=="Anticholesterol", n/45188,
                    ifelse(drug_group=="Antidiabetic", n/51888,
                           ifelse(drug_group=="Antiobesity",n/3757,
                                  ifelse(drug_group=="GLP1 Injectable",n/49866,
                                         ifelse(drug_group=="GLP1 Oral", n/3807,
                                                ifelse(drug_group=="Hepatoprotective", n/677 ,
                                                       ifelse(drug_group=="Hospitalization", n/3398, NA)))))))) %>%
  rename("Drug Group"="drug_group") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=0.6) +
  theme_minimal() +
  ggsci::scale_color_jco() +
  #ylim(0,75) +
  xlab("\n No. Elapsed Months \n(Before/After 1st GLP1)") +
  ylab("Population % (of Class-experienced) \n")



# ----------------------
# Drug usage 12 months before and after NASH and T2D Dx, NASH before T2D  -------------------------------

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, weight, nash, diabetes_onset)
NASH_Demographics$nash <- as.Date(NASH_Demographics$nash)
NASH_Demographics$diabetes_onset <- as.Date(NASH_Demographics$diabetes_onset)

NASH_Demographics <- NASH_Demographics %>% drop_na() %>% filter(nash>(diabetes_onset+365))
NASH_Demographics <- NASH_Demographics %>% mutate(nash=as.character(nash))
NASH_Demographics <- NASH_Demographics %>% mutate(diabetes_onset=as.character(diabetes_onset))
NASH_Demographics <- NASH_Demographics %>% mutate(nash=str_sub(nash, 1L, 7L))
NASH_Demographics <- NASH_Demographics %>% mutate(diabetes_onset=str_sub(diabetes_onset, 1L, 7L))

NASH_Demographics <- NASH_Demographics %>% left_join(Months_lookup, by=c("nash"="Month")) %>% 
  select(patid, Exact_Month, diabetes_onset) %>% distinct() %>% rename("NASH"="Exact_Month")  %>% 
  left_join(Months_lookup, by=c("diabetes_onset"="Month")) %>% 
  select(patid, Exact_Month, NASH ) %>% distinct() %>% rename("T2DM"="Exact_Month")

names(NASH_Demographics)[1] <- "patient"
NASH_Demographics <- NASH_Demographics %>% drop_na()


NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)
string_GLP1  <- paste0("\\b(",paste0(NASH_Ingredients$molecule[NASH_Ingredients$drug_group == "GLP1 Injectable"|NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-")
NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

GLP1_Pats <- NASH_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>%
  filter(Month==min(Month)) %>% rename("FirstGLP1"="Month")
NASH_Drug_Histories <- GLP1_Pats %>% left_join(NASH_Drug_Histories) %>% select(-disease)  %>% inner_join(NASH_Demographics)

NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)

names(NASH_Ingredients)[1] <- "Drugs"
NASH_Ingredients$Drugs <- as.numeric(NASH_Ingredients$Drugs)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 

NASH_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 4718
 

NASH_Drug_Histories %>% filter(NASH>0) %>% select(patient) %>% distinct() %>%
  left_join(
    NASH_Drug_Histories %>% select(patient, T2DM, NASH) %>% gather(drug_group, Month, T2DM:NASH) %>% select(1,3,2) %>% distinct() %>%
  bind_rows(NASH_Drug_Histories %>% group_by(patient, drug_group) %>% filter(Month==min(Month)) %>% select(patient, Month, drug_group))
  ) %>%
    group_by(drug_group) %>% summarise(n=mean(Month)) 



NASH_Drug_Histories %>% select(patient, T2DM, NASH) %>% gather(drug_group, Month, T2DM:NASH) %>% select(1,3,2) %>% distinct() %>%
  bind_rows(NASH_Drug_Histories %>% group_by(patient, drug_group) %>% filter(Month==min(Month)) %>% select(patient, Month, drug_group)) %>%
  filter(drug_group!="Hepatoprotective") %>%
  ggplot(aes(Month, colour=drug_group, fill=drug_group)) +
  geom_density(alpha=0.6) +
  theme_minimal() +
  xlab("\n Exact Month") + ylab("Patient density \n") +
  scale_fill_manual(values=c("#c49a7c","#eda864","#846910","#0b3360","#1b79e4", "#e47272","#a52a2a" ,"#87e2e8")) +
  scale_colour_manual(values=c("#c49a7c","#eda864","#846910","#0b3360","#1b79e4", "#e47272","#a52a2a" ,"#87e2e8")) 

    
# --------------------------------------


# GLP1 Usage per comorbidity status and BMI/HbA1c  -------------------------------

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, weight, nash, diabetes_onset, obesity_onset)
NASH_Demographics <- NASH_Demographics %>% 
  mutate(nash=ifelse(is.na(nash),0,1)) %>%
  mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset=ifelse(is.na(obesity_onset),0,1))


NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)
string_GLP1  <- paste0("\\b(",paste0(NASH_Ingredients$molecule[NASH_Ingredients$drug_group == "GLP1 Injectable"|NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-") %>% select(patient, Drugs) %>% distinct()

GLP1_Pats <- NASH_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>%
  mutate(GLP1="GLP1")

NASH_Demographics %>% left_join(GLP1_Pats, by=c("patid"="patient")) %>%
  group_by(nash, diabetes_onset, obesity_onset, GLP1) %>% summarise(n=sum(weight))




DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% select(-weight) %>% inner_join(NASH_Demographics)

DANU_Measures <- DANU_Measures %>% filter(test=="BMI"|test=="HbA1c Level") %>% 
  select(patid, weight, test, value) %>% distinct() %>% group_by(patid, test) %>% 
  filter(value==max(value)) %>% distinct() %>% ungroup()

DANU_Measures  %>% left_join(GLP1_Pats, by=c("patid"="patient"))  %>%
  group_by(GLP1, test) %>% summarise(n=mean(value))

BMI <- DANU_Measures %>% left_join(GLP1_Pats, by=c("patid"="patient"))  %>%
  filter(test=="BMI") %>% select(patid, value, GLP1) %>% mutate(GLP1=ifelse(is.na(GLP1),0,1))

BMI %>% filter(value<60) %>%
  ggplot(aes(value, GLP1)) +
 # geom_jitter() +
   geom_smooth(method = "glm", 
    method.args = list(family = "binomial"), 
    se = T, colour="firebrick", linewidth=2) +
  theme_minimal() +
    xlab("\n BMI") + ylab("Probablity of having been ON GLP1 \n")
  


HbA1c <- DANU_Measures %>% left_join(GLP1_Pats, by=c("patid"="patient"))  %>%
  filter(test=="HbA1c Level") %>% select(patid, value, GLP1) %>% mutate(GLP1=ifelse(is.na(GLP1),0,1))

HbA1c %>% filter(value<18) %>%
  ggplot(aes(value, GLP1)) +
 # geom_jitter() +
   geom_smooth(method = "glm", 
    method.args = list(family = "binomial"), 
    se = T, colour="midnightblue", linewidth=2) +
  theme_minimal() +
    xlab("\n HbA1c") + ylab("Probablity of having been ON GLP1 \n")
  

# ---------------

# Number of months / lines until each class initiation ---------------------------


NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt")




NAFLD_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Ingredients <- NAFLD_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_GLP1 <- paste0("\\b(",paste0(NAFLD_Ingredients$molecule[NAFLD_Ingredients$drug_group == "GLP1 Injectable"|NAFLD_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


# All pats
NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(-c(disease))
NAFLD_Drug_Histories <- gather(NAFLD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NAFLD_Drug_Histories$Month <- as.character(NAFLD_Drug_Histories$Month)
NAFLD_Drug_Histories$Month <- parse_number(NAFLD_Drug_Histories$Month)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% group_by(patient) %>% filter(Month<=12)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% filter(Drugs == "-")
Naive_until_12 <- NAFLD_Drug_Histories %>% group_by(patient) %>% count() %>% filter(n==12) %>% select(patient)

# Number of months
NAFLD_Drug_Histories <- fread("NAFLD Analysis Results 1.1/NAFLD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NAFLD_Drug_Histories <- NAFLD_Drug_Histories %>% select(-c(disease))
NAFLD_Drug_Histories <- Naive_until_12 %>% left_join(NAFLD_Drug_Histories)
NAFLD_Drug_Histories <- gather(NAFLD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NAFLD_Drug_Histories$Month <- as.character(NAFLD_Drug_Histories$Month)
NAFLD_Drug_Histories$Month <- parse_number(NAFLD_Drug_Histories$Month)

# Time to first  GLP1
NAFLD_Drug_Histories_GLP1 <- NAFLD_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_GLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_GLP1,Drugs)) else NA) 
NAFLD_Drug_Histories_GLP1 <- NAFLD_Drug_Histories_GLP1 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
NAFLD_Drug_Histories_GLP1 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
NAFLD_Drug_Histories_GLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
NAFLD_Drug_Histories_GLP1 %>% ungroup() %>% summarise(meNAFLDn=weighted.median(n, weight)) 
sum(NAFLD_Drug_Histories_GLP1$weight) 


# Number of lines
# Lines to first  GLP1
NAFLD_Drug_Histories_GLP1 <- NAFLD_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_GLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_GLP1,Drugs)) else NA) 
NAFLD_Drug_Histories_GLP1 <- NAFLD_Drug_Histories_GLP1 %>% select(-c(Month)) %>%  filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
NAFLD_Drug_Histories_GLP1 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
NAFLD_Drug_Histories_GLP1 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
NAFLD_Drug_Histories_GLP1 %>% ungroup() %>% summarise(meNAFLDn=weighted.median(n, weight)) 
sum(NAFLD_Drug_Histories_GLP1$weight) 




Months_vs_Lines_to_class <- fread("NASH Analysis Results 1.1/Months_vs_Lines_to_class.csv")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Months_vs_Lines_to_class, aes(x=average_months_to_class_all, y=average_lines_to_class_all, size = pop_all, fill=drug_class, colour=drug_class)) +
  geom_point(alpha=1)+
  geom_text_repel(aes(label = drug_class), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 10))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 10))+
  scale_colour_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  xlim(0,15) +
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")

# ----------------------

# Pathways to GLP1 -----------------



NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
string_GLP1 <- paste0("\\b(",paste0(NASH_Ingredients$molecule[NASH_Ingredients$drug_group == "GLP1 Injectable"|NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


# All pats
NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease))
NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)
NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient) %>% filter(Month<=12)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs == "-")
Naive_until_12 <- NASH_Drug_Histories %>% group_by(patient) %>% count() %>% filter(n==12) %>% select(patient)


NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-c(disease))
NASH_Drug_Histories <- Naive_until_12 %>% left_join(NASH_Drug_Histories)
NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)


NASH_Drug_Histories_GLP1 <- NASH_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_GLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_GLP1,Drugs)) else NA) 

NASH_Drug_Histories_GLP1 <- separate_rows(NASH_Drug_Histories_GLP1, Drugs, sep = ",", convert=T)

NASH_Drug_Histories_GLP1 <- NASH_Drug_Histories_GLP1 %>% filter(Drugs!="-") %>% left_join(NASH_Ingredients %>% select(molecule, drug_group), by=c("Drugs"="molecule")) %>%
  mutate(drug_group=ifelse(drug_group=="Anticholesterol", "Chol",
                                                ifelse(drug_group=="Antiobesity", "Obe",
                                                       ifelse(drug_group=="Hepatoprotective", "Hep",
                                                              ifelse(drug_group=="Antidiabetic", "Dia",
                                                                     ifelse(drug_group=="GLP1 Oral", "GLP1",
                                                                            ifelse(drug_group=="GLP1 Injectable", "GLP1", "Hosp"))))))) %>% select(-Drugs) %>% distinct()

unique(NASH_Drug_Histories_GLP1$drug_group)

NASH_Drug_Histories_GLP1 <- NASH_Drug_Histories_GLP1 %>% mutate(Index=ifelse(drug_group=="Chol", 1,
                                                                             ifelse(drug_group=="Obe",2,
                                                                                    ifelse(drug_group=="Dia",3,
                                                                                           ifelse(drug_group=="Hep",4,
                                                                                                  ifelse(drug_group=="GLP1",5,6))))))

NASH_Drug_Histories_GLP1 <- NASH_Drug_Histories_GLP1 %>% arrange(patient, weight, Month, Index) %>% select(-c(Month, Index)) %>% distinct() 

NASH_Drug_Histories_GLP1 <- NASH_Drug_Histories_GLP1 %>% group_by(patient, weight) %>% mutate(drug_group=paste(drug_group, collapse=" -> ")) %>% distinct()

data.frame(NASH_Drug_Histories_GLP1 %>% group_by(drug_group) %>% summarise(n=sum(weight)/45681.98) %>% arrange(-n))

# ---------------------------------------
# High risk with random forest ---------------------------------

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
length(unique(DANU_Measures$patid))
unique(DANU_Measures$test)

DANU_Measures <- DANU_Measures %>% select(patid, weight, test, claimed, value) %>% distinct()
DANU_Measures <- DANU_Measures %>% filter(test=="ALT Level"|test=="AST Level"|test=="Platelet Count")

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, weight) %>% mutate(group="NASH")
NAFLD_Demographics <- fread("NAFLD Analysis Results 1.1/NAFLD Demographics.txt")
NAFLD_Demographics <- NAFLD_Demographics %>% select(patid, weight) %>% mutate(group="NAFLD")

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis!="-")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% rename("group"="diagnosis")

DANU_Demographics <- DANU_Demographics %>% anti_join(NASH_Demographics %>% select(patid)) %>% anti_join(NAFLD_Demographics %>% select(patid)) 

groups <- DANU_Demographics %>% bind_rows(NASH_Demographics) %>% bind_rows(NAFLD_Demographics) 


groups <- groups %>% inner_join(DANU_Measures)

groups %>% select(patid, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))


groups <- groups %>% group_by(patid, test) %>% mutate(value=ifelse(test=="Platelet Count", min(value), max(value))) %>% slice(1)
groups <- groups %>% select(-claimed)
groups <- groups %>% spread(key=test, value=value) %>% drop_na()

Negative <- groups %>%  filter(`ALT Level`<20 & `AST Level`<20 & `Platelet Count`>300 & group!="NASH" & group != "NAFLD") %>% 
  ungroup() %>% select(-c(patid, weight, group)) 

Negative <- Negative[1:500,]

names(Negative) <- c("ALT", "AST", "Platelets")

Negative$group <- 0

Positive <- groups %>%  filter(`ALT Level`>50 & `AST Level`>50 & `Platelet Count`<150 & group=="NASH") %>% 
  ungroup() %>% select(-c(patid, weight, group)) 

Positive <- Positive[1:500,]
names(Positive) <- c("ALT", "AST", "Platelets")

Positive$group <- 1

to_train <- Negative %>% bind_rows(Positive)
to_train %>% group_by(group) %>% count()

to_train <- to_train %>% sample_n(1000)

groups %>% group_by(group) %>% summarise(n=sum(weight))


groups2 <- groups %>% ungroup() %>% select(-c(patid,weight)) 
groups2 <- groups2 %>% select(-group)

names(groups2) <- c("ALT", "AST", "Platelets")



library(randomForest)
modelAll_1_randomForest <- randomForest(group ~ ., data = to_train)
summary(modelAll_1_randomForest)


data.frame(predict(modelAll_1_randomForest, groups2)) %>%
  group_by( predict.modelAll_1_randomForest..groups2.) %>% count() %>% mutate(n=100*n/455923) 




data.frame(predict(modelAll_1_randomForest, groups2)) %>%
  ggplot(aes(predict.modelAll_1_randomForest..groups2.)) +
  geom_density(aes(y=..scaled..),  colour="navy", fill="navy", alpha=0.8, adjust=1) +
  theme_minimal() +
  xlab(" \n Propensity Score") + ylab(" Patients Density \n")




# ----------------------------

# Other procedures --------

NASH_Treated_Procedure_codes_lookup_unspec <- fread("NASH_Treated_Procedure_codes_lookup_unspec.csv")
NASH_Treated_Procedure_codes_lookup_unspec <- NASH_Treated_Procedure_codes_lookup_unspec %>% select(-description)

NASH_Treated_Procedure_codes_lookup_unspec <- NASH_Treated_Procedure_codes_lookup_unspec %>%  
  mutate(code=str_replace(code,"P=", "")) %>%  
  mutate(code=str_replace(code,"R=", "")) %>%  
  mutate(code=str_replace(code,"\\$", ""))

NASH_pts_prcds <- fread("NASH_pts_prcds.txt")

NASH_pts_prcds <- NASH_pts_prcds %>% left_join(NASH_Treated_Procedure_codes_lookup_unspec, by=c("PROC"="code"))

NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight)

NASH_pts_prcds <- NASH_pts_prcds %>% left_join(NASH_Drug_Histories, by=c("PTID"="patient"))

sum(NASH_Drug_Histories$weight) # 

NASH_pts_prcds %>% select(PTID, weight, Specify, Type) %>% distinct() %>%
  group_by(Specify, Type) %>% summarise(n=sum(weight)/1384888)

# ----------
# triglycerides -----------------------------------------------------

DANU_Measures_Additional <- fread("DANU Measures Additional 1.2/DANU Measures Additional.txt")
DANU_Measures_Additional <- DANU_Measures_Additional %>% filter(test=="Triglycerides") %>% select(patid, weight, value, unit) %>% distinct()
DANU_Measures_Additional <- DANU_Measures_Additional %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% ungroup()


unique(DANU_Measures_Additional$unit)


NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid) %>% mutate(group="NASH")
NAFLD_Demographics <- fread("NAFLD Analysis Results 1.1/NAFLD Demographics.txt")
NAFLD_Demographics <- NAFLD_Demographics %>% select(patid) %>% mutate(group="NAFLD")

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(patient) %>% rename("patid"="patient") %>% mutate(group="DIA") %>% 
  anti_join(NASH_Demographics %>% select(patid))  %>%
  anti_join(NAFLD_Demographics %>% select(patid))
OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% select(patient) %>% rename("patid"="patient") %>% mutate(group="OBE") %>% 
  anti_join(NASH_Demographics %>% select(patid))  %>%
  anti_join(NAFLD_Demographics %>% select(patid))


to_track <- OBE2_Drug_Histories %>% bind_rows(DIA_Drug_Histories) %>% bind_rows(NASH_Demographics) %>% bind_rows(NAFLD_Demographics)

to_track %>% inner_join(DANU_Measures_Additional) %>% group_by(group) %>% summarise(n=mean(value))

to_track %>% inner_join(DANU_Measures_Additional) %>% 
  mutate(group=factor(group, levels=c("NASH", "NAFLD", "DIA", "OBE"))) %>%
  ggplot(aes(value, colour=group, fill=group)) +
  geom_histogram(bins=100, alpha=0.7, show.legend = F) +
  xlim(0,700) +
  facet_wrap(~group, scale="free") +
  theme_minimal() +
  ggsci::scale_color_jco()+ggsci::scale_fill_jco() +
  xlab("Triglycerides")

to_track %>% inner_join(DANU_Measures_Additional) %>% 
  mutate(value=ifelse(value<150, "<150",
                      ifelse(value<=300, "<300", ">300"))) %>%
  group_by(group,value) %>% count() %>%
  spread(key=value, value=n) 

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")


NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis",
                                          ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) %>%
  select(patid, group) %>% inner_join(DANU_Measures_Additional) %>% group_by(group) %>% summarise(n=mean(value))

sum(NASH_Demographics$weight)





NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-")
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-Month) %>% distinct() 
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-disease) %>% distinct() 

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)
names(NASH_Ingredients)[1] <- "Drugs"
NASH_Ingredients$Drugs <- as.numeric(NASH_Ingredients$Drugs)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, drug_group) %>% distinct() 
NASH_Drug_Histories <- NASH_Drug_Histories %>% mutate(Exp=1) %>% spread(key=drug_group, value=Exp)
NASH_Drug_Histories[is.na(NASH_Drug_Histories)] <- 0
names(NASH_Drug_Histories)[1] <- "patid"

NASH_Drug_Histories <- NASH_Drug_Histories %>% inner_join(DANU_Measures_Additional %>% select(patid, value)) %>% select(-patid)



NASH_Drug_Histories %>% 
  filter(value<=1000) %>%
  ggplot(aes(value, Hepatoprotective      )) +
 # geom_jitter() +
   geom_smooth(method = "glm", 
    method.args = list(family = "binomial"), 
    se = T, colour="firebrick", linewidth=2) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
    xlab("\n Triglycerides") + ylab("Probablity of having been ON Hepatoprotective \n")



NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-")
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-Month) %>% distinct() 
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-disease) %>% distinct() 

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)
names(NASH_Ingredients)[1] <- "Drugs"
NASH_Ingredients$Drugs <- as.numeric(NASH_Ingredients$Drugs)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, drug_group) %>% distinct() 
names(NASH_Drug_Histories)[1] <- "patid"

DANU_Measures_Additional <- DANU_Measures_Additional %>% select(-unit) %>% mutate(value=ifelse(value<150, "<150",
                                                                   ifelse(value<300, "<300", ">300")))

DANU_Measures_Additional %>% inner_join(NASH_Demographics %>% select(patid)) %>% group_by(value) %>% summarise(n=sum(weight))
DANU_Measures_Additional <- DANU_Measures_Additional %>% inner_join(NASH_Demographics %>% select(patid))

NASH_Drug_Histories <- NASH_Drug_Histories %>% inner_join(DANU_Measures_Additional %>% select(patid, value, weight)) 

NASH_Drug_Histories  %>%
  group_by(value, drug_group) %>% summarise(n=sum(weight)) %>% mutate(n=ifelse(value=="<150", n/197351.,
                                                                   ifelse(value=="<300", n/234724., n/90167. ))) %>%
  spread(key=value, value=n)




# --------------------------

# Procedures done for pats dx last 2 years --------------------------------
NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")

NASH_Demographics <- NASH_Demographics %>% mutate(group=ifelse(!is.na(cirrhosis), "Cirrhosis", 
                                                               ifelse(!is.na(fibrosis), "Fibrosis", "NASH-only"))) 

max(NASH_Demographics$nash) # "2022-04-29"
min(NASH_Demographics$nash) # "2016-10-01"

NASH_Demographics <- NASH_Demographics %>% mutate(nash=as.Date(nash)) %>% filter(nash>="2020-07-01")

sum(NASH_Demographics$weight[is.na(NASH_Demographics$fibrosis)&is.na(NASH_Demographics$cirrhosis)]) # 306506
sum(NASH_Demographics$weight[!is.na(NASH_Demographics$fibrosis)&is.na(NASH_Demographics$cirrhosis)]) # 34229.28
sum(NASH_Demographics$weight[!is.na(NASH_Demographics$cirrhosis)]) # 98278.41

NASH_Demographics <- NASH_Demographics %>% select(patid, weight, group, nash, nafld, chronic_liver_disease, cirrhosis, liver_biopsy, 
                             liver_ultrasound, fibrosis, liver_failure, liver_imaging, liver_transplant)

NASH_Demographics <- gather(NASH_Demographics, Type, Date, nash:liver_transplant, factor_key=TRUE)

NASH_Demographics <- NASH_Demographics %>% mutate(Date=as.character(Date)) %>% mutate(Date=ifelse(is.na(Date), 0,1))

NASH_Demographics %>% filter(Date==1) %>% group_by(group, Type) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(group=="Cirrhosis", n/98278.41,
                  ifelse(group=="Fibrosis", n/34229.28, n/306506))) %>%
  spread(key=group, value=n)

# --------------------------------------------
# Drug usage 12 months before and after 1st NASH Dx CUMULATIVE  -------------------------------
NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")
NASH_Demographics <- NASH_Demographics %>% select(patid, weight, nash)
min(NASH_Demographics$nash)
NASH_Demographics <- NASH_Demographics %>% mutate(nash=as.character(nash))
NASH_Demographics <- NASH_Demographics %>% mutate(nash=str_sub(nash, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

NASH_Demographics <- NASH_Demographics %>% left_join(Months_lookup, by=c("nash"="Month")) %>% 
  select(patid, Exact_Month) %>% distinct()
names(NASH_Demographics)[1] <- "patient"
NASH_Demographics <- NASH_Demographics %>% drop_na()

NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Demographics <- NASH_Drug_Histories %>% select(patient) %>% inner_join(NASH_Demographics)

NASH_Demographics %>%
  ggplot(aes(Exact_Month)) +
  geom_density(fill="darkslategray4", colour="darkslategray", size=1, alpha=0.5) +
  theme_minimal() +
  xlab("\n Month of 1st NASH  Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

names(NASH_Demographics)[2] <- "First_NASH_Dx"

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
#NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-")
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)



NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)

NASH_Ingredients$molecule <- as.character(NASH_Ingredients$molecule)
names(NASH_Ingredients)[1] <- "Drugs"

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 
NASH_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 1384888

NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-disease)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()
length(unique(NASH_Drug_Histories$patient))

NASH_Drug_Histories <- NASH_Drug_Histories %>% arrange(patient, weight, Month, drug_group)
NASH_Drug_Histories <- NASH_Drug_Histories %>% mutate(Exp=1)

NASH_Drug_Histories <- NASH_Drug_Histories %>% spread(key=drug_group, value=Exp)


NASH_Drug_Histories <- NASH_Drug_Histories %>% group_by(patient, weight) %>% fill(Anticholesterol, Antidiabetic, Antiobesity, `GLP1 Injectable`, `GLP1 Oral`, Hepatoprotective, Hospitalization, `<NA>`)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-`<NA>`)

NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Demographics) %>% 
  mutate(Lapsed=Month-First_NASH_Dx) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(NASH_Drug_Histories)

NASH_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 670728

NASH_Drug_Histories <- gather(NASH_Drug_Histories, drug_group, Exp, Anticholesterol:Hospitalization, factor_key=TRUE)

NASH_Drug_Histories %>% filter(Exp==1) %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  ungroup() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)

NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Exp==1) %>% select(patient, weight, drug_group, Lapsed) %>% distinct()


NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  rename("Drug Group"="drug_group") %>%
  mutate(n=n/670728) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=.6) +
  theme_minimal() +
  ggsci::scale_color_jco() +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st NASH Dx)") +
  ylab("Population % \n")


NASH_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()  %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))



NASH_Drug_Histories %>% select(patient, weight, drug_group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
    mutate(n=ifelse(drug_group=="Anticholesterol", n/332627,
                    ifelse(drug_group=="Antidiabetic", n/270729,
                           ifelse(drug_group=="Antiobesity",n/55647,
                                  ifelse(drug_group=="GLP1 Injectable",n/74399,
                                         ifelse(drug_group=="GLP1 Oral", n/3193,
                                                ifelse(drug_group=="Hepatoprotective", n/17976,
                                                       ifelse(drug_group=="Hospitalization", n/77729, NA)))))))) %>%
  rename("Drug Group"="drug_group") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=0.6) +
  theme_minimal() +
  ggsci::scale_color_jco() +
  #ylim(0,75) +
  xlab("\n No. Elapsed Months \n(Before/After 1st NASH Dx)") +
  ylab("Population % (of Class-experienced) \n")

# ------------------------------------


# NASH tests by fibrosis stage -----------------------------------

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="Fibrosis Score"|test=="Fibrosis Stage"|test=="NASH Score"|test=="Fibrosis Activity")
DANU_Measures <- DANU_Measures %>% group_by(patid,test) %>% filter(value==max(value)) %>% slice(1)

Fibrosis_Stages <- DANU_Measures %>% filter(test=="Fibrosis Stage") %>%
  mutate(value=round(value,0)) %>% select(patid, value) %>% rename("Stage"="value")

DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="ALT Level"|test=="AST Level"|test=="Platelet Count")
DANU_Measures <- DANU_Measures %>% inner_join(Fibrosis_Stages %>% ungroup() %>% select(patid)) %>% group_by(patid, test) %>% mutate(value=ifelse(test=="Platelet Count", min(value), max(value))) %>% slice(1)
DANU_Measures <- DANU_Measures %>% select(patid, test, value)

DANU_Measures <- DANU_Measures %>% ungroup() %>% inner_join(Fibrosis_Stages %>% ungroup() %>% select(-test)) 

library(ggridges)


DANU_Measures %>% group_by(Stage, test) %>% summarise(n=median(value)) %>%
  spread(key=Stage, value=n)

DANU_Measures %>% filter(test=="AST Level"&value<=200) %>%
  ggplot(aes(x = value, y = as.factor(Stage), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n AST Level (IU)") + ylab("Fibrosis Stage \n")


DANU_Measures %>% filter(test=="ALT Level"&value<=200) %>%
  ggplot(aes(x = value, y = as.factor(Stage), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n ALT Level (IU)") + ylab("Fibrosis Stage \n")


DANU_Measures %>% filter(test=="Platelet Count"&value<=500) %>%
  ggplot(aes(x = value, y = as.factor(Stage), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n Platelet Count") + ylab("Fibrosis Stage \n")


Fibrosis_Stages <- Fibrosis_Stages %>% ungroup() %>% left_join(DANU_Measures %>% select(patid, weight)) %>% select(-test)
names(Fibrosis_Stages)[1] <- "patient"

NASH_Treatment_exp_Vector <- fread("NASH Analysis Results 1.1/NASH_Treatment_exp_Vector.txt")
NASH_Drug_Histories     <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
NASH_Drug_Histories <- NASH_Treatment_exp_Vector %>% left_join(NASH_Drug_Histories)

Fibrosis_Stages <- Fibrosis_Stages %>% inner_join(NASH_Treatment_exp_Vector)
Fibrosis_Stages %>% group_by(Stage) %>% summarise(n=sum(weight))

NASH_Drug_Histories <- Fibrosis_Stages %>% inner_join(NASH_Drug_Histories)

length(unique(NASH_Drug_Histories$patient)) # 167
sum(as.numeric(NASH_Drug_Histories$weight)) # 21400.43

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
NASH_Ingredients  <- NASH_Ingredients %>% select(molecule, drug_group)
names(NASH_Ingredients)[1] <- "Drugs"
NASH_Ingredients$Drugs <- as.numeric(NASH_Ingredients$Drugs)

NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-Month) %>% filter(Drugs != "-") %>% distinct()
NASH_Drug_Histories <- separate_rows(NASH_Drug_Histories, Drugs, sep = ",", convert=T)
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(-disease)
NASH_Drug_Histories <- NASH_Drug_Histories %>% left_join(NASH_Ingredients) 
NASH_Drug_Histories <- NASH_Drug_Histories %>% select(patient, weight, Stage, drug_group) %>% distinct() 


data.frame(NASH_Drug_Histories %>% select(patient, weight, Stage) %>% distinct() %>% group_by(Stage) %>% summarise(total=sum(weight)) %>%
  left_join(NASH_Drug_Histories %>% select(patient, weight, Stage, drug_group) %>% distinct()  %>% group_by(Stage, drug_group) %>% 
  summarise(n=sum(weight)))) %>% mutate(perc=n/total) %>%
  select(-c(total, n)) %>%
  spread(key=Stage, value=perc)


# -----------------------------------
# Last test result for those who tried GLP1 ---------------
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

NASH_Drug_Histories <- fread("NASH Analysis Results 1.1/NASH Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

NASH_Ingredients <- fread("NASH Analysis Results 1.1/NASH Ingredients.txt", integer64 = "character", stringsAsFactors = F)
NASH_Ingredients <- NASH_Ingredients  %>%  separate(drug_id, c('group', 'molecule'))
NASH_Ingredients <- NASH_Ingredients %>% select(molecule, drug_group)
unique(NASH_Ingredients$drug_group)
string_GLP1  <- paste0("\\b(",paste0(NASH_Ingredients$molecule[NASH_Ingredients$drug_group == "GLP1 Injectable"|NASH_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


NASH_Drug_Histories <- gather(NASH_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
NASH_Drug_Histories <- NASH_Drug_Histories %>% filter(Drugs!="-")
NASH_Drug_Histories$Month <- as.character(NASH_Drug_Histories$Month)
NASH_Drug_Histories$Month <- parse_number(NASH_Drug_Histories$Month)

GLP1_Pats <- NASH_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() 

GLP1_Pats_3m <- NASH_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% group_by(patient) %>% count() %>% filter(n>3) %>% select(patient) %>% distinct() 
GLP1_Pats_3m$group <- "GLP1"
Other_Pats <- NASH_Drug_Histories %>% select(patient) %>% distinct() %>% anti_join(GLP1_Pats) 
Other_Pats$group <- "Other"

To_compare <- GLP1_Pats_3m %>% bind_rows(Other_Pats) 


DANU_Measures <- fread("DANU Measures 1.1/DANU Measures.txt")
unique(DANU_Measures$test)
DANU_Measures <- DANU_Measures %>% select(patid, test, value, claimed) %>% filter(test=="ALT Level"|
                                                          test=="AST Level"|
                                                          test=="BMI"|
                                                          test=="HbA1c Level")
names(DANU_Measures)[1] <- "patient"

DANU_Measures <- DANU_Measures %>% inner_join(To_compare)

DANU_Measures <- DANU_Measures %>% group_by(patient, test) %>% filter(claimed==max(claimed)) %>% filter(value==max(value)) 
DANU_Measures <- DANU_Measures %>% ungroup()

DANU_Measures %>% group_by(group, test) %>% summarise(n=mean(value))

DANU_Measures %>% ungroup() %>%
  filter( (test=="ALT Level"&value<250) | (test=="AST Level"&value<200) | (test=="BMI"&value<70) | (test=="HbA1c Level"&value<14)) %>%
  ggplot(aes(value, colour=group, fill=group)) +
  geom_density(alpha=0.5) +
  facet_wrap(~test, scale="free") +
  theme_minimal() +
  ggsci::scale_color_jama() +
  ggsci::scale_fill_jama() +
  xlab("\n Last observed lab result") + ylab("Patient density \n ")


DANU_Measures %>% filter(test=="AST Level") %>% mutate(value=ifelse(value>=50, 50, 0)) %>% group_by(value) %>% count() # 0.1938493
DANU_Measures %>% filter(test=="ALT Level") %>% mutate(value=ifelse(value>=50, 50, 0)) %>% group_by(value) %>% count() # 0.2813602
DANU_Measures %>% filter(test=="HbA1c Level") %>% mutate(value=ifelse(value>=7.5, 7.5, 0)) %>% group_by(value) %>% count() # 0.2121011
DANU_Measures %>% filter(test=="BMI") %>% mutate(value=ifelse(value>=35, 35, 0)) %>% group_by(value) %>% count() # 0.4

# ----------

# Jay Figures DIA/OBE GLP1 October ---------------------

NASH_Demographics <- fread("NASH Analysis Results 1.1/NASH Demographics.txt")

sum(NASH_Demographics$weight) # 1384888

NASH_Demographics %>%
  mutate(nash=ifelse(is.na(nash),0,1)) %>%
  mutate(nafld=ifelse(is.na(nafld),0,1)) %>%
  mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset =ifelse(is.na(obesity_onset ),0,1)) %>%
  filter(diabetes_onset==1 & obesity_onset==1) %>%
  summarise(n=sum(weight))



NAFLD_Demographics <- fread("NAFLD Analysis Results 1.1/NAFLD Demographics.txt")

sum(NAFLD_Demographics$weight) # 14631378


NAFLD_Demographics %>%
  mutate(nafld=ifelse(is.na(nafld),0,1)) %>%
  mutate(diabetes_onset=ifelse(is.na(diabetes_onset),0,1)) %>%
  mutate(obesity_onset =ifelse(is.na(obesity_onset ),0,1)) %>%
  filter(diabetes_onset==0 & obesity_onset==0) %>%
  summarise(n=sum(weight))


6269112

6593892

DANU_Demographics <- fread("DANU Demographics 1.1/DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% filter(diagnosis!="-")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) 


High_Risk_pred_temporary <- fread("NASH Analysis Results 1.1/High_Risk_pred_temporary.txt")
High_Risk_pred_temporary <- High_Risk_pred_temporary %>% left_join(DANU_Demographics) %>% select(-group)
High_Risk_pred_temporary %>% group_by(diagnosis) %>% summarise(n=sum(weight))
names(High_Risk_pred_temporary)[1] <- "patient"
High_Risk_pred_temporary <- High_Risk_pred_temporary %>% drop_na()

DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
OBE2_Drug_Histories <- fread("OBE2 Analysis Results 1.1/OBE2 Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% select(-disease) 
DIA_Drug_Histories$Month <- as.character(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- parse_number(DIA_Drug_Histories$Month)
DIA_Drug_Histories$Month <- as.numeric(DIA_Drug_Histories$Month)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Month>=37&Month<49) %>% filter(Drugs!="-") %>% select(-c(Month, weight))

OBE2_Drug_Histories <- gather(OBE2_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% select(-disease) 
OBE2_Drug_Histories$Month <- as.character(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- parse_number(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories$Month <- as.numeric(OBE2_Drug_Histories$Month)
OBE2_Drug_Histories <- OBE2_Drug_Histories %>% filter(Month>=37&Month<49) %>% filter(Drugs!="-") %>% select(-c(Month, weight))


DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Ingredients  <- DANU_Ingredients %>% select(molecule, drug_group)
names(DANU_Ingredients)[1] <- "Drugs"
DANU_Ingredients$Drugs <- as.numeric(DANU_Ingredients$Drugs)
string_GLP1  <- paste0("\\b(",paste0(DANU_Ingredients$Drugs[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")

GLP1<- DIA_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% mutate(GLP1="GLP1") %>%
  full_join(OBE2_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% mutate(GLP1="GLP1")) %>%
  distinct()

High_Risk_pred_temporary %>% left_join(GLP1) %>%
  group_by(diagnosis, GLP1) %>% summarise(n=sum(weight)) %>%
  spread(key=GLP1, value=n) %>% mutate(perc=`GLP1`/(`GLP1`+`<NA>`))


# ------------
