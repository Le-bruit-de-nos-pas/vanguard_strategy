
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(neuralnet)
options(scipen = 999)

DANU_Ingredients <- fread("DIA Analysis Results 1.1/DANU Ingredients.txt", integer64 = "character", stringsAsFactors = F)
DANU_Ingredients <- DANU_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
length(unique(DANU_Ingredients$molecule)) 
string_GLP1 <- paste0("\\b(",paste0(DANU_Ingredients$molecule[DANU_Ingredients$drug_group == "GLP1 Injectable"|DANU_Ingredients$drug_group == "GLP1 Oral"], collapse = "|"),")\\b")


DIA_Drug_Histories <- fread("DIA Analysis Results 1.1/DIA Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector <- fread("DIA Analysis Results 1.1/Treatment_exp_Vector.txt")
DIA_Drug_Histories <- Treatment_exp_Vector %>% left_join(DIA_Drug_Histories) %>% select(-c(disease, weight)) 
DIA_Drug_Histories <- gather(DIA_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(Drugs!="-")
DIA_Drug_Histories <- DIA_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patient) %>% distinct() %>% left_join(DIA_Drug_Histories) 
DIA_Drug_Histories$Month <- str_replace_all(DIA_Drug_Histories$Month , "month", "m")
DIA_Drug_Histories <- separate_rows(DIA_Drug_Histories, Drugs, sep = ",", convert=T)

length(unique(DIA_Drug_Histories$Drugs)) # 37
length(unique(DIA_Drug_Histories$Month)) # 60
length(unique(DIA_Drug_Histories$patient))# 56119

DIA_Drug_Histories <- DIA_Drug_Histories %>% mutate(Var=paste(Month, Drugs, sep="_")) %>% select(-c(Month, Drugs))

DIA_Drug_Histories$Exp <- 1

DIA_Drug_Histories <- DIA_Drug_Histories %>% arrange(patient, Var) %>% spread(key=Var, value=Exp)

DIA_Drug_Histories[is.na(DIA_Drug_Histories)] <- 0

DIA_Drug_Histories %>% group_by(m60_47) %>% count()

test_df <- DIA_Drug_Histories %>% group_by(m60_47) %>% sample_n(1500) %>% ungroup() 

train <- test_df %>% sample_n(2000)
eval <- test_df %>% anti_join(train)

train <- train %>% select(-patient)
eval <- eval %>% select(-patient)

NN1_model <- neuralnet(m60_47 ~ . , data = train, hidden = c(30,10,5), linear.output = FALSE, err.fct = 'ce', likelihood = TRUE)

NN1_model$result.matrix[1,1] # 1.397397  


pred <- predict(NN1_model,eval) 
table(eval$m60_47==1, pred[,1]>0.5)


pred <- predict(NN1_model,DIA_Drug_Histories[,-1]) 
table(DIA_Drug_Histories$m60_47==1, pred[,1]>0.5)

sum(pred[,1]>0.5)
sum(DIA_Drug_Histories$m60_47==1)

# Not bad 
