library(tidyverse)
library(data.table)
library(survey)
options(scipen = 999)


options(survey.lonely.psu='adjust')

cat("R package versions:\n")

for (p in c("base", "survey","dplyr")) { 
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}


download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_ALB_CR.XPT", tf <- tempfile(), mode="wb")
Albumin_Creatinine <- foreign::read.xport(tf)[,]
Albumin_Creatinine <- Albumin_Creatinine[,c("SEQN", "URDACT")]
Albumin_Creatinine <- na.omit(Albumin_Creatinine)
names(Albumin_Creatinine)[2] <- "ACR"


download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_BIOPRO.XPT", tf <- tempfile(), mode="wb")
CBC <- foreign::read.xport(tf)[,]
CBC <- CBC[,c("SEQN", "LBXSCR")]
CBC <- na.omit(CBC)
length(unique(CBC$SEQN))
names(CBC)[2] <- "Creatinine_Serum"

Albumin_Creatinine <- Albumin_Creatinine %>% inner_join(CBC)

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.XPT", tf <- tempfile(), mode = "wb")
Dems <- foreign::read.xport(tf)[,]
Dems <- Dems[,c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1")]
Dems <- na.omit(Dems)
length(unique(Dems$SEQN))
names(Dems)[2] <- "Gender"
names(Dems)[3] <- "Age"
names(Dems)[4] <- "Race"

Albumin_Creatinine <- Albumin_Creatinine %>% inner_join(Dems) %>% na.omit()

# Based on CKD-EPI equation for calibrated creatinine: 
# eGFR=141 x [min(serum creatinine in mg/dL) /κ, 1)]**α x [max(serum creatinine/κ, 1)]**-1.20 x 0.993**age x (1.018 if female) x (1.159 if a Black person).

# κ = 0.7 if female and 0.9 if male
# α = -0.329 if female and -0.411 if male
# ** = raise to the power

Albumin_Creatinine <- Albumin_Creatinine %>% mutate(kappa = ifelse(Gender == 1, 0.9, 0.7)) %>%
                                             mutate(alpha = ifelse(Gender == 1, -0.411, -0.329))

Albumin_Creatinine <- Albumin_Creatinine %>% mutate(MIN = ifelse( Creatinine_Serum/kappa < 1 , Creatinine_Serum/kappa , 1)) %>%
                                             mutate(MAX = ifelse(Creatinine_Serum/kappa > 1 , Creatinine_Serum/kappa, 1))

Albumin_Creatinine <- Albumin_Creatinine %>% mutate(eGFR = 141 * MIN^alpha * MAX^(-1.2) * 0.993^Age  )

Albumin_Creatinine <- Albumin_Creatinine %>% mutate(eGFR = ifelse(Gender == 1, eGFR, eGFR * 1.018)) %>%
                                             mutate(eGFR = ifelse(Race == 4, eGFR * 1.159, eGFR))

mean(Albumin_Creatinine$eGFR) # 101
length(unique(Albumin_Creatinine$SEQN)) # 9308
length(unique(Albumin_Creatinine$SEQN[Albumin_Creatinine$ACR > 30 | Albumin_Creatinine$eGFR < 60])) #1619|> 17.39% somewhat bit biased

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_KIQ_U.XPT", tf <- tempfile(), mode = "wb")
Kidney_Quest <- foreign::read.xport(tf)[,]
Kidney_Quest <- Kidney_Quest[,c("SEQN", "KIQ022")]
Kidney_Quest <- na.omit(Kidney_Quest)
length(unique(Kidney_Quest$SEQN))
names(Kidney_Quest)[2] <- "Kidney_Awareness"

Kidney_Quest %>% inner_join(Albumin_Creatinine) %>% select(SEQN) %>% distinct() #7743
Kidney_Quest %>% inner_join(Albumin_Creatinine) %>%  filter(ACR > 30 | eGFR < 60) %>% select(SEQN) %>% distinct() #1463 #18.89% higher?
Kidney_Quest %>% inner_join(Albumin_Creatinine) %>%  filter( (ACR > 30 | eGFR < 60) & Kidney_Awareness == 1 ) %>% select(SEQN) %>% distinct() #213 #14.56% higher?
