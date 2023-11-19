library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(readxl)
library(utile.visuals)
options(scipen = 999)

# Drug Class Penetrance Ever --------------------------------------------------------------------

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer_2] 


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) # 174066

CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)
names(CANCX_Drug_Histories_v2)[4] <- "drug_id"

CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 Drug_formulary[, .(drug_id, drug_class)], 
                                 by="drug_id", all.x=TRUE)

length(unique(CANCX_Drug_Histories_v2$patid))
unique(CANCX_Drug_Histories_v2$drug_class)

# overall

data.frame(CANCX_Drug_Histories_v2 %>% select(patid, weight3, drug_class) %>% distinct() %>% 
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight3))/3452283))

setDT(CANCX_Drug_Histories_v2)

CANCX_Drug_Histories_v2 <- unique(CANCX_Drug_Histories_v2[, .(patid, weight3, drug_class)])

CANCX_Drug_Histories_v2[ , .(group_sum = sum(as.numeric(weight3))/3452283), by = drug_class] 


# Cachexia Dx vs Cachexia Pred vs No Cachexia

Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3[, .(patid, weight3, cachexia_id_type)]

Cachexia_identified_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = cachexia_id_type] 

# 1: Cachexia predicted 1252751.45
# 2: Cachexia/Sarcop dx   24788.95
# Other = 3452283 - (1252751.45+24788.95) = 2174743

CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 Cachexia_identified_pts_3, 
                                 by=c("patid", "weight3"), all.x=TRUE)
  
CANCX_Drug_Histories_v2[ , .(group_sum = sum(as.numeric(weight3))), by = c("cachexia_id_type","drug_class")] 




CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 CANCX_Active_pts_3, 
                                 by=c("patid", "weight3"), all.x=TRUE)
  
temp <- CANCX_Drug_Histories_v2[ , .(sum = sum(as.numeric(weight3))), by = c("primary_cancer_2", "drug_class")] 

temp <- merge(CancerType_Size, temp, by=c("primary_cancer_2"), all.x=TRUE)

temp[ drug_class=="Anamorelin" , c("primary_cancer_2", "sum")]

temp$sum <- temp$sum / temp$group_sum

temp <- temp %>% spread(key=drug_class, value=sum)

fwrite(temp, "temp.csv")

# --------------------------------------------------------


# BMI evolution before/after Anamorelin start ----------------------------------------


CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer_2] 
CANCX_Active_pts_3 <- CANCX_Active_pts_3[primary_cancer_2 %in% c("Intestinal Cancer", "Lung Cancer", "Pancreatic Cancer", "Gastrointestinal Cancer"), ]


Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3[, .(patid)]

CANCX_Active_pts_3 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 Cachexia_identified_pts_3, 
                                 by="patid", all=FALSE)



CANCX_Diagnosis_Histories_v2 <- fread("CANCX_Diagnosis_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Diagnosis_Histories_v2 <- gather(CANCX_Diagnosis_Histories_v2, Month, Dxs, month1:month74, factor_key=TRUE)
CANCX_Diagnosis_Histories_v2$Month <-  parse_number(as.character(CANCX_Diagnosis_Histories_v2$Month))
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2[CANCX_Diagnosis_Histories_v2$Dxs != "-", ]
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month) %>% ungroup()
names(CANCX_Diagnosis_Histories_v2)[2] <- "Onset"
names(CANCX_Diagnosis_Histories_v2)[1] <- "patid"



Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
string_Anamorelin <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$generic_name == "Anamorelin"], collapse = "|"),")\\b")


CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
setDT(CANCX_Drug_Histories_v2)
Anamorelin_Pats <- unique(CANCX_Drug_Histories_v2[grepl(string_Anamorelin, Drugs), .(patid)])
Anamorelin_Pats$Anamorelin <- 1



Anamorelin_Pats <- merge(CANCX_Active_pts_3,  # Active, Cachexia
                                 Anamorelin_Pats, 
                                 by="patid", all.x=TRUE)


Anamorelin_Pats <- merge(Anamorelin_Pats,  
                                 CANCX_Diagnosis_Histories_v2, 
                                 by="patid", all.x=TRUE)


Anamorelin_Pats <- Anamorelin_Pats %>% mutate(Anamorelin=ifelse(is.na(Anamorelin),0,1))


CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2  %>% filter(metastatic_cancer==1) %>% select(patid) 

Anamorelin_Pats <- merge(Anamorelin_Pats,  
                                 CANCX_Demographics_v2, 
                                 by="patid", all=FALSE)



CANCX_BMI_records_v2 <- fread("CANCX_BMI_records_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_BMI_records_v2 <- merge(Anamorelin_Pats[,.(patid)], 
                                 CANCX_BMI_records_v2, 
                                 by="patid", all.x=TRUE)

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2[, .(patid, month, bmi)]

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2[ , .(n = mean(as.numeric(bmi))), by = c("patid", "month")] 
names(CANCX_BMI_records_v2)[2] <- "Month"

CANCX_BMI_records_v2$Month <- as.numeric(CANCX_BMI_records_v2$Month)

CANCX_BMI_records_v2 <- merge(CANCX_BMI_records_v2, 
                                 Anamorelin_Pats, 
                                 by=c("patid"), all.x=TRUE)



CANCX_BMI_records_v2$Month <- CANCX_BMI_records_v2$Month - CANCX_BMI_records_v2$Onset

CANCX_BMI_records_v2 <- na.omit(CANCX_BMI_records_v2)

CANCX_BMI_records_v2 %>% group_by(Month, Anamorelin ) %>% summarise(n=mean(n)) %>% ungroup() %>%
  #filter(Month>(-24)&Month<24) %>%
  mutate(Anamorelin=as.factor(Anamorelin)) %>%
  ggplot(aes(Month, n,colour=Anamorelin, fill=Anamorelin)) +
  #coord_cartesian(ylim=c(15, 25)) +
  geom_smooth( size=2, alpha=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  xlab("\n Elapsed Time (months) to/from 1st Cancer Dx") + ylab("Average Monthly BMI (kg/m2) \n") +
  ggsci::scale_color_nejm() + ggsci::scale_fill_nejm() 




# ----------------------------------------------

# Anamorelin over time -------------------

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer_2] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) # 237000

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl("269", CANCX_Drug_Histories_v2$Drugs), ]


setDT(CANCX_Drug_Histories_v2)

CANCX_Drug_Histories_v2[ , .(group_sum = sum(as.numeric(weight3))), by = Month] 

data.frame(CANCX_Drug_Histories_v2 %>% select(patid, weight3) %>% distinct() %>% left_join(CANCX_Active_pts_3) %>% 
  group_by(primary_cancer_2) %>% summarise(n=sum(as.numeric(weight3))) %>%
  arrange(-n))


CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, metastatic_cancer)

data.frame(CANCX_Drug_Histories_v2 %>% select(patid, weight3) %>% distinct() %>% left_join(CANCX_Demographics_v2) %>% 
  group_by(metastatic_cancer) %>% summarise(n=sum(as.numeric(weight3))) %>%
  arrange(-n))



# --------------

# Psycholeptics over time -------------------

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer_2] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) # 174066


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
unique(Drug_formulary$drug_class)
string_Psycholeptics <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Psycholeptics"], collapse = "|"),")\\b")
Drug_formulary <- Drug_formulary[, c("drug_id", "generic_name")]

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl(string_Psycholeptics, CANCX_Drug_Histories_v2$Drugs), ]

setDT(CANCX_Drug_Histories_v2)
names(CANCX_Drug_Histories_v2)[4] <- "drug_id"

CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, drug_id, sep = ",", convert=T)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl(string_Psycholeptics, CANCX_Drug_Histories_v2$drug_id), ]

CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 Drug_formulary, 
                                 by="drug_id", all=FALSE)


temp <- data.frame(CANCX_Drug_Histories_v2 %>% group_by(Month, generic_name) %>% summarise(n=sum(as.numeric(weight3))) %>% ungroup() %>%
  spread(key=generic_name, value=n))

fwrite(temp, "temp.csv")

# --------------------

# Psycholeptics over time relative to cancer onset -------------------

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer_2] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) # 237000


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
unique(Drug_formulary$drug_class)
string_Psycholeptics <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Psycholeptics"], collapse = "|"),")\\b")
Drug_formulary <- Drug_formulary[, c("drug_id", "generic_name")]

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl(string_Psycholeptics, CANCX_Drug_Histories_v2$Drugs), ]

setDT(CANCX_Drug_Histories_v2)
names(CANCX_Drug_Histories_v2)[4] <- "drug_id"

CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, drug_id, sep = ",", convert=T)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl(string_Psycholeptics, CANCX_Drug_Histories_v2$drug_id), ]

CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 Drug_formulary, 
                                 by="drug_id", all=FALSE)

length(unique(CANCX_Drug_Histories_v2$Month))

CANCX_Diagnosis_Histories_v2 <- fread("CANCX_Diagnosis_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Diagnosis_Histories_v2 <- gather(CANCX_Diagnosis_Histories_v2, Month, Dxs, month1:month74, factor_key=TRUE)
CANCX_Diagnosis_Histories_v2$Month <-  parse_number(as.character(CANCX_Diagnosis_Histories_v2$Month))
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2[CANCX_Diagnosis_Histories_v2$Dxs != "-", ]
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% filter(Month>12&Month<=72)
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month) %>% ungroup()
names(CANCX_Diagnosis_Histories_v2)[2] <- "Onset"
CANCX_Diagnosis_Histories_v2$Onset <- CANCX_Diagnosis_Histories_v2$Onset-12

#CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2[ CANCX_Diagnosis_Histories_v2$Onset>30, ]

length(unique(CANCX_Diagnosis_Histories_v2$Onset))
range(CANCX_Diagnosis_Histories_v2$Onset)


CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% inner_join(CANCX_Drug_Histories_v2) %>% mutate(Month=Month-Onset)

temp <- data.frame(CANCX_Diagnosis_Histories_v2 %>% group_by(Month, generic_name) %>% summarise(n=sum(as.numeric(weight3))) %>% ungroup() %>%
  spread(key=generic_name, value=n))

fwrite(temp, "temp.csv")

# ------------------
# Adjusted persistency -----------------------

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



CANCX_Active_pts_2 <- fread("CANCX_Active_pts_2.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_2$weight2)) # 1004964, OK
CancerType_Size <- CANCX_Active_pts_2[ , .(group_sum = sum(as.numeric(weight2))), by = primary_cancer] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_2[, .(patid, weight2)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(Drugs!="-")
names(CANCX_Drug_Histories_v2)[1] <- "patient"


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

string_Anamorelin <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Anamorelin"], collapse = "|"),")\\b")
string_Antiemetic <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Antiemetic"], collapse = "|"),")\\b")
string_Platinum <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Platinum agent"], collapse = "|"),")\\b")
string_Surgery <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Surgery"], collapse = "|"),")\\b")


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(grepl(string_Anamorelin,Drugs)) 


CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, death_month_dd)
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% drop_na()
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% filter(death_month_dd!="")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% mutate(death_month_dd=format(as.Date(death_month_dd), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% left_join(Months_lookup, by=c("death_month_dd"="Month")) %>% select(patid, Exact_Month)

names(CANCX_Demographics_v2)[1] <- "patient"
names(CANCX_Demographics_v2)[2] <- "Death_Date"


trial <- CANCX_Drug_Histories_v2 %>% select(patient, weight2) %>% distinct() %>% 
  left_join(
  CANCX_Drug_Histories_v2 %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CANCX_Drug_Histories_v2 %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight2) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(CANCX_Demographics_v2) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1


# 
# Cachexia_identified_pts_2 <- fread("Cachexia_identified_pts_2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
# Cachexia_identified_pts_2 <- Cachexia_identified_pts_2[, .(patid, cachexia_id_type)]
# Cachexia_identified_pts_2$cachexia_id_type <- 1
# names(Cachexia_identified_pts_2)[1] <- "patient"
#  
CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, metastatic_cancer)
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% drop_na()
names(CANCX_Demographics_v2)[1] <- "patient"

trial <- trial %>% left_join(CANCX_Demographics_v2)

names(trial)[4] <- "dis"

fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)


data.frame(fittrial$est)[1:2,]
transpose(data.frame(fittrial$ci)[,1:120])

trial_transformed <- transpose(data.frame(fittrial$est)[1:2,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "None"
names(trial_transformed)[2] <- "Metastatic"

trial <- trial_transformed %>% gather(Group, Prop, None:Metastatic) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,1:120]))

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

# Total Persistency -----------------
CANCX_Active_pts_2 <- fread("CANCX_Active_pts_2.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_2$weight2)) # 1004964, OK
CancerType_Size <- CANCX_Active_pts_2[ , .(group_sum = sum(as.numeric(weight2))), by = primary_cancer] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_2[, .(patid, weight2)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(Drugs!="-")
names(CANCX_Drug_Histories_v2)[1] <- "patient"


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

unique(Drug_formulary$drug_class)
Drug_formulary <- Drug_formulary[ Drug_formulary$drug_class %in% c("Antiemetic", "Chemoprotective", "Corticosteroid", "Psycholeptics", "Progestin", "Appetite Stimulant", "Anamorelin"), ]
Drug_formulary <- Drug_formulary[, c("drug_id", "generic_name", "drug_class")]


CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)
names(CANCX_Drug_Histories_v2)[4] <- "drug_id"

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% left_join(Drug_formulary)
CANCX_Drug_Histories_v2 <- na.omit(CANCX_Drug_Histories_v2)


# CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
# CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, death_month_dd ,  metastatic_cancer)
# CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% mutate(death_month_dd=ifelse(death_month_dd=="",0,1))
# names(CANCX_Demographics_v2)[1] <- "patient"
# 
# CANCX_Drug_Histories_v2 %>% select(patient, weight2) %>% distinct() %>% left_join(CANCX_Demographics_v2) %>%
#   group_by(death_month_dd, metastatic_cancer) %>% summarise(n=sum(as.numeric(weight2))) 
# 
# 
# data.frame(CANCX_Drug_Histories_v2 %>% select(patient, weight2) %>% group_by(patient, weight2) %>% count() %>% left_join(CANCX_Demographics_v2) %>%
#   group_by(death_month_dd, metastatic_cancer, n) %>% summarise(n2=sum(as.numeric(weight2)))) 

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% group_by(patient, weight2, drug_class, generic_name)  %>% count()

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% ungroup() %>% group_by(drug_class, generic_name, n) %>% summarise(pop=sum(as.numeric(weight2)))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% ungroup() %>% spread(key=n, value=pop)

fwrite(CANCX_Drug_Histories_v2, "temp.csv")


# -------------------------
# Psycholeptics ever tried by cancer type ---------------

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer_2] 


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) #

CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)
names(CANCX_Drug_Histories_v2)[4] <- "drug_id"

CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 Drug_formulary[, .(drug_id, drug_class)], 
                                 by="drug_id", all.x=TRUE)

length(unique(CANCX_Drug_Histories_v2$patid))
unique(CANCX_Drug_Histories_v2$drug_class)
  
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(drug_class=="Psycholeptics")

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% left_join(Drug_formulary %>% select(drug_id, generic_name)) %>% select(-drug_class)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% select(-c(Month, drug_id)) %>% distinct()

CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 CANCX_Active_pts_3, 
                                 by=c("patid", "weight3"), all.x=TRUE)


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% group_by(primary_cancer_2, generic_name) %>% summarise(sum=sum(as.numeric(weight3)))

temp <- merge(CancerType_Size, CANCX_Drug_Histories_v2, by=c("primary_cancer_2"), all.x=TRUE)

temp$sum <- temp$sum / temp$group_sum

temp <- temp %>% spread(key=generic_name, value=sum)

fwrite(temp, "temp.csv")
# -------------
# Psycholeptics over time relative to cachexia onset -------------------

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) # 174066


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
unique(Drug_formulary$drug_class)
string_Psycholeptics <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Psycholeptics"], collapse = "|"),")\\b")
Drug_formulary <- Drug_formulary[, c("drug_id", "generic_name")]

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl(string_Psycholeptics, CANCX_Drug_Histories_v2$Drugs), ]

setDT(CANCX_Drug_Histories_v2)
names(CANCX_Drug_Histories_v2)[4] <- "drug_id"

CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, drug_id, sep = ",", convert=T)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl(string_Psycholeptics, CANCX_Drug_Histories_v2$drug_id), ]

CANCX_Drug_Histories_v2 <- merge(CANCX_Drug_Histories_v2, 
                                 Drug_formulary, 
                                 by="drug_id", all=FALSE)

length(unique(CANCX_Drug_Histories_v2$Month))


CANCX_Diagnosis_Histories_v2 <- fread("CANCX_Diagnosis_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Diagnosis_Histories_v2 <- gather(CANCX_Diagnosis_Histories_v2, Month, Dxs, month1:month74, factor_key=TRUE)
CANCX_Diagnosis_Histories_v2$Month <-  parse_number(as.character(CANCX_Diagnosis_Histories_v2$Month))
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2[CANCX_Diagnosis_Histories_v2$Dxs != "-", ]
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% filter(Month>12&Month<=72)

CANCX_Diagnosis_Histories_v2 <- separate_rows(CANCX_Diagnosis_Histories_v2, Dxs, sep = ",", convert=T)
CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% filter(!grepl("C", Dxs))

unique(CANCX_Diagnosis_Histories_v2$Dxs)

CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month) %>% ungroup() %>% distinct()
names(CANCX_Diagnosis_Histories_v2)[2] <- "Onset"
CANCX_Diagnosis_Histories_v2$Onset <- CANCX_Diagnosis_Histories_v2$Onset-12

#CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2[ CANCX_Diagnosis_Histories_v2$Onset>30, ]


length(unique(CANCX_Diagnosis_Histories_v2$Onset))
range(CANCX_Diagnosis_Histories_v2$Onset)


CANCX_Diagnosis_Histories_v2 <- CANCX_Diagnosis_Histories_v2 %>% inner_join(CANCX_Drug_Histories_v2) %>% mutate(Month=Month-Onset)
CANCX_Diagnosis_Histories_v2 <- distinct(CANCX_Diagnosis_Histories_v2)

temp <- data.frame(CANCX_Diagnosis_Histories_v2 %>% group_by(Month, generic_name) %>% summarise(n=sum(as.numeric(weight3))) %>% ungroup() %>%
  spread(key=generic_name, value=n))

fwrite(temp, "temp.csv")

# ----------------

# Psycholeptics with Fs -----------------------
Comorbidities_ICD10_F <- fread("Comorbidities_ICD10_F.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")

length(unique(Comorbidities_ICD10_F$patid))


CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) 



Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
unique(Drug_formulary$drug_class)
string_Psycholeptics <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Psycholeptics"], collapse = "|"),")\\b")
Drug_formulary <- Drug_formulary[, c("drug_id", "generic_name")]

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[grepl(string_Psycholeptics, CANCX_Drug_Histories_v2$Drugs), ]

CANCX_Drug_Histories_v2 %>% select(patid, weight3) %>% distinct() %>% summarise(n=sum(as.numeric(weight3)))


CANCX_Drug_Histories_v2 %>% filter(grepl("255", Drugs)) %>% select(patid, weight3) %>% distinct() %>% summarise(n=sum(as.numeric(weight3))) # 252322


CANCX_Drug_Histories_v2 %>% filter(grepl("255", Drugs))  %>% select(patid, weight3) %>% distinct() %>% 
  inner_join(Comorbidities_ICD10_F %>% filter(grepl("F20", ICD10CODE)|grepl("F31", ICD10CODE)) %>% select(patid) %>% distinct()) %>%
  summarise(n=sum(as.numeric(weight3))) # 252322

# ------------------------
# BMI evolution before/after each class initiation (antiemetic, anticachexia) ----------------------------------------


CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer] 


# Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
# Cachexia_identified_pts_3 <- Cachexia_identified_pts_3[, .(patid)]
# 
# CANCX_Active_pts_3 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
#                                  Cachexia_identified_pts_3, 
#                                  by="patid", all=FALSE)


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

string_to_track <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$indication == "Cachexia" | Drug_formulary$indication == "Nausea"], collapse = "|"),")\\b")

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
setDT(CANCX_Drug_Histories_v2)

To_track_Pats <- unique(CANCX_Drug_Histories_v2[grepl(string_to_track, Drugs), .(patid)])
To_track_Pats$To_track <- 1


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(grepl(string_to_track, Drugs))
CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(grepl(string_to_track, Drugs))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% left_join(Drug_formulary %>% select(drug_id, drug_class), by=c("Drugs"="drug_id")) %>% 
  select(patid, Month, drug_class) %>% distinct() %>% group_by(patid, drug_class) %>% filter(Month==min(Month)) %>% slice(1) %>% rename("Start"="Month")


To_track_Pats <- To_track_Pats %>% left_join(CANCX_Drug_Histories_v2)


To_track_Pats <- merge(CANCX_Active_pts_3, To_track_Pats,  by="patid", all.x=TRUE)

sum(is.na(To_track_Pats$To_track))
sum(is.na(To_track_Pats$Start))

To_track_Pats <- na.omit(To_track_Pats)


CANCX_BMI_records_v2 <- fread("CANCX_BMI_records_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_BMI_records_v2 <- merge(To_track_Pats[,.(patid)], CANCX_BMI_records_v2,  by="patid", all.x=TRUE)

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2[, .(patid, month, bmi)]

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2[ , .(n = mean(as.numeric(bmi))), by = c("patid", "month")] 

names(CANCX_BMI_records_v2)[2] <- "Month"

CANCX_BMI_records_v2$Month <- as.numeric(CANCX_BMI_records_v2$Month)

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% left_join(To_track_Pats, by=c("patid"="patid"))
  
CANCX_BMI_records_v2$Month <- CANCX_BMI_records_v2$Month - CANCX_BMI_records_v2$Start

CANCX_BMI_records_v2 <- na.omit(CANCX_BMI_records_v2)


CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2  %>% filter(death==0) %>% select(patid) 

CANCX_BMI_records_v2 %>% # inner_join(CANCX_Demographics_v2) %>%
  group_by(Month, drug_class) %>% summarise(n=mean(n)) %>% ungroup() %>%
  mutate(drug_class=as.factor(drug_class)) %>%
  ggplot(aes(Month, n,colour=drug_class, fill=drug_class)) +
  facet_wrap(~drug_class) +
  geom_smooth( size=2, alpha=0.5) +
  coord_cartesian(xlim=c(-36, 36), ylim=c(15, 30)) +
  #coord_cartesian(ylim=c(15, 30)) +
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  xlab("\n Elapsed Time (months) to/from 1st Month of Class Initiation") + 
  ylab("Average Monthly BMI (kg/m2) \n") +
  ggsci::scale_color_nejm() + 
  ggsci::scale_fill_nejm() 


# -------------------------


# BMI change in each Anamorelin episode ----------------------------------------


CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer] 


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

string_to_track <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class  == "Anamorelin" ], collapse = "|"),")\\b")

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)


CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <- as.character(CANCX_Drug_Histories_v2$Month)
CANCX_Drug_Histories_v2$Month <- parse_number(CANCX_Drug_Histories_v2$Month)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% mutate(ON = ifelse(grepl(string_to_track, Drugs), 1, 0))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% group_by(patid) %>% mutate(grp = rle(ON)$lengths %>% {rep(seq(length(.)), .)})

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(ON==1)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% select(patid, weight3, Month, ON, grp)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% select(-ON) %>% group_by(patid, weight3, grp) %>% filter(Month==min(Month)) %>% rename("Start"="Month") %>%
  left_join(CANCX_Drug_Histories_v2 %>% select(-ON) %>% group_by(patid, weight3, grp) %>% filter(Month==max(Month)) %>% rename("End"="Month"))



CANCX_BMI_records_v2 <- fread("CANCX_BMI_records_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% inner_join(CANCX_Drug_Histories_v2 %>% ungroup() %>% select(patid) %>% distinct())

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2[, .(patid, month, bmi)]

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2[ , .(n = mean(as.numeric(bmi))), by = c("patid", "month")] 

names(CANCX_BMI_records_v2)[2] <- "Month"

names(CANCX_BMI_records_v2)[3] <- "BMI"

CANCX_BMI_records_v2$Month <- as.numeric(CANCX_BMI_records_v2$Month)

data.frame(CANCX_Drug_Histories_v2 %>% ungroup() %>% select(-grp) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "Start"="Month")) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "End"="Month")) %>%
  drop_na() %>% mutate(Elapsed=End-Start, Change=100*(BMI.y-BMI.x)/BMI.x )) %>%
  mutate(colour=as.factor(ifelse(Change>0, 1, 2))) %>%
  ggplot(aes(Elapsed, Change, colour=colour, shape=colour)) +
  geom_jitter(alpha=0.6, height=0.4, size=2) +
  theme_minimal() +
    ggsci::scale_color_nejm() +
  xlab("\n Epsiode Duration \n (No. Months ON Anamorelin)") +
  ylab("% BMI Change from Start to End of Anamorelin \n (per episode)")




data.frame(CANCX_Drug_Histories_v2 %>% ungroup() %>% select(-grp) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "Start"="Month")) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "End"="Month")) %>%
  drop_na() %>% mutate(Elapsed=End-Start, Change=100*(BMI.y-BMI.x)/BMI.x )) %>%
  mutate(colour=as.factor(ifelse(Change>0, 1, 2)))



data.frame(CANCX_Drug_Histories_v2 %>% ungroup() %>% select(-grp) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "Start"="Month")) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "End"="Month")) %>%
  drop_na() %>% mutate(Elapsed=End-Start, Change=100*(BMI.y-BMI.x)/BMI.x )) %>%
  mutate(colour=as.factor(ifelse(Change>0, 1, 2))) %>% 
  left_join(CANCX_Active_pts_3) %>% group_by(colour, primary_cancer ) %>% count() %>%
  spread(key=colour, n)



data.frame(CANCX_Drug_Histories_v2 %>% ungroup() %>% select(-grp) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "Start"="Month")) %>%
  left_join(CANCX_BMI_records_v2, by=c("patid"="patid", "End"="Month")) %>%
  drop_na() %>% mutate(Elapsed=End-Start, Change=100*(BMI.y-BMI.x)/BMI.x )) %>%
  mutate(colour=as.factor(ifelse(Change>0, 1, 2))) %>%
  group_by(colour) %>% summarise(n=mean(BMI.y))


# -------------------------

# No. Anticancer lines ~ weight drug exp -------------------------------

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer] 

Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

string_to_track <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$indication  == "Cancer" ], collapse = "|"),")\\b")

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)


CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(Drugs!="-")

CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(grepl(string_to_track,Drugs))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% arrange(patid, weight3, Month, Drugs) %>%
  group_by(patid, weight3, Month) %>% mutate(treat_new = paste(Drugs, collapse=",")) 

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% ungroup() %>% select(patid, weight3, Month, treat_new) %>% distinct()

noLines <- CANCX_Drug_Histories_v2

noLines <- noLines %>% spread(key=Month, value=treat_new)

fwrite(noLines, "CANCX_Drugs_Histories_Cancer_Drugs_Only.txt", sep="\t")

temp <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)

temp <- temp %>% filter(!is.na(Drugs)) %>% select(patid, weight3, Drugs) %>% distinct()

temp %>% group_by(patid, weight3) %>% count() %>% ungroup() %>% summarise(mean=weighted.mean(n, as.numeric(weight3)))  # 4.10






CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer] 

Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

string_to_track <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$indication  == "Cachexia" ], collapse = "|"),")\\b")

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)


CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(Drugs!="-")

CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(grepl(string_to_track,Drugs))

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% left_join(Drug_formulary %>% select(drug_id, generic_name), by=c("Drugs"="drug_id")) %>% 
  select(patid, generic_name) %>% distinct()


data.frame(CANCX_Drug_Histories_v2 %>% inner_join(temp) %>% group_by(patid, weight3, generic_name) %>% count() %>% 
  ungroup() %>% group_by(generic_name) %>% summarise(mean=weighted.mean(n, as.numeric(weight3)))) %>%
  arrange(-mean)



CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2  %>%  select(patid, metastatic_cancer) 


data.frame(CANCX_Drug_Histories_v2 %>%  left_join(CANCX_Active_pts_3 %>% select(patid, weight3)) %>%
  left_join(CANCX_Demographics_v2) %>% group_by(generic_name, metastatic_cancer) %>% summarise(n=sum(as.numeric(weight3))) %>%
  spread(key=metastatic_cancer, value=n)) %>% mutate(X1=ifelse(is.na(X1),0,X1)) %>%
  mutate(perc=X1/(X1+X0)) %>% arrange(-perc)

# -------------------------------
# Patient Months ------------------------

noLines <- fread("CANCX_Drugs_Histories_Cancer_Drugs_Only.txt", sep="\t",  integer64 = "character", stringsAsFactors = F, colClasses = "character")

noLines <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
noLines <- noLines %>% filter(Drugs!="")

noLines <- noLines %>% group_by(patid, weight3) %>% count()

Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3[, .(patid)]
Cachexia_identified_pts_3$cachexia <- "cachexia"


noLines <- noLines %>% left_join(Cachexia_identified_pts_3)

sum(as.numeric(noLines$weight3) * noLines$n) # 44181875

noLines <- noLines %>% filter(cachexia=="cachexia")
sum(as.numeric(noLines$weight3) * noLines$n) # 17453942



CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK

data.frame(noLines %>% left_join(CANCX_Active_pts_3) %>% group_by(primary_cancer_2) %>% summarise(n1=sum(as.numeric(weight3)*n)) %>%
  left_join(noLines %>% left_join(CANCX_Active_pts_3) %>% filter(cachexia=="cachexia") %>% group_by(primary_cancer_2) %>% summarise(n2=sum(as.numeric(weight3)*n))))

#V2

noLines <- fread("CANCX_Drugs_Histories_Cancer_Drugs_Only.txt", sep="\t",  integer64 = "character", stringsAsFactors = F, colClasses = "character")

noLines <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
noLines <- noLines %>% filter(Drugs!="")
noLines <- separate_rows(noLines, Drugs, sep = ",", convert=T)

Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

noLines <- noLines %>% left_join(Drug_formulary %>% select(drug_id, drug_class, indication), by=c("Drugs"="drug_id")) %>%
  filter(indication=="Cancer") %>% select(-c(indication, Drugs)) %>% distinct()

noLines <- noLines %>% group_by(patid, weight3, drug_class) %>% count()

Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3[, .(patid)]
Cachexia_identified_pts_3$cachexia <- "cachexia"


noLines <- noLines %>% left_join(Cachexia_identified_pts_3)


data.frame(noLines  %>% group_by(drug_class) %>% summarise(n1=sum(as.numeric(weight3)*n)) %>%
  left_join(noLines %>% filter(cachexia=="cachexia") %>% group_by(drug_class) %>% summarise(n2=sum(as.numeric(weight3)*n))))

# ----------------------------------------




# Waterfall treatment months -  metastatic - pd1pdl1 - top 5----------------------
noLines <- fread("CANCX_Drugs_Histories_Cancer_Drugs_Only.txt", sep="\t",  integer64 = "character", stringsAsFactors = F, colClasses = "character")

noLines <- gather(noLines, Month, Drugs, month1:month60, factor_key=TRUE)
noLines <- noLines %>% filter(Drugs!="")


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
string_PD1 <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "PD1/PDL1"], collapse = "|"),")\\b")


noLines <- noLines %>% filter(grepl(string_PD1, Drugs))

noLines <- noLines %>% group_by(patid, weight3) %>% count()

Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% filter(cachexia_id_type=="Cachexia/Sarcop dx") %>% select(patid)
Cachexia_identified_pts_3$cachexia <- "cachexia"

noLines <- noLines %>% left_join(Cachexia_identified_pts_3)

noLines <- noLines %>% filter(cachexia=="cachexia")
sum(as.numeric(noLines$weight3) * noLines$n) # 15691857


CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Metastatic <- CANCX_Demographics_v2  %>% filter(metastatic_cancer==1) %>% select(patid) 

CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
Top5 <- CANCX_Active_pts_3[primary_cancer_2 %in% c("Breast Cancer", "Lung Cancer", "Pancreatic Cancer", "Prostate Cancer", "Intestinal Cancer", "Colon Cancer"), ]
Top5 <- Top5 %>% select(patid)

noLines %>% ungroup() %>% inner_join(Metastatic) %>% summarise(n2=sum(as.numeric(weight3)*n) / 15691857)
noLines %>% ungroup() %>% inner_join(Top5) %>% summarise(n2=sum(as.numeric(weight3)*n) / 15691857)
noLines %>% ungroup() %>% inner_join(Metastatic) %>% inner_join(Top5) %>% summarise(n2=sum(as.numeric(weight3)*n) / 15691857)

# ---------------
# Adjusted persistency -  cachexia-Dx vs no cachexia Dx --------------------------------

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



CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 1004964, OK
CancerType_Size <- CANCX_Active_pts_3[ , .(group_sum = sum(as.numeric(weight3))), by = primary_cancer_2 ] 

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(Drugs!="-")
names(CANCX_Drug_Histories_v2)[1] <- "patient"


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

unique(Drug_formulary$drug_class)

string_Platinum <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Platinum agent"], collapse = "|"),")\\b")
string_Hormonal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Hormonal Therapy"], collapse = "|"),")\\b")
string_Radiotherapy <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Radiotherapy"], collapse = "|"),")\\b")
string_Chemoprotective <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Chemoprotective"], collapse = "|"),")\\b")
string_Hospital <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Hospital inpatient"], collapse = "|"),")\\b")
string_PD1 <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "PD1/PDL1"], collapse = "|"),")\\b")
string_Topoisomerase <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Topoisomerase Inhibitor"], collapse = "|"),")\\b")
string_Antimicrotubule <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Antimicrotubule Agent"], collapse = "|"),")\\b")
string_Antimetabolites <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Antimetabolites"], collapse = "|"),")\\b")
string_Immuno <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Immuno Targeted"], collapse = "|"),")\\b")
string_Biologic  <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Biologic Therapy"], collapse = "|"),")\\b")


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(grepl(string_Biologic,Drugs)) 


CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, death_month_dd)
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% drop_na()
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% filter(death_month_dd!="")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% mutate(death_month_dd=format(as.Date(death_month_dd), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% left_join(Months_lookup, by=c("death_month_dd"="Month")) %>% select(patid, Exact_Month)

names(CANCX_Demographics_v2)[1] <- "patient"
names(CANCX_Demographics_v2)[2] <- "Death_Date"


trial <- CANCX_Drug_Histories_v2 %>% select(patient, weight3) %>% distinct() %>% 
  left_join(
  CANCX_Drug_Histories_v2 %>% select(patient) %>% group_by(patient) %>% count()
)  %>% ungroup() %>% 
  left_join(
    CANCX_Drug_Histories_v2 %>% select(patient, Month) %>% group_by(patient) %>% summarise(Max=max(Month))
    ) %>% select(-weight3) %>%
  mutate(status=ifelse(Max==60,0,2)) %>%
  left_join(CANCX_Demographics_v2) %>%
  mutate(Death_Date=ifelse(is.na(Death_Date),999,Death_Date)) %>%
  mutate(status2=ifelse(Death_Date==Max|Death_Date==Max+1|Death_Date==Max+2,1,status)) %>%
  select(patient, n, status2)
  
trial$n <- trial$n+1



Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3[, .(patid, cachexia_id_type)]
unique(Cachexia_identified_pts_3$cachexia_id_type)
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% filter(cachexia_id_type=="Cachexia/Sarcop dx")

Cachexia_identified_pts_3$cachexia_id_type <- 1
names(Cachexia_identified_pts_3)[1] <- "patient"

# CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
# CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, metastatic_cancer)
# CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% drop_na()
# names(CANCX_Demographics_v2)[1] <- "patient"

trial <- trial %>% left_join(Cachexia_identified_pts_3)

names(trial)[4] <- "dis"

trial[is.na(trial)] <- 0


fittrial=CumIncidence(trial$n, trial$status2, trial$dis, cencode = 0, xlab="Months", t=c(1:60), level = 0.95)


data.frame(fittrial$est)[1:2,]
transpose(data.frame(fittrial$ci)[,1:120])

trial_transformed <- transpose(data.frame(fittrial$est)[1:2,])
trial_transformed$V1 <-  trial_transformed$V1
trial_transformed$V2 <-  trial_transformed$V2
names(trial_transformed)[1] <- "No"
names(trial_transformed)[2] <- "Cachexia_Dx"

trial <- trial_transformed %>% gather(Group, Prop, No:Cachexia_Dx) %>% 
  bind_cols(transpose(data.frame(fittrial$ci)[,1:120]))

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


# -----------------------
# XGBoost for anamorelin users -----------------------

# 269


CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
length(unique(CANCX_Drug_Histories_v2$patid)) # 237000
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% select(-Month) %>% distinct()
CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% select(patid, Drugs) %>% distinct() %>% left_join(CANCX_Active_pts_3 %>% select(patid, primary_cancer_2 )) 
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% mutate(Drugs=paste0("D", Drugs)) %>% mutate(Exp=1) %>% spread(key=Drugs, value=Exp)
CANCX_Drug_Histories_v2[is.na(CANCX_Drug_Histories_v2)] <- 0


Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3[, .(patid, cachexia_id_type)]
unique(Cachexia_identified_pts_3$cachexia_id_type)
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% filter(cachexia_id_type=="Cachexia/Sarcop dx")
Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% select(patid) %>% left_join(CANCX_Drug_Histories_v2)
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2  %>% inner_join(Cachexia_identified_pts_3 %>% select(patid) %>% distinct())
CANCX_Drug_Histories_v2$primary_cancer_2 <- str_remove_all(CANCX_Drug_Histories_v2$primary_cancer_2, " Cancer")
CANCX_Drug_Histories_v2$Exp <- 1
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% spread(key=primary_cancer_2, value=Exp)
CANCX_Drug_Histories_v2[is.na(CANCX_Drug_Histories_v2)] <- 0


CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, age , gender, metastatic_cancer) %>% mutate(gender=ifelse(gender=="male",1,0))
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% left_join(CANCX_Demographics_v2)


CANCX_BMI_records_v2 <- fread("CANCX_BMI_records_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% inner_join(CANCX_Drug_Histories_v2 %>% select(patid)) %>% group_by(patid) %>%
  mutate(max=max(bmi), min=min(bmi)) %>% select(patid, max, min) %>% distinct()

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% ungroup() %>% mutate(drop=100*(as.numeric(max)-as.numeric(min))/as.numeric(max)) 

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% inner_join(CANCX_BMI_records_v2)

temp <- CANCX_Drug_Histories_v2 %>% select(-patid)


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")

library("randomForest")
library("DALEX")


temp$age <- as.numeric(temp$age)
temp$drop <- as.numeric(temp$drop)
temp$min <- as.numeric(temp$min)
temp$max <- as.numeric(temp$max)

modelAll_1_randomForest <- randomForest(D269 ~ ., data = temp)

summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)



data.frame(predict(modelAll_1_randomForest, temp[,-175])) %>% arrange()

explain_rf <- DALEX::explain(model = modelAll_1_randomForest,  
                        data = temp[, -175],
                           y = temp$D269 == 1, 
                       label = "Random Forest")

bd_rf <- predict_parts(explainer = explain_rf,
                 new_observation = temp[516, ],
                            type = "break_down")

plot(bd_rf,  max_features = 25)

temp %>% group_by(D269) %>% summarise(n=mean(as.numeric(metastatic_cancer)))

library(tidyverse)
library(xgboost)
library(caret)

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
              size = 3, alpha = 0.7,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="#FFCC33", high="#6600CC", 
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

temp$metastatic_cancer <- as.numeric(temp$metastatic_cancer)


model_hd = xgboost(data = as.matrix(temp[,-175]),
                   nround = 100,
                   objective = "binary:logistic",
                   label=as.matrix(temp[,175]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(temp[,-175]),
                              shap_approx = F)


var_importance(shap_result, top_n=25)


shap_long_hd = shap.prep(X_train = as.matrix(temp[,-175]) , top_n = 25)

plot.shap.summary(data_long = shap_long_hd)



temp %>% group_by(D269) %>% summarise(n=mean(drop))


temp %>% group_by(D269, metastatic_cancer) %>% count() %>% 
  spread(key=metastatic_cancer, value=n) %>%
  mutate(perc=`1`/(`1`+`0`))


temp %>% group_by(D269, Lung) %>% count() %>% 
  spread(key=Lung, value=n) %>%
  mutate(perc=`1`/(`1`+`0`))


temp %>% group_by(D269, Pancreatic) %>% count() %>% 
  spread(key=Pancreatic, value=n) %>%
  mutate(perc=`1`/(`1`+`0`))


# -------------------
# Anamorelin patients breakdown ----------------------
Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

Cachexia_identified_pts_3 <- separate_rows(Cachexia_identified_pts_3, cachexia_ids   , sep = "/", convert=T)

Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% mutate(type=ifelse(grepl("1:", cachexia_ids), 1,
                                                                              ifelse(grepl("2:", cachexia_ids), 2,
                                                                                     ifelse(grepl("3:", cachexia_ids), 3,
                                                                                            ifelse(grepl("4:", cachexia_ids), 4, NA)))))


#4: Dx, 3: 10% in 12m, 2: 5% in 6m, 1: 2% BMI<20

Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% select(patid, weight3, type) %>% distinct()



CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_3$weight3)) # 3452283, OK


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
string_Anamorelin <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$generic_name == "Anamorelin"], collapse = "|"),")\\b")


CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% inner_join(Cachexia_identified_pts_3 %>% select(patid) %>% distinct())
CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
setDT(CANCX_Drug_Histories_v2)
Anamorelin_Pats <- unique(CANCX_Drug_Histories_v2[grepl(string_Anamorelin, Drugs), .(patid)])
Anamorelin_Pats$Anamorelin <- 1


Cachexia_identified_pts_3 %>% left_join(Anamorelin_Pats) %>% mutate(Anamorelin=ifelse(is.na(Anamorelin), 0, Anamorelin)) %>%
  group_by(type, Anamorelin) %>% summarise(n=sum(as.numeric(weight3))) %>%
  spread(key=Anamorelin, value=n) %>% mutate(perc=`1`/(`1`+`0`))

#    type     `0`    `1`   perc
# 1     1 718543. 20120. 0.0272
# 2     2 910712. 21509. 0.0231
# 3     3 594023. 18046. 0.0295
# 4     4  14265. 10524. 0.425 


Anamorelin_Pats %>% left_join(CANCX_Active_pts_3 %>% select(patid, weight3)) %>%
  summarise(n=sum(as.numeric(weight3)))


Anamorelin_Pats %>% left_join(Cachexia_identified_pts_3) %>% 
  select(patid, type, weight3) %>% distinct() %>%
  group_by(type) %>% summarise(n=sum(as.numeric(weight3))/34746.51)

# 1     1 0.579
# 2     2 0.619
# 3     3 0.519
# 4     4 0.303



# -----------------------
# Anamorelin vs other cachexia Dx ---------------
Cachexia_identified_pts_3 <- fread("Cachexia_identified_pts_3.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")


Cachexia_identified_pts_3 <- separate_rows(Cachexia_identified_pts_3, cachexia_ids   , sep = "/", convert=T)

Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% mutate(type=ifelse(grepl("1:", cachexia_ids), 1,
                                                                              ifelse(grepl("2:", cachexia_ids), 2,
                                                                                     ifelse(grepl("3:", cachexia_ids), 3,
                                                                                            ifelse(grepl("4:", cachexia_ids), 4, NA)))))


#4: Dx, 3: 10% in 12m, 2: 5% in 6m, 1: 2% BMI<20

Cachexia_identified_pts_3 <- Cachexia_identified_pts_3 %>% select(patid, weight3, type) %>% distinct()

Cachexia_identified_pts_3 %>% select(patid, weight3) %>% distinct() %>% summarise(n=sum(as.numeric(weight3)))



CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Active_pts_3 <- CANCX_Active_pts_3 %>% filter(primary_cancer_2!="Other Cancer"&primary_cancer_2!="Head Cancer"&
                                                    primary_cancer_2!="Unspecified Cancer"&primary_cancer_2!="Unknown"&
                                                    primary_cancer_2!="Salivary Cancer"&primary_cancer_2!="Lymphoma Cancer"&
                                                    primary_cancer_2!="Myeloma Cancer"&primary_cancer_2!="Leukemia Cancer") %>%
  select(patid, weight3, primary_cancer_2)

unique(CANCX_Active_pts_3$primary_cancer_2)



CANCX_BMI_records_v2 <- fread("CANCX_BMI_records_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
BMI3plus <- CANCX_BMI_records_v2 %>% group_by(patid) %>% count() %>% filter(n>=3) %>% select(patid) %>% distinct()


Cachexia_identified_pts_3 %>% filter(type=="4") %>%
  select(patid) %>% distinct() %>%
  left_join(Cachexia_identified_pts_3) %>%
  inner_join(CANCX_Active_pts_3) %>%
  select(patid, weight3) %>% distinct() %>% inner_join(BMI3plus) %>% summarise(n=sum(as.numeric(weight3))) # 8569


Cachexia_identified_pts_3 %>% filter(type=="4") %>%
  select(patid) %>% distinct() %>%
  left_join(Cachexia_identified_pts_3) %>%
  filter(type!="4") %>%
    inner_join(CANCX_Active_pts_3) %>%
  select(patid, weight3, type) %>% distinct() %>% inner_join(BMI3plus) %>% group_by(type) %>% summarise(n=sum(as.numeric(weight3))) #



Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)
string_Anamorelin <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$generic_name == "Anamorelin"], collapse = "|"),")\\b")


CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% inner_join(Cachexia_identified_pts_3 %>% select(patid) %>% distinct())
CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2[CANCX_Drug_Histories_v2$Drugs != "-", ]
setDT(CANCX_Drug_Histories_v2)
Anamorelin_Pats <- unique(CANCX_Drug_Histories_v2[grepl(string_Anamorelin, Drugs), .(patid)])



Cachexia_identified_pts_3 %>% filter(type=="4") %>%
  select(patid) %>% distinct() %>%
  left_join(Cachexia_identified_pts_3) %>%
  filter(type!="4") %>%
    inner_join(CANCX_Active_pts_3) %>%
  inner_join(Anamorelin_Pats) %>%
  select(patid, weight3, type) %>% distinct() %>% inner_join(BMI3plus) %>% group_by(type) %>%  summarise(n=sum(as.numeric(weight3))) #

# 1     1 2434.
# 2     2 2438.
# 3     3 2096.




CANCX_BMI_records_v2 <- fread("CANCX_BMI_records_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% inner_join(CANCX_Active_pts_3)
CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% select(patid, month, bmi)

CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% inner_join(Cachexia_identified_pts_3 %>% select(patid)) %>% 
  left_join(Anamorelin_Pats %>% mutate(Anamorelin="Anamorelin"))

Min <- CANCX_BMI_records_v2 %>% group_by(patid) %>%  filter(bmi==min(as.numeric(bmi))) %>% rename("min"="bmi") %>% select(-month) %>% slice(1) %>% ungroup()  
Max <- CANCX_BMI_records_v2 %>% group_by(patid) %>%  filter(bmi==max(as.numeric(bmi))) %>% rename("max"="bmi") %>% select(-month) %>% slice(1) %>% ungroup() 

Min_Max <- Max %>% inner_join(Min) 

Min_Max <- Min_Max %>% mutate(Anamorelin=ifelse(is.na(Anamorelin), "No", "Yes"))

Min_Max %>%
  mutate(max=as.numeric(max), min=as.numeric(min)) %>%
  filter(max<35&min<35) %>% filter(min>15&max>15) %>%
  group_by(Anamorelin) %>% sample_n(1400) %>% ungroup() %>%
  ggplot(aes(max, min)) +
  geom_hex(bins = 50,alpha=0.8) +
  xlim(15,35) + ylim(15,35) +
  theme_minimal() +
  facet_wrap(~Anamorelin) +
  geom_abline(slope=1, intercept=0) +
  scale_fill_continuous(type = "viridis") +
  xlab("\n Baseline BMI") + ylab("Minimum BMI reached \n")


Min_Max <- Min_Max %>% inner_join(BMI3plus)

Min_Max %>% mutate(min=ifelse(min<17.5, "<17.5",
                              ifelse(min<20, "<20", 
                                     ifelse(min<22.5, "<22.5",
                                            ifelse(min<25, "<25", ">25"))))) %>%
  mutate(max=ifelse(max<17.5, "<17.5",
                              ifelse(max<20, "<20", 
                                     ifelse(max<22.5, "<22.5",
                                            ifelse(max<25, "<25", ">25"))))) %>%
  group_by(Anamorelin, min, max) %>% count() %>% spread(key=Anamorelin, value=n)



# ---------
# Total Persistency based on BMI drop -----------------
CANCX_Active_pts_2 <- fread("CANCX_Active_pts_2.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")
sum(as.numeric(CANCX_Active_pts_2$weight2)) # 1004964, OK
CancerType_Size <- CANCX_Active_pts_2[ , .(group_sum = sum(as.numeric(weight2))), by = primary_cancer] 
CANCX_Active_pts_2 <- CANCX_Active_pts_2 %>% filter(primary_cancer=="Gastroesophageal Cancer")
unique(CANCX_Active_pts_2$primary_cancer)
# Lung, Gastro, Colon, Breast, Prostate

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_2[, .(patid, weight2)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <-  parse_number(as.character(CANCX_Drug_Histories_v2$Month))


CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(Drugs!="-")
names(CANCX_Drug_Histories_v2)[1] <- "patient"


Drug_formulary <- read_xlsx("Drug formulary.xlsx", sheet = "Drug_formulary", col_types = "text")
Drug_formulary$drug_id <- as.numeric(Drug_formulary$drug_id)
setDT(Drug_formulary)

Drug_formulary <- Drug_formulary[ Drug_formulary$drug_class %in% c("Other Antineoplastics", "Alkylating Agent", "Antimetabolites", 
                                                                   "Antimicrotubule Agent", "Hormonal Therapy", "Topoisomerase Inhibitor",
                                                                   "Platinum agent", "Biologic Therapy", "PD1/PDL1" , "Radiotherapy",
                                                                   "Immuno Targeted"), ]
Drug_formulary <- Drug_formulary[, c("drug_id",  "drug_class")]


CANCX_Drug_Histories_v2 <- separate_rows(CANCX_Drug_Histories_v2, Drugs, sep = ",", convert=T)
names(CANCX_Drug_Histories_v2)[4] <- "drug_id"

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% left_join(Drug_formulary)
CANCX_Drug_Histories_v2 <- na.omit(CANCX_Drug_Histories_v2)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% group_by(patient, drug_class)  %>% count()



CANCX_BMI_records_v2 <- fread("CANCX_BMI_records_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_BMI_records_v2 <- CANCX_BMI_records_v2 %>% select(patid, bmi)
Min <- CANCX_BMI_records_v2 %>% group_by(patid) %>%  filter(bmi==min(as.numeric(bmi))) %>% rename("min"="bmi")  %>% slice(1) %>% ungroup()  
Max <- CANCX_BMI_records_v2 %>% group_by(patid) %>%  filter(bmi==max(as.numeric(bmi))) %>% rename("max"="bmi")  %>% slice(1) %>% ungroup() 
Min_Max <- Max %>% inner_join(Min) 
Min_Max <- Min_Max %>% mutate(Drop=100*(as.numeric(max)-as.numeric(min))/as.numeric(max)) 
Min_Max <- Min_Max %>% select(patid, Drop) %>% inner_join(CANCX_Drug_Histories_v2, by=c("patid"="patient"))

Min_Max %>%
  ggplot(aes(Drop, n)) +
  geom_smooth() + 
  xlim(0,20) +
  facet_wrap(~drug_class, scales="free") +
  theme_minimal() + 
  xlab("\n Max % BMI Drop") + ylab("No. Months ON Drug \n")


# -------------------
# Survival after anamorelin ------------------

CANCX_Demographics_v2 <- fread("CANCX_Demographics_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% select(patid, death_month_dd)
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% drop_na()
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% filter(death_month_dd!="")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% mutate(death_month_dd=format(as.Date(death_month_dd), "%Y-%m"))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
CANCX_Demographics_v2 <- CANCX_Demographics_v2 %>% left_join(Months_lookup, by=c("death_month_dd"="Month")) %>% select(patid, Exact_Month)

names(CANCX_Demographics_v2)[1] <- "patient"
names(CANCX_Demographics_v2)[2] <- "Death_Date"




CANCX_Active_pts_3 <- fread("CANCX_Active_pts_3.txt", sep=",",integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- fread("CANCX_Drug_Histories_v2.txt", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

CANCX_Drug_Histories_v2 <- merge(CANCX_Active_pts_3[, .(patid, weight3)], 
                                 CANCX_Drug_Histories_v2[, !c("disease", "weight")], 
                                 by="patid", all.x=TRUE)

CANCX_Drug_Histories_v2 <- gather(CANCX_Drug_Histories_v2, Month, Drugs, month1:month60, factor_key=TRUE)
CANCX_Drug_Histories_v2$Month <- as.character(CANCX_Drug_Histories_v2$Month)
CANCX_Drug_Histories_v2$Month <- parse_number(CANCX_Drug_Histories_v2$Month)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% filter(grepl("269", Drugs)) %>% 
  group_by(patid, weight3) %>% filter(Month==min(Month)) %>%
  select(patid, weight3, Month)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% ungroup() %>% 
  left_join(CANCX_Demographics_v2 %>% rename("patid"="patient")) 

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% mutate(Death_Date=ifelse(is.na(Death_Date), 61, Death_Date))
CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% mutate(Lapsed=Death_Date-Month)

CANCX_Drug_Histories_v2 <- CANCX_Drug_Histories_v2 %>% mutate(flag=ifelse(Death_Date==61, 0,1))


mean(CANCX_Drug_Histories_v2$Lapsed) # 7.401344
mean(CANCX_Drug_Histories_v2$Lapsed[CANCX_Drug_Histories_v2$Death_Date!=61]) # 3.478912


s1 <- survfit(Surv(Lapsed, flag) ~ 1, data = CANCX_Drug_Histories_v2)


survfit2(Surv(Lapsed, flag) ~ 1, data = CANCX_Drug_Histories_v2) %>% 
  ggsurvfit() +
  labs(x = "Months",y = "Overall survival probability") + 
  ylim(0,1) +
  theme_minimal() +
  add_confidence_interval() +
  add_risktable()

sfit <- survfit(Surv(Lapsed, flag)~1, data=CANCX_Drug_Histories_v2)
sfit
summary(sfit)

ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("None", "Pred", "Dx"), legend.title="Cachexia Status",  
           palette=c("honeydew4", "midnightblue", "firebrick"), 
           title="Kaplan-Meier Curve for Cancer Survival by Cachexia Status", 
           risk.table.height=0.2)


# ----------------------
