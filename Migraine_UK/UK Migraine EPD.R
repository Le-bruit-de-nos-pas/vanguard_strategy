library(tidyverse)
library(data.table)
options(scipen = 999)

epd_df <- fread("epd_202301.csv", sep=",", integer64 = "character", stringsAsFactors = F, colClasses = "character")

epd_df <- epd_df[ , c("PRACTICE_NAME", "PRACTICE_CODE",  "CHEMICAL_SUBSTANCE_BNF_DESCR", "BNF_DESCRIPTION", "BNF_CHAPTER_PLUS_CODE", 
                              "QUANTITY", "ITEMS", "TOTAL_QUANTITY", "ADQUSAGE", "NIC", "ACTUAL_COST")]

epd_df <- epd_df[ BNF_CHAPTER_PLUS_CODE == "04: Central Nervous System", -c("BNF_CHAPTER_PLUS_CODE")]

epd_df <- epd_df[grepl("iptan|gepant|umab|gotamine|miditan", CHEMICAL_SUBSTANCE_BNF_DESCR)]

epd_df[, CLASS := ifelse(grepl("iptan", CHEMICAL_SUBSTANCE_BNF_DESCR), "Triptan",
                        ifelse(grepl("gepant", CHEMICAL_SUBSTANCE_BNF_DESCR), "Gepant",
                               ifelse(grepl("umab", CHEMICAL_SUBSTANCE_BNF_DESCR), "Umabs", "Other")))]

# Mabs ought to be multiplied by 14x 
epd_df[, TOTAL_QUANTITY := ifelse(CLASS == "Umabs", as.numeric(TOTAL_QUANTITY) * 14, as.numeric(TOTAL_QUANTITY))]


# No. pills per class and per molecule month-over-month

epd_df[ , .(TOTAL_QUANTITY = sum(as.numeric(TOTAL_QUANTITY))), by = c("CLASS")]

epd_df[ , .(TOTAL_QUANTITY = sum(as.numeric(TOTAL_QUANTITY))), by = c("CHEMICAL_SUBSTANCE_BNF_DESCR")]



# No. pills per class / No. total pills (i.e., class share among classes)

epd_df[ , .(TOTAL_QUANTITY = 100*sum(as.numeric(TOTAL_QUANTITY))/sum(as.numeric(epd_df$TOTAL_QUANTITY))), by = c("CLASS")] 

epd_df[ , .(TOTAL_QUANTITY = 100*sum(as.numeric(TOTAL_QUANTITY))/sum(as.numeric(epd_df$TOTAL_QUANTITY))), by = c("CHEMICAL_SUBSTANCE_BNF_DESCR")] 


# No. Practices with each class / No. Practices (i.e., class share among practices)

unique(epd_df[, .(PRACTICE_NAME, CLASS)])[, .N, by = CLASS]

unique(epd_df[, .(PRACTICE_NAME, CLASS)])[, .N, by = CLASS][, .(CLASS, perc = 100 * N / length(unique(epd_df$PRACTICE_NAME)))]

unique(epd_df[, .(PRACTICE_NAME, CHEMICAL_SUBSTANCE_BNF_DESCR)])[, .N, by = CHEMICAL_SUBSTANCE_BNF_DESCR]

unique(epd_df[, .(PRACTICE_NAME, CHEMICAL_SUBSTANCE_BNF_DESCR)])[, .N, by = CHEMICAL_SUBSTANCE_BNF_DESCR][, .(CHEMICAL_SUBSTANCE_BNF_DESCR, perc = 100 * N / length(unique(epd_df$PRACTICE_NAME)))]


# Among practices prescribing Rimegepant, how much does Rimegepant represent of the total No. of pills?

epd_df[, .(TOTAL_CLASS_PILLS = sum(as.numeric(TOTAL_QUANTITY))), by = .(PRACTICE_NAME, CLASS)][
  unique(epd_df[, .(PRACTICE_NAME, TOTAL_PILLS = sum(as.numeric(TOTAL_QUANTITY))), by = "PRACTICE_NAME"]), on="PRACTICE_NAME"][
  , .(CLASS, N = mean(100 * TOTAL_CLASS_PILLS / TOTAL_PILLS)), by = "CLASS"]

