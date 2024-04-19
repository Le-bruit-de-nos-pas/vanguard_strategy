
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library("readxl")
options(scipen = 999)

# rstudioapi::addTheme("https://raw.githubusercontent.com/patrickshox/Mojave-Dark-RStudio-Theme/master/Mojave%20Dark.rstheme", apply=TRUE, force=TRUE)

# Number of Scripts per molecule ------
pain_med_treatments_CachexiaPts <- fread("Source/pain_med_treatments_CachexiaPts.txt")
unique(pain_med_treatments_CachexiaPts$paid_status)
setDT(pain_med_treatments_CachexiaPts)
N_scripts <- pain_med_treatments_CachexiaPts[
  paid_status %in% c("PAID", "P"),.(count = .N), by = generic_name
  ][order(-count)]
N_scripts$perc <- round(N_scripts$count / sum(N_scripts$count),3)
# -----

# Percentage scripts with missing supply days per molecule -----
pain_med_treatments_CachexiaPts <- fread("Source/pain_med_treatments_CachexiaPts.txt")
pain_med_treatments_CachexiaPts$days_sup[is.nan(pain_med_treatments_CachexiaPts$days_sup)] <- NA
denominator_data <- pain_med_treatments_CachexiaPts[paid_status %in% c("PAID", "P"), 
                                                    .(denominator = .N), by = generic_name]
numerator_data <- pain_med_treatments_CachexiaPts[paid_status %in% c("PAID", "P") & is.na(days_sup), 
                                                  .(numerator = .N), by = generic_name]
avail_sup_days <- merge(denominator_data, numerator_data, by = "generic_name", all.x = TRUE)
avail_sup_days[is.na(avail_sup_days)] <- 0
avail_sup_days$perc <- avail_sup_days$numerator / avail_sup_days$denominator
avail_sup_days <- avail_sup_days[order(-perc)]



# -------

# Fill in missing supply days ----
pain_med_treatments_CachexiaPts <- fread("Source/pain_med_treatments_CachexiaPts.txt")
pain_med_treatments_CachexiaPts$days_sup[is.nan(pain_med_treatments_CachexiaPts$days_sup)] <- NA
pain_med_treatments_CachexiaPts$days_sup <- as.numeric(pain_med_treatments_CachexiaPts$days_sup)
pain_med_treatments_CachexiaPts <- pain_med_treatments_CachexiaPts[paid_status %in% c("PAID", "P"), ]
sum(is.na(pain_med_treatments_CachexiaPts$days_sup))
pain_med_treatments_CachexiaPts <- pain_med_treatments_CachexiaPts[!(generic_name %in% c("Alfentanil", "Remifentanil", "Nalbuphine"))]
pain_med_treatments_CachexiaPts <- pain_med_treatments_CachexiaPts %>% group_by(generic_name) %>% mutate(mean=mean(days_sup, na.rm=T))
pain_med_treatments_CachexiaPts <- pain_med_treatments_CachexiaPts %>% mutate(days_sup=ifelse(is.na(days_sup), mean, days_sup))

# --------------------------
