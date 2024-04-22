
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library("readxl")
options(scipen = 999)

# Theme ----
# rstudioapi::addTheme("https://raw.githubusercontent.com/patrickshox/Mojave-Dark-RStudio-Theme/master/Mojave%20Dark.rstheme", apply=TRUE, force=TRUE)
# ----
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
fwrite(pain_med_treatments_CachexiaPts, "Source/pain_med_treatments_CachexiaPts_filledIN.txt")
# ------
# Pain Drug Usage ------

pain_med_treatments_CachexiaPts <- fread("Source/pain_med_treatments_CachexiaPts_filledIN.txt")
pain_med_treatments_CachexiaPts <- pain_med_treatments_CachexiaPts[, c("patid", "generic_name", "drug_group", "from_dt", "days_sup", "qty")]
range(as.Date(pain_med_treatments_CachexiaPts$from_dt))
CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt")
CancerDrug_Experienced$group <- "Exp"

CachexiaPats_ALL_NEW <- fread("Source/CachexiaPats_ALL_NEW.txt")
CachexiaPats_ALL_NEW$cch <- "cch"
pain_pats <- unique(pain_med_treatments_CachexiaPts[, .(patid)])
pain_pats$pain <- "pain"



merge(merge(CachexiaPats_ALL_NEW, CancerDrug_Experienced, by = "patid", all.x = TRUE), 
  pain_pats, by = "patid", all.x = TRUE)[, .(count = .N), by = .(cch, group, pain)]

#    cch group pain count
# 1: cch  <NA> <NA>  5109
# 2: cch  <NA> pain 16255 (76%)
# 3: cch   Exp pain 25908
# 4: cch   Exp <NA>  4698 (85%)


pain_med_treatments_CachexiaPts <- merge(pain_med_treatments_CachexiaPts, CancerDrug_Experienced, by = "patid", all.x = TRUE)
pain_med_treatments_CachexiaPts[, group := ifelse(is.na(group), "no", group)]


pain_med_treatments_CachexiaPts[, .N, by = .(patid, group)][, .(mean = mean(N)), by = group]

#    group     mean
# 1:    no 13.95005
# 2:   Exp 15.06319

pain_med_treatments_CachexiaPts %>%  
  mutate(group=ifelse(group=="no", "Anticancer_Naive", "Anticancer_Treated")) %>%
  group_by(patid, group) %>% count() %>%
  ggplot(aes(n, colour=group, fill=group)) +
  geom_density(alpha=0.3, size=1) +
  xlim(0, 100) +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Pain Scripts") +
  ylab("Patient density \n") +
    scale_fill_manual(values=c(   "#095d7b",  "#6d084d")) +
  scale_colour_manual(values=c( "#095d7b", "#6d084d")) 





pain_med_treatments_CachexiaPts[, .(sum = sum(days_sup, na.rm = TRUE)), by = .(patid, group)][, .(mean = mean(sum)), by = group]

#    group     mean
# 1:    no 325.1099
# 2:   Exp 332.2972


pain_med_treatments_CachexiaPts %>%  
  mutate(group=ifelse(group=="no", "Anticancer_Naive", "Anticancer_Treated")) %>%
  group_by(patid, group) %>% summarise(sum=sum(days_sup)) %>%
  ggplot(aes(sum, colour=group, fill=group)) +
  geom_density(alpha=0.3, size=1) +
  xlim(0, 1000) +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Supply Days") +
  ylab("Patient density \n") +
    scale_fill_manual(values=c(   "#095d7b",  "#6d084d")) +
  scale_colour_manual(values=c( "#095d7b", "#6d084d")) 



# --------