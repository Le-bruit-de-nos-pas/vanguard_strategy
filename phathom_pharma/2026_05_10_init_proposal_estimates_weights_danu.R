library(tidyverse)
library(data.table)

options(scipen = 999)


# DANU Demographics
DANU_Demographics <- fread("DANU Diagnosed Population 3.0/DANU Demographics.txt")

sum(DANU_Demographics$weight) # 163861371

# DIA Comorbs per year

DIA_Comorbidity_Annual_Inventories <- fread("DANU Comorbidity Annual Inventories/DIA Comorbidity Annual Inventories.txt")

length(unique(DIA_Comorbidity_Annual_Inventories$patid)) # 343912

# OBE Comorbs per year

OBE2_Comorbidity_Annual_Inventories <- fread("DANU Comorbidity Annual Inventories/OBE2 Comorbidity Annual Inventories.txt")

length(unique(OBE2_Comorbidity_Annual_Inventories$patid)) # 721893

Comorbidity_Annual_Inventories <- unique(
  rbind(
    DIA_Comorbidity_Annual_Inventories,
    OBE2_Comorbidity_Annual_Inventories
  )
)

length(unique(Comorbidity_Annual_Inventories$patid)) # 1065717

length(unique(Comorbidity_Annual_Inventories$diagnosis)) # 2011

# Ever had during the 5 years (overestimate)

Comorbidity_Annual_Inventories[ , total_5y := rowSums(.SD, na.rm = TRUE),
    .SDcols = c("year1", "year2", "year3", "year4", "year5")
]


master_df_gerd <- merge(
  unique(Comorbidity_Annual_Inventories[ (grepl("^K2", diagnosis) | grepl("R12", diagnosis) ) & total_5y > 0, .(patid)]),
  DANU_Demographics[ , .(patid, weight, gender, age, race, region, division, plan)],
  by = "patid", all.x = TRUE
)

sum(master_df_gerd$weight) # 57519918



Comorbidity_Annual_Inventories <- Comorbidity_Annual_Inventories[
  (!is.na(diagnosis)) & (grepl("^K2", diagnosis) | diagnosis == "R12"),
  .(patid, diagnosis, total_5y)
]

length(unique(Comorbidity_Annual_Inventories$diagnosis)) # 10

Comorbidity_wide <- dcast(
  Comorbidity_Annual_Inventories,
  patid ~ diagnosis,
  value.var = "total_5y",
  fun.aggregate = sum,
  fill = 0
)


master_df_gerd <- merge(
    master_df_gerd,
   Comorbidity_wide,
    by = "patid", all.x = TRUE
)


DANU_Drug_Utilizations <- fread("DANU utilizations/DANU Drug Utilizations.txt")

DANU_Drug_Utilizations <- DANU_Drug_Utilizations[
  drug_ahfs_class %in% c(
    "Antihistamines (Gi Drugs)",
    "Proton-Pump Inhibitors",
    "Histamine H2-Antagonists",
    "Antacids And Adsorbents",
    "Protectants",
    "Antiulcer Agents And Acid Suppress.,Misc"
  )
]


DANU_Drug_Utilizations <- DANU_Drug_Utilizations[
  drug_ahfs_class == "Histamine H2-Antagonists",
  drug_ahfs_class := "Antihistamines (Gi Drugs)"
]

DANU_Drug_Utilizations[
  drug_ahfs_class %in% c(
    "Antacids And Adsorbents",
    "Antiulcer Agents And Acid Suppress.,Misc",
    "Protectants"
  ),
  drug_ahfs_class := "Other Antiacid|Antiulcer"
]


unique(DANU_Drug_Utilizations$drug_ahfs_class)


DANU_Drug_Utilizations_summary <- DANU_Drug_Utilizations[
  ,
  .(
    prescription_count = sum(prescription_count, na.rm = TRUE),
    rx_visits = sum(rx_visits, na.rm = TRUE),
    drug_supply_days = sum(drug_supply_days, na.rm = TRUE),
    brandname_supply_days = sum(brandname_supply_days, na.rm = TRUE),
    generic_supply_days = sum(generic_supply_days, na.rm = TRUE)
  ),
  by = patid
]


sum(is.na(master_df_gerd))


master_df_gerd <- merge(
  master_df_gerd,
  DANU_Drug_Utilizations_summary,
  by = "patid", all.x = TRUE
)

sum(master_df_gerd$weight) #57519918
 
master_df_gerd[is.na(master_df_gerd)] <- 0


# PLOTS

df_age <- master_df_gerd %>%
  filter(K21>1) %>%
  group_by(age, gender) %>%
  summarise(n = sum(weight, na.rm = TRUE), .groups = "drop")

sum(df_age$n)

plot <- df_age %>% rename("Gender"=gender) %>%
  mutate(Gender=ifelse(Gender=="F", "Female","Male")) %>%
ggplot(aes(x = age, n, colour=Gender, fill=Gender )) +
  geom_col(width=0.7) +
  scale_fill_manual(values = c( "#a14a6a", "#2e3e5c")) +
    scale_colour_manual(values = c( "#a14a6a", "#2e3e5c")) +
  labs(
    x = " \n Age (years)",
    y = "Weighted GERD population \n",
    title = "GERD Distribution ~ Age & Gender [37 Million - (K21≥1) ]"
  ) +
  coord_cartesian(ylim=c(0,1500000)) +
   theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

plot
ggsave(file = "age_gender_37million.svg", plot = plot, width = 8, height = 5)


master_df_gerd %>% mutate(total=K20+K21+K22+K23+K25+K26+K27+K28+K29+R12) %>%
  mutate(total=ifelse(total>=50,50,total)) %>%
  group_by(total) %>% summarise(n=sum(weight)) %>%
  ggplot(aes(total, n)) +
  geom_smooth(se=FALSE)


plot <- master_df_gerd %>% mutate(total=K20+K21+K22+K23+K25+K26+K27+K28+K29+R12) %>%
  rename("Gender"=gender) %>%
  mutate(Gender=ifelse(Gender=="F", "Female","Male")) %>%
  ggplot(aes(age, total,  colour=Gender, fill=Gender)) +
  geom_smooth() +
  coord_cartesian(ylim=c(0,15)) +
  labs(
    x = " \n Age (years)",
    y = "5-year # of Distinct GERD Diagnostic Claims \n",
    title = "5-year # of Distinct GERD Diagnostic Claims ~ Age & Gender"
  ) +
  facet_wrap(~Gender) +
    scale_fill_manual(values = c( "#a14a6a", "#2e3e5c")) +
    scale_colour_manual(values = c( "#a14a6a", "#2e3e5c")) +
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )



ggsave(file = "diagnosis_age_gender.svg", plot = plot, width = 8, height = 5)



plot <- master_df_gerd %>% 
  rename("Gender"=gender) %>%
  mutate(Gender=ifelse(Gender=="F", "Female","Male")) %>%
  ggplot(aes(age, prescription_count ,  colour=Gender, fill=Gender)) +
  geom_smooth() +
  coord_cartesian(ylim=c(0,15)) +
  labs(
    x = " \n Age (years)",
    y = "5-year # of Distinct GERD Prescription Rx Claims \n",
    title = "5-year # of Distinct GERD Prescription Rx Claims ~ Age & Gender"
  ) +
  facet_wrap(~Gender) +
    scale_fill_manual(values = c( "#a14a6a", "#2e3e5c")) +
    scale_colour_manual(values = c( "#a14a6a", "#2e3e5c")) +
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

plot
ggsave(file = "prescriptions_age_gender.svg", plot = plot, width = 8, height = 5)


master_df_gerd %>% group_by(plan) %>% summarise(n=sum(weight))
master_df_gerd %>% group_by(race) %>% summarise(n=sum(weight))


master_df_gerd %>% mutate(prescription_count=ifelse(prescription_count>=1,1,0)) %>%
  group_by(prescription_count) %>% summarise(n=sum(weight))


plot <- master_df_gerd %>% mutate(total=K20+K21+K22+K23+K25+K26+K27+K28+K29+R12) %>%
  mutate(total=ifelse(total>=100,100, total)) %>%
  group_by(total) %>% summarise(n=sum(weight)) %>%
  ggplot(aes(total, n)) +
  geom_col(fill="#2e3e5c", colour="#2e3e5c", width=0.5) +
   labs(
    x = " \n 5-year # of Distinct GERD Diagnostic Claims",
    y = "Weighted GERD population \n",
    title = "5-year # of Distinct GERD Diagnostic Claims"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

plot
ggsave(file = "dx_codes_number.svg", plot = plot, width = 8, height = 5)




plot <- master_df_gerd %>%
  mutate(prescription_count=ifelse(prescription_count>=100,100, prescription_count)) %>%
  group_by(prescription_count) %>% summarise(n=sum(weight)) %>%
  ggplot(aes(prescription_count, n)) +
  geom_col(fill="#2e3e5c", colour="#2e3e5c", width=0.5) +
   labs(
    x = " \n 5-year # of Distinct GERD Rx prescription Claims",
    y = "Weighted GERD population \n",
    title = "5-year # of Distinct GERD Rx prescription Claims"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

plot
ggsave(file = "rx_codes_number.svg", plot = plot, width = 8, height = 5)


fwrite(master_df_gerd, "master_df_gerd.txt")
