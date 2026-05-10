# NEW REPO

library(data.table)
library(tidyverse)

OBE2_Comorbidity_Inventories <- fread("OBE2 Analysis Results 1.1/OBE2 Comorbidity Inventories.txt")

head(OBE2_Comorbidity_Inventories)

# K21-K21.9, K22.7, R12
data.frame(OBE2_Comorbidity_Inventories %>% filter(grepl("K21", diagnosis)|
                                          grepl("K22", diagnosis)|
                                          grepl("R12", diagnosis)) %>%
  select(diagnosis) %>% distinct())

gerd_pats <- OBE2_Comorbidity_Inventories %>% filter(grepl("K21", diagnosis)|
                                          grepl("R12", diagnosis)) %>%
  select(patid, weight) %>% distinct()

OBE2_Comorbidity_Inventories %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 132324569

sum(gerd_pats$weight) # 36136343



fread("OBE2 Analysis Results 1.1/OBE2 Comorbidity Annual Inventories.txt") %>%
  filter(diagnosis=="K21")


# OLD REPO

library(data.table)
library(tidyverse)


DANU_Drug_utilizations <- fread("DANU Utilizations/DANU Drug utilizations.txt")

DANU_Drug_utilizations %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 162676153

unique_classes <- data.frame(unique(DANU_Drug_utilizations$drug_ahfs_class))

DANU_Drug_utilizations <- DANU_Drug_utilizations %>% filter(drug_ahfs_class %in% c("Antihistamines (Gi Drugs)",
                                                         "Proton-Pump Inhibitors",
                                                         "Histamine H2-Antagonists",
                                                         "Antacids And Adsorbents",
                                                         "Protectants",
                                                         "Antiulcer Agents And Acid Suppress.,Misc"))


DANU_Drug_utilizations %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Histamine H2-Antagonists", 
                                "Antihistamines (Gi Drugs)", drug_ahfs_class)) %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class %in% c("Antacids And Adsorbents", "Antiulcer Agents And Acid Suppress.,Misc", "Protectants"), 
                                "Other Antiacid|Antiulcer", drug_ahfs_class )) %>% 
  group_by(drug_ahfs_class) %>% summarise(n=sum(weight)) %>% rename("tot"="n") %>%
  left_join(
    DANU_Drug_utilizations %>% 
       mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Histamine H2-Antagonists", 
                                "Antihistamines (Gi Drugs)", drug_ahfs_class)) %>% 
        mutate(drug_ahfs_class=ifelse(drug_ahfs_class %in% c("Antacids And Adsorbents", "Antiulcer Agents And Acid Suppress.,Misc", "Protectants"), 
                                "Other Antiacid|Antiulcer", drug_ahfs_class )) %>% 
      filter(drug_supply_days==generic_supply_days ) %>% 
      group_by(drug_ahfs_class) %>% summarise(n=sum(weight)) 
  ) %>%
  mutate(perc=n/tot) %>% mutate(perc=ifelse(is.na(perc),1,perc)) 



DANU_Drug_utilizations %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Histamine H2-Antagonists", 
                                "Antihistamines (Gi Drugs)", drug_ahfs_class)) %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class %in% c("Antacids And Adsorbents", "Antiulcer Agents And Acid Suppress.,Misc", "Protectants"), 
                                "Other Antiacid|Antiulcer", drug_ahfs_class )) %>% 
  group_by(drug_ahfs_class) %>% summarise(n=sum(weight)) %>% rename("tot"="n") %>% ungroup() %>%
  mutate(glob=sum(tot), perc=tot/glob) 


DANU_Drug_utilizations %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Histamine H2-Antagonists", 
                                "Antihistamines (Gi Drugs)", drug_ahfs_class)) %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class %in% c("Antacids And Adsorbents", "Antiulcer Agents And Acid Suppress.,Misc", "Protectants"), 
                                "Other Antiacid|Antiulcer", drug_ahfs_class )) %>%
  select(patid, weight, drug_ahfs_class) %>% distinct() %>%
 # mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Proton-Pump Inhibitors", "PPI", "Other")) %>%
  mutate(exp=1) %>% distinct() %>%
  spread(key=drug_ahfs_class, value=exp) %>%
  group_by(`Antihistamines (Gi Drugs)`, `Other Antiacid|Antiulcer`, `Proton-Pump Inhibitors`) %>% summarise(n=sum(weight)) %>%
  arrange(`Proton-Pump Inhibitors`, `Antihistamines (Gi Drugs)`, `Other Antiacid|Antiulcer`) %>%
  mutate(n2=(n/58224533)) %>% mutate(n3=n2*26000000)


DANU_Drug_utilizations %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Histamine H2-Antagonists", 
                                "Antihistamines (Gi Drugs)", drug_ahfs_class)) %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class %in% c("Antacids And Adsorbents", "Antiulcer Agents And Acid Suppress.,Misc", "Protectants"), 
                                "Other Antiacid|Antiulcer", drug_ahfs_class )) %>%
  mutate(drug_supply_days=drug_supply_days/5) %>%
  filter(drug_supply_days<=366) %>%
  ggplot(aes(drug_supply_days, colour=drug_ahfs_class, fill=drug_ahfs_class)) +
  geom_density(bins=50, alpha=0.5) +
  xlab("\n Number of Supply Days per Year") + ylab("Patient density \n") +
  facet_wrap(~drug_ahfs_class, scales="free_y") +
  theme_minimal() +
   theme(
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )




plot <- DANU_Drug_utilizations %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Histamine H2-Antagonists", 
                                "Antihistamines (Gi Drugs)", drug_ahfs_class)) %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class %in% c("Antacids And Adsorbents", "Antiulcer Agents And Acid Suppress.,Misc", "Protectants"), 
                                "Other Antiacid|Antiulcer", drug_ahfs_class )) %>%
  mutate(drug_supply_days=drug_supply_days/5) %>%
  filter(drug_supply_days<=366) %>%
  rename("drug_class"="drug_ahfs_class") %>%
  ggplot(aes(drug_class, drug_supply_days, colour=drug_class, fill=drug_class)) +
  #geom_density(bins=50, alpha=0.5) +
  geom_boxplot(outliers = FALSE, notch = TRUE, alpha=0.5) +
  xlab("Drug Class \n") + ylab("\nNumber of Supply Days per Year") +
  facet_wrap(~drug_class, scales="free", ncol=1) +
  theme_minimal() +
  coord_flip() +
   theme(
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
    scale_fill_manual(values = c("#e0cf68", "#914236", "#8499b1")) +
  scale_colour_manual(values = c("#e0cf68", "#914236", "#8499b1")) 

ggsave(file="../out/supp_days.svg", plot=plot, width=7, height=4)


