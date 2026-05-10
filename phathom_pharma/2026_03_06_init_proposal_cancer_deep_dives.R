library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
options(scipen = 999)


New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box %>% summarise(n=sum(weight)) # 31772455
New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))





PONS_Comorbidity_Inventories <- fread("PONS Comorbidity Inventories.txt")

names(PONS_Comorbidity_Inventories)[3] <- "ICD"

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(ICD=="K21")

PONS_Comorbidity_Inventories <-PONS_Comorbidity_Inventories %>% select(patid, weight) %>% distinct()


temp <- data.frame(New_Primary_Cancer_Box %>% group_by(Primary_Cancer) %>% summarise(tot=sum(weight)) %>%
  left_join(New_Primary_Cancer_Box %>% inner_join(PONS_Comorbidity_Inventories) %>% group_by(Primary_Cancer) %>% summarise(num=sum(weight))) %>%
  mutate(perc=num/tot) %>% arrange(perc))

temp <- temp %>% filter(Primary_Cancer!="-")


# Compute non-GERD share and reshape to long format
temp_long <- temp %>%
  mutate(Non_GERD = 1 - perc,
         GERD = perc) %>%
  select(Primary_Cancer, GERD, Non_GERD) %>%
  pivot_longer(cols = c(GERD, Non_GERD),
               names_to = "Status",
               values_to = "Percent")

# Make Primary_Cancer a factor in original order
temp_long$Primary_Cancer <- factor(temp_long$Primary_Cancer,
                                   levels = unique(temp$Primary_Cancer))

# Plot
plot <- ggplot(temp_long, aes(x = Primary_Cancer, y = Percent, fill = Status)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  geom_text(aes(label = scales::percent(Percent, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "GERD vs Non-GERD Share by Primary Cancer Type",
    x = "\nPrimary Cancer Type",
    y = "Share of Patients\n"
  ) +
    scale_fill_manual(values = c("#914236", "#8499b1")) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


ggsave(file = "plot.svg", plot = plot, width =10, height = 5)




CAN_Drug_Histories <- fread("CAN Drug Histories.txt")


PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)
PONS_Ingredients_JN_ChemoClass <- fread("PONS Ingredients JN with chemo class.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% 
  select(generic_name, drug_class, chemo_class) %>% mutate(chemo_class = ifelse(chemo_class=="none",drug_class, chemo_class)) %>%
  select(generic_name, chemo_class) %>%  left_join(PONS_Ingredients)

PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>%  separate(drug_id, c('group', 'molecule'))
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, month1:month60)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-Month) %>% distinct()
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)
names(CAN_Drug_Histories)[2] <- "molecule"
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-molecule) %>% distinct()


CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(exp=1)


temp <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) %>% distinct() %>%
  select(-died) %>%
  spread(key=chemo_class, value=exp)


temp <- temp %>% left_join(PONS_Comorbidity_Inventories %>% mutate(gerd=1))

names(temp)

temp <- temp %>% select(-Androgen,-Antiemetic, -`Appetite Stimulant`, -`Cannabinoid`, -Chemoprotective, -Corticosteroid, -Death, -Nutrition, -`Hospital Inpatient`, -`Nutrition`,  -`Surgery Inpatient`, -`Progestin`)

names(temp)


temp[is.na(temp)] <- 0



drug_cols <- c("Alkylating Agent", "Antimetabolites", "Antimicrotubule Agent",
               "Biologic", "Hormonal Therapy", "Immuno/Targeted", "Other Antineoplastics",
               "PD1/PDL1", "Platinum agent", "Radio", "Topoisomerase Inhibitor")


# Function to calculate % GERD by drug and cancer
gerd_summary <- lapply(drug_cols, function(drug) {
  temp %>%
    mutate(drug_taken = ifelse(!is.na(.data[[drug]]) & .data[[drug]]==1, "Yes",
                               ifelse(!is.na(.data[[drug]]), "No", NA))) %>%
    filter(!is.na(drug_taken)) %>%  # exclude NA drug info
    group_by(drug_taken) %>%
    summarise(
      n_patients = n(),
      gerd_cases = sum(gerd),
      perc_gerd = gerd_cases / n_patients
    ) %>%
    mutate(drug_class = drug) %>%
    ungroup()
})

# Combine all drug classes
gerd_summary_df <- bind_rows(gerd_summary)

# Arrange nicely
gerd_summary_df <- gerd_summary_df %>%
  arrange(drug_class, drug_taken)

# View example
head(gerd_summary_df, 20)


gerd_summary_df <- gerd_summary_df %>% select(-n_patients, -gerd_cases)

gerd_summary_df <- gerd_summary_df %>% spread(key=drug_taken, value=perc_gerd)

data.frame(gerd_summary_df)







temp <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) %>% distinct() %>%
  select(-died) %>%
  spread(key=chemo_class, value=exp)


temp <- temp %>% left_join(PONS_Comorbidity_Inventories %>% mutate(gerd=1))

names(temp)

temp <- temp %>% select(-Androgen,-Antiemetic, -`Appetite Stimulant`, -`Cannabinoid`, -Chemoprotective, -Corticosteroid, -Death, -Nutrition, -`Hospital Inpatient`, -`Nutrition`,  -`Surgery Inpatient`, -`Progestin`)

names(temp)


temp[is.na(temp)] <- 0


temp <- temp %>% select(-`<NA>`)


temp$Primary_Cancer <- factor(temp$Primary_Cancer)


# Define drug columns
drug_cols <- c("Alkylating Agent", "Antimetabolites", "Antimicrotubule Agent",
               "Biologic", "Hormonal Therapy", "Immuno/Targeted", "Other Antineoplastics",
               "PD1/PDL1", "Platinum agent", "Radio", "Topoisomerase Inhibitor")


# Fit logistic regression: gerd ~ all drugs + Primary_Cancer
formula_str <- paste("gerd ~", paste(paste0("`", drug_cols, "`"), collapse = " + "), "")
fit <- glm(as.formula(formula_str), data=temp, family=binomial)
summary(fit)

# Extract coefficients and standard errors
coefs <- summary(fit)$coefficients

# List of drug columns in the model
drug_vars <- c("`Alkylating Agent`", "Antimetabolites", "`Antimicrotubule Agent",
               "Biologic", "`Hormonal Therapy`", "`Immuno/Targeted`", "`Other Antineoplastics`",
               "`PD1/PDL1`", "`Platinum agent`", "Radio", "`Topoisomerase Inhibitor`")


# Keep only drug rows
drug_coefs <- coefs[rownames(coefs) %in% drug_vars, ]

# Compute OR and 95% CI manually
OR_table <- data.frame(
  Drug = rownames(drug_coefs),
  Estimate = drug_coefs[, "Estimate"],
  StdErr = drug_coefs[, "Std. Error"],
  OR = exp(drug_coefs[, "Estimate"]),
  CI_lower = exp(drug_coefs[, "Estimate"] - 1.96 * drug_coefs[, "Std. Error"]),
  CI_upper = exp(drug_coefs[, "Estimate"] + 1.96 * drug_coefs[, "Std. Error"])
)

# Round for readability
OR_table <- OR_table %>%
  mutate(across(c(OR, CI_lower, CI_upper), ~round(., 3)))

# View result
OR_table



# Prepare data
OR_table <- OR_table %>%
  mutate(
    Drug = gsub("`", "", Drug),   # remove backticks for labels
    OR_lower = CI_lower,
    OR_upper = CI_upper
  )

# Forest plot
plot <- ggplot(OR_table) +
  geom_segment(aes(
    y = reorder(Drug, OR), yend = reorder(Drug, OR),
    x = OR_lower, xend = OR_upper
  ),
  size = 3, lineend = "round", color = "firebrick", alpha=0.5
  ) +
  geom_point(aes(y = reorder(Drug, OR), x = OR), size = 4, color = "firebrick") +
  labs(
    title = "Odds Ratios for GERD by Chemo Type",
    x = "Odds Ratio",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

plot

ggsave(file = "plot.svg", plot = plot, width =9, height = 3)








temp <- New_Primary_Cancer_Box %>% left_join(PONS_Comorbidity_Inventories %>% mutate(gerd=1)) %>%
  select(-died, -weight) %>% distinct() %>% mutate(exp=1) %>% spread(key=Primary_Cancer, value=exp) 


temp[is.na(temp)] <- 0


# All cancer columns
cancer_cols <- names(temp)[!(names(temp) %in% c("patid", "gerd", "-", "weight"))]

# Build formula: gerd ~ Bone Cancer + Brain Cancer + ...
formula_str <- paste("gerd ~", paste(paste0("`", cancer_cols, "`"), collapse = " + "), "")
formula_str

fit_cancer <- glm(as.formula(formula_str), data = temp, family = binomial)

summary(fit_cancer)


cancer_cols_backtick <- paste0("`", cancer_cols, "`")
cancer_cols_backtick


coefs <- summary(fit_cancer)$coefficients

# Keep only cancer rows
cancer_coefs <- coefs[rownames(coefs) %in% cancer_cols_backtick, ]

OR_table_2 <- data.frame(
  Cancer = rownames(cancer_coefs),
  Estimate = cancer_coefs[, "Estimate"],
  StdErr = cancer_coefs[, "Std. Error"],
  OR = exp(cancer_coefs[, "Estimate"]),
  CI_lower = exp(cancer_coefs[, "Estimate"] - 1.96 * cancer_coefs[, "Std. Error"]),
  CI_upper = exp(cancer_coefs[, "Estimate"] + 1.96 * cancer_coefs[, "Std. Error"])
)

# Round for readability
OR_table_2 <- OR_table_2 %>%
  mutate(across(c(OR, CI_lower, CI_upper), ~round(., 3)))

OR_table_2


# Prepare data
OR_table_2 <- OR_table_2 %>%
  mutate(
    Cancer = gsub("`", "", Cancer),   # remove backticks for labels
    OR_lower = CI_lower,
    OR_upper = CI_upper
  )

# Forest plot
plot <- ggplot(OR_table_2) +
  geom_segment(aes(
    y = reorder(Cancer, OR), yend = reorder(Cancer, OR),
    x = OR_lower, xend = OR_upper
  ),
  size = 3, lineend = "round", color = "#8499b1"
  ) +
  geom_point(aes(y = reorder(Cancer, OR), x = OR), size = 4, color = "navy") +
  labs(
    title = "Odds Ratios for GERD by Cancer Type",
    x = "Odds Ratio",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

plot

ggsave(file = "plot.svg", plot = plot, width =9, height = 5)



PONS_Drug_Utilizations <- fread("PONS Drug Utilizations.txt")



PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% filter(drug_ahfs_class %in% c("Antihistamines (Gi Drugs)",
                                                         "Proton-Pump Inhibitors",
                                                         "Histamine H2-Antagonists",
                                                         "Antacids And Adsorbents",
                                                         "Protectants",
                                                         "Antiulcer Agents And Acid Suppress.,Misc"))



PONS_Drug_Utilizations  <- PONS_Drug_Utilizations %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class=="Histamine H2-Antagonists", 
                                "Antihistamines (Gi Drugs)", drug_ahfs_class)) %>% 
  mutate(drug_ahfs_class=ifelse(drug_ahfs_class %in% c("Antacids And Adsorbents", "Antiulcer Agents And Acid Suppress.,Misc", "Protectants"), 
                                "Other Antiacid|Antiulcer", drug_ahfs_class )) %>% distinct()
  
PONS_Drug_Utilizations <- PONS_Drug_Utilizations %>% select(patid, weight, drug_ahfs_class) %>% distinct()


temp <- New_Primary_Cancer_Box %>% 
  left_join(PONS_Drug_Utilizations %>% select(-weight) %>% 
                                       mutate(exp=1) %>% spread(key=drug_ahfs_class, value=exp))  %>%
  select(-died) 

data.frame(temp %>% group_by(Primary_Cancer, `Proton-Pump Inhibitors`) %>% summarise(n=sum(weight)) %>%
  spread(key=`Proton-Pump Inhibitors`,value=n) %>%
  mutate(perc=`1`/(`1`+`<NA>` )))

temp <- temp %>% select(-`Antihistamines (Gi Drugs)`, -`Other Antiacid|Antiulcer`) 



# Make sure the PPI column is 0/1
temp$PPI <- ifelse(is.na(temp$`Proton-Pump Inhibitors`), 0, temp$`Proton-Pump Inhibitors`)

# Vector of cancer columns
cancer_cols <- c("Bone Cancer", "Brain Cancer", "Breast Cancer", "Gastroesophageal Cancer",
                 "Head Cancer", "Intestinal Cancer", "Kidney Cancer", "Leukemia Cancer",
                 "Liver Cancer", "Lung Cancer", "Lymphoma Cancer", "Myeloma Cancer",
                 "Other Cancer", "Pancreatic Cancer", "Prostate Cancer", "Reproductive Cancer",
                 "Respiratory Cancer", "Salivary Cancer", "Skin Cancer", "Thyroid Cancer",
                 "Unspecified Cancer", "Urinary Cancer")

# Backticks for non-syntactic names
cancer_cols_backticks <- paste0("`", cancer_cols, "`")


temp <- temp %>% select(-weight, -`Proton-Pump Inhibitors`) %>% mutate(exp=1) %>% distinct() %>% spread(key=Primary_Cancer, value=exp)

temp[is.na(temp)] <- 0

formula_str <- paste("PPI ~", paste(cancer_cols_backticks, collapse = " + "))
formula_str

# Fit logistic regression
fit_ppi <- glm(as.formula(formula_str), data = temp, family = binomial)

# Summary
summary(fit_ppi)

# Calculate ORs and 95% CI
coefs <- summary(fit_ppi)$coefficients
OR <- exp(coefs[,1])
CI_lower <- exp(coefs[,1] - 1.96 * coefs[,2])
CI_upper <- exp(coefs[,1] + 1.96 * coefs[,2])

# Combine in a table
OR_table_ppi <- data.frame(
  Cancer = rownames(coefs),
  Estimate = coefs[,1],
  StdErr = coefs[,2],
  OR = OR,
  CI_lower = CI_lower,
  CI_upper = CI_upper
)

OR_table_ppi



# Prepare data
OR_table_ppi <- OR_table_ppi %>%
  mutate(
    Cancer = gsub("`", "", Cancer),   # remove backticks for labels
    OR_lower = CI_lower,
    OR_upper = CI_upper
  )

OR_table_ppi <- OR_table_ppi %>% filter(Cancer!="(Intercept)")

# Forest plot
plot <- ggplot(OR_table_ppi) +
  geom_segment(aes(
    y = reorder(Cancer, OR), yend = reorder(Cancer, OR),
    x = OR_lower, xend = OR_upper
  ),
  size = 3, lineend = "round", color = "#8499b1"
  ) +
  geom_point(aes(y = reorder(Cancer, OR), x = OR), size = 4, color = "navy") +
  labs(
    title = "Odds Ratios for PPI Usage by Cancer Type",
    x = "Odds Ratio",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

plot

ggsave(file = "plot.svg", plot = plot, width =9, height = 5)








temp <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories, by=c("patid"="patient")) %>% distinct() %>%
  select(-died) %>%
  spread(key=chemo_class, value=exp)


names(temp)

temp <- temp %>% select(-Androgen,-Antiemetic, -`Appetite Stimulant`, -`Cannabinoid`, -Chemoprotective, -Corticosteroid, -Death, -Nutrition, -`Hospital Inpatient`, -`Nutrition`,  -`Surgery Inpatient`, -`Progestin`)

names(temp)


temp[is.na(temp)] <- 0


temp <- temp %>% select(-`<NA>`)


temp <- temp %>% left_join( PONS_Drug_Utilizations %>% select(-weight) %>% 
                                       mutate(exp=1) %>% spread(key=drug_ahfs_class, value=exp)) %>%
  select(-`Antihistamines (Gi Drugs)`, -`Other Antiacid|Antiulcer`)
  

temp$PPI <- ifelse(is.na(temp$`Proton-Pump Inhibitors`), 0, temp$`Proton-Pump Inhibitors`)

temp <- temp %>% select(-`Proton-Pump Inhibitors`)


# Define drug columns
drug_cols <- c("Alkylating Agent", "Antimetabolites", "Antimicrotubule Agent",
               "Biologic", "Hormonal Therapy", "Immuno/Targeted", "Other Antineoplastics",
               "PD1/PDL1", "Platinum agent", "Radio", "Topoisomerase Inhibitor")


# Fit logistic regression: gerd ~ all drugs + Primary_Cancer
formula_str <- paste("PPI ~", paste(paste0("`", drug_cols, "`"), collapse = " + "), "")
fit <- glm(as.formula(formula_str), data=temp, family=binomial)
summary(fit)

# Extract coefficients and standard errors
coefs <- summary(fit)$coefficients

# List of drug columns in the model
drug_vars <- c("`Alkylating Agent`", "Antimetabolites", "`Antimicrotubule Agent",
               "Biologic", "`Hormonal Therapy`", "`Immuno/Targeted`", "`Other Antineoplastics`",
               "`PD1/PDL1`", "`Platinum agent`", "Radio", "`Topoisomerase Inhibitor`")


# Keep only drug rows
drug_coefs <- coefs[rownames(coefs) %in% drug_vars, ]

# Compute OR and 95% CI manually
OR_table <- data.frame(
  Drug = rownames(drug_coefs),
  Estimate = drug_coefs[, "Estimate"],
  StdErr = drug_coefs[, "Std. Error"],
  OR = exp(drug_coefs[, "Estimate"]),
  CI_lower = exp(drug_coefs[, "Estimate"] - 1.96 * drug_coefs[, "Std. Error"]),
  CI_upper = exp(drug_coefs[, "Estimate"] + 1.96 * drug_coefs[, "Std. Error"])
)

# Round for readability
OR_table <- OR_table %>%
  mutate(across(c(OR, CI_lower, CI_upper), ~round(., 3)))

# View result
OR_table



# Prepare data
OR_table <- OR_table %>%
  mutate(
    Drug = gsub("`", "", Drug),   # remove backticks for labels
    OR_lower = CI_lower,
    OR_upper = CI_upper
  )

# Forest plot
plot <- ggplot(OR_table) +
  geom_segment(aes(
    y = reorder(Drug, OR), yend = reorder(Drug, OR),
    x = OR_lower, xend = OR_upper
  ),
  size = 3, lineend = "round", color = "firebrick", alpha=0.5
  ) +
  geom_point(aes(y = reorder(Drug, OR), x = OR), size = 4, color = "firebrick") +
  labs(
    title = "Odds Ratios for PPI by Chemo Type",
    x = "Odds Ratio",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

plot

ggsave(file = "plot.svg", plot = plot, width =9, height = 3)
