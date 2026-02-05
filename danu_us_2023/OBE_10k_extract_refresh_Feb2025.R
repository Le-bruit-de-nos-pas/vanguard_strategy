library(data.table)
library(tidyverse)

# BMIs -----------

Doses_OBE_Drgs <- fread("Doses_OBE_Drgs.txt")

Drugs_lookup <- Doses_OBE_Drgs %>% select(drug_id, generic_name, drug_group, drug_class) %>% distinct()

First_GLP1_exact <- Doses_OBE_Drgs %>% filter(drug_group=="GLP1 Injectable") %>%
  group_by(patid) %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt==min(from_dt))

First_GLP1_exact <- First_GLP1_exact %>% select(patid, drug_group, from_dt) %>% distinct()

length(unique(First_GLP1_exact$patid)) # 1067



length(unique(OBE_Box_Histories_All$patid)) # 2047

OBE_Box_Histories_All <- OBE_Box_Histories_All %>% gather(month, box, month1:month60)

GLP1_pats <- OBE_Box_Histories_All %>% filter(box=="I") %>% select(patid) %>% distinct()

length(unique(GLP1_pats$patid)) # 955


BMI_OBEpts <- fread("BMI_OBEpts.txt")


BMI_OBEpts <- BMI_OBEpts %>% left_join(First_GLP1_exact, by=c("PTID"="patid"))

BMI_OBEpts$DATE <- as.Date(BMI_OBEpts$DATE)
BMI_OBEpts$from_dt <- as.Date(BMI_OBEpts$from_dt)



test <- BMI_OBEpts %>% filter(!is.na(from_dt)) %>%
  ungroup() %>% mutate(from_dt=as.numeric(from_dt)) %>%
  mutate(from_dt=from_dt/365)

summary(lm(VALUE ~ from_dt, data=test))

# 
# Call:
# lm(formula = VALUE ~ from_dt, data = test)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -22.495  -5.875  -1.667   4.903  55.860 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 49.31883    1.78482  27.632  < 2e-16 ***
# from_dt     -0.16841    0.03408  -4.941 7.85e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.802 on 15821 degrees of freedom
# Multiple R-squared:  0.001541,	Adjusted R-squared:  0.001478 
# F-statistic: 24.41 on 1 and 15821 DF,  p-value: 7.848e-07




BMI_OBEpts %>% filter(!is.na(from_dt)) %>%
  ungroup() %>%  # sample_n(1000) %>%
  ggplot(aes(from_dt, VALUE)) +
  geom_jitter(shape=1,  size=1, colour="deepskyblue4", fill="deepskyblue4", alpha=0.4) +
  geom_smooth(method="loess", colour="firebrick", fill="deepskyblue4", alpha=0.5) +
  coord_cartesian(ylim=c(0,60)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  xlab("\n Exact Date of First Obesity GLP1 Initiation") +
  ylab("LOESS BMI Fit \n")




test <- BMI_OBEpts %>% 
  mutate(drug_group=ifelse(is.na(drug_group), "GLP1 Naive", "GLP1 Experienced"))


wilcox.test(test$VALUE[test$drug_group=="GLP1 Naive"], test$VALUE[test$drug_group=="GLP1 Experienced"])

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  test$VALUE[test$drug_group == "GLP1 Naive"] and test$VALUE[test$drug_group == "GLP1 Experienced"]
# W = 587872098, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

BMI_OBEpts %>% # sample_n(5000) %>%
  mutate(drug_group=ifelse(is.na(drug_group), "GLP1 Naive", "GLP1 Experienced")) %>%
  filter(VALUE<=80) %>%
  ggplot(aes(VALUE, colour=drug_group, fill=drug_group)) +
  geom_density(alpha=0.6) +
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
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  xlab("\n Exact BMI Record") +
  ylab("Gaussian Kernal \n Patient Density \n") +
  scale_fill_manual(values=c("firebrick" , "deepskyblue4")) +
  scale_colour_manual(values=c("firebrick" , "deepskyblue4")) 
  




test <- BMI_OBEpts %>% 
  mutate(drug_group=ifelse(is.na(drug_group), "GLP1 Naive", "GLP1 Experienced")) %>%
  group_by(PTID, drug_group) %>% summarise(VALUE=max(VALUE))


wilcox.test(test$VALUE[test$drug_group=="GLP1 Naive"], test$VALUE[test$drug_group=="GLP1 Experienced"])

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  test$VALUE[test$drug_group == "GLP1 Naive"] and test$VALUE[test$drug_group == "GLP1 Experienced"]
# W = 1052930, p-value = 4.499e-13
# alternative hypothesis: true location shift is not equal to 0

test %>%
  filter(VALUE<=80) %>%
  ggplot(aes(VALUE, colour=drug_group, fill=drug_group)) +
  geom_density(alpha=0.6) +
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
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  xlab("\n Exact BMI Record") +
  ylab("Gaussian Kernal \n Patient Density \n") +
  scale_fill_manual(values=c("firebrick" , "deepskyblue4")) +
  scale_colour_manual(values=c("firebrick" , "deepskyblue4")) 
  
# ----------
# Comorbidities -----------


Dxs_OBEpts <- fread("Dxs_OBEpts.txt")
Dxs_OBEpts <- Dxs_OBEpts %>% select(-PROV_UNIQUE)
Dxs_OBEpts <- Dxs_OBEpts %>% drop_na()%>% filter(DIAG!="")
Dxs_OBEpts <- Dxs_OBEpts %>% select(-DATE) %>% distinct()



Doses_OBE_Drgs <- fread("Doses_OBE_Drgs.txt")

Drugs_lookup <- Doses_OBE_Drgs %>% select(drug_id, generic_name, drug_group, drug_class) %>% distinct()

First_GLP1_exact <- Doses_OBE_Drgs %>% filter(drug_group=="GLP1 Injectable") %>%
  group_by(patid) %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt==min(from_dt))

First_GLP1_exact <- First_GLP1_exact %>% select(patid, drug_group, from_dt) %>% distinct()

length(unique(First_GLP1_exact$patid)) # 1067


Dxs_OBEpts <- Dxs_OBEpts %>% left_join(First_GLP1_exact %>% select(patid) %>% mutate(GLP1="GLP1"), by=c("PTID"="patid"))

Dxs_OBEpts <- Dxs_OBEpts %>% mutate(GLP1=ifelse(is.na(GLP1), "Naive", GLP1))


Dxs_OBEpts$DIAG <- str_sub(Dxs_OBEpts$DIAG,1L, 3L)


Dxs_OBEpts <- Dxs_OBEpts %>% distinct() 


# Count total number of unique patients per group
patient_counts <- Dxs_OBEpts %>%
  group_by(GLP1) %>%
  summarise(total_patients = n_distinct(PTID)) %>%
  pivot_wider(names_from = GLP1, values_from = total_patients, values_fill = list(total_patients = 0))


# Rename columns for clarity
colnames(patient_counts) <- c("GLP1_patients", "Naive_patients")

# Create a summary table: count occurrences of each DIAG in each group
diag_counts <- Dxs_OBEpts %>%
  group_by(DIAG, GLP1) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = GLP1, values_from = count, values_fill = list(count = 0))


# Rename columns
colnames(diag_counts) <- c("DIAG", "GLP1", "Naive")

# Merge with total patient counts
diag_counts <- diag_counts %>%
  mutate(
    Naive_patients = patient_counts$Naive_patients,
    GLP1_patients = patient_counts$GLP1_patients
  )


# Apply Fisher’s Exact Test per diagnosis
test_results <- diag_counts %>%
  rowwise() %>%
  mutate(
    test = list(fisher.test(matrix(c(GLP1, GLP1_patients - GLP1,
                                     Naive, Naive_patients - Naive), 
                               nrow = 2))),
    p_value = test$p.value,
    odds_ratio = ifelse(is.null(test$estimate), NA, test$estimate)  # Handle cases where OR is missing
  ) %>%
  select(-test) %>%
  ungroup()


# Adjust p-values using FDR correction
test_results <- test_results %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr")) %>%
  arrange(p_adj)



# Print top results
data.frame(head(test_results, 25))


# Visualization: Top 10 most significant diagnoses
ggplot(test_results %>% head(50), 
       aes(x = reorder(DIAG, odds_ratio), y = odds_ratio)) +
  geom_bar(stat = "identity", fill="deepskyblue4", alpha=0.5) +
  coord_flip() +
  labs(title = "Top 50 Most Different Diagnoses \n[FDR-Adjusted Fisher's Exact test p-value]\nGLP1-experienced vs GLP1-naive \n",
       x = "Diagnosis ICD10 Code \n",
       y = "Odds Ratio") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
        plot.margin = margin(5, 5, 5, 5, "pt"))  + 
  geom_hline(yintercept = 1) 





# Convert to wide format: Each row = 1 patient, Each column = 1 diagnosis (binary presence)
wide_df <- Dxs_OBEpts %>%
  mutate(present = 1) %>%  # Create presence indicator
  pivot_wider(names_from = DIAG, values_from = present, values_fill = list(present = 0))

# Separate out the patient IDs and group labels
patient_info <- wide_df %>% select(PTID, GLP1)
diagnosis_matrix <- wide_df %>% select(-PTID, -GLP1)  # Only diagnosis columns

# Perform PCA (scale = TRUE for variance normalization)
pca_model <- prcomp(diagnosis_matrix, center = TRUE, scale. = TRUE)



# Extract PC scores and merge with group labels
pca_scores <- as.data.frame(pca_model$x) %>%
  cbind(patient_info)



# Run t-tests for all PCs
pc_t_tests <- lapply(names(pca_scores)[1:(ncol(pca_scores) - 2)], function(pc) {
  test <- t.test(pca_scores[[pc]] ~ pca_scores$GLP1)
  data.frame(PC = pc, p_value = test$p.value)
}) %>%
  bind_rows()


# Adjust p-values using FDR correction
pc_t_tests <- pc_t_tests %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr")) %>%
  arrange(p_adj)



# Show top significant PCs
print(head(pc_t_tests, 10))


pca_scores %>%
  ggplot(aes(PC7, fill=GLP1)) +
  geom_density(alpha=0.6) + xlim(-10,10) +
  scale_fill_manual(values = c("firebrick", "deepskyblue4")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  ylab("Gaussian Kernel Patient Density \n ")
 

# Extract PCA loadings
pca_loadings <- as.data.frame(pca_model$rotation)

# View top contributing diagnoses for PC3 (replace with any PC of interest)
top_PC7 <- pca_loadings %>%
  select(PC7) %>%
  mutate(DIAG = rownames(pca_loadings)) %>%
  arrange(desc(abs(PC7))) %>%  # Sort by absolute contribution
  head(10)  # Show top 10 contributing diagnoses

print(top_PC7)  # Display the top 10 contributing diagnoses

# Bar plot of top contributors for PC7
ggplot(top_PC7, aes(x = reorder(DIAG, abs(PC7)), y = PC7, fill = PC7 < 0)) +
  geom_bar(stat = "identity", alpha=0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "deepskyblue4")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  labs(title = "Top 10 Diagnoses Contributing to PC7",
       x = "ICD10 Diagnosis Code \n",
       y = "\n Loading Value",
       fill = "Direction") +
  theme_minimal()

# ----------
# STOPS -----------


OBE_Drug_Histories_RxExp <- fread("OBE Drug Histories_RxExp.txt")

OBE_Drug_Histories_RxExp <- OBE_Drug_Histories_RxExp %>%
  gather(month, drugs, month1:month60)

OBE_Drug_Histories_RxExp$month <- parse_number(as.character(OBE_Drug_Histories_RxExp$month))


Doses_OBE_Drgs <- fread("Doses_OBE_Drgs.txt")

range(Doses_OBE_Drgs$from_dt) #" 2018-09-30" "2024-09-30"

# Sep 2024 == 60


Doses_OBE_Drgs$from_dt <- as.Date(Doses_OBE_Drgs$from_dt)


Doses_OBE_Drgs <- Doses_OBE_Drgs %>%
  mutate(month_index = lubridate::interval(from_dt, as.Date("2024-09-30")) %/% months(1)) %>%
  mutate(month_index=60-month_index)

range(Doses_OBE_Drgs$month_index)



Drugs_lookup <- Doses_OBE_Drgs %>% select(drug_id, generic_name, drug_group, drug_class) %>% distinct()


OBE_Drug_Histories_RxExp <- OBE_Drug_Histories_RxExp %>% mutate(ON=ifelse(grepl("51", drugs), 1,
                                               ifelse(grepl("58", drugs), 1,
                                                      ifelse(grepl("59", drugs), 1, 0))))

OBE_Drug_Histories_RxExp <- OBE_Drug_Histories_RxExp %>% select(-weight)

OBE_Drug_Histories_RxExp <- OBE_Drug_Histories_RxExp %>% arrange(patid, month) %>%
  group_by(patid) %>% mutate(change=ifelse(ON!=lag(ON), 1,0))
 
OBE_Drug_Histories_RxExp <- OBE_Drug_Histories_RxExp %>% 
  mutate(change=ifelse(is.na(change),0,change))

stops <- OBE_Drug_Histories_RxExp %>%
  filter(change==1 & ON==0 & month<=48) 


stops <- stops %>% select(patid, month) %>% distinct() %>%  rename("stops"="month") %>% ungroup()

length(unique(stops$patid)) #378


OBE_Drug_Histories_RxExp %>% left_join(stops) %>%
  filter(month>stops & ON==1) %>%  select(patid) %>% distinct()  #157 42%


Doses_OBE_Drgs <- Doses_OBE_Drgs %>% filter(drug_group=="GLP1 Injectable") %>%
  select(drug_id, generic_name, brand_name, brand_name, patid, month_index) %>% distinct()


befores <- Doses_OBE_Drgs %>% left_join(stops) %>%
  filter(month_index<stops) %>% select(patid, brand_name) %>% distinct()

afters <- Doses_OBE_Drgs %>% left_join(stops) %>%
  filter(month_index>stops) %>% select(patid, brand_name) %>% distinct()

befores %>% inner_join(afters)  %>% select(patid) %>% distinct() # 122  come back to the same

# ----------
# Number of months ON GLP1 --------------------

Doses_OBEonly_Drgs <- fread("Doses_OBEonly_Drgs.txt")

Doses_OBEonly_Drgs %>% filter(grepl("GLP1", drug_group)) %>%
  select(patid) %>% distinct() # 894


Doses_OBEonly_Drgs %>% filter(grepl("GLP1", drug_group)) %>%
  select(patid, brand_name) %>% distinct() %>%
  group_by(patid) %>% count() %>% ungroup() %>% group_by(n) %>% count()


Doses_OBEonly_Drgs %>% filter(grepl("GLP1", drug_group)) %>%
  select(brand_name) %>% distinct() 

range(Doses_OBEonly_Drgs$from_dt) # "2018-09-30" "2024-09-30"

Doses_OBEonly_Drgs <- Doses_OBEonly_Drgs %>%
  mutate(month_index = lubridate::interval(from_dt, as.Date("2024-09-30")) %/% months(1)) %>%
  mutate(month_index=60-month_index)

range(Doses_OBEonly_Drgs$month_index)

Drugs_lookup <- Doses_OBEonly_Drgs %>% select(drug_id, generic_name, drug_group, drug_class) %>% distinct()

# 58, 59, 61



OBE_Only_Drug_Histories_RxExp <- fread("OBE_Only_Drug_Histories_RxExp.txt")

OBE_Only_Drug_Histories_RxExp <- OBE_Only_Drug_Histories_RxExp %>%
  gather(month, drugs, month1:month60)

OBE_Only_Drug_Histories_RxExp$month <- parse_number(as.character(OBE_Only_Drug_Histories_RxExp$month))

OBE_Only_Drug_Histories_RxExp <- OBE_Only_Drug_Histories_RxExp %>% select(-weight)

data.frame(OBE_Only_Drug_Histories_RxExp %>% filter(drugs!="-") %>%
  filter(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs)) %>%
  group_by(patid) %>% count() %>% ungroup() %>% rename("months"="n") %>%
    mutate(months=ifelse(months>=24,24,months)) %>%
  group_by(months) %>% count() %>% ungroup() %>% mutate(tot=sum(n)) %>% 
  mutate(perc=n/tot))



data.frame(OBE_Only_Drug_Histories_RxExp %>% filter(drugs!="-") %>%
  filter(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs)) %>%
    mutate(year=ifelse(month<=12,1,
                       ifelse(month<=24,2,
                              ifelse(month<=36,3,
                                     ifelse(month<=48,4,5))))) %>%
  group_by(patid, year) %>% count() %>% ungroup() %>% rename("months"="n") %>%
    mutate(months=ifelse(months>=24,24,months)) %>%
  group_by(year, months) %>% count() %>% ungroup() %>% group_by(year) %>%
    mutate(tot=sum(n)) %>% 
  mutate(perc=n/tot)) %>%
  select(year, months, perc) %>% distinct() %>%
  spread(key=year, value=round(perc,0))


OBE_Only_Drug_Histories_RxExp <- OBE_Only_Drug_Histories_RxExp %>% mutate(ON=ifelse(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs),1,0))


OBE_Only_Drug_Histories_RxExp <- OBE_Only_Drug_Histories_RxExp %>%
  arrange(patid, month) %>%
   group_by(patid) %>% mutate(grp = rle(ON)$lengths %>% {rep(seq(length(.)), .)})


data.frame(OBE_Only_Drug_Histories_RxExp %>% filter(drugs!="-") %>%
  filter(ON==1) %>%
  group_by(patid, grp) %>% count() %>% ungroup() %>% rename("months"="n") %>%
    mutate(months=ifelse(months>=24,24,months)) %>%
  group_by(months) %>% count() %>% ungroup() %>% mutate(tot=sum(n)) %>% 
  mutate(perc=n/tot))


data.frame(OBE_Only_Drug_Histories_RxExp %>% filter(drugs!="-") %>%
  filter(ON==1) %>% filter(grepl("61", drugs)) %>%
  group_by(patid, grp) %>% count() %>% ungroup() %>% rename("months"="n") %>%
    mutate(months=ifelse(months>=24,24,months)) %>%
  group_by(months) %>% count() %>% ungroup() %>% mutate(tot=sum(n)) %>% 
  mutate(perc=n/tot))

data.frame(OBE_Only_Drug_Histories_RxExp %>% filter(drugs!="-") %>%
  filter(ON==1) %>% filter(grepl("58", drugs)) %>%
  group_by(patid) %>% count() %>% ungroup() %>% rename("months"="n") %>%
    mutate(months=ifelse(months>=24,24,months)) %>%
  group_by(months) %>% count() %>% ungroup() %>% mutate(tot=sum(n)) %>% 
  mutate(perc=n/tot))

# -----------------
# Stocks Before/After GLP1 --------------------

OBE_Only_Box_Histories_All <- fread("OBE_Only_Box_Histories_All.txt")


OBE_Only_Box_Histories_All <- OBE_Only_Box_Histories_All %>%
  gather(month, stocks, month1:month60)

OBE_Only_Box_Histories_All$month <- parse_number(as.character(OBE_Only_Box_Histories_All$month))

OBE_Only_Box_Histories_All <- OBE_Only_Box_Histories_All %>% select(-weight)

unique(OBE_Only_Box_Histories_All$stocks)

OBE_Only_Box_Histories_All %>% arrange(patid, month) %>% group_by(patid) %>%
  filter(stocks!="I" & lead(stocks)=="I") %>%
  group_by(stocks) %>% count()

OBE_Only_Box_Histories_All %>% arrange(patid, month) %>% group_by(patid) %>%
  filter(stocks!="I" & lag(stocks)=="I") %>%
  group_by(stocks) %>% count()



OBE_Only_Box_Histories_All %>% arrange(patid, month) %>% group_by(patid) %>%
  filter(stocks!="I" & lead(stocks)=="I") %>%
    mutate(year=ifelse(month<=12,1,
                       ifelse(month<=24,2,
                              ifelse(month<=36,3,
                                     ifelse(month<=48,4,5))))) %>%
  group_by(year, stocks) %>% count() %>%
   ungroup() %>% group_by(year) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot) %>% select(-c(n,tot)) %>%
  spread(key=year, value=perc)





OBE_Only_Box_Histories_All %>% arrange(patid, month) %>% group_by(patid) %>%
  filter(stocks!="I" & lag(stocks)=="I") %>%
    mutate(year=ifelse(month<=12,1,
                       ifelse(month<=24,2,
                              ifelse(month<=36,3,
                                     ifelse(month<=48,4,5))))) %>%
  group_by(year, stocks) %>% count() %>%
   ungroup() %>% group_by(year) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot) %>% select(-c(n,tot)) %>%
  spread(key=year, value=perc)




# -----------------
# Patient-months brand GLP1 --------------------

OBE_Only_Drug_Histories_All <- fread("OBE_Only_Drug_Histories_All.txt")


OBE_Only_Drug_Histories_All <- OBE_Only_Drug_Histories_All %>%
  gather(month, drugs, month1:month60)

OBE_Only_Drug_Histories_All$month <- parse_number(as.character(OBE_Only_Drug_Histories_All$month))

OBE_Only_Drug_Histories_All <- OBE_Only_Drug_Histories_All %>% select(-weight)


OBE_Only_Drug_Histories_All <- OBE_Only_Drug_Histories_All %>% filter(drugs!="-")


OBE_Only_Drug_Histories_All  %>%
  filter(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs)) %>%
    mutate(year=ifelse(month<=12,1,
                       ifelse(month<=24,2,
                              ifelse(month<=36,3,
                                     ifelse(month<=48,4,5))))) %>%
  select(year, patid) %>% distinct() %>% group_by(year) %>% count()  # 803



OBE_Only_Drug_Histories_All %>% 
  mutate(Saxenda=ifelse(grepl("61", drugs), 1,0)) %>%
  mutate(Wegovy=ifelse(grepl("58", drugs), 1,0)) %>%
  mutate(Zepbound=ifelse(grepl("59", drugs), 1,0))  %>%
  filter(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs)) %>% # 4617
  mutate(Brand=ifelse(Zepbound==1,"Zepbound",
                      ifelse(Saxenda==1,"Saxenda", "Wegovy"))) %>%
  group_by(Brand) %>% count() %>% ungroup() %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)




OBE_Only_Drug_Histories_All %>% 
   mutate(year=ifelse(month<=12,1,
                       ifelse(month<=24,2,
                              ifelse(month<=36,3,
                                     ifelse(month<=48,4,5))))) %>%
  mutate(Saxenda=ifelse(grepl("61", drugs), 1,0)) %>%
  mutate(Wegovy=ifelse(grepl("58", drugs), 1,0)) %>%
  mutate(Zepbound=ifelse(grepl("59", drugs), 1,0))  %>%
  filter(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs)) %>% # 4617
  mutate(Brand=ifelse(Zepbound==1,"Zepbound",
                      ifelse(Saxenda==1,"Saxenda", "Wegovy"))) %>%
  group_by(year, Brand) %>% count() %>% ungroup() %>%
  group_by(year) %>%
  mutate(tot=sum(n)) %>%
  mutate(perc=n/tot) %>%
  select(-c(n,tot)) %>%
  spread(key=Brand, value=perc)




OBE_Only_Drug_Histories_All %>% 
  mutate(Saxenda=ifelse(grepl("61", drugs), 1,0)) %>%
  mutate(Wegovy=ifelse(grepl("58", drugs), 1,0)) %>%
  mutate(Zepbound=ifelse(grepl("59", drugs), 1,0))  %>%
  filter(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs)) %>% # 4617
  group_by(patid) %>% filter(month==min(month)) %>%
  mutate(Brand=ifelse(Zepbound==1,"Zepbound",
                      ifelse(Saxenda==1,"Saxenda", "Wegovy"))) %>%
  group_by(Brand) %>% count() %>% ungroup() %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)




OBE_Only_Drug_Histories_All %>% 
   mutate(year=ifelse(month<=12,1,
                       ifelse(month<=24,2,
                              ifelse(month<=36,3,
                                     ifelse(month<=48,4,5))))) %>%
  mutate(Saxenda=ifelse(grepl("61", drugs), 1,0)) %>%
  mutate(Wegovy=ifelse(grepl("58", drugs), 1,0)) %>%
  mutate(Zepbound=ifelse(grepl("59", drugs), 1,0))  %>%
  filter(grepl("58", drugs)|grepl("59", drugs)|grepl("61", drugs)) %>% # 4617
    group_by(patid) %>% filter(month==min(month)) %>%
  mutate(Brand=ifelse(Zepbound==1,"Zepbound",
                      ifelse(Saxenda==1,"Saxenda", "Wegovy"))) %>%
  group_by(year, Brand) %>% count() %>% ungroup() %>%
  group_by(year) %>%
  mutate(tot=sum(n)) %>%
  mutate(perc=n/tot) %>%
  select(-c(n,tot)) %>%
  spread(key=Brand, value=perc)



# -----------------
#  % GLP1 out of those with Dx --------------------

Dxs_OBEpts <- fread("Dxs_OBEpts.txt")

range(Dxs_OBEpts$DATE)

Dxs_OBEpts$DATE <- as.Date(Dxs_OBEpts$DATE)
Dxs_OBEpts <- Dxs_OBEpts %>% filter(grepl("E66", DIAG)) %>% select(PTID, DATE) %>% distinct()

Dxs_OBEpts <- Dxs_OBEpts %>%
  mutate(month_index = lubridate::interval(DATE, as.Date("2024-09-30")) %/% months(1)) %>%
  mutate(month_index=60-month_index)


Dxs_OBEpts <- Dxs_OBEpts %>% filter(month_index>=1) %>%
  mutate(year=ifelse(month_index<=12,1,
                       ifelse(month_index<=24,2,
                              ifelse(month_index<=36,3,
                                     ifelse(month_index<=48,4,5))))) 


Dxs_OBEpts %>% select(PTID, year) %>% distinct() %>%
  group_by(year) %>% count() %>% mutate(n=n/10000)




Doses_OBE_Drgs <- fread("Doses_OBE_Drgs.txt")

Doses_OBE_Drgs$from_dt <- as.Date(Doses_OBE_Drgs$from_dt)

Doses_OBE_Drgs <- Doses_OBE_Drgs %>%
  mutate(month_index = lubridate::interval(from_dt, as.Date("2024-09-30")) %/% months(1)) %>%
  mutate(month_index=60-month_index)


Doses_OBE_Drgs <- Doses_OBE_Drgs %>% filter(month_index>=1) %>%
  mutate(year=ifelse(month_index<=12,1,
                       ifelse(month_index<=24,2,
                              ifelse(month_index<=36,3,
                                     ifelse(month_index<=48,4,5))))) 



Doses_OBE_Drgs <- Doses_OBE_Drgs %>% filter(paid_status =="PAID") %>% 
  filter(grepl("GLP", drug_group)) %>%
  select(patid, year) %>% distinct()



Doses_OBE_Drgs %>% select(patid, year) %>% distinct() %>%
  group_by(year) %>% count() %>% mutate(n=n/10000)



Dxs_OBEpts %>% select(PTID, year) %>% distinct() %>%
  group_by(year) %>% count() %>% mutate(n=n)



Doses_OBE_Drgs %>% select(patid, year) %>% distinct() %>%
  rename("PTID"="patid") %>%
  inner_join(Dxs_OBEpts) %>%
  group_by(year) %>% count() 



# -----------------