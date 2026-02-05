# =============================================================================
# Obesity-Related Disorders & Claims Data Analysis Pipeline
# =============================================================================

# ─── 1. Packages & Global Settings ───────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(lubridate)
  library(viridis)
  library(ggsci)
  library(scales)
  library(RColorBrewer)
})

options(scipen = 999, stringsAsFactors = FALSE)

theme_set(theme_minimal(base_size = 11) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                  panel.grid.major.x = element_blank(),
                  plot.title = element_text(face = "bold")))

# ─── 2. Constants & Lookup Tables ────────────────────────────────────────────

FINAL_DATE <- as_date("2021-10-01")

AGE_BREAKS <- seq(0, 100, by = 5)
AGE_LABELS <- c(paste0(AGE_BREAKS[-length(AGE_BREAKS)], "-", AGE_BREAKS[-1]-1), "100+")

ICD10_LABELS <- c(
  "E661" = "Drug-induced Obesity (E661)",
  "R638" = "Deviated Food Habit (R638)",
  "R635" = "Weight Gain (R635)",
  "R632" = "Bulimia (R632)",
  "E662" = "Obesity with Alveolar Hypoventilation Syndrome (E662)",
  "E668" = "Morbid Obesity (E668)",
  "E669" = "Obesity (E669)"
)

# ─── 3. Utility Functions ────────────────────────────────────────────────────

save_csv <- function(x, name, ...) {
  write_csv(x, file.path("output", paste0(name, ".csv")), ...)
  invisible(x)
}

plot_icd_bar <- function(df, log_scale = FALSE, title_suffix = "") {
  p <- df %>%
    count(icd10code) %>%
    mutate(icd10code = fct_reorder(icd10code, n)) %>%
    ggplot(aes(icd10code, n)) +
    geom_col(fill = if(log_scale) "firebrick" else "midnightblue", alpha = 0.85) +
    geom_text(aes(label = n), vjust = -0.5, size = 3.2) +
    scale_x_discrete(labels = ICD10_LABELS) +
    labs(x = NULL,
         y = if(log_scale) "Log₁₀ (patient-visits)" else "Number of patient-visits",
         title = paste("Obesity-related disorders – patient-visits", title_suffix)) +
    theme(plot.title = element_text(size = 13))

  if (log_scale) {
    p + scale_y_log10(limits = c(1, 2e6))
  } else {
    p + ylim(0, 1.7e6)
  }
}

plot_time_series <- function(df, color_var = NULL, title = NULL) {
  ggplot(df, aes(datamonth, n)) +
    geom_line(linewidth = 1.3, alpha = 0.9) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
    labs(x = NULL, y = "Number of patient-visits", title = title) +
    {if (!is.null(color_var)) aes(color = {{color_var}}) else NULL} +
    {if (!is.null(color_var)) scale_color_viridis_d() else scale_color_manual(values = "firebrick")}
}

create_age_groups <- function(age) {
  cut(age, breaks = c(AGE_BREAKS, Inf), labels = AGE_LABELS, right = FALSE)
}

# ─── 4. Load Core Datasets ───────────────────────────────────────────────────

obesity_dx     <- fread("Obesity_Disease_Data.csv",   colClasses = "character")
act_obesity    <- fread("Act_Data_Obesity.csv",       colClasses = "character")
ff1            <- fread("Obesity_FF1Data.csv",        colClasses = "character")
all_age_sex    <- fread("all_obesity_age_sex.csv",    colClasses = "character")
act_all_drugs  <- fread("Act_Data_Obesity_All_Drugs.csv", colClasses = "character")

# Optional reference tables (uncomment if needed)
# M_KaCode   <- fread("M_KaCode.txt",   sep = "\t", colClasses = "character")
# M_DataKbn  <- fread("M_DataKbn.txt",  sep = "\t", colClasses = "character")
# M_Drug     <- fread("M_Drug.txt",     sep = "\t", colClasses = "character")

# ─── 5. ICD-10 & Visit Type Visualizations ───────────────────────────────────

# ICD-10 frequency – linear & log
print(plot_icd_bar(obesity_dx))
print(plot_icd_bar(obesity_dx, log_scale = TRUE))

# Outpatient vs Inpatient
obesity_dx %>%
  count(nyugaikbn) %>%
  mutate(nyugaikbn = factor(nyugaikbn, levels = c("1","2"),
                            labels = c("Outpatient", "Inpatient"))) %>%
  ggplot(aes(nyugaikbn, n, fill = nyugaikbn)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick", "midnightblue")) +
  geom_text(aes(label = n), vjust = -0.4) +
  ylim(0, 1.8e6) +
  labs(title = "Outpatient vs Inpatient care (Apr 2008 – Oct 2021)",
       x = NULL, y = "Number of patient-visits")

# ─── 6. Temporal Trends ──────────────────────────────────────────────────────

visits_month <- obesity_dx %>%
  mutate(datamonth = as_date(datamonth)) %>%
  count(datamonth) %>%
  arrange(datamonth)

save_csv(visits_month, "Obesity_Hospital_Visits_Month")

print(plot_time_series(visits_month,
                       title = "Obesity-related patient visits per month (2008–2021)"))

# ─── 7. Visits per Patient & Follow-up Rate ──────────────────────────────────

visits_per_pt <- obesity_dx %>%
  count(patientid, name = "n_visits") %>%
  arrange(patientid)

followup <- obesity_dx %>%
  mutate(datamonth = as_date(datamonth)) %>%
  group_by(patientid) %>%
  summarise(first_visit = min(datamonth), .groups = "drop") %>%
  mutate(followup_years = as.numeric(FINAL_DATE - first_visit) / 365.25)

visits_norm <- followup %>%
  left_join(visits_per_pt, by = "patientid") %>%
  mutate(visits_per_year = n_visits / followup_years)

# Distributions
visits_per_pt %>%
  ggplot(aes(y = n_visits)) +
  geom_violin(fill = "grey92", color = NA) +
  geom_jitter(width = 0.4, size = 1.6, alpha = 0.4, color = "firebrick") +
  scale_y_log10() +
  labs(title = "Number of visits per patient (log scale)", y = NULL)

visits_norm %>%
  ggplot(aes(y = visits_per_year)) +
  geom_violin(fill = "gold2", alpha = 0.4, color = NA) +
  geom_jitter(width = 0.4, size = 1.5, alpha = 0.35, color = "navy") +
  labs(title = "Visits per patient per year of follow-up", y = NULL)

# ─── 8. BMI & Department (FF1 data) ──────────────────────────────────────────

ff1_clean <- ff1 %>%
  filter(!is.na(weight), !is.na(height), weight > 0, height > 0) %>%
  mutate(across(c(weight, height), as.numeric),
         BMI = weight / (height / 100)^2) %>%
  filter(BMI > 10) %>%
  select(patientid, BMI, kacodeuni)

# BMI distribution
ff1_clean %>%
  ggplot(aes(y = BMI)) +
  geom_violin(fill = "yellow", alpha = 0.5, color = "grey40") +
  geom_jitter(width = 0.4, size = 1.6, alpha = 0.5, color = "firebrick") +
  labs(title = "BMI distribution – obesity-related patients", y = "BMI")

# ─── 9. Demographics – Age & Sex ─────────────────────────────────────────────

demo <- obesity_dx %>%
  inner_join(all_age_sex, by = c("patientid", "datamonth")) %>%
  mutate(age = as.numeric(age),
         sex  = factor(sex, levels = c("1","2"), labels = c("Male","Female")),
         AgeGroup = create_age_groups(age))

save_csv(demo, "Obesity_Disease_Data_demographics")

# Stacked ICD-10 by age group
demo %>%
  count(icd10code, AgeGroup) %>%
  mutate(icd10code = fct_reorder(icd10code, n, sum)) %>%
  ggplot(aes(icd10code, n, fill = AgeGroup)) +
  geom_col(position = "stack", alpha = 0.9) +
  scale_x_discrete(labels = ICD10_LABELS) +
  scale_fill_viridis_d() +
  ylim(0, 1.7e6) +
  labs(title = "Patient-visits by obesity disorder & age group",
       x = NULL, y = "Number of patient-visits")

# Stacked by sex
demo %>%
  count(icd10code, sex) %>%
  mutate(icd10code = fct_reorder(icd10code, n, sum)) %>%
  ggplot(aes(icd10code, n, fill = sex)) +
  geom_col(position = "stack", alpha = 0.9) +
  scale_x_discrete(labels = ICD10_LABELS) +
  scale_fill_nejm() +
  ylim(0, 1.7e6) +
  labs(title = "Patient-visits by obesity disorder & sex",
       x = NULL, y = "Number of patient-visits")

# Time series by age group
visits_age_month <- demo %>%
  filter(age >= 18) %>%
  mutate(datamonth = as_date(datamonth)) %>%
  count(datamonth, AgeGroup) %>%
  arrange(datamonth)

print(plot_time_series(visits_age_month, color_var = AgeGroup,
                       title = "Obesity-related visits per month by age group"))

# Unique patients per age group (max age)
max_age_pt <- demo %>%
  group_by(patientid) %>%
  summarise(age = max(age), .groups = "drop") %>%
  mutate(AgeGroup = create_age_groups(age))

max_age_pt %>%
  filter(age >= 18) %>%
  count(AgeGroup) %>%
  ggplot(aes(AgeGroup, n, fill = AgeGroup)) +
  geom_col() +
  scale_fill_viridis_d() +
  geom_text(aes(label = n), vjust = -0.4) +
  labs(title = "Unique patients per age group (max age recorded)",
       x = NULL, y = "Number of unique patients")

# ─── 10. Drug Exposure Labeling & Cohort Analysis ─────────────────────────────

demo_drug <- demo %>%
  select(-c(fromdate, nyugaikbn, icd10code, diseasecode, diseasename_eng)) %>%
  left_join(act_obesity, by = c("patientid", "datamonth")) %>%
  distinct() %>%
  mutate(
    datamonth = as_date(datamonth),
    DrugStatus = if_else(is.na(datakbn), "OFF Drug", "ON Drug")
  )

# ─── Cohort splits ───────────────────────────────────────────────────────────

cohorts <- list(
  "2008–2012"   = demo_drug %>% filter(datamonth <  "2012-10-01"),
  "2012–2017"   = demo_drug %>% filter(datamonth >= "2012-10-01" & datamonth < "2017-04-01"),
  "2017–2021"   = demo_drug %>% filter(datamonth >= "2017-04-01"),
  "2020–2021"   = demo_drug %>% filter(datamonth >= "2020-10-01")
)

plot_on_off_age <- function(df, period_label, ylim_max = 10000) {
  max_age <- df %>%
    group_by(patientid) %>%
    summarise(age = max(age), DrugStatus = first(DrugStatus), .groups = "drop") %>%
    mutate(AgeGroup = create_age_groups(age))

  max_age %>%
    count(AgeGroup, DrugStatus) %>%
    ggplot(aes(AgeGroup, n, fill = DrugStatus)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("OFF Drug" = "deeppink4", "ON Drug" = "aquamarine4")) +
    ylim(0, ylim_max) +
    geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.4) +
    labs(title = glue::glue("Unique patients by age & drug status ({period_label})"),
         x = NULL, y = "Number of unique patients")
}

# Generate plots for main cohorts
walk2(cohorts[1:3], names(cohorts)[1:3],
      ~ print(plot_on_off_age(.x, .y, ylim_max = 10000)))

# Proportion ON drug – 2017–2021
prop_2017_2021 <- cohorts[["2017–2021"]] %>%
  group_by(patientid) %>%
  summarise(age = max(age), DrugStatus = first(DrugStatus), .groups = "drop") %>%
  mutate(AgeGroup = create_age_groups(age)) %>%
  count(AgeGroup, DrugStatus) %>%
  pivot_wider(names_from = DrugStatus, values_from = n, values_fill = 0) %>%
  mutate(total = `OFF Drug` + `ON Drug`,
         pct_on = 100 * `ON Drug` / total)

prop_2017_2021 %>%
  ggplot(aes(AgeGroup, pct_on, fill = AgeGroup)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f", pct_on)), vjust = -0.4) +
  scale_fill_viridis_d() +
  ylim(0, 12) +
  labs(title = "% Unique patients ON mazindol (Apr 2017 – Oct 2021)",
       y = "Percentage (%)", x = NULL)

# Last 12 months (2020 Oct – 2021 Oct)
prop_last12m <- cohorts[["2020–2021"]] %>%
  group_by(patientid) %>%
  summarise(age = max(age), DrugStatus = first(DrugStatus), .groups = "drop") %>%
  mutate(AgeGroup = create_age_groups(age)) %>%
  count(AgeGroup, DrugStatus) %>%
  pivot_wider(names_from = DrugStatus, values_from = n, values_fill = 0) %>%
  mutate(total = `OFF Drug` + `ON Drug`,
         pct_on = 100 * `ON Drug` / total)

prop_last12m %>%
  ggplot(aes(AgeGroup, pct_on, fill = AgeGroup)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f", pct_on)), vjust = -0.4) +
  scale_fill_viridis_d(option = "C") +
  ylim(0, 12) +
  labs(title = "% Unique patients ON mazindol – last 12 months",
       y = "Percentage (%)", x = NULL)

# ─── 11. Most frequent drugs – last year ─────────────────────────────────────

top_drugs_last_year <- act_all_drugs %>%
  group_by(patientid, receiptcode) %>%
  summarise(n = n(), .groups = "drop") %>%
  # inner_join(M_Drug, by = "receiptcode") %>%           # uncomment if M_Drug available
  # group_by(druggeneralname_eng) %>%
  # summarise(n_patients = n_distinct(patientid)) %>%
  # arrange(desc(n_patients))

# save_csv(top_drugs_last_year, "Drugs_Obesity_Patients_N_LastYear")

# ─── End of pipeline ─────────────────────────────────────────────────────────
message("Analysis pipeline completed.")

