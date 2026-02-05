# =============================================================================
# Type 2 Diabetes Mellitus (T2DM) Claims & Patient Data Analysis Pipeline
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
  # library(fs)   # only if still needed for directory listing
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

ICD10_T2DM_LABELS <- c(
  "E11"  = "T2 DM",
  "E110" = "T2 DM w/ hyperosmolarity",
  "E111" = "T2 DM w/ ketoacidosis",
  "E112" = "T2 DM w/ kidney complications",
  "E113" = "T2 DM w/ ophthalmic complications",
  "E114" = "T2 DM w/ neurological complications",
  "E115" = "T2 DM w/ circulatory complications",
  "E116" = "T2 DM w/ other specified complications",
  "E117" = "T2 DM w/ multiple diabetic complications",
  "E119" = "T2 DM w/o complications",
  "E12"  = "Malnutrition-related DM",
  "E13"  = "Other specified DM",
  "E14"  = "Unspecified DM"
  # ... add more if needed
)

# ─── 3. Utility Functions ────────────────────────────────────────────────────

save_csv <- function(x, name, ...) {
  write_csv(x, file.path("output", paste0(name, ".csv")), ...)
  invisible(x)
}

plot_icd_bar_t2dm <- function(df, title_suffix = "") {
  df %>%
    count(icd10code) %>%
    mutate(icd10code = fct_reorder(icd10code, n)) %>%
    ggplot(aes(icd10code, n)) +
    geom_col(fill = "midnightblue", alpha = 0.85) +
    geom_text(aes(label = n), vjust = -0.5, size = 3.2) +
    scale_x_discrete(labels = ICD10_T2DM_LABELS) +
    ylim(0, 42e6) +
    labs(x = NULL, y = "Number of patient-visits",
         title = paste("T2DM-related patient-visits by ICD-10 code", title_suffix)) +
    theme(plot.title = element_text(size = 13))
}

plot_on_off_age <- function(df, drug_name, period_label, max_y = 10000) {
  df %>%
    group_by(AgeGroup, receiptcode) %>%
    summarise(n = n(), .groups = "drop") %>%
    ggplot(aes(AgeGroup, n, fill = receiptcode)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("OFF Drug" = "deeppink4", "ON Drug" = "aquamarine4")) +
    geom_text(aes(label = n), position = position_dodge(0.9), vjust = -0.4) +
    ylim(0, max_y) +
    labs(title = glue::glue("Unique patients {drug_name} status by age group ({period_label})"),
         x = NULL, y = "Number of unique patients") +
    theme(legend.title = element_blank())
}

plot_proportion_on_drug <- function(prop_df, drug_name, period_label, max_y = 16) {
  prop_df %>%
    ggplot(aes(AgeGroup, proportion, fill = AgeGroup)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = sprintf("%.2f", proportion)), vjust = -0.4) +
    scale_fill_viridis_d() +
    ylim(0, max_y) +
    labs(title = glue::glue("% Unique patients ON {drug_name} ({period_label})"),
         y = "Percentage (%)", x = NULL)
}

create_age_groups <- function(age) {
  cut(age, breaks = c(AGE_BREAKS, Inf), labels = AGE_LABELS, right = FALSE)
}

# ─── 4. Load Core Datasets ───────────────────────────────────────────────────

t2dm_ff1_raw        <- fread("T2D_FF1_Data.csv", colClasses = "character")
t2dm_icd_all        <- fread("T2DM_ICD10_all.csv", colClasses = "character")
t2dm_nyugaikbn_all  <- fread("T2_DM_nyugaikbn_all.csv", colClasses = "character")
t2dm_visits_month   <- fread("T2_DM_visits_per_month.csv", colClasses = "character")

# Optional / processed files
# N_patients_month_per_age      <- fread("N_patients_month_per_age.csv", ...)
# T2D_patients_per_age          <- fread("T2D_patients_per_age.csv", ...)
# T2DM_id_age_at_last_month     <- fread("T2DM_id_age_at_last_month.csv", ...)

# Drug-specific files (Year -1, -2, -3)
drug_files <- list(
  "Dulaglutide"   = list(y1 = "Dulaglutide_YearMinus1.csv",   y2 = "Dulaglutide_YearMinus2.csv",   y3 = "Dulaglutide_YearMinus3.csv"),
  "Ipragliflozin" = list(y1 = "Ipragliflozine_YearMinus1.csv", y2 = "Ipragliflozine_YearMinus2.csv", y3 = "Ipragliflozine_YearMinus3.csv"),
  "Glimepiride"   = list(y1 = "Glimepiride_YearMinus1.CSV",    y2 = "Glimepiride_YearMinus2.CSV",    y3 = "Glimepiride_YearMinus3.CSV"),
  "Pioglitazone"  = list(y1 = "Pioglitazone_YearMinus1.CSV",   y2 = "Pioglitazone_YearMinus2.CSV",   y3 = "Pioglitazone_YearMinus3.CSV"),
  "Sitagliptin"   = list(y1 = "Sitagliptin_YearMinus1.CSV",    y2 = "Sitagliptin_YearMinus2.CSV",    y3 = "Sitagliptin_YearMinus3.CSV"),
  "Metformin"     = list(y1 = "Metformin_YearMinus1.CSV",      y2 = "Metformin_YearMinus2.CSV",      y3 = "Metformin_YearMinus3.CSV")
)

base_demo_file <- "T2D_ID_datamonth_age_YearMinus1.csv"  # used for -1 year; adapt pattern for -2/-3

# ─── 5. BMI & Department Analysis (FF1 data) ─────────────────────────────────

t2dm_ff1_clean <- t2dm_ff1_raw %>%
  filter(!is.na(weight), !is.na(height), weight > 0, height > 0) %>%
  mutate(across(c(weight, height), as.numeric),
         BMI = weight / (height / 100)^2) %>%
  filter(BMI > 10.1, BMI < 100) %>%
  left_join(fread("M_KaCode.txt", sep = "\t", colClasses = "character"),
            by = "kacodeuni") %>%
  select(patientid, BMI, kaname_eng)

# Summary stats
summary(t2dm_ff1_clean$BMI)

# BMI distribution
t2dm_ff1_clean %>%
  ggplot(aes(y = BMI)) +
  geom_violin(fill = "brown4", alpha = 0.7, color = NA) +
  geom_jitter(width = 0.35, size = 1.4, alpha = 0.4, color = "grey20") +
  labs(title = "BMI distribution – T2DM-related patients (2008–2021)", y = "BMI")

# Visits by medical department
t2dm_ff1_clean %>%
  count(kaname_eng) %>%
  arrange(n) %>%
  ggplot(aes(reorder(kaname_eng, n), n, fill = n)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4) +
  scale_fill_viridis_c() +
  ylim(0, 140000) +
  labs(title = "Number of registered visits by Medical Department",
       x = NULL, y = NULL)

# ─── 6. ICD-10 & Visit Type Visualizations ───────────────────────────────────

print(plot_icd_bar_t2dm(t2dm_icd_all))

# Outpatient vs Inpatient
t2dm_nyugaikbn_all %>%
  mutate(nyugaikbn = factor(nyugaikbn, levels = c("1","2"),
                            labels = c("Outpatient", "Inpatient"))) %>%
  count(nyugaikbn) %>%
  ggplot(aes(nyugaikbn, n, fill = nyugaikbn)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick", "midnightblue")) +
  geom_text(aes(label = n), vjust = -0.4) +
  ylim(0, 110e6) +
  labs(title = "Outpatient vs Inpatient care – T2DM (2008–2021)",
       x = NULL, y = "Number of patient-visits")

# ─── 7. Temporal Trends ──────────────────────────────────────────────────────

visits_month_t2dm <- t2dm_visits_month %>%
  mutate(datamonth = as_date(datamonth)) %>%
  count(datamonth) %>%
  arrange(datamonth)

print(ggplot(visits_month_t2dm, aes(datamonth, n)) +
        geom_line(color = "firebrick", linewidth = 1.5, alpha = 0.8) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
        labs(title = "T2DM-related patient-visits per month (2008–2021)",
             x = NULL, y = "Number of patient-visits"))

# ─── 8. Visits per Patient & Follow-up (large processed files) ───────────────

# Note: This section assumes you still have the individual CSV files.
# If already merged → load merged file directly instead of looping.

# Example pattern (adapt if you now have a single file):
# all_disease_files <- fs::dir_ls(path = ".", regexp = "Processed_queries_T2D_sub_DiseaseData_FILTERED.*\\.csv")
# df <- map_dfr(all_disease_files, ~ fread(.x, colClasses = "character") %>% select(patientid, datamonth))

# Then:
# visits_per_patient <- df %>% count(patientid, name = "n_visits")
# followup_years <- df %>%
#   mutate(datamonth = as_date(datamonth)) %>%
#   group_by(patientid) %>%
#   summarise(first = min(datamonth)) %>%
#   mutate(followup_years = as.numeric(FINAL_DATE - first) / 365.25)

# visits_norm <- left_join(followup_years, visits_per_patient, by = "patientid") %>%
#   mutate(visits_per_year = n_visits / followup_years)

# Quick distribution plots (uncomment if data loaded)
# ggplot(visits_per_patient, aes(y = n_visits)) + geom_violin(fill = "firebrick", alpha = 0.6) + scale_y_log10()
# ggplot(visits_norm, aes(y = visits_per_year)) + geom_violin(fill = "cyan4", alpha = 0.6) + scale_y_log10()

# ─── 9. Age Group Trends & Unique Patients ────────────────────────────────────

# (Load and process N_patients_month_per_age / T2D_patients_per_age / T2DM_id_age_at_last_month as needed)

# Example time series by age group (adapt column names if necessary)
# N_patients_month_per_age %>%
#   mutate(age = as.numeric(age),
#          AgeGroup = create_age_groups(age),
#          datamonth = as_date(datamonth)) %>%
#   group_by(datamonth, AgeGroup) %>%
#   summarise(n = sum(f0_, na.rm = TRUE), .groups = "drop") %>%
#   ggplot(aes(datamonth, n, color = AgeGroup)) +
#   geom_line(linewidth = 1.4, alpha = 0.9) +
#   scale_color_viridis_d() +
#   scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
#   labs(title = "T2DM visits per month by age group", x = NULL, y = "Visits")

# ─── 10. Drug Prescription Analysis (Year -1 / -2 / -3) ──────────────────────

process_drug_cohort <- function(base_file, drug_file, period = "-1") {
  base <- fread(base_file, colClasses = "character")
  drug <- fread(drug_file, colClasses = "character")

  joined <- base %>%
    left_join(drug, by = c("patientid", "datamonth")) %>%
    distinct() %>%
    mutate(
      receiptcode = if_else(is.na(receiptcode), "OFF Drug", "ON Drug"),
      datamonth   = as_date(datamonth),
      age         = as.numeric(age),
      AgeGroup    = create_age_groups(age)
    ) %>%
    group_by(patientid) %>%
    arrange(patientid, age) %>%
    summarise(across(everything(), max), .groups = "drop")

  joined$receiptcode <- factor(joined$receiptcode, levels = c("OFF Drug", "ON Drug"))

  joined
}

# Example usage for one drug and period (repeat for all)
# dulaglutide_y1 <- process_drug_cohort("T2D_ID_datamonth_age_YearMinus1.csv",
#                                       "Dulaglutide_YearMinus1.csv", "-1")

# Then visualize
# print(plot_on_off_age(dulaglutide_y1, "Dulaglutide", "Year -1"))
# prop <- dulaglutide_y1 %>%
#   count(AgeGroup, receiptcode) %>%
#   pivot_wider(names_from = receiptcode, values_from = n, values_fill = 0) %>%
#   mutate(total = `OFF Drug` + `ON Drug`,
#          proportion = 100 * `ON Drug` / total)
# print(plot_proportion_on_drug(prop, "Dulaglutide", "Year -1"))

# ─── End of pipeline ─────────────────────────────────────────────────────────
message("T2DM analysis pipeline completed.")

