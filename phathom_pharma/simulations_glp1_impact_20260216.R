
library(dplyr)
library(ggplot2)

set.seed(123)

# Total population fraction for simulation
n_patients <- 220000  

# BMI range
bmi_vals <- seq(16, 50, 0.1)

# Approximate BMI distribution: skewed normal using dnorm + exponential adjustment
# Start with normal peak ~27, sd ~6, then skew right a little
baseline_density <- dnorm(bmi_vals, mean = 27, sd = 6)
skew_factor <- exp((bmi_vals - 27)/20)   # skew right
density_vals <- baseline_density * skew_factor

# Normalize to probabilities
prob_vals <- density_vals / sum(density_vals)

# Number of simulated patients per BMI
n_per_bmi <- round(prob_vals * n_patients)

# Create patient-level BMI vector
bmi_population <- rep(bmi_vals, times = n_per_bmi)
length(bmi_population)  # should be close to 220,000

# Check distribution visually
ggplot(data.frame(BMI = bmi_population), aes(x = BMI)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Simulated US Adult BMI Distribution",
       x = "BMI",
       y = "Density") +
  theme_minimal()



# Logistic GLP1 probability function
# Adjust parameters to get ~10% overall uptake and right BMI slope
glp1_mid   <- 34    # midpoint BMI where probability ~50% of max
glp1_slope <- 0.25     # steepness
glp1_max   <- 0.35     # maximum probability at very high BMI

# Compute probability for each patient's BMI
p_glp1 <- glp1_max / (1 + exp(-glp1_slope * (bmi_population - glp1_mid)))

# Assign GLP1 status randomly based on probability
glp1_status <- rbinom(length(bmi_population), size = 1, prob = p_glp1)

# Create patient-level dataframe
patients <- data.frame(
  BMI = bmi_population,
  GLP1 = glp1_status
)

# Quick check: overall GLP1 %
mean(patients$GLP1)  # should be around 10%


patients %>% ggplot(aes(BMI, GLP1)) + geom_smooth()


plot <- patients %>% group_by(BMI) %>% summarise(mean=mean(GLP1)) %>% 
  ggplot(aes(BMI, mean)) + geom_line(colour="#8499b1", linewidth=0.3) + 
  geom_smooth(fill="#593f62", colour="#593f62", alpha=0.3) +
  theme_minimal() +
   labs(title = "GLP1 Penetrance ~ BMI",
       x = "\n BMI",
       y = "Proportion ON GLP1\n") +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(text = element_text(face = "bold"))


ggsave(file="../out/glp1_funct_bmi.svg", plot=plot, width=4, height=4)


# Count patients per BMI per group
bmi_counts <- patients %>%
  group_by(GLP1, BMI) %>%
  summarise(n = n()) %>%
  ungroup() %>% mutate(GLP1=ifelse(GLP1==0, "No GLP1", "GLP1 User")) %>%
  mutate(GLP1=as.factor(GLP1))

plot <- ggplot(bmi_counts, aes(x = BMI, y = n, colour = GLP1, fill = GLP1)) +
  geom_area(alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("No GLP1" = "#8499b1", "GLP1 User" = "#593f62")) +
  scale_color_manual(values = c("No GLP1" = "#8499b1", "GLP1 User" = "#593f62")) +
  labs(title = "BMI Distribution by GLP1 Status \n [ Population Size ]",
       x = "\n BMI",
       y = "Number of Patient Samples [ x1000 ] \n",
       fill = "GLP1 Status",
       color = "GLP1 Status") +
  theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(text = element_text(face = "bold"))


ggsave(file="../out/bmi_distribution_glp1_no_glp1.svg", plot=plot, width=4, height=4)



# Parameters
baseline_min <- 0.01   # 5% at BMI 16
baseline_max <- 0.35   # 35% at BMI 50
bmid <- 30             # midpoint where prevalence increases fastest
slope <- 0.15          # steepness

# Logistic-like baseline function
p_gerd_base <- baseline_min + 
  (baseline_max - baseline_min) / (1 + exp(-slope * (patients$BMI - bmid)))



sum(patients$GLP1)

# Relative risk by BMI
rr_low  <- 4.0   # max effect at low BMI
rr_high <- 1.0   # min effect at high BMI
k       <- 0.15  # decay rate

rr_bmi <- rr_high + (rr_low - rr_high) * exp(-k * (patients$BMI - min(patients$BMI)))


# If patient is on GLP1
patients$GERD_Prob <- ifelse(
  patients$GLP1 == 1,
  pmin(p_gerd_base * rr_bmi, 0.95),  # cap at 95%
  p_gerd_base
)

# Binary GERD flag
patients$GERD <- rbinom(nrow(patients), 1, patients$GERD_Prob)

# Quick checks
mean(patients$GERD)                      # should be ~20% overall
mean(patients$GERD[patients$GLP1 == 1]) # higher, ~2x baseline
mean(patients$GERD[patients$GLP1 == 0]) # baseline by BMI

head(patients)

summary_df <- patients %>%
  mutate(GLP1 = factor(GLP1, labels = c("No GLP1", "GLP1 User"))) %>%
  group_by(GLP1) %>%
  summarise(
    mean_gerd = mean(GERD),
    n = n(),
    se = sqrt(mean_gerd * (1 - mean_gerd) / n)
  )


plot <- ggplot(summary_df, aes(x = GLP1, y = mean_gerd, fill = GLP1)) +
  geom_col(width = 0.8, alpha=0.7) +
  geom_errorbar(aes(ymin = mean_gerd - 2.576*se, ymax = mean_gerd + 2.576*se),
                width = 0.5, size=1) +
  scale_fill_manual(values = c("No GLP1" = "#8499b1", "GLP1 User" = "#593f62")) +
  labs(
    title = "GERD Prevalence by GLP1 Status",
    x = "\n GLP1 Status",
    y = "GERD Prevalence \n"
  ) +
  theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(text = element_text(face = "bold"))

ggsave(file="../out/gerd_prevalence_glp1_no_glp1.svg", plot=plot, width=4, height=4)




plot <- patients %>% mutate(GLP1=as.factor(GLP1)) %>%
  ggplot(aes(x = BMI, y = GERD)) +
  geom_smooth(aes(color = GLP1, fill = GLP1),  span = 0.3, se=T) +
  scale_color_manual(values = c("0" = "#8499b1", "1" = "#593f62"),
                     labels = c("No GLP1", "GLP1 User")) +
  scale_fill_manual(values = c("0" = "#8499b1", "1" = "#593f62"),
                     labels = c("No GLP1", "GLP1 User")) +
  labs(
    title = "Simulated GERD Prevalence by BMI",
    x = "\n BMI",
    y = "GERD Probability \n"
  ) +
  theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(text = element_text(face = "bold"))

ggsave(file="../out/gerd_probability_glp1_no_glp1.svg", plot=plot, width=4, height=4)




# Over time


library(ggplot2)
library(dplyr)

years <- 2026:2050

# Smooth mean BMI increase (saturating)
mean_bmi <- 27 + (29 - 27) * (1 - exp(-0.05 * (years - 2026)))

# Slightly increasing SD to allow tail to grow faster
sd_bmi <- 6 + 0.1 * (years - 2026)/(2050-2026)

# Fraction >30 (computed independently)
frac_over30 <- 1 - pnorm(30, mean = mean_bmi, sd = sd_bmi)

df <- data.frame(Year = years, mean_BMI = mean_bmi, SD = sd_bmi, frac_over30 = frac_over30)


# Plot mean BMI vs fraction >30
plot <- ggplot(df) +
  geom_line(aes(x=Year, y=mean_BMI, color="Mean BMI [ kg/m2 ] "), size=2.2) +
  geom_line(aes(x=Year, y=frac_over30*50, color="BMI >30 [ % ]"), size=2.2) + 
  scale_y_continuous(
    name="Mean BMI \n", breaks = seq(0, 35, by = 2),
    sec.axis = sec_axis(~./50, name="% BMI >30 \n",  breaks = seq(0.2, 0.9, by = 0.05))  # scale fraction for plotting
  ) +
  labs(title="Mean BMI & Fraction >30 Over Time",  color = "") +
  scale_color_manual(values=c("#8499b1", "#593f62")) +
   theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(text = element_text(face = "bold"))

plot

ggsave(file="../out/bmis_over_time.svg", plot=plot, width=4, height=4)


# Create BMI bins for visualization
patients <- patients %>% 
  mutate(BMI_bin = cut(BMI, breaks = seq(15, 50, by = 1)))

# Compute GERD prevalence per BMI bin
gerd_by_bmi <- patients %>%
  group_by(BMI_bin) %>%
  summarise(
    mean_BMI = mean(BMI),
    GERD_prev = mean(GERD)
  )



# Function to map BMI to GERD probability
get_gerd_prob <- approxfun(gerd_by_bmi$mean_BMI, gerd_by_bmi$GERD_prev, rule=3)



n_patients <- 100000  # per year

gerd_over_time <- lapply(1:nrow(df), function(i) {
  year <- df$Year[i]
  mean_b <- df$mean_BMI[i]
  sd_b   <- df$SD[i]

  # Sample BMI for this year
  bmi_sim <- rnorm(n_patients, mean = mean_b, sd = sd_b)
  # Map BMI to GERD probability using the previous curve
  gerd_prob <- get_gerd_prob(bmi_sim)
  
  # Compute mean GERD prevalence for this year
  data.frame(Year = year,
             GERD_prev = mean(gerd_prob))
}) %>% bind_rows()

gerd_over_time



plot <- ggplot(gerd_over_time, aes(x=Year, y=GERD_prev)) +
  geom_smooth(color="#8499b1", size=2.5) +
  labs(title="Projected GERD Prevalence\nOver Time [ US Population ]",
       x="\n Year",
       y="GERD Prevalence \n") +
  scale_y_continuous(
    name="GERD Prevalence [%] \n",
    limits = c(0.1, 0.20),          # adjust as needed
    breaks = seq(0.1, 0.20, by=0.02),  # breaks every 5%
    labels = scales::percent_format(accuracy = 1)  # show as % 
  ) +
  theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(text = element_text(face = "bold"))

ggsave(file="../out/gerd_over_time.svg", plot=plot, width=4, height=4)



# Patient level BMI drift

years <- 2026:2050

start_glp1  <- mean(patients$GLP1)  # ~10%
target_glp1 <- 0.25

rate <- 0.08  # controls speed of adoption

glp1_target_curve <- start_glp1 + 
  (target_glp1 - start_glp1) * (1 - exp(-rate * (years - 2026)))

glp1_target_curve

# Mean BMI trajectory (smooth, saturating)
mean_bmi_year <- 27 + (29 - 27) * (1 - exp(-0.06 * (years - 2026)))

bmi_shift <- mean_bmi_year - mean(mean_bmi_year)[1]

patients$BMI_2026 <- patients$BMI

glp1_prob_fn <- function(bmi) {
  glp1_mid   <- 34
  glp1_slope <- 0.25
  glp1_max   <- 0.35
  
  glp1_max / (1 + exp(-glp1_slope * (bmi - glp1_mid)))
}


gerd_base_fn <- function(bmi) {
  baseline_min <- 0.01
  baseline_max <- 0.35
  bmid <- 30
  slope <- 0.15
  
  baseline_min + 
    (baseline_max - baseline_min) / (1 + exp(-slope * (bmi - bmid)))
}

rr_fn <- function(bmi) {
  rr_low  <- 4.0
  rr_high <- 1.0
  k <- 0.15
  
  rr_high + (rr_low - rr_high) * exp(-k * (bmi - min(bmi)))
}


patients_over_time <- lapply(seq_along(years), function(i) {
  
  year <- years[i]
  
  # Shift BMI
  bmi_year <- patients$BMI_2026 + bmi_shift[i]
  
  # ---- Passive GLP1 (BMI driven) ----
  p_glp1_passive <- glp1_prob_fn(bmi_year)
  glp1_passive <- rbinom(length(bmi_year), 1, p_glp1_passive)
  
  # ---- Target 25% GLP1 ----
  target_rate <- glp1_target_curve[i]
  
  # Adjust probabilities proportionally to hit target
  scaling_factor <- target_rate / mean(p_glp1_passive)
  p_glp1_target <- pmin(p_glp1_passive * scaling_factor, 0.95)
  
  glp1_target <- rbinom(length(bmi_year), 1, p_glp1_target)
  
  data.frame(
    Year = year,
    BMI = bmi_year,
    GLP1_passive = glp1_passive,
    GLP1_target25 = glp1_target
  )
})

patients_over_time <- bind_rows(patients_over_time)



data.frame(patients_over_time %>%
  group_by(Year) %>%
  summarise(GLP1_passive  = mean(GLP1_passive ), GLP1_target25= mean(GLP1_target25 ) ))

library(tidyr)

glp1_rates <- patients_over_time %>%
  group_by(Year) %>%
  summarise(
    Passive = mean(GLP1_passive),
    Target25 = mean(GLP1_target25)
  ) %>%
  pivot_longer(-Year, names_to = "Scenario", values_to = "Rate")


# ggplot(glp1_rates, aes(x = Year, y = Rate, color = Scenario)) +
#   geom_line(size = 2) +
#   scale_y_continuous(
#     labels = scales::percent_format(accuracy = 1),
#     breaks = seq(0, 0.30, by = 0.05),
#     limits = c(0, 0.30)
#   ) +
#   scale_color_manual(
#     values = c("Passive" = "#8499b1",
#                "Target25" = "#593f62")
#   ) +
#   labs(
#     title = "GLP1 Uptake Over Time:\nPassive [BMI drift] vs Target 25% Adoption",
#     x = "\nYear",
#     y = "GLP1 Penetration [%]\n",
#     color = ""
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "top",
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     text = element_text(face = "bold")
#   )


glp1_wide <- glp1_rates %>%
  pivot_wider(names_from = Scenario, values_from = Rate)

plot <- ggplot(glp1_wide, aes(x = Year)) +
    geom_ribbon(aes(ymin = Passive, ymax = Target25),
              fill = "#593f62", alpha = 0.4) +
  geom_line(aes(y = Passive, color = "Passive"), size = 2) +
  geom_line(aes(y = Target25, color = "Target 25%"), size = 2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 0.30, by = 0.05),
    limits = c(0, 0.30)
  ) +
  scale_color_manual(
    values = c("Passive" = "#ADC6CC",
               "Target 25%" = "#151C5C")
  ) +
  labs(
    title = "GLP1 Uptake Over Time:\nPassive (BMI drift-driven) vs. \nTarget 25% Adoption",
    x = "\nYear",
    y = "GLP1 Penetration [%]\n",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

ggsave(file="../out/glp1_over_time.svg", plot=plot, width=4, height=4)



patients_over_time %>%
  group_by(Year, BMI) %>%
  summarise(
    Passive = mean(GLP1_passive),
    Target25 = mean(GLP1_target25)
  )


gerd_lookup <- patients %>%
  group_by(BMI) %>%
  summarise(
    GERD_noGLP1  = mean(GERD_Prob[GLP1 == 0]),
    GERD_withGLP1 = mean(GERD_Prob[GLP1 == 1])
  )

gerd_lookup[is.na(gerd_lookup)] <- 0

glp1_bmi_year <- patients_over_time %>%
  mutate(BMI=round(BMI, 1)) %>%
  group_by(Year, BMI) %>%
  summarise(
    Passive  = mean(GLP1_passive),
    Target25 = mean(GLP1_target25),
    .groups = "drop"
  )


glp1_bmi_year <- glp1_bmi_year %>%
  left_join(gerd_lookup, by = "BMI")


glp1_bmi_year[is.na(glp1_bmi_year)] <- 0


glp1_bmi_year <- glp1_bmi_year %>%
  mutate(
    GERD_passive =
      (1 - Passive)  * GERD_noGLP1 +
      Passive        * GERD_withGLP1,

    GERD_target25 =
      (1 - Target25) * GERD_noGLP1 +
      Target25       * GERD_withGLP1
  )


bmi_weights <- patients_over_time %>%
  group_by(Year, BMI) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(weight = n / sum(n)) %>%
  select(-n)

glp1_bmi_year <- glp1_bmi_year %>%
  left_join(bmi_weights %>% ungroup() %>% mutate(BMI=round(BMI, 1)), by = c("Year","BMI"))


gerd_over_time <- glp1_bmi_year %>%
  group_by(Year) %>%
  summarise(
    Passive  = sum(GERD_passive  * weight, na.rm = TRUE),
    Target25 = sum(GERD_target25 * weight, na.rm = TRUE)
  )

mean(gerd_over_time$Passive)
mean(gerd_over_time$Target25)


gerd_over_time %>% rename("Passive")
ggplot(gerd_over_time, aes(x = Year)) +
  geom_ribbon(aes(ymin = Passive, ymax = Target25),
              fill = "#e6b3b3", alpha = 0.4) +
  geom_smooth(aes(y = Passive, color = "Passive"), size = 2) +
  geom_smooth(aes(y = Target25, color = "Target25"), size = 2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    breaks = seq(0, 0.30, by = 0.02), limits=c(0.1,0.16)
  ) +
  scale_color_manual(
    values = c("Passive" = "#7f0000",
               "Target25" = "#c51b8a")
  ) +
  labs(
    title = "Projected GERD Prevalence\nGLP1 Adoption Scenarios",
    x = "\nYear",
    y = "GERD Prevalence [%]\n",
    color = ""
  ) +

  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(face = "bold")
  )


# Start GLP1

t <- seq(0, 24, by = 0.1)

rr_fun <- function(t, A, k1, k2, floor) {
  1 + A * (1 - exp(-k1 * t)) * exp(-k2 * t) +
    (floor - 1) * (1 - exp(-0.3 * t))
}


df <- bind_rows(
  data.frame(
    Month = t,
    RR = rr_fun(t, A = 3.5, k1 = 2.5, k2 = 0.4, floor = 1.6),
    Scenario = "b) Sharp Early Peak"
  ),
  data.frame(
    Month = t,
    RR = rr_fun(t, A = 2.8, k1 = 1.5, k2 = 0.3, floor = 1.5),
    Scenario = "c) Moderate Peak"
  ),
  data.frame(
    Month = t,
    RR = rr_fun(t, A = 2.2, k1 = 1.2, k2 = 0.5, floor = 1.4),
    Scenario = "d) Slower Rise"
  ),
  data.frame(
    Month = t,
    RR = rr_fun(t, A = 4.0, k1 = 3.5, k2 = 0.6, floor = 1.7),
    Scenario = "a) Very Sharp Peak"
  )
)

df

plot <- ggplot(df, aes(Month, RR, color = Scenario)) +
  geom_line(size = 2.5, alpha=0.8) +
  scale_y_continuous(limits = c(0, 4)) +
  labs(
    title = "GERD Relative Risk After Start GLP1",
    x = "\nMonths Since Initiation",
    y = "Relative Risk\n"
  ) +
  scale_colour_manual(values=c("#615f85", "#4c7195", "#be94a4" , "#e0cf68")) +
  theme_minimal() +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

plot

ggsave(file="../out/start_glp1_rr_gerd.svg", plot=plot, width=5, height=4)



# BMI drops

library(dplyr)
library(ggplot2)

set.seed(42)

# --- Step 1: Simulate BMI population ---
population_size <- 10000
mean_bmi <- 30
sd_bmi <- 7

bmi_population <- rnorm(population_size, mean = mean_bmi, sd = sd_bmi)

# Truncate to BMI > 25
bmi_population <- bmi_population[bmi_population > 25]
population_size_truncated <- length(bmi_population)
population_size_truncated
# 7609 (similar to Python example)

# --- Step 2: Define weight loss distributions ---
mean_loss_6m <- 7.5 / 100   # 7.5%
sd_loss_6m   <- 3 / 100     # ±3%
mean_loss_12m <- 17.5 / 100 # 17.5%
sd_loss_12m   <- 5 / 100    # ±5%

# Generate initial random weight losses
loss_6m <- rnorm(population_size_truncated, mean = mean_loss_6m, sd = sd_loss_6m)
loss_12m <- rnorm(population_size_truncated, mean = mean_loss_12m, sd = sd_loss_12m)

# --- Step 3: Apply raw weight losses ---
bmi_after_6m <- bmi_population * (1 - loss_6m)
bmi_after_12m <- bmi_population * (1 - loss_12m)

df_truncated <- data.frame(
  Initial_BMI = bmi_population,
  BMI_after_6m = bmi_after_6m,
  BMI_after_12m = bmi_after_12m
)

# --- Step 4: Rank-based proportional adjustment ---
# Higher BMI patients lose slightly more
ranked_idx <- order(df_truncated$Initial_BMI, decreasing = TRUE)

# Create linear scaled loss vectors
loss_6m_adjusted <- seq(mean_loss_6m - sd_loss_6m, mean_loss_6m + sd_loss_6m, length.out = population_size_truncated)
loss_12m_adjusted <- seq(mean_loss_12m - sd_loss_12m, mean_loss_12m + sd_loss_12m, length.out = population_size_truncated)

# Apply adjusted loss according to rank
adjusted_loss_6m <- loss_6m_adjusted[ranked_idx]
adjusted_loss_12m <- loss_12m_adjusted[ranked_idx]

bmi_after_6m_adj <- bmi_population * (1 - adjusted_loss_6m)
bmi_after_12m_adj <- bmi_population * (1 - adjusted_loss_12m)

df_adjusted <- data.frame(
  Initial_BMI = bmi_population,
  BMI_after_6m = bmi_after_6m_adj,
  BMI_after_12m = bmi_after_12m_adj
)

# --- Step 5: Summary statistics ---
summary_stats <- df_adjusted %>% summarise(
  Initial_Mean = mean(Initial_BMI),
  After6M_Mean = mean(BMI_after_6m),
  After12M_Mean = mean(BMI_after_12m),
  Initial_SD = sd(Initial_BMI),
  After6M_SD = sd(BMI_after_6m),
  After12M_SD = sd(BMI_after_12m),
  Initial_Above30 = mean(Initial_BMI > 30) * 100,
  Initial_Above25 = mean(Initial_BMI > 25) * 100,
  After6M_Above30 = mean(BMI_after_6m > 30) * 100,
  After6M_Above25 = mean(BMI_after_6m > 25) * 100,
  After12M_Above30 = mean(BMI_after_12m > 30) * 100,
  After12M_Above25 = mean(BMI_after_12m > 25) * 100
)

summary_stats

# Reshape to long format
df_long <- df_adjusted %>%
  pivot_longer(
    cols = c(Initial_BMI, BMI_after_6m, BMI_after_12m),
    names_to = "Timepoint",
    values_to = "BMI"
  )

# Plot with legend
plot <- ggplot(df_long, aes(x = BMI, fill = Timepoint)) +
  #geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  geom_density(alpha = 0.7, colour="white") +
  labs(
    title = "Best-case BMI Distributions\nAfter GLP-1 Start",
    x = "\nBMI",
    y = paste("Patient Proportion \n"),
    fill = "Timepoint"
  ) +
  scale_fill_manual(
    values = c("Initial_BMI" = "#593f62", 
               "BMI_after_6m" = "#151C5C", 
               "BMI_after_12m" = "#ADC6CC"),
    labels = c("BMI after 12 months", "BMI after 6 months","Initial BMI")
  ) +
  xlim(17,50) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

ggsave(file="../out/bmi_drops.svg", plot=plot, width=6, height=4)





library(ggplot2)
library(dplyr)

t <- seq(0, 24, by = 0.1)  # months

# Function for %BMI change
bmi_drop_fun <- function(t, max_drop, k1, k2, floor = 0.15, rebound = 0.02) {
  # max_drop: maximum % reduction at peak
  # k1: speed of initial drop
  # k2: speed of tapering/stabilizing
  # floor: minimum stable reduction fraction (e.g., 15%)
  # rebound: tiny increase after stabilization
  drop <- max_drop * (1 - exp(-k1 * t)) * exp(-k2 * t)
  stabilized <- floor * (1 - exp(-0.1 * t))
  rebound_effect <- rebound * (1 - exp(-0.05 * (t - 12))) * (t > 12)
  drop + stabilized + rebound_effect
}

df_bmi <- bind_rows(
  data.frame(
    Month = t,
    BMI_Reduction = bmi_drop_fun(t, max_drop = 0.18, k1 = 0.5, k2 = 0.05, floor = 0.02, rebound = 0.03),
    Scenario = "a) Sharp Early Drop"
  ),
  data.frame(
    Month = t,
    BMI_Reduction = bmi_drop_fun(t, max_drop = 0.10, k1 = 0.4, k2 = 0.03, floor = 0.05, rebound = 0.015),
    Scenario = "b) Moderate Responders"
  ),
  data.frame(
    Month = t,
    BMI_Reduction = bmi_drop_fun(t, max_drop = 0.10, k1 = 0.3, k2 = 0.02, floor = 0.10, rebound = 0.02),
    Scenario = "c) Slower Good Responder"
  ),
  data.frame(
    Month = t,
    BMI_Reduction = bmi_drop_fun(t, max_drop = 0.18, k1 = 0.6, k2 = 0.06, floor = 0.02, rebound = 0.005),
    Scenario = "d) Very Sharp Drop"
  )
)

# Plot
plot_bmi <- ggplot(df_bmi, aes(Month, -BMI_Reduction, color = Scenario)) +
  geom_line(size = 2, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Projected % BMI Reduction After Start GLP1",
    x = "\nMonths Since Initiation",
    y = "BMI Reduction (%)\n"
  ) +
  scale_colour_manual(values=c("#615f85", "#4c7195", "#be94a4" , "#e0cf68")) +
  theme_minimal() +
  ylim(-0.20,0.10) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

plot_bmi

ggsave(file="../out/glp1_bmi_reduction.svg", plot=plot_bmi, width=6, height=4)


# Net effect

library(ggplot2)

set.seed(123)

n <- 10000

# Use a normal distribution
net_effect <- rnorm(n, mean = 0.12, sd = 0.07)

# Introduce skew: negative tail ~20%
net_effect <- net_effect - 0.1 * (runif(n) < 0.2)

# Clip extreme values for realism
net_effect <- pmin(pmax(net_effect, -0.2), 0.35)

# Compute density manually
dens <- density(net_effect)
df_dens <- data.frame(x = dens$x, y = dens$y / max(dens$y))  # normalize y to max 1
df_dens$`Net Effect` <- ifelse(df_dens$x < 0, "Negative", "Positive")

# Plot with two colors
plot <- ggplot(df_dens, aes(x = x, y = y, fill = `Net Effect`)) +
  geom_area(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Negative" = "#593f62", "Positive" = "#8499b1")) +
  labs(
    title = "Simulated Distribution\nNet Effect of GLP1 on GERD Risk",
    x = "\nNet GERD Risk (pos = higher risk)",
    y = "Patient density \n"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold")
  )

plot

ggsave(file="../out/net_effect.svg", plot=plot, width=4, height=4)
