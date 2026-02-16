

# GERD % Breakdown US Map ------------- 

library(ggplot2)
library(dplyr)
library(maps)
library(tidyverse)

# Get US state map data
us_map <- map_data("state")

# Create region lookup using built-in state.region
region_df <- data.frame(
  state = tolower(state.name),
  region = state.region
)

# Rename regions to match your labels
region_df$region <- recode(region_df$region,
                           "South" = "South",
                           "North Central" = "Midwest/North Central",
                           "Northeast" = "Northeast",
                           "West" = "West")

# Assign GERD percentages
gerd_share <- data.frame(
  region = c("South",
             "Midwest/North Central",
             "Northeast",
             "West"),
  percent = c(49, 21, 18, 12)
)

# Merge everything
map_df <- us_map %>%
  left_join(region_df, by = c("region" = "state")) %>%
  left_join(gerd_share, by = c("region.y" = "region"))

unique(map_df$region.y)

# Plot
plot <- ggplot(map_df, aes(long, lat, group = group, fill = as.factor(percent) )) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(title = "      Regional % Distribution of GERD Patients in the U.S.",
    subtitle = "      Share of national GERD population by Census region") +
  theme_void() +
  scale_fill_manual(values=c("#6d7780", "#8499b1", "#7b6d8d", "#36151e")) +
  theme(legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold"))

ggsave(file="../out/map_us_gerdprev.svg", plot=plot, width=6, height=4)

# -------------------
# ------------
# Overall vs GERD-costs - Pure GERD  -------------


# Create dataset
df <- data.frame(
  Category = c("Inpatient services", "Emergency Department",
               "Outpatient services", "Pharmacy"),
  All_Cause = c(7046, 1636, 9877, 3748),
  GERD = c(3410, 452, 2903, 190)
)

# Convert to long format
df_long <- df %>%
  pivot_longer(-Category,
               names_to = "Type",
               values_to = "Cost")

# Control stacking order (bottom to top)
df_long$Category <- factor(df_long$Category,
                           levels = c("Pharmacy",
                                      "Emergency Department",
                                      "Inpatient services",
                                      "Outpatient services"))

# Compute cumulative positions for connecting lines
df_long <- df_long %>%
  group_by(Type) %>%
  arrange(Category) %>%
  mutate(
    ymin = cumsum(lag(Cost, default = 0)),
    ymax = cumsum(Cost),
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()

# Create plot
p <- ggplot(df_long, aes(x = Type, y = Cost, fill = Category)) +
  geom_bar(stat = "identity", width = 0.8, alpha=0.75) +
  geom_segment(data = df_long %>% pivot_wider(names_from = Type,
                                              values_from = ymid),
               aes(x = 1,
                   xend = 2,
                   y = All_Cause,
                   yend = GERD,
                   group = Category),
               inherit.aes = FALSE,
               color = "grey40",
               linetype = "dashed",
               size = 0.8) +

  scale_fill_manual(values = c(
    "Inpatient services" = "#8499b1",
    "Emergency Department" = "#7b6d8d",
    "Outpatient services" = "#36151e",
    "Pharmacy" = "#6d7780"
  )) +

  labs(
    title = "Average Annual Cost per GERD Patient",
    subtitle = "All-Cause vs GERD-Related Costs",
    x = "",
    y = "Annual Cost per Patient (USD) \n",
    fill = "Cost Component"
  ) +

  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(file="../out/annual_costs.svg", plot=p, width=5, height=5)

# --------------------

# -----------


# Overall vs GERD-costs - All GERD types -------------


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Build simplified dataset (means only)

df <- tribble(
  ~Disease, ~Category, ~All_Cause, ~Disease_Related,
  "GERD", "IP", 7046, 3410,
  "GERD", "ED", 1636, 452,
  "GERD", "OP", 9877, 2903,
  "GERD", "Pharmacy", 3748, 190,

  "NDBE", "IP", 6961, 3141,
  "NDBE", "ED", 1490, 321,
  "NDBE", "OP", 12810, 5011,
  "NDBE", "Pharmacy", 4347, 282,

  "IND", "IP", 7392, 3872,
  "IND", "ED", 1604, 407,
  "IND", "OP", 12646, 5092,
  "IND", "Pharmacy", 4499, 303,

  "LGD", "IP", 7144, 3347,
  "LGD", "ED", 1408, 311,
  "LGD", "OP", 16363, 8185,
  "LGD", "Pharmacy", 4807, 397,

  "HGD", "IP", 14058, 9585,
  "HGD", "ED", 1694, 555,
  "HGD", "OP", 23482, 13448,
  "HGD", "Pharmacy", 4958, 650,

  "EAC", "IP", 83222, 72353,
  "EAC", "ED", 4938, 2789,
  "EAC", "OP", 88835, 70160,
  "EAC", "Pharmacy", 6609, 1016
)


df_long <- df %>%
  pivot_longer(cols = c(All_Cause, Disease_Related),
               names_to = "Type",
               values_to = "Cost")

df_long$Category <- factor(df_long$Category,
                           levels = c("Pharmacy", "ED", "IP", "OP"))

df_long <- df_long %>%
  group_by(Disease, Type) %>%
  arrange(Category) %>%
  mutate(
    ymin = cumsum(lag(Cost, default = 0)),
    ymax = cumsum(Cost),
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()


p <- ggplot(df_long, aes(x = Type, y = Cost/1000, fill = Category)) +
  geom_bar(stat = "identity", width = 0.8, alpha=0.75) +


  facet_wrap(~Disease, nrow = 1, scales="free_y") +

  scale_fill_manual(values = c(
    "IP" = "#8499b1",
    "ED" = "#7b6d8d",
    "OP" = "#36151e",
    "Pharmacy" = "#6d7780"
  )) +

  labs(
    title = "Annual Per-Patient Healthcare Costs Across Acid-Related Disease Stages",
    subtitle = "All-Cause vs Disease-Related Costs (USD 2020, PPPY)",
    x = "",
    y = "Annual Cost per Patient \n(x1000 USD) \n",
    fill = "Cost Component"
  ) +

  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

p
ggsave(file="../out/costs.svg", plot=p, width=9, height=5)


# ------------------






# GERD BMI Distribution ---------------------

library(ggplot2)
library(dplyr)

set.seed(123)
n <- 200000

# Tuned parameters
bmi <- rlnorm(n, meanlog = log(36), sdlog = 0.20)

df <- data.frame(BMI = bmi)

# Check proportions (optional)
prop.table(table(cut(df$BMI,
                     breaks = c(-Inf, 20, 25, 30, 40, Inf))))


dens <- density(df$BMI, from = 15, to = 60)
df_dens <- data.frame(x = dens$x, y = dens$y)

df_dens$Group <- cut(df_dens$x,
                     breaks = c(-Inf, 20, 25, 30, 40, Inf),
                     labels = c("Underweight (<20)",
                                "Normal (20–25)",
                                "Overweight (25–30)",
                                "Obese (30–40)",
                                "Severe Obesity (>40)"))


plot <- ggplot(df_dens, aes(x = x, y = y, fill = Group)) +
  geom_area(alpha = 0.6) +
  geom_vline(xintercept = c(20,25,30,40),
             linetype = "dashed",
             color = "grey40") +
  labs(
    title = "Estimated Continuous BMI Distribution in GERD Population",
    subtitle = "Approximated BMI category proportions",
    x = "\n BMI",
    y = "Patient density \n",
    fill = "BMI Category"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values=c("#e0cf68", "#6d7780", "#8499b1", "#7b6d8d", "#36151e")) 

ggsave(file="../out/bmibreakdown.svg", plot=plot, width=7, height=4)


# ------------

# ------------

# GERD Age Distribution ------------

library(ggplot2)
library(dplyr)

set.seed(123)
n <- 300000

# Mixture weights
w_old <- 0.55
w_mid <- 0.45

# Younger/mid distribution
age_mid <- 15 + rgamma(round(n * w_mid), shape = 4.5, scale = 6)

# Older-heavy distribution (fatter right tail)
age_old <- 15 + rgamma(round(n * w_old), shape = 7, scale = 8)

age <- c(age_mid, age_old)

df_age <- data.frame(Age = age)

# Check proportions (optional)
prop.table(table(cut(df_age$Age,
                     breaks = c(15,20,30,40,50,60,70,Inf))))

dens_age <- density(df_age$Age, from = 15, to = 95)
df_dens_age <- data.frame(x = dens_age$x, y = dens_age$y)

df_dens_age$Group <- cut(df_dens_age$x,
                         breaks = c(-Inf,20,30,40,50,60,70,Inf),
                         labels = c("15–19",
                                    "20–29",
                                    "30–39",
                                    "40–49",
                                    "50–59",
                                    "60–69",
                                    "70+"))


plot <- ggplot(df_dens_age, aes(x = x, y = y, fill = Group)) +
  geom_area(alpha = 0.6) +
  geom_vline(xintercept = c(20,30,40,50,60,70),
             linetype = "dashed",
             color = "grey40") +
  labs(
    title = "Estimated Continuous Age Distribution in GERD Population",
    subtitle = "Approximated age category proportions",
    x = "\n Age (years)",
    y = "Patient density \n",
    fill = "Age Category"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c(
  "#e0cf68",  # 15-19
  "#6d7780",  # 20-29
  "#8499b1",  # 30-39
  "#7b6d8d",  # 40-49
  "#36151e",  # 50-59
  "#a85c5c",  # 60-69
  "#b8a07e"   # 70+
))

ggsave(file="../out/agebreakdown.svg", plot=plot, width=7, height=4)

# ------------

# ------------

# % GERD Prevalence Segments ------------



library(ggplot2)
library(dplyr)

# Define ranges
gerd_ranges <- tibble(
  Group = c("a) General Population", "b) GLP1 patients", "c) H. pylori", "d) Chemotherapy Patients"),
  Min = c(0.15, 0.20, 0.25, 0.35),
  Max = c(0.20, 0.35, 0.35, 0.55)
)

# Plot
plot <- ggplot(gerd_ranges) +
  geom_segment(aes(
    y = Group, yend = Group,
    x = Min*100, xend = Max*100
  ),
  size = 6, lineend = "round", color = "#8499b1"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits=c(0,70)) +
  labs(
    title = "Estimated GERD Prevalence Ranges \n",
    x = "\n % GERD Prevalence",
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
ggsave(file="../out/gerd_prev_groups.svg", plot=plot, width=7, height=2.5)

# ------------

# ------------

# GERD % Breakdown Donut ------------


# Data
gerd_buckets <- tibble(
  MECE_Group = c("GLP1-treated",
                 "Chemotherapy-induced",
                 "Rare acid diseases",
                 "Drug-induced",
                 "H. pylori–infected",
                 "Lifestyle/Other"),
  Percent = c(5, 8, 2, 10, 10, 65)
)

# Keep intended order
gerd_buckets$MECE_Group <- factor(
  gerd_buckets$MECE_Group,
  levels = gerd_buckets$MECE_Group
)

# Compute donut positions
gerd_buckets <- gerd_buckets %>%
  mutate(
    Fraction = Percent / sum(Percent),
    ymax = cumsum(Fraction),
    ymin = lag(ymax, default = 0),
    label_pos = (ymax + ymin) / 2
  )

# Plot
plot <- ggplot(gerd_buckets,
               aes(ymax = ymax, ymin = ymin,
                   xmax = 4, xmin = 3,
                   fill = MECE_Group)) +
  geom_rect(color = "white", alpha = 0.85) +
  coord_polar(theta = "y") +
  xlim(c(2, 4.6)) +
  geom_text(aes(x = 4.25,
                y = label_pos,
                label = paste0(Percent, "%")),
            size = 4,
            fontface = "bold",
            hjust = 0) +
  scale_fill_manual(values = c("#e0cf68", "#99b48b", "#914236", "#ecc6d2", "#5C449E", "#8499b1")) +
  labs(title = "Breakdown of GERD Population by Etiology") +
  theme_void() +
  theme(
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

plot

ggsave(file = "../out/donut.svg", plot = plot, width = 6, height = 5)
# ------------

# ------------

# GERD Clusters t-SNE ------------




library(MASS)
set.seed(1)


subgroups <- c("GLP1-treated",
               "Chemotherapy-induced",
               "Rare acid diseases",
               "Drug-induced",
               "H. pylori–infected",
               "Lifestyle/Other")

counts <- c(50, 80, 20, 100, 120, 630)

n_features <- 5


# Explicit means for nudging clusters
# Features: e.g., age, BMI, severity, meds, labs
cluster_means <- list(

  # Metabolic cluster
  "GLP1-treated"          = c(5, 5, 5, 5, 5),
  "Lifestyle/Other"       = c(6, 5.5, 5, 6, 5),

  # Slightly shifted but still metabolic neighborhood
  "H. pylori–infected"    = c(7, 6.5, 6, 7, 6),

  # Drug & Rare closer to each other
  "Drug-induced"          = c(10, 11, 9, 11, 9),
  "Rare acid diseases"    = c(2.5, 14.5, 13, 9.5, 10),

  # Clearly separate chemo
  "Chemotherapy-induced"  = c(15, 16, 15, 17, 16)
)


# Generate multivariate normal data
sim_data <- do.call(rbind, lapply(subgroups, function(g){

  mu <- cluster_means[[g]]

  if(g %in% c("Drug-induced", "Rare acid diseases")){
    # larger spread
    Sigma <- diag(runif(n_features, 1.3, 2.0))
  } else if(g == "H. pylori–infected"){
    Sigma <- diag(runif(n_features, 0.9, 1.4))
  } else {
    Sigma <- diag(runif(n_features, 0.5, 1))
  }

  X <- mvrnorm(n = counts[which(subgroups == g)],
               mu = mu,
               Sigma = Sigma)

  data.frame(X, Group = g)
}))



library(Rtsne)
tsne_out <- Rtsne(sim_data[,1:n_features], perplexity = 30, verbose = FALSE)
sim_data$TSNE1 <- tsne_out$Y[,1]
sim_data$TSNE2 <- tsne_out$Y[,2]


plot <- ggplot(sim_data, aes(x = TSNE1, y = TSNE2, color = Group)) +
  geom_point(alpha = 0.6, size = 2, shape=1, stroke=2) +
  scale_color_manual(values = c("#99b48b", "#ecc6d2", "#e0cf68", "#5C449E", "#8499b1", "#914236")) +
  labs(title = "Simulated GERD Subgroups (t-SNE projection clusters)",
       x = "t-SNE 1", y = "t-SNE 2") +
   theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 


plot



ggsave(file="../out/tsne.svg", plot=plot, width=7, height=6)

# ------------

# ------------

# GERD disease duration distribution ------------


# Disease duration
library(ggplot2)

# Parameters
meanlog <- log(4)
sdlog   <- 0.9

# Create grid
x <- seq(0.01, 20, length.out = 1000)
density <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)

df <- data.frame(Duration = x, Density = density)

plot <- ggplot(df, aes(x = Duration, y = Density)) +
  geom_area(fill = "#2f3e46", alpha = 0.6) +
  labs(title = "Estimated GERD Disease Duration Distribution",
       x = "\nDisease Duration (Years)",
       y = "Patient density \n") +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

ggsave(file="../out/duration_breakdown.svg", plot=plot, width=5, height=4)
# ------------

# ------------



# GERD Frequency distributin --------------


library(ggplot2)

# Grid for Beta scaling
x_unit <- seq(0.001, 0.999, length.out = 1000)
x_days <- x_unit * 365

# Individual densities
dens_mild <- 0.65 * dbeta(x_unit, shape1 = 1.2, shape2 = 8.5)     # Mild 50%
dens_mod  <- 0.30 * dbeta(x_unit, shape1 = 3, shape2 = 4.5)         # Moderate 40%
dens_sev  <- 0.05 * dbeta(x_unit, shape1 = 6, shape2 = 1.5)       # Severe 10%

# Combine into a dataframe
df <- data.frame(
  Days = rep(x_days, 3),
  Density = c(dens_mild, dens_mod, dens_sev),
  Severity = factor(rep(c("Mild / Intermittent", "Moderate", "Severe / Chronic"),
                        each = length(x_days)),
                    levels = c("Mild / Intermittent", "Moderate", "Severe / Chronic"))
)

# Plot overlapping densities
plot <- ggplot(df, aes(x = Days, y = Density, fill = Severity)) +
  geom_area(alpha = 0.6, position = "identity", color = NA) +
  scale_fill_manual(values = c("#7b6d8d", "#2f3e46", "#914236")) +
  labs(
    title = "Suggested GERD Frequency Aggregation",
    x = "\n GERD Days per Year",
    y = "Patient density\n",
    fill = "Frequency|Severity"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

ggsave(file="../out/frequency_breakdown.svg", plot=plot, width=7, height=4)

# ------------

# ------------

# Gerd Markov flows ------------



stock_names <- c("Reserve", "Acute", "Intermittent", "Chronic")

# Stocks: Reserve, Acute, Intermittent, Chronic
stock_names <- c("Reserve", "Acute", "Intermittent", "Chronic")

# Larger diagonals -> most people stay in the same bucket
P_stable <- matrix(c(
  0.95, 0.05, 0,    0,    # Reserve
  0.10, 0.80, 0.10, 0,    # Acute
  0,    0.10, 0.80, 0.10, # Intermittent
  0,    0,    0.05, 0.95  # Chronic
), nrow=4, byrow=TRUE)

rownames(P_stable) <- colnames(P_stable) <- stock_names
P_stable


library(reshape2)
library(ggplot2)

P_long <- melt(P_stable, varnames = c("From", "To"), value.name = "Probability")

ggplot(P_long, aes(x = To, y = From, fill = Probability)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Probability, 2)), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "#993333") +
  labs(
    title = "GERD Transition Probability Matrix",
    x = "To Bucket",
    y = "From Bucket",
    fill = "Probability"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


library(dplyr)
library(tidyr)
library(ggplot2)



stock_names <- c("Reserve", "Acute", "Intermittent", "Chronic")
pop <- c(Reserve = 176e6, Acute = 24e6, Intermittent = 14e6, Chronic = 6e6)

P_stable


n_years <- 20
stocks <- matrix(NA, nrow = n_years+1, ncol = 4)
stocks[1, ] <- pop

for (t in 1:n_years) {
  stocks[t+1, ] <- stocks[t, ] %*% P_stable
}

stocks_df <- as.data.frame(stocks) %>%
  mutate(Year = 0:n_years) %>%
  pivot_longer(cols = -Year, names_to = "Bucket", values_to = "Population")

ggplot(stocks_df, aes(x = Year, y = Population/1e6, color = Bucket)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("Reserve"="#99b48b",
                                "Acute"="#ecc6d2",
                                "Intermittent"="#8499b1",
                                "Chronic"="#914236")) +
  labs(
    title = "Simulated GERD Population Evolution (US adults)",
    x = "Year",
    y = "Population (millions)",
    color = "Bucket"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )





# ------------

# ------------






# Needs likelihood distribution per GERD source ------------



library(ggplot2)
library(dplyr)

set.seed(42)
n <- 2000

groups <- c("GLP1-treated", "Chemotherapy-induced", "Rare acid diseases",
            "Drug-induced", "H. pylori–infected", "Lifestyle/Other")

# Mean high/very-high need per group
means <- c(0.05, 0.3, 0.6, 0.1, 0.15, 0.05)

# Approximate dispersion: smaller = tighter around mean, larger = more spread
sds <- c(0.03, 0.1, 0.05, 0.05, 0.08, 0.05)

# Function to convert mean & sd to Beta parameters
beta_params <- function(mu, sigma){
  alpha <- ((1 - mu)/sigma^2 - 1/mu)*mu^2
  beta <- alpha*(1/mu - 1)
  return(c(alpha, beta))
}

sim_data <- do.call(rbind, lapply(1:length(groups), function(i){
  pars <- beta_params(means[i], sds[i])
  tibble(
    Group = groups[i],
    NeedScore = rbeta(n, pars[1], pars[2])
  )
}))

# Plot overlapping densities
plot <- ggplot(sim_data, aes(x = NeedScore, fill = Group, color = Group)) +
  geom_density(alpha = 0.7, size = 0.00001) +
  scale_fill_manual(values = c("#99b48b", "#ecc6d2", "#e0cf68", "#5C449E", "#8499b1", "#914236")) +
  scale_color_manual(values = c("#99b48b", "#ecc6d2", "#e0cf68", "#5C449E", "#8499b1", "#914236")) +
  labs(title = "Estimated Proportion of Possibly 'In-need' For Alternative Therapy",
       subtitle = "By GERD Subgroup|Source",
       x = "\n Proportion of Possibly 'In-need' For Alternative Therapy",
       y = "Patient density \n") +
  xlim(0,1) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 1),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(file="../out/needs_density.svg", plot=plot, width=7, height=4)


# ------------

# ------------
