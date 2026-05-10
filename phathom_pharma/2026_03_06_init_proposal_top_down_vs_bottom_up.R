
library(ggplot2)
library(dplyr)
library(maps)
library(tidyverse)


# Data
data <- data.frame(
  Segment = c("Rare acid", "GLP1", "Chemo/Cancer", "NSAID/Opioid", "H pylori", "Lifestyle"),
  Population_M = c(0.9, 9.0, 10.3, 28.1, 25.0, 172.1)
)

data <- data %>% arrange(Population_M) %>% mutate(Segment=as.factor(Segment, order=Segment))

# Bar plot
plot <- ggplot(data, aes(x = reorder(Segment, -Population_M), y = Population_M, fill=Segment)) +
  geom_bar(stat = "identity", alpha=0.8) +
  labs(
    title = "Hypothetical Population Size (Insured US Adults)",
    x = "\nUS Population Segment",
    y = "Hypothetical Population (millions)\n"
  ) +
  geom_text(aes(label = round(Population_M,1)), 
            vjust = -0.3, size = 4) +
   theme_minimal() +
  scale_fill_manual(values = c("#e0cf68", "#99b48b", "#5C449E", "#8499b1", "#ecc6d2", "#914236")) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


plot
ggsave(file = "../out/donut.svg", plot = plot, width = 8, height = 5)







# Data
data <- data.frame(
  Segment = c("Rare acid", "GLP1", "Chemo/Cancer", "NSAID/Opioid", "H pylori", "Lifestyle"),
  Population_M = c(0.9, 9.0, 10.3, 28.1, 25.0, 172.1),
  GERD_prev = c(1.00, 0.25, 0.35, 0.16, 0.18, 0.17)
)

data <- data %>%
  arrange(desc(Population_M)) %>%
  mutate(Segment = factor(Segment, levels = Segment))

# Compute shares
data <- data %>%
  mutate(
    GERD = GERD_prev,
    No_GERD = 1 - GERD_prev
  ) 

# Long format for stacked bars
data_long <- data %>%
  select(Segment, GERD, No_GERD) %>%
  pivot_longer(cols = c(GERD, No_GERD),
               names_to = "Status",
               values_to = "Percent")

# Plot
plot <- ggplot(data_long,
               aes(x = reorder(Segment, -Percent), y = Percent, fill = Status)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "GERD vs Non-GERD Share by Population Segment",
    x = "\nUS Population Segment",
    y = "Share of Segment\n"
  ) +
    geom_text(
    aes(label = scales::percent(Percent, accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#914236", "#8499b1")) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot
ggsave(file = "../out/donut.svg", plot = plot, width = 8, height = 5)




# Compute GERD population
data <- data %>%
  mutate(GERD_M = Population_M * GERD_prev) %>%
  arrange(Population_M) %>%
  mutate(Segment = factor(Segment, levels = Segment))

# Plot
plot <- ggplot(data, aes(x = reorder(Segment, -Population_M), y = GERD_M, fill = Segment)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(GERD_M,1)),
            vjust = -0.3, size = 4) +
  labs(
    title = "Absolute GERD Population by Segment",
    x = "\nUS Population Segment",
    y = "GERD Population (millions)\n"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#914236", "#e0cf68", "#99b48b", "#5C449E", "#ecc6d2", "#8499b1")) +

  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

plot

ggsave(file = "../out/donut.svg", plot = plot, width = 8, height = 5)




library(ggplot2)
library(dplyr)

# Data: absolute GERD patients and Rx treated
data <- data.frame(
  Segment = c("Rare acid", "GLP1", "Chemo/Cancer", "NSAID/Opioid", "H pylori", "Lifestyle"),
  GERD_M = c(0.9, 2.25, 3.6, 4.5, 4.5, 29.25),
  Rx_M = c(0.86, 1.46, 2.88, 3.15, 2.70, 14.95)
)

# Maintain same order as previous charts
data <- data %>%
  arrange(-GERD_M) %>%
  mutate(Segment = factor(Segment, levels = Segment))

# Plot
plot <- ggplot(data, aes(x = Segment, y = Rx_M, fill = Segment)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Rx_M,1)),
            vjust = -0.3, size = 4) +
  labs(
    title = "Absolute GERD Patients on Rx by Segment",
    x = "\nUS Population Segment",
    y = "Rx-treated GERD Population (millions)\n"
  ) +
  theme_minimal() +
    scale_fill_manual(values = c("#8499b1", "#ecc6d2", "#5C449E", "#99b48b", "#e0cf68", "#914236")) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

plot

ggsave(file = "../out/donut.svg", plot = plot, width = 8, height = 5)

