P <- matrix(c(
  # Lapsed   Biguanide  Antidiabetic  DPP4   SGLT2  Insulin  Oral GLP1  Inj. GLP1
  0.97,     0.01,      0.01,         0.00,  0.00,  0.00,    0.00,      0.01,    # Lapsed (mostly stays)
  0.02,     0.90,      0.02,         0.02,  0.02,  0.01,    0.01,      0.00,    # Biguanide → small shifts
  0.01,     0.03,      0.85,         0.04,  0.02,  0.02,    0.02,      0.01,    # Antidiabetic → GLP1 increases
  0.01,     0.03,      0.04,         0.85,  0.02,  0.02,    0.02,      0.01,    # DPP4 → GLP1 increases
  0.01,     0.02,      0.02,         0.02,  0.85,  0.02,    0.04,      0.02,    # SGLT2 → More to GLP1
  0.01,     0.01,      0.02,         0.02,  0.02,  0.85,    0.03,      0.04,    # Insulin → More to GLP1
  0.00,     0.00,      0.00,         0.01,  0.01,  0.02,    0.90,      0.06,    # Oral GLP1 increasing
  0.00,     0.00,      0.00,         0.00,  0.02,  0.02,    0.05,      0.90     # Injectable GLP1 increasing
), byrow = TRUE, nrow = 8)



initial_state <- c(12351, 5773, 2114, 1006, 1507, 3824, 288, 3318)  # Lapsed, Biguanide, Antidiabetic, DPP4, SGLT2, Insulin, Oral GLP1, Injectable GLP1


simulate_markov <- function(P, state, months) {
  history <- matrix(0, nrow = months, ncol = length(state))
  print(history)
  
  history[1, ] <- state
  print(history)
  
  for (t in 2:months) {
    print(history)
    history[t, ] <- history[t-1, ] %*% P  # Matrix multiplication
    print(history)
  }
  
  return(history)
}


# Simulate for 24 months
months <- 24

history <- simulate_markov(P, initial_state, months)

# Convert to data frame for visualization
df <- as.data.frame(history)
colnames(df) <- c("Lapsed", "Biguanide", "Antidiabetic", "DPP4", "SGLT2", "Insulin", "Oral GLP1", "Inj. GLP1")
df$Month <- 1:months
print(df)

# Plot the evolution
library(ggplot2)

df_long <- tidyr::pivot_longer(df, cols = -Month, names_to = "Drug", values_to = "Patients")

custom_colors <- c(
  "Lapsed" = "#b2b2b2",        # Dark Gray
  "Biguanide" = "#b4a2cb",     # Teal Green
  "Antidiabetic" = "#2fa0d5",  # Burnt Orange
  "DPP4" = "#2c3c96",         # Muted Purple
  "SGLT2" = "#dcca18",        # Deep Pink
  "Insulin" = "#66A61E",      # Olive Green
  "Oral GLP1" = "#e14b4b",    # Gold
  "Inj. GLP1" = "#a02424"     # Brown
)


ggplot(df_long, aes(x = Month, y = Patients, color = Drug)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal(base_size = 14) +
  labs(title = "Stocks Distribution Over Time",
       subtitle = "Tracking the changes in stock allocations",
       x = "Month", y = "Number of Patients", color = "Stock") +
  theme(legend.position = "right",
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12)) +
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
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

