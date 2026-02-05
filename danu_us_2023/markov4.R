set.seed(123)  # For reproducibility

# Step 1: Initialize Patients
stock_labels <- c("Lapsed", "Biguanide", "Antidiabetic", "DPP4", "SGLT2", "Insulin", "Oral GLP1", "Inj. GLP1")

initial_state <- c(12351, 5773, 2114, 1006, 1507, 3824, 288, 3318)

# Assign random BMI
total_patients <- sum(initial_state)
patient_bmi <- rnorm(total_patients, mean = 30, sd = 5)

# Sort BMI descending
bmi_order <- order(patient_bmi, decreasing = TRUE)
patient_bmi <- patient_bmi[bmi_order]

# Stock order from highest to lowest BMI preference
priority_stocks <- c("Inj. GLP1", "Oral GLP1", "Insulin", "SGLT2", "DPP4", "Antidiabetic", "Biguanide", "Lapsed")
initial_state_priority <- initial_state[match(priority_stocks, stock_labels)]

# Assign stock labels based on sorted BMI
patients <- rep(NA, total_patients)
start <- 1

for (i in seq_along(priority_stocks)) {
  end <- start + initial_state_priority[i] - 1
  patients[start:end] <- priority_stocks[i]
  start <- end + 1
}

# Reorder stock_labels to maintain the original order
patients <- factor(patients, levels = stock_labels)



# Step 2: Define Transition Matrix
P <- matrix(c(
  0.97, 0.01, 0.01, 0.00, 0.00, 0.00, 0.00, 0.01,  
  0.02, 0.90, 0.02, 0.02, 0.02, 0.01, 0.01, 0.00,  
  0.01, 0.03, 0.85, 0.04, 0.02, 0.02, 0.02, 0.01,  
  0.01, 0.03, 0.04, 0.85, 0.02, 0.02, 0.02, 0.01,  
  0.01, 0.02, 0.02, 0.02, 0.85, 0.02, 0.04, 0.02,  
  0.01, 0.01, 0.02, 0.02, 0.02, 0.85, 0.03, 0.04,  
  0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.90, 0.06,  
  0.00, 0.00, 0.00, 0.00, 0.02, 0.02, 0.05, 0.90   
), byrow = TRUE, nrow = 8)


colnames(P) <- stock_labels
rownames(P) <- stock_labels

print(P)

# Step 3: Simulate Over Time



simulate_patients <- function(patients, patient_bmi, P, months) {
  
  history <- matrix(0, nrow = months, ncol = length(stock_labels))
  colnames(history) <- stock_labels
  
  bmi_history <- matrix(NA, nrow = months, ncol = length(stock_labels))
  colnames(bmi_history) <- stock_labels
  
  for (month in 1:months) {
    
    # Record patient counts
    history[month, ] <- table(factor(patients, levels = stock_labels))
    
    # Record average BMI per stock
    for (i in seq_along(stock_labels)) {
      stock <- stock_labels[i]
      in_stock <- patients == stock
      if (any(in_stock)) {
        bmi_history[month, i] <- mean(patient_bmi[in_stock])
      }
    }

    new_patients <- patients
    
    for (stock in stock_labels) {
      stock_idx <- which(patients == stock)
      num_patients <- length(stock_idx)
      if (num_patients == 0) next

      probs <- P[stock, ]
      n_dest <- floor(num_patients * probs)
      remainder <- num_patients - sum(n_dest)

      # Randomly distribute remaining
      if (remainder > 0) {
        extra_indices <- sample(seq_along(probs), size = remainder, prob = probs, replace = TRUE)
        for (i in extra_indices) {
          n_dest[i] <- n_dest[i] + 1
        }
      }

      # Sort current patients by BMI (descending)
      stock_bmi <- patient_bmi[stock_idx]
      bmi_order <- order(stock_bmi, decreasing = TRUE)
      sorted_idx <- stock_idx[bmi_order]

      # Assign patients to destination stocks according to BMI ranking
      start <- 1
      for (i in seq_along(stock_labels)) {
        end <- start + n_dest[i] - 1
        if (n_dest[i] > 0) {
          target_indices <- sorted_idx[start:end]
          new_patients[target_indices] <- stock_labels[i]
          start <- end + 1
        }
      }
    }

    # Adjust BMI based on movements into/out of GLP1s
    moved_in_glp1 <- (new_patients %in% c("Oral GLP1", "Inj. GLP1")) & !(patients %in% c("Oral GLP1", "Inj. GLP1"))
    moved_out_glp1 <- !(new_patients %in% c("Oral GLP1", "Inj. GLP1")) & (patients %in% c("Oral GLP1", "Inj. GLP1"))

    patient_bmi[moved_in_glp1] <- patient_bmi[moved_in_glp1] * 1.10
    patient_bmi[moved_out_glp1] <- patient_bmi[moved_out_glp1] * 0.90

    # Finalize new patient states
    patients <- new_patients
  }

  return(list(counts = history, bmi = bmi_history))
}




# Run for 24 months
months <- 48

history <- simulate_patients(patients, patient_bmi, P, months)

# Step 4: Convert to Data Frame for Visualization
df <- as.data.frame(history$counts)
df$Month <- 1:months
print(df)

df_long <- tidyr::pivot_longer(df, cols = -Month, names_to = "Stock", values_to = "Patients")

# Custom Colors
custom_colors <- c(
  "Lapsed" = "#b2b2b2",        
  "Biguanide" = "#b4a2cb",    
  "Antidiabetic" = "#2fa0d5",  
  "DPP4" = "#2c3c96",         
  "SGLT2" = "#dcca18",        
  "Insulin" = "#66A61E",      
  "Oral GLP1" = "#e14b4b",    
  "Inj. GLP1" = "#a02424"     
)

# Step 5: Plot the Evolution
library(ggplot2)

ggplot(df_long, aes(x = Month, y = Patients, color = Stock)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = custom_colors) +  
  theme_minimal(base_size = 14) +
  labs(title = "Stocks Distribution Over Time",
       subtitle = "Simulating individual patient transitions",
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


df_bmi <- as.data.frame(history$bmi)
df_bmi$Month <- 1:months
# Add Month column
df_bmi$Month <- 1:nrow(df_bmi)

df_bmi_long <- tidyr::pivot_longer(df_bmi, cols = -Month, names_to = "Stock", values_to = "Avg_BMI")

ggplot(df_bmi_long, aes(x = Month, y = Avg_BMI, color = Stock)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  labs(title = "Average BMI by Stock Over Time",
       x = "Month", y = "Average BMI", color = "Stock") +
  coord_cartesian(ylim=c(20,40))

