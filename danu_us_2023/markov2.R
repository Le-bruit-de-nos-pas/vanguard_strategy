set.seed(123)  # For reproducibility

# Step 1: Initialize Patients
stock_labels <- c("Lapsed", "Biguanide", "Antidiabetic", "DPP4", "SGLT2", "Insulin", "Oral GLP1", "Inj. GLP1")

initial_state <- c(12351, 5773, 2114, 1006, 1507, 3824, 288, 3318)

# Create a patient-level dataset (each patient assigned to a stock)
patients <- unlist(mapply(rep, stock_labels, initial_state))

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

simulate_patients <- function(patients, P, months) {
  
  history <- matrix(0, nrow = months, ncol = length(stock_labels))
  colnames(history) <- stock_labels
  #print(history)
  
  for (month in 1:months) {
    
    #print(history)
    # Count patients in each stock
    history[month, ] <- table(factor(patients, levels = stock_labels))
    #print(history)
    
    # Sample movements for each stock
    new_patients <- patients
 
    
    for (stock in stock_labels) {
      stock_idx <- which(patients == stock)
      num_patients <- length(stock_idx)
      if (num_patients == 0) next
  
      probs <- P[stock, ]
  
      # Step 1: calculate how many go to each destination (floor to avoid rounding issues)
      n_dest <- floor(num_patients * probs)
      remainder <- num_patients - sum(n_dest)
  
      # Step 2: randomly allocate remaining patients to destinations (to match total count)
      if (remainder > 0) {
        extra_indices <- sample(seq_along(probs), size = remainder, prob = probs, replace = TRUE)
        for (i in extra_indices) {
          n_dest[i] <- n_dest[i] + 1
        }
      }
  
      # Step 3: assign patients accordingly
      start <- 1
      for (i in seq_along(stock_labels)) {
        end <- start + n_dest[i] - 1
        if (n_dest[i] > 0) {
          new_patients[stock_idx[start:end]] <- stock_labels[i]
          start <- end + 1
        }
      }
    }
    
    patients <- new_patients
    #print(patients)
  }
  
  return(history)
}



# Run for 24 months
months <- 48

history <- simulate_patients(patients, P, months)

# Step 4: Convert to Data Frame for Visualization
df <- as.data.frame(history)
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
