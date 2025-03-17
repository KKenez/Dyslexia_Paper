# Function to calculate net benefits
calculate_net_benefits <- function(df, thresholds = seq(0, 1, by = 0.01), probabilities_column, outcomes_column) {
  net_benefits <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    threshold_value <- thresholds[i]
    
    # Avoid extreme threshold values
    if (threshold_value >= 0.99) {
      net_benefits[i] <- NA
      next
    }
    
    # Apply threshold and calculate net benefit
    df_temp <- df %>%
      mutate(Prediction = as.numeric(.data[[probabilities_column]] >= threshold_value))
    
    # Compute components
    TP <- sum(df_temp$Prediction == 1 & df_temp[[outcomes_column]] == 1)
    FP <- sum(df_temp$Prediction == 1 & df_temp[[outcomes_column]] == 0)
    N <- nrow(df)
    
    net_benefits[i] <- (TP / N) - (FP / N) * threshold_value / (1 - threshold_value)
  }
  
  # Return a dataframe
  data.frame(Threshold = thresholds, NetBenefit = net_benefits)
}

