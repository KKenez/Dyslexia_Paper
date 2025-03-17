test_model <- function(data, dataid, validationdata, validationdataid, modelidentifier, paradigm, gameregressors, backgroundregressors, outcome, ...) {
  library(caret)
  # data: the dataset for training and evaluation
  # dataid: the identifier of the data
  # modelindentifier: the model's unique identifier, could be anything
  # paradigm: "bayesian", "logistic", "random_forest"
  # gameregressors: the regressors in the model which originate from the dyslexia game itself
  # backgroundregressors: the regressors in the model which come from the background variables
  # outcome: outcome variable ("Dyslexia")
  # ...: all things the models themselves need, for example gameprior, backgroundprior for bayesian model
  
  # Setup part
  start_time <- Sys.time()
  extra_args <- list(...)
  accuracy <- NA
  precision <- NA
  recall <- NA
  decisionthreshold <- 0.24
  
  #TODO: Finish the results dataframe
  results <- data.frame(
    ID = modelidentifier,
    Paradigm = paradigm,
    Gameregressors = paste(gameregressors, collapse = ", "),
    Backgroundregressors = paste(backgroundregressors, collapse = ", "),
    Outcome = outcome,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    Runtime = NA,  # Will be set at the end
    stringsAsFactors = FALSE
  )
  
  # This chunk adds extra arguments as columns in the results dataframe
  if (length(extra_args) > 0) {
    for (arg_name in names(extra_args)) {
      results[[arg_name]] <- extra_args[[arg_name]]
    }
  }
  
  # Modelling part
  if (paradigm == "bayesian") {
    library(brms)
    
    formula <- bf(outcome ~ game_effect + background_effect, nl = TRUE) +
      lf(game_effect ~ paste(gameregressors, collapse = "+")) +
      lf(background_effect ~ paste(backgroundregressors, collapse = "+"))
    
    gameprior <- extra_args[["gameprior"]]
    backgroundprior <- extra_args[["backgroundprior"]]
    
    priors <- c(gameprior, backgroundprior)
    
    model <- brm(
      formula = formula,
      prior = priors,
      family = bernoulli(link = 'logit'),
      data = data,
      ...
    )
    
    # Saving model, results
    saveRDS(model, file = paste0(modelidentifier, ".rds"))
    
    model_predicted_probabilities <- fitted(model, newdata = data)
    predicted_probs <- model_predicted_probabilities[, 1]  # Extract mean probabilities
    
    predicted_classes <- ifelse(predicted_probs > decisionthreshold, 1, 0)
    
    predicted_classes <- factor(predicted_classes, levels = c(0, 1))
    actual_classes <- factor(data[[outcome]], levels = c(0, 1))
    
    cm <- confusionMatrix(predicted_classes, actual_classes)
    
    # Extract values
    results$FN <- cm$table[2, 1]  # False Negatives
    results$TN <- cm$table[1, 1]  # True Negatives
    results$FP <- cm$table[1, 2]  # False Positives
    results$TP <- cm$table[2, 2]  # True Positives
    
    # Store performance metrics
    results$Accuracy <- cm$overall["Accuracy"]
    results$Precision <- cm$byClass["Precision"]
    results$Recall <- cm$byClass["Recall"]
  
  }
  
  else if (paradigm == "logistic") {
    library(glmnet)
    
    x <- model.matrix(as.formula(paste(outcome, "~ . - 1")), data = data)
    y <- data[[outcome]]
    
    model <- cv.glmnet(x, y, family = "binomial", alpha = 0.5)
    
    results$predictions <- predict(model, newx = x, type = "response")
    results$model <- model
    saveRDS(model, file = paste0(modelidentifier, ".rds"))
  }
  
  else if (paradigm == "random_forest") {
    library(randomForest)
    
    model <- randomForest(as.formula(paste(outcome, "~", paste(regressors, collapse = "+"))), data = data, ntree = 200)
    
    results$predictions <- predict(model, newdata = data, type = "prob")[, 2]
    results$model <- model
    saveRDS(model, file = paste0(modelidentifier, ".rds"))
  }
  
  else {
    stop("Unsupported modeling paradigm")
  }
  
  end_time <- Sys.time()
  results$runtime <- end_time - start_time
  
  return(results)
}