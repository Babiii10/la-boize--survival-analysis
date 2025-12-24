options(xtable.include.colnames=T)
options(xtable.include.rownames=T)
#Packages
#rm(list=ls())
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("missMDA")#imputepca
usePackage("ggplot2")#Graphs
usePackage("stats")
usePackage("tidyr")
usePackage("e1071")#svm
usePackage("pROC")#roccurve
usePackage("devtools")
usePackage("readxl")
usePackage("superml")
usePackage("shiny")
usePackage("shinythemes")
usePackage("bslib")
# if (!is.element("factoextra", installed.packages()[,1]))
#   install_github("kassambara/factoextra")
#usePackage("factoextra")#PCA graphs
usePackage("reshape2")#melt function
usePackage("xlsx")#import fichier xls#Fonctions
usePackage("randomForest")
usePackage("missForest")
usePackage("Hmisc")
usePackage("corrplot")
usePackage("penalizedSVM")
usePackage("DT")
usePackage("shinycssloaders")
usePackage("writexl")
usePackage("glmnet")#for lasso, elasticnet, ridge regression
usePackage("survival")#for cox regression and survival analysis
usePackage("survminer")#for Kaplan-Meier plots and survival visualization
usePackage("ranger")#for Random Survival Forest
usePackage("riskRegression")#for C-index and prediction metrics
usePackage("pec")#for prediction error curves and Brier score
usePackage("prodlim")#for product limit estimation (required by pec)
usePackage("timeROC")#for time-dependent ROC curves with censoring
usePackage("xgboost")#for xgboost gradient boosting
usePackage("lightgbm")#for lightgbm gradient boosting
usePackage("class")#for k-nearest neighbors
usePackage("shinyFeedback")#for user feedback in UI


##########################
# Survival Analysis Helper Functions
##########################

# Calculate C-index (Concordance Index / Harrell's C-statistic)
# Measures discriminative ability of survival models (0.5 = random, 1.0 = perfect)
calculate_cindex <- function(predicted_risk, time, status){
  tryCatch({
    # Use Hmisc's rcorr.cens which calculates C-index
    # predicted_risk should be higher for higher risk (shorter survival)
    result <- rcorr.cens(-predicted_risk, Surv(time, status))
    # Return C-index (Dxy/2 + 0.5 = C-index, or use result["C Index"])
    return(as.numeric(result["C Index"]))
  }, error = function(e) {
    warning(paste("Error calculating C-index:", e$message))
    return(NA)
  })
}

# Calculate Integrated Brier Score (IBS)
# Lower is better (0 = perfect predictions)
calculate_ibs <- function(model, data, time_col, status_col, times = NULL, formula = NULL){
  tryCatch({
    if(is.null(times)){
      # Use quartiles of observed event times
      event_times <- data[data[[status_col]] == 1, time_col]
      times <- quantile(event_times, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    }

    if(is.null(formula)){
      formula <- as.formula(paste("Surv(", time_col, ",", status_col, ") ~ 1"))
    }

    # Calculate prediction error using pec package
    pec_result <- pec(object = list("model" = model),
                      formula = formula,
                      data = data,
                      times = times,
                      exact = FALSE,
                      cens.model = "marginal",
                      splitMethod = "none",
                      B = 0,
                      verbose = FALSE)

    # Extract Integrated Brier Score
    ibs <- crps(pec_result, times = times)[2]  # [1] is reference, [2] is model
    return(as.numeric(ibs))
  }, error = function(e) {
    warning(paste("Error calculating IBS:", e$message))
    return(NA)
  })
}

# Calculate Brier Score at specific time point
calculate_brier_at_time <- function(model, data, time_col, status_col, time_point, formula = NULL){
  tryCatch({
    if(is.null(formula)){
      formula <- as.formula(paste("Surv(", time_col, ",", status_col, ") ~ 1"))
    }

    pec_result <- pec(object = list("model" = model),
                      formula = formula,
                      data = data,
                      times = time_point,
                      exact = FALSE,
                      cens.model = "marginal",
                      splitMethod = "none",
                      B = 0,
                      verbose = FALSE)

    # Extract Brier Score at time point
    bs <- pec_result$AppErr$model[1]
    return(as.numeric(bs))
  }, error = function(e) {
    warning(paste("Error calculating Brier Score:", e$message))
    return(NA)
  })
}

# Extract predicted risk scores from survival models
# Returns linear predictor (risk score) - higher = worse prognosis
get_risk_scores <- function(model, newdata, model_type = "cox"){
  tryCatch({
    if(model_type == "cox" || model_type == "coxnet"){
      # Cox model: use linear predictor
      if(inherits(model, "coxph")){
        risk_scores <- predict(model, newdata = newdata, type = "lp")
      } else if(inherits(model, "cv.glmnet")){
        # For glmnet cox models
        x_matrix <- as.matrix(newdata[, -c(1:2)])  # Exclude time and status
        risk_scores <- predict(model, newx = x_matrix, s = "lambda.min", type = "link")[,1]
      }
    } else if(model_type == "rsf" || model_type == "ranger"){
      # Random Survival Forest
      if(inherits(model, "ranger")){
        # For ranger, use predicted mortality (CHF at last time)
        pred <- predict(model, data = newdata)
        risk_scores <- pred$chf[, ncol(pred$chf)]  # Cumulative hazard at last time
      }
    } else {
      warning(paste("Unknown model type:", model_type))
      return(NULL)
    }
    return(risk_scores)
  }, error = function(e) {
    warning(paste("Error extracting risk scores:", e$message))
    return(NULL)
  })
}

# Calculate median survival time from survival curves
get_median_survival <- function(model, newdata = NULL, model_type = "cox"){
  tryCatch({
    if(is.null(newdata)){
      # Get median from training data (embedded in model)
      if(inherits(model, "coxph")){
        surv_obj <- survfit(model)
        median_surv <- summary(surv_obj)$table["median"]
      } else if(inherits(model, "ranger")){
        # For ranger, calculate median from prediction
        median_surv <- median(model$survival.times, na.rm = TRUE)
      }
    } else {
      # Calculate median for new data
      if(inherits(model, "coxph")){
        surv_obj <- survfit(model, newdata = newdata)
        median_surv <- summary(surv_obj)$table["median"]
      } else if(inherits(model, "ranger")){
        pred <- predict(model, data = newdata)
        # Extract median survival time (requires survival matrix)
        median_surv <- NA  # Simplified - would need proper calculation
      }
    }
    return(as.numeric(median_surv))
  }, error = function(e) {
    warning(paste("Error calculating median survival:", e$message))
    return(NA)
  })
}

##########################
# Survival Model Building Functions
##########################

# Fit Cox Proportional Hazards Model
fit_cox_model <- function(data, time_col = "time", status_col = "status", covariates = NULL){
  tryCatch({
    # Build formula
    if(is.null(covariates)){
      # Use all columns except time and status
      covariates <- setdiff(colnames(data), c(time_col, status_col))
    }

    formula_str <- paste("Surv(", time_col, ",", status_col, ") ~", paste(covariates, collapse = " + "))
    formula_obj <- as.formula(formula_str)

    # Fit Cox model
    cox_model <- coxph(formula_obj, data = data)

    return(cox_model)
  }, error = function(e) {
    warning(paste("Error fitting Cox model:", e$message))
    return(NULL)
  })
}

# Fit Cox model with Lasso/ElasticNet/Ridge penalization
fit_coxnet_model <- function(data, time_col = "time", status_col = "status", alpha = 1, nfolds = 10){
  tryCatch({
    # Prepare data
    x_matrix <- as.matrix(data[, !colnames(data) %in% c(time_col, status_col)])
    y_surv <- Surv(data[[time_col]], data[[status_col]])

    # Fit penalized Cox model with cross-validation
    # alpha = 1 for Lasso, alpha = 0 for Ridge, 0 < alpha < 1 for ElasticNet
    cv_model <- cv.glmnet(x = x_matrix, y = y_surv,
                          family = "cox",
                          alpha = alpha,
                          nfolds = nfolds,
                          type.measure = "C")

    return(cv_model)
  }, error = function(e) {
    warning(paste("Error fitting penalized Cox model:", e$message))
    return(NULL)
  })
}

# Fit Random Survival Forest using ranger
fit_rsf_model <- function(data, time_col = "time", status_col = "status",
                          num_trees = 500, mtry = NULL, min_node_size = NULL){
  tryCatch({
    # Build formula
    formula_str <- paste("Surv(", time_col, ",", status_col, ") ~ .")
    formula_obj <- as.formula(formula_str)

    # Set default parameters if not provided
    n_features <- ncol(data) - 2  # Exclude time and status
    if(is.null(mtry)){
      mtry <- floor(sqrt(n_features))
    }
    if(is.null(min_node_size)){
      min_node_size <- 5
    }

    # Fit Random Survival Forest
    rsf_model <- ranger(formula_obj,
                        data = data,
                        num.trees = num_trees,
                        mtry = mtry,
                        min.node.size = min_node_size,
                        importance = "permutation",
                        splitrule = "logrank",
                        verbose = FALSE,
                        seed = 42)

    return(rsf_model)
  }, error = function(e) {
    warning(paste("Error fitting Random Survival Forest:", e$message))
    return(NULL)
  })
}

# Tune Random Survival Forest hyperparameters
tune_rsf_model <- function(data, time_col = "time", status_col = "status",
                           ntree_values = c(100, 500, 1000),
                           mtry_values = NULL,
                           nodesize_values = c(3, 5, 10)){
  tryCatch({
    n_features <- ncol(data) - 2

    if(is.null(mtry_values)){
      mtry_values <- c(floor(sqrt(n_features)), floor(n_features/3), floor(n_features/2))
    }

    # Grid search
    best_cindex <- 0
    best_params <- list(ntree = 500, mtry = floor(sqrt(n_features)), nodesize = 5)

    for(nt in ntree_values){
      for(mt in mtry_values){
        for(ns in nodesize_values){
          model <- fit_rsf_model(data, time_col, status_col,
                                num_trees = nt, mtry = mt, min_node_size = ns)

          if(!is.null(model)){
            # Get OOB prediction error as proxy for C-index
            cindex <- model$prediction.error  # Actually concordance error, need to convert

            if(cindex > best_cindex){
              best_cindex <- cindex
              best_params <- list(ntree = nt, mtry = mt, nodesize = ns)
            }
          }
        }
      }
    }

    # Fit final model with best parameters
    final_model <- fit_rsf_model(data, time_col, status_col,
                                 num_trees = best_params$ntree,
                                 mtry = best_params$mtry,
                                 min_node_size = best_params$nodesize)

    final_model$best_params <- best_params

    return(final_model)
  }, error = function(e) {
    warning(paste("Error tuning RSF model:", e$message))
    return(NULL)
  })
}

##########################
# Survival Statistical Tests Functions
##########################

# Perform Log-rank test for each variable
perform_logrank_test <- function(data, time_col = "time", status_col = "status"){
  tryCatch({
    # Variables to test (exclude time and status)
    vars_to_test <- setdiff(colnames(data), c(time_col, status_col))

    results <- data.frame(
      variable = vars_to_test,
      pvalue = NA,
      chisq = NA,
      stringsAsFactors = FALSE
    )

    for(i in seq_along(vars_to_test)){
      var <- vars_to_test[i]

      # Dichotomize variable by median (for continuous variables)
      tryCatch({
        if(is.numeric(data[[var]])){
          var_groups <- ifelse(data[[var]] >= median(data[[var]], na.rm = TRUE), "High", "Low")
        } else {
          var_groups <- as.factor(data[[var]])
        }

        # Log-rank test
        test_result <- survdiff(Surv(data[[time_col]], data[[status_col]]) ~ var_groups)
        pval <- 1 - pchisq(test_result$chisq, df = length(levels(as.factor(var_groups))) - 1)

        results$pvalue[i] <- pval
        results$chisq[i] <- test_result$chisq
      }, error = function(e){
        results$pvalue[i] <- NA
        results$chisq[i] <- NA
      })
    }

    return(results)
  }, error = function(e) {
    warning(paste("Error performing log-rank test:", e$message))
    return(NULL)
  })
}

# Perform univariate Cox regression (Wald test)
perform_cox_univariate <- function(data, time_col = "time", status_col = "status"){
  tryCatch({
    vars_to_test <- setdiff(colnames(data), c(time_col, status_col))

    results <- data.frame(
      variable = vars_to_test,
      hazard_ratio = NA,
      HR_lower_95 = NA,
      HR_upper_95 = NA,
      pvalue = NA,
      coefficient = NA,
      stringsAsFactors = FALSE
    )

    for(i in seq_along(vars_to_test)){
      var <- vars_to_test[i]

      formula_str <- paste("Surv(", time_col, ",", status_col, ") ~", var)

      tryCatch({
        cox_model <- coxph(as.formula(formula_str), data = data)
        coef_summary <- summary(cox_model)$coefficients
        conf_int <- summary(cox_model)$conf.int

        results$coefficient[i] <- coef_summary[1, "coef"]
        results$hazard_ratio[i] <- exp(coef_summary[1, "coef"])
        results$pvalue[i] <- coef_summary[1, "Pr(>|z|)"]

        if(!is.null(conf_int) && nrow(conf_int) > 0){
          results$HR_lower_95[i] <- conf_int[1, "lower .95"]
          results$HR_upper_95[i] <- conf_int[1, "upper .95"]
        }
      }, error = function(e){
        results$hazard_ratio[i] <- NA
        results$pvalue[i] <- NA
      })
    }

    return(results)
  }, error = function(e) {
    warning(paste("Error performing Cox univariate test:", e$message))
    return(NULL)
  })
}

##########################
# Survival Visualization Functions
##########################

# Plot Kaplan-Meier survival curves
plot_kaplan_meier <- function(time, status, risk_groups = NULL, title = "Kaplan-Meier Survival Curve",
                              show_risk_table = TRUE, show_conf_int = TRUE){
  tryCatch({
    # Create data frame
    if(is.null(risk_groups)){
      plot_data <- data.frame(time = time, status = status)
      fit <- survfit(Surv(time, status) ~ 1, data = plot_data)
      show_pval <- FALSE
    } else {
      plot_data <- data.frame(time = time, status = status, group = risk_groups)
      fit <- survfit(Surv(time, status) ~ group, data = plot_data)
      show_pval <- TRUE
    }

    # Create plot
    p <- ggsurvplot(
      fit,
      data = plot_data,
      risk.table = show_risk_table,
      pval = show_pval,
      conf.int = show_conf_int,
      xlim = c(0, max(time, na.rm = TRUE)),
      break.time.by = max(time, na.rm = TRUE) / 10,
      ggtheme = theme_minimal(),
      title = title,
      xlab = "Time",
      ylab = "Survival probability",
      legend.title = "Group",
      legend.labs = if(!is.null(risk_groups)) levels(as.factor(risk_groups)) else NULL,
      palette = c("#E7B800", "#2E9FDF", "#FC4E07"),
      risk.table.height = 0.25
    )

    return(p)
  }, error = function(e) {
    warning(paste("Error plotting Kaplan-Meier curve:", e$message))
    # Return simple plot as fallback
    plot_data <- data.frame(time = time, status = status)
    fit <- survfit(Surv(time, status) ~ 1, data = plot_data)
    plot(fit, xlab = "Time", ylab = "Survival probability", main = title)
    return(NULL)
  })
}

# Plot cumulative hazard
plot_cumulative_hazard <- function(time, status, risk_groups = NULL, title = "Cumulative Hazard"){
  tryCatch({
    if(is.null(risk_groups)){
      plot_data <- data.frame(time = time, status = status)
      fit <- survfit(Surv(time, status) ~ 1, data = plot_data)
    } else {
      plot_data <- data.frame(time = time, status = status, group = risk_groups)
      fit <- survfit(Surv(time, status) ~ group, data = plot_data)
    }

    p <- ggsurvplot(
      fit,
      data = plot_data,
      fun = "cumhaz",
      ggtheme = theme_minimal(),
      title = title,
      xlab = "Time",
      ylab = "Cumulative Hazard",
      legend.title = "Group"
    )

    return(p)
  }, error = function(e) {
    warning(paste("Error plotting cumulative hazard:", e$message))
    return(NULL)
  })
}

##########################
# Temporal Classification Functions (Time-Dependent Metrics)
##########################

# Get survival predictions at multiple time points
# Returns a matrix: [patients x time_points] with S(t) for each patient
get_survival_predictions <- function(model, newdata, times, model_type = "cox"){
  tryCatch({
    if(model_type == "cox"){
      # Cox model predictions
      surv_obj <- survfit(model, newdata = newdata)

      # Extract survival probabilities at specified times
      surv_matrix <- matrix(NA, nrow = nrow(newdata), ncol = length(times))

      for(i in 1:nrow(newdata)){
        single_surv <- survfit(model, newdata = newdata[i, , drop = FALSE])
        surv_probs <- summary(single_surv, times = times, extend = TRUE)$surv
        surv_matrix[i, ] <- surv_probs
      }

    } else if(model_type == "coxnet"){
      # Penalized Cox model
      x_matrix <- as.matrix(newdata[, -c(1:2)])  # Exclude time and status

      # Get linear predictor
      lp <- predict(model, newx = x_matrix, s = "lambda.min", type = "link")[,1]

      # Use baseline hazard to compute survival probabilities
      # For simplicity, we'll use the Cox approach with the linear predictor
      # This requires refitting a cox model with offset
      surv_matrix <- matrix(NA, nrow = nrow(newdata), ncol = length(times))

      # Simplified: use exp(-exp(lp)) as rough approximation
      for(j in 1:length(times)){
        # Baseline survival estimate (simplified)
        surv_matrix[, j] <- exp(-exp(lp) * times[j] / max(times))
      }

    } else if(model_type == "rsf" || model_type == "ranger"){
      # Random Survival Forest
      pred <- predict(model, data = newdata)

      # Extract survival matrix
      if(!is.null(pred$survival)){
        # Interpolate to requested times
        model_times <- pred$unique.death.times
        surv_matrix <- matrix(NA, nrow = nrow(newdata), ncol = length(times))

        for(i in 1:nrow(newdata)){
          patient_surv <- pred$survival[i, ]
          # Interpolate
          surv_matrix[i, ] <- approx(x = model_times, y = patient_surv,
                                     xout = times, rule = 2, method = "constant",
                                     f = 0)$y
        }
      }
    }

    colnames(surv_matrix) <- paste0("t_", times)
    return(surv_matrix)

  }, error = function(e) {
    warning(paste("Error getting survival predictions:", e$message))
    return(NULL)
  })
}

# Calculate classification metrics at a specific time point
# Uses 1-S(t) as risk score: probability of having event by time t
calculate_classification_metrics_at_time <- function(surv_probs, actual_time, actual_status,
                                                      eval_time, threshold = NULL){
  tryCatch({
    # FILTER: Remove patients censored before eval_time (lost to follow-up)
    # We only keep patients for whom we KNOW the status at eval_time:
    # - Patients with event at or before eval_time (time <= eval_time & status == 1)
    # - Patients still at risk after eval_time (time > eval_time)
    # EXCLUDE: Patients censored before eval_time (time < eval_time & status == 0)

    valid_patients <- (actual_time > eval_time) | (actual_time <= eval_time & actual_status == 1)

    # Filter data
    surv_probs_filtered <- surv_probs[valid_patients]
    actual_time_filtered <- actual_time[valid_patients]
    actual_status_filtered <- actual_status[valid_patients]

    # Convert S(t) to risk score: 1 - S(t) = probability of event by time t
    risk_scores <- 1 - surv_probs_filtered

    # Create actual binary outcome at eval_time
    # 1 = had event by eval_time (event occurred before or at eval_time)
    # 0 = event-free at eval_time (survived past eval_time)
    actual_class <- ifelse(actual_time_filtered <= eval_time & actual_status_filtered == 1, 1, 0)

    # If no threshold provided, find optimal threshold using Youden index
    if(is.null(threshold)){
      roc_obj <- roc(actual_class, risk_scores, direction = ">", quiet = TRUE)

      # Find Youden index
      coords_result <- coords(roc_obj, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))
      threshold <- coords_result$threshold
      auc_value <- as.numeric(auc(roc_obj))
      sensitivity <- coords_result$sensitivity
      specificity <- coords_result$specificity
    } else {
      # Use provided threshold
      roc_obj <- roc(actual_class, risk_scores, direction = ">", quiet = TRUE)
      auc_value <- as.numeric(auc(roc_obj))
      coords_result <- coords(roc_obj, threshold, ret = c("sensitivity", "specificity"))
      sensitivity <- coords_result$sensitivity
      specificity <- coords_result$specificity
    }

    # Make predictions based on threshold
    # If risk_score >= threshold, predict event (class 1)
    predicted_class <- ifelse(risk_scores >= threshold, 1, 0)

    # Confusion matrix
    confusion_matrix <- table(Predicted = factor(predicted_class, levels = c(0, 1)),
                               Actual = factor(actual_class, levels = c(0, 1)))

    return(list(
      time = eval_time,
      auc = auc_value,
      sensitivity = sensitivity,
      specificity = specificity,
      threshold = threshold,
      confusion_matrix = confusion_matrix,
      predicted_class = predicted_class,
      actual_class = actual_class,
      risk_scores = risk_scores,  # Add risk scores for visualization
      n_patients = sum(valid_patients),
      n_excluded = sum(!valid_patients)
    ))

  }, error = function(e) {
    warning(paste("Error calculating classification metrics at time", eval_time, ":", e$message))
    return(NULL)
  })
}

# Calculate temporal classification metrics across multiple time points
# Uses timeROC package for proper AUC calculation with censoring
# If training_thresholds provided, uses them instead of recalculating (for validation)
# max_time_points: limit number of time points for performance (default 10, max 20)
calculate_temporal_metrics <- function(model, data, time_col = "time", status_col = "status",
                                       time_points = NULL, model_type = "cox",
                                       training_thresholds = NULL, compute_ci = TRUE,
                                       max_time_points = 10){
  tryCatch({
    if(is.null(time_points)){
      # Use quantiles of observed event times
      # Limit number of time points for performance
      max_time_points <- min(max_time_points, 20)  # Cap at 20 for performance
      max_time_points <- max(max_time_points, 5)   # Minimum 5 for meaningful analysis

      # Auto-adjust for large datasets
      n_samples <- nrow(data)
      if(n_samples > 1000 && max_time_points > 8){
        max_time_points <- 8
        message("Large dataset detected (n=", n_samples, "). Reducing to ", max_time_points, " time points for performance.")
      } else if(n_samples > 500 && max_time_points > 10){
        max_time_points <- 10
        message("Medium dataset detected (n=", n_samples, "). Using ", max_time_points, " time points.")
      }

      event_times <- data[data[[status_col]] == 1, time_col]
      time_points <- seq(from = quantile(event_times, 0.1, na.rm = TRUE),
                        to = quantile(event_times, 0.9, na.rm = TRUE),
                        length.out = max_time_points)
    }

    # Get risk scores for timeROC
    # For survival models, we need a continuous risk score
    risk_scores <- get_risk_scores(model, data, model_type)

    if(is.null(risk_scores) || length(risk_scores) == 0){
      warning("Could not extract risk scores from model")
      return(NULL)
    }

    # Calculate time-dependent AUC using timeROC
    # This properly handles censoring
    timeROC_obj <- timeROC(
      T = data[[time_col]],           # observed time
      delta = data[[status_col]],     # event indicator (0=censored, 1=event)
      marker = risk_scores,           # prognostic marker (higher = higher risk)
      cause = 1,                      # cause of interest
      weighting = "marginal",         # inverse probability weighting method
      times = time_points,            # time points for evaluation
      iid = compute_ci                # compute influence functions for CI (slower but more informative)
    )

    # Extract AUC values at each time point
    auc_values <- timeROC_obj$AUC

    # Extract confidence intervals if computed
    auc_ci_lower <- NULL
    auc_ci_upper <- NULL
    if(compute_ci && !is.null(timeROC_obj$inference)){
      # 95% CI using normal approximation
      auc_se <- timeROC_obj$inference$vect_sd_1  # Standard errors
      auc_ci_lower <- auc_values - 1.96 * auc_se
      auc_ci_upper <- auc_values + 1.96 * auc_se
      # Bound between 0 and 1
      auc_ci_lower <- pmax(0, pmin(1, auc_ci_lower))
      auc_ci_upper <- pmax(0, pmin(1, auc_ci_upper))
    }

    # Get survival predictions for sensitivity/specificity calculation
    surv_matrix <- get_survival_predictions(model, data, time_points, model_type)

    if(is.null(surv_matrix)){
      warning("Could not get survival predictions")
      return(NULL)
    }

    # Calculate sensitivity/specificity with Youden threshold for each time point
    results_list <- list()
    is_validation <- !is.null(training_thresholds)

    for(j in 1:length(time_points)){
      t <- time_points[j]
      surv_probs <- surv_matrix[, j]

      # Use training threshold if provided (VALIDATION SET)
      # Otherwise calculate optimal threshold (TRAINING SET)
      threshold_to_use <- if(is_validation) training_thresholds[j] else NULL

      # Get detailed metrics (sensitivity, specificity, threshold)
      metrics <- calculate_classification_metrics_at_time(
        surv_probs = surv_probs,
        actual_time = data[[time_col]],
        actual_status = data[[status_col]],
        eval_time = t,
        threshold = threshold_to_use  # NULL for train (calculate), value for validation (apply)
      )

      # Replace manual AUC with timeROC AUC (more robust)
      if(!is.null(metrics)){
        metrics$auc <- auc_values[j]
        if(compute_ci){
          metrics$auc_ci_lower <- if(!is.null(auc_ci_lower)) auc_ci_lower[j] else NA
          metrics$auc_ci_upper <- if(!is.null(auc_ci_upper)) auc_ci_upper[j] else NA
        }
      }

      results_list[[j]] <- metrics
    }

    # Compile into data frame
    metrics_df <- data.frame(
      time = time_points,
      AUC = auc_values,
      Sensitivity = sapply(results_list, function(x) if(!is.null(x)) x$sensitivity else NA),
      Specificity = sapply(results_list, function(x) if(!is.null(x)) x$specificity else NA),
      Threshold = sapply(results_list, function(x) if(!is.null(x)) x$threshold else NA),
      N_patients = sapply(results_list, function(x) if(!is.null(x)) x$n_patients else NA),
      N_excluded = sapply(results_list, function(x) if(!is.null(x)) x$n_excluded else NA)
    )

    # Add confidence intervals if computed
    if(compute_ci && !is.null(auc_ci_lower)){
      metrics_df$AUC_CI_lower <- auc_ci_lower
      metrics_df$AUC_CI_upper <- auc_ci_upper
    }

    return(list(
      metrics_df = metrics_df,
      detailed_results = results_list,
      timeROC_obj = timeROC_obj,  # Store timeROC object for advanced usage
      time_points = time_points,   # Store time points for validation reuse
      thresholds = metrics_df$Threshold  # Store thresholds for validation reuse
    ))

  }, error = function(e) {
    warning(paste("Error calculating temporal metrics:", e$message))
    return(NULL)
  })
}

# Plot temporal evolution of classification metrics
plot_temporal_classification_metrics <- function(temporal_metrics, title = "Time-Dependent Classification Metrics",
                                                  show_ci = TRUE){
  tryCatch({
    if(is.null(temporal_metrics) || is.null(temporal_metrics$metrics_df)){
      return(NULL)
    }

    metrics_df <- temporal_metrics$metrics_df

    # Check if CI available
    has_ci <- "AUC_CI_lower" %in% colnames(metrics_df) && "AUC_CI_upper" %in% colnames(metrics_df)

    # Reshape for ggplot
    metrics_long <- reshape2::melt(metrics_df, id.vars = "time",
                                   measure.vars = c("AUC", "Sensitivity", "Specificity"),
                                   variable.name = "Metric", value.name = "Value")

    # Create base plot
    p <- ggplot(metrics_long, aes(x = time, y = Value, color = Metric, group = Metric)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = c("AUC" = "#E7B800", "Sensitivity" = "#2E9FDF", "Specificity" = "#FC4E07"))

    # Add confidence interval ribbon for AUC if available
    if(has_ci && show_ci){
      p <- p + geom_ribbon(data = metrics_df,
                          aes(x = time, ymin = AUC_CI_lower, ymax = AUC_CI_upper),
                          fill = "#E7B800", alpha = 0.2, inherit.aes = FALSE)
    }

    # Add theme and labels
    p <- p +
      theme_minimal() +
      labs(title = title,
           x = "Time",
           y = "Metric Value",
           color = "Metric") +
      ylim(0, 1) +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))

    return(p)

  }, error = function(e) {
    warning(paste("Error plotting temporal metrics:", e$message))
    return(NULL)
  })
}

# Plot time-dependent ROC curve at a specific time using timeROC object
plot_timeROC_curve <- function(timeROC_obj, time_point_idx = 1, title = NULL){
  tryCatch({
    if(is.null(timeROC_obj)){
      return(NULL)
    }

    # Get FP and TP rates for the specified time
    FP <- timeROC_obj$FP[, time_point_idx]
    TP <- timeROC_obj$TP[, time_point_idx]
    time_value <- timeROC_obj$times[time_point_idx]
    auc_value <- timeROC_obj$AUC[time_point_idx]

    # Create data frame
    roc_data <- data.frame(FPR = FP, TPR = TP)

    # Create title if not provided
    if(is.null(title)){
      title <- paste0("Time-Dependent ROC Curve at t = ", round(time_value, 2),
                     "\nAUC = ", round(auc_value, 3))
    }

    # Create plot
    p <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
      geom_line(color = "#E7B800", size = 1.2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
      theme_minimal() +
      labs(title = title,
           x = "False Positive Rate (1 - Specificity)",
           y = "True Positive Rate (Sensitivity)") +
      xlim(0, 1) + ylim(0, 1) +
      coord_equal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))

    return(p)

  }, error = function(e) {
    warning(paste("Error plotting timeROC curve:", e$message))
    return(NULL)
  })
}

##########################
# Export Helper Functions for Survival Analysis
##########################

# Export comprehensive results to Excel workbook
export_survival_results <- function(temporal_metrics_learning, temporal_metrics_validation = NULL,
                                    model_info = NULL, filename = "survival_results.xlsx"){
  tryCatch({
    require(writexl)

    # Create list of sheets
    sheets_list <- list()

    # Sheet 1: Learning Set Temporal Metrics
    if(!is.null(temporal_metrics_learning) && !is.null(temporal_metrics_learning$metrics_df)){
      sheets_list$Learning_Temporal_Metrics <- temporal_metrics_learning$metrics_df
    }

    # Sheet 2: Validation Set Temporal Metrics
    if(!is.null(temporal_metrics_validation) && !is.null(temporal_metrics_validation$metrics_df)){
      sheets_list$Validation_Temporal_Metrics <- temporal_metrics_validation$metrics_df
    }

    # Sheet 3: Confusion Matrices (Learning)
    if(!is.null(temporal_metrics_learning) && !is.null(temporal_metrics_learning$detailed_results)){
      # Get median time point confusion matrix
      n_times <- length(temporal_metrics_learning$detailed_results)
      median_idx <- ceiling(n_times / 2)
      result_median <- temporal_metrics_learning$detailed_results[[median_idx]]

      if(!is.null(result_median) && !is.null(result_median$confusion_matrix)){
        cm <- result_median$confusion_matrix
        cm_df <- as.data.frame.matrix(cm)
        cm_df <- cbind(Predicted = rownames(cm_df), cm_df)
        cm_df$Time <- round(result_median$time, 2)
        cm_df$Dataset <- "Learning"
        sheets_list$Confusion_Matrices <- cm_df
      }
    }

    # Sheet 4: Model Information
    if(!is.null(model_info)){
      sheets_list$Model_Info <- model_info
    }

    # Sheet 5: Metadata
    metadata <- data.frame(
      Item = c("Export Date", "Export Time", "R Version", "Application", "Format Version"),
      Value = c(Sys.Date(), format(Sys.time(), "%H:%M:%S"), R.version.string,
                "Survival Analysis Hybrid App", "1.0")
    )
    sheets_list$Metadata <- metadata

    # Write to Excel
    write_xlsx(sheets_list, filename)

    return(TRUE)

  }, error = function(e) {
    warning(paste("Error exporting results:", e$message))
    return(FALSE)
  })
}

# Export results to CSV (single file with all metrics)
export_results_csv <- function(temporal_metrics_learning, temporal_metrics_validation = NULL,
                                filename = "temporal_metrics.csv"){
  tryCatch({
    # Combine learning and validation metrics
    df_combined <- NULL

    if(!is.null(temporal_metrics_learning) && !is.null(temporal_metrics_learning$metrics_df)){
      df_learning <- temporal_metrics_learning$metrics_df
      df_learning$Dataset <- "Learning"
      df_combined <- df_learning
    }

    if(!is.null(temporal_metrics_validation) && !is.null(temporal_metrics_validation$metrics_df)){
      df_validation <- temporal_metrics_validation$metrics_df
      df_validation$Dataset <- "Validation"

      if(!is.null(df_combined)){
        # Ensure same columns
        common_cols <- intersect(colnames(df_combined), colnames(df_validation))
        df_combined <- rbind(df_combined[, common_cols], df_validation[, common_cols])
      } else {
        df_combined <- df_validation
      }
    }

    if(!is.null(df_combined)){
      write.csv(df_combined, filename, row.names = FALSE)
      return(TRUE)
    } else {
      return(FALSE)
    }

  }, error = function(e) {
    warning(paste("Error exporting CSV:", e$message))
    return(FALSE)
  })
}

##########################
# Visualization Functions for Time-Dependent Classification
##########################

# Plot risk score distribution with Youden threshold
# Shows scatter of risk scores at a specific time point with threshold line
plot_risk_scatter_with_threshold <- function(temporal_metrics, data,
                                             time_col = "time", status_col = "status",
                                             time_point_index = NULL,
                                             title = "Risk Score Distribution with Youden Threshold"){
  tryCatch({
    if(is.null(temporal_metrics) || is.null(temporal_metrics$detailed_results)){
      warning("No temporal metrics available")
      return(NULL)
    }

    # Default to median time point
    if(is.null(time_point_index)){
      n_times <- length(temporal_metrics$detailed_results)
      time_point_index <- ceiling(n_times / 2)
    }

    result_at_time <- temporal_metrics$detailed_results[[time_point_index]]

    if(is.null(result_at_time)){
      warning("No result at specified time point")
      return(NULL)
    }

    eval_time <- result_at_time$time
    threshold <- result_at_time$threshold

    # Get risk scores (1 - S(t))
    risk_scores <- result_at_time$risk_scores
    actual_class <- result_at_time$actual_class

    if(is.null(risk_scores) || is.null(actual_class)){
      warning("Missing risk scores or actual class")
      return(NULL)
    }

    # Create data frame for plotting
    plot_df <- data.frame(
      Sample = 1:length(risk_scores),
      Risk_Score = risk_scores,
      Status = ifelse(actual_class == 1, "Event by time t", "Event-free at time t"),
      Predicted = ifelse(risk_scores >= threshold, "High Risk", "Low Risk")
    )

    # Create plot
    p <- ggplot(plot_df, aes(x = Sample, y = Risk_Score)) +
      # Add threshold line
      geom_hline(yintercept = threshold, linetype = "dashed", color = "red", size = 1.2) +
      # Add points colored by actual status
      geom_point(aes(color = Status, shape = Predicted), size = 3, alpha = 0.7) +
      # Add threshold annotation
      annotate("text", x = max(plot_df$Sample) * 0.85, y = threshold + 0.05,
               label = paste0("Youden Threshold = ", round(threshold, 3)),
               color = "red", size = 4, fontface = "bold") +
      scale_color_manual(values = c("Event by time t" = "#E74C3C",
                                     "Event-free at time t" = "#3498DB")) +
      scale_shape_manual(values = c("High Risk" = 17, "Low Risk" = 16)) +
      labs(title = paste0(title, " (t = ", round(eval_time, 2), ")"),
           x = "Sample Index",
           y = "Risk Score (1 - S(t))",
           color = "Actual Status",
           shape = "Predicted Risk") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.position = "bottom",
            legend.box = "vertical")

    return(p)

  }, error = function(e) {
    warning(paste("Error creating risk scatter plot:", e$message))
    return(NULL)
  })
}

# Plot risk score density with threshold
plot_risk_density_with_threshold <- function(temporal_metrics,
                                             time_point_index = NULL,
                                             title = "Risk Score Distribution by Actual Status"){
  tryCatch({
    if(is.null(temporal_metrics) || is.null(temporal_metrics$detailed_results)){
      warning("No temporal metrics available")
      return(NULL)
    }

    # Default to median time point
    if(is.null(time_point_index)){
      n_times <- length(temporal_metrics$detailed_results)
      time_point_index <- ceiling(n_times / 2)
    }

    result_at_time <- temporal_metrics$detailed_results[[time_point_index]]

    if(is.null(result_at_time)){
      warning("No result at specified time point")
      return(NULL)
    }

    eval_time <- result_at_time$time
    threshold <- result_at_time$threshold
    risk_scores <- result_at_time$risk_scores
    actual_class <- result_at_time$actual_class

    if(is.null(risk_scores) || is.null(actual_class)){
      warning("Missing risk scores or actual class")
      return(NULL)
    }

    # Create data frame
    plot_df <- data.frame(
      Risk_Score = risk_scores,
      Status = ifelse(actual_class == 1, "Event by time t", "Event-free at time t")
    )

    # Create density plot
    p <- ggplot(plot_df, aes(x = Risk_Score, fill = Status)) +
      geom_density(alpha = 0.5) +
      geom_vline(xintercept = threshold, linetype = "dashed", color = "red", size = 1.2) +
      annotate("text", x = threshold + 0.05, y = 0,
               label = paste0("Threshold = ", round(threshold, 3)),
               color = "red", size = 4, fontface = "bold", angle = 90, vjust = -0.5) +
      scale_fill_manual(values = c("Event by time t" = "#E74C3C",
                                    "Event-free at time t" = "#3498DB")) +
      labs(title = paste0(title, " (t = ", round(eval_time, 2), ")"),
           x = "Risk Score (1 - S(t))",
           y = "Density",
           fill = "Actual Status") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.position = "bottom")

    return(p)

  }, error = function(e) {
    warning(paste("Error creating risk density plot:", e$message))
    return(NULL)
  })
}

##########################
# Display Helper Functions for Survival Analysis
##########################

# Display risk score quantiles
display_risk_quantiles <- function(risk_scores){
  tryCatch({
    if(is.null(risk_scores) || length(risk_scores) == 0){
      return(NULL)
    }

    quantiles <- quantile(risk_scores, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    mean_risk <- mean(risk_scores, na.rm = TRUE)

    risk_summary <- data.frame(
      Statistic = c("Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Mean"),
      Risk_Score = c(quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5], mean_risk)
    )

    return(risk_summary)

  }, error = function(e) {
    warning(paste("Error displaying risk quantiles:", e$message))
    return(NULL)
  })
}

# Display survival statistics
display_survival_statistics <- function(model, data = NULL, time_col = "time", status_col = "status",
                                        model_type = "cox", risk_scores = NULL){
  tryCatch({
    stats_list <- list()

    # Median survival time
    if(model_type == "cox"){
      if(is.null(data)){
        surv_obj <- survfit(model)
      } else {
        # For overall population
        median_surv <- median(data[[time_col]][data[[status_col]] == 1], na.rm = TRUE)

        # Also calculate model-based median
        surv_obj <- survfit(model, newdata = data)
      }

      tryCatch({
        median_time <- summary(surv_obj)$table["median"]
        stats_list$median_survival <- median_time
      }, error = function(e){
        stats_list$median_survival <- NA
      })
    }

    # Survival probabilities at specific times (1, 3, 5 years or equivalent)
    if(!is.null(data)){
      max_time <- max(data[[time_col]], na.rm = TRUE)
      time_points <- c(max_time * 0.2, max_time * 0.5, max_time * 0.8)

      if(model_type == "cox"){
        surv_obj <- survfit(model)
        surv_summary <- summary(surv_obj, times = time_points, extend = TRUE)

        stats_list$survival_probs <- data.frame(
          Time = time_points,
          Survival_Probability = surv_summary$surv
        )
      }
    }

    # Risk group statistics if risk scores provided
    if(!is.null(risk_scores) && !is.null(data)){
      median_risk <- median(risk_scores, na.rm = TRUE)
      risk_groups <- ifelse(risk_scores >= median_risk, "High", "Low")

      # Median survival by risk group
      high_risk_times <- data[[time_col]][risk_groups == "High" & data[[status_col]] == 1]
      low_risk_times <- data[[time_col]][risk_groups == "Low" & data[[status_col]] == 1]

      stats_list$median_by_group <- data.frame(
        Group = c("High Risk", "Low Risk"),
        Median_Survival = c(median(high_risk_times, na.rm = TRUE),
                           median(low_risk_times, na.rm = TRUE)),
        N_events = c(sum(risk_groups == "High" & data[[status_col]] == 1),
                    sum(risk_groups == "Low" & data[[status_col]] == 1))
      )
    }

    return(stats_list)

  }, error = function(e) {
    warning(paste("Error displaying survival statistics:", e$message))
    return(NULL)
  })
}

##########################
importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
  # datapath: path of the file
  #extention: extention of the file : csv, xls, ou xlsx
  if(extension=="csv"){
    toto <<- read.csv2(datapath,header = F,sep =sep,dec=dec,na.strings = NAstring,stringsAsFactors = F,row.names=NULL,check.names = F )
  }
  if(extension=="xlsx"){
    options(warn=-1)
    filerm<<-file.rename(datapath,paste(datapath, ".xlsx", sep=""))
    options(warn=0)
    toto <<- read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet) %>% as.data.frame()
    #toto <<- read_xlsx(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
    #toto <-read.xlsx2(file = datapath,sheetIndex = sheet)
    #toto <-read_excel(datapath,na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
  }
  #remove empty column
  if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
    toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #remove empty row
  if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
    toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  print(class(toto))
  
  rnames<-as.character(as.matrix(toto[,1]))
  cnames<-as.character(as.matrix(toto[1,]))
  toto<-toto[,-1]
  toto<-toto[-1,]
  row.names(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]

  toto<-as.data.frame(toto)
  rownames(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]
  return(toto)
}

# downloaddataset <- function(x,file,cnames=T,rnames=T){
#   ext<-strsplit(x = file,split = "[.]")[[1]][2]
#   if(ext=="csv"){
#     if(sum(cnames,rnames)==2){
#       write.csv(x,file)
#       }
#     else{
#       write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
#       }
#   }
#   if(ext=="xlsx"){
#     write.xlsx(x,file,col.names = cnames,row.names =rnames )
#   }
#   
# }

# df <- reactive({
#   req(input$learningfile)
#   file <- input$learningfile
#   ext <- tools::file_ext(file$datapath)
#   
#   req(file)
#   validate(need(ext == "xlsx", "Veuillez télécharger un fichier CSV"))
#   
#   df = read_excel(file$datapath)
#   print(head(df))
#   return( df)
# })


downloaddataset <- function(x,file,cnames=T,rnames=T){
  ext = tools::file_ext(file)
  if(ext=="csv"){
    if(sum(cnames,rnames)==2){
      write.csv(x,file)
    }
    else{
      write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
    }
  }
  if(ext=="xlsx"){
    #write.xlsx(x,file,col.names = cnames,row.names =rnames )
    writexl::write_xlsx(x,file, col_names = cnames)
  }
  
}

downloadplot <- function(file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  
  if(ext=="png"){
    png(file)
  }
  if(ext=="jpg"){
    jpeg(file)
  }  
  if(ext=="pdf"){
    pdf(file) 
  }     
}
# renamvar<-function(names){
#   #rename the duplicate name by adding ".1, .2 ....
#   #toto is a vector of the col names of the tab
#   names[is.na(names)]<-"NA"
#   for(i in 1:length(names)){
#     ind <- which(names%in%names[i])
#     if(length(ind)>1){
#       nb<-c(1:length(ind))
#       newnames<-paste(names[ind],".",nb,sep="")
#       
#       names[ind]<-newnames
#     }
#   }
#   return(names)
# }
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
transformdata<-function(toto,transpose,zeroegalNA){
#   if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
#     toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
#   #remove empty rows
#   if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
#     toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
#   #remove empty columns
  
  # transpose du data frame
  if(transpose){
    toto<-t(toto)
    }
  
  if(zeroegalNA){
    toto[which(toto==0,arr.ind = T)]<-NA
    }
  
toto<-as.data.frame(toto[,c(colnames(toto)[1],sort(colnames(toto)[-1]))])
}
confirmdata<-function(toto, invers = FALSE, time_col = NULL, status_col = NULL, id_col = NULL){
  toto<-as.data.frame(toto)

  # Default behavior: if no columns specified, assume first two columns
  if(is.null(time_col) && is.null(status_col)){
    message("No columns specified, using default: columns 1 and 2 as time and status")
    if(ncol(toto) < 2){
      stop("Data must have at least 2 columns: time and status")
    }
    time_col <- colnames(toto)[1]
    status_col <- colnames(toto)[2]
  }

  # Validate that specified columns exist
  if(!is.null(time_col) && !time_col %in% colnames(toto)){
    stop("Time column '", time_col, "' not found in data. Available columns: ", paste(colnames(toto), collapse = ", "))
  }
  if(!is.null(status_col) && !status_col %in% colnames(toto)){
    stop("Status column '", status_col, "' not found in data. Available columns: ", paste(colnames(toto), collapse = ", "))
  }
  if(!is.null(id_col) && id_col != "None (use row names)" && !id_col %in% colnames(toto)){
    stop("ID column '", id_col, "' not found in data. Available columns: ", paste(colnames(toto), collapse = ", "))
  }

  # Reorder columns: put time and status first, then others
  # Handle ID column if specified
  if(!is.null(id_col) && id_col != "None (use row names)"){
    # Use ID column as row names
    rownames(toto) <- toto[[id_col]]
    # Remove ID column from data
    toto <- toto[, colnames(toto) != id_col, drop = FALSE]
    # Update column references if needed
    if(time_col == id_col) time_col <- colnames(toto)[1]
    if(status_col == id_col) status_col <- colnames(toto)[1]
  }

  # Get all column names except time and status
  other_cols <- setdiff(colnames(toto), c(time_col, status_col))

  # Reorder: time, status, then all others
  toto <- toto[, c(time_col, status_col, other_cols), drop = FALSE]

  # Rename time and status columns to standard names
  colnames(toto)[1] <- "time"
  colnames(toto)[2] <- "status"

  # Convert time to numeric and validate
  toto[,1] <- as.numeric(as.character(toto[,1]))

  # Validate time values
  if(any(is.na(toto[,1]))){
    na_count <- sum(is.na(toto[,1]))
    warning("Time column contains ", na_count, " NA values. These rows will cause issues in survival analysis.")
  }

  if(any(toto[,1] <= 0, na.rm = TRUE)){
    neg_count <- sum(toto[,1] <= 0, na.rm = TRUE)
    stop("Time column must contain only positive values. Found ", neg_count, " values <= 0. Survival time must be > 0.")
  }

  # Convert status to numeric and recode based on user preference
  toto[,2] <- as.numeric(as.character(toto[,2]))

  # Validate status values
  if(any(is.na(toto[,2]))){
    na_count <- sum(is.na(toto[,2]))
    warning("Status column contains ", na_count, " NA values. These rows will cause issues in survival analysis.")
  }

  # Get unique status values (sorted)
  unique_status <- sort(unique(toto[,2][!is.na(toto[,2])]))

  # Validate that status has exactly 2 values
  if(length(unique_status) != 2){
    stop("Status column must contain exactly 2 unique values (event and censored). Found: ",
         length(unique_status), " unique values: ", paste(unique_status, collapse = ", "))
  }

  # Recode status to 0 (censored) and 1 (event) based on invers parameter
  if(!all(unique_status %in% c(0, 1))){
    # User can define which value corresponds to event vs censored
    if(invers){
      # invers = TRUE: lowest value = censored (0), highest = event (1)
      message("Status encoding: ", unique_status[1], " -> 0 (censored), ", unique_status[2], " -> 1 (event)")
      toto[,2] <- ifelse(toto[,2] == unique_status[1], 0, 1)
    } else {
      # invers = FALSE: lowest value = event (1), highest = censored (0)
      message("Status encoding: ", unique_status[1], " -> 1 (event), ", unique_status[2], " -> 0 (censored)")
      toto[,2] <- ifelse(toto[,2] == unique_status[1], 1, 0)
    }
  }

  # Convert remaining columns to numeric (features)
  if(ncol(toto) > 2){
    for (i in 3:ncol(toto)){
      toto[,i] <- as.numeric(as.character(toto[,i]))
    }
  }

  # Final validation summary
  n_events <- sum(toto[,2] == 1, na.rm = TRUE)
  n_censored <- sum(toto[,2] == 0, na.rm = TRUE)
  message("Data confirmed: ", nrow(toto), " observations, ",
          n_events, " events, ", n_censored, " censored, ",
          ncol(toto) - 2, " features")

  return(toto)
}


importfunction<-function(importparameters){
  previousparameters<-NULL
  validation<-NULL
  learning<-NULL
  
  if(is.null(importparameters$learningfile)&is.null(importparameters$modelfile)){return()}
  
  if(!is.null(importparameters$modelfile) ){
    load(file = importparameters$modelfile$datapath)
    previous<-state
    learning<-previous$data$LEARNING
    validation<-previous$data$VALIDATION
    #lev<-previous$data$LEVELS
    previousparameters<-previous$parameters
  }

  if(!is.null(importparameters$learningfile)  ){
    #if(importparameters$confirmdatabutton==0){
      datapath<- importparameters$learningfile$datapath
      #datapath <- input$learningfile$datapath
      #print(datapath)
      #print(paste(datapath, ".xlsx", sep=""))
      #out<<-tryCatch(
      learning<-importfile(datapath = datapath,extension = importparameters$extension,NAstring=importparameters$NAstring,
                           sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
      #              ,error=function(e) e )
      #            if(any(class(out)=="error")){tablearn<-data.frame()}
      #            else{tablearn<<-out}
      #            validate(need(ncol(tablearn)>1 & nrow(tablearn)>1,"problem import"))
      
      learning<-transformdata(toto = learning,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
      
    #}
    if(importparameters$confirmdatabutton!=0){
      learning<-confirmdata(toto = learning,
                           invers = importparameters$invers,
                           time_col = importparameters$time_column,
                           status_col = importparameters$status_column,
                           id_col = importparameters$id_column)

      #learning<-learning[-which(apply(X = learning,MARGIN=1,function(x){sum(is.na(x))})==ncol(learning)),]

#       lev<-levels(x = tablearn[,1])
#       print(lev)
#       names(lev)<-c("positif","negatif")
    }
    # else{lev<-NULL}
  }

  
  if(!is.null(importparameters$validationfile)  ){
    
    # if(importparameters$confirmdatabutton==0){
      datapathV<- importparameters$validationfile$datapath
      # out<<-tryCatch(
      validation<-importfile(datapath = datapathV,extension = importparameters$extension,
                 NAstring=importparameters$NAstring,sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
      #             ,error=function(e) e)
      #             if(any(class(out)=="error")){tabval<-NULL}
      #            else{tabval<<-out}
      #            validate(need(ncol(tabval)>1 & nrow(tabval)>1,"problem import"))
        validation<-transformdata(toto = validation,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
      
      
    # }
    if(importparameters$confirmdatabutton!=0){
      validation<-confirmdata(toto = validation,
                             invers = importparameters$invers,
                             time_col = importparameters$time_column,
                             status_col = importparameters$status_column,
                             id_col = importparameters$id_column)

      #validation<-validation[-which(apply(X = validation,MARGIN=1,function(x){sum(is.na(x))})==ncol(validation)),]

    }
    
  }

  res<-list("learning"=learning,"validation"=validation,previousparameters=previousparameters)#,"lev"=lev)
  return(res)
}


# selectvar<-function(resPCA,toto){
#   #select variables which are correlate to the axes correlate to the cotegorial variable of the first column
#   restri<-dimdesc(resPCA,axes = c(1:(min(ncol(toto),10)-1)) )
#   varquali<-vector()
#   score<-0
#   #restri is a dimdesc data
#   for (i in 1:length(restri)){
#     if ( !is.null(restri[[i]]$quali ) ) {
#       score<-score+restri[[i]]$quali[[1]]
#       varquali<-c(varquali,row.names(restri[[i]]$quanti))
#     }
#   }
#   #score<-1- ( ( (1+score)*(nrow(toto)-1) )/(nrow(toto)-ncol(toto)-1) )
#   return(list("varquali"=varquali,"score"=score))
# }

# selectdata<-function(toto){
#   #remove variable  with less than 2 value and replace 0 by NA
#   n<-ncol(toto)
#   toto[which(toto==0 ,arr.ind = T )]<-NA
#   vec<-rep(T,length=n)
#   for(i in 2:n){
#     vec[i]<-( (length(unique(toto[,i]))>2) )
#   }
#   #rm var with less than 3 values (0 or NA , and 2 other (important for the rempNA PCA))
#   toto<-toto[,as.logical(vec)]
#   return(toto)
# }

selectdatafunction<-function(learning,selectdataparameters){
  learningselect<-selectprctvalues(toto = learning,prctvalues = selectdataparameters$prctvalues,selectmethod =selectdataparameters$selectmethod)
  if(selectdataparameters$NAstructure==T){
    if(selectdataparameters$structdata=="selecteddata"){learning<-learningselect}
    restestNAstructure<-testNAstructure(toto = learning,threshold = selectdataparameters$thresholdNAstructure,maxvaluesgroupmin=selectdataparameters$maxvaluesgroupmin,
                                        minvaluesgroupmax=selectdataparameters$minvaluesgroupmax)
    if(!is.null(restestNAstructure)){
      learningselect<-cbind(learningselect[,!colnames(learningselect)%in%restestNAstructure$restestNAstructure$names],restestNAstructure$varNAstructure)}
  }
  else{restestNAstructure<-NULL}
  
  return(list(learningselect=learningselect,structuredfeatures=restestNAstructure$varNAstructure,datastructuredfeatures=restestNAstructure$restestNAstructure))
}

testObject <- function(object){
  #test if the object is in the global environnement
  exists(as.character(substitute(object)))
}

selectprctvalues<-function(toto,prctvalues=100,selectmethod="nogroup"){ 
  n<-ncol(toto)
  if (selectmethod=="nogroup"){
    NAvec<-vector(length =max(n,0) )
    for(i in 1:n){
      NAvec[i]<-  (sum(!is.na(toto[,i]))/nrow(toto)  ) 
    }
    vec<-(NAvec>=(prctvalues/100))
    
  } 
  
  if(selectmethod!="nogroup"){
    nbcat<-length(levels(toto[,1]))
    tabgroup<-matrix(nrow = nbcat, ncol=n )
    for(i in 1:nbcat){
      tab<-toto[which(toto[,1]==levels(toto[,1])[i]),]
      for(j in 1:(n) ){
        tabgroup[i,j]<-(sum(!is.na(tab[,j]))/nrow(tab))  
      }  
    }
    if(selectmethod=="onegroup"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(max (x) >= (prctvalues/100)) }) 
    }
    if(selectmethod=="bothgroups"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(min (x) >= (prctvalues/100)) }) 
    }
  }
  totoselect<-toto[,as.logical(vec)]
}

heatmapNA<-function(toto,maintitle="Distribution of NA",graph=T){
 
    if(ncol(toto)==1){errorplot(text = " No structured variables")}
    else{
      names<- paste(toto[,1],1:length(toto[,1]))
      tab<-as.data.frame(toto[,-1])
      tab[which(!is.na(tab) ,arr.ind = T )]<-"Value"
      tab[which(is.na(tab) ,arr.ind = T )]<-"NA"
      #tab<-cbind(paste(toto[,1],1:length(toto[,1])),tab)
      tab<-apply(tab,2,as.factor)
      rownames(tab)<-names
      if(!graph){ return(cbind(rownames(toto),tab))}
      if(graph){
      tabm <- melt(tab)
      #tabm<-tabm[-c(1:nrow(toto)),]
      colnames(tabm)<-c("individuals","variables","value")
      tabm$variables<-as.character(tabm$variables)
      tabm$individuals<-as.character(tabm$individuals)
      if(ncol(toto)>60){
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value)) + scale_fill_manual(values=c("lightgrey","steelblue"),name="")+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
      else{
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_manual(values=c("lightgrey","steelblue"))+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
    }
  }
}

distributionvalues<-function(toto,prctvaluesselect,nvar,maintitle="Number of variables according to\nthe % of values's selected",graph=T,ggplot=T){
  percentagevalues<-seq(0,1,by = 0.01)
  prctall<-apply(X = toto,MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto)
  prctvalueswhithoutgroup<-sapply(X = percentagevalues,FUN = function(x,prct=prctall){sum(x<=prct)})
  prctlev1<-apply(X = toto[which(toto[,1]==levels(toto[,1])[1]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[1]),])
  prctlev2<-apply(X = toto[which(toto[,1]==levels(toto[,1])[2]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[2]),])
  
  nvareachgroups<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,min))})  
  nvaronegroup<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,max))})  
  
  distribvalues<-data.frame("percentagevalues"=percentagevalues,"all samples"=prctvalueswhithoutgroup,"each groups"= nvareachgroups,"at least one group"=nvaronegroup)
  if(!graph)(return(distribvalues))
  col<-gg_color_hue(ncol(distribvalues)-1)
  if(!ggplot){
    matplot(x=distribvalues$percentagevalues,distribvalues[,-1],type=c("l","l"),lty = c(1,1,1),
            col=c("red","green","blue"), xlab="percentage of values selected",ylab="Number of variables",main=maintitle)
    legend("bottomright",colnames(distribvalues[,-1]),col=c("red","green","blue"),lty=1)
    abline(v = prctvaluesselect,lty=3,col="grey")
    abline(h = nvar,lty=3,col="grey")
  }
  if (ggplot){
    distribvalueslong<- melt(distribvalues,id.vars = "percentagevalues",variable.name = "select_method",value.name = "number_of_variables")  # convert to long format
    p<-ggplot(data=distribvalueslong,
              aes(x=percentagevalues, y=number_of_variables, colour=select_method)) +geom_line()+
      ggtitle(maintitle)
    p+theme(plot.title=element_text( size=15),legend.text=element_text(size=10),legend.title=element_text(color = 0),legend.position=c(0.20,0.15))+
      geom_vline(xintercept=prctvaluesselect,linetype=3)+
      geom_hline(yintercept=nvar,linetype=3)
  }
}

proptestNA<-function(toto){
  group<-toto[,1]
  toto[,1]<-as.character(toto[,1])
  toto[which(!is.na(toto),arr.ind=T)]<-"value"
  toto[which(is.na(toto),arr.ind=T)]<-"NA"
  pval<-vector("numeric",length = ncol(toto))
  lessgroup<-vector("character",length = ncol(toto))
  prctmore<-vector("numeric",length = ncol(toto))
  prctless<-vector("numeric",length = ncol(toto))
  for (i in 1:ncol(toto)){
    conting<-table(group,factor(toto[,i],levels=c("value","NA")))
    options(warn=-1)
    res<-prop.test(conting)
    options(warn=0)
    pval[i]<-res$p.value
    prctmore[i]<-max(res$estimate)
    prctless[i]<-min(res$estimate)
    if(res$estimate[1]==res$estimate[2]){ lessgroup[i]<-"NA"}
    else{lessgroup[i]<-rownames(conting)[which(res$estimate==min(res$estimate))]}
  }
  pval[is.na(pval)]<-1
  return(data.frame("pval"=pval,"lessgroup"=lessgroup,"prctless"=prctless,"prctmore"=prctmore,"names"=colnames(toto)))
}

testNAstructure<-function(toto,threshold=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){
  class<-toto[,1]
  resproptest<-proptestNA(toto=toto)
  vecond<-c(resproptest$pval<=threshold & resproptest$prctless<=(maxvaluesgroupmin/100) & resproptest$prctmore>=(minvaluesgroupmax/100))
  if(sum(vecond)>0){
    resp<-resproptest[vecond,]
    totopropselect<-data.frame(toto[,vecond])
    colnames(totopropselect)<-resp$names
    totopropselect<-as.data.frame(totopropselect[, order(resp[,2])])
    colnames(totopropselect)<-resp$names[order(resp[,2])]
  }
  else{return(NULL)}

  return(list("varNAstructure"=totopropselect,"restestNAstructure"=resp))
}

transformdatafunction<-function(learningselect,structuredfeatures,datastructuresfeatures,transformdataparameters){
  learningtransform<-learningselect
  if(!is.null(structuredfeatures)){
    for(i in 1:ncol(structuredfeatures)){
      learningtransform[which(is.na(structuredfeatures[,i])&learningselect[,1]==as.character(datastructuresfeatures[i,"lessgroup"])),as.character(datastructuresfeatures[i,"names"])]<-0
    }
  }
  if(transformdataparameters$log){ 
    learningtransform[,-1]<-transformationlog(x = learningtransform[,-1]+1,logtype=transformdataparameters$logtype)}
  if(transformdataparameters$arcsin){
    learningtransform[,-1]<-apply(X = learningtransform[,-1],MARGIN = 2,FUN = function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))})
    learningtransform[,-1]<-asin(sqrt(learningtransform[,-1]))
  }
  if(transformdataparameters$standardization){
    learningtransformsd<<-learningtransform
    sdlearningtransform<-apply(X = learningtransform[-1],MARGIN = 2,FUN = sd,na.rm=T)
    #print('sdlearningtransform')
    #print(sdlearningtransform)
    learningtransform[,-1]<-scale(learningtransform[,-1],center = F,scale=sdlearningtransform)
    #learningtransform[,-1]<-scale(learningtransform[,-1], center = F, scale = TRUE)
  }
  learningtransform<-replaceNA(toto=learningtransform,rempNA=transformdataparameters$rempNA,pos=T,NAstructure = F)
  
  return(learningtransform)
}

transformationlog<-function(x,logtype){
  if(logtype=="log10"){x<-log10(x)}
  if(logtype=="log2"){x<-log2(x)}
  if(logtype=="logn"){x<-log(x)}
  return(x)
}

histplot<-function(toto,graph=T){

    data<-data.frame("values"=as.vector(as.matrix(toto[,-1])))
    if(graph==F){ return(datahistogram(data = data,nbclass = 20))}
    if(graph==T){
    ggplot(data=data,aes(x=values) )+ 
      geom_histogram(col="lightgrey",fill="steelblue",bins=20)+ggtitle("Distribution of values")+
      theme(plot.title = element_text(size=15))+
       annotate("text",x=Inf,y=Inf,label=paste(nrow(data),"values"),size=6,vjust=2,hjust=1.5)
  }
}
datahistogram<-function(data,nbclass){
  dh<-hist(data[,1],nclass=nbclass,plot=F)
  minclass<-dh$breaks[-(length(dh$breaks))]
  maxclass<-dh$breaks[2:(length(dh$breaks))]
  count<-dh$counts
  res<-data.frame("count"=count,"minclass"=minclass,"maxclass"=maxclass)
}

replaceNA<-function(toto,rempNA="z",pos=F,NAstructure=F,thresholdstruct=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){ 
  #rempNA: remplace Non ATtributes values by zero("z"), the mean of the colum (moy), 
  # the mean in each group define by the factor of the first column(moygr), itarative pca (pca), or keep th NA
  if(NAstructure){
    totoNAstruct<-replaceproptestNA(toto = toto,threshold = thresholdstruct ,rempNA =rempNA,maxvaluesgroupmin,minvaluesgroupmax)
    toto[,colnames(totoNAstruct)]<-totoNAstruct
  }
  
  if (rempNA == "none" | sum(is.na(toto))==0 ) {return(toto)}
  cnames<-colnames(toto)
  class<-(toto[,1])
  cat<-levels(class)
  toto<-as.data.frame(toto[,-1],optional = T)
  #toto<-apply(toto,MARGIN = 2,function(x)as.numeric(x))
  n<-ncol(toto) 
  #par default je remplace les NA par 0
  if (rempNA == "z") {
    toto[which(is.na(toto),arr.ind = T)]<-0
  }
  if (rempNA== "moy") {
    toto<-na.aggregate(toto)}
  if(rempNA=="moygr"){
    
    for (i in 1:length(cat)){
      tab<-toto[which(class==cat[i]),]
      tab<-na.aggregate(tab)
      toto[which(class==cat[i]),]<-tab
    }
    toto[which(is.na(toto) ,arr.ind = T )]<-0
  }
  if (rempNA == "pca"){
    
    #prise en compte des liaisons entre variable et de la ressemblance entre individus    
    #nb<-estim_ncpPCA(toto[,(nbqualisup+1):n],ncp.min = 0,ncp.max = 10,method.cv = "Kfold")    #take a lot time
    nindiv<-nrow(toto)
    prctnacol<-apply(X = toto,MARGIN = 2,FUN=function(x){ if(sum(!is.na(x))<=0){x<-rep(0,length=nindiv)}
      else{x}})
    toto<-imputePCA(prctnacol,ncp = min(n-1,5),method.cv="Kfold")$completeObs
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
    toto<-as.data.frame(toto)
    
  }
  if(rempNA=="missforest"){
    toto<-missForest(toto,maxiter = 5)$ximp
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
  }
  
  toto<-cbind(class,toto)
  toto[which(is.na(toto),arr.ind = T)]<-0
  
  colnames(toto)<-cnames
  
  return(toto)
}
mdsplot<-function(toto,ggplot=T,maintitle="MDS representation of the individuals",graph=T){
  class<-toto[,1]
  toto<-toto[-1]
  d <- dist(toto) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2] 
  coord<-(data.frame("class"=class,x,y))
  if(!graph){return(coord)}
  if(!ggplot){
    colr<-c("red","blue")
    
    plot(x, y, xlab="", ylab="",pch=20,main=maintitle, type="p",col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    text(x, y, labels = row.names(toto), cex=.7,col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    legend("topleft",legend=levels(class),text.col = colr)
  }
  #MDS ggplot
  if(ggplot){
    p <- ggplot(coord, aes(x, y,label=rownames(toto)))
    p + geom_text(aes(colour = class))+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

heatmapplot<-function(toto,ggplot=T,maintitle="Heatmap of the transform data ",scale=F,graph=T){
  row.names(toto)<-paste(toto[,1],1:length(toto[,1]))
  toto<-as.matrix(toto[,-1])
  if(!graph){return(toto)}
  #colnames(toto)<-seq(1:ncol(toto))
  if(scale)toto<-scale(toto, center = F, scale = TRUE)
  if(!ggplot){
      heatmap.2(toto,Rowv = NA,Colv=F,trace="none",dendrogram = "none",key=T,margins=c(2,4),keysize=1.30,main=maintitle)
    }
  if(ggplot){
    titi<-melt(toto,value.name = "Intensity")
    colnames(titi)<-c("Individuals","Variables","Intensity")
    titi[,2]<-as.character(titi[,2])
    ggplot(titi, aes( Variables, Individuals,fill = Intensity),colour=NA) + geom_raster()+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

#############
testfunction<-function(tabtransform,testparameters){
  #condition tests
  if (testparameters$SFtest){
    datatesthypothesis<-SFtest(tabtransform,shaptest=T,Ftest=T,threshold=0.05)
  }
  else{datatesthypothesis<-data.frame()}

  #diff test
  if(testparameters$test=="notest"){
    tabdiff<-tabtransform
    datatest<-NULL
    testparameters<-NULL
    useddata<-NULL
    multivariateresults<-NULL
  }
  else if(testparameters$test%in%c("lasso","elasticnet","ridge","cox")){
    # Multivariate selection methods
    multivariateresults<-multivariateselection(toto = tabtransform,
                                               method = testparameters$test,
                                               lambda = testparameters$lambda,
                                               alpha = testparameters$alpha,
                                               nlambda = 100)
    datatest<-multivariateresults$results

    if(nrow(datatest)==0){
      print("no variables selected by multivariate method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars<-multivariateresults$selected_vars
      indvar<-(colnames(tabtransform)%in%selected_vars)
      indvar[1]<-T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata<-data.frame("names"=datatest$name,
                           "coefficient"=datatest$coefficient,
                          "logFC"=datatest$logFoldChange,
                          "mean1"=datatest$mean_group1,
                          "mean2"=datatest$mean_group2)
    }
  }else if (testparameters$test=="clustEnet"){
    # Clustering + Elastic Net selection method
    cat("Running Clustering + ElasticNet variable selection...\n")
    
    # Get parameters with defaults
    n_clusters <- if(!is.null(testparameters$n_clusters)) testparameters$n_clusters else 100
    n_bootstrap <- if(!is.null(testparameters$n_bootstrap)) testparameters$n_bootstrap else 500
    alpha_enet <- if(!is.null(testparameters$alpha)) testparameters$alpha else 0.5
    min_selection_freq <- if(!is.null(testparameters$min_selection_freq)) testparameters$min_selection_freq else 0.5
    preprocess <- if(!is.null(testparameters$preprocess)) testparameters$preprocess else TRUE
    min_patients <- if(!is.null(testparameters$min_patients)) testparameters$min_patients else 20
    
    multivariateresults <- clustEnetSelection(toto = tabtransform,
                                              n_clusters = n_clusters,
                                              n_bootstrap = n_bootstrap,
                                              alpha_enet = alpha_enet,
                                              min_selection_freq = min_selection_freq,
                                              preprocess = preprocess,
                                              min_patients = min_patients)
    datatest <- multivariateresults$results
    
    if(nrow(datatest)==0){
      print("no variables selected by clustering + elasticnet method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars <- multivariateresults$selected_vars
      indvar <- (colnames(tabtransform) %in% selected_vars)
      indvar[1] <- T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata <- data.frame("names"=datatest$name,
                             "SelectionFrequency"=datatest$SelectionFrequency,
                             "logFC"=datatest$logFoldChange,
                             "mean1"=datatest$mean_group1,
                             "mean2"=datatest$mean_group2)
    }
  }
  else{
    # Univariate tests (Wtest, Ttest)
    multivariateresults<-NULL
    datatest<-diffexptest(toto = tabtransform,test = testparameters$test )
    #differential expressed
    logFC<-datatest[,5]
    if(testparameters$adjustpval){pval<-datatest[,3]}
    if(!testparameters$adjustpval){pval<-datatest[,2]}
    datatestdiff<-datatest[which( (pval<testparameters$thresholdpv)&abs(logFC)>testparameters$thresholdFC ),]
    if(dim(datatestdiff)[1]==0){
      print("no differentially expressed variables")
      tabdiff<<-data.frame()
    }
    else{
      indvar<-(colnames(tabtransform)%in%datatestdiff$name)
      indvar[1]<-T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
    }
    useddata<-data.frame("names"=datatest[,1],
                         "pval"=pval,
                         "logFC"=datatest[,5],
                         "mean1"=datatest[,9],
                         "mean2"=datatest[,10])
  }
  return(list("tabdiff"=tabdiff,
              "datatest"=datatest,
              "hypothesistest"=datatesthypothesis,
              "useddata"=useddata,
              "testparameters"=testparameters,
              "multivariateresults"=multivariateresults))
}
  

diffexptest<-function(toto,test="Wtest"){
  #fonction test if the variables (in column) of toto (dataframe) are differently
  #expressed according to the first variable (first column)
  #For binary classification: Wilcoxon/Student test
  #For multi-class: Kruskal-Wallis/ANOVA test
  #For survival: Log-rank test (univariate) or Cox Wald test (univariate)
  #test= Ttest: Student test (parametric), Wtest: Wilcoxon (nonparametric)
  #      Kruskal: Kruskal-Wallis (multi-class nonparametric), ANOVA: ANOVA (multi-class parametric)
  #      logrank: Log-rank test (survival univariate), coxwald: Cox Wald test (survival univariate)

  # Check if this is survival data (time and status columns)
  is_survival <- all(c("time", "status") %in% colnames(toto))

  if(is_survival && test %in% c("logrank", "coxwald")){
    # SURVIVAL ANALYSIS - New code
    cat(paste("Performing", test, "test for survival data...\n"))

    if(test == "logrank"){
      results <- perform_logrank_test(data = toto, time_col = "time", status_col = "status")

      listgen <- data.frame(
        name = results$variable,
        pval = results$pvalue,
        BHadjustpval = p.adjust(results$pvalue, method = "BH"),
        chisq = results$chisq
      )
      colnames(listgen) <- c("name", "pval_logrank", "BHadjustpval_logrank", "chisq")

    } else if(test == "coxwald"){
      results <- perform_cox_univariate(data = toto, time_col = "time", status_col = "status")

      listgen <- data.frame(
        name = results$variable,
        pval = results$pvalue,
        BHadjustpval = p.adjust(results$pvalue, method = "BH"),
        hazard_ratio = results$hazard_ratio,
        HR_lower_95 = results$HR_lower_95,
        HR_upper_95 = results$HR_upper_95,
        coefficient = results$coefficient
      )
      colnames(listgen) <- c("name", "pval_coxwald", "BHadjustpval_coxwald",
                            "Hazard_Ratio", "HR_lower_95", "HR_upper_95", "Coefficient")
    }

    return(listgen)

  } else {
    # CLASSIFICATION ANALYSIS - Original code
    group<-toto[,1]
    toto<-toto[,-1]
    n_classes <- length(levels(group))

  # Detect if binary or multi-class
  if(n_classes == 2){
    # BINARY CLASSIFICATION - Original code
    pval<-vector()
    adjustpval<-vector()
    mlev1<-vector()
    namelev1<-levels(group)[1]
    mlev2<-vector()
    namelev2<-levels(group)[2]
    FC1o2<-vector()
    FC2o1<-vector()
    auc<-vector()
    resyounden<-matrix(ncol = 4,nrow = ncol(toto))
    for (i in 1:max(1,ncol(toto)) ){
      lev1<-toto[which(group==namelev1),i]
      lev2<-toto[which(group==namelev2),i]
      mlev1[i]<-mean(lev1,na.rm = T)+0.0001
      mlev2[i]<-mean(lev2,na.rm = T)+0.0001

      FC1o2[i]<-mlev1[i]/mlev2[i]
      FC2o1[i]<-mlev2[i]/mlev1[i]
      auc[i]<-auc(roc(group,toto[,i],quiet=TRUE))
      resyounden[i,]<-younden(response = group,predictor = toto[,i])
      if( test=="Ttest"){pval[i]<-t.test(x = lev1,y = lev2)$p.value}
      else if( test=="Wtest"){pval[i]<-wilcox.test(lev1 ,lev2,exact = F)$p.value }
    }
    pval[which(is.na(pval))]<-1
    adjustpval<-p.adjust(pval, method = "BH")
    logFC1o2<-log2(abs(FC1o2))
    logFC2o1<-log2(abs(FC2o1))

    listgen<-data.frame(colnames(toto),pval,adjustpval,auc,FC1o2,logFC1o2,FC2o1,logFC2o1,mlev1,mlev2,resyounden)
    colnames(listgen)<-c("name",paste("pval",test,sep = ""),paste("BHadjustpval",test,sep = ""),"AUC",paste("FoldChange ",namelev1,"/",namelev2,sep = ""),paste("logFoldChange ",namelev1,"/",namelev2,sep = ""),
                         paste("FoldChange ",namelev2,"/",namelev1,sep = ""),paste("logFoldChange ",namelev2,"/",namelev1,sep = ""),paste("mean",namelev1,sep = ""),paste("mean",namelev2,sep = ""),
                         "younden criterion","sensibility younden","specificity younden","threshold younden")
    return(listgen)

  } else {
    # MULTI-CLASS CLASSIFICATION - New implementation
    pval<-vector()
    adjustpval<-vector()

    # Calculate mean for each class
    means_by_class <- matrix(nrow = ncol(toto), ncol = n_classes)
    colnames_means <- paste("mean", levels(group), sep = "_")

    # Calculate overall mean for fold change reference
    mean_overall <- vector()

    # For multi-class, use multiclass.roc from pROC
    auc_multiclass <- vector()

    for (i in 1:max(1,ncol(toto)) ){
      # Statistical test
      if(test == "Kruskal" || test == "Wtest"){
        # Kruskal-Wallis test (non-parametric for multiple groups)
        pval[i] <- tryCatch({
          kruskal.test(toto[,i] ~ group)$p.value
        }, error = function(e) return(1))
      } else if(test == "ANOVA" || test == "Ttest"){
        # ANOVA (parametric for multiple groups)
        pval[i] <- tryCatch({
          summary(aov(toto[,i] ~ group))[[1]][1,"Pr(>F)"]
        }, error = function(e) return(1))
      }

      # Calculate means for each class
      for(j in 1:n_classes){
        class_data <- toto[which(group == levels(group)[j]), i]
        means_by_class[i, j] <- mean(class_data, na.rm = TRUE) + 0.0001
      }

      # Overall mean for reference
      mean_overall[i] <- mean(toto[,i], na.rm = TRUE) + 0.0001

      # Multi-class AUC (one-vs-rest average)
      auc_multiclass[i] <- tryCatch({
        roc_obj <- multiclass.roc(group, toto[,i], quiet=TRUE)
        as.numeric(auc(roc_obj))
      }, error = function(e) return(0.5))
    }

    pval[which(is.na(pval))]<-1
    adjustpval<-p.adjust(pval, method = "BH")

    # Build result dataframe for multi-class
    listgen <- data.frame(
      name = colnames(toto),
      pval = pval,
      adjustpval = adjustpval,
      auc = auc_multiclass,
      mean_overall = mean_overall
    )

    # Add means for each class
    for(j in 1:n_classes){
      listgen[, paste("mean", levels(group)[j], sep = "_")] <- means_by_class[, j]
    }

    # Rename columns
    colnames(listgen)[2] <- paste("pval", test, sep = "")
    colnames(listgen)[3] <- paste("BHadjustpval", test, sep = "")
    colnames(listgen)[4] <- "AUC_multiclass"

    return(listgen)
  }
  }  # End of classification else
}

younden<-function(response,predictor){
  res<-roc(response,predictor,quiet=T)
  youndenscore<-res$sensitivities+res$specificities-1
  best<-which(youndenscore==max(youndenscore))[1] # Only the first best is kept
  youndenbest<-youndenscore[best]
  sensiyounden<-res$specificities[best]
  speciyounden<-res$sensitivities[best]
  thresholdyounden<-res$thresholds[best]
  return(c(youndenbest,sensiyounden,speciyounden,thresholdyounden))
}

##########################
# Multivariate variable selection functions
##########################

multivariateselection<-function(toto, method="lasso", lambda=NULL, alpha=0.5, nlambda=100){
  # Function for multivariate variable selection using regularization methods
  # toto: dataframe with first column as group (factor) and other columns as features
  # method: "lasso" (alpha=1), "elasticnet" (0<alpha<1), "ridge" (alpha=0)
  # lambda: regularization parameter (NULL for automatic selection via CV)
  # alpha: elastic net mixing parameter (0=ridge, 1=lasso)
  # nlambda: number of lambda values to test

  lev <- levels(toto[,1])
  n_classes <- length(lev)
  x <- as.matrix(toto[,-1])

  # Set alpha based on method
  if(method == "lasso"){
    alpha <- 1
  } else if(method == "ridge" | method == "cox"){
    alpha <- 0
  } else if(method == "elasticnet"){
    # alpha is provided by user, default 0.5
  }

  # Detect if binary or multi-class
  if(n_classes == 2){
    # BINARY CLASSIFICATION - Original code
    # Encode group so that 1 = first level (positif), 0 = second level (negatif)
    group <- ifelse(toto[,1] == lev[1], 1, 0)

    # Perform cross-validation to find optimal lambda if not provided
    if(is.null(lambda)){
      set.seed(20011203)
      cvfit <- cv.glmnet(x, group, family="binomial", alpha=alpha, nlambda=nlambda,
                         type.measure="auc", nfolds=min(5, nrow(toto)-1)
                         )
      lambda <- cvfit$lambda.min  # lambda that gives minimum CV error
      lambda_1se <- cvfit$lambda.1se  # lambda within 1 SE of minimum
    } else {
      cvfit <- NULL
      lambda_1se <- lambda
    }

    # Fit model with optimal lambda
    fit <- glmnet(x, group, family="binomial", alpha=alpha, lambda=lambda)

    # Extract coefficients
    coef_matrix <- as.matrix(coef(fit))
    coef_values <- coef_matrix[-1, 1]  # Remove intercept
    names(coef_values) <- colnames(x)

    # Select non-zero coefficients
    selected_vars <- names(coef_values[coef_values != 0])

    # Calculate additional statistics for selected variables
    if(length(selected_vars) > 0){
      # AUC for each selected variable
      auc_values <- sapply(selected_vars, function(var){
        auc(roc(group, x[, var], quiet=TRUE))
      })

      # Mean values by group
      mlev1 <- colMeans(x[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
      mlev2 <- colMeans(x[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)

      # Fold change : class 1 sur class 2:  case versus control
      FC1o2 <- mlev1 / (mlev2 + 0.0001)
      logFC1o2 <- log2(abs(FC1o2))

      # Create results dataframe
      results <- data.frame(
        name = selected_vars,
        coefficient = coef_values[selected_vars],
        AUC = auc_values,
        FoldChange = FC1o2,
        logFoldChange = logFC1o2,
        mean_group1 = mlev1,
        mean_group2 = mlev2,
        stringsAsFactors = FALSE
      )

      # Sort by absolute coefficient value
      results <- results[order(abs(results$coefficient), decreasing=TRUE), ]
    } else {
      results <- data.frame()
    }

    # Return results with model information
    return(list(
      results = results,
      selected_vars = selected_vars,
      all_coefficients = coef_values,
      lambda = lambda,
      lambda_1se = lambda_1se,
      alpha = alpha,
      cvfit = cvfit,
      fit = fit,
      method = method
    ))

  } else {
    # MULTI-CLASS CLASSIFICATION - New implementation
    # Encode group as numeric 0, 1, 2, ...
    group_numeric <- as.numeric(toto[,1]) - 1

    # Perform cross-validation to find optimal lambda if not provided
    if(is.null(lambda)){
      set.seed(20011203)
      cvfit <- cv.glmnet(x, group_numeric, family="multinomial",
                         alpha=alpha, nlambda=nlambda,
                         type.measure="class", nfolds=min(5, nrow(toto)-1),
                         type.multinomial = "grouped"
                         )
      lambda <- cvfit$lambda.min  # lambda that gives minimum CV error
      lambda_1se <- cvfit$lambda.1se  # lambda within 1 SE of minimum
    } else {
      cvfit <- NULL
      lambda_1se <- lambda
    }

    # Fit model with optimal lambda
    fit <- glmnet(x, group_numeric, family="multinomial", alpha=alpha, lambda=lambda,
                  type.multinomial = "grouped")

    # Extract coefficients (list of matrices, one per class)
    coef_list <- coef(fit, s=lambda)

    # Aggregate coefficients across classes (use max absolute value)
    coef_aggregated <- rep(0, ncol(x))
    names(coef_aggregated) <- colnames(x)

    for(class_idx in 1:n_classes){
      coef_matrix <- as.matrix(coef_list[[class_idx]])
      coef_values_class <- coef_matrix[-1, 1]  # Remove intercept
      # Keep maximum absolute coefficient across classes
      coef_aggregated <- pmax(abs(coef_aggregated), abs(coef_values_class))
    }

    # Select non-zero coefficients
    selected_vars <- names(coef_aggregated[coef_aggregated > 1e-10])

    # Calculate additional statistics for selected variables
    if(length(selected_vars) > 0){
      # Multi-class AUC for each selected variable
      auc_values <- sapply(selected_vars, function(var){
        tryCatch({
          roc_obj <- multiclass.roc(toto[,1], x[, var], quiet=TRUE)
          as.numeric(auc(roc_obj))
        }, error = function(e) return(0.5))
      })

      # Mean values by group for each class
      means_matrix <- matrix(nrow=length(selected_vars), ncol=n_classes)
      for(j in 1:n_classes){
        means_matrix[, j] <- colMeans(x[which(toto[,1] == lev[j]), selected_vars, drop=FALSE], na.rm=TRUE)
      }
      colnames(means_matrix) <- paste("mean", lev, sep="_")

      # Create results dataframe
      results <- data.frame(
        name = selected_vars,
        coefficient_max = coef_aggregated[selected_vars],
        AUC_multiclass = auc_values,
        stringsAsFactors = FALSE
      )

      # Add means for each class
      results <- cbind(results, means_matrix)

      # Sort by absolute coefficient value
      results <- results[order(abs(results$coefficient_max), decreasing=TRUE), ]
    } else {
      results <- data.frame()
    }

    # Return results with model information
    return(list(
      results = results,
      selected_vars = selected_vars,
      all_coefficients = coef_aggregated,
      coef_list = coef_list,  # Full list of coefficients per class
      lambda = lambda,
      lambda_1se = lambda_1se,
      alpha = alpha,
      cvfit = cvfit,
      fit = fit,
      method = method,
      n_classes = n_classes
    ))
  }
}

##########################
# Clustering + Elastic Net selection function
##########################

# Preprocess peptides: filter low variance and low frequency variables
preprocess_peptides <- function(peptide_data, min_patients = 20) {
  # Filter variables with too few non-zero patients
  n_nonzero <- colSums(peptide_data != 0, na.rm = TRUE)
  keep_peptides <- n_nonzero >= min_patients
  
  # Filter variables with near-zero variance
  variances <- apply(peptide_data, 2, var, na.rm = TRUE)
  keep_var <- variances > 1e-10
  
  return(peptide_data[, keep_peptides & keep_var, drop=FALSE])
}

# Variable selection using clustering and elastic net with bootstrap
varselClust <- function(toto, n_clusters = 100, n_bootstrap = 500, alpha_enet = 0.5,
                        min_selection_freq = 0.5, preprocess = TRUE, min_patients = 20){
  
  withProgress(message = 'Sélection de variables en cours...', value = 0, {
    
    # Extract group and data
    lev <- levels(toto[,1])
    group <- ifelse(toto[,1] == lev[1], 1, 0)
    y <- group
    data <- as.matrix(toto[,-1])
    
    # Optional preprocessing
    if(preprocess && ncol(data) > min_patients){
      incProgress(0.05, detail = "Prétraitement des données...")
      cat("Preprocessing data: filtering low variance and low frequency variables...\n")
      data_preprocessed <- preprocess_peptides(data, min_patients = min_patients)
      if(ncol(data_preprocessed) < ncol(data)){
        cat(sprintf("  Preprocessing: %d → %d variables (removed %d)\n",
                    ncol(data), ncol(data_preprocessed), ncol(data) - ncol(data_preprocessed)))
        data <- data_preprocessed
      }
    }
    
    if(ncol(data) == 0){
      warning("No variables remaining after preprocessing")
      return(list(
        selected_peptides_per_cluster = character(0),
        final_selected_peptides = character(0),
        selection_frequencies = data.frame()
      ))
    }
    
    # Step 1: Clustering based on Spearman correlation
    incProgress(0.1, detail = sprintf("Clustering (%d variables)...", ncol(data)))
    cat(sprintf("Step 1: Clustering %d variables into %d clusters...\n", ncol(data), n_clusters))
    correlation_matrix <- cor(data, use = "pairwise.complete.obs", method = "spearman")
    distance_matrix <- 1 - abs(correlation_matrix)
    distance_matrix[is.na(distance_matrix)] <- 1
    hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
    k <- min(n_clusters, ncol(data))
    clusters <- cutree(hc, k = k)
    
    # Step 2: Select one variable per cluster using Wilcoxon test
    incProgress(0.05, detail = "Sélection par cluster...")
    cat(sprintf("Step 2: Selecting one variable per cluster (Wilcoxon test)...\n"))
    selected_peptides <- c()
    
    for (i in 1:k){
      cluster_peptides <- names(clusters[clusters == i])
      
      if (length(cluster_peptides) > 1){
        p_values <- c()
        for (peptide in cluster_peptides){
          test_result <- tryCatch({
            wilcox.test(data[, peptide] ~ y, exact = FALSE)
          }, error = function(e){
            list(p.value = 1)
          })
          p_values <- c(p_values, test_result$p.value)
        }
        min_p_value_index <- which.min(p_values)
        selected_peptide <- cluster_peptides[min_p_value_index]
      } else {
        selected_peptide <- cluster_peptides[1]
      }
      selected_peptides <- c(selected_peptides, selected_peptide)
    }
    
    data_clust <- data[, selected_peptides, drop=FALSE]
    cat(sprintf("  Selected %d variables (one per cluster)\n", ncol(data_clust)))
    
    # Step 3: Bootstrap + Elastic Net selection (70% de la progression)
    incProgress(0, detail = sprintf("Bootstrap + Elastic Net (0/%d)...", n_bootstrap))
    cat(sprintf("Step 3: Bootstrap + Elastic Net selection (%d iterations)...\n", n_bootstrap))
    set.seed(123)
    selected_peptides_list <- list()
    
    progress_step <- 0.7 / n_bootstrap  # 70% du total pour le bootstrap
    
    for (b in 1:n_bootstrap) {
      if(b %% 50 == 0) {
        incProgress(progress_step * 50, 
                    detail = sprintf("Bootstrap: %d/%d (%.1f%%)", b, n_bootstrap, (b/n_bootstrap)*100))
        cat(sprintf("  Bootstrap iteration: %d/%d\n", b, n_bootstrap))
      }
      
      bootstrap_indices <- sample(1:nrow(data_clust), replace = TRUE)
      X_bootstrap <- data_clust[bootstrap_indices, , drop=FALSE]
      y_bootstrap <- y[bootstrap_indices]
      
      lasso_model <- tryCatch({
        cv.glmnet(as.matrix(X_bootstrap),
                  y_bootstrap,
                  family = "binomial",
                  alpha = alpha_enet)
      }, error = function(e){
        NULL
      })
      
      if(!is.null(lasso_model)){
        coef_lasso <- coef(lasso_model, s = "lambda.min")
        selected_peptides_iter <- rownames(coef_lasso)[which(coef_lasso != 0)][-1]
        selected_peptides_list[[b]] <- selected_peptides_iter
      }
    }
    
    # Step 4: Count selection frequencies
    incProgress(0.05, detail = "Frequency calculation...")
    peptide_selection_counts <- table(unlist(selected_peptides_list))
    data_of_frequencies <- sort(peptide_selection_counts, decreasing = TRUE)
    data_of_frequencies_df <- as.data.frame(data_of_frequencies)
    colnames(data_of_frequencies_df) <- c("Variable", "SelectionCount")
    data_of_frequencies_df$SelectionFrequency <- data_of_frequencies_df$SelectionCount / n_bootstrap
    
    # Step 5: Select final variables
    incProgress(0.05, detail = "Final selection...")
    threshold_count <- ceiling(n_bootstrap * min_selection_freq)
    final_selected_peptides <- names(peptide_selection_counts[peptide_selection_counts >= threshold_count])
    
    cat(sprintf("  Final selection: %d variables selected in >= %.0f%% of bootstraps (threshold: %d/%d)\n",
                length(final_selected_peptides), min_selection_freq * 100, threshold_count, n_bootstrap))
    
    incProgress(0, detail = "Done!")
    
    return(list(
      selected_peptides_per_cluster = selected_peptides,
      final_selected_peptides = final_selected_peptides,
      selection_frequencies = data_of_frequencies_df,
      n_clusters = k,
      n_bootstrap = n_bootstrap,
      alpha = alpha_enet,
      min_selection_freq = min_selection_freq
    ))
    
  }) # Fin withProgress
}

##########################
# Wrapper function for clustering + elasticnet to match other test methods
##########################

clustEnetSelection <- function(toto, n_clusters = 100, n_bootstrap = 500,
                               alpha_enet = 0.5, min_selection_freq = 0.5,
                               preprocess = TRUE, min_patients = 20){
  # Run varselClust
  clust_result <- varselClust(toto,
                              n_clusters = n_clusters,
                              n_bootstrap = n_bootstrap,
                              alpha_enet = alpha_enet,
                              min_selection_freq = min_selection_freq,
                              preprocess = preprocess,
                              min_patients = min_patients)
  
  selected_vars <- clust_result$final_selected_peptides
  
  # If no variables selected, return empty results
  if(length(selected_vars) == 0){
    return(list(
      results = data.frame(),
      selected_vars = character(0),
      all_coefficients = numeric(0),
      clust_result = clust_result,
      method = "clustEnet"
    ))
  }
  
  # Calculate statistics for selected variables (similar to multivariateselection)
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  x <- as.matrix(toto[,-1])
  
  # Get selection frequencies for selected variables
  freq_df <- clust_result$selection_frequencies
  freq_values <- freq_df$SelectionFrequency[match(selected_vars, freq_df$Variable)]
  
  # AUC for each selected variable
  auc_values <- sapply(selected_vars, function(var){
    auc(roc(group, x[, var], quiet=TRUE))
  })
  
  # Mean values by group
  mlev1 <- colMeans(x[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
  mlev2 <- colMeans(x[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)
  
  # Fold change
  FC1o2 <- mlev1 / (mlev2 + 0.0001)
  logFC1o2 <- log2(abs(FC1o2))
  
  # Create results dataframe
  results <- data.frame(
    name = selected_vars,
    SelectionFrequency = freq_values,
    AUC = auc_values,
    FoldChange = FC1o2,
    logFoldChange = logFC1o2,
    mean_group1 = mlev1,
    mean_group2 = mlev2,
    stringsAsFactors = FALSE
  )
  
  # Sort by selection frequency
  results <- results[order(results$SelectionFrequency, decreasing=TRUE), ]
  
  # Return results
  return(list(
    results = results,
    selected_vars = selected_vars,
    all_frequencies = clust_result$selection_frequencies,
    clust_result = clust_result,
    method = "clustEnet"
  ))
}


volcanoplot<-function(logFC,pval,thresholdFC=0,thresholdpv=0.05,graph=T,maintitle="Volcano plot",completedata){
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  
  threshold <- (as.numeric(abs(logFC) > thresholdFC &pval< thresholdpv ) +1)*2
  listgen<-data.frame("logFC"=logFC,"pval"=pval,"threshold"=threshold)
  if(!graph){return(completedata)}
  ##Construct the plot object
  g = ggplot(data=listgen, aes(x=logFC, y=-log10(pval))) +
    geom_point(alpha=0.4, size=1.75, colour=threshold) +
    theme(legend.position = "none") +
    #xlim(c(-(max(listgen$logFC)+0.2), max(listgen$logFC)+0.2)) + ylim(c(0, max(-log10(listgen$pval))+0.2)) +
    xlab("log2 fold change") + ylab("-log10 p-value")+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))+
    annotate("text",x=Inf,y=Inf,label=paste(substring(colnames(completedata)[3],first=4)),size=6,vjust=2,hjust=1.5)
  
  g
} 

barplottest<-function(feature,logFC,levels,pval,mean1,mean2,thresholdpv=0.05,thresholdFC=1,graph=T,maintitle="Mean by group for differentially expressed variables"){
  feature<-rep(feature,each=2)
  group<-rep(c(levels[1],levels[2]),times=(length(feature)/2))
  group<-factor(group,levels =c(levels[1],levels[2]))
  pval2<-rep((pval< thresholdpv),each=2)
  logFC2<-rep((abs(logFC)> thresholdFC),each=2) 
  mean<-vector() 
  mean[seq(from=1,to=length(feature),by = 2)]<-mean1
  mean[seq(from=2,to=length(feature),by = 2)]<-mean2
  data<-data.frame(feature,group,pval,logFC,mean,logFC2,pval2)
  data<-data[order(data$pval),]
  if(!graph){
    data<-data[order(data[,1]),]
    return(data[which((data$pval2==TRUE)& (data$logFC2==TRUE)),c(1,2,5)])}
  else{
    ggplot(data[which( ( data$pval2) & (data$logFC2) ),], aes(feature, mean,fill=group))+geom_bar(stat="identity", position="dodge")+ 
      ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}
errorplot<-function(text=paste("error /n","text error")){
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text,cex = 1.6, col = "black")}

barplottestSF<-function(toto,graph=T){
  #toto: dataframe res from conditiontest function
  if(!graph){return(toto)}
  rescond<-vector()
  for (i in (1:nrow(toto))){
    if(toto$samplenorm[i]=="norm" & toto$varequal[i]!="varequal"){rescond[i]<-"norm"}
    else if(toto$samplenorm[i]=="norm" & toto$varequal[i]=="varequal"){rescond[i]<-"both"}
    else if( toto$samplenorm[i]!="norm" &toto$varequal[i]=="varequal"){rescond[i]<-"varequal"}
    else{rescond[i]<-"none"}
    
  }
  data<-as.factor(rescond)
  p<-qplot(factor(data), geom="bar", fill=factor(data))
  p+ggtitle("Repartition of the variables according to the test results")+
    theme(plot.title=element_text(size=15))
}

SFtest<-function(toto,shaptest=T,Ftest=T,threshold=0.05){
  x<-toto[,1]
  toto<-toto[,-1]
  pvalF<-vector()
  pvalnormlev1<-vector()
  pvalnormlev2<-vector()
  vlev1<-vector()
  vlev2<-vector()
  samplenorm<-vector()
  varequal<-vector()
  conditiontest<-data.frame("name"=colnames(toto))
  for (i in 1:ncol(toto) ){
    lev1<-toto[which(x==levels(x)[1]),i]
    lev2<-toto[which(x==levels(x)[2]),i]
    if(shaptest){
      #pvalnormTem[i]<-shapiro.test(Tem)$p.value
      
      out<- tryCatch(shapiro.test(lev1)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev1[i]<-1
      else{pvalnormlev1[i]<-out}
      
      out<- tryCatch(shapiro.test(lev2)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev2[i]<-1
      else{pvalnormlev2[i]<-out}
      
      if((pvalnormlev2[i]>=threshold) & (pvalnormlev1[i]>=threshold)){samplenorm[i]<-"norm"}
      else{samplenorm[i]<-"notnorm"}
    }
    if(Ftest){
      #to perform a fisher test the value have to be normal
      pvalF[i]<-var.test(lev1,lev2)$p.value
      if(is.na(pvalF[i]))pvalF[i]<-1
      vlev1[i]<-var(lev1)
      vlev2[i]<-var(lev2)
      if(pvalF[i]>=threshold){varequal[i]<-"varequal"}
      else{varequal[i]<-"varnotequal"}
    }
  }
  if(shaptest){ conditiontest<-data.frame(conditiontest,pvalnormlev1,pvalnormlev2,"samplenorm"=samplenorm)
                colnames(conditiontest)<-c("names",paste("pvalshapiro",levels(x)[1],sep=""),paste("pvalshapiro",levels(x)[2],sep = ""),"samplenorm")
  }
  if(Ftest){conditiontest<-data.frame(conditiontest,"pvalF"=pvalF,"variancelev1"=vlev1,"variancelev2"=vlev2,"varequal"=varequal)}
  return(conditiontest) 
}

####

#' GridSearchCV wrapper for Random Forest using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune (ntree, mtry, nodesize, maxnodes)
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_rf_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)
  # library(randomForest)
  if(!requireNamespace("superml", quietly = TRUE)) {
    stop("Package 'superml' is required but not installed")
  }

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      n_estimators = c(100, 500, 1000),  # ntree in randomForest
      max_depth = c(5, 10, 15, 20, NULL),  # maxnodes (NULL = unlimited)
      min_samples_split = c(2, 5, 10),  # nodesize
      max_features = c("sqrt", "log2", floor(ncol(X)/3), floor(ncol(X)/2))  # mtry
    )
  }

  # Create trainer object
  rf_trainer <- superml::RFTrainer$new()

  # Create GridSearchCV object
  gst <-  superml::GridSearchCV$new(
    trainer = rf_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for XGBoost using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_xgb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      n_estimators = c(50, 100, 200),  # nrounds
      max_depth = c(3, 6, 9, 12),
      learning_rate = c(0.01, 0.05, 0.1, 0.3),  # eta
      gamma = c(0, 0.1, 0.5),
      subsample = c(0.6, 0.8, 1.0),
      colsample_bytree = c(0.6, 0.8, 1.0),
      min_child_weight = c(1, 3, 5)
    )
  }

  # Create trainer object
  xgb_trainer <- XGBTrainer$new()

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = xgb_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for Naive Bayes using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_nb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      laplace = c(0, 0.5, 1, 2, 5)  # Smoothing parameter
    )
  }

  # Create trainer object
  nb_trainer <- NBTrainer$new()

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = nb_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for KNN using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
#' 
# tune_knn_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#   # KNN n'est PAS supporté par superml::GridSearchCV
#   # Utiliser uniquement la cross-validation manuelle
#   
#   # Default parameter grid if not provided
#   if(is.null(param_grid)) {
#     max_k <- min(floor(sqrt(length(y))), 30)
#     param_grid <- list(
#       n_neighbors = seq(3, max_k, by = 2)  # k parameter, odd numbers only
#     )
#   }
#   
#   # Utiliser la cross-validation manuelle traditionnelle
#   k_values <- param_grid$n_neighbors
#   best_k <- 3
#   best_acc <- 0
#   
#   set.seed(20011203)
#   for(k_test in k_values){
#     n_folds_cv <- min(5, length(y))
#     fold_size <- floor(length(y) / n_folds_cv)
#     accuracies <- numeric(n_folds_cv)
#     
#     for(fold in 1:n_folds_cv){
#       test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, length(y))
#       train_idx <- setdiff(1:length(y), test_idx)
#       
#       pred <- class::knn(train = X[train_idx, ],
#                          test = X[test_idx, ],
#                          cl = y[train_idx],
#                          k = k_test)
#       accuracies[fold] <- mean(pred == y[test_idx])
#     }
#     
#     avg_acc <- mean(accuracies)
#     if(avg_acc > best_acc){
#       best_acc <- avg_acc
#       best_k <- k_test
#     }
#   }
#   
#   return(list(
#     best_params = list(n_neighbors = best_k),
#     grid_search = NULL,
#     best_score = best_acc
#   ))
# }
tune_knn_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    max_k <- min(floor(sqrt(length(y))), 30)
    param_grid <- list(
      n_neighbors = seq(3, max_k, by = 2),  # k parameter, odd numbers only
      weights = c("uniform", "distance"),
      algorithm = c("brute", "kd_tree")
    )
  }

  # Create trainer object
  knn_trainer <- superml::KNNTrainer$new(type = "class")

  # Create GridSearchCV object
  gst <- superml::GridSearchCV$new(
    trainer = knn_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for Logistic Regression (ElasticNet) using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_elasticnet_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      alpha = c(0, 0.25, 0.5, 0.75, 1.0),  # 0=Ridge, 1=Lasso, 0.5=ElasticNet
      lambda = c(0.001, 0.01, 0.1, 1.0, 10),
      penalty = c("elasticnet")
    )
  }

  # Create trainer object
  lm_trainer <- LMTrainer$new(family = "binomial")

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = lm_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}


# tune_elasticnet_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#   
#   # Default parameter grid if not provided
#   if(is.null(param_grid)) {
#     param_grid <- list(
#       alpha = c(0, 0.25, 0.5, 0.75, 1.0),
#       lambda = NULL  # cv.glmnet trouvera le meilleur lambda
#     )
#   }
#   
#   # Encoder y comme 0/1 si c'est un facteur
#   if(is.factor(y)) {
#     y_numeric <- as.numeric(y) - 1
#   } else {
#     y_numeric <- y
#   }
#   
#   best_alpha <- param_grid$alpha[1]
#   best_lambda <- NULL
#   best_auc <- 0
#   
#   set.seed(20011203)
#   for(alpha_test in param_grid$alpha){
#     cvfit <- glmnet::cv.glmnet(as.matrix(X), y_numeric, 
#                                family="binomial", 
#                                alpha=alpha_test,
#                                type.measure="auc", 
#                                nfolds=min(10, length(y)-1))
#     
#     # Obtenir le meilleur AUC pour cet alpha
#     auc_max <- max(cvfit$cvm)
#     
#     if(auc_max > best_auc){
#       best_auc <- auc_max
#       best_alpha <- alpha_test
#       best_lambda <- cvfit$lambda.min
#     }
#   }
#   
#   return(list(
#     best_params = list(alpha = best_alpha, lambda = best_lambda),
#     grid_search = NULL,
#     best_score = best_auc
#   ))
# }

####

# ============================================================================
# MODEL FUNCTION - SURVIVAL ANALYSIS ONLY (as of 2025-12-24)
# ============================================================================
# This function builds survival models for censored time-to-event data.
#
# SUPPORTED MODELS:
#   - Cox Proportional Hazards (cox)
#   - Random Survival Forest (rsf)
#   - Penalized Cox: Lasso, ElasticNet, Ridge (coxlasso, coxelasticnet, coxridge)
#
# DEPRECATED MODELS (removed from UI, code retained for reference):
#   - Classification models: SVM, XGBoost, LightGBM, KNN, NaiveBayes, RandomForest
#   These models do NOT handle censored survival data correctly.
#   They ignore time-to-event information and censoring status.
#
# IMPORTANT: Application now enforces survival models only via ui.R and server.R.
# Classification model code below will never execute but is kept for reference.
# ============================================================================

modelfunction <- function(learningmodel,
                          validation=NULL,
                          modelparameters,
                          transformdataparameters,
                          datastructuresfeatures=NULL,
                          learningselect){
  if(modelparameters$modeltype!="nomodel"){

    # Check if this is a survival model or classification model
    is_survival_model <- modelparameters$modeltype %in% c("rsf", "cox", "coxlasso", "coxelasticnet", "coxridge")

    if(is_survival_model){
      # For survival models: expect time and status columns
      if(!("time" %in% colnames(learningmodel)) || !("status" %in% colnames(learningmodel))){
        stop("Survival models require 'time' and 'status' columns in data")
      }
    } else {
      # DEPRECATED: Classification model support (code retained for reference)
      # This branch should never execute as ui.R/server.R now restrict to survival models only
      warning("Classification models are deprecated. Use Cox, RSF, or penalized Cox models for survival analysis.")

      # For classification models: expect group column
      colnames(learningmodel)[1]<-"group"

      if(modelparameters$invers){
        learningmodel[,1]<-factor(learningmodel[,1],levels = rev(levels(learningmodel[,1])),ordered = TRUE)
      }
      lev<-levels(x = learningmodel[,1])
      names(lev)<-c("positif","negatif")
    }

    #Build model - SURVIVAL MODELS
    if(modelparameters$modeltype == "rsf"){
      # Random Survival Forest
      cat("Building Random Survival Forest model...\n")

      ntree_param <- ifelse(is.null(modelparameters$ntree), 500, modelparameters$ntree)

      # Determine mtry parameter
      n_features <- ncol(learningmodel) - 2  # Exclude time and status

      if(is.null(modelparameters$autotunerf) || modelparameters$autotunerf){
        cat("Auto-tuning RSF hyperparameters...\n")
        # Use tuning function
        model <- tune_rsf_model(data = learningmodel,
                               time_col = "time",
                               status_col = "status",
                               ntree_values = c(100, 500, ntree_param),
                               mtry_values = c(floor(sqrt(n_features)), floor(n_features/3), floor(n_features/2)),
                               nodesize_values = c(3, 5, 10))

        optimal_mtry <- if(!is.null(model$best_params)) model$best_params$mtry else floor(sqrt(n_features))
      } else {
        # Use manual parameters
        optimal_mtry <- ifelse(is.null(modelparameters$mtry), floor(sqrt(n_features)), modelparameters$mtry)

        model <- fit_rsf_model(data = learningmodel,
                              time_col = "time",
                              status_col = "status",
                              num_trees = ntree_param,
                              mtry = optimal_mtry,
                              min_node_size = 5)
      }

      # Extract risk scores
      riskscores <- get_risk_scores(model, learningmodel, model_type = "rsf")

      # No predictclass or scorelearning for survival models
      # Store results in survival-compatible format
      scorelearning <- data.frame(risk_score = riskscores)
      predictclasslearning <- NULL  # No classification in survival
      classlearning <- NULL

    } else if(modelparameters$modeltype == "cox"){
      # Cox Proportional Hazards
      cat("Building Cox Proportional Hazards model...\n")

      model <- fit_cox_model(data = learningmodel,
                            time_col = "time",
                            status_col = "status")

      # Extract risk scores
      riskscores <- get_risk_scores(model, learningmodel, model_type = "cox")

      scorelearning <- data.frame(risk_score = riskscores)
      predictclasslearning <- NULL
      classlearning <- NULL

    } else if(modelparameters$modeltype %in% c("coxlasso", "coxelasticnet", "coxridge")){
      # Penalized Cox models
      cat(paste("Building", modelparameters$modeltype, "model...\n"))

      # Determine alpha
      alpha <- switch(modelparameters$modeltype,
                     "coxlasso" = 1,
                     "coxelasticnet" = ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha),
                     "coxridge" = 0)

      model <- fit_coxnet_model(data = learningmodel,
                               time_col = "time",
                               status_col = "status",
                               alpha = alpha,
                               nfolds = 10)

      # Extract risk scores
      riskscores <- get_risk_scores(model, learningmodel, model_type = "coxnet")

      # Get selected variables
      coef_matrix <- coef(model, s = "lambda.min")
      selected_vars <- rownames(coef_matrix)[coef_matrix[,1] != 0]
      cat(sprintf("Selected %d variables with %s\n", length(selected_vars), modelparameters$modeltype))

      scorelearning <- data.frame(risk_score = riskscores)
      predictclasslearning <- NULL
      classlearning <- NULL

    # ========================================================================
    # DEPRECATED CLASSIFICATION MODELS - Code retained for reference only
    # ========================================================================
    # NOTE: These models do not support censored survival data and should NOT be used.
    # ui.R and server.R now prevent selection of these models.
    # Code below will never execute but is kept for historical reference.
    # ========================================================================

    } else if (modelparameters$modeltype=="randomforest"){
      # DEPRECATED: RandomForest classification - does not handle censored data
      warning("RandomForest classification is deprecated for survival analysis. Use Random Survival Forest (rsf) instead.")
      learningmodel<-as.data.frame(learningmodel[sort(rownames(learningmodel)),])

      x<-as.data.frame(learningmodel[,-1])
      colnames(x)<-colnames(learningmodel)[-1]
      x<-as.data.frame(x[,sort(colnames(x))])
      set.seed(20011203)
      ntree_param <- ifelse(is.null(modelparameters$ntree), 1000, modelparameters$ntree)

      # Determine mtry parameter
      if(is.null(modelparameters$autotunerf) || modelparameters$autotunerf){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for Random Forest hyperparameter tuning...\n")

          # Prepare parameter grid
          param_grid <- list(
            n_estimators = if(!is.null(modelparameters$rf_grid_ntree)) modelparameters$rf_grid_ntree else c(100, 500, 1000),
            max_features = if(!is.null(modelparameters$rf_grid_mtry)) modelparameters$rf_grid_mtry else c("sqrt", "log2"),
            min_samples_split = if(!is.null(modelparameters$rf_grid_nodesize)) modelparameters$rf_grid_nodesize else c(2, 5, 10)
          )

          # Run GridSearchCV
          grid_result <- tryCatch({
            tune_rf_gridsearch(X = x, y = learningmodel[,1],
                              param_grid = param_grid,
                              n_folds = 5,
                              scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to tuneRF:", e$message, "\n")
            NULL
          })

          if(!is.null(grid_result)) {
            # Extract best parameters from GridSearchCV
            best_params <- grid_result$best_params

            # Convert superml parameters to randomForest parameters
            optimal_mtry <- if(!is.null(best_params$max_features)) {
              if(best_params$max_features == "sqrt") floor(sqrt(ncol(x)))
              else if(best_params$max_features == "log2") floor(log2(ncol(x)))
              else as.numeric(best_params$max_features)
            } else floor(sqrt(ncol(x)))

            ntree_param <- if(!is.null(best_params$n_estimators)) best_params$n_estimators else ntree_param
            nodesize_param <- if(!is.null(best_params$min_samples_split)) best_params$min_samples_split else 1

            cat(sprintf("GridSearchCV best params: ntree=%d, mtry=%d, nodesize=%d, score=%.4f\n",
                       ntree_param, optimal_mtry, nodesize_param, grid_result$best_score))
          } else {
            # Fallback to tuneRF if GridSearchCV fails
            tuneRF_result <- tuneRF(x = x, y = learningmodel[,1],
                                    doBest = FALSE,
                                    ntreeTry = ntree_param,
                                    stepFactor = 1.5,
                                    improve = 0.01,
                                    trace = FALSE,
                                    plot = FALSE)
            optimal_mtry <- tuneRF_result[which.min(tuneRF_result[,2]), 1]
            nodesize_param <- 1
          }
        } else {
          # Use traditional tuneRF to find optimal mtry parameter
          tuneRF_result <- tuneRF(x = x, y = learningmodel[,1],
                                  doBest = FALSE,
                                  ntreeTry = ntree_param,
                                  stepFactor = 1.5,
                                  improve = 0.01,
                                  trace = FALSE,
                                  plot = FALSE)
          # Extract optimal mtry (the one with minimum OOB error)
          optimal_mtry <- tuneRF_result[which.min(tuneRF_result[,2]), 1]
          nodesize_param <- 1
        }
      } else {

        # Use manual mtry parameter

        optimal_mtry <- ifelse(is.null(modelparameters$mtry), floor(sqrt(ncol(x))), modelparameters$mtry)
        nodesize_param <- 1
      }

      # Build final model with optimal or manual parameters
      model <- randomForest(x = x, y = learningmodel[,1],
                           ntree = ntree_param,
                           mtry = optimal_mtry,
                           nodesize = nodesize_param,
                           
                           importance = TRUE)

 

      # Store optimal parameters in model object

      model$optimal_mtry <- optimal_mtry

      model$ntree_used <- ntree_param
      model$nodesize_used <- nodesize_param
      if(modelparameters$fs){
        featureselect<-selectedfeature(model=model,modeltype = "randomforest",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }

      # Handle scores for binary and multi-class
      n_classes <- get_n_classes(learningmodel[,1])

      if(n_classes == 2){
        # Binary classification - extract probability for positive class
        scorelearning =data.frame(model$votes[,lev["positif"]])
        colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
        predictclasslearning<-factor(levels = lev)
        predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclasslearning<-as.factor(predictclasslearning)
      } else {
        # Multi-class classification - use probability matrix
        scorelearning <- model$votes  # Matrix (n_samples x n_classes)
        # Predict using argmax (no threshold for multi-class)
        predictclasslearning <- predict_from_scores(scorelearning, learningmodel[,1], threshold=NULL)
      }
      #predictclasslearning==model$predicted
    }   
    
    if(modelparameters$modeltype=="svm"){
      # DEPRECATED: SVM - does not handle censored survival data
      warning("SVM is deprecated for survival analysis. Use Cox models instead.")
      # Determine hyperparameters
      if(is.null(modelparameters$autotunesvm) || modelparameters$autotunesvm){
        # Perform hyperparameter tuning using tune.svm
        tune_result <- tune.svm(group ~ ., data = learningmodel,
                               gamma = 10^(-5:2), cost = 10^(-3:2),
                               cross=min(dim(learningmodel)[1]-2,10),
                               #kernel=c("linear", "polynomial", "radial", "sigmoid"),
                               # ranges=list(kernel=c("linear", "polynomial",
                               #                      "radial", "sigmoid")),
                               tunecontrol = tune.control(sampling = "cross"))

        # Extract best model and parameters
        # model <- tune_result$best.model
        # model$cost <- tune_result$best.parameters$cost
        # model$gamma <- tune_result$best.parameters$gamma
        cat('tunning results :  \n')
        print(tune_result)
        cost_param <- tune_result$best.parameters$cost
        gamma_param <- tune_result$best.parameters$gamma
        
      } else {
        # Use manual hyperparameters
        cat("define svm parameters manually \n")
        cost_param <- ifelse(is.null(modelparameters$cost), 1, modelparameters$cost)
        gamma_param <- ifelse(is.null(modelparameters$gamma), 0.1, modelparameters$gamma)
        # kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)

        # model <- svm(group ~ ., data = learningmodel,
        #             kernel= kernel_param , 
        #             cost=cost_param, gamma=gamma_param,
        #             probability=FALSE)
        # model$cost <- cost_param
        # model$gamma <- gamma_param
        # model$kernel <- kernel_param
      }
      
      model <- svm(group ~ ., data = learningmodel,
                   kernel= 'radial' , #kernel_param , 
                   cost=cost_param, gamma=gamma_param,
                   type = "C-classification",
                   probability=TRUE)
      model$cost <- cost_param
      model$gamma <- gamma_param
      #model$kernel <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)

      if(modelparameters$fs){

        featureselect<-selectedfeature(model=model,modeltype = "svm",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }

      # Get scores for binary and multi-class
      n_classes <- get_n_classes(learningmodel[,1])

      if(n_classes == 2){
        # Binary classification - use decision values
        scorelearning <-model$decision.values
        if(sum(lev==(strsplit(colnames(scorelearning),split = "/")[[1]]))==0){
          scorelearning<-scorelearning*(-1)
          colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
        }

        predictclasslearning<-factor(levels = lev)
        predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclasslearning<-as.factor(predictclasslearning)
      } else {
        # Multi-class classification - use probabilities
        # SVM with probability=TRUE returns probability matrix for multi-class
        pred_probs <- attr(predict(model, learningmodel[,-1], probability=TRUE), "probabilities")
        scorelearning <- pred_probs  # Matrix (n_samples × n_classes)

        # Reorder columns to match level order
        scorelearning <- scorelearning[, lev]

        # Predict using argmax (no threshold for multi-class)
        predictclasslearning <- predict_from_scores(scorelearning, learningmodel[,1], threshold=NULL)
      }
    }

    if(modelparameters$modeltype=="lightgbm"){
      # DEPRECATED: LightGBM classification - does not handle censored survival data
      warning("LightGBM is deprecated for survival analysis. Use Cox or RSF models instead.")

      # LightGBM gradient boosting
      x <- as.matrix(learningmodel[,-1])
      n_classes <- get_n_classes(learningmodel[,1])

      # Encode y based on binary or multi-class
      if(n_classes == 2){
        # Binary: 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
        y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      } else {
        # Multi-class: encode as 0, 1, 2, ... (n_classes-1)
        y <- as.numeric(learningmodel[,1]) - 1
      }

      # Create LightGBM dataset
      dtrain <- lgb.Dataset(data = x, label = y)
      # Determine hyperparameters
      if(is.null(modelparameters$autotunelgb) || modelparameters$autotunelgb){
        # Perform hyperparameter tuning using cross-validation
        set.seed(20011203)
        # Parameter grid search - adapt to binary or multi-class
        if(n_classes == 2){
          best_params <- list(
            objective = "binary",
            metric = "auc",
            num_leaves = 31,
            learning_rate = 0.05,
            feature_fraction = 0.9,
            bagging_fraction = 0.8,
            bagging_freq = 5,
            verbose = -1
          )
        } else {
          best_params <- list(
            objective = "multiclass",
            num_class = n_classes,
            metric = "multi_logloss",
            num_leaves = 31,
            learning_rate = 0.05,
            feature_fraction = 0.9,
            bagging_fraction = 0.8,
            bagging_freq = 5,
            verbose = -1
          )
        }

        # Cross-validation to find optimal nrounds
        cv_results <- lgb.cv(
          params = best_params,
          data = dtrain,
          nrounds = 200,
          nfold = min(5, nrow(learningmodel)-1),
          early_stopping_rounds = 10,
          verbose = -1
        )

        optimal_nrounds <- cv_results$best_iter
        # Train final model with optimal parameters
        model <- lgb.train(
          params = best_params,
          data = dtrain,
          nrounds = optimal_nrounds,
          verbose = -1
        )
        
        # Store optimal parameters
        model$optimal_nrounds <- optimal_nrounds
        model$optimal_num_leaves <- best_params$num_leaves
        model$optimal_learning_rate <- best_params$learning_rate
      } else {
        # Use manual hyperparameters
        nrounds_param <- ifelse(is.null(modelparameters$nrounds_lgb), 100, modelparameters$nrounds_lgb)
        num_leaves_param <- ifelse(is.null(modelparameters$num_leaves), 31, modelparameters$num_leaves)
        learning_rate_param <- ifelse(is.null(modelparameters$learning_rate_lgb), 0.05, modelparameters$learning_rate_lgb)

        # Adapt params to binary or multi-class
        if(n_classes == 2){
          params <- list(
            objective = "binary",
            metric = "auc",
            num_leaves = num_leaves_param,
            learning_rate = learning_rate_param,
            feature_fraction = 0.9,
            bagging_fraction = 0.8,
            bagging_freq = 5,
            verbose = -1
          )
        } else {
          params <- list(
            objective = "multiclass",
            num_class = n_classes,
            metric = "multi_logloss",
            num_leaves = num_leaves_param,
            learning_rate = learning_rate_param,
            feature_fraction = 0.9,
            bagging_fraction = 0.8,
            bagging_freq = 5,
            verbose = -1
          )
        }

 

        model <- lgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds_param,
          verbose = -1
        )

 

        # Store parameters
        model$optimal_nrounds <- nrounds_param
        model$optimal_num_leaves <- num_leaves_param
        model$optimal_learning_rate <- learning_rate_param
      }

      # Make predictions (probabilities) - adapt to binary or multi-class
      predictions_raw <- predict(model, x)

      if(n_classes == 2){
        # Binary: predictions_raw is a vector of probabilities for positive class
        scorelearning <- data.frame(predictions_raw)
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        predictclasslearning<-factor(levels = lev)
        predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclasslearning<-as.factor(predictclasslearning)
      } else {
        # Multi-class: predictions_raw is a matrix (n_samples × n_classes)
        scorelearning <- matrix(predictions_raw, ncol=n_classes, byrow=TRUE)
        colnames(scorelearning) <- lev
        # Predict using argmax (no threshold for multi-class)
        predictclasslearning <- predict_from_scores(scorelearning, learningmodel[,1], threshold=NULL)
      }
    }

    if(modelparameters$modeltype=="naivebayes"){
      # DEPRECATED: Naive Bayes - does not handle censored survival data
      warning("Naive Bayes is deprecated for survival analysis. Use Cox models instead.")
      # Naive Bayes classifier
      # Check if GridSearchCV should be used
      optimal_laplace <- 0  # Default value

      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
        # Use GridSearchCV from superml for hyperparameter tuning
        cat("Using GridSearchCV for Naive Bayes hyperparameter tuning...\n")

        # Prepare parameter grid
        param_grid <- list(
          laplace = if(!is.null(modelparameters$nb_grid_laplace)) modelparameters$nb_grid_laplace else c(0, 0.5, 1, 2, 5)
        )

        # Run GridSearchCV
        grid_result <- tryCatch({
          X_df <- as.data.frame(learningmodel[,-1])
          tune_nb_gridsearch(X = X_df, y = learningmodel[,1],
                            param_grid = param_grid,
                            n_folds = 5,
                            scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, using default laplace=0:", e$message, "\n")
          NULL
        })

        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          optimal_laplace <- if(!is.null(best_params$laplace)) best_params$laplace else 0
          cat(sprintf("GridSearchCV best params: laplace=%.2f, score=%.4f\n",
                     optimal_laplace, grid_result$best_score))
        }
      }

      # Build model with optimal or default laplace parameter
      model <- naiveBayes(x = learningmodel[,-1], y = learningmodel[,1], laplace = optimal_laplace)

      # Store model type and optimal parameter
      model$model_type <- "naivebayes"
      model$optimal_laplace <- optimal_laplace

      # Make predictions (probabilities) - adapt to binary or multi-class
      pred_probs <- e1071:::predict.naiveBayes(model, learningmodel[,-1], type="raw")
      n_classes <- get_n_classes(learningmodel[,1])

      if(n_classes == 2){
        # Binary: extract probability for positive class
        scorelearning <- data.frame(pred_probs[, lev["positif"]])
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        predictclasslearning<-factor(levels = lev)
        predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclasslearning<-as.factor(predictclasslearning)
      } else {
        # Multi-class: pred_probs is already a matrix (n_samples × n_classes)
        scorelearning <- pred_probs[, lev]  # Reorder columns to match level order
        # Predict using argmax (no threshold for multi-class)
        predictclasslearning <- predict_from_scores(scorelearning, learningmodel[,1], threshold=NULL)
      }
    }

    if(modelparameters$modeltype=="knn"){
      # DEPRECATED: KNN - does not handle censored survival data
      warning("KNN is deprecated for survival analysis. Use Cox or RSF models instead.")
      # K-Nearest Neighbors
      # Determine k parameter
      if(is.null(modelparameters$autotuneknn) || modelparameters$autotuneknn){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for KNN hyperparameter tuning...\n")

          # Prepare parameter grid
          max_k <- min(floor(sqrt(nrow(learningmodel))), 30)
          param_grid <- list(
            n_neighbors = if(!is.null(modelparameters$knn_grid_k)) modelparameters$knn_grid_k else seq(3, max_k, by=2)
          )

          # Run GridSearchCV
          grid_result <- tryCatch({
            X_df <- as.data.frame(learningmodel[,-1])
            tune_knn_gridsearch(X = X_df, y = learningmodel[,1],
                               param_grid = param_grid,
                               n_folds = 5,
                               scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to manual CV:", e$message, "\n")
            NULL
          })

          if(!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_k <- if(!is.null(best_params$n_neighbors)) best_params$n_neighbors else 5
            cat(sprintf("GridSearchCV best params: k=%d, score=%.4f\n",
                       optimal_k, grid_result$best_score))
          } else {
            # Fallback to traditional CV if GridSearchCV fails
            # Automatic tuning: try different k values via cross-validation
            set.seed(20011203)
            # Test k values from 3 to min(sqrt(n), 20)
            max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
            k_values <- seq(3, max_k, by=2) # odd numbers only

            # Cross-validation to find best k
            best_k <- 3
            best_acc <- 0
            for(k_test in k_values){
              # Simple leave-one-out or 5-fold CV
              n_folds <- min(5, nrow(learningmodel))
              fold_size <- floor(nrow(learningmodel) / n_folds)
              accuracies <- numeric(n_folds)
              for(fold in 1:n_folds){
                test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
                train_idx <- setdiff(1:nrow(learningmodel), test_idx)
                pred <- knn(train = learningmodel[train_idx, -1],
                           test = learningmodel[test_idx, -1],
                           cl = learningmodel[train_idx, 1],
                           k = k_test)
                accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
              }

              avg_acc <- mean(accuracies)
              if(avg_acc > best_acc){
                best_acc <- avg_acc
                best_k <- k_test
              }
            }

            optimal_k <- best_k
          }
        } else {
          # Use traditional CV for hyperparameter tuning
          # Automatic tuning: try different k values via cross-validation
          set.seed(20011203)
          # Test k values from 3 to min(sqrt(n), 20)
          max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
          k_values <- seq(3, max_k, by=2) # odd numbers only

          # Cross-validation to find best k
          best_k <- 3
          best_acc <- 0
          for(k_test in k_values){
            # Simple leave-one-out or 5-fold CV
            n_folds <- min(5, nrow(learningmodel))
            fold_size <- floor(nrow(learningmodel) / n_folds)
            accuracies <- numeric(n_folds)
            for(fold in 1:n_folds){
              test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
              train_idx <- setdiff(1:nrow(learningmodel), test_idx)
              pred <- knn(train = learningmodel[train_idx, -1],
                         test = learningmodel[test_idx, -1],
                         cl = learningmodel[train_idx, 1],
                         k = k_test)
              accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
            }

            avg_acc <- mean(accuracies)
            if(avg_acc > best_acc){
              best_acc <- avg_acc
              best_k <- k_test
            }
          }

          optimal_k <- best_k
        }

      } else {
        # Use manual k parameter
        optimal_k <- ifelse(is.null(modelparameters$k_neighbors), 5, modelparameters$k_neighbors)
      }

 

      # KNN doesn't have a traditional "model" object, store parameters
      model <- list(
        train_data = learningmodel[,-1],
        train_labels = learningmodel[,1],
        optimal_k = optimal_k,
        model_type = "knn"

      )

      # Make predictions using knn with probability estimation
      # For probability, we'll use the proportion of k neighbors in each class
      n_classes <- get_n_classes(learningmodel[,1])

      if(n_classes == 2){
        # Binary classification - calculate probability for positive class only
        scorelearning_vec <- numeric(nrow(learningmodel))

        for(i in 1:nrow(learningmodel)){
          # Leave-one-out prediction for training set
          train_idx <- setdiff(1:nrow(learningmodel), i)

          # Get k nearest neighbors
          distances <- apply(learningmodel[train_idx, -1], 1, function(row) {
            sqrt(sum((as.numeric(learningmodel[i, -1]) - as.numeric(row))^2))
          })

          k_nearest_idx <- order(distances)[1:optimal_k]
          k_nearest_labels <- learningmodel[train_idx, 1][k_nearest_idx]

          # Calculate probability as proportion of positif class
          scorelearning_vec[i] <- sum(k_nearest_labels == lev["positif"]) / optimal_k
        }

        scorelearning <- data.frame(scorelearning_vec)
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")

        predictclasslearning<-factor(levels = lev)
        predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclasslearning<-as.factor(predictclasslearning)

      } else {
        # Multi-class - calculate probabilities for all classes
        scorelearning_matrix <- matrix(0, nrow=nrow(learningmodel), ncol=n_classes)
        colnames(scorelearning_matrix) <- lev

        for(i in 1:nrow(learningmodel)){
          # Leave-one-out prediction for training set
          train_idx <- setdiff(1:nrow(learningmodel), i)

          # Get k nearest neighbors
          distances <- apply(learningmodel[train_idx, -1], 1, function(row) {
            sqrt(sum((as.numeric(learningmodel[i, -1]) - as.numeric(row))^2))
          })

          k_nearest_idx <- order(distances)[1:optimal_k]
          k_nearest_labels <- learningmodel[train_idx, 1][k_nearest_idx]

          # Calculate probability for each class as proportion of k neighbors
          for(j in 1:n_classes){
            scorelearning_matrix[i, j] <- sum(k_nearest_labels == lev[j]) / optimal_k
          }
        }

        scorelearning <- scorelearning_matrix
        # Predict using argmax (no threshold for multi-class)
        predictclasslearning <- predict_from_scores(scorelearning, learningmodel[,1], threshold=NULL)
      }

    }

    if(modelparameters$modeltype=="elasticnet"){
      # DEPRECATED: ElasticNet classification - does not handle censored survival data
      warning("ElasticNet classification is deprecated. Use Cox ElasticNet (coxelasticnet) for survival analysis.")
      # Penalized Logistic Regression (ElasticNet)
      x <- as.matrix(learningmodel[,-1])
      n_classes <- get_n_classes(learningmodel[,1])

      # Encode y based on binary or multi-class
      if(n_classes == 2){
        # Binary: 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
        y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
        family_param <- "binomial"
        type_measure_param <- "auc"
      } else {
        # Multi-class: factor with original levels
        y <- learningmodel[,1]
        family_param <- "multinomial"
        type_measure_param <- "class"
      }
      
      # Get hyperparameters (use defaults if not provided)
      alpha_param <- ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha)
      lambda_param <- modelparameters$lambda  # NULL for CV selection
      
      # Check if GridSearchCV should be used
      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch && is.null(lambda_param)){
        # Use GridSearchCV from superml for comprehensive hyperparameter tuning
        cat("Using GridSearchCV for ElasticNet hyperparameter tuning...\n")
        
        # Prepare parameter grid
        param_grid <- list(
          alpha = if(!is.null(modelparameters$en_grid_alpha)) modelparameters$en_grid_alpha else c(0, 0.25, 0.5, 0.75, 1.0),
          lambda = if(!is.null(modelparameters$en_grid_lambda)) modelparameters$en_grid_lambda else c(0.001, 0.01, 0.1, 1.0)
        )
        
        # Run GridSearchCV
        grid_result <- tryCatch({
          X_df <- as.data.frame(x)
          tune_elasticnet_gridsearch(X = X_df, y = learningmodel[,1],
                                     param_grid = param_grid,
                                     n_folds = 5,
                                     scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, falling back to cv.glmnet:", e$message, "\n")
          NULL
        })
        
        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          alpha_param <- if(!is.null(best_params$alpha)) best_params$alpha else 0.5
          lambda_param <- if(!is.null(best_params$lambda)) best_params$lambda else NULL
          
          cat(sprintf("GridSearchCV best params: alpha=%.3f, lambda=%.4f, score=%.4f\n",
                      alpha_param, lambda_param, grid_result$best_score))
          
          # Use the best parameters to fit with cv.glmnet for consistency
          set.seed(20011203)
          cvfit <- cv.glmnet(x, y, family=family_param, alpha=alpha_param,
                             type.measure=type_measure_param, nfolds=min(10, nrow(learningmodel)-1))
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                        cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
        } else {
          # Fallback to traditional cv.glmnet if GridSearchCV fails
          set.seed(20011203)
          cvfit <- cv.glmnet(x, y, family=family_param, alpha=alpha_param,
                             type.measure=type_measure_param, nfolds=min(10, nrow(learningmodel)-1))
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                        cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
        }
      } else if(is.null(lambda_param)){
        # Perform cross-validation to find optimal lambda if not provided
        set.seed(20011203)
        cvfit <- cv.glmnet(x, y, family=family_param, alpha=alpha_param,
                           type.measure=type_measure_param, nfolds=min(10, nrow(learningmodel)-1))
        lambda_param <- cvfit$lambda.min
        model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                      cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
      } else {
        # Manual mode: use specified lambda and alpha parameters
        cat("Creating ElasticNet model with manual parameters: alpha=", alpha_param, ", lambda=", lambda_param, "\n")
        fit <- glmnet(x, y, family=family_param, alpha=alpha_param, lambda=lambda_param)
        model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                      cvfit=NULL, optimal_lambda=lambda_param, lambda_1se=NULL)
      }
      
      cat("la classe de model$glmnet_model est : ", class(model$glmnet_model), "\n" )
      
      # Feature selection based on non-zero coefficients
      if(modelparameters$fs){
        coef_values <- as.matrix(coef(model$glmnet_model, s=lambda_param))
        selected_features <- rownames(coef_values)[which(coef_values[-1,1] != 0)]
        if(length(selected_features) > 0){
          learningmodel <- learningmodel[, c("group", selected_features)]
          x <- as.matrix(learningmodel[,-1])
          # Refit model with selected features
          if(is.null(modelparameters$lambda)){
            cvfit <- cv.glmnet(x, y, family=family_param, alpha=alpha_param,
                               type.measure=type_measure_param, nfolds=min(10, nrow(learningmodel)-1))
            lambda_param <- cvfit$lambda.min
            # Refit model with optimal lambda to ensure we have a valid glmnet object
            fit <- glmnet(x, y, family=family_param, alpha=alpha_param, lambda=lambda_param)
            cat("class of fitted modele :  ", class(fit))
            model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                          cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
          } else {
            fit <- glmnet(x, y, family=family_param, alpha=alpha_param, lambda=lambda_param)
            model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                          cvfit=NULL, optimal_lambda=lambda_param, lambda_1se=NULL)
          }
        }
      }
      
      # Make predictions (probabilities) - adapt to binary or multi-class
      # Use appropriate predict method based on model class
      if(inherits(model$glmnet_model, "cv.glmnet")){
        predictions_raw <- glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x, s=lambda_param, type="response")
      } else {
        predictions_raw <- glmnet::predict.glmnet(model$glmnet_model, newx=x, s=lambda_param, type="response")
      }

      if(n_classes == 2){
        # Binary: predictions_raw is a vector/matrix with 1 column
        scorelearning <- data.frame(as.vector(predictions_raw))
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")

        predictclasslearning<-factor(levels = lev)
        predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclasslearning<-as.factor(predictclasslearning)
      } else {
        # Multi-class: predictions_raw is a 3D array (n_samples x n_classes x 1) for multinomial
        # Extract the probability matrix
        if(length(dim(predictions_raw)) == 3){
          scorelearning <- predictions_raw[,,1]  # Extract matrix from 3D array
        } else {
          scorelearning <- predictions_raw  # Already a matrix
        }
        colnames(scorelearning) <- lev

        # Predict using argmax (no threshold for multi-class)
        predictclasslearning <- predict_from_scores(scorelearning, learningmodel[,1], threshold=NULL)
      }
    }

    if(modelparameters$modeltype=="xgboost"){
      # DEPRECATED: XGBoost classification - does not handle censored survival data
      warning("XGBoost is deprecated for survival analysis. Use Cox or RSF models instead.")
      # XGBoost gradient boosting
      x <- as.matrix(learningmodel[,-1])
      n_classes <- get_n_classes(learningmodel[,1])

      # Encode y based on binary or multi-class
      if(n_classes == 2){
        # Binary: 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
        y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      } else {
        # Multi-class: encode as 0, 1, 2, ... (n_classes-1)
        y <- as.numeric(learningmodel[,1]) - 1
      }

      # Create DMatrix for XGBoost
      dtrain <- xgb.DMatrix(data = x, label = y)

      # Determine hyperparameters
      if(is.null(modelparameters$autotunexgb) || modelparameters$autotunexgb){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for XGBoost hyperparameter tuning...\n")

          # Prepare parameter grid
          param_grid <- list(
            n_estimators = if(!is.null(modelparameters$xgb_grid_nrounds)) modelparameters$xgb_grid_nrounds else c(50, 100, 200),
            max_depth = if(!is.null(modelparameters$xgb_grid_maxdepth)) modelparameters$xgb_grid_maxdepth else c(3, 6, 9),
            learning_rate = if(!is.null(modelparameters$xgb_grid_eta)) modelparameters$xgb_grid_eta else c(0.01, 0.1, 0.3),
            gamma = if(!is.null(modelparameters$xgb_grid_gamma)) modelparameters$xgb_grid_gamma else c(0, 0.1, 0.5),
            subsample = if(!is.null(modelparameters$xgb_grid_subsample)) modelparameters$xgb_grid_subsample else c(0.8, 1.0)
          )

          # Run GridSearchCV
          grid_result <- tryCatch({
            # Convert data for superml
            X_df <- as.data.frame(x)
            tune_xgb_gridsearch(X = X_df, y = learningmodel[,1],
                               param_grid = param_grid,
                               n_folds = 5,
                               scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to xgb.cv:", e$message, "\n")
            NULL
          })

          if(!is.null(grid_result)) {
            # Extract best parameters from GridSearchCV
            best_params <- grid_result$best_params

            optimal_nrounds <- if(!is.null(best_params$n_estimators)) best_params$n_estimators else 100
            optimal_max_depth <- if(!is.null(best_params$max_depth)) best_params$max_depth else 6
            optimal_eta <- if(!is.null(best_params$learning_rate)) best_params$learning_rate else 0.3
            optimal_gamma <- if(!is.null(best_params$gamma)) best_params$gamma else 0
            optimal_subsample <- if(!is.null(best_params$subsample)) best_params$subsample else 1.0
            optimal_min_child_weight <- if(!is.null(best_params$min_child_weight)) best_params$min_child_weight else 1

            cat(sprintf("GridSearchCV best params: nrounds=%d, max_depth=%d, eta=%.3f, gamma=%.3f, score=%.4f\n",
                       optimal_nrounds, optimal_max_depth, optimal_eta, optimal_gamma, grid_result$best_score))

            # Create final parameters list based on binary or multi-class
            if(n_classes == 2){
              final_params <- list(
                objective = "binary:logistic",
                eval_metric = "auc",
                max_depth = optimal_max_depth,
                eta = optimal_eta,
                gamma = optimal_gamma,
                subsample = optimal_subsample,
                min_child_weight = optimal_min_child_weight
              )
            } else {
              final_params <- list(
                objective = "multi:softprob",
                num_class = n_classes,
                eval_metric = "mlogloss",
                max_depth = optimal_max_depth,
                eta = optimal_eta,
                gamma = optimal_gamma,
                subsample = optimal_subsample,
                min_child_weight = optimal_min_child_weight
              )
            }

            # Train final model with optimal parameters
            model <- xgb.train(
              params = final_params,
              data = dtrain,
              nrounds = optimal_nrounds,
              verbose = 0
            )

            # Store optimal parameters
            model$optimal_nrounds <- optimal_nrounds
            model$optimal_max_depth <- optimal_max_depth
            model$optimal_eta <- optimal_eta
            model$optimal_gamma <- optimal_gamma
            model$optimal_subsample <- optimal_subsample
            model$optimal_min_child_weight <- optimal_min_child_weight
          } else {
            # Fallback to traditional xgb.cv if GridSearchCV fails
            # Perform hyperparameter tuning using cross-validation
            set.seed(20011203)

            # Parameter grid search - adapt to binary or multi-class
            if(n_classes == 2){
              best_params <- list(
                objective = "binary:logistic",
                eval_metric = "auc",
                max_depth = 6,
                eta = 0.3,
                min_child_weight = 1
              )
            } else {
              best_params <- list(
                objective = "multi:softprob",
                num_class = n_classes,
                eval_metric = "mlogloss",
                max_depth = 6,
                eta = 0.3,
                min_child_weight = 1
              )
            }

            # Cross-validation to find optimal nrounds
            cv_results <- xgb.cv(
              params = best_params,
              data = dtrain,
              nrounds = 200,
              nfold = min(5, nrow(learningmodel)-1),
              early_stopping_rounds = 10,
              verbose = 0
            )

            optimal_nrounds <- cv_results$best_iteration

            # Train final model with optimal parameters
            model <- xgb.train(
              params = best_params,
              data = dtrain,
              nrounds = optimal_nrounds,
              verbose = 0
            )

            # Store optimal parameters
            model$optimal_nrounds <- optimal_nrounds
            model$optimal_max_depth <- best_params$max_depth
            model$optimal_eta <- best_params$eta
            model$optimal_min_child_weight <- best_params$min_child_weight
          }
        } else {
          # Use traditional xgb.cv for hyperparameter tuning
          # Perform hyperparameter tuning using cross-validation
          set.seed(20011203)

          # Parameter grid search - adapt to binary or multi-class
          if(n_classes == 2){
            best_params <- list(
              objective = "binary:logistic",
              eval_metric = "auc",
              max_depth = 6,
              eta = 0.3,
              min_child_weight = 1
            )
          } else {
            best_params <- list(
              objective = "multi:softprob",
              num_class = n_classes,
              eval_metric = "mlogloss",
              max_depth = 6,
              eta = 0.3,
              min_child_weight = 1
            )
          }

          # Cross-validation to find optimal nrounds
          cv_results <- xgb.cv(
            params = best_params,
            data = dtrain,
            nrounds = 200,
            nfold = min(5, nrow(learningmodel)-1),
            early_stopping_rounds = 10,
            verbose = 0
          )

          optimal_nrounds <- cv_results$best_iteration

          # Train final model with optimal parameters
          model <- xgb.train(
            params = best_params,
            data = dtrain,
            nrounds = optimal_nrounds,
            verbose = 0
          )

          # Store optimal parameters
          model$optimal_nrounds <- optimal_nrounds
          model$optimal_max_depth <- best_params$max_depth
          model$optimal_eta <- best_params$eta
          model$optimal_min_child_weight <- best_params$min_child_weight
        }

      } else {
        # Use manual hyperparameters
        nrounds_param <- ifelse(is.null(modelparameters$nrounds), 100, modelparameters$nrounds)
        max_depth_param <- ifelse(is.null(modelparameters$max_depth), 6, modelparameters$max_depth)
        eta_param <- ifelse(is.null(modelparameters$eta), 0.3, modelparameters$eta)

        # Adapt params to binary or multi-class
        if(n_classes == 2){
          params <- list(
            objective = "binary:logistic",
            eval_metric = "auc",
            max_depth = max_depth_param,
            eta = eta_param,
            min_child_weight = 1
          )
        } else {
          params <- list(
            objective = "multi:softprob",
            num_class = n_classes,
            eval_metric = "mlogloss",
            max_depth = max_depth_param,
            eta = eta_param,
            min_child_weight = 1
          )
        }

        model <- xgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds_param,
          verbose = 0
        )

        # Store parameters
        model$optimal_nrounds <- nrounds_param
        model$optimal_max_depth <- max_depth_param
        model$optimal_eta <- eta_param
        model$optimal_min_child_weight <- 1
      }

      # Make predictions (probabilities) - adapt to binary or multi-class
      predictions_raw <- xgboost:::predict.xgb.Booster(model, x)

      if(n_classes == 2){
        # Binary: predictions_raw is a vector of probabilities for positive class
        scorelearning <- data.frame(predictions_raw)
        colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
        predictclasslearning<-factor(levels = lev)
        predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclasslearning<-as.factor(predictclasslearning)
      } else {
        # Multi-class: predictions_raw is a matrix (n_samples × n_classes)
        # reshape=TRUE ensures we get a matrix (nrows × nclasses)
        scorelearning <- matrix(predictions_raw, ncol=n_classes, byrow=TRUE)
        colnames(scorelearning) <- lev
        # Predict using argmax (no threshold for multi-class)
        predictclasslearning <- predict_from_scores(scorelearning, learningmodel[,1], threshold=NULL)
      }
    }

    #levels(predictclassval)<-paste("test",levels(predictclasslearning),sep="")
    levels(predictclasslearning)<-paste("test",lev,sep="")
    classlearning<-learningmodel[,1]
    
    ##########
    # # Calculate Youden threshold from training data
    # youden_result <- younden(classlearning, scorelearning[,1])
    # youden_threshold <- youden_result[4]  # 4th element is the threshold
    # 
    # # Update model parameters with Youden threshold
    # modelparameters$thresholdmodel <- youden_threshold
    # 
    # # Recalculate predictions using Youden threshold instead of fixed 0.5
    # predictclasslearning <- factor(levels = lev)
    # predictclasslearning[which(scorelearning[,1] >= youden_threshold)] <- lev["positif"]
    # predictclasslearning[which(scorelearning[,1] < youden_threshold)] <- lev["negatif"]
    # predictclasslearning <- as.factor(predictclasslearning)
    # levels(predictclasslearning)<-paste("test",lev,sep="")
    
    ########

    # Create results based on model type
    if(is_survival_model){
      # For survival models: store risk scores
      reslearningmodel <- list(
        riskscores = scorelearning$risk_score,
        scorelearning = scorelearning$risk_score,  # Keep for compatibility
        predictclasslearning = NULL,
        classlearning = NULL
      )
    } else {
      # For classification models: traditional format
      reslearningmodel<-data.frame(classlearning,scorelearning,predictclasslearning)
      colnames(reslearningmodel) <-c("classlearning","scorelearning","predictclasslearning")
    }

    datalearningmodel<-list("learningmodel"=learningmodel,"reslearningmodel"=reslearningmodel)
    
    if (modelparameters$adjustval){
      #Validation
      if(!is_survival_model){
        colnames(validation)[1]<-"group"
      }
      validationdiff<-validation[,which(colnames(validation)%in%colnames(learningmodel))]
      learningselect2<-learningselect
      if(transformdataparameters$log) { 
        validationdiff[,-1]<-transformationlog(x = validationdiff[,-1]+1,logtype =transformdataparameters$logtype )
        learningselect2[,-1]<-transformationlog(x = learningselect2[,-1]+1,logtype=transformdataparameters$logtype)}
      if(transformdataparameters$arcsin){
        maxlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = max,na.rm=T)
        minlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = min,na.rm=T)
        for (i in 2:dim(validationdiff)[2]){
        validationdiff[,i]<-(validationdiff[,i]-minlearn[i-1])/(maxlearn[i-1]-minlearn[i-1])
        #validationdiff[,-1]<-apply(X = as.data.frame(validationdiff[,-1]),MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        validationdiff[which(validationdiff[,i]>1),i]<-1
        validationdiff[which(validationdiff[,i]<0),i]<-0
        validationdiff[,i]<-asin(sqrt(validationdiff[,i]))
        }     
        learningselect2[,-1]<-apply(X = learningselect2[,-1],MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        learningselect2[,-1]<-asin(sqrt(learningselect2[,-1]))
      }
      if(transformdataparameters$standardization){
        learningselectval<<-learningselect2
        sdselect<-apply(learningselect2[,which(colnames(learningselect2)%in%colnames(validationdiff))], 2, sd,na.rm=T)
        print('sdselect')
        print(sdselect)
        validationdiff[,-1]<-scale(validationdiff[,-1],center=F,scale=sdselect[-1])
      }

      #NAstructure if NA ->0
      if(!is.null(datastructuresfeatures)){
        validationdiff[which(is.na(validationdiff),arr.ind = T)[which(which(is.na(validationdiff),arr.ind = T)[,2]%in%which(colnames(validationdiff)%in%datastructuresfeatures$names)),]]<-0
      }
      #
      validationmodel<<- replaceNAvalidation(as.data.frame(validationdiff[,-1]),toto=as.data.frame(learningmodel[,-1]),rempNA=transformdataparameters$rempNA)
      colnames(validationmodel)<-colnames(validationdiff)[-1]
      rownames(validationmodel)<-rownames(validationdiff)
      
      #prediction a partir du model
      # SURVIVAL MODELS VALIDATION
      if(is_survival_model){
        # For survival models: predict risk scores on validation set
        if(modelparameters$modeltype == "rsf"){
          risksval <- get_risk_scores(model, validationmodel, model_type = "rsf")
        } else if(modelparameters$modeltype == "cox"){
          risksval <- get_risk_scores(model, validationmodel, model_type = "cox")
        } else if(modelparameters$modeltype %in% c("coxlasso", "coxelasticnet", "coxridge")){
          risksval <- get_risk_scores(model, validationmodel, model_type = "coxnet")
        }

        scoreval <- risksval
        predictclassval <- NULL
        classval <- NULL

      } else if(modelparameters$modeltype=="randomforest"){
        pred_probs_val <- randomForest:::predict.randomForest(
          object=model, type="prob", newdata=validationmodel
        )
        
        if(n_classes_val == 2){
          # Binary: extract probability for positive class
          scoreval <- pred_probs_val[,lev["positif"]]
          predictclassval <- vector(length = length(scoreval))
          predictclassval[scoreval >= modelparameters$thresholdmodel] <- lev["positif"]
          predictclassval[scoreval < modelparameters$thresholdmodel] <- lev["negatif"]
          predictclassval <- as.factor(predictclassval)
        } else {
          # Multi-class: use probability matrix + argmax
          scoreval <- pred_probs_val[, lev]
          predictclassval <- predict_from_scores(scoreval, validation[,1], threshold=NULL)
        }
      }
      
      if(modelparameters$modeltype=="svm"){
      # DEPRECATED: SVM - does not handle censored survival data
      warning("SVM is deprecated for survival analysis. Use Cox models instead.")
        if(!is.null(model)){
          if(n_classes_val == 2){
            # Binary: use decision values
            scoreval <- attr(e1071:::predict.svm(
              model, newdata=validationmodel, decision.values=T
            ), "decision.values")
            if(sum(lev == strsplit(colnames(scoreval), "/")[[1]]) == 0){
              scoreval <- scoreval * (-1)
            }
            predictclassval <- vector(length = length(scoreval))
            predictclassval[scoreval >= modelparameters$thresholdmodel] <- lev["positif"]
            predictclassval[scoreval < modelparameters$thresholdmodel] <- lev["negatif"]
            predictclassval <- as.factor(predictclassval)
          } else {
            # Multi-class: use probabilities
            pred_probs_val <- attr(e1071:::predict.svm(
              model, newdata=validationmodel, probability=TRUE
            ), "probabilities")
            scoreval <- pred_probs_val[, lev]
            predictclassval <- predict_from_scores(scoreval, validation[,1], threshold=NULL)
          }
        }
      }

      if(modelparameters$modeltype=="elasticnet"){
      # DEPRECATED: ElasticNet classification - does not handle censored survival data
      warning("ElasticNet classification is deprecated. Use Cox ElasticNet (coxelasticnet) for survival analysis.")
        req(model$glmnet_model)
        x_val <- as.matrix(validationmodel)
        
        # Handle both cv.glmnet and glmnet objects
        if(inherits(model$glmnet_model, "cv.glmnet")){
          predictions_raw_val <- glmnet:::predict.cv.glmnet(
            model$glmnet_model, newx=x_val, s=model$lambda, type="response"
          )
        } else {
          predictions_raw_val <- glmnet::predict.glmnet(
            model$glmnet_model, newx=x_val, s=model$lambda, type="response"
          )
        }
        
        if(n_classes_val == 2){
          # Binary
          scoreval <- as.vector(predictions_raw_val)
          predictclassval <- vector(length = length(scoreval))
          predictclassval[scoreval >= modelparameters$thresholdmodel] <- lev["positif"]
          predictclassval[scoreval < modelparameters$thresholdmodel] <- lev["negatif"]
          predictclassval <- as.factor(predictclassval)
        } else {
          # Multi-class: extract matrix from 3D array
          if(length(dim(predictions_raw_val)) == 3){
            scoreval <- predictions_raw_val[,,1]
          } else {
            scoreval <- predictions_raw_val
          }
          colnames(scoreval) <- lev
          predictclassval <- predict_from_scores(scoreval, validation[,1], threshold=NULL)
        }
      }
      if(modelparameters$modeltype=="xgboost"){
      # DEPRECATED: XGBoost classification - does not handle censored survival data
      warning("XGBoost is deprecated for survival analysis. Use Cox or RSF models instead.")
        # XGBoost validation predictions
        x_val <- as.matrix(validationmodel)
        dval <- xgb.DMatrix(data = x_val)
        predictions_raw_val <- xgboost:::predict.xgb.Booster(model, dval)
        
        if(n_classes_val == 2){
          # Binary
          scoreval <- predictions_raw_val
          predictclassval<-vector(length = length(scoreval) )
          predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
          predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
          predictclassval<-as.factor(predictclassval)
        } else {
          # Multi-class
          scoreval <- matrix(predictions_raw_val, ncol=n_classes_val, byrow=TRUE)
          colnames(scoreval) <- lev
          # Predict using argmax
          predictclassval <- predict_from_scores(scoreval, validation[,1], threshold=NULL)
        }
      }
      

      if(modelparameters$modeltype=="lightgbm"){
      # DEPRECATED: LightGBM classification - does not handle censored survival data
      warning("LightGBM is deprecated for survival analysis. Use Cox or RSF models instead.")
        # LightGBM validation predictions
        x_val <- as.matrix(validationmodel)
        predictions_raw_val <- predict(model, x_val)
        
        if(n_classes_val == 2){
          # Binary
          scoreval <- predictions_raw_val
          predictclassval<-vector(length = length(scoreval) )
          predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
          predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
          predictclassval<-as.factor(predictclassval)
        } else {
          # Multi-class
          scoreval <- matrix(predictions_raw_val, ncol=n_classes_val, byrow=TRUE)
          colnames(scoreval) <- lev
          # Predict using argmax
          predictclassval <- predict_from_scores(scoreval, validation[,1], threshold=NULL)
        }
      }

      if(modelparameters$modeltype=="naivebayes"){
      # DEPRECATED: Naive Bayes - does not handle censored survival data
      warning("Naive Bayes is deprecated for survival analysis. Use Cox models instead.")
        # Naive Bayes validation predictions
        pred_probs_val <- e1071:::predict.naiveBayes(model, validationmodel, type="raw")
        
        if(n_classes_val == 2){
          # Binary
          scoreval <- pred_probs_val[, lev["positif"]]
          predictclassval<-vector(length = length(scoreval) )
          predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
          predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
          predictclassval<-as.factor(predictclassval)
        } else {
          # Multi-class
          scoreval <- pred_probs_val[, lev]  # Reorder to match level order
          # Predict using argmax
          predictclassval <- predict_from_scores(scoreval, validation[,1], threshold=NULL)
        }
      }

      if(modelparameters$modeltype=="knn"){
      # DEPRECATED: KNN - does not handle censored survival data
      warning("KNN is deprecated for survival analysis. Use Cox or RSF models instead.")
        # KNN validation predictions
        if(n_classes_val == 2){
          # Binary
          scoreval_vec <- numeric(nrow(validationmodel))
          for(i in 1:nrow(validationmodel)){
            distances <- apply(model$train_data, 1, function(row) {
              sqrt(sum((as.numeric(validationmodel[i, ]) - as.numeric(row))^2))
            })
            k_nearest_idx <- order(distances)[1:model$optimal_k]
            k_nearest_labels <- model$train_labels[k_nearest_idx]
            scoreval_vec[i] <- sum(k_nearest_labels == lev["positif"]) / model$optimal_k
          }
          scoreval <- scoreval_vec
          
          predictclassval<-vector(length = length(scoreval) )
          predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
          predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
          predictclassval<-as.factor(predictclassval)
        } else {
          # Multi-class
          scoreval_matrix <- matrix(0, nrow=nrow(validationmodel), ncol=n_classes_val)
          colnames(scoreval_matrix) <- lev
          
          for(i in 1:nrow(validationmodel)){
            distances <- apply(model$train_data, 1, function(row) {
              sqrt(sum((as.numeric(validationmodel[i, ]) - as.numeric(row))^2))
            })
            k_nearest_idx <- order(distances)[1:model$optimal_k]
            k_nearest_labels <- model$train_labels[k_nearest_idx]
            
            for(j in 1:n_classes_val){
              scoreval_matrix[i, j] <- sum(k_nearest_labels == lev[j]) / model$optimal_k
            }
          }
          scoreval <- scoreval_matrix
          # Predict using argmax
          predictclassval <- predict_from_scores(scoreval, validation[,1], threshold=NULL)
        }
      }

      if(sum(lev==(levels(predictclassval)))==0){
        predictclassval<-factor(predictclassval,levels = rev(levels(predictclassval)),ordered = TRUE)
      }
      classval<- validation[,1]
      if(sum(lev==(levels(classval)))==0){
        classval<-factor(classval,levels = rev(levels(classval)),ordered = TRUE)
      }
      
      # Create validation results based on model type
      if(is_survival_model){
        # For survival models
        resvalidationmodel <- list(
          riskscores = scoreval,
          scoreval = scoreval,  # Keep for compatibility
          predictclassval = NULL,
          classval = NULL
        )

        # Combine validation data with time and status
        validationmodel_full <- cbind(validationdiff[, 1:2], validationmodel)
        colnames(validationmodel_full)[1:2] <- c("time", "status")

        datavalidationmodel <- list(
          "validationdiff" = validationdiff,
          "validationmodel" = validationmodel_full,
          "resvalidationmodel" = resvalidationmodel,
          "auc" = NA  # No AUC for survival models
        )
      } else {
        # For classification models
        #levels(predictclassval)<-paste("test",levels(predictclassval),sep="")
        levels(predictclassval)<-paste("test",lev,sep="")
        resvalidationmodel<-data.frame(classval,scoreval,predictclassval)
        colnames(resvalidationmodel) <-c("classval","scoreval","predictclassval")
        auc<-auc(roc(as.vector(classval), as.vector(scoreval),quiet=T))
        datavalidationmodel<-list("validationdiff"=validationdiff,"validationmodel"=validationmodel,"resvalidationmodel"=resvalidationmodel,"auc"=auc)
      }
      
    }
    else{datavalidationmodel<-list()}

    # Return results with appropriate groups field
    groups_val <- if(is_survival_model) NULL else lev
    res<-list("datalearningmodel"=datalearningmodel,"model"=model,"datavalidationmodel"=datavalidationmodel,"groups"=groups_val,"parameters"=modelparameters)
  }
}


replaceNAvalidation<-function(validationdiff,toto,rempNA){
  validationdiffssNA<-validationdiff
  for(i in 1:nrow(validationdiff)){
    validationdiffssNA[i,]<-replaceNAoneline(lineNA = validationdiff[i,],toto = toto,rempNA =rempNA)
  }
  return(validationdiffssNA)
}

replaceNAoneline<-function(lineNA,toto,rempNA){
  alldata<-rbind(lineNA,toto)
  if(rempNA=="moygr"){ 
    #print("impossible de remplacer les NA par la moyenne par group pour la validation")
    linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA ="moy")[1,-1]        }
  
  else{linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA =rempNA)[1,-1]}
  
  return(linessNA)
}




ROCcurve<-function(validation,decisionvalues,maintitle="Roc curve",graph=T,ggplot=T){
  validation<-factor(validation,levels = rev(levels(validation)),ordered = TRUE)
  n_classes <- length(levels(validation))

  # Detect if binary or multi-class
  if(n_classes == 2){
    # BINARY CLASSIFICATION - Original code
    #argument : validation, vector of appartenance,
    #            decisionvalues, vector of scores
    data<-roc(validation,decisionvalues)
    if(!graph){return(data.frame("sensitivity"=data$sensitivities,"specificity"=data$specificities,"thresholds"=data$thresholds))}
    if(!ggplot){plot(data)}
    if(ggplot){
      y<-rev(data$sensitivities)
      x<-rev(data$specificities)
      roc<-data.frame(x,y)
      auc<-as.numeric(auc(data))

      col<-gg_color_hue(3)
      roccol<-col[1]
      bin = 0.01
      diag = data.frame(x = seq(0, 1, by = bin), y = rev(seq(0, 1, by = bin)))
      p <- ggplot(data = roc, aes(x = x, y = y)) +
        geom_point(color = roccol) +
        geom_line(color = roccol) +
        geom_line(data = diag, aes(x = x, y = y), color =col[3])
      sp = 19
      f <- p + geom_point(data = diag, aes(x = x, y = y), color = "lightgrey", shape = sp) +
        theme(axis.text = element_text(size = 16),
              title = element_text(size = 15) ,
              axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
              axis.text.y =  element_text(size = 12 , face =  'bold'),
              axis.title.x = element_text(size = 15 , face = 'bold'),
              axis.title.y =  element_text(size = 15 , face = 'bold')
              ) +
        labs(y = "Sensitivity", x = "1 - Specificity", title = maintitle) +
        annotate("text",x=0.2,y=0.1,label=paste("AUC = ",as.character(round(auc,digits = 3))),size=7,colour= roccol)+
        scale_x_reverse()

      f
    }
  } else {
    # MULTI-CLASS CLASSIFICATION - New implementation
    # decisionvalues should be a matrix (n_samples x n_classes) for multi-class

    # Check if decisionvalues is a matrix
    if(!is.matrix(decisionvalues)){
      # If it's a vector, we can't properly plot multi-class ROC
      # Return a simple message
      if(!graph){
        return(data.frame(message="Multi-class ROC requires probability matrix"))
      }
      if(ggplot){
        p <- ggplot() +
          annotate("text", x=0.5, y=0.5, label="Multi-class ROC requires\nprobability matrix for each class", size=6) +
          labs(title = maintitle) +
          theme_minimal()
        return(p)
      }
    }

    # Calculate One-vs-Rest ROC curves for each class
    roc_list <- list()
    auc_values <- vector()
    class_names <- levels(validation)

    for(i in 1:n_classes){
      # Create binary indicator for this class
      binary_response <- ifelse(as.numeric(validation) == (n_classes - i + 1), 1, 0)

      # Get probabilities for this class
      class_probs <- decisionvalues[, i]

      # Calculate ROC
      roc_obj <- tryCatch({
        roc(binary_response, class_probs, quiet=TRUE)
      }, error = function(e){
        return(NULL)
      })

      if(!is.null(roc_obj)){
        roc_list[[class_names[n_classes - i + 1]]] <- roc_obj
        auc_values[i] <- as.numeric(auc(roc_obj))
      }
    }

    # Calculate mean AUC
    mean_auc <- mean(auc_values, na.rm=TRUE)

    if(!graph){
      return(data.frame(
        class = names(roc_list),
        auc = auc_values
      ))
    }

    if(ggplot){
      # Plot One-vs-Rest ROC curves
      col <- gg_color_hue(n_classes + 1)
      bin = 0.01
      diag = data.frame(x = seq(0, 1, by = bin), y = rev(seq(0, 1, by = bin)))

      # Create plot
      p <- ggplot() +
        geom_line(data = diag, aes(x = x, y = y), color = col[n_classes + 1], linetype="dashed")

      # Add ROC curve for each class
      for(i in 1:length(roc_list)){
        class_name <- names(roc_list)[i]
        roc_obj <- roc_list[[class_name]]

        y <- rev(roc_obj$sensitivities)
        x <- rev(roc_obj$specificities)
        roc_df <- data.frame(x=x, y=y, class=class_name)

        p <- p +
          geom_line(data = roc_df, aes(x = x, y = y, color = class), size=1)
      }

      # Add AUC annotations
      auc_text <- paste0(names(roc_list), ": ", round(auc_values, 3), collapse="\n")
      mean_auc_text <- paste0("Mean AUC: ", round(mean_auc, 3))

      f <- p +
        theme(axis.text = element_text(size = 16),
              title = element_text(size = 15),
              axis.text.x = element_text(size = 12, face = 'bold'),
              axis.text.y = element_text(size = 12, face = 'bold'),
              axis.title.x = element_text(size = 15, face = 'bold'),
              axis.title.y = element_text(size = 15, face = 'bold'),
              legend.position = "right") +
        labs(y = "Sensitivity (TPR)", x = "1 - Specificity (FPR)",
             title = maintitle, color = "Class") +
        annotate("text", x=0.3, y=0.1, label=mean_auc_text, size=5, fontface="bold") +
        scale_x_reverse()

      f
    }
  }
}

scoremodelplot<-function(class,score,names,threshold,type,graph,printnames){
  class<-factor(class,levels =rev(levels(class)))

  if(type=="boxplot"){
    boxplotggplot(class =class,score =score,names=names,threshold=threshold,
                  graph = graph)
  }
  else if(type=="points"){
    plot_pred_type_distribution(class = class, score = score,names=names,threshold=threshold,graph=graph,printnames=printnames  )
  } 
}

boxplotggplot<-function(class,score,names,threshold,maintitle="Score representation ",graph=T){
  data<-data.frame("names"=names,"class"= class,"score"=as.vector(score))
  if(!graph){return(data)}
  p<-ggplot(data, aes(x=class, y=score)) +
    scale_fill_manual( values = c("#00BFC4","#F8766D") ) +
    geom_boxplot(aes(fill=class)) +
    geom_hline(yintercept = threshold, color='red', alpha=0.6) +
    ggtitle(maintitle) + 
    theme(plot.title=element_text( size=15), 
          axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
          axis.text.y =  element_text(size = 12 , face =  'bold'),
          axis.title.x = element_text(size = 15 , face = 'bold'), 
          axis.title.y =  element_text(size = 15 , face = 'bold'),
          legend.text = element_text( size = 12 , face = 'bold'),
          legend.title = element_text(size = 14 , face =  'bold'))
  
  p
}

plot_pred_type_distribution <- function(class,score,names, threshold,maintitle="Score representation",printnames=F,graph=T) {
  #in this function the levels of the class is inverted in order to have the control group on the left side of the graph
  df<-data.frame(names,class,score)
  colnames(df)<-c("names","class","score")
  v <-rep(NA, nrow(df))
  v <- ifelse(df$score >= threshold & df$class == levels(class)[2], "TruePositiv", v)
  v <- ifelse(df$score >= threshold & df$class == levels(class)[1], "FalsePositiv", v)
  v <- ifelse(df$score < threshold & df$class ==  levels(class)[2], "FalseNegativ", v)
  v <- ifelse(df$score < threshold & df$class == levels(class)[1], "TrueNegativ", v)
  
  df$predtype <-factor(v,levels = c("FalseNegativ","FalsePositiv","TrueNegativ","TruePositiv"),ordered = T)
  if(!graph){return(df)}
  set.seed(20011203)
  if(printnames){
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_text(label=names,colour=palet(df$predtype,multiple = TRUE))+
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15),
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  else{
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15), 
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  g
  
}

palet<-function(predtype,multiple=FALSE){
  if(multiple){col<-as.character(predtype)}
  else{col<-sort(unique(as.character(predtype)))}
  col[which(col=="FalseNegativ")]<-"#C77CFF"
  col[which(col=="FalsePositiv")]<-"#00BA38"
  col[which(col=="TrueNegativ")]<-"#00BFC4"
  col[which(col=="TruePositiv")]<-"#F8766D"
  return(col)
}


selectedfeature<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype="learn"){
  rmvar<-testmodel(model=model,modeltype = modeltype,tab=tab,validation=validation,
                   criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  i=0
  tabdiff2<-tab
  while(rmvar!=0){
    i<-i+1
    print(paste(i,"eliminates features"))
    tabdiff2<-tabdiff2[,-rmvar]
    if(modeltype=="svm"){
      tune_result <- tune.svm(x=tabdiff2[,-1], y=tabdiff2[,1],
                             gamma = 10^(-5:2), cost = 10^(-3:2),
                             cross=min(dim(tabdiff2)[1]-2,10))
      model <- tune_result$best.model
      model$cost <- tune_result$best.parameters$cost
      model$gamma <- tune_result$best.parameters$gamma
    }
    if (modeltype=="randomforest"){      
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T)
    }
      rmvar<-testmodel(model=model,modeltype = modeltype,tab=tabdiff2,validation=validation,
                     criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  }
  res<-list("dataset"=tabdiff2,"model"=model)
  return(res)
}


testmodel<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype){
  #retourn la variable a enlever
  importancevar<-importancemodelsvm(model = model,modeltype=modeltype,tabdiff=tab,criterion = criterionimportance)
  lessimportantevar<-which(importancevar==min(importancevar,na.rm =T) )
  test<-vector()
  if(modeltype=="svm"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$fitted)}
    if(criterionmodel=="auc"){
      if (fstype=='learn'){aucmod<-auc(roc(tab[,1], as.vector(model$decision.values),quiet=T))}
      if (fstype=='val'){
        print("")
        #predict sur la validation
        #mais pour ca validation doit etre = a validationmodel, avec toute les transformation
        }}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tune_result_diff <- tune.svm(x=tabdiff2[,-1], y=tabdiff2[,1],
                                  gamma = 10^(-5:2), cost = 10^(-3:2),
                                  cross=min(dim(tabdiff2)[1]-2,10))
      resmodeldiff <- tune_result_diff$best.model
      if(criterionmodel=="accuracy"){test[i]<-resmodeldiff$tot.accuracy-model$tot.accuracy}
      if(criterionmodel=="BER"){
        #print(paste("Ber test :",BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted) ))
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted)}
      if(criterionmodel=="auc"){
        test[i]<-auc(roc(tabdiff2[,1], as.vector(resmodeldiff$decision.values),quiet=T))-aucmod}
    }}
  if(modeltype=="randomforest"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$predicted)}
    if(criterionmodel=="auc"){aucmod<-auc(roc(tab[,1], as.vector(model$votes[,1]),quiet=T))}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      resmodeldiff <-randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T,trace=T)
      if(criterionmodel=="accuracy"){test[i]<-mean(resmodeldiff$confusion[,3])-mean(model$confusion[,3])}
      if(criterionmodel=="BER"){
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$predicted)}
      if(criterionmodel=="auc"){
        test[i]<-auc(roc(tabdiff2[,1], as.vector(resmodeldiff$votes[,1]),quiet=T))-aucmod}
    }
  }
  #print(paste("test :",max(test)))
  if(max(test)>=0){num<-lessimportantevar[which(test==max(test))[1]]}
  else(num<-0)
  #print(paste( "num", num))
  return(num)
} 

importancemodelsvm<-function(model,modeltype,tabdiff,criterion){
  #function calculate the importance of each variable of the model
  #first column of tabdiff is the group
  importancevar<-vector()
  if(criterion=="accuracy"){
    if(modeltype=="svm"){
      for (i in 2:ncol(tabdiff)){
        vec<-vector()
        tabdiffmodif<-tabdiff
        for( j in 1:20){
          tabdiffmodif[,i]<-tabdiffmodif[sample(1:nrow(tabdiff)),i]
          #tabdiffmodif<-tabdiffmodif[,-i]
          
          resmodeldiff<-svm(y =tabdiffmodif[,1],x=tabdiffmodif[,-1],cross=10,
                            type ="C-classification",
                            kernel= ifelse(is.null(model$kernel),"radial",model$kernel),
                            cost=model$cost,
                            gamma=model$gamma)
          vec[j]<-abs(resmodeldiff$tot.accuracy-model$tot.accuracy)
        }
        importancevar[i]<-mean(vec)}
      
    }
    if(modeltype=="randomforest"){
      
      tabdiff<-as.data.frame(tabdiff[,c(colnames(tabdiff)[1],sort(colnames(tabdiff[,-1])))])
      tabdiff<-as.data.frame(tabdiff2[sort(rownames(tabdiff)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff[,-1],tabdiff[,1],ntree=1000,importance=T,keep.forest=T)
      importancevar<-model$importance[,4]
      importancevar<-c(NA,importancevar)
    }
  }
  if(criterion=="fscore"){
    importancevar<-Fscore(tab = as.data.frame(tabdiff[,-1]),class=tabdiff[,1])
  }
  return(importancevar)
}
Fscore<-function(tab,class){
  tabpos<-as.data.frame(tab[which(class==levels(class)[1]),])
  npos<-nrow(tabpos)
  tabneg<-as.data.frame(tab[which(class==levels(class)[2]),])
  nneg<-nrow(tabneg)
  fscore<-vector()
  for(i in 1:ncol(tab)){
    moypos<-mean(tabpos[,i])
    moyneg<-mean(tabneg[,i])
    moy<-mean(tab[,i])
    numerateur<-(moypos-moy)^2+(moyneg-moy)^2
    denominateur<-(sum((tabpos[,i]-moypos)^2)*(1/(npos-1)))+(sum((tabneg[,i]-moyneg)^2)*(1/(nneg-1)))
    fscore[i]<-numerateur/denominateur
  }
  return(c(NA,fscore))
}

BER<-function(class,classpredict){
  pos<-which(class==levels(class)[1])
  neg<-which(class==levels(class)[2])
  (1/2)*( sum(class[pos]!=classpredict[pos])/length(pos)+ sum(class[neg]!=classpredict[neg])/length(neg)  )
}

nll<-function(element){
  if(is.null(element)){return("")}
  else{return(element)}
}

sensibility<-function(predict,class){
data<-table(predict,class)
sensi<-round(data[1,1]/(data[1,1]+data[2,1]),digits = 3)
return(sensi)
}
specificity<-function(predict,class){
  data<-table(predict,class )
  round(data[2,2]/(data[1,2]+data[2,2]),digit=3)
}

# cette fonction construit un tableau de parametres a tester a partir d'une liste de parametres
# chaque element de la liste est un vecteur de valeurs a tester pour le parametre correspondant
constructparameters<-function(listparameters){
  resparameters<-data.frame(listparameters[[1]])
  namescol<-names(listparameters)
  
  for(i in 2:length(listparameters)){
    tt<-rep(listparameters[[i]],each=nrow(resparameters))
    res<-resparameters
    if(length(listparameters[[i]])>1){
      for (j in 1:(length(listparameters[[i]])-1)){
        res<-rbind(res,resparameters)
      }
    }
    resparameters<-cbind(res,tt)
  }
  colnames(resparameters)<-namescol
  return(resparameters)
}

testparametersfunction<-function(learning,validation,tabparameters){
  set.seed(20011203)
  # results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=9 )
  # colnames(results)<-c("auc validation","sensibility validation","specificityvalidation",
  #                      "auc learning","sensibility learning","specificity learning",
  #                      "number of features in model","number of differented features",
  #                      "number of features selected")
  
  results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=10)
  colnames(results)<-c("auc validation","sensibility validation","specificity validation",
                       "auc learning","sensibility learning","specificity learning",
                       "threshold used","number of features in model",
                       "number of differented features","number of features selected")
  print(paste(nrow(tabparameters),"parameters "))
  for (i in 1:nrow(tabparameters)){
    print(i)
    parameters<-tabparameters[i,]
    if(!parameters$NAstructure){tabparameters[i,c("thresholdNAstructure","structdata","maxvaluesgroupmin","minvaluesgroupmax")]<-rep(x = NA,4)    }
    #selectdataparameterst<-parameters[1:7]
    selectdataparameters<<-list("prctvalues"=parameters$prctvalues,
                                "selectmethod"=parameters$selectmethod,
                                "NAstructure"=parameters$NAstructure,
                                "structdata"=parameters$structdata,
                                "thresholdNAstructure"=parameters$thresholdNAstructure,
                                "maxvaluesgroupmin"=parameters$maxvaluesgroupmin,"minvaluesgroupmax"=parameters$minvaluesgroupmax)
    resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
    
    #transformdataparameters<<-parameters[8:11]
    if(!parameters$log){tabparameters[i,"logtype"]<-NA}
    transformdataparameters<<-list("log"=parameters$log,"logtype"=parameters$logtype,"standardization"=parameters$standardization,"arcsin"=parameters$arcsin,"rempNA"=parameters$rempNA)
    
    learningtransform<-transformdatafunction(learningselect = resselectdata$learningselect,structuredfeatures = resselectdata$structuredfeatures,
                                             datastructuresfeatures =   resselectdata$datastructuresfeatures,transformdataparameters = transformdataparameters)
    
    testparameters<<-list("SFtest"=FALSE,"test"=parameters$test,"adjustpval"=as.logical(parameters$adjustpv),"thresholdpv"=parameters$thresholdpv,"thresholdFC"=parameters$thresholdFC)
    restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters)
    
    if(parameters$test=="notest"){
      learningmodel<-learningtransform
      tabparameters[i,c("adjustpv","thresholdpv","thresholdFC")]<-rep(x = NA,3)
    }
    else{learningmodel<-restest$tabdiff}
    
    if(ncol(learningmodel)!=0){
    
    # Determine if automatic tuning should be used based on tuning_method parameter
    use_autotuning <- (!is.null(parameters$tuning_method) && parameters$tuning_method == "automatic")
    
    # Set autotuning flags for each model type
    autotunerf_flag <- use_autotuning
    autotunesvm_flag <- use_autotuning
    autotunexgb_flag <- use_autotuning
    autotunelgb_flag <- use_autotuning
    autotuneknn_flag <- use_autotuning
      
    modelparameters<<-list("modeltype"=parameters$model,
                           "invers"=FALSE,
                           "thresholdmodel"=parameters$thresholdmodel,
                           "fs"=as.logical(parameters$fs),
                           "adjustval"=!is.null(validation),
                           "autotunerf"=autotunerf_flag,
                           "autotunesvm"=autotunesvm_flag,
                           "autotunexgb"=autotunexgb_flag,
                           "autotunelgb"=autotunelgb_flag,
                           "autotuneknn"=autotuneknn_flag
                           )
    validate(need(ncol(learning)!=0,"No select dataset"))
    

    #resmodel<<-modelfunction(learningmodel = learningmodel,validation = validation,modelparameters = modelparameters,
    #                         transformdataparameters = transformdataparameters,datastructuresfeatures =  datastructuresfeatures)
    out<- tryCatch(modelfunction(learningmodel = learningmodel,
                                 validation = validation,
                                 modelparameters = modelparameters,
                                 transformdataparameters = transformdataparameters,
                                 datastructuresfeatures =  datastructuresfeatures,
                                 learningselect = resselectdata$learningselect), 
                   error = function(e) e)
    if(any(class(out)=="error"))parameters$model<-"nomodel"
    else{
      
      resmodel<-out
      
      # Apply threshold optimization if requested
      if(!is.null(parameters$threshold_method) && parameters$threshold_method != "fixed" && parameters$model != "nomodel"){
        tryCatch({
          # Calculate optimal threshold from ROC curve on learning data
          classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
          scorelearning <- resmodel$datalearningmodel$reslearningmodel$scorelearning
          
          # Create ROC object
          roc_obj <- roc(classlearning, scorelearning, quiet=TRUE)
          
          # Find optimal threshold based on selected method
          if(parameters$threshold_method == "youden"){
            # Youden method: maximizes sensitivity + specificity - 1
            optimal_coords <- coords(roc_obj, "best", best.method="youden", ret=c("threshold", "sensitivity", "specificity"))
            optimal_threshold <- optimal_coords$threshold
            
            # Display optimization results
            cat(sprintf("    ✓ Youden optimization (iter %d): threshold=%.4f (sens=%.3f, spec=%.3f, Youden=%.3f)\n",
                        i, optimal_threshold,
                        optimal_coords$sensitivity,
                        optimal_coords$specificity,
                        optimal_coords$sensitivity + optimal_coords$specificity - 1))
            
          } else if(parameters$threshold_method == "equiprob"){
            # Equiprobability method: closest point to diagonal (minimizes |FP-FN|)
            optimal_coords <- coords(roc_obj, "best", best.method="closest.topleft", ret=c("threshold", "sensitivity", "specificity"))
            optimal_threshold <- optimal_coords$threshold
            
            # Calculate false positive and false negative rates for display
            fp_rate <- 1 - optimal_coords$specificity
            fn_rate <- 1 - optimal_coords$sensitivity
            
            # Display optimization results
            cat(sprintf("    ✓ Equiprobability optimization (iter %d): threshold=%.4f (sens=%.3f, spec=%.3f, FPR=%.3f, FNR=%.3f)\n",
                        i, optimal_threshold,
                        optimal_coords$sensitivity,
                        optimal_coords$specificity,
                        fp_rate, fn_rate))
          }
          
          # Recalculate predicted classes using optimal threshold for learning data
          # IMPORTANT: In this application, levels(classlearning)[1] = "positif" (case)
          # Score represents probability of being positive, so high score → predict positive
          # Therefore: score >= threshold → levels[1] (positif), score < threshold → levels[2] (negatif)
          resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[1], levels(classlearning)[2])
          resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- factor(resmodel$datalearningmodel$reslearningmodel$predictclasslearning, levels = levels(classlearning))
          
          # If validation data exists, apply optimal threshold to validation predictions as well
          # Same logic: high score → predict positive (level 1)
          if(!is.null(validation)){
            classval <- resmodel$datavalidationmodel$resvalidationmodel$classval
            scoreval <- resmodel$datavalidationmodel$resvalidationmodel$scoreval
            resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[1], levels(classval)[2])
            resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- factor(resmodel$datavalidationmodel$resvalidationmodel$predictclassval, levels = levels(classval))
          }
          
          # Update threshold in parameters for record
          parameters$thresholdmodel <- optimal_threshold
        }, error = function(e){
          # If threshold optimization fails, continue with original threshold
          cat(sprintf("    ✗ Threshold optimization FAILED (iteration %d): %s\n", i, e$message))
          cat(sprintf("      → Keeping initial threshold: %.4f\n", parameters$thresholdmodel))
          warning(paste("Threshold optimization failed:", e$message))
        })
      } else {
        # For "fixed" threshold method, use the threshold from parameters (already set to 0.5 for proba, 0 for SVM)
        # The classes are already predicted in modelfunction with this threshold
        # No need to recalculate, just ensure threshold is recorded
        if(parameters$model != "nomodel" && parameters$model != "svm"){
          # For probabilistic models, threshold should be 0.5 (already set)
          # For SVM, threshold is 0 (handled in modelfunction)
          # Just ensure the threshold is recorded correctly
          if(is.null(parameters$thresholdmodel) || is.na(parameters$thresholdmodel)){
            parameters$thresholdmodel <- 0.5
          }
        }
      }
      
      
      # # Apply Youden threshold optimization if requested
      # if(!is.null(parameters$optimize_threshold) && parameters$optimize_threshold && parameters$model != "nomodel"){
      #   tryCatch({
      #     # Calculate optimal threshold using Youden method from ROC curve on learning data
      #     classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
      #     scorelearning <- resmodel$datalearningmodel$reslearningmodel$scorelearning
      #     
      #     # Create ROC object
      #     roc_obj <- roc(classlearning, scorelearning, quiet=TRUE)
      #     
      #     # Find optimal threshold using Youden method (maximizes sensitivity + specificity - 1)
      #     optimal_coords <- coords(roc_obj, "best", best.method="youden", ret=c("threshold", "sensitivity", "specificity"))
      #     optimal_threshold <- optimal_coords$threshold
      #     
      #     # Recalculate predicted classes using optimal threshold for learning data
      #     #resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[2], levels(classlearning)[1])
      #     # IMPORTANT: In this application, levels(classlearning)[1] = "positif" (case)
      #     # Score represents probability of being positive, so high score → predict positive
      #     # Therefore: score >= threshold → levels[1] (positif), score < threshold → levels[2] (negatif)
      #     resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[1], levels(classlearning)[2])
      #     resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- factor(resmodel$datalearningmodel$reslearningmodel$predictclasslearning, levels = levels(classlearning))
      #     
      #     # If validation data exists, apply optimal threshold to validation predictions as well
      #     if(!is.null(validation)){
      #       classval <- resmodel$datavalidationmodel$resvalidationmodel$classval
      #       scoreval <- resmodel$datavalidationmodel$resvalidationmodel$scoreval
      #       # resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[2], levels(classval)[1])
      #       resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[1], levels(classval)[2])
      #       resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- factor(resmodel$datavalidationmodel$resvalidationmodel$predictclassval, levels = levels(classval))
      #     }
      #     
      #     # Update threshold in parameters for record
      #     parameters$thresholdmodel <- optimal_threshold
      #   }, error = function(e){
      #     # If threshold optimization fails, continue with original threshold
      #     cat(sprintf("    ✗ Youden optimization FAILED (iteration %d): %s\n", i, e$message))
      #     cat(sprintf("      → Keeping initial threshold: %.4f\n", parameters$thresholdmodel))
      #     warning(paste("Threshold optimization failed:", e$message))
      #   })
      # }
      
    }
    }
    else{parameters$model<-"nomodel"}
    #numberfeaturesselected
    # results[i,9]<-positive(dim(resselectdata$learningselect)[2]-1)
    #numberfeaturesdiff
    #numberfeaturesselected (shifted from 9 to 10)
    results[i,10]<-positive(dim(resselectdata$learningselect)[2]-1)
    #numberfeaturesdiff (shifted from 8 to 9)
    if(parameters$test!="notest"){
      results[i,8]<-positive(dim(restest$tabdiff)[2]-1)
    }
    #numberfeaturesmodel
    if(parameters$model!="nomodel"){
      # results[i,7]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      results[i,8]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      #thresholdused (NEW: index 7)
      results[i,7]<-round(parameters$thresholdmodel, digits = 4)
      #auclearning
      results[i,4]<-round(as.numeric(auc(roc(resmodel$datalearningmodel$reslearningmodel$classlearning,resmodel$datalearningmodel$reslearningmodel$scorelearning,quiet=T))),digits = 3)
      #sensibilitylearning
      results[i,5]<-sensibility(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      #specificitylearning
      results[i,6]<-specificity(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      if(!is.null(validation)){
      #aucvalidation
      results[i,1]<-round(as.numeric(auc(roc(resmodel$datavalidationmodel$resvalidationmodel$classval,resmodel$datavalidationmodel$resvalidationmodel$scoreval,quiet=T))),digits = 3)
      #sensibilityvalidation
      results[i,2]<-sensibility(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
      #specificityvalidation
      results[i,3]<-specificity(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
    }
    }
  }
  return(cbind(results,tabparameters))
}

##
importanceplot<-function(model,learningmodel,modeltype,graph=T){
  validate(need(!is.null(model),"No model"))
  validate(need(ncol(learningmodel)>2,"only one feature"))
  if(modeltype=="randomforest"){
    var_importance<- data.frame(variables=rownames(model$importance),
                                importance=as.vector(model$importance[,4]))

    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )

    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+coord_flip()+ylab("Variable Importance (Mean Decrease in Gini Index)")+
      theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of variables in the model")+scale_fill_grey()
  }
  if(modeltype=="svm"){
    importancevar<-importancemodelsvm(model = model,modeltype="svm",tabdiff=learningmodel,criterion = "fscore")

    var_importance<-as.data.frame(cbind(colnames(learningmodel),importancevar)[-1,])
    var_importance[,1]<-as.character(var_importance[,1])
    var_importance[,2]<-as.numeric(as.character(var_importance[,2]))
    colnames(var_importance)<-c("variables","importance")
    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )

    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+coord_flip()+ylab("Variable Importance (fscore)")+theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of variables in the model")+scale_fill_grey()
  }
  if(modeltype=="elasticnet"){
    # Extract coefficients from elasticnet model
    coef_matrix <- as.matrix(coef(model$glmnet_model, s=model$lambda))
    coef_values <- coef_matrix[-1, 1]  # Remove intercept
    names(coef_values) <- colnames(learningmodel)[-1]

    # Keep only non-zero coefficients
    nonzero_coefs <- coef_values[coef_values != 0]

    if(length(nonzero_coefs) > 0){
      var_importance <- data.frame(
        variables = names(nonzero_coefs),
        importance = abs(nonzero_coefs),
        stringsAsFactors = FALSE
      )

      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)

      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Absolute Coefficient)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No variables with non-zero coefficients")
    }
  }
  if(modeltype=="xgboost"){
    # Extract feature importance from XGBoost model
    importance_matrix <- xgb.importance(model = model)

    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )

      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)

      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Gain)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")
    }
  }
  if(modeltype=="lightgbm"){
    # Extract feature importance from LightGBM model
    importance_matrix <- lgb.importance(model = model)

 

    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )

      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
 

      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Gain)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+scale_fill_grey()

    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")

    }

  }
  if(modeltype=="naivebayes"){
    # Naive Bayes doesn't have traditional feature importance
    # We can compute conditional probabilities per class
    var_importance <- data.frame()
    g <- errorplot(text = "Naive Bayes: Feature importance not available\nModel uses probabilistic independence assumptions")

  }
  if(modeltype=="knn"){
    # KNN doesn't have traditional feature importance
    # Could compute based on feature scaling but not meaningful
    var_importance <- data.frame()
    g <- errorplot(text = "KNN: Feature importance not available\nModel uses distance-based classification")

  }
  if(!graph){return(var_importance)}
  if(graph){
    g
  }
}


positive<-function(x){
  if(x<0){x<-0}
  else{x}
  return(x)
}

#' #' GridSearchCV wrapper for Random Forest using superml
#' 
#' #' @param X Feature matrix (data.frame or matrix)
#' 
#' #' @param y Target vector
#' 
#' #' @param param_grid List of parameters to tune (ntree, mtry, nodesize, maxnodes)
#' 
#' #' @param n_folds Number of cross-validation folds
#' 
#' #' @param scoring Scoring metric(s)
#' 
#' #' @return List with best parameters and best score
#' 
#' tune_rf_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#'   
#'   library(superml)
#'   
#'   library(randomForest)
#'   
#'   
#'   
#'   # Default parameter grid if not provided
#'   
#'   if(is.null(param_grid)) {
#'     
#'     param_grid <- list(
#'       
#'       n_estimators = c(100, 500, 1000),  # ntree in randomForest
#'       
#'       max_depth = c(5, 10, 15, 20, NULL),  # maxnodes (NULL = unlimited)
#'       
#'       min_samples_split = c(2, 5, 10),  # nodesize
#'       
#'       max_features = c("sqrt", "log2", floor(ncol(X)/3), floor(ncol(X)/2))  # mtry
#'       
#'     )
#'     
#'   }
#'   
#'   
#'   
#'   # Create trainer object
#'   
#'   rf_trainer <- RFTrainer$new()
#'   
#'   
#'   
#'   # Create GridSearchCV object
#'   
#'   gst <- GridSearchCV$new(
#'     
#'     trainer = rf_trainer,
#'     
#'     parameters = param_grid,
#'     
#'     n_folds = n_folds,
#'     
#'     scoring = scoring
#'     
#'   )
#'   
#'   
#'   
#'   # Fit the grid search
#'   
#'   gst$fit(cbind(y = y, X), "y")
#'   
#'   
#'   
#'   # Get best iteration
#'   
#'   best_result <- gst$best_iteration(metric = scoring[1])
#'   
#'   
#'   
#'   return(list(
#'     
#'     best_params = best_result,
#'     
#'     grid_search = gst,
#'     
#'     best_score = best_result$score
#'     
#'   ))
#'   
#' }
#' 
#' 
#' 
#' #' GridSearchCV wrapper for XGBoost using superml
#' 
#' #' @param X Feature matrix (data.frame or matrix)
#' 
#' #' @param y Target vector
#' 
#' #' @param param_grid List of parameters to tune
#' 
#' #' @param n_folds Number of cross-validation folds
#' 
#' #' @param scoring Scoring metric(s)
#' 
#' #' @return List with best parameters and best score
#' 
#' tune_xgb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#'   
#'   library(superml)
#'   
#'   
#'   
#'   # Default parameter grid if not provided
#'   
#'   if(is.null(param_grid)) {
#'     
#'     param_grid <- list(
#'       
#'       n_estimators = c(50, 100, 200),  # nrounds
#'       
#'       max_depth = c(3, 6, 9, 12),
#'       
#'       learning_rate = c(0.01, 0.05, 0.1, 0.3),  # eta
#'       
#'       gamma = c(0, 0.1, 0.5),
#'       
#'       subsample = c(0.6, 0.8, 1.0),
#'       
#'       colsample_bytree = c(0.6, 0.8, 1.0),
#'       
#'       min_child_weight = c(1, 3, 5)
#'       
#'     )
#'     
#'   }
#'   
#'   
#'   
#'   # Create trainer object
#'   
#'   xgb_trainer <- XGBTrainer$new()
#'   
#'   
#'   
#'   # Create GridSearchCV object
#'   
#'   gst <- GridSearchCV$new(
#'     
#'     trainer = xgb_trainer,
#'     
#'     parameters = param_grid,
#'     
#'     n_folds = n_folds,
#'     
#'     scoring = scoring
#'     
#'   )
#'   
#'   
#'   
#'   # Fit the grid search
#'   
#'   gst$fit(cbind(y = y, X), "y")
#'   
#'   
#'   
#'   # Get best iteration
#'   
#'   best_result <- gst$best_iteration(metric = scoring[1])
#'   
#'   
#'   
#'   return(list(
#'     
#'     best_params = best_result,
#'     
#'     grid_search = gst,
#'     
#'     best_score = best_result$score
#'     
#'   ))
#'   
#' }
#' 
#' 
#' 
#' #' GridSearchCV wrapper for Naive Bayes using superml
#' #' @param X Feature matrix (data.frame or matrix)
#' #' @param y Target vector
#' #' @param param_grid List of parameters to tune
#' #' @param n_folds Number of cross-validation folds
#' #' @param scoring Scoring metric(s)
#' #' @return List with best parameters and best score
#' 
#' tune_nb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#'   
#'   library(superml)
#'   # Default parameter grid if not provided
#'   
#'   if(is.null(param_grid)) {
#'     
#'     param_grid <- list(
#'       
#'       laplace = c(0, 0.5, 1, 2, 5)  # Smoothing parameter
#'       
#'     )
#'   }
#'   # Create trainer object
#'   nb_trainer <- NBTrainer$new()
#' 
#'   # Create GridSearchCV object
#'   gst <- GridSearchCV$new(
#'     trainer = nb_trainer,
#'     parameters = param_grid,
#'     n_folds = n_folds,
#'     scoring = scoring
#'   )
#'   # Fit the grid search
#'   
#'   gst$fit(cbind(y = y, X), "y")
#'   
#'   # Get best iteration
#'   
#'   best_result <- gst$best_iteration(metric = scoring[1])
#'   
#'   return(list(
#'     best_params = best_result,
#'     grid_search = gst,
#'     best_score = best_result$score
#'   ))
#'   
#' }
#' 
#' 
#' 
#' #' GridSearchCV wrapper for KNN using superml
#' #' @param X Feature matrix (data.frame or matrix)
#' #' @param y Target vector
#' #' @param param_grid List of parameters to tune
#' #' @param n_folds Number of cross-validation folds
#' #' @param scoring Scoring metric(s)
#' #' @return List with best parameters and best score
#' tune_knn_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#'   library(superml)
#'   
#'   # Default parameter grid if not provided
#'   if(is.null(param_grid)) {
#'     max_k <- min(floor(sqrt(length(y))), 30)
#'     param_grid <- list(
#'       n_neighbors = seq(3, max_k, by = 2),  # k parameter, odd numbers only
#'       weights = c("uniform", "distance"),
#'       algorithm = c("brute", "kd_tree")
#'     )
#'   }
#'   
#'   # Create trainer object
#'   knn_trainer <- KNNTrainer$new()
#'   
#'   # Create GridSearchCV object
#'   gst <- GridSearchCV$new(
#'     trainer = knn_trainer,
#'     parameters = param_grid,
#'     n_folds = n_folds,
#'     scoring = scoring
#'     
#'   )
#'   
#'   
#'   # Fit the grid search
#'   gst$fit(cbind(y = y, X), "y")
#'   
#'   # Get best iteration
#'   best_result <- gst$best_iteration(metric = scoring[1])
#'   
#'   return(list(
#'     best_params = best_result,
#'     grid_search = gst,
#'     best_score = best_result$score
#'     
#'   ))
#'   
#' }
#' 
#' 
#' 
#' #' GridSearchCV wrapper for Logistic Regression (ElasticNet) using superml
#' #' @param X Feature matrix (data.frame or matrix)
#' #' @param y Target vector
#' #' @param param_grid List of parameters to tune
#' #' @param n_folds Number of cross-validation folds
#' #' @param scoring Scoring metric(s)
#' #' @return List with best parameters and best score
#' 
#' tune_elasticnet_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#'   
#'   library(superml)
#'   # Default parameter grid if not provided
#'   
#'   if(is.null(param_grid)) {
#'     param_grid <- list(
#'       alpha = c(0, 0.25, 0.5, 0.75, 1.0),  # 0=Ridge, 1=Lasso, 0.5=ElasticNet
#'       lambda = c(0.001, 0.01, 0.1, 1.0, 10),
#'       penalty = c("elasticnet")
#'     )
#'   }
#'   
#'   # Create trainer object
#'   lm_trainer <- LMTrainer$new()
#'   
#'   # Create GridSearchCV object
#'   gst <- GridSearchCV$new(
#'     trainer = lm_trainer,
#'     parameters = param_grid,
#'     n_folds = n_folds,
#'     scoring = scoring
#'   )
#'   
#'   # Fit the grid search
#'   gst$fit(cbind(y = y, X), "y")
#' 
#'   # Get best iteration
#'   best_result <- gst$best_iteration(metric = scoring[1])
#' 
#'   return(list(
#'     best_params = best_result,
#'     grid_search = gst,
#'     best_score = best_result$score
#'   ))
#'   
#' }
