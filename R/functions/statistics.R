# Custom statistics functions

# main treatment model — continuous outcomes (OLS)
run_main_treatment_model <- function(data,
                                     outcome,
                                     condition_var = "condition",
                                     covariates    = NULL,
                                     weights       = NULL,
                                     adjust_method = "BH") {
  
  # Formula
  rhs           <- paste(c(condition_var, covariates), collapse = " + ")
  model_formula <- as.formula(paste(outcome, "~", rhs))
  
  # Baseline (control) level
  baseline <- levels(data[[condition_var]])[1]
  
  # Fit
  fit <- lm(
    model_formula,
    data    = data,
    weights = if (!is.null(weights)) data[[weights]] else NULL
  )
  
  # Robust VCOV (HC2)
  vcov_robust <- sandwich::vcovHC(fit, type = "HC2")
  
  results <- lmtest::coeftest(fit, vcov = vcov_robust) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, paste0("^", condition_var))) |>
    mutate(
      outcome          = outcome,
      condition        = str_remove(term, condition_var),
      baseline         = baseline,
      p.value_adjusted = p.adjust(p.value, method = adjust_method), 
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    select(-term)
  
  return(results)
}

# treatment model — binary outcomes (logistic regression)
run_main_treatment_model_binary <- function(data,
                                            outcome,
                                            condition_var = "condition",
                                            covariates    = NULL,
                                            weights       = NULL,
                                            adjust_method = "BH") {
  
  rhs           <- paste(c(condition_var, covariates), collapse = " + ")
  model_formula <- as.formula(paste(outcome, "~", rhs))
  baseline      <- levels(data[[condition_var]])[1]
  
  fit <- glm(
    model_formula,
    data    = data,
    family  = binomial(link = "logit"),
    weights = if (!is.null(weights)) data[[weights]] else NULL
  )
  
  vcov_robust <- sandwich::vcovHC(fit, type = "HC2")
  
  # Log-odds results — for inference
  log_odds <- lmtest::coeftest(fit, vcov = vcov_robust) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, paste0("^", condition_var))) |>
    mutate(
      outcome              = outcome,
      condition            = str_remove(term, condition_var),
      baseline             = baseline,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      ),
      odds_ratio   = exp(estimate),
      or_conf.low  = exp(conf.low),
      or_conf.high = exp(conf.high)
    ) |>
    select(-term)
  
  # Marginal effects — for interpretation (probability scale)
  marginal_effects <- marginaleffects::avg_comparisons(
    fit,
    variables = condition_var,
    vcov      = vcov_robust,
    newdata = data |> select(all_of(c(outcome, condition_var, covariates)))
  ) |>
    as_tibble() |>
    select(contrast, estimate, conf.low, conf.high, p.value) |>
    mutate(
      condition            = str_extract(contrast, "intervention_\\d+"),
      outcome              = outcome,
      baseline             = baseline,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    )
  
  list(
    log_odds         = log_odds,
    marginal_effects = marginal_effects
  )
}


# main moderator model — continuous outcomes (OLS)
run_moderator_model <- function(data,
                                outcome,
                                moderator,
                                condition_var = "condition",
                                covariates    = NULL,
                                weights       = NULL,
                                adjust_method = "BH") {
  
  rhs           <- paste(c(paste0(condition_var, " * ", moderator), covariates),
                         collapse = " + ")
  model_formula <- as.formula(paste(outcome, "~", rhs))
  baseline      <- levels(data[[condition_var]])[1]
  
  fit <- lm(
    model_formula,
    data    = data,
    weights = if (!is.null(weights)) data[[weights]] else NULL
  )
  
  vcov_robust <- sandwich::vcovHC(fit, type = "HC2")
  
  interaction_effects <- lmtest::coeftest(fit, vcov = vcov_robust) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, ":")) |>
    mutate(
      baseline             = baseline,
      condition            = str_extract(term, "intervention_\\d+"),
      moderator_level      = str_remove(str_extract(term, "(?<=:).+"), moderator),
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    )
  
  is_numeric_mod <- is.numeric(data[[moderator]])
  
  if (!is_numeric_mod) {
    predicted_effects <- marginaleffects::avg_comparisons(
      fit,
      variables = condition_var,
      by        = moderator,
      vcov      = vcov_robust,
      newdata   = "mean"
    ) |>
      as_tibble() |>
      mutate(
        condition            = str_extract(contrast, "intervention_\\d+"),
        moderator_level      = .data[[moderator]],
        baseline             = baseline,
        p.value_adjusted     = p.adjust(p.value, method = adjust_method),
        significant_adjusted = case_when(
          p.value_adjusted < .001 ~ "***",
          p.value_adjusted < .01  ~ "**",
          p.value_adjusted < .05  ~ "*",
          TRUE                    ~ NA_character_
        )
      ) |>
      filter(!is.na(condition)) |>
      select(condition, moderator_level, estimate, conf.low, conf.high,
             p.value, p.value_adjusted, significant_adjusted, baseline)
  }
  
  if (is_numeric_mod) {
    predicted_effects <- marginaleffects::comparisons(
      fit,
      variables = condition_var,
      vcov      = vcov_robust,
      newdata   = do.call(
        marginaleffects::datagrid,
        c(list(model = fit),
          setNames(list(fivenum(data[[moderator]])), moderator))
      )
    ) |>
      as_tibble() |>
      mutate(
        condition            = str_extract(contrast, "intervention_\\d+"),
        moderator_value      = .data[[moderator]],
        baseline             = baseline,
        p.value_adjusted     = p.adjust(p.value, method = adjust_method),
        significant_adjusted = case_when(
          p.value_adjusted < .001 ~ "***",
          p.value_adjusted < .01  ~ "**",
          p.value_adjusted < .05  ~ "*",
          TRUE                    ~ NA_character_
        )
      ) |>
      filter(!is.na(condition)) |>
      select(condition, moderator_value, estimate, conf.low, conf.high,
             p.value, p.value_adjusted, significant_adjusted, baseline)
  }
  
  list(
    interaction_effects = interaction_effects,
    predicted_effects   = predicted_effects
  )
}

# moderator model — binary outcomes (logistic regression)
run_moderator_model_binary <- function(data,
                                       outcome,
                                       moderator,
                                       condition_var = "condition",
                                       covariates    = NULL,
                                       weights       = NULL,
                                       adjust_method = "BH") {
  
  rhs           <- paste(c(paste0(condition_var, " * ", moderator), covariates),
                         collapse = " + ")
  model_formula <- as.formula(paste(outcome, "~", rhs))
  baseline      <- levels(data[[condition_var]])[1]
  
  fit <- glm(
    model_formula,
    data    = data,
    family  = binomial(link = "logit"),
    weights = if (!is.null(weights)) data[[weights]] else NULL
  )
  
  vcov_robust    <- sandwich::vcovHC(fit, type = "HC2")
  is_numeric_mod <- is.numeric(data[[moderator]])
  
  interaction_effects <- lmtest::coeftest(fit, vcov = vcov_robust) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, ":")) |>
    mutate(
      baseline             = baseline,
      condition            = str_extract(term, "intervention_\\d+"),
      moderator_level      = str_remove(str_extract(term, "(?<=:).+"), moderator),
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      ),
      odds_ratio   = exp(estimate),
      or_conf.low  = exp(conf.low),
      or_conf.high = exp(conf.high)
    ) |>
    select(-term)
  
  if (!is_numeric_mod) {
    predicted_effects <- marginaleffects::avg_comparisons(
      fit,
      variables = condition_var,
      by        = moderator,
      vcov      = vcov_robust,
      newdata   = "mean"
    ) |>
      as_tibble() |>
      mutate(
        condition       = str_extract(contrast, "intervention_\\d+"),
        moderator_level = .data[[moderator]],
        baseline        = baseline
      ) |>
      filter(!is.na(condition)) |>
      select(condition, moderator_level, estimate, conf.low, conf.high,
             p.value, baseline)
  }
  
  if (is_numeric_mod) {
    predicted_effects <- marginaleffects::comparisons(
      fit,
      variables = condition_var,
      vcov      = vcov_robust,
      newdata   = do.call(
        marginaleffects::datagrid,
        c(list(model = fit),
          setNames(list(fivenum(data[[moderator]])), moderator))
      )
    ) |>
      as_tibble() |>
      mutate(
        condition       = str_extract(contrast, "intervention_\\d+"),
        moderator_value = .data[[moderator]],
        baseline        = baseline
      ) |>
      filter(!is.na(condition)) |>
      select(condition, moderator_value, estimate, conf.low, conf.high,
             p.value, baseline)
  }
  
  list(
    interaction_effects = interaction_effects,
    predicted_effects   = predicted_effects
  )
}

# run basic f-test for differential attrition, with condition as only predictor
run_attrition_f_test <- function(data, 
                                 outcome, 
                                 condition_var = "condition") {
  
  # Completion indicator for the specific outcome
  model_data  <- data %>%
    mutate(
      completed = if_else(
        is.na(.data[[outcome]]),
        FALSE,
        TRUE,
      ), 
      completed_numeric = as.numeric(completed)
    )  
  
  # skip test if no variation (i.e. if everyone completed/attrited)
  if(length(unique(model_data$completed)) < 2){
    return(tibble(outcome = outcome, Chi2 = NA_real_, p_value = NA_real_))
  }
  
  formula <- as.formula(paste("completed ~", condition_var))
  
  model <- lm(formula, data = model_data)
  
  # Only the coefficients for the condition variable (not the intercept)
  test_terms <- grep(condition_var, names(model$coefficients), value = TRUE)
  
  f_test <- car::linearHypothesis(model, test_terms, white.adjust = "hc2")
  
  tibble(
    outcome = outcome,
    F_statistic = f_test$F[2],
    p_value = f_test$`Pr(>F)`[2]
  )
}

# run f-test for heterogeneous attrition, for a single outcome, across multiple 
# covariates
run_attrition_interactions <- function(data, 
                                       outcome, 
                                       condition_var = "condition", 
                                       covariates) {
  
  # Completion indicator
  model_data  <- data %>%
    mutate(
      completed = if_else(
        is.na(.data[[outcome]]),
        FALSE,
        TRUE,
      ), 
      completed_numeric = as.numeric(completed)
    )  
  
  # Skip if no variation
  if(length(unique(model_data$completed)) < 2){
    return(tibble(outcome = outcome, 
                  covariate = covariates, 
                  F_statistic = NA_real_, 
                  p_value = NA_real_))
  }
  
  # Loop over covariates
  interaction_tests <- covariates %>%
    map_df(function(cov) {
      
      # Build formula for condition * covariate
      formula <- as.formula(paste0("completed ~ ", condition_var, " * ", cov))
      model <- lm(formula, data = model_data)
      
      # Identify interaction terms (condition:covariate)
      interaction_terms <- grep(":", names(coef(model)), value = TRUE)
      
      # Skip if no interaction terms
      if(length(interaction_terms) == 0){
        return(tibble(outcome = outcome, 
                      covariate = cov, 
                      F_statistic = NA_real_, 
                      p_value = NA_real_))
      }
      
      # Joint F-test with robust SE
      f_test <- car::linearHypothesis(model, 
                                      interaction_terms, 
                                      white.adjust = "hc1")
      
      tibble(
        outcome = outcome,
        covariate = cov,
        F_statistic = f_test$F[2],
        p_value = f_test$`Pr(>F)`[2]
      )
      
    }) |> 
    # adjust for multiple comparison
    mutate(adjusted_p.value = p.adjust(p_value, method = "BH"))
  
  return(interaction_tests)
}

# Function to calculate inverse-probability weights using a random forest
get_ipw_weights_rf <- function(data,
                               outcome,
                               condition_var = "condition",
                               weight_predictors,
                               ntree = 200) {
  
  # Completion indicator
  dat <- data |>
    mutate(
      completed = factor(
        !is.na(.data[[outcome]]),
        levels = c(FALSE, TRUE),
        labels = c("no", "yes")
      )
    )
  
  # Build formula explicitly
  predictors <- c(condition_var, weight_predictors)
  rf_formula <- as.formula(
    paste("completed ~", paste(predictors, collapse = " + "))
  )
  
  # Fit random forest
  rf_model <- randomForest::randomForest(
    formula  = rf_formula,
    data     = dat,
    importance = TRUE,
    ntree    = ntree,
    na.action = na.exclude   # safety net for any remaining NAs
  )
  
  # Predicted probability of completion
  p_complete <- predict(rf_model, newdata = dat, type = "prob")[, "yes"]
  
  # Inverse probability weights + trimming at 99th percentile
  dat <- dat |>
    mutate(
      p_complete  = p_complete,
      ipw         = 1 / p_complete,
      ipw_trimmed = pmin(ipw, quantile(ipw, 0.99))
    )
  
  return(dat)
}

# function to run persistence models 
run_persistence_model <- function(data,
                                  outcome,
                                  condition_var = "condition",
                                  covariates    = NULL,
                                  weights       = NULL,
                                  id_var        = "id",
                                  time_var      = "time",
                                  adjust_method = "BH") {
  
  # Formula: condition × time interaction
  rhs           <- paste(c(paste0(condition_var, " * ", time_var), 
                           covariates),
                         collapse = " + ")
  model_formula <- as.formula(paste(outcome, "~", rhs))
  
  # Baseline (control) level
  baseline <- levels(data[[condition_var]])[1]
  
  # Fit
  fit <- lm(
    model_formula,
    data    = data,
    weights = if (!is.null(weights)) data[[weights]] else NULL
  )
  
  # Cluster-robust VCOV at participant level
  vcov_clustered <- sandwich::vcovCL(fit, 
                                     cluster = as.formula(paste0("~", id_var)
                                     )
  )
  
  # Interaction terms (condition × time)
  interaction_effects <- lmtest::coeftest(fit, vcov = vcov_clustered) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, ":")) |>
    mutate(
      baseline             = baseline,
      outcome              = outcome,
      condition            = str_extract(term, "intervention_\\d+"),
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    )
  
  # Predicted effects within each wave
  predicted_effects <- marginaleffects::avg_comparisons(
    fit,
    variables = condition_var,
    by        = time_var,
    vcov      = vcov_clustered,
    newdata   = "mean"
  ) |>
    as_tibble() |>
    mutate(
      condition            = str_extract(contrast, "intervention_\\d+"),
      baseline             = baseline,
      outcome              = outcome,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    filter(!is.na(condition)) |>
    select(condition, !!time_var := .data[[time_var]], 
           estimate, conf.low, conf.high,
           p.value, p.value_adjusted, significant_adjusted, 
           baseline, outcome)
  
  list(
    interaction_effects = interaction_effects,
    predicted_effects   = predicted_effects
  )
}

run_persistence_model_binary <- function(data,
                                         outcome,
                                         condition_var = "condition",
                                         covariates    = NULL,
                                         weights       = NULL,
                                         id_var        = "id",
                                         time_var      = "time",
                                         adjust_method = "BH") {
  
  # Formula: condition × time interaction
  rhs           <- paste(c(paste0(condition_var, " * ", time_var), covariates),
                         collapse = " + ")
  model_formula <- as.formula(paste(outcome, "~", rhs))
  
  # Baseline (control) level
  baseline <- levels(data[[condition_var]])[1]
  
  # Fit
  fit <- glm(
    model_formula,
    data    = data,
    family  = binomial(link = "logit"),
    weights = if (!is.null(weights)) data[[weights]] else NULL
  )
  
  # Cluster-robust VCOV at participant level
  vcov_clustered <- sandwich::vcovCL(fit, 
                                     cluster = as.formula(paste0("~", id_var)
                                     )
  )
  
  # Interaction terms (condition × time) — log-odds scale
  interaction_effects <- lmtest::coeftest(fit, vcov = vcov_clustered) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, ":")) |>
    mutate(
      baseline             = baseline,
      outcome              = outcome,
      condition            = str_extract(term, "intervention_\\d+"),
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      ),
      odds_ratio   = exp(estimate),
      or_conf.low  = exp(conf.low),
      or_conf.high = exp(conf.high)
    ) |>
    select(-term)
  
  # Predicted effects within each wave — probability scale
  predicted_effects <- marginaleffects::avg_comparisons(
    fit,
    variables = condition_var,
    by        = time_var,
    vcov      = vcov_clustered,
    newdata   = "mean"
  ) |>
    as_tibble() |>
    mutate(
      condition            = str_extract(contrast, "intervention_\\d+"),
      baseline             = baseline,
      outcome              = outcome,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    filter(!is.na(condition)) |>
    select(condition, !!time_var := .data[[time_var]], 
           estimate, conf.low, conf.high,
           p.value, p.value_adjusted, significant_adjusted, 
           baseline, outcome)
  
  list(
    interaction_effects = interaction_effects,
    predicted_effects   = predicted_effects
  )
}

# functio to run trust dimensions model 
run_trust_dimensions_model <- function(data,
                                       dimensions,
                                       condition_var  = "condition",
                                       id_var         = "id",
                                       covariates     = NULL,
                                       adjust_method  = "BH") {
  
  # reshape to long format
  long_data <- data |>
    select(all_of(c(id_var, condition_var, covariates, dimensions))) |>
    pivot_longer(
      cols      = all_of(dimensions),
      names_to  = "dimension",
      values_to = "value"
    ) |>
    mutate(dimension = factor(dimension, levels = dimensions))
  
  # baseline
  baseline     <- levels(data[[condition_var]])[1]
  baseline_dim <- levels(long_data$dimension)[1]
  
  # formula with interaction
  rhs           <- paste(c(paste0(condition_var, " * dimension"),
                           covariates), collapse = " + ")
  model_formula <- as.formula(paste("value ~", rhs))
  
  # fit OLS
  fit <- lm(model_formula, data = long_data)
  
  # clustered SEs by participant
  vcov_clustered <- sandwich::vcovCL(fit, 
                                     cluster = as.formula(paste0("~", id_var)))
  
  # --- Interaction terms ---
  interaction_effects <- lmtest::coeftest(fit, vcov = vcov_clustered) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, ":")) |>
    mutate(
      condition            = str_extract(term, "intervention_\\d+"),
      dimension            = str_remove(str_extract(term, "(?<=:).+"), 
                                        "dimension"),
      baseline_dimension   = baseline_dim,
      baseline             = baseline,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    select(-term)
  
  # --- Predicted effects per condition × dimension ---
  predicted_effects <- marginaleffects::avg_comparisons(
    fit,
    variables = condition_var,
    by        = "dimension",
    vcov      = vcov_clustered,
    newdata   = "mean"
  ) |>
    as_tibble() |>
    mutate(
      condition            = str_extract(contrast, "intervention_\\d+"),
      baseline             = baseline,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    filter(!is.na(condition)) |>
    select(condition, dimension, estimate, conf.low, conf.high,
           p.value, p.value_adjusted, significant_adjusted, baseline)
  
  list(
    interaction_effects = interaction_effects,
    predicted_effects   = predicted_effects
  )
}

run_items_model <- function(data,
                            items,
                            outcome_name,
                            condition_var = "condition",
                            id_var        = "id",
                            covariates    = NULL,
                            adjust_method = "BH") {
  
  # reshape to long format
  long_data <- data |>
    select(all_of(c(id_var, condition_var, covariates, items))) |>
    pivot_longer(
      cols      = all_of(items),
      names_to  = "item",
      values_to = "value"
    ) |>
    mutate(item = factor(item, levels = items))
  
  # baseline
  baseline     <- levels(data[[condition_var]])[1]
  baseline_item <- levels(long_data$item)[1]
  
  # formula with interaction
  rhs           <- paste(c(paste0(condition_var, " * item"), covariates),
                         collapse = " + ")
  model_formula <- as.formula(paste("value ~", rhs))
  
  # fit OLS
  fit <- lm(model_formula, data = long_data)
  
  # clustered SEs by participant
  vcov_clustered <- sandwich::vcovCL(fit, cluster = as.formula(paste0("~", id_var)))
  
  # --- Interaction terms ---
  interaction_effects <- lmtest::coeftest(fit, vcov = vcov_clustered) |>
    broom::tidy(conf.int = TRUE) |>
    filter(str_detect(term, ":")) |>
    mutate(
      condition            = str_extract(term, "intervention_\\d+"),
      item                 = str_remove(str_extract(term, "(?<=:).+"), "item"),
      baseline_item        = baseline_item,
      baseline             = baseline,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    select(-term)
  
  # --- Predicted effects per condition × item ---
  predicted_effects <- marginaleffects::avg_comparisons(
    fit,
    variables = condition_var,
    by        = "item",
    vcov      = vcov_clustered,
    newdata   = "mean"
  ) |>
    as_tibble() |>
    mutate(
      condition            = str_extract(contrast, "intervention_\\d+"),
      baseline             = baseline,
      outcome              = outcome_name,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    filter(!is.na(condition)) |>
    select(condition, item, estimate, conf.low, conf.high,
           p.value, p.value_adjusted, significant_adjusted, baseline, outcome)
  
  list(
    interaction_effects = interaction_effects,
    predicted_effects   = predicted_effects
  )
}

