# Custom statistics functions

# compare treatment effects
compare_estimates <- function(h_result, l_result,
                              join_by = "condition",
                              tost_delta_pct = 0.5) {

  # Standardize columns; SE derived from std.error or from 95% CI width if absent
  prep <- function(df, suffix) {
    df |>
      select(all_of(join_by), estimate) |>
      mutate(
        se    = if ("std.error" %in% names(df)) df$std.error
                else if (all(c("conf.low", "conf.high") %in% names(df)))
                  (df$conf.high - df$conf.low) / (2 * 1.96)
                else NA_real_,
        p_adj = if ("p.value_adjusted" %in% names(df)) df$p.value_adjusted
                else NA_real_
      ) |>
      rename_with(~ paste0(.x, "_", suffix), c(estimate, se, p_adj))
  }

  joined <- inner_join(prep(h_result, "h"), prep(l_result, "l"), by = join_by)

  # Three-level inferential category based on BH-adjusted p and direction
  infer_cat <- function(est, p_adj) {
    case_when(
      p_adj < 0.05 & est > 0 ~ "sig_positive",
      p_adj < 0.05 & est < 0 ~ "sig_negative",
      TRUE                   ~ "not_significant"
    )
  }

  # TOST: equivalence declared when |ATE_h - ATE_l| < delta = tost_delta_pct × |ATE_h|
  # p_tost is the maximum of the two one-sided p-values; equivalent if p_tost < 0.05
  joined |>
    mutate(
      delta      = tost_delta_pct * abs(estimate_h),
      diff       = estimate_h - estimate_l,
      se_diff    = sqrt(se_h^2 + se_l^2),
      p_lower    = pnorm((diff + delta) / se_diff, lower.tail = FALSE),
      p_upper    = pnorm((diff - delta) / se_diff),
      p_tost     = pmax(p_lower, p_upper),
      equivalent = p_tost < 0.05,
      same_infer = infer_cat(estimate_h, p_adj_h) == infer_cat(estimate_l, p_adj_l)
    ) |>
    summarise(
      spearman_rho    = cor(estimate_h, estimate_l, method = "spearman",
                            use = "pairwise.complete.obs"),
      pearson_r       = cor(estimate_h, estimate_l, use = "pairwise.complete.obs"),
      rmse            = sqrt(mean((estimate_h - estimate_l)^2, na.rm = TRUE)),
      directional_pct = mean(sign(estimate_h) == sign(estimate_l), na.rm = TRUE) * 100,
      inferential_pct = if (!anyNA(p_adj_h) && !anyNA(p_adj_l))
        mean(same_infer, na.rm = TRUE) * 100 else NA_real_,
      tost_pct        = mean(equivalent, na.rm = TRUE) * 100
    )
}

# compare distribution overlap
compute_ovl <- function(x, y, n_grid = 512) {
  lo <- min(c(x, y)); hi <- max(c(x, y))
  if (lo == hi) return(NA_real_)
  # Evaluate both KDEs on the same grid, then integrate the overlap area
  d_h <- density(x, from = lo, to = hi, n = n_grid)
  d_l <- density(y, from = lo, to = hi, n = n_grid)
  sum(pmin(d_h$y, d_l$y)) * (hi - lo) / n_grid
}

# calibration regression
run_calibration <- function(h_result, l_result) {
  joined <- inner_join(
    h_result |> select(condition, estimate_h = estimate),
    l_result |> select(condition, estimate_l = estimate),
    by = "condition"
  )

  # ATE_h = α + β × ATE_l
  fit   <- lm(estimate_h ~ estimate_l, data = joined)
  coefs <- broom::tidy(fit, conf.int = TRUE)

  tibble(
    alpha     = coefs$estimate[1],
    alpha_lo  = coefs$conf.low[1],
    alpha_hi  = coefs$conf.high[1],
    beta      = coefs$estimate[2],
    beta_lo   = coefs$conf.low[2],
    beta_hi   = coefs$conf.high[2],
    r_squared = broom::glance(fit)$r.squared,
    # Joint F-test  H0: α = 0 AND β = 1  (perfect calibration on the original scale)
    p_joint   = car::linearHypothesis(
                  fit, c("(Intercept) = 0", "estimate_l = 1")
                )$`Pr(>F)`[2],
    # Slope-only F-test  H0: β = 1  (proportional calibration; constant offset allowed)
    p_beta_1  = car::linearHypothesis(fit, "estimate_l = 1")$`Pr(>F)`[2]
  )
}

compare_distributions <- function(human_data, llm_data, outcome, condition_val) {
  x <- human_data |> filter(condition == condition_val) |> pull(all_of(outcome)) |> na.omit()
  y <- llm_data   |> filter(condition == condition_val) |> pull(all_of(outcome)) |> na.omit()
  tibble(
    condition      = as.character(condition_val),
    ovl            = compute_ovl(x, y),
    ks_d           = suppressWarnings(ks.test(x, y)$statistic),
    variance_ratio = var(y) / var(x)  # > 1: clones more variable; < 1: less variable
  )
}


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
      condition            = str_remove(contrast, " - .+$"),
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
      condition            = str_extract(term, paste0("(?<=", condition_var, ")[^:]+")),
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
        condition            = str_remove(contrast, " - .+$"),
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
        condition            = str_remove(contrast, " - .+$"),
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
      condition            = str_extract(term, paste0("(?<=", condition_var, ")[^:]+")),
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
        condition       = str_remove(contrast, " - .+$"),
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
        condition       = str_remove(contrast, " - .+$"),
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
      condition            = str_extract(term, paste0("(?<=", condition_var, ")[^:]+")),
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
      condition            = str_remove(contrast, " - .+$"),
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
      condition            = str_extract(term, paste0("(?<=", condition_var, ")[^:]+")),
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
      condition            = str_remove(contrast, " - .+$"),
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
      condition            = str_extract(term, paste0("(?<=", condition_var, ")[^:]+")),
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
      condition            = str_remove(contrast, " - .+$"),
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
      condition            = str_extract(term, paste0("(?<=", condition_var, ")[^:]+")),
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
      condition            = str_remove(contrast, " - .+$"),
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

# Stack human and LLM data; test ATE discrepancy via condition × source interaction.
# The interaction estimate = ATE_human − ATE_clone for each intervention.
run_stacked_model <- function(human_data, llm_data,
                               outcome,
                               condition_var = "condition",
                               adjust_method = "BH") {

  stacked <- bind_rows(
    human_data |> mutate(source = 0L),
    llm_data   |> mutate(source = 1L)
  ) |>
    mutate(source = factor(source, labels = c("human", "llm")))

  baseline      <- levels(stacked[[condition_var]])[1]
  model_formula <- as.formula(paste(outcome, "~", condition_var, "* source"))
  fit           <- lm(model_formula, data = stacked)
  vcov_robust   <- sandwich::vcovHC(fit, type = "HC2")

  lmtest::coeftest(fit, vcov = vcov_robust) |>
    broom::tidy(conf.int = TRUE) |>
    # keep only condition:source interaction terms (one per intervention)
    filter(str_detect(term, ":") & str_detect(term, "source")) |>
    mutate(
      outcome              = outcome,
      condition            = str_extract(term, paste0("(?<=", condition_var, ")[^:]+")),
      baseline             = baseline,
      p.value_adjusted     = p.adjust(p.value, method = adjust_method),
      significant_adjusted = case_when(
        p.value_adjusted < .001 ~ "***",
        p.value_adjusted < .01  ~ "**",
        p.value_adjusted < .05  ~ "*",
        TRUE                    ~ NA_character_
      )
    ) |>
    select(condition, outcome, estimate, std.error, conf.low, conf.high,
           p.value, p.value_adjusted, significant_adjusted, baseline)
}

# Compare cell-mean outcomes in the control condition across demographic groups.
# r and RMSE measure how well clone baseline levels match human baseline levels.
compare_demographic_baselines <- function(human_data, llm_data,
                                           outcome,
                                           moderators,
                                           condition_var = "condition") {
  control_val <- levels(human_data[[condition_var]])[1]

  map_dfr(moderators, function(mod) {
    h_cells <- human_data |>
      filter(.data[[condition_var]] == control_val) |>
      group_by(cell = .data[[mod]]) |>
      summarise(mean_h = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")

    l_cells <- llm_data |>
      filter(.data[[condition_var]] == control_val) |>
      group_by(cell = .data[[mod]]) |>
      summarise(mean_l = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")

    inner_join(h_cells, l_cells, by = "cell") |>
      summarise(
        moderator = mod,
        r         = cor(mean_h, mean_l, use = "pairwise.complete.obs"),
        rmse      = sqrt(mean((mean_h - mean_l)^2, na.rm = TRUE)),
        n_cells   = n()
      )
  })
}

# Regress outcome on each demographic moderator + condition FE separately.
# Clone R² >> human R² per moderator signals over-reliance on demographic stereotypes.
compare_demographic_predictability <- function(human_data, llm_data,
                                                outcome,
                                                predictors,
                                                condition_var = "condition") {
  results <- map(predictors, function(mod) {
    formula <- as.formula(paste(outcome, "~", mod, "+", condition_var))
    fit_h   <- lm(formula, data = human_data)
    fit_l   <- lm(formula, data = llm_data)

    rsq <- tibble(
      moderator = mod,
      source    = c("human", "llm"),
      r_squared = c(broom::glance(fit_h)$r.squared, broom::glance(fit_l)$r.squared)
    )

    coefs_h <- broom::tidy(fit_h, conf.int = TRUE) |>
      filter(str_detect(term, fixed(mod))) |>
      select(term, est_h = estimate, lo_h = conf.low, hi_h = conf.high)

    coefs_l <- broom::tidy(fit_l, conf.int = TRUE) |>
      filter(str_detect(term, fixed(mod))) |>
      select(term, est_l = estimate, lo_l = conf.low, hi_l = conf.high)

    coefs <- inner_join(coefs_h, coefs_l, by = "term") |>
      mutate(moderator = mod)

    list(r_squared = rsq, coefficients = coefs)
  })

  list(
    r_squared    = map_dfr(results, "r_squared"),
    coefficients = map_dfr(results, "coefficients")
  )
}
