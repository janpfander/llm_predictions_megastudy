# Custom statistics functions

# compare treatment effects (v1 only — the benchmark preregistration does not
# use this function; its metrics live in pooled_metrics() below)
# tost_delta (absolute, in outcome units) overrides the v1 relative bound
# when supplied; the original preregistration keeps the relative default.
compare_estimates <- function(h_result, l_result,
                              join_by = "condition",
                              tost_delta_pct = 0.5,
                              tost_delta = NULL) {

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

  # TOST: equivalence declared when |ATE_h - ATE_l| < delta
  # (fixed tost_delta if supplied, else tost_delta_pct × |ATE_h|);
  # p_tost is the maximum of the two one-sided p-values; equivalent if p_tost < 0.05
  joined |>
    mutate(
      delta      = if (!is.null(tost_delta)) tost_delta
                   else tost_delta_pct * abs(estimate_h),
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

# Wasserstein-1 (earth mover's) distance between two samples: the integral of
# the absolute ECDF difference over the observed range, in outcome scale
# points. Bandwidth-free companion to the KDE-based OVL (whose value depends
# on kernel bandwidth, and hence on the two samples' sizes, and leaks density
# past the 0/100 scale bounds).
compute_w1 <- function(x, y, n_grid = 512) {
  lo <- min(c(x, y)); hi <- max(c(x, y))
  if (lo == hi) return(0)
  gr <- seq(lo, hi, length.out = n_grid)
  mean(abs(ecdf(x)(gr) - ecdf(y)(gr))) * (hi - lo)
}

# calibration regression
# robust = TRUE swaps classical for HC2 heteroskedasticity-robust inference:
# the outcome side (ATE_h) carries known, condition-varying sampling error
# that classical F-tests ignore. weight_by_precision = TRUE additionally
# weights by 1/se_h^2 (needs a std.error column in h_result); unused by both
# preregistrations. Defaults reproduce the original preregistration exactly.
run_calibration <- function(h_result, l_result,
                            robust = FALSE, weight_by_precision = FALSE) {
  joined <- inner_join(
    h_result |>
      mutate(se_h = if ("std.error" %in% names(h_result)) std.error else NA_real_) |>
      select(condition, estimate_h = estimate, se_h),
    l_result |> select(condition, estimate_l = estimate),
    by = "condition"
  )

  # ATE_h = α + β × ATE_l
  w <- if (weight_by_precision) 1 / joined$se_h^2 else NULL
  fit <- lm(estimate_h ~ estimate_l, data = joined, weights = w)
  V   <- if (robust) sandwich::vcovHC(fit, type = "HC2") else NULL

  # tidy() emits one row per restriction, each carrying the same joint F-test
  # p-value; distinct() collapses them to the single test result
  f_test_p <- function(hypothesis) {
    (if (is.null(V)) car::linearHypothesis(fit, hypothesis)
     else            car::linearHypothesis(fit, hypothesis, vcov. = V)) |>
      broom::tidy() |>
      distinct(p.value) |>
      pull(p.value)
  }

  (if (is.null(V)) broom::tidy(fit, conf.int = TRUE)
   else broom::tidy(lmtest::coeftest(fit, vcov. = V), conf.int = TRUE)) |>
    mutate(term = if_else(term == "(Intercept)", "alpha", "beta")) |>
    select(term, estimate, lo = conf.low, hi = conf.high) |>
    pivot_wider(names_from = term, values_from = c(estimate, lo, hi),
                names_glue = "{term}_{.value}") |>
    transmute(
      alpha     = alpha_estimate,
      alpha_lo,
      alpha_hi,
      beta      = beta_estimate,
      beta_lo,
      beta_hi,
      r_squared = broom::glance(fit) |> pull(r.squared),
      # Joint F-test  H0: α = 0 AND β = 1  (perfect calibration on the original scale)
      p_joint   = f_test_p(c("(Intercept) = 0", "estimate_l = 1")),
      # Slope-only F-test  H0: β = 1  (proportional calibration; constant offset allowed)
      p_beta_1  = f_test_p("estimate_l = 1")
    )
}

# include_w1 = TRUE adds the Wasserstein-1 distance (benchmark preregistration);
# the default keeps the original preregistration's three-metric output unchanged.
compare_distributions <- function(human_data, llm_data, outcome, condition_val,
                                  include_w1 = FALSE) {
  x <- human_data |> filter(condition == condition_val) |> pull(all_of(outcome)) |> na.omit()
  y <- llm_data   |> filter(condition == condition_val) |> pull(all_of(outcome)) |> na.omit()
  out <- tibble(
    condition      = as.character(condition_val),
    ovl            = compute_ovl(x, y),
    ks_d           = suppressWarnings(ks.test(x, y)$statistic),
    variance_ratio = var(y) / var(x)  # > 1: clones more variable; < 1: less variable
  )
  if (include_w1) out <- out |> mutate(w1 = compute_w1(x, y), .after = ovl)
  out
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
                                condition_var     = "condition",
                                covariates        = NULL,
                                weights           = NULL,
                                adjust_method     = "BH",
                                compute_predicted = TRUE) {
  
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

  # `predicted_effects` (marginal effects via marginaleffects) is the expensive
  # part of this function. Callers that only need the interaction coefficients
  # — e.g. the amendment's subgroup leaderboard, which scores estimate-only
  # pairs across many mock teams — can skip it with compute_predicted = FALSE.
  predicted_effects <- NULL

  if (compute_predicted && !is_numeric_mod) {
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

  if (compute_predicted && is_numeric_mod) {
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
# min_cells_r: a Pearson r over 2 cell means is ±1 by construction and over 3
# nearly so; setting it suppresses r below that many cells (RMSE leads there).
# The default 0 keeps the original preregistration's output unchanged.
compare_demographic_baselines <- function(human_data, llm_data,
                                           outcome,
                                           moderators,
                                           condition_var = "condition",
                                           min_cells_r = 0) {
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
        r         = if (n() >= min_cells_r)
          cor(mean_h, mean_l, use = "pairwise.complete.obs") else NA_real_,
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


# ---------------------------------------------------------------------------
# Amendment additions (Silicon Sample Tournament leaderboard)
#
# These functions are NOT used by the original preregistration (v1); they are
# added for the cross-team leaderboard in `preregistration_benchmark.qmd`. The
# original preregistration reports the comparison metrics as point estimates
# only — the bootstrap intervals below are introduced by the amendment so that
# every approach can be shown on a forest plot with an uncertainty band.
# ---------------------------------------------------------------------------

# Measurement-error-adjusted comparison metrics. The reference (human) effects
# are noisy estimates of fixed true effects: estimate_h = true effect + sampling
# error with known variance se_h^2. That noise cannot contribute to the
# covariance with the predictions (it is independent of them) but it inflates
# the observed variance of the reference effects, so the raw Pearson r is
# biased toward zero. Subtracting the known noise variance from the reference
# variance recovers the correlation with the *true* effects (Spearman
# disattenuation with known error variances; the fixed-effects, method-of-
# moments analogue of the two-stage meta-analytic r_adj in Ashokkumar et al.).
# The same identity corrects the RMSE: E[(h - l)^2] = true squared error +
# noise variance. Only the reference side is corrected — submission-side
# sampling noise is handled at the source by the Tier-1 precision requirement
# (Tiers 2-3 submit point predictions with no sampling-noise analogue).
# Guards: if the estimated true variance (or true squared error) is <= 0 the
# noise swamps the signal and the adjusted value is undefined -> NA. The
# adjusted correlation is truncated to [-1, 1]. Uncertainty intervals come from
# the same cluster bootstrap as every other metric.
adjusted_metrics <- function(pairs) {
  ok <- pairs |> filter(!is.na(estimate_h), !is.na(estimate_l), !is.na(se_h))

  if (nrow(ok) < 3) return(tibble(pearson_adj = NA_real_, rmse_adj = NA_real_))

  var_true <- var(ok$estimate_h) - mean(ok$se_h^2)
  mse_true <- mean((ok$estimate_h - ok$estimate_l)^2) - mean(ok$se_h^2)

  tibble(
    pearson_adj = if (var_true > 0 && sd(ok$estimate_l) > 0)
      max(-1, min(1, cov(ok$estimate_l, ok$estimate_h) /
                       (sd(ok$estimate_l) * sqrt(var_true))))
      else NA_real_,
    rmse_adj    = if (mse_true > 0) sqrt(mse_true) else NA_real_
  )
}

# Estimate-based metrics on pre-joined pairs pooled across outcomes:
# directional agreement, Spearman rho, Pearson r, and the noise-corrected
# pearson_adj (via adjusted_metrics(), reference side only). Expects columns
# estimate_h, se_h (reference = Human 1) and estimate_l (the submission).
# include_rmse = TRUE adds rmse and its noise-corrected companion rmse_adj —
# only meaningful when all pairs share one unit; the benchmark converts every
# estimate to pp of scale range at pair-building, so there RMSE pools across
# all outcomes.
pooled_metrics <- function(pairs, include_rmse = FALSE) {
  adj <- adjusted_metrics(pairs)
  out <- pairs |>
    summarise(
      directional_pct = mean(sign(estimate_h) == sign(estimate_l), na.rm = TRUE) * 100,
      spearman_rho    = cor(estimate_h, estimate_l, method = "spearman",
                            use = "pairwise.complete.obs"),
      pearson_r       = cor(estimate_h, estimate_l, use = "pairwise.complete.obs")
    ) |>
    mutate(pearson_adj = adj$pearson_adj)
  if (include_rmse)
    out <- out |>
      mutate(rmse = sqrt(mean((pairs$estimate_h - pairs$estimate_l)^2, na.rm = TRUE)),
             rmse_adj = adj$rmse_adj)
  out
}

# Pooled calibration regression on pre-joined pairs (benchmark only). The same
# regression as run_calibration() — ATE_h = alpha + beta * ATE_l — but fit on
# the intervention × outcome pairs pooled across all outcomes (in pp of scale
# range, converted at pair-building), as Ashokkumar et al. fit their
# calibration slope across all effects of an archive. HC2-robust inference by
# default: the outcome side carries known, condition-varying sampling error.
run_calibration_pooled <- function(pairs, robust = TRUE) {
  run_calibration(
    pairs |> transmute(condition = paste(condition, outcome),
                       estimate = estimate_h, std.error = se_h),
    pairs |> transmute(condition = paste(condition, outcome),
                       estimate = estimate_l),
    robust = robust
  )
}

# Signed-effect metrics for estimate-only pairs (no SE / inferential category).
# Used for the subgroup-heterogeneity leaderboard, where the pooled comparison
# mirrors the original preregistration's rq3 pooled row (directional agreement,
# Spearman ρ, Pearson r only).
signed_metrics <- function(pairs) {
  out <- pairs |>
    summarise(
      directional_pct = mean(sign(estimate_h) == sign(estimate_l), na.rm = TRUE) * 100,
      spearman_rho    = cor(estimate_h, estimate_l, method = "spearman",
                            use = "pairwise.complete.obs"),
      pearson_r       = cor(estimate_h, estimate_l, use = "pairwise.complete.obs")
    )
  # Adjusted correlation where the reference SEs travel with the pairs. This is
  # where disattenuation earns its keep: small subgroup cells make reference
  # noise non-uniform, so raw r is unevenly deflated across cells.
  if ("se_h" %in% names(pairs))
    out <- out |> mutate(pearson_adj = adjusted_metrics(pairs)$pearson_adj)
  out
}

# Demographic parity gap (DPD, after Park et al. 2026 — adapted: they take
# the gap in per-group predictive accuracy over paired agent-person data; with
# no person-level pairing here, the gap is over group-mean baseline errors):
# one bias number per moderator. Per moderator level, the absolute error
# between the submission's and the human control-condition group mean; the DPD
# is the gap between the worst- and best-served group. 0 = the approach serves every group equally
# well — or equally badly, which is why worst_abs_err (the worst-served
# group's absolute error, Park et al.'s primary concern) is reported alongside
# the gap. Large DPD values flag that baseline accuracy is concentrated in
# some groups. Complements compare_demographic_baselines() (which averages
# over groups and so can hide exactly this gap). Groups under min_n are
# skipped: a group mean over few respondents has an SE of several scale
# points, and the max-over-groups error is a max-of-noise statistic that is
# mechanically positive even for a perfect approach — read the DPD against
# the human-replication reference row, which carries the same affliction.
demographic_parity_gap <- function(human_data, llm_data, outcome, moderators,
                                   condition_val, min_n = 30) {
  map_dfr(moderators, function(mod) {
    groups <- levels(human_data[[mod]])
    errs <- map_dbl(groups, function(grp) {
      x <- human_data |>
        filter(condition == condition_val, .data[[mod]] == grp) |>
        pull(all_of(outcome)) |> na.omit()
      y <- llm_data |>
        filter(condition == condition_val, .data[[mod]] == grp) |>
        pull(all_of(outcome)) |> na.omit()
      if (length(x) < min_n || length(y) < min_n) return(NA_real_)
      abs(mean(y) - mean(x))
    })
    if (all(is.na(errs))) {
      tibble(moderator = mod, dpd = NA_real_, worst_abs_err = NA_real_,
             worst_group = NA_character_, best_group = NA_character_)
    } else {
      tibble(
        moderator     = mod,
        dpd           = max(errs, na.rm = TRUE) - min(errs, na.rm = TRUE),
        worst_abs_err = max(errs, na.rm = TRUE),
        worst_group   = groups[which.max(errs)],
        best_group    = groups[which.min(errs)]
      )
    }
  })
}

# Generic cluster bootstrap. Resamples the levels of `cluster` with
# replacement, applies the summary function `f` (which returns a one-row tibble
# of named metrics) to each replicate, and returns the point estimate (from `f`
# on the full data) together with percentile confidence intervals, in long
# form: one row per metric with columns value, lo, hi.
cluster_boot <- function(df, f, cluster = "condition", n_boot = 1000,
                         conf = 0.95, seed = 2026) {
  if (!is.null(seed)) set.seed(seed)

  point <- f(df) |>
    pivot_longer(everything(), names_to = "metric", values_to = "value")

  parts    <- split(df, as.character(df[[cluster]]))
  cl_names <- names(parts)

  boot <- map_dfr(seq_len(n_boot), function(b) {
    drawn <- sample(cl_names, length(cl_names), replace = TRUE)
    f(bind_rows(parts[drawn])) |>
      pivot_longer(everything(), names_to = "metric", values_to = "value")
  })

  alpha <- (1 - conf) / 2
  point |>
    left_join(
      boot |>
        group_by(metric) |>
        summarise(lo = quantile(value, alpha,     na.rm = TRUE),
                  hi = quantile(value, 1 - alpha, na.rm = TRUE),
                  .groups = "drop"),
      by = "metric"
    )
}

# Per-group metrics: applies a pooled-metric function (`pooled_metrics` or
# `signed_metrics`) within each level of `group`, giving one score per group
# instead of a single pooled score. With group = "outcome" it scores each
# outcome across the interventions; with group = "condition" it scores each
# intervention across the outcomes. Drives the field-distribution figures,
# which show the spread of approaches per outcome / per intervention. Per
# intervention the score is taken across `outcomes_continuous` (all on the same
# 0-100 scale), so the correlations are not scale-confounded.
metrics_by_group <- function(pairs, group, f = pooled_metrics) {
  pairs |>
    group_by(.data[[group]]) |>
    # .keep = TRUE: f may need the grouping column itself
    group_modify(~ f(.x), .keep = TRUE) |>
    ungroup()
}

# Distribution of a metric across approaches (one row per metric): mean,
# median, sd, min, max over the submissions, excluding the human-replication
# reference row. Feeds the companion table under the headline field-distribution
# figure. Expects long input with columns submission, metric, value.
summarise_field <- function(field_long, ceiling_label = "Human replication") {
  field_long |>
    filter(submission != ceiling_label) |>
    group_by(metric) |>
    summarise(
      mean   = mean(value,   na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd     = sd(value,     na.rm = TRUE),
      min    = min(value,    na.rm = TRUE),
      max    = max(value,    na.rm = TRUE),
      .groups = "drop"
    )
}
