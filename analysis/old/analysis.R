# =============================================================================
# analysis/old/analysis.R
#
# Runs the preregistered analyses (preregistration.qmd) on the REAL cleaned
# data (data/cleaned_human.rds, data/cleaned_llm.rds) and writes the resulting
# data frames + figures to analysis/old/results/.
#
# This is the single precompute step for the workshop deck: the slides only
# READ these artifacts -- no model fitting at render time.
#
# DEVIATIONS FROM preregistration.qmd (intentional, for preliminary results):
#   1. No 50/50 human split. The human sample is too small at this stage to
#      spend half on a human-human ceiling, so every analysis is simply
#      Human vs. LLM (all the `_h2` / "Human 2" objects are dropped).
#   2. Subgroups use ONLY partisan identity, reduced to Democrats vs.
#      Republicans (Independent/Other dropped). Besides matching the talk's
#      scope, this keeps the preregistered HC2 estimator well-defined: the
#      tiny "Other" group (~4 per condition) creates singleton condition x
#      Other cells with leverage hat = 1; HC2 divides by (1 - hat) and injects
#      NaN into the whole sandwich covariance. droplevels() preserves factor
#      order so Republican stays the reference level.
#   3. LLM results are reported PER MODEL (gpt-4o-mini, gemini-2.5-flash-lite)
#      rather than pooled, so model quality can be compared in the talk.
#
# Regenerate from current data with:  Rscript analysis/old/analysis.R
# =============================================================================

# --- Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(sandwich)
  library(lmtest)
  library(marginaleffects)
  library(broom)
  library(MetBrewer)   # plots.R builds palettes at source time
  library(patchwork)
  library(here)
})

# Shared analysis functions (run_*, compare_* live in R/functions/statistics.R)
list.files(here("R/functions"), pattern = "\\.R$", full.names = TRUE) |>
  walk(source)

results_dir <- here("analysis/old/results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

save_result <- function(obj, name) {
  saveRDS(obj, file.path(results_dir, paste0(name, ".rds")))
  message(sprintf("  saved %-34s (%s)", paste0(name, ".rds"),
                  if (is.data.frame(obj)) paste0(nrow(obj), " rows")
                  else class(obj)[1]))
}

# Some prereg fits are mathematically undefined on partial real data (tiny or
# empty cells, near-constant outcomes). Attempt the EXACT prereg fit per unit;
# record undefined units in _meta$analysis_skips instead of aborting.
analysis_skips <- list()
safe_bind <- function(units, fn, label, .id = NULL) {
  res <- map(units, possibly(fn, otherwise = NULL))
  ok  <- !map_lgl(res, is.null)
  if (any(!ok)) {
    bad <- units[!ok]
    message(sprintf("  [%s] SKIPPED %d/%d: %s",
                    label, sum(!ok), length(units), paste(bad, collapse = ", ")))
    analysis_skips[[label]] <<- bad
  }
  if (!is.null(.id)) list_rbind(set_names(res[ok], units[ok]), names_to = .id)
  else               list_rbind(res[ok])
}

# --- Data --------------------------------------------------------------------

human_raw <- readRDS(here("data/cleaned_human.rds"))
llm_raw   <- readRDS(here("data/cleaned_llm.rds"))

# Only conditions the clones actually ran can be compared (humans also saw the
# 4 interactive arms). Restrict both to the shared set; keep control as ref.
shared_conditions <- intersect(levels(human_raw$condition),
                               levels(llm_raw$condition))

prep_conditions <- function(d) {
  d |>
    filter(condition %in% shared_conditions) |>
    mutate(condition = relevel(droplevels(factor(condition)), ref = "control"))
}

human_data <- prep_conditions(human_raw)

# Split LLM by model -> readable names
model_label_map <- c(
  "openai/gpt-4o-mini"           = "GPT-4o-mini",
  "google/gemini-2.5-flash-lite" = "Gemini-2.5-flash-lite"
)
llm_prepped <- llm_raw |>
  prep_conditions() |>
  mutate(model = recode(user_model, !!!model_label_map))

models       <- sort(unique(llm_prepped$model))
llm_by_model <- map(set_names(models),
                     \(m) filter(llm_prepped, model == m))

# Reduced partisan data: Democrats vs Republicans only (see header note).
# droplevels keeps Republican as the reference level.
to_dem_rep <- function(d) {
  d |>
    filter(party %in% c("Republican", "Democrat")) |>
    mutate(party = droplevels(party))
}
human_party    <- to_dem_rep(human_data)
llm_party      <- map(llm_by_model, to_dem_rep)

message(sprintf("Human N = %d | LLM models: %s",
                nrow(human_data),
                paste(sprintf("%s (N=%d)", models, map_int(llm_by_model, nrow)),
                      collapse = ", ")))

# --- Variable-set definitions (verbatim from preregistration.qmd ~300-355) ---

outcomes_primary   <- "trust_multidimensional"
outcomes_secondary <- c("trust_post", "distrust_post", "funding_perceptions",
                        "policy_role_mean", "inst_trust_mean")
outcomes_binary    <- "newsletter_signup"
outcomes_tertiary  <- c("belief_post", "concern_mean", "policy_general",
                        "policy_specific_mean", "behavior_mean")
outcomes_continuous <- c(outcomes_primary, outcomes_secondary, outcomes_tertiary)
outcomes_illustrative <- c("trust_multidimensional", "donation_ams",
                           "funding_perceptions", "policy_general")

# DEVIATION 2: subgroup analyses use partisan identity only.
moderators_cat <- c("party")

outcome_label_map <- c(
  trust_multidimensional = "Multidimensional trust (primary)",
  trust_post             = "Single-item trust",
  distrust_post          = "Distrust",
  donation_ams           = "Donation to AMS ($)",
  newsletter_signup      = "Newsletter sign-up",
  funding_perceptions    = "Funding perceptions",
  policy_role_mean       = "Policy role",
  inst_trust_mean        = "Institutional trust",
  belief_post            = "Climate belief",
  concern_mean           = "Climate concern",
  policy_general         = "General climate policy",
  policy_specific_mean   = "Specific climate policy",
  behavior_mean          = "Pro-climate behavior"
)

assign_tier <- function(out) case_when(
  out == outcomes_primary                            ~ "Primary",
  out %in% c(outcomes_secondary, outcomes_binary)    ~ "Secondary",
  TRUE                                               ~ "Tertiary"
)

prep_ate <- function(df) {
  df |>
    select(condition, estimate) |>
    mutate(
      se    = if ("std.error"        %in% names(df)) df$std.error        else NA_real_,
      p_adj = if ("p.value_adjusted" %in% names(df)) df$p.value_adjusted else NA_real_
    )
}

# =============================================================================
# SECTION 1 -- Average Treatment Effects
# =============================================================================

message("\n[1] ATE recovery (all outcomes, per model)...")

# Human models fit once; reused against every LLM model.
ate_h <- map(set_names(outcomes_continuous), function(o)
  possibly(\() run_main_treatment_model(human_data, outcome = o),
           otherwise = NULL)())
if (any(map_lgl(ate_h, is.null))) {
  bad <- names(ate_h)[map_lgl(ate_h, is.null)]
  analysis_skips[["ate_human"]] <- bad
  message(sprintf("  [ate_human] SKIPPED: %s", paste(bad, collapse = ", ")))
}
ate_h <- compact(ate_h)
outcomes_ate <- names(ate_h)
h_bin <- possibly(\() run_main_treatment_model_binary(human_data,
                  "newsletter_signup"), otherwise = NULL)()

# Per model: per-outcome ATE models, pooled pairs, metrics, calibration.
ate_model_results <- list()
pooled_ates       <- list()
ate_metrics       <- list()
calib_metrics     <- list()

for (m in models) {
  ld    <- llm_by_model[[m]]
  l_bin <- possibly(\() run_main_treatment_model_binary(ld,
             "newsletter_signup"), otherwise = NULL)()
  has_bin <- !is.null(h_bin) && !is.null(l_bin)

  res <- map(set_names(outcomes_ate), function(o) {
    l <- possibly(\() run_main_treatment_model(ld, outcome = o),
                  otherwise = NULL)()
    if (is.null(l)) NULL else list(h = ate_h[[o]], l = l)
  }) |> compact()
  outs <- names(res)
  ate_model_results[[m]] <- res

  bin_pair <- function(rename_fn) {
    if (!has_bin) return(NULL)
    inner_join(
      prep_ate(h_bin$marginal_effects) |>
        rename(estimate_h = estimate, se_h = se, p_adj_h = p_adj),
      prep_ate(l_bin$marginal_effects) |>
        rename(estimate_l = estimate, se_l = se, p_adj_l = p_adj),
      by = "condition"
    ) |> mutate(outcome = "newsletter_signup")
  }

  pooled_ates[[m]] <- bind_rows(
    map_dfr(outs, function(o) {
      inner_join(
        prep_ate(res[[o]]$h) |> rename(estimate_h = estimate, se_h = se, p_adj_h = p_adj),
        prep_ate(res[[o]]$l) |> rename(estimate_l = estimate, se_l = se, p_adj_l = p_adj),
        by = "condition"
      ) |> mutate(outcome = o)
    }),
    bin_pair()
  ) |>
    mutate(tier = assign_tier(outcome), outcome_label = outcome_label_map[outcome],
           model = m)

  per_out <- bind_rows(
    map_dfr(outs, function(o) {
      compare_estimates(res[[o]]$h, res[[o]]$l) |> mutate(outcome = o)
    }),
    if (has_bin) compare_estimates(h_bin$marginal_effects, l_bin$marginal_effects) |>
      mutate(outcome = "newsletter_signup")
  ) |>
    mutate(tier = assign_tier(outcome), outcome_label = outcome_label_map[outcome])

  pooled_row <- pooled_ates[[m]] |>
    mutate(
      delta      = 0.5 * abs(estimate_h),
      diff       = estimate_h - estimate_l,
      se_diff    = sqrt(se_h^2 + se_l^2),
      p_tost     = pmax(pnorm((diff + delta) / se_diff, lower.tail = FALSE),
                        pnorm((diff - delta) / se_diff)),
      equivalent = p_tost < 0.05,
      same_sign  = sign(estimate_h) == sign(estimate_l),
      infer_h    = case_when(
        !is.na(p_adj_h) & p_adj_h < 0.05 & estimate_h > 0 ~ "sig_positive",
        !is.na(p_adj_h) & p_adj_h < 0.05 & estimate_h < 0 ~ "sig_negative",
        TRUE ~ "not_significant"),
      infer_l    = case_when(
        !is.na(p_adj_l) & p_adj_l < 0.05 & estimate_l > 0 ~ "sig_positive",
        !is.na(p_adj_l) & p_adj_l < 0.05 & estimate_l < 0 ~ "sig_negative",
        TRUE ~ "not_significant")
    ) |>
    summarise(
      spearman_rho    = cor(estimate_h, estimate_l, method = "spearman",
                            use = "pairwise.complete.obs"),
      pearson_r       = cor(estimate_h, estimate_l, use = "pairwise.complete.obs"),
      rmse            = NA_real_,
      directional_pct = mean(same_sign,  na.rm = TRUE) * 100,
      inferential_pct = mean(infer_h == infer_l, na.rm = TRUE) * 100,
      tost_pct        = mean(equivalent, na.rm = TRUE) * 100
    ) |>
    mutate(outcome = "pooled", tier = "All outcomes", outcome_label = "All outcomes")

  ate_metrics[[m]] <- bind_rows(pooled_row, per_out) |> mutate(model = m)

  calib_metrics[[m]] <- bind_rows(
    map_dfr(outs, function(o) {
      run_calibration(res[[o]]$h, res[[o]]$l) |> mutate(outcome = o)
    }),
    if (has_bin) run_calibration(h_bin$marginal_effects, l_bin$marginal_effects) |>
      mutate(outcome = "newsletter_signup")
  ) |>
    mutate(tier = assign_tier(outcome), outcome_label = outcome_label_map[outcome],
           model = m)
}

pooled_ates   <- list_rbind(pooled_ates)
ate_metrics   <- list_rbind(ate_metrics)
calib_metrics <- list_rbind(calib_metrics)

save_result(pooled_ates,   "ate_pooled_pairs")
save_result(ate_metrics,   "ate_metrics")
save_result(calib_metrics, "ate_calibration")

# fig-rq1-pooled / fig-rq1-all are built in the deck from ate_pooled_pairs.rds
# (it carries estimate_h, estimate_l, condition, outcome, tier, model).

# --- Section 1b: response distributions in control (illustrative) -----------

message("[1b] Control-condition response distributions...")
control_val <- levels(human_data$condition)[1]

dist_control <- map_dfr(models, function(m) {
  safe_bind(outcomes_illustrative,
    function(o) compare_distributions(human_data, llm_by_model[[m]], o, control_val) |>
      mutate(outcome = o),
    paste0("dist_", m)) |>
    mutate(model = m)
}) |>
  mutate(tier = assign_tier(outcome), outcome_label = outcome_label_map[outcome])
save_result(dist_control, "dist_control")

# Tidy values for the deck's control-condition density plot.
dist_control_values <- bind_rows(
  map_dfr(outcomes_illustrative, function(o)
    human_data |> filter(condition == control_val) |>
      transmute(value = .data[[o]], source = "Human", outcome = o)),
  map_dfr(models, function(m)
    map_dfr(outcomes_illustrative, function(o)
      llm_by_model[[m]] |> filter(condition == control_val) |>
        transmute(value = .data[[o]], source = m, outcome = o)))
) |>
  filter(!is.na(value)) |>
  mutate(outcome_label = outcome_label_map[outcome])
save_result(dist_control_values, "dist_control_values")

# =============================================================================
# SECTION 2 -- Subgroup Effects (partisan identity, Dem vs Rep only)
# =============================================================================

message("\n[2] Subgroup heterogeneity (party: Dem vs Rep, per model)...")

# Human moderator models fit once per illustrative outcome.
mod_h <- map(set_names(outcomes_illustrative), function(o)
  possibly(\() run_moderator_model(human_party, outcome = o, moderator = "party"),
           otherwise = NULL)())

rq3_interactions       <- list()
rq3_metrics_by_outcome <- list()

for (m in models) {
  lp <- llm_party[[m]]
  mod_l <- map(set_names(outcomes_illustrative), function(o)
    possibly(\() run_moderator_model(lp, outcome = o, moderator = "party"),
             otherwise = NULL)())

  ok <- intersect(
    names(compact(mod_h)),
    names(compact(mod_l))
  )

  rq3_interactions[[m]] <- map_dfr(ok, function(o) {
    inner_join(
      mod_h[[o]]$interaction_effects |>
        select(condition, moderator_level, estimate_h = estimate),
      mod_l[[o]]$interaction_effects |>
        select(condition, moderator_level, estimate_l = estimate),
      by = c("condition", "moderator_level")
    ) |> mutate(outcome = o, moderator = "party")
  }) |> mutate(model = m)

  rq3_metrics_by_outcome[[m]] <- map_dfr(ok, function(o) {
    compare_estimates(
      mod_h[[o]]$interaction_effects,
      mod_l[[o]]$interaction_effects,
      join_by = c("condition", "moderator_level")
    ) |> mutate(outcome = o, moderator = "party",
                outcome_label = outcome_label_map[o], tier = assign_tier(o))
  }) |> mutate(model = m)
}

rq3_interactions       <- list_rbind(rq3_interactions)
rq3_metrics_by_outcome <- list_rbind(rq3_metrics_by_outcome)

rq3_pooled_row <- rq3_interactions |>
  group_by(model) |>
  summarise(
    spearman_rho    = cor(estimate_h, estimate_l, method = "spearman",
                          use = "pairwise.complete.obs"),
    pearson_r       = cor(estimate_h, estimate_l, use = "pairwise.complete.obs"),
    directional_pct = mean(sign(estimate_h) == sign(estimate_l), na.rm = TRUE) * 100,
    .groups = "drop"
  )

save_result(rq3_interactions,       "subgroup_interactions")
save_result(rq3_metrics_by_outcome, "subgroup_metrics")
save_result(rq3_pooled_row,         "subgroup_pooled")

# fig-rq3 is built in the deck from subgroup_interactions.rds.

# --- Section 2b: within-subgroup distributions (primary outcome) ------------

message("[2b] Within-party-subgroup distributions (primary outcome)...")
subgroup_dist <- map_dfr(models, function(m) {
  map_dfr(levels(human_party$party), function(grp) {
    x <- human_party |>
      filter(condition == control_val, party == grp) |>
      pull(trust_multidimensional) |> na.omit()
    y <- llm_party[[m]] |>
      filter(condition == control_val, party == grp) |>
      pull(trust_multidimensional) |> na.omit()
    if (length(x) < 10 | length(y) < 10) return(NULL)
    tibble(model = m, moderator = "party", group = grp,
           ovl = compute_ovl(x, y),
           ks_d = suppressWarnings(ks.test(x, y)$statistic),
           variance_ratio = var(y) / var(x))
  })
})
save_result(subgroup_dist, "subgroup_dist")

# Tidy values for the deck's within-party density plot (primary outcome).
subgroup_dist_values <- bind_rows(
  map_dfr(levels(human_party$party), function(grp)
    human_party |> filter(condition == control_val, party == grp) |>
      transmute(value = trust_multidimensional, source = "Human", group = grp)),
  map_dfr(models, function(m)
    map_dfr(levels(human_party$party), function(grp)
      llm_party[[m]] |> filter(condition == control_val, party == grp) |>
        transmute(value = trust_multidimensional, source = m,
                  group = grp, model = m)))
) |>
  filter(!is.na(value))
save_result(subgroup_dist_values, "subgroup_dist_values")

# =============================================================================
# SECTION 3 -- Demographic Baseline Calibration (party only)
# =============================================================================

message("\n[3] Demographic baseline calibration & predictability (party)...")

baseline_cal <- map_dfr(models, function(m) {
  compare_demographic_baselines(
    human_party, llm_party[[m]],
    outcome = "trust_multidimensional", moderators = moderators_cat
  ) |> mutate(model = m)
})
save_result(baseline_cal, "demographic_baseline")

pred_rsq   <- list()
pred_coefs <- list()
for (m in models) {
  pt <- compare_demographic_predictability(
    human_party, llm_party[[m]],
    outcome = "trust_multidimensional", predictors = moderators_cat
  )
  pred_rsq[[m]]   <- pt$r_squared    |> mutate(model = m)
  pred_coefs[[m]] <- pt$coefficients |> mutate(model = m)
}
pred_rsq   <- list_rbind(pred_rsq)
pred_coefs <- list_rbind(pred_coefs)
save_result(pred_rsq,   "demographic_predictability_rsq")
save_result(pred_coefs, "demographic_predictability_coefs")

# fig-rq4b-coefs is built in the deck from demographic_predictability_coefs.rds.

# --- Metadata + data snapshots ----------------------------------------------

meta <- list(
  snapshot_date     = Sys.Date(),
  generated_at      = Sys.time(),
  human_n           = nrow(human_data),
  llm_n_by_model    = map_int(llm_by_model, nrow),
  models            = models,
  shared_conditions = shared_conditions,
  party_subset      = "Democrat vs Republican only (Independent/Other dropped)",
  human_split       = "none (no 50/50 ceiling at preliminary stage)",
  outcomes_continuous   = outcomes_continuous,
  outcomes_illustrative = outcomes_illustrative,
  outcome_label_map = outcome_label_map,
  analysis_skips    = analysis_skips,
  session_info      = sessionInfo()
)
save_result(meta, "_meta")
save_result(human_data, "data_snapshot_human")
save_result(llm_by_model, "data_snapshot_llm")

# --- Console summary ---------------------------------------------------------

cat("\n================ ATE recovery (pooled across outcomes) ================\n")
ate_metrics |>
  filter(outcome == "pooled") |>
  select(model, spearman_rho, pearson_r, directional_pct,
         inferential_pct, tost_pct) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  print()

cat("\n================ Subgroup (party) pooled =============================\n")
print(rq3_pooled_row |> mutate(across(where(is.numeric), \(x) round(x, 2))))

cat("\n================ Demographic predictability R^2 (party) ==============\n")
pred_rsq |>
  pivot_wider(names_from = source, values_from = r_squared) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

if (length(analysis_skips)) {
  cat("\nNOTE: some fits were undefined on current data (see _meta$analysis_skips):\n")
  for (nm in names(analysis_skips))
    cat(sprintf("  - %s: %s\n", nm, paste(analysis_skips[[nm]], collapse = ", ")))
}

message(sprintf(
  "\nDone. %d result objects written (no figures). Snapshot %s | Human N = %d",
  length(list.files(results_dir, pattern = "\\.rds$")),
  as.character(meta$snapshot_date), nrow(human_data)
))
