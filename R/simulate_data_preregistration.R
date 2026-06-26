############################################################
# Simulate data for LLM clone preregistration
# LLM Predictions Megastudy
#
# Generates two datasets with identical structure:
#   (1) human_data_preregistration.rds  — "human" sample
#   (2) llm_data_preregistration.rds    — "LLM clone" sample
#
# No actual treatment effects or human-vs-LLM differences
# are simulated. The goal is to produce two plausibly
# structured datasets so the comparison analysis code
# can be written and tested before real data exist.
############################################################

library(tidyverse)

############################################################
# Parameters
############################################################

N_per_condition         <- 1000
N_per_control_condition <- 2000
N_interventions         <- 20
N                       <- N_per_condition * N_interventions + N_per_control_condition
control_subtexts        <- c("neckties", "baseball", "dances")
institutions            <- c("epa", "nasa", "noaa", "universities", "federal_gov")
alien_info_levels <- c(
  "Never", "Once or twice during the year", "Several times during the year",
  "Once or twice a month", "Once or twice a week", "Almost every day",
  "Once or more per day"
)

############################################################
# Helper functions
############################################################

r_slider <- function(n) pmin(pmax(rnorm(n, 50, 30), 0), 100)
r_likert <- function(n, lo = 1, hi = 7) sample(lo:hi, n, replace = TRUE)
r_cat    <- function(n, levels) factor(sample(levels, n, replace = TRUE), levels = levels)

simulate_behavior_item <- function(n, na_label, p_na = 0.1) {
  is_na <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(p_na, 1 - p_na))
  list(
    value          = if_else(is_na, NA_real_, r_slider(n)),
    not_applicable = if_else(is_na, na_label, NA_character_)
  )
}

############################################################
# Main simulation function
############################################################

simulate_dataset <- function(seed) {
  set.seed(seed)

  # --- Condition assignment ---
  dat <- tibble(
    id        = 1:N,
    condition = sample(c(
      rep(paste0("intervention_", 1:N_interventions), each = N_per_condition),
      rep("control", N_per_control_condition)
    ))
  ) |>
    mutate(
      control_subtext = if_else(
        condition == "control",
        sample(control_subtexts, N, replace = TRUE),
        NA_character_
      ),
      control_subtext = factor(control_subtext, levels = control_subtexts)
    )

  # --- Demographics ---
  dat <- dat |>
    mutate(
      gender     = r_cat(N, c("Male", "Female", "Other")),
      year_birth = round(pmin(pmax(rnorm(N, 1979, 18), 1934), 2006)),
      age        = 2025L - year_birth,
      race = r_cat(N, c(
        "White / Caucasian", "Black / African American",
        "Hispanic / Latino", "Asian / Asian American", "Other"
      )),
      education = r_cat(N, c(
        "Less than high school", "High school diploma / GED",
        "Some college or Associate's degree", "Bachelor's degree",
        "Master's degree / Professional degree", "Doctorate degree / Ph.D."
      )),
      education_climate_primary    = r_cat(N, c("Yes", "No", "Not applicable")),
      education_climate_highschool = r_cat(N, c("Yes", "No", "Not applicable")),
      education_climate_university = r_cat(N, c("Yes", "No", "Not applicable")),
      income = r_cat(N, c(
        "Less than $30,000", "$30,000 to $55,999", "$56,000 to $99,999",
        "$100,000 to $167,999", "$168,000 or more"
      )),
      household_size = r_cat(N, c("1", "2", "3", "4", "5", "6 or more")),
      social_class   = r_cat(N, c("Lower class", "Working class", "Middle class", "Upper class")),
      urban_rural    = r_cat(N, c(
        "A large city", "A suburb near a large city", "A small city or town", "A rural area"
      )),
      zip_code = sprintf("%05d", sample(10000:99999, N, replace = TRUE))
    )

  # --- Partisan identity ---
  dat <- dat |>
    mutate(
      party = r_cat(N, c("Republican", "Democrat", "Independent", "Other")),
      party_importance = if_else(
        party %in% c("Republican", "Democrat"), r_slider(N), NA_real_
      )
    )

  # --- Religion ---
  dat <- dat |>
    mutate(
      religion = r_cat(N, c(
        "I am not religious", "Protestant", "Catholic", "Orthodox Christian",
        "Mormon", "Muslim", "Jewish", "Hindu", "Buddhist", "Other religion"
      )),
      born_again = if_else(
        religion %in% c("Protestant", "Catholic", "Orthodox Christian", "Mormon"),
        r_cat(N, c("Yes", "No")), NA_character_
      ),
      born_again  = factor(born_again, levels = c("Yes", "No")),
      religiosity = if_else(religion != "I am not religious", r_slider(N), NA_real_)
    )

  # --- Need for epistemic autonomy (6 items, 1-7; item 6 reverse-scored) ---
  epist_nms   <- paste0("epist_auton_", 1:6)
  epist_items <- map(set_names(1:6, epist_nms), ~ r_likert(N, 1, 7)) |> as_tibble()
  dat <- bind_cols(dat, epist_items) |>
    mutate(
      epist_auton_6r   = 8L - epist_auton_6,
      epist_auton_mean = rowMeans(cbind(
        epist_auton_1, epist_auton_2, epist_auton_3,
        epist_auton_4, epist_auton_5, epist_auton_6r
      ))
    )

  # --- Pre-treatment outcomes ---
  dat <- dat |> mutate(belief_pre = r_slider(N), trust_pre = r_slider(N))

  # Alienation from climate science
  alien_inst    <- map(set_names(1:2, c("alien_inst_1",    "alien_inst_2")),    ~ r_likert(N)) |> as_tibble()
  alien_social  <- map(set_names(1:2, c("alien_social_1",  "alien_social_2")),  ~ r_likert(N)) |> as_tibble()
  alien_spatial <- map(set_names(1:2, c("alien_spatial_1", "alien_spatial_2")), ~ r_likert(N)) |> as_tibble()
  alien_info    <- map(set_names(1:10, paste0("alien_info_", 1:10)), ~ r_cat(N, alien_info_levels)) |> as_tibble()

  dat <- bind_cols(dat, alien_inst, alien_social, alien_spatial, alien_info) |>
    mutate(
      alien_inst_mean    = rowMeans(cbind(alien_inst_1, alien_inst_2)),
      alien_social_mean  = rowMeans(cbind(alien_social_1, alien_social_2)),
      alien_spatial_mean = rowMeans(cbind(alien_spatial_1, alien_spatial_2)),
      across(starts_with("alien_info_"),
             ~ as.integer(factor(.x, levels = alien_info_levels)),
             .names = "{.col}_num"),
      alien_info_mean = rowMeans(pick(ends_with("_num") & starts_with("alien_info_")))
    )

  # --- Primary outcome: multidimensional trust (12 items, 0-100) ---
  tc <- map(set_names(1:3, paste0("trust_competence_",  1:3)), ~ r_slider(N)) |> as_tibble()
  ti <- map(set_names(1:3, paste0("trust_integrity_",   1:3)), ~ r_slider(N)) |> as_tibble()
  tb <- map(set_names(1:3, paste0("trust_benevolence_", 1:3)), ~ r_slider(N)) |> as_tibble()
  to <- map(set_names(1:3, paste0("trust_openness_",    1:3)), ~ r_slider(N)) |> as_tibble()

  dat <- bind_cols(dat, tc, ti, tb, to) |>
    mutate(
      trust_competence       = rowMeans(tc),
      trust_integrity        = rowMeans(ti),
      trust_benevolence      = rowMeans(tb),
      trust_openness         = rowMeans(to),
      trust_multidimensional = rowMeans(cbind(
        trust_competence, trust_integrity, trust_benevolence, trust_openness
      ))
    )

  # --- Secondary outcomes ---
  dat <- dat |>
    mutate(
      trust_post          = r_slider(N),
      distrust_post       = r_slider(N),
      donation_ams        = round(runif(N, 0, 10), 1),
      donation_self       = 10 - donation_ams,
      newsletter_signup   = sample(c(TRUE, FALSE), N, replace = TRUE, prob = c(0.2, 0.8)),
      funding_perceptions = r_slider(N)
    )

  pr <- map(set_names(1:4, paste0("policy_role_", 1:4)), ~ r_slider(N)) |> as_tibble()
  dat <- bind_cols(dat, pr) |> mutate(policy_role_mean = rowMeans(pr))

  it <- map(set_names(seq_along(institutions), paste0("inst_trust_", institutions)), ~ r_slider(N)) |> as_tibble()
  dat <- bind_cols(dat, it) |> mutate(inst_trust_mean = rowMeans(it))

  # --- Tertiary outcomes ---
  dat <- dat |> mutate(belief_post = r_slider(N))

  co <- map(set_names(1:3, paste0("concern_", 1:3)), ~ r_slider(N)) |> as_tibble()
  dat <- bind_cols(dat, co) |> mutate(concern_mean = rowMeans(co))

  dat <- dat |> mutate(policy_general = r_slider(N))

  ps <- map(set_names(1:7, paste0("policy_specific_", 1:7)), ~ r_slider(N)) |> as_tibble()
  dat <- bind_cols(dat, ps) |> mutate(policy_specific_mean = rowMeans(ps))

  na_labels   <- c("I am a vegetarian", "I never drive by myself",
                   "I already have enough solar panels installed", "I never fly")
  item_names  <- c("meat", "transport", "solar", "fly")
  p_na_values <- c(0.08, 0.12, 0.15, 0.10)

  beh_na <- map2(na_labels, p_na_values, ~ simulate_behavior_item(N, .x, .y))
  beh_cols <- map_dfc(seq_along(item_names), function(i) {
    tibble(
      !!paste0("behavior_", item_names[i])        := beh_na[[i]]$value,
      !!paste0("behavior_", item_names[i], "_na") := beh_na[[i]]$not_applicable
    )
  }) |>
    mutate(behavior_talk = r_slider(N), behavior_donate = r_slider(N))

  dat <- bind_cols(dat, beh_cols) |>
    mutate(
      behavior_mean = rowMeans(
        cbind(behavior_meat, behavior_transport, behavior_solar,
              behavior_fly, behavior_talk, behavior_donate),
        na.rm = TRUE
      )
    )

  # --- Attrition ---
  outcome_vars <- c(
    paste0("trust_competence_",  1:3), paste0("trust_integrity_",   1:3),
    paste0("trust_benevolence_", 1:3), paste0("trust_openness_",    1:3),
    "trust_competence", "trust_integrity", "trust_benevolence",
    "trust_openness",   "trust_multidimensional",
    "trust_post", "distrust_post", "donation_ams", "donation_self",
    "funding_perceptions", paste0("policy_role_", 1:4), "policy_role_mean",
    paste0("inst_trust_", institutions), "inst_trust_mean",
    "belief_post", paste0("concern_", 1:3), "concern_mean",
    "policy_general", paste0("policy_specific_", 1:7), "policy_specific_mean",
    "behavior_meat", "behavior_transport", "behavior_solar", "behavior_fly",
    "behavior_talk", "behavior_donate", "behavior_mean"
  )

  item_skip_rates <- c(
    trust_multidimensional = 0.01, trust_competence = 0.01,
    trust_integrity = 0.01, trust_benevolence = 0.01, trust_openness = 0.01,
    trust_post = 0.01, distrust_post = 0.01,
    donation_ams = 0.03, donation_self = 0.03,
    newsletter_signup = 0.05, funding_perceptions = 0.02,
    policy_role_mean = 0.02, inst_trust_mean = 0.02,
    belief_post = 0.02, concern_mean = 0.02,
    policy_general = 0.02, policy_specific_mean = 0.03, behavior_mean = 0.03
  )

  dat <- dat |>
    group_by(condition) |>
    mutate(dropout = row_number() %in% sample(n(), size = ceiling(0.10 * n()))) |>
    ungroup() |>
    mutate(across(all_of(outcome_vars), ~ if_else(dropout, NA_real_, .x))) |>
    mutate(newsletter_signup = if_else(dropout, NA, newsletter_signup))

  for (var in names(item_skip_rates)) {
    rate <- item_skip_rates[[var]]
    dat <- dat |>
      mutate(!!var := if_else(
        !dropout & runif(n()) < rate,
        if (is.logical(.data[[var]])) NA else NA_real_,
        .data[[var]]
      ))
  }

  dat <- dat |> select(-dropout)

  # --- Factor codings ---
  dat |>
    mutate(
      condition  = relevel(factor(condition), ref = "control"),
      gender     = factor(gender, levels = c("Male", "Female", "Other")),
      race = factor(race, levels = c(
        "White / Caucasian", "Black / African American",
        "Hispanic / Latino", "Asian / Asian American", "Other"
      )),
      education = factor(education, levels = c(
        "Less than high school", "High school diploma / GED",
        "Some college or Associate's degree", "Bachelor's degree",
        "Master's degree / Professional degree", "Doctorate degree / Ph.D."
      )),
      education_climate_primary    = factor(education_climate_primary,    levels = c("Yes", "No", "Not applicable")),
      education_climate_highschool = factor(education_climate_highschool, levels = c("Yes", "No", "Not applicable")),
      education_climate_university = factor(education_climate_university, levels = c("Yes", "No", "Not applicable")),
      income = factor(income, levels = c(
        "Less than $30,000", "$30,000 to $55,999", "$56,000 to $99,999",
        "$100,000 to $167,999", "$168,000 or more"
      )),
      household_size = factor(household_size, levels = c("1", "2", "3", "4", "5", "6 or more")),
      social_class = factor(social_class, levels = c(
        "Lower class", "Working class", "Middle class", "Upper class"
      )),
      urban_rural = factor(urban_rural, levels = c(
        "A large city", "A suburb near a large city", "A small city or town", "A rural area"
      )),
      party = factor(party, levels = c("Republican", "Democrat", "Independent", "Other")),
      religion = factor(religion, levels = c(
        "I am not religious", "Protestant", "Catholic", "Orthodox Christian",
        "Mormon", "Muslim", "Jewish", "Hindu", "Buddhist", "Other religion"
      )),
      age_band = cut(age,
                     breaks = c(17, 29, 39, 49, 59, 69, Inf),
                     labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
                     right  = TRUE)
    )
}

############################################################
# Generate and save both datasets
############################################################

# All mock datasets live alongside the other preregistration data, where both
# preregistration.qmd and amendment_preregistration.qmd read them from.
out_dir <- "data/simulation_preregistration"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

human_data <- simulate_dataset(seed = 123)
saveRDS(human_data, file.path(out_dir, "human_data_preregistration.rds"))
write_csv(human_data, file.path(out_dir, "human_data_preregistration.csv"))
cat("Human data saved. N =", nrow(human_data), "\n")

llm_data <- simulate_dataset(seed = 456)
saveRDS(llm_data, file.path(out_dir, "llm_data_preregistration.rds"))
write_csv(llm_data, file.path(out_dir, "llm_data_preregistration.csv"))
cat("LLM data saved.   N =", nrow(llm_data), "\n")

############################################################
# Multi-team placeholder data (amendment preregistration)
#
# Five mock prediction "teams" for the cross-team leaderboard
# demonstration in amendment_preregistration.qmd. Rather than
# re-simulating from scratch, each team is a stratified bootstrap
# resample (within condition) of the single LLM placeholder
# dataset above — the same individual-level data, re-sampled for
# several teams. This keeps the design balanced and the teams
# comparable, differing only by sampling noise, and avoids any
# fresh mock-data simulation. ids are reassigned within team.
############################################################

resample_team <- function(base_data, seed) {
  set.seed(seed)
  base_data |>
    group_by(condition) |>
    slice_sample(prop = 1, replace = TRUE) |>
    ungroup() |>
    mutate(id = row_number())
}

team_seeds <- set_names(4561:4565, paste0("team_", 1:5))

llm_data_teams <- imap_dfr(team_seeds, function(seed, team_name) {
  resample_team(llm_data, seed = seed) |>
    mutate(team = team_name)
}) |>
  mutate(team = factor(team, levels = names(team_seeds)))

saveRDS(llm_data_teams, file.path(out_dir, "llm_data_preregistration_teams.rds"))
write_csv(llm_data_teams, file.path(out_dir, "llm_data_preregistration_teams.csv"))
cat("Multi-team LLM data saved. N =", nrow(llm_data_teams),
    "across", length(team_seeds), "teams\n")

############################################################
# Mock Tier-2 and Tier-3 submissions (amendment preregistration)
#
# The amendment demonstrates all three tier code paths. Tier 1
# is an individual-level dataset (the per-team data above, used
# directly). Tiers 2 and 3 are collapsed views, generated here
# rather than inside amendment_preregistration.qmd so that no
# mock-data simulation lives in the preregistration scripts.
#
# The collapse logic mirrors the submission-template example
# generator (silicon-sample-submission/maintainer/make_examples.R):
# a Tier-2 cell-level file (mean plus a 95% PI on the cell mean) and
# a Tier-3 effect-level file (ATE vs. control with a 95% PI).
############################################################

# Continuous outcomes the amendment leaderboard pools over.
outcomes_continuous <- c(
  "trust_multidimensional",
  "trust_post", "distrust_post", "funding_perceptions",
  "policy_role_mean", "inst_trust_mean",
  "belief_post", "concern_mean", "policy_general",
  "policy_specific_mean", "behavior_mean"
)

z95 <- qnorm(0.975)

# Collapse one team's individual-level data to raw cell statistics
# (condition x outcome). Internal helper: `sd` and `n` are used here to
# derive the submitted schemas (a cell-mean PI for Tier 2, an effect PI
# for Tier 3); they are not themselves part of any submission file.
cell_stats <- function(team_data) {
  team_data |>
    select(condition, all_of(outcomes_continuous)) |>
    pivot_longer(-condition, names_to = "outcome", values_to = "value") |>
    group_by(condition, outcome) |>
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd   = sd(value, na.rm = TRUE),
      n    = sum(!is.na(value)),
      .groups = "drop"
    )
}

# Mock Tier-2 submission: team_4 cell means with a widened 95% prediction
# interval on each mean standing in for the team's epistemic uncertainty.
# Submitted schema: condition, outcome, mean, pi_lower, pi_upper.
cells_t2 <- cell_stats(llm_data_teams |> filter(team == "team_4")) |>
  mutate(
    se_mean  = sd / sqrt(n),
    pi_lower = mean - z95 * 2 * se_mean,
    pi_upper = mean + z95 * 2 * se_mean
  ) |>
  select(condition, outcome, mean, pi_lower, pi_upper)
saveRDS(cells_t2, file.path(out_dir, "mock_submission_tier2_cells.rds"))

# Mock Tier-3 submission: team_5 ATEs vs. control with a widened 95%
# prediction interval standing in for a team's epistemic uncertainty.
# Derived from the same raw cell statistics, as in make_examples.R.
cells_team5  <- cell_stats(llm_data_teams |> filter(team == "team_5"))
control_cells <- cells_team5 |>
  filter(condition == "control") |>
  transmute(outcome, mean_ctrl = mean, sd_ctrl = sd, n_ctrl = n)

effects_t3 <- cells_team5 |>
  filter(condition != "control") |>
  left_join(control_cells, by = "outcome") |>
  mutate(
    ate      = mean - mean_ctrl,
    se       = sqrt(sd^2 / n + sd_ctrl^2 / n_ctrl),
    pi_lower = ate - z95 * 2 * se,
    pi_upper = ate + z95 * 2 * se
  ) |>
  select(condition, outcome, ate, pi_lower, pi_upper)
saveRDS(effects_t3, file.path(out_dir, "mock_submission_tier3_effects.rds"))

cat("Mock Tier-2 cells saved.   rows =", nrow(cells_t2), "\n")
cat("Mock Tier-3 effects saved. rows =", nrow(effects_t3), "\n")
