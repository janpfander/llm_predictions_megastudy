library(tidycensus)
library(tidyverse)

tidycensus::census_api_key(Sys.getenv("my_census_api"))

# --- Marginal quotas (not used) ---

# --- Gender (18+) ---
gender_quotas <- get_estimates(
  geography        = "state",
  product          = "characteristics",
  breakdown        = c("SEX", "AGE"),
  breakdown_labels = TRUE,
  vintage          = 2024
) |>
  filter(SEX %in% c("Male", "Female"), AGE >= 18) |>
  group_by(SEX) |>
  summarise(value = sum(value)) |>
  mutate(pct = value / sum(value))

# --- Race / Hispanic origin (18+) ---
race_quotas <- get_estimates(
  geography        = "state",
  product          = "characteristics",
  breakdown        = c("RACE", "HISP", "AGE"),
  breakdown_labels = TRUE,
  vintage          = 2024
) |>
  filter(HISP != "Both Hispanic Origins", AGE >= 18) |>
  mutate(
    category = case_when(
      HISP == "Hispanic"                                          ~ "Hispanic / Latino",
      RACE == "White alone"                                       ~ "White (non-Hispanic)",
      RACE == "Black alone"                                       ~ "Black / African American",
      RACE == "Asian alone"                                       ~ "Asian / Asian American",
      RACE %in% c("American Indian and Alaska Native alone",
                  "Native Hawaiian and Other Pacific Islander alone",
                  "Two or more races")                            ~ "Other"
    )
  ) |>
  group_by(category) |>
  summarise(value = sum(value)) |>
  mutate(pct = value / sum(value))

# --- Age (18+) ---
age_quotas <- get_estimates(
  geography        = "state",
  product          = "characteristics",
  breakdown        = "AGE",
  breakdown_labels = TRUE,
  vintage          = 2024
) |>
  group_by(AGE) |>
  summarise(value = sum(value)) |>
  filter(AGE >= 18) |>
  mutate(
    age_group = case_when(
      AGE <= 29 ~ "18-29",
      AGE <= 44 ~ "30-44",
      AGE <= 59 ~ "45-59",
      TRUE      ~ "60+"
    )
  ) |>
  group_by(age_group) |>
  summarise(value = sum(value)) |>
  mutate(pct = value / sum(value))

quota_table <- bind_rows(
  gender_quotas |>
    transmute(
      Variable     = "Gender",
      Category     = SEX,
      `Target (%)` = scales::percent(pct, accuracy = 0.1)
    ),
  age_quotas |>
    transmute(
      Variable     = "Age",
      Category     = age_group,
      `Target (%)` = scales::percent(pct, accuracy = 0.1)
    ),
  race_quotas |>
    transmute(
      Variable     = "Race / Ethnicity",
      Category     = category,
      `Target (%)` = scales::percent(pct, accuracy = 0.1)
    )
)

# --- Crossed quotas (with gender) ---

# gender × age
gender_age_quotas <- get_estimates(
  geography        = "state",
  product          = "characteristics",
  breakdown        = c("SEX", "AGE"),
  breakdown_labels = TRUE,
  vintage          = 2024
) |>
  filter(SEX %in% c("Male", "Female"), AGE >= 18) |>
  mutate(
    age_group = case_when(
      AGE <= 29 ~ "18-29",
      AGE <= 44 ~ "30-44",
      AGE <= 59 ~ "45-59",
      TRUE      ~ "60+"
    )
  ) |>
  group_by(SEX, age_group) |>
  summarise(value = sum(value)
            ) |>
  ungroup() |> 
  mutate(total_pop = sum(value)) |> 
  group_by(age_group) |>
  mutate(
    total_age = sum(value),
    pct_total = total_age / total_pop,
    pct_sex   = value / total_age
  ) |>
  ungroup()

# gender × race
gender_race_quotas <- get_estimates(
  geography        = "state",
  product          = "characteristics",
  breakdown        = c("SEX", "RACE", "HISP", "AGE"),
  breakdown_labels = TRUE,
  vintage          = 2024
) |>
  filter(SEX %in% c("Male", "Female"),
         HISP != "Both Hispanic Origins",
         AGE >= 18) |>
  mutate(
    category = case_when(
      HISP == "Hispanic"                                          ~ "Hispanic / Latino",
      RACE == "White alone"                                       ~ "White (non-Hispanic)",
      RACE == "Black alone"                                       ~ "Black / African American",
      RACE == "Asian alone"                                       ~ "Asian / Asian American",
      TRUE                                                        ~ "Other"
    )
  ) |>
  group_by(SEX, category) |>
  summarise(value = sum(value)
  ) |>
  ungroup() |> 
  mutate(total_pop = sum(value)) |>
  group_by(category) |>
  mutate(
    total_race = sum(value),
    pct_total  = total_race / total_pop,
    pct_sex    = value / total_race
  ) |>
  ungroup()

# make table
n_total <- 22000

quota_table_crossed <- bind_rows(
  gender_age_quotas |>
    select(age_group, SEX, pct_total, pct_sex) |>
    pivot_wider(names_from = SEX, values_from = pct_sex) |>
    mutate(
      n_total_cell = round(pct_total * n_total),
      n_male       = round(Male   * n_total_cell),
      n_female     = round(Female * n_total_cell)
    ) |>
    transmute(
      Variable     = "Age",
      Category     = age_group,
      `Total`      = sprintf("%d (%.1f%%)", n_total_cell, pct_total * 100),
      `Male`       = sprintf("%d (%.1f%%)", n_male,       Male      * 100),
      `Female`     = sprintf("%d (%.1f%%)", n_female,     Female    * 100)
    ) |>
    distinct(),
  gender_race_quotas |>
    select(category, SEX, pct_total, pct_sex) |>
    pivot_wider(names_from = SEX, values_from = pct_sex) |>
    mutate(
      n_total_cell = round(pct_total * n_total),
      n_male       = round(Male   * n_total_cell),
      n_female     = round(Female * n_total_cell)
    ) |>
    transmute(
      Variable     = "Race / Ethnicity",
      Category     = category,
      `Total`      = sprintf("%d (%.1f%%)", n_total_cell, pct_total * 100),
      `Male`       = sprintf("%d (%.1f%%)", n_male,       Male      * 100),
      `Female`     = sprintf("%d (%.1f%%)", n_female,     Female    * 100)
    ) |>
    distinct()
)

write_csv(quota_table_crossed, "data/quotas.csv")
