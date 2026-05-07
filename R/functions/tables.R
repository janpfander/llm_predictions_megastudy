# Custom functions for tables

make_table_treatment_effects <- function(outcomes_group,
                                         main_model_results,
                                         title           = NULL,
                                         outcome_labels  = NULL,
                                         binary_outcomes = NULL,
                                         binary_predictions = NULL) {
  
  # only keep outcomes that have been estimated
  available_outcomes <- unique(main_model_results$outcome)
  outcomes_group     <- outcomes_group[outcomes_group %in% available_outcomes]
  
  if (length(outcomes_group) == 0) {
    message("No estimated outcomes found.")
    return(NULL)
  }
  
  # default outcome labels
  if (is.null(outcome_labels)) {
    outcome_labels <- setNames(outcomes_group, outcomes_group)
  } else {
    outcome_labels <- outcome_labels[names(outcome_labels) %in% outcomes_group]
  }
  
  # default title
  if (is.null(title)) {
    title <- "Treatment effects"
  }
  
  # build cell content — join binary predictions if provided
  results <- main_model_results |>
    filter(outcome %in% outcomes_group)
  
  if (!is.null(binary_predictions) && !is.null(binary_outcomes)) {
    binary_pp <- binary_predictions |>
      filter(outcome %in% binary_outcomes) |>
      select(condition, outcome, ame = estimate)
    
    results <- results |>
      left_join(binary_pp, by = c("condition", "outcome"))
  } else {
    results <- results |> mutate(ame = NA_real_)
  }
  
  table_data <- results |>
    mutate(
      cell = case_when(
        outcome %in% (binary_outcomes %||% character(0)) & !is.na(ame) ~
          sprintf("%.2f%s / %.1f%% (p = %.3f, p_adj = %.3f)",
                  estimate,
                  ifelse(is.na(significant_adjusted), "", significant_adjusted),
                  ame * 100,
                  p.value,
                  p.value_adjusted),
        TRUE ~
          sprintf("%.2f%s (p = %.3f, p_adj = %.3f)",
                  estimate,
                  ifelse(is.na(significant_adjusted), "", significant_adjusted),
                  p.value,
                  p.value_adjusted)
      ),
      outcome = factor(outcome, levels = outcomes_group)
    ) |>
    arrange(condition) |>
    select(condition, outcome, cell) |>
    pivot_wider(names_from = outcome, values_from = cell)
  
  # build gt table
  gt_table <- table_data |>
    gt(rowname_col = "condition") |>
    cols_label(
      .list = setNames(
        as.list(unname(outcome_labels)),
        names(outcome_labels)
      )
    ) |>
    tab_header(
      title    = title,
      subtitle = "Estimate* (unadjusted p-value, BH-adjusted p-value)"
    ) |>
    tab_footnote(
      footnote = "* p_adj < .05; ** p_adj < .01; *** p_adj < .001 (BH-adjusted). HC2 robust standard errors."
    ) |>
    tab_style(
      style     = cell_text(size = px(9)),
      locations = cells_body()
    ) |>
    tab_options(
      table.font.size         = 10,
      column_labels.font.size = 9,
      data_row.padding        = px(3),
      latex.use_longtable     = TRUE
    )
  
  # add binary outcome footnote
  if (!is.null(binary_outcomes) && any(binary_outcomes %in% outcomes_group)) {
    binary_in_table <- binary_outcomes[binary_outcomes %in% outcomes_group]
    gt_table <- gt_table |>
      tab_footnote(
        footnote  = "For binary outcomes, estimates are log-odds / average marginal effect in percentage points (pp). P-values are based on log-odds.",
        locations = cells_column_labels(columns = any_of(binary_in_table))
      ) 
  }
  
  gt_table
}

make_moderator_table <- function(moderator_name,
                                 outcomes_group,
                                 outcome_labels  = NULL,
                                 moderator_results,
                                 title           = NULL,
                                 binary_outcomes = NULL) {
  
  # only keep outcomes that have been estimated
  available_outcomes <- moderator_results |>
    filter(moderator == moderator_name) |>
    pull(outcome) |>
    unique()
  
  outcomes_group <- outcomes_group[outcomes_group %in% available_outcomes]
  
  if (length(outcomes_group) == 0) {
    message("No estimated outcomes found for moderator: ", moderator_name)
    return(NULL)
  }
  
  # default outcome labels
  if (is.null(outcome_labels)) {
    outcome_labels <- setNames(outcomes_group, outcomes_group)
  } else {
    outcome_labels <- outcome_labels[names(outcome_labels) %in% outcomes_group]
  }
  
  # default title
  if (is.null(title)) {
    title <- paste0("Moderation of treatment effects by ", moderator_name)
  }
  
  # detect whether moderator is continuous or categorical
  is_continuous <- moderator_results |>
    filter(moderator == moderator_name) |>
    pull(moderator_level) |>
    na_if("") |>
    is.na() |>
    all()
  
  # build cell content
  table_data <- moderator_results |>
    filter(moderator == moderator_name,
           outcome %in% outcomes_group) |>
    mutate(
      cell = sprintf(
        "%.2f%s (p = %.3f, p_adj = %.3f)",
        estimate,
        ifelse(is.na(significant_adjusted), "", significant_adjusted),
        p.value,
        p.value_adjusted
      ),
      outcome         = factor(outcome, levels = outcomes_group),
      moderator_level = if (is_continuous) moderator_name else moderator_level
    ) |>
    select(condition, moderator_level, outcome, cell) |>
    pivot_wider(names_from = outcome, values_from = cell) |>
    arrange(condition, moderator_level)
  
  # build gt table
  gt_table <- table_data |>
    gt(rowname_col = "moderator_level",
       groupname_col = "condition") |>
    cols_label(
      .list = setNames(
        as.list(unname(outcome_labels)),
        names(outcome_labels)
      )
    ) |>
    tab_header(
      title    = title,
      subtitle = "Estimate* (unadjusted p-value, BH-adjusted p-value)"
    ) |>
    tab_footnote(
      footnote = "* p_adj < .05; ** p_adj < .01; *** p_adj < .001 (BH-adjusted). Stars based on adjusted p-values."
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style     = cell_text(size = px(9)),
      locations = cells_body()
    ) |>
    tab_options(
      table.font.size         = 10,
      column_labels.font.size = 9,
      row_group.font.size     = 10,
      data_row.padding        = px(3)
    )
  
  # add binary outcome footnote
  if (!is.null(binary_outcomes) && any(binary_outcomes %in% outcomes_group)) {
    binary_in_table <- binary_outcomes[binary_outcomes %in% outcomes_group]
    gt_table <- gt_table |>
      tab_footnote(
        footnote  = "Estimates are odds ratios from logistic regression.",
        locations = cells_column_labels(columns = any_of(binary_in_table))
      )
  }
  
  gt_table
}

plot_intercorrelation_table <- function(items, item_labels) {
  
  control_data |>
    select(all_of(items)) |>
    rename_with(~ unname(item_labels[.x])) |>
    correlation::correlation() |>
    summary(redundant = FALSE) |>
    as_tibble() |>
    rename(` ` = 1) |>
    rounded_numbers() |> 
    mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x)))) |>
    tt(width = 1) |>
    style_tt(align = "l") |>
    style_tt(i = 0, bold = TRUE) |>
    style_tt(fontsize = 0.85)
}

make_persistence_table <- function(outcomes_group,
                                   followup_results_interaction,
                                   followup_results_predicted,
                                   title           = NULL,
                                   outcome_labels  = NULL,
                                   binary_outcomes = NULL) {
  
  # only keep outcomes that have been estimated
  available_outcomes <- unique(followup_results_interaction$outcome)
  outcomes_group     <- outcomes_group[outcomes_group %in% available_outcomes]
  
  if (length(outcomes_group) == 0) {
    message("No estimated outcomes found.")
    return(NULL)
  }
  
  # default outcome labels
  if (is.null(outcome_labels)) {
    outcome_labels <- setNames(outcomes_group, outcomes_group)
  } else {
    outcome_labels <- outcome_labels[names(outcome_labels) %in% outcomes_group]
  }
  
  # default title
  if (is.null(title)) {
    title <- "Persistence of treatment effects"
  }
  
  # interaction rows
  interaction_rows <- followup_results_interaction |>
    filter(outcome %in% outcomes_group) |>
    mutate(
      row_label = "Interaction",
      cell = sprintf(
        "%.2f%s (p = %.3f, p_adj = %.3f)",
        estimate,
        ifelse(is.na(significant_adjusted), "", significant_adjusted),
        p.value,
        p.value_adjusted
      ),
      outcome = factor(outcome, levels = outcomes_group)
    ) |>
    select(condition, row_label, outcome, cell)
  
  # predicted rows — one per wave
  predicted_rows <- followup_results_predicted |>
    filter(outcome %in% outcomes_group) |>
    mutate(
      row_label = case_when(
        time == "experiment" ~ "Experiment",
        time == "follow_up"  ~ "Follow-up",
        TRUE                 ~ time
      ),
      cell = sprintf(
        "%.2f%s (p = %.3f, p_adj = %.3f)",
        estimate,
        ifelse(is.na(significant_adjusted), "", significant_adjusted),
        p.value,
        p.value_adjusted
      ),
      outcome = factor(outcome, levels = outcomes_group)
    ) |>
    select(condition, row_label, outcome, cell)
  
  # combine and pivot
  table_data <- bind_rows(predicted_rows, interaction_rows) |>
    mutate(
      row_label = factor(row_label,
                         levels = c("Experiment", "Follow-up", "Interaction"))
    ) |>
    pivot_wider(names_from = outcome, values_from = cell) |>
    arrange(condition, row_label)
  
  # build gt table
  gt_table <- table_data |>
    gt(rowname_col = "row_label",
       groupname_col = "condition") |>
    cols_label(
      .list = setNames(
        as.list(unname(outcome_labels)),
        names(outcome_labels)
      )
    ) |>
    tab_header(
      title    = title,
      subtitle = "Estimate* (unadjusted p-value, BH-adjusted p-value)"
    ) |>
    tab_source_note(
      source_note = "* p_adj < .05; ** p_adj < .01; *** p_adj < .001 (BH-adjusted). 'Interaction' tests whether the treatment effect changed between experiment and follow-up. 'Experiment' and 'Follow-up' show predicted effects within each wave."
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_stub(rows = row_label == "Interaction")
    ) |>
    tab_style(
      style     = cell_text(size = px(9)),
      locations = cells_body()
    ) |>
    tab_options(
      table.font.size         = 10,
      column_labels.font.size = 9,
      row_group.font.size     = 10,
      data_row.padding        = px(3)
    )
  
  # add binary outcome footnote
  if (!is.null(binary_outcomes) && any(binary_outcomes %in% outcomes_group)) {
    binary_in_table <- binary_outcomes[binary_outcomes %in% outcomes_group]
    gt_table <- gt_table |>
      tab_footnote(
        footnote  = "Estimates are average marginal effects on the probability scale.",
        locations = cells_column_labels(columns = any_of(binary_in_table))
      ) 
  }
  
  gt_table
}


