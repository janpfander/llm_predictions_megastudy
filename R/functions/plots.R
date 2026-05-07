# Custom plot functions

# set general theme for plots
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = rel(1)),
    plot.subtitle      = element_text(face = "plain", size = rel(0.9), color = "grey70"),
    axis.title         = element_text(face = "bold", size = rel(0.85)),
    axis.title.x       = element_text(hjust = 0, margin = ggplot2::margin(t = 10)),
    axis.title.y       = element_text(hjust = 1, margin = ggplot2::margin(r = 10)),
    axis.text          = element_text(size = rel(0.8)),
    axis.ticks         = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(linewidth = 0.25, colour = "grey90"),
    panel.spacing      = unit(1, "lines"),
    strip.text         = element_text(face = "bold", size = rel(0.9), hjust = 0),
    strip.background   = element_rect(fill = "white", colour = NA),
    legend.position    = "top",
    legend.justification = "left",
    legend.title       = element_text(face = "bold", size = rel(0.8)),
    legend.text        = element_text(size = rel(0.8)),
    legend.key.size    = unit(0.7, "line"),
    legend.key         = element_blank(),
    legend.margin      = ggplot2::margin(t = -5, b = 0, l = 0, r = 0)
  )

# categorical moderator level colors
# used in moderator plots with categorical moderators
moderator_level_colors <- met.brewer("Juarez", n = 10)
partisan_colors <- c(
  "Republican" = moderator_level_colors[1],  # dark red
  "Democrat"   = moderator_level_colors[3],   # blue
  "Independent" = moderator_level_colors[9],  # dark green
  "Other"   = moderator_level_colors[2] # grey
)



plot_treatment_effects <- function(outcomes_group, main_model_results, data) {
  
  baseline_means <- data |>
    filter(condition == levels(condition)[1]) |>
    summarise(across(all_of(outcomes_group), ~ mean(.x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "outcome", values_to = "baseline_mean") |>
    mutate(label = paste0("Baseline mean (control): ", round(baseline_mean, 1)))
  
  main_model_results |>
    filter(outcome %in% outcomes_group) |>
    mutate(
      outcome_label = recode(outcome, !!!outcome_label_map),
      outcome_label = factor(outcome_label, 
                             levels = outcome_label_map[outcomes_group])
    ) |>
    ggplot(aes(x = estimate, y = reorder(condition, estimate))) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      height = 0, linewidth = 0.5, color = "grey30"
    ) +
    
    geom_point(size = 1, color = "grey20") +
    
    geom_text(
      aes(x = conf.high, label = significant_adjusted),
      nudge_x = 0.05, hjust = 0, vjust = 0.5, size = 3, fontface = "bold"
    ) +
    
    geom_text(
      aes(x = estimate, label = round(estimate, digits = 2)),
      vjust = -0.4, size = 3, check_overlap = TRUE
    ) +
    
    geom_text(
      data        = baseline_means,
      aes(x = Inf, y = -Inf, label = label),
      hjust = 1, vjust = -0.5, size = 2.5,
      color = "grey40", fontface = "italic", inherit.aes = FALSE
    ) +
    
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    scale_y_discrete(expand = expansion(add = c(0.5, 1.5))) +
    labs(x = "Treatment effect (OLS estimate)", y = NULL) +
    
    facet_wrap(~ outcome_label, scales = "free_x", ncol = 2) +
    
    plot_theme
}

plot_persistence <- function(outcomes_group,
                             followup_results_predicted,
                             followup_results_interaction) {
  
  # filter to requested outcomes
  predicted   <- followup_results_predicted   |> filter(outcome %in% outcomes_group)
  interaction <- followup_results_interaction |> filter(outcome %in% outcomes_group)
  
  # condition order based on experiment estimates
  # condition_order <- predicted |>
  #   filter(time == "experiment") |>
  #   group_by(outcome, condition) |>
  #   summarise(estimate = mean(estimate), .groups = "drop")
  
  # colors from tiepolo palette
  persistence_colors <- c(
    "Experiment" = moderator_level_colors[1],
    "Follow-up"  = moderator_level_colors[10]
  )
  
  predicted |>
    # left_join(condition_order, by = c("outcome", "condition"),
    #           suffix = c("", "_order")) |>
    mutate(
      #condition = reorder_within(condition, estimate_order, outcome),
      time      = factor(time,
                         levels = c("follow_up", "experiment"),
                         labels = c("Follow-up", "Experiment"))
    ) |>
    ggplot(aes(x = estimate, y = condition, color = time, shape = time)) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      position  = position_dodge(width = 0.6),
      height    = 0,
      linewidth = 0.5
    ) +
    
    geom_point(
      position = position_dodge(width = 0.6),
      size     = 1
    ) +
    
    geom_text(
      aes(x = conf.high, label = significant_adjusted),
      position    = position_dodge(width = 0.6),
      hjust       = -0.2,
      vjust       = 0.5,
      size        = 3,
      fontface    = "bold",
      show.legend = FALSE
    ) +
    
    geom_text(
      data = interaction |>
        # left_join(condition_order, by = c("outcome", "condition"),
        #           suffix = c("", "_order")) |>
        mutate(
          #condition = reorder_within(condition, estimate_order, outcome),
          sig_label = case_when(
            p.value_adjusted < .001 ~ "***",
            p.value_adjusted < .01  ~ "**",
            p.value_adjusted < .05  ~ "*",
            TRUE                    ~ "n.s."
          )
        ),
      aes(x = Inf, y = condition, label = paste0("Diff: ", sig_label)),
      hjust       = 1.2,
      vjust       = 0,
      size        = 3,
      fontface    = "bold",
      color       = "grey20",
      inherit.aes = FALSE
    ) +
    
    scale_y_reordered() +
    
    scale_color_manual(values = persistence_colors) +
    
    scale_shape_manual(values = c("Experiment" = 16, "Follow-up" = 17)) +
    
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.20))) +
    
    facet_wrap(~ outcome, 
               scales = "free_x", 
               ncol = 2
               ) +
    
    labs(
      x     = "Treatment effect (OLS estimate)",
      y     = NULL,
      color = NULL,
      shape = NULL
    ) +
    
    plot_theme
}

# appendix moderator plot — categorical moderator
# one plot per moderator, panels = outcomes, predictions with interaction stars
plot_moderator_appendix_categorical <- function(moderator_name,
                                                outcomes_group,
                                                moderator_results_predicted,
                                                data) {
  plots <- outcomes_group |>
    map(function(o) {
      plot_data <- moderator_results_predicted |>
        filter(moderator == moderator_name, outcome == o) |>
        mutate(
          condition = factor(condition, levels = paste0("intervention_", 1:20)),
          star_pos  = conf.high * 1.05
        )
      
      ggplot(plot_data, aes(x = condition,
                            y = estimate,
                            color = moderator_level)) +
        
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
        
        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          width     = 0,
          linewidth = 0.5,
          alpha     = 0.7,
          position  = position_dodge(width = 0.5)
        ) +
        
        geom_point(
          size     = 2,
          alpha    = 0.7,
          position = position_dodge(width = 0.5)
        ) +
        
        geom_text(
          aes(y = star_pos, label = significant_adjusted, group = moderator_level),
          hjust       = 0,
          vjust       = 0.8,
          size        = 3,
          fontface    = "bold",
          color       = "grey20",
          show.legend = FALSE,
          position    = position_dodge(width = 0.5)
        ) +
        
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
        
        scale_color_manual(
          values = moderator_level_colors,
          name   = NULL
        ) +
        
        coord_flip() +
        
        labs(
          title = o,
          y     = "Treatment effect estimate",
          x     = NULL
        ) +
        
        plot_theme 
    })
  
  wrap_plots(plots, ncol = 2) +
    plot_layout(axes = "collect_y", guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "top")
}

# appendix moderator plot — continuous moderator
plot_moderator_appendix_continuous <- function(moderator_name,
                                               outcomes_group,
                                               moderator_results) {
  plots <- outcomes_group |>
    map(function(o) {
      plot_data <- moderator_results |>
        filter(moderator == moderator_name, outcome == o) |>
        mutate(
          condition = factor(condition, levels = paste0("intervention_", 1:20)),
          star_pos  = conf.high * 1.05
        )
      
      ggplot(plot_data, aes(x = condition, y = estimate)) +
        
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
        
        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          width     = 0,
          linewidth = 0.5,
          alpha     = 0.7,
          color     = "grey30"
        ) +
        
        geom_point(
          size  = 2,
          alpha = 0.7,
          color = "grey20"
        ) +
        
        geom_text(
          aes(y = star_pos, label = significant_adjusted),
          hjust       = 0,
          vjust       = 0.8,
          size        = 3,
          fontface    = "bold",
          color       = "grey20",
          show.legend = FALSE
        ) +
        
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
        
        coord_flip() +
        
        labs(
          title = o,
          y     = "Interaction estimate (per unit increase in moderator)",
          x     = NULL
        ) +
        
        plot_theme
    })
  
  wrap_plots(plots, ncol = 2) +
    plot_layout(axes = "collect_y")
}

plot_items_model <- function(predicted_effects,
                             interaction_effects,
                             item_colors,
                             item_labels,
                             title_predicted   = "Effects within items",
                             title_interaction = "Differences across items",
                             ref_item_label    = NULL) {
  
  p_predicted <- predicted_effects |>
    mutate(
      condition = factor(condition, levels = paste0("intervention_", 1:20)),
      star_pos  = conf.high * 1.05,
      item      = factor(item, levels = names(item_labels))
    ) |>
    ggplot(aes(x = condition, y = estimate, color = item)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0, linewidth = 0.5, alpha = 0.7,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      size = 2, alpha = 0.7,
      position = position_dodge(width = 0.6)
    ) +
    geom_text(
      aes(y = star_pos, label = significant_adjusted, group = item),
      hjust = 0, vjust = 0.8, size = 3, fontface = "bold",
      color = "grey20", show.legend = FALSE,
      position = position_dodge(width = 0.6)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
    scale_color_manual(values = item_colors, labels = item_labels, name = NULL) +
    coord_flip() +
    labs(title = title_predicted, y = "Treatment effect estimate", x = NULL) +
    plot_theme 
  
  p_interactions <- interaction_effects |>
    mutate(
      condition = factor(condition, levels = paste0("intervention_", 1:20)),
      star_pos  = conf.high * 1.05,
      item      = factor(item, levels = names(item_labels)[-1])
    ) |>
    ggplot(aes(x = condition, y = estimate, color = item)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0, linewidth = 0.5, alpha = 0.7,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      size = 2, alpha = 0.7,
      position = position_dodge(width = 0.6)
    ) +
    geom_text(
      aes(y = star_pos, label = significant_adjusted, group = item),
      hjust = 0, vjust = 0.8, size = 3, fontface = "bold",
      color = "grey20", show.legend = FALSE,
      position = position_dodge(width = 0.6)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
    scale_color_manual(
      values = item_colors[-1],
      labels = item_labels[-1],
      name   = NULL
    ) +
    coord_flip() +
    labs(
      title = title_interaction,
      y     = paste0("Interaction estimate (relative to ",
                     if (!is.null(ref_item_label)) ref_item_label else "reference item",
                     ")"),
      x     = NULL
    ) +
    guides(color = "none") + 
    plot_theme 
  
  p_predicted + p_interactions +
    plot_layout(axes = "collect", guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "top")
}


