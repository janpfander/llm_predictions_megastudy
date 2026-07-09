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

# Shared MetBrewer "Juarez" palette and colour roles for the tournament
# (amendment) figures, so every cross-team plot uses one consistent scale.
tournament_palette <- met.brewer("Juarez", n = 8)
field_colours <- c(
  violin  = tournament_palette[[2]],   # blue — field density
  point   = tournament_palette[[8]],   # dark navy — one dot per approach
  marker  = "grey15",                  # field mean / median markers
  ceiling = tournament_palette[[1]],   # red — human–human ceiling
  null    = "grey70"                   # null / ideal reference line
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


# ---------------------------------------------------------------------------
# Amendment addition (Silicon Sample Tournament leaderboard)
#
# Forest-plot leaderboard. Not used by the original preregistration. Each row
# is a contribution (approach / team); one panel per evaluation metric. The
# human–human ceiling is pinned at the top with a dotted vertical reference
# line dropped from its value through the panel; an optional grey dotted null
# line is drawn per metric where one is meaningful (omitted otherwise).
#
# `data` is long: submission, metric (factor; its levels set the panel order
# and labels), value, lo, hi. `sort_metric` is the metric label used to order
# the rows. `null_lines` is a named numeric keyed by metric label.
# ---------------------------------------------------------------------------
plot_leaderboard_forest <- function(data,
                                    sort_metric,
                                    ceiling_label = "Human–human ceiling",
                                    null_lines    = NULL,
                                    point_colour  = field_colours[["violin"]]) {

  # Row order: contributions ranked by `sort_metric`, ceiling pinned on top.
  ranked <- data |>
    filter(as.character(metric) == sort_metric, submission != ceiling_label) |>
    arrange(value) |>
    pull(submission) |>
    unique()
  data <- data |>
    mutate(submission = factor(submission,
                               levels = c(ranked, ceiling_label)))

  ceiling_df <- data |>
    filter(submission == ceiling_label) |>
    distinct(metric, xint = value)

  p <- ggplot(data, aes(x = value, y = submission))

  if (!is.null(null_lines)) {
    null_df <- tibble(metric = factor(names(null_lines),
                                      levels = levels(data$metric)),
                      xint   = unname(null_lines)) |>
      filter(!is.na(metric))
    p <- p + geom_vline(data = null_df, aes(xintercept = xint),
                        linetype = "dotted", colour = "grey75", linewidth = 0.4)
  }

  p +
    geom_vline(data = ceiling_df, aes(xintercept = xint),
               linetype = "dotted", colour = "grey50", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0,
                   linewidth = 0.5, colour = "grey40") +
    geom_point(aes(fill = submission == ceiling_label),
               shape = 21, size = 2.4, colour = "white", stroke = 0.4) +
    scale_fill_manual(values = c(`TRUE` = "grey20", `FALSE` = point_colour),
                      guide = "none") +
    facet_wrap(~ metric, scales = "free_x", nrow = 1) +
    labs(x = NULL, y = NULL) +
    plot_theme
}


# ---------------------------------------------------------------------------
# Distribution of predictive quality across approaches. Not used by the
# original preregistration. The headline display of the amendment: one panel
# per evaluation metric, showing where the *field* of approaches lands rather
# than ranking them. Each approach is one dot over a one-sided (half) violin of
# the field's density; the human–human ceiling is a red cross. In pooled mode
# the field mean (diamond) and median (tick) are marked and an optional
# null/ideal line is drawn per metric. In by-group mode the same is shown per
# level of `x` (one row per outcome or per intervention).
#
# The half violin is built from a kernel density drawn as a polygon (rather
# than gghalves / ggdist, whose geoms do not render under faceted free scales
# on the current ggplot2). `data` is long: submission, metric (factor; its
# levels set the panel order and labels), value, and — when `x` is given — the
# grouping column named by `x`. `ref_lines` is a named numeric keyed by metric
# label.
# ---------------------------------------------------------------------------
plot_metric_distribution <- function(data,
                                     ref_lines     = NULL,
                                     ceiling_label = "Human–human ceiling",
                                     x             = NULL,
                                     width         = 0.42,
                                     seed          = 2026) {

  pts     <- data |> filter(submission != ceiling_label)
  ceiling <- data |> filter(submission == ceiling_label)

  # Category on the y axis: a single row in pooled mode, one row per level of
  # `x` (outcome / intervention) otherwise, kept in factor / sorted order.
  cat_levels <- if (is.null(x)) {
    ""
  } else if (is.factor(data[[x]])) {
    levels(factor(data[[x]]))
  } else {
    sort(unique(as.character(data[[x]])))
  }
  # Map each row to its integer y position. Use match() (not name indexing):
  # the pooled category is the empty string, and `c(""=1)[""]` returns NA in R,
  # which would silently drop every data layer.
  cat_of <- function(df) if (is.null(x)) rep("", nrow(df)) else as.character(df[[x]])

  pts     <- pts     |> mutate(.yi = match(cat_of(pts),     cat_levels))
  ceiling <- ceiling |> mutate(.yi = match(cat_of(ceiling), cat_levels))

  # One-sided (half) violin: kernel density of the field per metric × category,
  # scaled to `width` and drawn as a polygon sitting above the category line.
  violin <- pts |>
    group_by(metric, .yi) |>
    group_modify(function(s, key) {
      v <- s$value[is.finite(s$value)]
      if (length(unique(v)) < 2) return(tibble(value = numeric(), yo = numeric()))
      d <- density(v)
      tibble(value = c(d$x, rev(d$x)),
             yo    = key$.yi + c(d$y / max(d$y) * width, rep(0, length(d$x))))
    }) |>
    ungroup() |>
    mutate(pid = interaction(metric, .yi, drop = TRUE))

  # One dot per approach, jittered just below each category line. Seed the
  # jitter without disturbing the global RNG stream.
  old_seed <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv)
  set.seed(seed)
  pts <- pts |> mutate(.py = .yi - runif(n(), 0.06, 0.26))
  if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)

  summ <- pts |>
    group_by(metric, .yi) |>
    summarise(mean   = mean(value,   na.rm = TRUE),
              median = median(value, na.rm = TRUE), .groups = "drop")

  # Finite vertical span for the reference line, drawn with geom_segment: on the
  # current ggplot2, geom_vline blanks the whole panel under faceted free
  # scales, and an infinite (-Inf/Inf) segment does the same in pooled mode.
  y_lo <- 0.70
  y_hi <- length(cat_levels) + width + 0.10
  ref_df <- if (!is.null(ref_lines)) {
    tibble(metric = factor(names(ref_lines), levels = levels(data$metric)),
           xint   = unname(ref_lines), y = y_lo, yend = y_hi) |>
      filter(!is.na(metric))
  }

  p <- ggplot() +
    geom_polygon(data = violin, aes(value, yo, group = pid),
                 fill = field_colours[["violin"]], colour = NA, alpha = 0.30) +
    geom_point(data = pts, aes(value, .py),
               colour = field_colours[["point"]], alpha = 0.55, size = 0.9)

  if (is.null(x)) {
    p <- p +
      geom_point(data = summ, aes(median, .yi), shape = 124, size = 5,
                 colour = field_colours[["marker"]]) +
      geom_point(data = summ, aes(mean, .yi), shape = 23, size = 2.8,
                 fill = "white", colour = field_colours[["marker"]], stroke = 0.6)
  }

  p <- p +
    geom_point(data = ceiling, aes(value, .yi), shape = 4, size = 2,
               stroke = 0.9, colour = field_colours[["ceiling"]])

  # Reference line added last: on the current ggplot2 a segment/vline drawn
  # before the data layers blanks the panel under faceted free scales.
  if (!is.null(ref_df))
    p <- p + geom_segment(data = ref_df,
                          aes(x = xint, xend = xint, y = y, yend = yend),
                          linetype = "dotted", colour = field_colours[["null"]],
                          linewidth = 0.4)

  p +
    scale_y_continuous(breaks = seq_along(cat_levels), labels = cat_levels,
                       expand = expansion(add = c(0.35, width + 0.15))) +
    facet_wrap(~ metric, scales = "free_x", nrow = 1) +
    labs(x = NULL, y = NULL) +
    plot_theme +
    (if (is.null(x)) theme(axis.text.y = element_blank()) else NULL)
}


