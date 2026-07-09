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
                                    ceiling_label = "Human replication",
                                    null_lines    = NULL,
                                    point_colour  = field_colours[["violin"]]) {

  # Row order: contributions ranked by `sort_metric`, ceiling pinned on top.
  # Rows with NA on the sort metric (e.g. the no-effect floor baseline, whose
  # correlation is undefined) sit at the bottom rather than being dropped.
  ranked <- data |>
    filter(as.character(metric) == sort_metric, submission != ceiling_label) |>
    arrange(desc(is.na(value)), value) |>
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
# the field's density (gghalves::geom_half_violin); the human–human ceiling is a
# red cross. In pooled mode the field mean (diamond) and median (tick) are
# marked and an optional null/ideal line is drawn per metric. In by-group mode
# the same is shown per level of `x` (one row per outcome or per intervention).
#
# Built with gghalves; the layout is horizontal via coord_flip(). Under
# coord_flip on the current ggplot2, facet scales = "free_x" frees each panel's
# value axis while keeping the shared category axis labelled only once (left);
# "free_y" wrongly shares the value axis. The dots use geom_jitter rather than
# gghalves::geom_half_point, which errors on this ggplot2.
#
# `data` is long: submission, metric (factor; its levels set the panel order and
# labels), value, and — when `x` is given — the grouping column named by `x`.
# `ref_lines` is a named numeric keyed by metric label.
# ---------------------------------------------------------------------------
plot_metric_distribution <- function(data,
                                     ref_lines     = NULL,
                                     ceiling_label = "Human replication",
                                     x             = NULL,
                                     jitter        = 0.12,
                                     seed          = 2026,
                                     value_trans   = NULL,
                                     value_breaks  = waiver(),
                                     mean_ci       = FALSE) {

  pts     <- data |> filter(submission != ceiling_label)
  ceiling <- data |> filter(submission == ceiling_label)

  # Category on the (flipped) y axis: a single blank row in pooled mode, one row
  # per level of `x` (outcome / intervention) otherwise, in factor / sorted order.
  cat_levels <- if (is.null(x)) {
    ""
  } else if (is.factor(data[[x]])) {
    levels(factor(data[[x]]))
  } else {
    sort(unique(as.character(data[[x]])))
  }
  cat_of <- function(df) {
    factor(if (is.null(x)) rep("", nrow(df)) else as.character(df[[x]]),
           levels = cat_levels)
  }
  pts     <- pts     |> mutate(.cat = cat_of(pts))
  ceiling <- ceiling |> mutate(.cat = cat_of(ceiling))

  summ <- pts |>
    group_by(metric, .cat) |>
    summarise(mean   = mean(value,   na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              ci     = qnorm(0.975) * sd(value, na.rm = TRUE) /
                       sqrt(sum(!is.na(value))),
              .groups = "drop")

  ref_df <- if (!is.null(ref_lines)) {
    tibble(metric = factor(names(ref_lines), levels = levels(data$metric)),
           yint   = unname(ref_lines)) |>
      filter(!is.na(metric))
  }

  # Reproducible point jitter without disturbing the global RNG stream.
  old_seed <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv)
  set.seed(seed)

  p <- ggplot(pts, aes(.cat, value)) +
    gghalves::geom_half_violin(side = "r", width = 0.9,
                               fill = field_colours[["violin"]],
                               colour = NA, alpha = 0.35) +
    geom_jitter(width = jitter, height = 0, colour = field_colours[["point"]],
                alpha = 0.55, size = 0.9)

  if (is.null(x)) {
    if (mean_ci)
      p <- p + geom_errorbar(data = summ,
                             aes(.cat, ymin = mean - ci, ymax = mean + ci),
                             width = 0.1, linewidth = 0.5,
                             colour = field_colours[["marker"]],
                             inherit.aes = FALSE)
    p <- p +
      geom_point(data = summ, aes(.cat, median), shape = 124, size = 5,
                 colour = field_colours[["marker"]]) +
      geom_point(data = summ, aes(.cat, mean), shape = 23, size = 2.8,
                 fill = "white", colour = field_colours[["marker"]], stroke = 0.6)
  } else if (mean_ci) {
    # one field mean and its 95% CI per group row, nudged just below the row's
    # dots/half-violin so each distribution carries a legible central summary
    # with a clear gap from the cloud above it.
    nudge <- position_nudge(x = -0.22)
    p <- p +
      geom_errorbar(data = summ,
                    aes(.cat, ymin = mean - ci, ymax = mean + ci),
                    width = 0.16, linewidth = 0.5, position = nudge,
                    colour = field_colours[["marker"]], inherit.aes = FALSE) +
      geom_point(data = summ, aes(.cat, mean), shape = 23, size = 2.4,
                 fill = "white", colour = field_colours[["marker"]],
                 stroke = 0.6, position = nudge, inherit.aes = FALSE)
  }

  p <- p +
    geom_point(data = ceiling, aes(.cat, value), shape = 4, size = 2,
               stroke = 0.9, colour = field_colours[["ceiling"]])

  if (!is.null(ref_df))
    p <- p + geom_hline(data = ref_df, aes(yintercept = yint),
                        linetype = "dotted", colour = field_colours[["null"]],
                        linewidth = 0.4)

  if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)

  # value_trans (e.g. "log2") transforms the value axis — the y aesthetic,
  # which coord_flip() displays horizontally. value_breaks are in data space.
  if (!is.null(value_trans))
    p <- p + scale_y_continuous(transform = value_trans, breaks = value_breaks)

  p <- p +
    facet_wrap(~ metric, scales = "free_x", nrow = 1) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    plot_theme

  if (is.null(x))
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  p
}


# ---------------------------------------------------------------------------
# Benchmark headline panels (one key metric per question; the full ladders
# live in tables). Not used by the original preregistration.
# ---------------------------------------------------------------------------

# Predicted vs. human estimates, all approaches pooled: one point per
# approach x pair, dashed identity line, the human replication reference as
# red crosses. `pairs_df` is stacked pairs with a `submission` column and
# estimate_l / estimate_h. symmetric = TRUE centres both axes on zero with a
# shared range and a fixed aspect ratio, so the identity line runs at 45°.
plot_pred_scatter <- function(pairs_df,
                              ref_label   = "Human replication",
                              x_lab       = "Predicted effect",
                              y_lab       = "Human effect",
                              tag         = NULL,
                              point_alpha = 0.3,
                              point_size  = 0.8,
                              ref_alpha   = 1,
                              ref_size    = 1.4,
                              symmetric   = FALSE) {
  pts <- pairs_df |> filter(submission != ref_label)
  ref <- pairs_df |> filter(submission == ref_label)
  p <- ggplot(pts, aes(estimate_l, estimate_h)) +
    geom_abline(linetype = "dashed", colour = "grey55", linewidth = 0.4) +
    geom_point(colour = field_colours[["point"]], alpha = point_alpha,
               size = point_size) +
    geom_point(data = ref, shape = 4, size = ref_size, stroke = 0.7,
               alpha = ref_alpha, colour = field_colours[["ceiling"]]) +
    labs(x = x_lab, y = y_lab, tag = tag) +
    plot_theme
  if (symmetric) {
    m <- max(abs(c(pairs_df$estimate_l, pairs_df$estimate_h)), na.rm = TRUE)
    p <- p + coord_fixed(xlim = c(-m, m), ylim = c(-m, m))
  }
  p
}

# Overlay layers for a plot_pred_scatter(): the average calibration regression
# of human on predicted for the field (per-approach OLS coefficients averaged,
# a between-approach 95% CI) and for the human replication reference (single
# fit, model CI), drawn as coloured lines, with a corner readout of correlation
# (r, adj.) and the calibration coefficients (beta, alpha with CI). Returns a
# list of layers to add with `+`. The r / adj. values are passed in from the
# established metric functions (leaderboard, signed_metrics) so they match the
# ranked panel; the coefficients are computed here from the plotted pairs.
calibration_overlay <- function(pairs_df, r_field, radj_field,
                                r_human, radj_human,
                                ref_label = "Human replication") {
  coefs <- function(df, across) {
    per <- df |>
      group_by(submission) |>
      group_modify(~ broom::tidy(lm(estimate_h ~ estimate_l, data = .x),
                                 conf.int = TRUE)) |>
      ungroup() |>
      mutate(term = if_else(term == "(Intercept)", "alpha", "beta"))
    if (across)
      per |> group_by(term) |>
        summarise(est = mean(estimate),
                  lo  = est - qnorm(0.975) * sd(estimate) / sqrt(dplyr::n()),
                  hi  = est + qnorm(0.975) * sd(estimate) / sqrt(dplyr::n()),
                  .groups = "drop")
    else
      per |> transmute(term, est = estimate, lo = conf.low, hi = conf.high)
  }
  get <- function(tb, tm) tb |> filter(term == tm)
  fc <- coefs(pairs_df |> filter(submission != ref_label), across = TRUE)
  hc <- coefs(pairs_df |> filter(submission == ref_label), across = FALSE)

  ci  <- function(r) sprintf("%.2f [%.2f, %.2f]", r$est, r$lo, r$hi)
  lab <- function(name, r, radj, cf) {
    # the noise-corrected r is structurally undefined for some pair sets (its
    # true-variance guard); drop the "(adj.)" clause rather than print NA.
    rpart <- if (is.na(radj)) sprintf("r = %.2f", r)
             else sprintf("r = %.2f (adj. %.2f)", r, radj)
    sprintf("%s:  %s\n   β = %s,  α = %s",
            name, rpart, ci(get(cf, "beta")), ci(get(cf, "alpha")))
  }

  m <- max(abs(c(pairs_df$estimate_l, pairs_df$estimate_h)), na.rm = TRUE)
  list(
    geom_abline(intercept = get(fc, "alpha")$est, slope = get(fc, "beta")$est,
                colour = field_colours[["point"]], linewidth = 0.8),
    geom_abline(intercept = get(hc, "alpha")$est, slope = get(hc, "beta")$est,
                colour = field_colours[["ceiling"]], linewidth = 0.8),
    annotate("text", x = -m, y = 0.99 * m, hjust = 0, vjust = 1, size = 2.8,
             lineheight = 0.95, colour = field_colours[["point"]],
             label = lab("Field", r_field, radj_field, fc)),
    annotate("text", x = -m, y = 0.68 * m, hjust = 0, vjust = 1, size = 2.8,
             lineheight = 0.95, colour = field_colours[["ceiling"]],
             label = lab("Human", r_human, radj_human, hc))
  )
}

# Single-metric ranking with an integrated field-summary row: approaches
# ordered by `metric_main` (filled points, cluster-bootstrap CIs as bars), the
# companion metric as open points on the same rows (no bars), the human
# replication reference pinned on top, and an "All approaches" row at the
# bottom — a meta-analytic-style summary carrying the field's density (drawn
# upward into a blank spacer row), the field mean as a diamond, and a 95% CI
# on that mean. `forest_df` is the long cluster_boot output: submission,
# metric, value, lo, hi.
plot_field_forest_single <- function(forest_df,
                                     metric_main      = "Pearson r",
                                     metric_companion = "Pearson r (adj.)",
                                     ref_label        = "Human replication",
                                     field_label      = "All approaches",
                                     null_line        = 0,
                                     x_lab            = "Pearson r",
                                     tag              = NULL) {
  main <- forest_df |> filter(as.character(metric) == metric_main)
  comp <- forest_df |>
    filter(as.character(metric) == metric_companion, !is.na(value))

  ranked <- main |>
    filter(submission != ref_label) |>
    arrange(desc(is.na(value)), value) |>
    pull(submission)
  # summary row at the bottom, blank spacer row above it for its density
  lvls <- c(field_label, "", ranked, ref_label)
  main <- main |> mutate(submission = factor(submission, levels = lvls))
  comp <- comp |> mutate(submission = factor(submission, levels = lvls))

  ref_val <- main |> filter(submission == ref_label) |> pull(value)
  rng <- range(c(main$value, main$lo, main$hi, comp$value, null_line),
               na.rm = TRUE)

  field_vals <- main |>
    filter(submission != ref_label) |>
    pull(value) |>
    na.omit()
  dens <- density(field_vals, from = rng[1], to = rng[2])
  # numeric y positions index the discrete rows: the polygon rises from the
  # summary row (position 1) through the spacer (position 2)
  dens_poly <- tibble(
    x = c(dens$x[1], dens$x, dens$x[length(dens$x)]),
    y = 1 + c(0, dens$y / max(dens$y) * 1.6, 0)
  )
  comp_vals <- comp |>
    filter(submission != ref_label) |>
    pull(value) |>
    na.omit()
  field_summ <- tibble(
    mean      = mean(field_vals),
    ci        = qnorm(0.975) * sd(field_vals) / sqrt(length(field_vals)),
    mean_comp = if (length(comp_vals)) mean(comp_vals) else NA_real_
  )

  ggplot(main, aes(value, submission)) +
    geom_vline(xintercept = null_line, linetype = "dotted",
               colour = field_colours[["null"]], linewidth = 0.4) +
    geom_vline(xintercept = ref_val, linetype = "dotted", colour = "grey50",
               linewidth = 0.4) +
    geom_polygon(data = dens_poly, aes(x, y),
                 fill = field_colours[["violin"]], colour = NA, alpha = 0.55,
                 inherit.aes = FALSE) +
    geom_errorbarh(data = field_summ,
                   aes(xmin = mean - ci, xmax = mean + ci, y = 1),
                   height = 0.35, linewidth = 0.5,
                   colour = field_colours[["marker"]], inherit.aes = FALSE) +
    geom_point(data = field_summ |> filter(!is.na(mean_comp)),
               aes(mean_comp, 1), shape = 21, size = 2, fill = "white",
               colour = field_colours[["point"]], stroke = 0.5,
               inherit.aes = FALSE) +
    geom_point(data = field_summ, aes(mean, 1), shape = 23, size = 3,
               fill = "white", colour = field_colours[["marker"]],
               stroke = 0.7, inherit.aes = FALSE) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.45,
                   colour = "grey40") +
    geom_point(data = comp, shape = 21, size = 2, fill = "white",
               colour = field_colours[["point"]], stroke = 0.5) +
    geom_point(aes(fill = submission == ref_label), shape = 21, size = 2.2,
               colour = "white", stroke = 0.4) +
    scale_fill_manual(values = c(`TRUE`  = field_colours[["ceiling"]],
                                 `FALSE` = field_colours[["point"]]),
                      guide = "none") +
    scale_y_discrete(drop = FALSE) +
    coord_cartesian(xlim = rng) +
    labs(x = x_lab, y = NULL, tag = tag) +
    plot_theme
}

# Response densities in one condition, one facet per outcome: every Tier-1
# approach a thin line, the human reference (Human 1) bold, the replication
# half (Human 2) dashed red. `annotations` (optional) is a tibble with
# `outcome` (raw names) and `label`, printed in each facet's top corner —
# used to stamp the key metric onto the panel it describes.
plot_density_overlay <- function(human_ref, human_rep, team_data, outcomes,
                                 condition_val, outcome_labels = NULL,
                                 annotations = NULL, team_col = "team",
                                 x_lab = "Response", ncol = 4) {
  relabel <- function(df) df |>
    mutate(outcome = factor(
      if (is.null(outcome_labels)) outcome else unname(outcome_labels[outcome]),
      levels = if (is.null(outcome_labels)) outcomes
               else unname(outcome_labels[outcomes])
    ))
  longify <- function(d) d |>
    filter(condition == condition_val) |>
    select(any_of(team_col), all_of(outcomes)) |>
    pivot_longer(all_of(outcomes), names_to = "outcome",
                 values_to = "value") |>
    relabel()

  p <- ggplot(longify(team_data), aes(value)) +
    geom_density(aes(group = .data[[team_col]]),
                 colour = scales::alpha(field_colours[["point"]], 0.22),
                 linewidth = 0.3, na.rm = TRUE) +
    geom_density(data = longify(human_rep), colour = field_colours[["ceiling"]],
                 linetype = "dashed", linewidth = 0.55, na.rm = TRUE) +
    geom_density(data = longify(human_ref), colour = "grey15",
                 linewidth = 0.7, na.rm = TRUE) +
    facet_wrap(~ outcome, scales = "free_y", ncol = ncol) +
    # headroom above the curves so the corner stamp does not sit on the peaks
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.30))) +
    labs(x = x_lab, y = "Density") +
    plot_theme

  if (!is.null(annotations))
    p <- p + geom_text(data = relabel(annotations),
                       aes(x = Inf, y = Inf, label = label),
                       hjust = 1.05, vjust = 1.4, size = 2.6, lineheight = 0.9,
                       colour = "grey30", inherit.aes = FALSE)

  p
}


