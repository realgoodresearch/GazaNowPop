make_stratified_folds <- function(n, k = 5, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  idx <- sample(seq_len(n))
  split(idx, rep(seq_len(k), length.out = n))
}

add_observation_mask <- function(md, holdout1 = integer(), holdout2 = integer(), seed = NULL) {
  md_fold <- md

  holdout1 <- sort(unique(as.integer(holdout1)))
  holdout2 <- sort(unique(as.integer(holdout2)))

  obs1 <- setdiff(seq_len(md$J1), holdout1)
  obs2 <- setdiff(seq_len(md$J2), holdout2)

  md_fold$N1_obs <- length(obs1)
  md_fold$N2_obs <- length(obs2)
  md_fold$idx1_obs <- as.integer(obs1)
  md_fold$idx2_obs <- as.integer(obs2)
  md_fold$y1_obs <- as.integer(md$y1[obs1])
  md_fold$y2_obs <- as.integer(md$y2[obs2])
  md_fold$holdout1_idx <- holdout1
  md_fold$holdout2_idx <- holdout2

  if (!is.null(seed)) {
    md_fold$seed <- as.integer(seed)
  }

  md_fold
}

extract_provider_oos_draws <- function(draws_df, md, provider, tower_indices, fold_id) {
  if (length(tower_indices) == 0) {
    return(tibble())
  }

  mu_prefix <- paste0("mu_y", provider)
  yrep_prefix <- paste0("y", provider, "_rep")
  tower_ids <- md[[paste0("tower", provider, "_id")]]
  y_obs <- md[[paste0("y", provider)]]

  draws_df %>%
    select(.draw, all_of(paste0(mu_prefix, "[", tower_indices, "]")), all_of(paste0(yrep_prefix, "[", tower_indices, "]"))) %>%
    pivot_longer(
      cols = -.draw,
      names_to = c(".value", "tower_index"),
      names_pattern = paste0("(", mu_prefix, "|", yrep_prefix, ")\\[(\\d+)\\]")
    ) %>%
    transmute(
      fold = as.integer(fold_id),
      .draw = .draw,
      provider = as.integer(provider),
      tower_index = as.integer(tower_index),
      tower_id = tower_ids[tower_index],
      y_obs = y_obs[tower_index],
      mu_y = .data[[mu_prefix]],
      y_rep = .data[[yrep_prefix]]
    )
}

extract_oos_prediction_draws <- function(fit, md, fold_id) {
  vars <- c(
    paste0("mu_y1[", md$holdout1_idx, "]"),
    paste0("mu_y2[", md$holdout2_idx, "]"),
    paste0("y1_rep[", md$holdout1_idx, "]"),
    paste0("y2_rep[", md$holdout2_idx, "]")
  )
  vars <- unique(vars[nzchar(vars)])

  if (length(vars) == 0) {
    return(tibble())
  }

  draws_df <- posterior::as_draws_df(fit$draws(vars))

  bind_rows(
    extract_provider_oos_draws(draws_df, md, provider = 1, tower_indices = md$holdout1_idx, fold_id = fold_id),
    extract_provider_oos_draws(draws_df, md, provider = 2, tower_indices = md$holdout2_idx, fold_id = fold_id)
  )
}

summarize_prediction_draws <- function(pred_draws) {
  pred_draws %>%
    group_by(fold, provider, tower_index, tower_id, y_obs) %>%
    summarise(
      mu_y = mean(mu_y),
      y_rep_mean = mean(y_rep),
      y_rep_lower = quantile(y_rep, 0.025),
      y_rep_upper = quantile(y_rep, 0.975),
      .groups = "drop"
    ) %>%
    mutate(
      pred_obs_ratio = y_rep_mean / pmax(y_obs, 1),
      mu_obs_ratio = mu_y / pmax(y_obs, 1),
      error = y_rep_mean - y_obs,
      abs_error = abs(error),
      percent_error = error / pmax(y_obs, 1),
      covered = y_obs >= y_rep_lower & y_obs <= y_rep_upper
    )
}

compute_prediction_metrics <- function(pred_summary) {
  by_fold_provider <- pred_summary %>%
    group_by(fold, provider) %>%
    summarise(
      n_towers = n(),
      rmse = sqrt(mean((y_rep_mean - y_obs)^2)),
      mae = mean(abs_error),
      coverage = mean(covered),
      cor = if (n() > 1) cor(y_obs, y_rep_mean) else NA_real_,
      .groups = "drop"
    )

  overall <- pred_summary %>%
    group_by(provider) %>%
    summarise(
      fold = 0L,
      n_towers = n(),
      rmse = sqrt(mean((y_rep_mean - y_obs)^2)),
      mae = mean(abs_error),
      coverage = mean(covered),
      cor = if (n() > 1) cor(y_obs, y_rep_mean) else NA_real_,
      .groups = "drop"
    )

  bind_rows(by_fold_provider, overall) %>%
    mutate(provider = factor(provider, levels = c(1, 2)))
}

extract_fit_diagnostics <- function(fit, fold_id) {
  summary_df <- fit$summary()

  sampler_diag <- tryCatch(
    posterior::as_draws_df(fit$sampler_diagnostics()),
    error = function(e) NULL
  )

  tibble(
    fold = as.integer(fold_id),
    max_rhat = if ("rhat" %in% names(summary_df)) max(summary_df$rhat, na.rm = TRUE) else NA_real_,
    min_ess_bulk = if ("ess_bulk" %in% names(summary_df)) min(summary_df$ess_bulk, na.rm = TRUE) else NA_real_,
    min_ess_tail = if ("ess_tail" %in% names(summary_df)) min(summary_df$ess_tail, na.rm = TRUE) else NA_real_,
    n_divergent = if (!is.null(sampler_diag) && "divergent__" %in% names(sampler_diag)) {
      sum(sampler_diag$divergent__, na.rm = TRUE)
    } else {
      NA_real_
    },
    max_treedepth = if (!is.null(sampler_diag) && "treedepth__" %in% names(sampler_diag)) {
      max(sampler_diag$treedepth__, na.rm = TRUE)
    } else {
      NA_real_
    },
    mean_accept_stat = if (!is.null(sampler_diag) && "accept_stat__" %in% names(sampler_diag)) {
      mean(sampler_diag$accept_stat__, na.rm = TRUE)
    } else {
      NA_real_
    }
  )
}

make_oos_plot <- function(pred_summary, model_name) {
  coverage <- mean(pred_summary$covered)
  rmse <- sqrt(mean((pred_summary$y_rep_mean - pred_summary$y_obs)^2))
  r <- if (nrow(pred_summary) > 1) cor(pred_summary$y_obs, pred_summary$y_rep_mean) else NA_real_

  label_txt <- paste0(
    "RMSE = ", round(rmse, 1),
    "\nR = ", round(r, 2),
    "\nCoverage = ", round(100 * coverage, 1), "%"
  )

  ggplot(
    pred_summary,
    aes(x = y_obs, y = y_rep_mean, color = factor(provider))
  ) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = y_rep_lower, ymax = y_rep_upper), width = 0) +
    geom_point(size = 2, alpha = 0.8) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      label = label_txt,
      hjust = 1.1,
      vjust = -0.5,
      size = 4,
      color = "black"
    ) +
    scale_color_discrete(name = "Provider") +
    labs(
      x = "Observed subscribers",
      y = "OOS predicted subscribers",
      title = paste("Observed vs OOS Predicted Tower Subscribers -", model_name)
    ) +
    theme_minimal()
}

make_fold_metric_plot <- function(metrics, model_name) {
  metrics %>%
    filter(fold > 0) %>%
    ggplot(aes(x = factor(fold), y = rmse, fill = provider)) +
    geom_col(position = "dodge") +
    labs(
      x = "Fold",
      y = "RMSE",
      fill = "Provider",
      title = paste("OOS RMSE by Fold -", model_name)
    ) +
    theme_minimal()
}

make_diagnostic_plot <- function(diagnostics, model_name) {
  diagnostics %>%
    select(fold, max_rhat, min_ess_bulk, n_divergent) %>%
    pivot_longer(
      cols = -fold,
      names_to = "metric",
      values_to = "value"
    ) %>%
    ggplot(aes(x = factor(fold), y = value, group = 1)) +
    geom_line(color = "gray40") +
    geom_point(size = 2, color = "black") +
    facet_wrap(~metric, scales = "free_y") +
    labs(
      x = "Fold",
      y = NULL,
      title = paste("Cross-Validation Diagnostics -", model_name)
    ) +
    theme_minimal()
}

write_xval_results <- function(
  model_name,
  model_out_dir,
  oos_draws,
  fold_diagnostics,
  fit_paths = character(),
  md_paths = character(),
  fold_assignments = tibble(),
  k_folds = NA_integer_
) {
  oos_summary <- summarize_prediction_draws(oos_draws)
  fold_metrics <- compute_prediction_metrics(oos_summary)

  write.csv(
    oos_summary,
    file.path(model_out_dir, "oos_prediction_summary.csv"),
    row.names = FALSE
  )
  write.csv(
    fold_metrics,
    file.path(model_out_dir, "fold_metrics.csv"),
    row.names = FALSE
  )
  write.csv(
    fold_diagnostics,
    file.path(model_out_dir, "fold_diagnostics.csv"),
    row.names = FALSE
  )

  saveRDS(
    list(
      model_name = model_name,
      k_folds = k_folds,
      fit_paths = fit_paths,
      md_paths = md_paths,
      fold_assignments = fold_assignments,
      oos_draws = oos_draws,
      oos_summary = oos_summary,
      fold_metrics = fold_metrics,
      fold_diagnostics = fold_diagnostics
    ),
    file = file.path(model_out_dir, "xval_results.rds")
  )

  p_oos <- make_oos_plot(oos_summary, model_name)
  ggsave(
    filename = file.path(model_out_dir, "observed_vs_oos_predicted.png"),
    plot = p_oos,
    width = 8,
    height = 6,
    dpi = 300
  )

  p_rmse <- make_fold_metric_plot(fold_metrics, model_name)
  ggsave(
    filename = file.path(model_out_dir, "oos_rmse_by_fold.png"),
    plot = p_rmse,
    width = 8,
    height = 5,
    dpi = 300
  )

  p_diag <- make_diagnostic_plot(fold_diagnostics, model_name)
  ggsave(
    filename = file.path(model_out_dir, "fold_diagnostics.png"),
    plot = p_diag,
    width = 9,
    height = 6,
    dpi = 300
  )

  list(
    oos_summary = oos_summary,
    fold_metrics = fold_metrics,
    fold_diagnostics = fold_diagnostics
  )
}
