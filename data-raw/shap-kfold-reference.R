# Archived reference script -- not part of the package.
#
# A preserved record of an exploratory k-fold XGBoost SHAP analysis. It is kept
# for reference only and will not run as-is: it depends on objects
# (`model_list`, `X`, `folds`, `formula_var_name_to_english`, `x_to_x_mat`)
# that are never defined here, and on packages (`xgboost`, `SHAPforxgboost`,
# `data.table`, `pacman`) that are not package dependencies.
#
# This directory is listed in .Rbuildignore, so nothing here is built, checked,
# linted, or shipped in the tarball.

pacman::p_load(xgboost, SHAPforxgboost, data.table)

# Extract SHAP per fold ----
get_k_fold_shap_list <- function(model_list, X, folds) {
  shap_list <- vector("list", length(model_list))
  
  for (i in seq_along(model_list)) {
    model <- model_list[[i]]
    test_idx <- which(folds == i)
    X_test <- X[test_idx, ]
    
    # Compute SHAP values in long format for test set
    shap_long <- shap.prep(xgb_model = model,
                           X_train = as.matrix(X_test))
    
    # Compute mean |SHAP| + 95% CI per feature
    mean_ci_abs_shap_values <- shap_long %>%
      group_by(variable) %>%
      mutate(abs_shap_value = abs(value)) %>%
      summarise(
        mean_abs_shap_value = mean(abs_shap_value, na.rm = TRUE),
        s = sd(abs_shap_value, na.rm = TRUE),
        n = sum(!is.na(abs_shap_value)),
        se = s / sqrt(n),
        ci_lower = mean_abs_shap_value - 1.96*se,
        ci_upper = mean_abs_shap_value + 1.96*se
      ) %>%
      ungroup()
    
    shap_list[[i]] <- list(
      shap_long = shap_long,
      mean_ci_abs_shap_values = mean_ci_abs_shap_values
    )
  }
  
  return(shap_list)
}

# VIMP ----
get_k_fold_shap_vimp <- function(shap_list, var_to_english_map = formula_var_name_to_english) {
  fold_means_list <- lapply(shap_list, `[[`, "mean_ci_abs_shap_values")
  shap_fold_means <- rbindlist(fold_means_list, use.names = TRUE, fill = TRUE)
  
  shap_avg <- shap_fold_means %>%
    group_by(variable) %>%
    summarise(
      mean_abs_shap = mean(mean_abs_shap_value, na.rm = TRUE),
      se            = sd(mean_abs_shap_value, na.rm = TRUE) / sqrt(n()),
      ci_lower      = mean_abs_shap - 1.96 * se,
      ci_upper      = mean_abs_shap + 1.96 * se
    ) %>%
    ungroup()
  
  if (!is.null(var_to_english_map)) {
    shap_avg <- shap_avg %>%
      mutate(variable_english = dplyr::recode(variable, !!!var_to_english_map)) %>%
      arrange(desc(mean_abs_shap)) %>%
      mutate(variable_english = factor(variable_english, levels = rev(variable_english)))
  }
  
  ggplot(shap_avg, aes(x = mean_abs_shap, y = variable_english)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.25, color = "grey50") +
    labs(
      x = "Mean |SHAP|",
      y = NULL,
      title = "Average SHAP VIMP Across K Folds"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 16)
    )
}

# Beeswarm ----
get_k_fold_shap_beeswarm <- function(shap_list, var_to_english_map = formula_var_name_to_english) {
  all_shap_all_folds <- lapply(seq_along(shap_list), function(i) {
    shap_long <- shap_list[[i]]$shap_long
    # Make unique ID across folds
    shap_long %>% mutate(ID = ID + (i-1)*nrow(shap_long))
  })
  all_shap_all_folds <- rbindlist(all_shap_all_folds, use.names = TRUE, fill = TRUE)
  
  if (!is.null(var_to_english_map)) {
    all_shap_all_folds <- all_shap_all_folds %>%
      mutate(variable = dplyr::recode(variable, !!!var_to_english_map))
  }
  
  # Return combined long-format SHAP
  shap.plot.summary(all_shap_all_folds)
}



shap_list <- get_k_fold_shap_list(model_list, X, folds)
get_k_fold_shap_vimp(shap_list)
get_k_fold_shap_beeswarm(shap_list)


# 2D ----

# Choose one model to look at SHAP values for 
plot_shap_2d <- function(model, X, f1, f2, xlim_vals = NULL) {
  # Prepare SHAP values
  shap <- shap.prep(model, X_train = x_to_x_mat(X))

  # Filter for the two features
  shap_f1 <- shap[variable == f1]
  shap_f2 <- shap[variable == f2]

  # Rename columns to keep them distinct
  setnames(shap_f1, old = c("rfvalue", "value"), new = c("rfvalue_f1", "value_f1"))
  setnames(shap_f2, old = c("rfvalue", "value"), new = c("rfvalue_f2", "value_f2"))

  # Keep only ID + feature info
  shap_f1_small <- shap_f1[, .(ID, rfvalue_f1, value_f1)]
  shap_f2_small <- shap_f2[, .(ID, rfvalue_f2, value_f2)]

  # Join by ID
  shap_2d <- merge(shap_f1_small, shap_f2_small, by = "ID")


  p <- ggplot(shap_2d, aes(x = rfvalue_f1, y = value_f1, color = rfvalue_f2)) +
    geom_point(alpha = 0.6) +
    scale_color_viridis_c(option = "viridis") +
    labs(
      x = formula_var_name_to_english[[f1]],
      y = paste("SHAP value for", formula_var_name_to_english[[f1]]),
      color = formula_var_name_to_english[[f2]]
    ) +
    theme_minimal()

  # Optional x-axis limit
  if (!is.null(xlim_vals)) {
    p <- p + xlim(xlim_vals)
  }

  print(p)
  return(p)
}


p <- plot_shap_2d(
  model = model_list[[2]],
  X = X,
  f1 = "cpb_duration",
  f2 = "aocc_prop"
)


# Averages results across K folds 
# f1 is along X axis, f2 is color 
plot_shap_2d_k_fold <- function(model_list, X, f1, f2, xlim_vals = NULL,
                             shape_var = NULL, color_rescale = FALSE) {
  shap_all <- rbindlist(
    lapply(model_list, function(m) {
      shap <- shap.prep(m, X_train = x_to_x_mat(X))
      shap[, model_id := as.integer(.GRP), by = variable] # optional marker
      return(shap)
    }),
    fill = TRUE
  )

  # Average across models by ID + variable
  shap_avg <- shap_all[, .(
    rfvalue = mean(rfvalue, na.rm = TRUE),
    value = mean(value, na.rm = TRUE)
  ), by = .(ID, variable)]

  # Extract feature 1 and feature 2
  shap_f1 <- shap_avg[variable == f1]
  shap_f2 <- shap_avg[variable == f2]

  # Rename to keep columns distinct
  setnames(shap_f1, old = c("rfvalue", "value"), new = c("rfvalue_f1", "value_f1"))
  setnames(shap_f2, old = c("rfvalue", "value"), new = c("rfvalue_f2", "value_f2"))

  # Merge by ID
  shap_2d <- merge(shap_f1[, .(ID, rfvalue_f1, value_f1)],
                   shap_f2[, .(ID, rfvalue_f2, value_f2)],
                   by = "ID")

  if (!is.null(shape_var)) {
    shap_2d[, shape_col := X[[shape_var]][match(ID, rownames(X))]]
    p <- ggplot(shap_2d, aes(x = rfvalue_f1, y = value_f1, color = rfvalue_f2, shape = factor(shape_col)))
  } else {
    p <- ggplot(shap_2d, aes(x = rfvalue_f1, y = value_f1, color = rfvalue_f2))
  }

  # Build the rest of the plot
  p <- p +
    geom_point(alpha = 0.6) +
    labs(
      x = formula_var_name_to_english[[f1]],
      y = paste("Mean SHAP value for", formula_var_name_to_english[[f1]]),
      color = formula_var_name_to_english[[f2]],
      shape = if (!is.null(shape_var)) formula_var_name_to_english[[shape_var]] else NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )

  if (!is.null(xlim_vals)) {
    p <- p + xlim(xlim_vals)
  }

  if(color_rescale) {
    vmin <- quantile(shap_2d$rfvalue_f2, 0.01, na.rm = TRUE)
    vmax <- quantile(shap_2d$rfvalue_f2, 0.99, na.rm = TRUE)
    
    p <- p + scale_color_viridis_c(option = "viridis", limits = c(vmin, vmax))
  }
  else {
    p <- p + scale_color_viridis_c(option = "viridis") 
  }

  print(p)
  return(p)
}

p <- plot_shap_2d_k_fold(
  model_list = model_list,
  X = X,
  f1 = "do2_interp_m2_mean",
  f2 = "hx_copd"
)


p <- plot_shap_2d_k_fold(
  model_list = model_list,
  X = X,
  f1 = "do2_interp_m2_mean",
  f2 = "hx_dm"
)

p <- plot_shap_2d_k_fold(
  model_list = model_list,
  X = X,
  f1 = "cpb_duration",
  f2 = "aocc_prop"
)


p1 <- plot_shap_2d_k_fold(
  model_list = model_list,
  X = X %>%  filter(hx_dm == 0), 
  f1 = "precpb_gluc_io_mean",
  f2 = "gluc_io_mean", color_rescale = TRUE
)

p2 <- plot_shap_2d_k_fold(
  model_list = model_list,
  X = X %>%  filter(hx_dm == 1), 
  f1 = "precpb_gluc_io_mean",
  f2 = "gluc_io_mean", color_rescale = TRUE
)
p1 + ylim(c(-0.3, 0.15)) + labs(title = "No DM") | p2 + ylim(c(-0.3, 0.15)) + labs(title = "Yes DM") 


p1 <- plot_shap_2d_k_fold(
  model_list = model_list,
  X = X %>%  filter(hx_dm == 0), 
  f2 = "precpb_gluc_io_mean",
  f1 = "gluc_io_mean", color_rescale = TRUE
)

p2 <- plot_shap_2d_k_fold(
  model_list = model_list,
  X = X %>%  filter(hx_dm == 1), 
  f2 = "precpb_gluc_io_mean",
  f1 = "gluc_io_mean", color_rescale = TRUE
)
p1 + labs(title = "No DM") | p2 + labs(title = "Yes DM") 




plot_shap_2d_k_fold(
  model_list = model_list,
  X = X,
  f2 = "precpb_gluc_io_mean",
  f1 = "gluc_io_mean",
  shape_var = "hx_dm"
)




summary(X$map_mean)


# ggplot(shap_2d, aes(x = value_f1, y = value_f2)) +
#   geom_point() +
#   # stat_summary_2d(fun = mean, bins = 50) +  # average SHAP in each 2D bin
#   scale_fill_viridis_c(option = "viridis") +
#   labs(
#     x = formula_var_name_to_english[[f1]],
#     y = formula_var_name_to_english[[f2]],
#     fill = paste("SHAP for", formula_var_name_to_english[[f1]])
#   ) +
#   theme_minimal()

