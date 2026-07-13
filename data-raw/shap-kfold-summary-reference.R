# Archived reference script -- not part of the package.
#
# A preserved record of an exploratory k-fold XGBoost SHAP summary analysis. It
# is kept for reference only and will not run as-is: it depends on objects
# (`model_list`, `X`, `figure_dir`, `formula_var_name_to_english`, `x_to_x_mat`)
# that are never defined here, and on packages (`SHAPforxgboost`, `dplyr`) that
# are not fully declared for this workflow.
#
# This directory is listed in .Rbuildignore, so nothing here is built, checked,
# linted, or shipped in the tarball.

shap_values_list_mat <- lapply(model_list, function(model) {
  as.matrix(shap.values(xgb_model = model, X_train = x_to_x_mat(X))$shap_score)
})

# --- Dimensions ---
n_obs <- nrow(shap_values_list_mat[[1]])
n_feat <- ncol(shap_values_list_mat[[1]])
n_folds <- length(shap_values_list_mat)

# --- Pre-allocate 3D array ---
shap_array <- array(NA, dim = c(n_obs, n_feat, n_folds),
                    dimnames = list(NULL, colnames(shap_values_list_mat[[1]]), NULL))

# --- Fill array ---
for (i in seq_len(n_folds)) {
  shap_array[,,i] <- shap_values_list_mat[[i]]
}

# --- Average across folds (element-wise) ---
shap_mean <- apply(shap_array, c(1,2), mean, na.rm = TRUE)

shap_mean <- as.data.frame(shap_mean)

# --- Prepare long-format for plotting ---
shap_long_avg <- shap.prep(
  shap_contrib = shap_mean,
  X_train = x_to_x_mat(X)
)

sum(X$std_dose_mg_hr > 14)

shap_long_avg$var_name <- shap_long_avg$variable 

# Optional: rename feature labels
shap_long_avg$variable <- dplyr::recode(shap_long_avg$variable, !!!formula_var_name_to_english)

shap_long_avg$rfvalue_capped <- shap_long_avg$rfvalue

# Cap at 99th percentile per variable
shap_long_avg <- shap_long_avg %>%
  group_by(var_name) %>%
  mutate(
    stdfvalue_original = stdfvalue,
    rfvalue_capped = pmin(rfvalue, quantile(rfvalue, 0.99, na.rm = TRUE)),
    stdfvalue = (rfvalue_capped - min(rfvalue_capped)) / (max(rfvalue_capped) - min(rfvalue_capped))
  ) %>%
  ungroup()


# --- Plot averaged SHAP summary (beeswarm) ---
shap.plot.summary(shap_long_avg)


shap_long_avg_subset <- shap_long_avg %>% 
  filter(var_name %in% c("std_dose_mg_hr", "ci_mean", "map_mean", "wbc_pr", "age", "bmi"))  %>%
  # drop other variables completely
  droplevels()
shap.plot.summary(shap_long_avg_subset)


outfile <- file.path(figure_dir, "shap_summary_kfold.png")

# Save SHAP summary plot
png(outfile, width = 12, height = 8, units = "in", res = 300)
shap.plot.summary(shap_long_avg)
dev.off()


outfile <- file.path(figure_dir, "shap_summary_kfold_subset.png")
png(outfile, width = 12, height = 8, units = "in", res = 300)
shap.plot.summary(shap_long_avg_subset)
dev.off()


# Conditional SHAP ----
shap.plot.dependence(
  shap_long_avg, 
  x = "Mean MAP (mmHg)",       # feature on x-axis
  color_feature = "Age (years)" # color by age to see interaction effect
)

# SHAP dependence plot of MAP, colored by BMI
shap.plot.dependence(
  shap_long_avg, 
  x = "Mean MAP (mmHg)",       # feature on x-axis
  color_feature = "BMI"
)

# Create age groups
age_groups <- cut(
  X$age,
  breaks = c(0, 55, 65, 75, Inf),
  labels = c("<55", "55–64", "65–74", "≥75"),
  right = FALSE,
  ordered_result = TRUE
)
age_groups <- cut(
  X$age,
  breaks = c(0, 50, 60, 70, 80, Inf),
  labels = c("<50", "50–59", "60–69", "70–79", "≥80"),
  right = FALSE,
  ordered_result = TRUE
)
summary(age_groups)
# Create BMI groups
bmi_groups <- cut(
  X$bmi,
  breaks = c(0, 25, 30, Inf),
  labels = c("<25", "25–29.9", "≥30"),
  right = FALSE,
  ordered_result = TRUE
)


shap_map <- shap_long_avg %>% filter(var_name == "map_mean")
shap_ci <- shap_long_avg %>% filter(var_name == "ci_mean")
shap_std <- shap_long_avg %>% filter(var_name == "std_dose_mg_hr")


shap_map <- shap_map %>% 
  mutate(rf_value_ci = shap_ci$rfvalue,
         rf_value_std = shap_std$rfvalue_capped)

shap_map$age_groups <- age_groups
shap_map$bmi_groups <- bmi_groups


ggplot(shap_map, aes(x = rfvalue, y = value, color = rf_value_std)) +
  geom_point(alpha = 0.4, size = 0.7) +
  facet_grid(bmi_groups ~ age_groups) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "MAP", y = "SHAP (MAP)", color = "Pressor dose") +
  theme_minimal()





pressor_groups <- cut(
  X$std_dose_mg_hr,
  breaks = c(0, 0.2, 0.4, 0.6, Inf),
  labels = c("0–0.19", "0.20–0.39", "0.40–0.59", "≥0.60"),
  right = FALSE,
  ordered_result = TRUE
)
summary(pressor_groups)
# Create BMI groups
map_groups <- cut(
  X$map_mean,
  breaks = c(0, 65, 70, 75, Inf),
  labels = c("<65", "65–69", "70–74", "≥75"),
  right = FALSE,
  ordered_result = TRUE
)
summary(map_groups)

# shap_map <- shap_long_avg %>% filter(var_name == "map_mean")
shap_ci <- shap_long_avg %>% filter(var_name == "ci_mean")
# shap_std <- shap_long_avg %>% filter(var_name == "std_dose_mg_hr")
shap_age <- shap_long_avg %>% filter(var_name == "age")


shap_ci <- shap_ci %>%
  mutate(rf_value_age = shap_age$rfvalue)

shap_ci$pressor_groups <- pressor_groups
shap_ci$map_groups <- map_groups


ggplot(shap_ci, aes(x = rfvalue, y = value, color = rf_value_age)) +
  geom_point(alpha = 0.4, size = 0.7) +
  facet_grid(pressor_groups ~ map_groups) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "CI", y = "SHAP (MAP)", color = "Age") +
  theme_minimal()










cut(
  X$age,
  breaks = c(0, 55, 65, 75, Inf),
  labels = c("<55", "55–64", "65–74", "≥75"),
  right = FALSE,
  ordered_result = TRUE
)

facet_groups2 <- cut(
  X$bmi,
  breaks = c(0, 25, 30, Inf),
  labels = c("<25", "25–29.9", "≥30"),
  right = FALSE,
  ordered_result = TRUE
)
