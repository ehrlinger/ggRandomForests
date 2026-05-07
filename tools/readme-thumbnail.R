## Build the README/pkgdown overview figure: a 2x2 patchwork showing
## predicted survival, VIMP, OOB error, and partial dependence — drawn from
## the ggRandomForests-survival vignette (pbc data).
##
## Usage (from package root):
##   Rscript tools/readme-thumbnail.R
##
## Output: man/figures/README-overview.png

library(ggplot2)
library(dplyr)
library(randomForestSRC)
library(ggRandomForests)
library(patchwork)

theme_set(theme_bw())

event_colors <- c("steelblue", "firebrick")

# --- Data prep (mirrors vignettes/ggRandomForests-survival.qmd) ------------
data("pbc", package = "randomForestSRC")

pbc <- pbc |>
  mutate(
    years     = days / 365.25,
    age       = age / 365.25,
    treatment = factor(
      ifelse(treatment == 1, "DPCA",
             ifelse(treatment == 2, "Placebo", NA)),
      levels = c("DPCA", "Placebo")
    )
  ) |>
  select(-days)

resp_cols <- c("status", "years")
for (nm in setdiff(names(pbc), resp_cols)) {
  v <- pbc[[nm]]
  if (is.numeric(v) && !is.factor(v) && length(unique(v[!is.na(v)])) <= 5) {
    pbc[[nm]] <- factor(v)
  }
}

pbc_imputed <- impute(Surv(years, status) ~ ., data = pbc,
                      nsplit = 10, nimpute = 5)

# --- Fit forest ------------------------------------------------------------
set.seed(1)
rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., data = pbc_imputed,
                   nsplit = 10, tree.err = TRUE, importance = TRUE)

# --- Panels ----------------------------------------------------------------
strip <- theme(
  plot.title  = element_text(size = 11, face = "bold"),
  axis.title  = element_text(size = 9),
  legend.position = "none"
)

p_surv <- plot(gg_rfsrc(rfsrc_pbc), alpha = 0.2) +
  scale_color_manual(values = event_colors) +
  labs(y = "Survival", x = "Years") +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("Predicted survival") + strip

p_vimp <- plot(gg_vimp(rfsrc_pbc)) +
  ggtitle("Variable importance") + strip

p_err <- plot(gg_error(rfsrc_pbc)) +
  ggtitle("OOB error") + strip

ti   <- rfsrc_pbc$time.interest
t1yr <- ti[which.min(abs(ti - 1))]
t3yr <- ti[which.min(abs(ti - 3))]
pd   <- gg_partial_rfsrc(rfsrc_pbc, xvar.names = "bili",
                         partial.time = c(t1yr, t3yr))

p_pd <- ggplot(pd$continuous,
               aes(x = x, y = yhat,
                   color = factor(round(time, 2)),
                   group = factor(time))) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = event_colors) +
  labs(y = "Survival", x = "Bilirubin") +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("Partial dependence") + strip

# --- Assemble & save -------------------------------------------------------
thumb <- (p_surv | p_vimp) / (p_err | p_pd)

out <- "man/figures/README-overview.png"
ggsave(out, plot = thumb, width = 7, height = 6, dpi = 100)
message("Wrote ", out)
