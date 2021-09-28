message(timestamp(quiet = TRUE))
### Project Setup ==================================================================================
library(here)
output_directory <- here("outputs", "models-diagnostics")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE, mode = "0775")


### Load Packages ==================================================================================
suppressPackageStartupMessages({
  library(ragg)
  library(svglite)
  library(ggplot2)
  library(ggtext)
  library(patchwork)
  library(data.table)
  library(forcats)
  library(ggbeeswarm)
  library(glue)
  # library(future)
  # library(future.callr)
  # library(future.apply)
})


### project setup ==================================================================================
# plan(future.callr::callr, workers = 40)
# message(sprintf("Number of workers: %d", future::nbrOfWorkers()))


### Tables and Figures Theme =======================================================================
options(
  ggplot2.discrete.colour = function(...) scale_colour_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.discrete.fill = function(...) scale_fill_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.colour = function(...) scale_colour_viridis_c(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.fill = function(...) scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
)
theme_set(theme_minimal(base_family = "Verdana"))
theme_update(
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_markdown(),
  plot.subtitle = element_markdown(face = "italic"),
  plot.caption = element_markdown(face = "italic"),
  axis.title.x = element_markdown(),
  axis.text.x = element_markdown(),
  axis.title.y = element_markdown(),
  axis.text.y = element_markdown()
)


### Functions ======================================================================================


### Analysis =======================================================================================
files <- list.files(here("data"), ".*models-performance.csv$", full.names = TRUE, recursive = TRUE)

perfs_dt <- rbindlist(lapply(
  X = files,
  FUN = \(x) fread(x)[j = cohort := sub("-.*", "", basename(x))]
))[j = .SD[.N == length(files) * 2], by = c("dataset", "model")]


metrics <- c("AIC", "BIC", "R2_conditional", "R2_marginal", "ICC", "RMSE", "Sigma")

perfs_dt[
  j = c(metrics) := lapply(.SD, function(x) scale(x)[, 1]),
  by = "cohort",
  .SDcols = metrics
]

p <- ggplot(perfs_dt) +
  aes(colour = sub(" --.*", "", model)) +
  geom_boxplot(outlier.shape = NA) +
  geom_beeswarm(shape = 1) +
  stat_summary(geom = "point", fun = mean, shape = 18, colour = "firebrick", size = 4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "none")

svglite(filename = file.path(output_directory, "perfs.svg"), width = 16, height = 12, scaling = 1.25)
  wrap_plots(
    p +
      aes(
        x = fct_reorder(sub(" -- ", "<br>", model), AIC),
        y = AIC
      ) +
      labs(x = NULL),
    p +
      aes(
        x = fct_reorder(sub(" -- ", "<br>", model), RMSE),
        y = RMSE
      ) +
      labs(x = NULL),
    p +
      aes(
        x = fct_reorder(sub(" -- ", "<br>", model), -1 * R2_conditional),
        y = R2_conditional
      ) +
      labs(x = NULL),
    nrow = 3,
    ncol = 1
  ) +
    plot_annotation(
      title = "Working Models for Male And Female",
      subtitle = sprintf(
        "Cohorts: %s<br>Datasets: %s",
        glue_collapse(sort(unique(perfs_dt[["cohort"]])), sep = ", ", last = " and "),
        glue_collapse(sort(unique(perfs_dt[["dataset"]])), sep = ", ", last = " and ")
      ),
      tag_levels = "A"
    )
invisible(dev.off())


### Complete =======================================================================================
message("Success!", appendLF = TRUE)
message(timestamp(quiet = TRUE))
