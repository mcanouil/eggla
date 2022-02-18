message(timestamp(quiet = TRUE))
### Project Setup ==================================================================================
library(here)
output_directory <- here("outputs", "models-diagnostics")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE, mode = "0775")


### Load Packages ==================================================================================
suppressPackageStartupMessages({
  library(svglite)
  library(ggplot2)
  library(ggtext)
  library(patchwork)
  library(data.table)
})


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
best_models <- c(
  "Linear Splines -- Random Intercepts",
  "Cubic Splines -- Random Linear Splines",
  "Cubic Splines -- Random Intercepts",
  "Cubic Slope -- Random Cubic Slopes",
  "Cubic Slope -- Random Quadratic Slopes",
  "Cubic Slope -- Random Linear Slopes",
  "Cubic Slope -- Random Intercepts"
)

best_dataset <- "Clean"

res_models <- readRDS(file.path(output_directory, "summary_models.rds"))
res_models[
  j = `:=`(
    dataset = c(
      "visits_raw" = "Raw",
      "visits_clean" = "Clean",
      # "visits_clean[age <= 11]" = "Clean<br><i style='font-size: 6pt'>(0 ≤ age ≤ 11)</i>",
      # "visits_clean[age <= 12]" = "Clean<br><i style='font-size: 6pt'>(0 ≤ age ≤ 12)</i>",
      # "visits_clean[age <= 13]" = "Clean<br><i style='font-size: 6pt'>(0 ≤ age ≤ 13)</i>",
      "visits_clean[between(age, 1, 18)]" = "Clean<br><i style='font-size: 6pt'>(1 ≤ age ≤ 18)</i>"
    )[dataset],
    model = setNames(
      object = c(
        "Linear Splines -- Random Linear Splines",
        "Linear Splines -- Random Intercepts",
        "Cubic Splines -- Random Cubic Splines",
        "Cubic Splines -- Random Quadratic Splines",
        "Cubic Splines -- Random Linear Splines",
        "Cubic Splines -- Random Intercepts",
        "Cubic Slope -- Random Cubic Slopes",
        "Cubic Slope -- Random Quadratic Slopes",
        "Cubic Slope -- Random Linear Slopes",
        "Cubic Slope -- Random Intercepts"
      ),
      nm = unique(models)
    )[models]
  )
][
  j = model := fifelse(correlation_structure == "NULL", model, sub("--", "-- C-AR1 --", model))
]

good_res_models <- res_models[model %in% best_models & dataset %in% best_dataset][
  j = rbindlist(list(
    (function(x) {
      data.table(age = seq(0, 16, 0.1))[
        j = bmi := exp(predict(x[[1]], .SD, level = 0)),
        .SDcols = "age"
      ]
    })(lme)
  )),
  by = list(sex, dataset, model)
]

svglite(filename = file.path(output_directory, "models-adiposity.svg"), width = 12, height = 6)
  ggplot(data = good_res_models) +
    aes(
      x = age,
      y = bmi,
      colour = sprintf("%s -- %s", model, c("0" = "Male", "1" = "Female")[as.character(sex)])
    ) +
    geom_path() +
    labs(
      x = "AGE (years)",
      y = "BMI (kg/m²)",
      colour = NULL
    ) +
    facet_grid(cols = vars(sub(" --.*", "", model)))
invisible(dev.off())


### Complete =======================================================================================
message("Success!", appendLF = TRUE)
message(timestamp(quiet = TRUE))
