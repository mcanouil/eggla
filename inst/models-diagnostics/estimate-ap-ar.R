# Code initially written by: Anni Heiskala <Anni.Heiskala@oulu.fi>
# Modified by: Mickaël Canouil <mickael.canouil@cnrs.fr>

# # MIT License
#
# Copyright (c) 2022 Mickaël Canouil & Anni Heiskala
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


library(here)
library(data.table)
library(stats)
library(utils)
library(eggla)
library(ggplot2)
library(ggtext)
library(ragg)
library(patchwork)

od <- here("outputs/04-check-ap-ar")
dir.create(od, showWarnings = FALSE)

# Import data and results
pheno_dt <- fread(here("outputs/01-format_phenotypes/phenotypes.csv"))
models_rds <- lapply(
  X = (function(.x) `names<-`(.x, sub("^.*-([^-]*)\\.zip$", "\\1", basename(.x))))(
    list.files(
      path = here("outputs/02-egg_analysis"),
      full.names = TRUE
    )
  ),
  FUN = function(ifile) {
    readRDS(unzip(
      zipfile = ifile,
      files = "model-object.rds",
      exdir = tempdir()
    ))
  }
)

predicted_data <- lapply(
  X = models_rds,
  FUN = function(model_rds) {
    data.table(
      egg_id = unique(model_rds[["groups"]][["egg_id"]]),
      egg_ageyears = list(seq(from = 0.25, to = 10, by = 0.05))
    )[
      j = list(egg_ageyears = unlist(egg_ageyears)),
      by = "egg_id"
    ][
      j = log_bmi_pred := predict(
        object = model_rds,
        newdata = .SD,
        interval = "prediction"
      )
    ][
      j = bmi_pred := exp(log_bmi_pred)
    ][
      j = `:=`(
        AP = egg_ageyears %in% egg_ageyears[which(diff(sign(diff(bmi_pred))) == -2) + 1],
        AR = egg_ageyears %in% egg_ageyears[which(diff(sign(diff(bmi_pred))) == +2) + 1]
      ),
      by = "egg_id"
    ]
  }
)

# Plot one male individual
# one_individual_data <- predicted_data[["male"]][egg_id %in% egg_id[1]]
# one_individual_apar <- melt(
#   data = one_individual_data[AP | AR],
#   id.vars = c("egg_id", "egg_ageyears"),
#   measure.vars = c("AP", "AR")
# )[
#   (value)
# ][
#   j = labels := sprintf(
#     "<b style=\"color:%s;\">%s = %s</b>",
#     c("#b22222", "#22b222")[variable],
#     variable,
#     egg_ageyears
#   )
# ]
# ggplot(data = one_individual_data) +
#   geom_line(mapping = aes(x = egg_ageyears, y = bmi_pred)) +
#   geom_vline(
#     data = one_individual_apar,
#     mapping = aes(
#       xintercept = egg_ageyears,
#       colour = variable,
#       linetype = variable
#     ),
#     size = 1,
#     show.legend = FALSE
#   ) +
#   scale_x_continuous(
#     sec.axis = dup_axis(
#       name = NULL,
#       breaks = one_individual_apar[["egg_ageyears"]],
#       labels = one_individual_apar[["labels"]]
#     )
#   ) +
#   scale_colour_manual(values = c("#b22222", "#22b222")) +
#   scale_linetype_manual(values = c(2, 4)) +
#   labs(
#     x = "AGE (years)",
#     y = "BMI (kg/m²)",
#     colour = NULL,
#     linetype = NULL
#   ) +
#   theme_minimal() +
#   theme(axis.text.x.top = element_markdown()) +
#   theme(panel.grid.minor.x = element_blank())


# Plot average for female and male individuals
p_apar <- mapply(
  data = predicted_data,
  sex = names(predicted_data),
  SIMPLIFY = FALSE,
  FUN = function(data, sex) {
    predicted_dt_apar <- melt(
      data = data[AP | AR],
      id.vars = c("egg_id", "egg_ageyears"),
      measure.vars = c("AP", "AR")
    )[
      (value)
    ][
      j = list(egg_ageyears = mean(egg_ageyears)),
      by = "variable"
    ][
      j = labels := sprintf(
        "<b style=\"color:%s;\">%s &#8771; %0.2f</b>",
        c("#b22222", "#22b222")[variable],
        variable,
        egg_ageyears
      )
    ]
    ggplot(data = data) +
      aes(x = egg_ageyears, y = bmi_pred) +
      geom_path(# Comment or remove this for big cohort
        mapping = aes(group = factor(egg_id)),
        colour = "#333333",
        na.rm = TRUE,
        alpha = 0.05,
        show.legend = FALSE
      ) +
      stat_smooth(
        method = "loess", # Comment for big cohort, i.e., to use default method which is "gam"
        formula = y ~ x, # Comment this for big cohort, i.e., to use default "gam" formula 
        linetype = 2,
        colour = "#2222b2",
        se = FALSE
      ) +
      geom_vline(
        data = predicted_dt_apar,
        mapping = aes(
          xintercept = egg_ageyears,
          colour = variable,
          linetype = variable
        ),
        size = 1,
        show.legend = FALSE
      ) +
      scale_x_continuous(
        sec.axis = dup_axis(
          name = NULL,
          breaks = predicted_dt_apar[["egg_ageyears"]],
          labels = predicted_dt_apar[["labels"]]
        )
      ) +
      scale_y_log10() +
      scale_colour_manual(values = c("#b22222", "#22b222")) +
      scale_linetype_manual(values = c(2, 4)) +
      labs(
        x = "AGE (years)",
        y = "BMI (kg/m²)",
        colour = NULL,
        linetype = NULL,
        title = toupper(sex)
      ) +
      theme_minimal() +
      theme(axis.text.x.top = element_markdown()) +
      theme(panel.grid.minor.x = element_blank())
  }
)

agg_png(
  file = file.path(od, "ap-ar.png"),
  width = 16 / 2.54,
  height = 16 / 2.54,
  units = "in",
  res = 300,
  scaling = 0.75
)
print(wrap_plots(p_apar, nrow = 2))
invisible(dev.off())
