# # MIT License
#
# Copyright (c) 2022 Mickaël Canouil
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
library(utils)
library(ggplot2)
library(ggdist)
library(ggbeeswarm)
library(ggh4x)
library(ggtext)
library(scales)
library(ragg)

od <- here("outputs/03-check-parameters")
dir.create(od, showWarnings = FALSE)

# Import data and results
pheno_dt <- fread(here("outputs/01-format_phenotypes/phenotypes.csv"))
slopes_dt <- rbindlist(
  l = unlist(
    x = lapply(
      X = list.files(
        path = here("outputs/02-egg_analysis"),
        full.names = TRUE
      ),
      FUN = function(ifile) {
        lapply(
          X = utils::unzip(
            zipfile = ifile,
            files = c("derived-auc.csv", "derived-slopes.csv")[2],
            exdir = tempdir()
          ),
          FUN = function(jfile) {
            fread(jfile)[j = file := basename(ifile)]
          }
        )
      }
    ),
    recursive = FALSE
  ),
  fill = TRUE
)
auc_dt <- rbindlist(
  l = unlist(
    x = lapply(
      X = list.files(
        path = here("outputs/02-egg_analysis"),
        full.names = TRUE
      ),
      FUN = function(ifile) {
        lapply(
          X = utils::unzip(
            zipfile = ifile,
            files = c("derived-auc.csv", "derived-slopes.csv")[1],
            exdir = tempdir()
          ),
          FUN = function(jfile) {
            fread(jfile)[j = file := basename(ifile)]
          }
        )
      }
    ),
    recursive = FALSE
  ),
  fill = TRUE
)

random_individuals <- intersect(
  pheno_dt[["ID"]],
  intersect(slopes_dt[["egg_id"]], auc_dt[["egg_id"]])
)

# random_individuals <- pheno_dt[j = list(x = ID[seq(10)]), by = "sex"][["x"]]

pheno_dt <- pheno_dt[
  ID %in% random_individuals
]
slopes_dt <- slopes_dt[
  egg_id %in% random_individuals
]
auc_dt <- auc_dt[
  egg_id %in% random_individuals
]

# Format slopes
slopes_long_dt <- melt(
  data = slopes_dt[
    j = sex := as.numeric(!grepl("female", file))
  ][
    j = lapply(.SD, mean),
    by = "sex",
    .SDcols = patterns(
      paste(
        sprintf(
          c("^pred_period_%s$", "^slope_%s--.*$"),
          rep(sub("^.*_(.*)--.*$", "\\1", grep("^slope_", names(slopes_dt), value = TRUE)), each = 2)
        ),
        collapse = "|"
      )
    )
  ],
  id.vars = "sex",
  measure.vars = patterns(pred_period = "^pred_period_", slope = "^slope_")
)[
  j = params := sub(".*_", "", grep("^slope_", names(slopes_dt), value = TRUE))[as.numeric(variable)]
][
  j = params := factor(params, levels = unique(params))
][
  j = c("start", "end") := tstrsplit(
    x = sub("^slope_", "", params),
    split = "--"
  )
][
  j = c("start", "end") := lapply(.SD, as.numeric),
  .SDcols = c("start", "end")
][
  j = yend := pred_period + end * slope
]

p_slope <- ggplot(
  data = pheno_dt[
    j = params := as.character(cut(
      x = age,
      breaks = unique(unlist(slopes_long_dt[j = c("start", "end")])),
      include.lowest = TRUE
    ))
  ][
    j = params := sub(",", "--", gsub("\\(|\\)|\\[|\\]", "", params))
  ][
    j = params := factor(params, levels = unique(params))
  ]
) +
  aes(x = age, y = bmi) +
  geom_rect(
    data = unique(slopes_long_dt[j = c("params", "start", "end")]),
    mapping = aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = params),
    alpha = 0.15,
    inherit.aes = FALSE
  ) +
  geom_path(# Comment or remove this for big cohort
    mapping = aes(group = factor(ID)),
    colour = "#333333",
    na.rm = TRUE,
    alpha = 0.05,
    show.legend = FALSE
  ) +
  # geom_point(
  #   mapping = aes(group = factor(ID)),
  #   colour = "#333333",
  #   size = 0.25,
  #   na.rm = TRUE,
  #   alpha = 0.10,
  #   show.legend = FALSE
  # ) +
  stat_smooth(
    method = "loess", # Comment for big cohort, i.e., to use default method which is "gam"
    formula = y ~ x, # Commentthis for big cohort, i.e., to use default "gam" formula 
    linetype = 2,
    colour = "firebrick",
    se = FALSE
  ) +
  # stat_smooth(
  #   data = ~ .x[params %in% unique(slopes_long_dt[["params"]])],
  #   mapping = aes(group = params),
  #   method = "lm",
  #   formula = y ~ x,
  #   linetype = 1,
  #   colour = "firebrick",
  #   se = FALSE
  # ) +
  scale_x_sqrt(
    expand = c(0, 0),
    breaks = unique(unlist(slopes_long_dt[j = c("start", "end")]), use.names = FALSE),
    labels = function(x)  sub("\\.0+", "", x)
  ) +
  scale_y_log10() +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  scale_fill_viridis_d(option = "plasma", end = 0.85) +
  labs(
    x = "AGE (years)",
    y = "BMI (kg/m²)",
    colour = "Intervals",
    fill = "Intervals"
  ) +
  facet_grid(
    cols = vars(sex),
    # margins = TRUE,
    labeller = labeller(
      .cols = function(x) c("0" = "FEMALE", "1" = "MALE", "2" = "FEMALE", "(all)" = "ALL")[x]
    )
  ) +
  geom_segment(
    data = slopes_long_dt,
    mapping = aes(x = start, y = exp(pred_period), xend = end, yend = exp(yend), colour = params),
    size = 1,
    inherit.aes = FALSE,
    linetype = 1
  ) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())

agg_png(
  file = file.path(od, "slopes.png"),
  width = 16 / 2.54,
  height = 9 / 2.54,
  units = "in",
  res = 300,
  scaling = 0.75
)
print(p_slope)
invisible(dev.off())

# Format AUCs
p_auc <- ggplot(
  data = melt(
    data = auc_dt[
      j = sex := sprintf("<b>%s</b>",
        c(
          "0" = "FEMALE", "1" = "MALE", "2" = "FEMALE", "(all)" = "ALL"
        )[
          as.character(as.numeric(!grepl("female", file)))
        ]
      )
    ],
    id.vars = c("egg_id", "sex"),
    measure.vars = patterns("^auc_"),
    variable.name = "period_interval",
    value.name = "auc"
  )[
    j = period_interval := gsub("auc_", "", period_interval)
  ][
    j = period_interval := factor(
      x = period_interval,
      levels = unique(period_interval)
    )
  ][
    j = period_interval := sprintf(
      "<b><i style='color: %s'>%s</i></b>",
      viridis_pal(
        option = "plasma",
        end = 0.85
      )(length(unique(period_interval)))[period_interval],
      period_interval
    )
  ][
    j = period_interval := factor(
      x = period_interval,
      levels = unique(period_interval)
    )
  ]
) +
  aes(x = period_interval, y = auc) +
  stat_halfeye(
    mapping = aes(fill = period_interval),
    justification = -0.20,
    .width = 0,
    scale = 1
  ) +
  geom_boxplot(mapping = aes(colour = period_interval), width = 0.25, outlier.colour = NA) +
  geom_quasirandom(
    mapping = aes(fill = period_interval, colour = period_interval),
    # colour = "white",
    shape = 21,
    alpha = 0.05,
    groupOnX = T,
    width = 0.15#,
    # side = -1,
    # cex = 0.5
  ) +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  scale_fill_viridis_d(option = "plasma", end = 0.85) +
  labs(
    x = "Period Interval (years)",
    y = "Area Under The Curve (AUC)",
    colour = "Intervals",
    fill = "Intervals"
  ) +
  theme_minimal() +
  facet_wrap(
    facets = vars(sex, period_interval),
    scales = "free",
    ncol = 4,
    nrow = 2
  ) +
  theme(
    strip.text = element_markdown(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

agg_png(
  file = file.path(od, "aucs.png"),
  width = 16 / 2.54,
  height = 9 / 2.54,
  units = "in",
  res = 300,
  scaling = 0.75
)
print(p_auc)
invisible(dev.off())

out <- melt(
  data = auc_dt[
    j = sex := sprintf("<b>%s</b>",
      c(
        "0" = "FEMALE", "1" = "MALE", "2" = "FEMALE", "(all)" = "ALL"
      )[
        as.character(as.numeric(!grepl("female", file)))
      ]
    )
  ],
  id.vars = c("egg_id", "sex"),
  measure.vars = patterns("^auc_"),
  variable.name = "period_interval",
  value.name = "auc"
)[
  j = period_interval := gsub("auc_", "", period_interval)
][
  j = period_interval := factor(
    x = period_interval,
    levels = unique(period_interval)
  )
][
  j = as.data.table(matrix(
    round(
      x = c(
        length(auc), min(auc), max(auc),
        mean(auc), sd(auc),
        quantile(auc, 0.25), quantile(auc, 0.5), quantile(auc, 0.75)
      ),
      digits = 2
    ),
    nrow = 1,
    dimnames = list(NULL, c("N", "min", "max", "mean", "sd", "p25", "p50", "p75"))
  )),
  by = c("sex", "period_interval")
]

dcast(out, sex + N ~ period_interval, value.var = c("min", "max", "mean", "sd", "p25", "p50", "p75"))
