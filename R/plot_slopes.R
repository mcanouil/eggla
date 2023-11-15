#' Plot derived slopes from a model fitted by `time_model()`.
#'
#' @param fit A model object from a statistical model such as
#'   from a call to `time_model()`.
#' @param method The type of model provided in `fit`,
#'   _i.e._, one of `"cubic_slope"`, `"linear_splines"` or `"cubic_splines"`.
#' @param period The intervals knots on which AUCs are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#'
#' @return A `patchwork` `ggplot2` object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(eggla)
#' data("bmigrowth")
#' ls_mod <- time_model(
#'   x = "age",
#'   y = "log(bmi)",
#'   cov = NULL,
#'   data = bmigrowth[bmigrowth[["sex"]] == 0, ],
#'   method = "linear_splines"
#' )
#' plot_slopes(
#'   fit = ls_mod,
#'   method = "linear_splines"
#' )
plot_slopes <- function(
  fit,
  method,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(0.75, 5.5, 11),
    "cubic_splines" = c(1, 8, 12)
  )[[method]]
) {
  params <- variable <- yend <- pred_period <- end <- NULL # no visible binding for global variable from data.table
  patterns <- slope <- .data <- start <- NULL # no visible binding for global variable from data.table
  stopifnot(inherits(fit, "lme"))
  id_var <- names(fit[["groups"]])
  age_var <- grep("age", all.vars(fit[["terms"]]), value = TRUE, ignore.case = TRUE)
  bmi_var <- grep("bmi", all.vars(fit[["terms"]]), value = TRUE, ignore.case = TRUE)

  okabe_ito_palette <- c(
    "#E69F00FF", "#56B4E9FF", "#009E73FF", "#F0E442FF", "#0072B2FF",
    "#D55E00FF", "#CC79A7FF", "#999999FF"
  )

  pheno_dt <- data.table::as.data.table(fit[["data"]])
  slopes_dt <- data.table::as.data.table(compute_slopes(
    fit = fit,
    method = method,
    period = period,
    knots = knots
  ))

  slopes_long_dt <- data.table::melt(
    data = slopes_dt[
      j = lapply(.SD, mean),
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
    measure.vars = patterns(pred_period = "^pred_period_", slope = "^slope_")
  )[
    j = params := sub(".*_", "", grep("^slope_", names(slopes_dt), value = TRUE))[as.numeric(variable)]
  ][
    j = params := factor(params, levels = unique(params))
  ][
    j = c("start", "end") := data.table::tstrsplit(
      x = sub("^slope_", "", params),
      split = "--"
    )
  ][
    j = c("start", "end") := lapply(.SD, as.numeric),
    .SDcols = c("start", "end")
  ][
    j = yend := pred_period + end * slope
  ]

  if (any(grepl("log", all.names(fit[["terms"]][[2]])))) {
    f <- exp
    yscale <- ggplot2::scale_y_log10
  } else {
    f <- identity
    yscale <- ggplot2::scale_y_continuous
  }

  ggplot2::ggplot(
    data = pheno_dt[
      j = params := lapply(.SD, function(x) {
        as.character(cut(
          x = x,
          breaks = unique(unlist(slopes_long_dt[j = c("start", "end")])),
          include.lowest = TRUE
        ))
      }),
      .SDcols = c(age_var)
    ][
      j = params := sub(",", "--", gsub("\\(|\\)|\\[|\\]", "", params))
    ][
      j = params := factor(params, levels = unique(params))
    ]
  ) +
    ggplot2::aes(x = .data[[age_var]], y = .data[[bmi_var]]) +
    ggplot2::geom_rect(
      data = unique(slopes_long_dt[j = c("params", "start", "end")]),
      mapping = ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = params),
      alpha = 0.15,
      inherit.aes = FALSE
    ) +
    (
      if (data.table::uniqueN(fit[["groups"]]) < 500) {
        list(
          ggplot2::geom_path(
            mapping = ggplot2::aes(group = factor(.data[[id_var]])),
            colour = "#333333",
            na.rm = TRUE,
            alpha = 0.05,
            show.legend = FALSE
          )
        )
      }
    ) +
    ggplot2::stat_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cr"),
      linetype = 2,
      colour = okabe_ito_palette[6],
      se = FALSE
    ) +
    ggplot2::scale_x_sqrt(
      expand = c(0, 0),
      breaks = unique(unlist(slopes_long_dt[j = c("start", "end")]), use.names = FALSE),
      labels = function(x)  sub("\\.0+", "", x)
    ) +
    yscale() +
    ggplot2::scale_colour_manual(values = okabe_ito_palette[c(1, 3, 5, 7, 2, 4, 6, 8)]) +
    ggplot2::scale_fill_manual(values = okabe_ito_palette[c(1, 3, 5, 7, 2, 4, 6, 8)]) +
    ggplot2::labs(
      x = "AGE (years)",
      y = "BMI (kg/m\u00B2)",
      colour = "Intervals",
      fill = "Intervals"
    ) +
    ggplot2::geom_segment(
      data = slopes_long_dt,
      mapping = ggplot2::aes(
        x = start,
        y = f(pred_period),
        xend = end,
        yend = f(yend),
        colour = params
      ),
      linewidth = 1,
      inherit.aes = FALSE,
      linetype = 1
    )
}
