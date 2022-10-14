#' Plot derived area under the curves from a model fitted by `egg_model()`.
#'
#' @inheritParams egg_aucs
#'
#' @return A `patchwork` `ggplot2` object.
#'
#' @export
#'
#' @examples
#' data("bmigrowth")
#' res <- egg_model(
#'   formula = log(bmi) ~ age,
#'   data = bmigrowth[bmigrowth[["sex"]] == 0, ],
#'   id_var = "ID",
#'   random_complexity = 1
#' )
#' plot_egg_aucs(
#'   fit = res,
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
#' ) +
#'   theme_minimal() +
#'   theme(
#'     axis.line = element_line(colour = "grey20", size = rel(1)),
#'     axis.ticks = element_line(colour = "grey20"),
#'     panel.grid.minor.x = element_blank(),
#'     panel.grid.minor.y = element_blank(),
#'     panel.grid.major.x = element_blank()
#'   )
plot_egg_aucs <- function(fit, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17), knots = c(1, 8, 12)) {
  period_interval <- patterns <- auc <- NULL # no visible binding for global variable from data.table
  stopifnot(inherits(fit, "lme"))
  id_var <- names(fit[["groups"]])

  auc_dt <- data.table::as.data.table(egg_aucs(
    fit = fit,
    period = period,
    knots = knots
  ))

  if (nzchar(system.file(package = "ggdist")) & nzchar(system.file(package = "ggbeeswarm"))) {
    gpl <- list(
      ggdist::stat_halfeye(
        mapping = ggplot2::aes(fill = period_interval),
        justification = -0.20,
        .width = 0,
        scale = 1
      ),
      ggbeeswarm::geom_quasirandom(
        mapping = ggplot2::aes(fill = period_interval, colour = period_interval),
        shape = 21,
        alpha = 0.10,
        groupOnX = TRUE,
        width = 0.15
      )
    )
  } else {
    gpl <- list(
      ggplot2::geom_point(
        mapping = ggplot2::aes(fill = period_interval, colour = period_interval),
        position = ggplot2::position_jitter(width = 0.15),
        shape = 21,
        alpha = 0.05
      )
    )
  }

  ggplot2::ggplot(
    data = data.table::melt(
      data = auc_dt,
      id.vars = id_var,
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
        scales::viridis_pal(
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
    ggplot2::aes(x = period_interval, y = auc) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(colour = period_interval),
      width = 0.25,
      outlier.colour = NA
    ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", end = 0.85) +
    ggplot2::scale_fill_viridis_d(option = "plasma", end = 0.85) +
    ggplot2::labs(
      x = "Period Interval (years)",
      y = "Area Under The Curve (AUC)",
      colour = "Intervals",
      fill = "Intervals"
    ) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(period_interval),
      scales = "free",
      ncol = 4,
      nrow = 2
    ) +
    ggplot2::theme(
      strip.text = ggtext::element_markdown(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    gpl
}
