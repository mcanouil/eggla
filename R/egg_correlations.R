
#' Compute the derived parameters correlations from a cubic splines mixed-effects model by `egg_model()`.
#'
#' Based on computed area under the curves (_i.e._, `egg_aucs()`)
#' and slopes (_i.e._, `egg_slopes()`) for several intervals using
#' a model fitted by `egg_model()`, compute the correlations between
#' each intervals derived parameters.
#'
#' @param fit A model object from a statistical model
#'   such as from a call to `egg_model()`.
#' @param period The intervals knots on which slopes are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#'
#' @return A `ggplot2` object with correlations between each intervals derived parameters.
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
#' egg_correlations(
#'   fit = res,
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
#'   knots = c(2, 8, 12)
#' )
egg_correlations <- function(
  fit,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = c(2, 8, 12)
) {
pl <- lapply(
    X = list(
      egg_aucs(fit, period, knots),
      egg_slopes(fit, period, knots)
    ),
    FUN = function(data) {
      data_corrr_fmt <- data_corrr <- corrr::correlate(data[grep("^auc_|^slope_", names(data))])
      data_corrr_fmt[, -1] <- round(data_corrr_fmt[, -1], digits = 3)
      patchwork::wrap_plots(
        gridExtra::tableGrob(data_corrr_fmt),
        corrr::network_plot(
          rdf = data_corrr,
          min_cor = 0,
          colors = c("#b22222", "#22b222")
        ) +
        ggplot2::theme(legend.position = "top") +
        ggplot2::guides(
          color = ggplot2::guide_colourbar(
            barwidth = ggplot2::unit(0.25, "npc"),
            barheight = ggplot2::unit(0.05, "npc")
          )
        ) +
        ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "lines")),
        ncol = 1,
        heights = c(0.3, 0.70)
      )
    }
  )
  patchwork::wrap_plots(pl, ncol = 2, nrow = 1)
}
