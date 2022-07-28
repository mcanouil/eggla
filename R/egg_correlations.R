
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
  lapply(
    X = list(
      AUC = egg_aucs(fit, period, knots),
      SLOPE = egg_slopes(fit, period, knots)
    ),
    FUN = function(data) {
      corrr::correlate(data[grep("^auc_|^slope_", names(data))])
    }
  )
}
