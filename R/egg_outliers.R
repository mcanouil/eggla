#' Compute outliers detection in AUCs/Slopes derived parameters from a cubic splines mixed-effects model by `egg_model()`.
#'
#' Based on computed area under the curves (_i.e._, `egg_aucs()`)
#' and slopes (_i.e._, `egg_slopes()`) for several intervals using
#' a model fitted by `egg_model()`, compute an outlier detection.
#'
#' @param fit A model object from a statistical model
#'   such as from a call to `egg_model()`.
#' @param period The intervals knots on which slopes are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#'
#' @return A `data.frame` listing the individuals which are not outliers based on several criteria.
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
#' head(egg_outliers(
#'   fit = res,
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
#'   knots = c(1, 8, 12)
#' )[Outlier != 0])
egg_outliers <- function(
  fit,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = c(1, 8, 12)
) {
  data.table::rbindlist(
    l = lapply(
      X = list(
        AUC = egg_aucs(fit, period, knots),
        SLOPE = egg_slopes(fit, period, knots)
      ),
      FUN = function(data) {
        cbind.data.frame(
          ID = data[, 1],
          as.data.frame(performance::check_outliers(
            x = data[, -1],
            method = c("iqr", "zscore")
          ))
        )
      }
    ),
    idcol = "parameter"
  )
}
