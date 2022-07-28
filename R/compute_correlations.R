#' Compute the derived parameters correlations from a cubic splines mixed-effects model by `time_model()`.
#'
#' Based on computed area under the curves (_i.e._, `compute_aucs()`)
#' and slopes (_i.e._, `compute_slopes()`) for several intervals using
#' a model fitted by `time_model()`, compute the correlations between
#' each intervals derived parameters.
#'
#' @param fit A model object from a statistical model such as
#'   from a call to `time_model()`.
#' @param method The type of model provided in `fit`,
#'   _i.e._, one of `"cubic_slope"`, `"linear_splines"` or `"cubic_splines"`.
#' @param period The intervals knots on which AUCs are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#'
#' @return A `list` object with correlations between each intervals derived parameters.
#'
#' @export
#'
#' @examples
#' data("bmigrowth")
#' ls_mod <- time_model(
#'   x = "age",
#'   y = "log(bmi)",
#'   cov = NULL,
#'   data = bmigrowth[bmigrowth[["sex"]] == 0, ],
#'   method = "linear_splines"
#' )
#' compute_correlations(
#'   fit = ls_mod,
#'   method = "linear_splines",
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)#,
#'   # knots = list(
#'   #   "cubic_slope" = NULL,
#'   #   "linear_splines" = c(5.5, 11),
#'   #   "cubic_splines" = c(2, 8, 12)
#'   # )[[method]]
#' )
compute_correlations <- function(
  fit,
  method,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(5.5, 11),
    "cubic_splines" = c(2, 8, 12)
  )[[method]]
) {
  lapply(
    X = list(
      AUC = compute_aucs(fit, method, period, knots),
      SLOPE = compute_slopes(fit, method, period, knots)
    ),
    FUN = function(data) {
      corrr::correlate(data[grep("^auc_|^slope_", names(data))])
    }
  )
}
