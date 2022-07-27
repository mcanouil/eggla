#' Compute correlations between AUCs/Slopes derived parameters.
#'
#' Based on computed area under the curves (_i.e._, `compute_aucs()`)
#' and slopes (_i.e._, `compute_slopes()`) for several intervals using
#' a model fitted by `time_model()`, compute the correlations between
#' each intervals derived parameters.
#'
#' @param dt A `data.frame` with AUC or slopes for each individuals/samples.
#'
#' @return A `data.frame` with correlations between each intervals derived parameters.
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
#' aucs <- compute_aucs(
#'   fit = ls_mod,
#'   method = "linear_splines",
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)#,
#'   # knots = list(
#'   #   "cubic_slope" = NULL,
#'   #   "linear_splines" = c(5.5, 11),
#'   #   "cubic_splines" = c(2, 8, 12)
#'   # )[[method]]
#' )
#' compute_correlations(aucs)
compute_correlations <- function(data) {

}
