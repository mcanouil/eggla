
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
#' @inheritParams predict_bmi
#'
#' @return A `data.table` object with correlations between each intervals derived parameters.
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
#'   knots = c(1, 8, 12)
#' )
egg_correlations <- function(
  fit,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = c(1, 8, 12),
  start = 0.25,
  end = 10,
  step = 0.01,
  filter = NULL
) {
  AP <- AR <- what <- NULL # no visible binding for global variable from data.table
  dt <- Reduce(
    f = function(x, y) merge(x, y, by = names(fit[["groups"]]), all = TRUE),
    x = lapply(
      X = list(
        AUC = egg_aucs(fit, period, knots),
        SLOPE = egg_slopes(fit, period, knots),
        APAR = data.table::setnames(
          x = data.table::dcast(
            data = compute_apar(
              fit = fit,
              from = "predicted",
              start = start,
              end = end,
              step = step,
              filter = filter
            )[
              AP | AR
            ][
              j = what := data.table::fifelse(
                test = paste(AP, AR) %in% paste(FALSE, TRUE),
                yes = "AR",
                no = "AP"
              )
            ],
            formula = egg_id ~ what,
            value.var = c("egg_ageyears", "egg_bmi")
          ),
          old = function(x) {
            out <- sapply(strsplit(sub("^egg_", "", x), "_"), function(.x) {
              paste(rev(.x), collapse = "_")
            })
            out[grepl("^egg_id$", x)] <- "egg_id"
            out
          }
        )
      ),
      FUN = function(data) {
        data.table::setnames(data, "egg_id", names(fit[["groups"]]), skip_absent = TRUE)
      }
    )
  )
  data.table::as.data.table(
    x = stats::cor(
      x = dt[grep("^auc_|^slope_|^AP_|^AR_", names(dt))],
      use = "pairwise.complete.obs"
    ),
    keep.rownames = "term"
  )
}
