#' Compute outliers detection in derived parameters from a cubic splines mixed-effects model by `time_model()`.
#'
#' Based on computed area under the curves (_i.e._, `compute_aucs()`)
#' and slopes (_i.e._, `compute_slopes()`) for several intervals using
#' a model fitted by `time_model()`, compute an outlier detection.  
#' For details, see methods `iqr` and `zscore` of `performance::check_outliers()`.
#'
#' @param fit A model object from a statistical model such as
#'   from a call to `time_model()`.
#' @param method The type of model provided in `fit`,
#'   _i.e._, one of `"cubic_slope"`, `"linear_splines"` or `"cubic_splines"`.
#' @param period The intervals knots on which AUCs are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#' @inheritParams predict_bmi
#' @param from A string indicating the type of data to be used for the AP and AR
#'   computation, either "predicted" or "observed". Default is "predicted".
#' @param outlier_method The outlier detection method(s). Default is `"iqr"`. Can be
#'   `"cook"`, `"pareto"`, `"zscore"`, `"zscore_robust"`, `"iqr"`, `"ci"`, `"eti"`,
#'   `"hdi"`, `"bci"`, `"mahalanobis"`, `"mahalanobis_robust"`, `"mcd"`, `"ics"`,
#'   `"optics"` or `"lof"`.
#'  See `performance::check_outliers()` <https://easystats.github.io/performance/reference/check_outliers.html> for details.
#' @param outlier_threshold A list containing the threshold values for each method (_e.g._,
#'   `list('mahalanobis' = 7, 'cook' = 1)`), above which an observation is
#'   considered as outlier. If `NULL`, default values will be used (see
#'   'Details'). If a numeric value is given, it will be used as the threshold
#'   for any of the method run.
#'  See `performance::check_outliers()` <https://easystats.github.io/performance/reference/check_outliers.html> for details.
#'
#' @return A `data.frame` listing the individuals which are not outliers based on several criteria.
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
#'   method = "cubic_splines"
#' )
#' head(compute_outliers(
#'   fit = ls_mod,
#'   method = "cubic_splines",
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)#,
#'   # knots = list(
#'   #   "cubic_slope" = NULL,
#'   #   "linear_splines" = c(5.5, 11),
#'   #   "cubic_splines" = c(1, 8, 12)
#'   # )[[method]]
#' )[Outlier != 0])
compute_outliers <- function(
  fit,
  method,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(5.5, 11),
    "cubic_splines" = c(1, 8, 12)
  )[[method]],
  from = c("predicted", "observed"),
  start = 0.25,
  end = 10,
  step = 0.01,
  filter = NULL,
  outlier_method = "iqr",
  outlier_threshold = list(iqr = 2)
) {
  value <- what <- AP <- AR <- variable <- NULL # no visible binding for global variable from data.table
  from <- match.arg(from, c("predicted", "observed"))
  outlier_method <- match.arg(
    arg = outlier_method,
    choices = c(
      "cook", "pareto", "zscore", "zscore_robust", "iqr", "ci",
      "eti", "hdi", "bci", "mahalanobis", "mahalanobis_robust",
      "mcd", "ics", "optics", "lof"
    )
  )
  apar_dt <- compute_apar(
    fit = fit,
    from = from,
    start = start,
    end = end,
    step = step,
    filter = filter
  )[
    AP | AR
  ][
    j = what := data.table::fifelse(paste(AP, AR) %in% paste(FALSE, TRUE), "AR", "AP")
  ]
  if (nrow(apar_dt) > 0) {
    apar_dt <- data.table::setnames(
      x = data.table::dcast(
        data = apar_dt,
        formula = egg_id ~ what,
        value.var = c("egg_ageyears", "egg_bmi")
      ),
      old = function(x) {
        out <- sapply(strsplit(sub("^egg_", "", x), "_"), function(.x) {
          paste(rev(.x), collapse = "_")
        })
        out[grepl("^egg_id$", x)] <- names(fit[["groups"]])
        out
      }
    )
    long_dt <- data.table::melt.data.table(
      data = data.table::as.data.table(Reduce(
        f = function(x, y) merge(x, y, all = TRUE),
        x = list(
          compute_aucs(fit, method, period, knots),
          compute_slopes(fit, method, period, knots),
          apar_dt
        )
      )),
      id.vars = names(fit[["groups"]])
    )[!is.na(value)]
  } else {
    long_dt <- data.table::melt.data.table(
      data = data.table::as.data.table(Reduce(
        f = function(x, y) merge(x, y, all = TRUE),
        x = list(
          compute_aucs(fit, method, period, knots),
          compute_slopes(fit, method, period, knots)
        )
      )),
      id.vars = names(fit[["groups"]])
    )[!is.na(value)]
  }
  long_dt <- long_dt[
    grep(
      pattern = paste(
        c("^slope_.*$", "^auc_.*$", "^AP_.*", "^AR_.*"),
        collapse = "|"
      ),
      x = variable
    )
  ]
  data.table::rbindlist(
    l = lapply(
      X = split(
        x = long_dt[j = .SD, .SDcols = -"variable"],
        f = droplevels(long_dt[["variable"]])
      ),
      FUN = function(data) {
        cbind.data.frame(
          ID = data[[names(fit[["groups"]])]],
          as.data.frame(performance::check_outliers(
            x = data.table::setDT(data)[j = .SD, .SDcols = -c(names(fit[["groups"]]))],
            method = outlier_method,
            threshold = outlier_threshold
          ))
        )
      }
    ),
    idcol = "parameter"
  )
}
