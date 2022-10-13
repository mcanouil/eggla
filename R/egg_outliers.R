#' Compute outliers detection in AUCs/Slopes derived parameters from a cubic splines mixed-effects model by `egg_model()`.
#'
#' Based on computed area under the curves (_i.e._, `egg_aucs()`)
#' and slopes (_i.e._, `egg_slopes()`) for several intervals using
#' a model fitted by `egg_model()`, compute an outlier detection.  
#' For details, see methods `iqr` and `zscore` of `performance::check_outliers()`.
#'
#' @param fit A model object from a statistical model
#'   such as from a call to `egg_model()`.
#' @param period The intervals knots on which slopes are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#' @inheritParams predict_bmi
#' @param from A string indicating the type of data to be used for the AP and AR
#'   computation, either "predicted" or "observed". Default is "predicted".
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
  knots = c(1, 8, 12),
  from = c("predicted", "observed"),
  start = 0.25,
  end = 10,
  step = 0.05,
  filter = NULL
) {
  value <- what <- AP <- AR <- NULL # no visible binding for global variable from data.table
  from <- match.arg(from, c("predicted", "observed"))
  apar_dt <- data.table::setnames(
    x = data.table::dcast(
      data = compute_apar(
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
      ],
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
        egg_aucs(fit, period, knots),
        egg_slopes(fit, period, knots),
        apar_dt
      )
    )),
    id.vars = names(fit[["groups"]])
  )[!is.na(value)]
  data.table::rbindlist(
    l = lapply(
      X = c(
        list(AUC_global = egg_aucs(fit, period, knots)),
        list(SLOPE_global = egg_slopes(fit, period, knots)),
        list(APAR_global = apar_dt),
        split(
          x = long_dt[j = .SD, .SDcols = -"variable"],
          f = long_dt[["variable"]]
        )
      ),
      FUN = function(data) {
        cbind.data.frame(
          ID = data[[names(fit[["groups"]])]],
          as.data.frame(performance::check_outliers(
            x = data.table::setDT(data)[j = .SD, .SDcols = -c(names(fit[["groups"]]))],
            method = c("iqr", "zscore")
          ))
        )
      }
    ),
    idcol = "parameter"
  )
}
