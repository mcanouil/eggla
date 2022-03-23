#' Compute adiposity peak (AP) and adiposity rebound (AR).
#'
#' @param fit A model object from a statistical model
#'   such as from a call `nlme::lme()` and `time_model()`.
#' @param id_var A length one character vector 
#'   with the variable name definiing individuals.
#' @param age_var A length one character vector
#'   with the variable name definiing the time variable, _i.e._, age).
#'
#' @return A `data.table` object.
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
#' res_apar <- compute_apar(
#'   fit = res,
#'   id_var = "ID",
#'   age_var = "age"
#' )
#'
#' head(res_apar[AP | AR])
compute_apar <- function(fit, id_var, age_var) {
    bmi_pred <- egg_ageyears <- log_bmi_pred <- NULL
    out <- data.table::setnames(
      x = data.table::data.table(
        egg_id = unique(fit[["groups"]][[id_var]]),
        egg_ageyears = list(seq(from = 0.25, to = 10, by = 0.05))
      ),
      old = c("egg_id", "egg_ageyears"),
      new = c(id_var, age_var)
    )[
      j = `names<-`(list(unlist(.SD)), age_var),
      .SDcols = c(age_var),
      by = c(id_var)
    ][
      j = log_bmi_pred := stats::predict(
        object = fit,
        newdata = .SD,
        interval = "prediction"
      )
    ][
      j = bmi_pred := exp(log_bmi_pred)
    ]
    data.table::setnames(
      x = out,
      old = c(id_var, age_var),
      new = c("egg_id", "egg_ageyears")
    )[
      j = `:=`(
        AP = egg_ageyears %in% egg_ageyears[which(diff(sign(diff(bmi_pred))) == -2) + 1],
        AR = egg_ageyears %in% egg_ageyears[which(diff(sign(diff(bmi_pred))) == +2) + 1]
      ),
      by = "egg_id"
    ]
  }
