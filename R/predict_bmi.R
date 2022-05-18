#' Predict BMI for a range of ages from a model fit.
#'
#' Predict BMI values a cubic splines mixed model regression
#' with three splines parametrisation as random effect.
#' This function also works for any model obtained using `time_model()`.
#'
#' @param fit A model object from a statistical model
#'   such as from a call `nlme::lme()`, `time_model()` or `egg_model()`.
#' @param start The start of the time window to compute AP and AR.
#' @param end The end of the time window to compute AP and AR.
#' @param step The step to increment the sequence.
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
#'
#' predict_bmi(res)
predict_bmi <- function(fit, start = 0.25, end = 10, step = 0.05) {
  stopifnot(inherits(fit, "lme"))
  bmi <- egg_ageyears <- egg_bmi <- egg_id <- NULL # no visible binding for global variable from data.table

  id_var <- names(fit[["groups"]])
  model_vars <- all.vars(fit[["terms"]])
  age_var <- grep("age", model_vars, value = TRUE, ignore.case = TRUE)
  bmi_var_pos <- grep("bmi", model_vars, ignore.case = TRUE)
  bmi_var <- model_vars[bmi_var_pos]
  covariates <- setdiff(model_vars, c(id_var, age_var, bmi_var))

  if (any(grepl("log", all.names(fit[["terms"]][[bmi_var_pos + 1]])))) {
    f <- exp
  } else {
    f <- identity
  }

  out <- data.table::setnames(
    x = data.table::data.table(
      egg_id = as.character(unique(fit[["groups"]][[id_var]])),
      egg_ageyears = list(seq(from = start, to = end, by = step))
    ),
    old = c("egg_id", "egg_ageyears"),
    new = c(id_var, age_var)
  )[
    data.table::as.data.table(fit[["data"]])[
      j = unique(.SD),
      .SDcols = c(id_var, covariates)
    ][
      j = (id_var) := lapply(.SD, as.character),
      .SDcols = c(id_var)
    ],
    on = id_var
  ][
    j = `names<-`(list(unlist(.SD)), age_var),
    .SDcols = c(age_var),
    by = c(id_var, covariates)
  ][
    j = bmi := f(stats::predict(
      object = fit,
      newdata = .SD,
      interval = "prediction"
    ))
  ]

  data.table::setnames(
    x = out,
    old = c(id_var, age_var, bmi_var, covariates),
    new = c("egg_id", "egg_ageyears", "egg_bmi", sprintf("egg_%s", covariates)),
    skip_absent = TRUE
  )
}
