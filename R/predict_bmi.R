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
#' @param filter A string following `data.table` syntax for filtering on `"i"`
#'   (_i.e._, row elements), _e.g._, `filter = "source == 'A'"`.
#'   Default is `NULL`.
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
#' predict_bmi(res)[]
#'
#' ## For multiple sources of measures or multiple measures at one age
#' set.seed(1234)
#' dta <- bmigrowth[bmigrowth[["sex"]] == 0, ]
#' dta[["source"]] <- c("A", "B")[rbinom(n = nrow(dta), size = 1, prob = 0.65) + 1]
#'
#' res <- egg_model(
#'   formula = log(bmi) ~ age + source,
#'   data = dta,
#'   id_var = "ID",
#'   random_complexity = 1
#' )
#'
#' predict_bmi(res)[order(egg_id, egg_ageyears)]
#'
#' predict_bmi(res, filter = "source == 'A'")[order(egg_id, egg_ageyears)]
predict_bmi <- function(fit, start = 0.25, end = 10, step = 0.05, filter = NULL) {
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

  if (!is.null(filter) && is.character(filter)) {
    out <- try(
      expr = eval(parse(text = sprintf("out[i = %s, j = .SD]", filter))),
      silent = TRUE
    )
    if (inherits(out, "try-error")) {
      stop("\"filter\" argument malformed! Please ensure, it follows `data.table` syntax for \"i\" (i.e., row elements).")
    }
  }

  data.table::setnames(
    x = out,
    old = c(id_var, age_var, bmi_var, covariates),
    new = c("egg_id", "egg_ageyears", "egg_bmi", sprintf("egg_%s", covariates)),
    skip_absent = TRUE
  )

  if (
    sum(
      out[
        j = list(id_not_unique = anyDuplicated(egg_ageyears)),
        by = c("egg_id")
      ][["id_not_unique"]]
    ) > 0
  ) {
    warning(paste(
      "Multiple BMI measures (for the same age) have been detected and are aggregated using geometric mean!",
      "Use \"filter\" (or \"filter_apar\" in `run_eggla_lmm()`) parameter to apply some filtering, e.g., filter = \"source == 'clinic'\".",
      sep = "\n"
    ))
    out <- out[
      j = list(egg_bmi = exp(mean(log(egg_bmi)))),
      by = c("egg_id", "egg_ageyears")
    ]
  }

  out
}
