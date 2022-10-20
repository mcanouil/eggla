#' Compute adiposity peak (AP) and adiposity rebound (AR).
#'
#' @inheritParams predict_bmi
#' @param from A string indicating the type of data to be used for the AP and AR
#'   computation, either "predicted" or "observed". Default is "predicted".
#'
#' @return A `data.table` object.
#'
#' @export
#'
#' @examples
#' library(eggla)
#' data("bmigrowth")
#' res <- egg_model(
#'   formula = log(bmi) ~ age,
#'   data = bmigrowth[bmigrowth[["sex"]] == 0, ],
#'   id_var = "ID",
#'   random_complexity = 1
#' )
#'
#' head(compute_apar(fit = res, from = "predicted")[AP | AR])
#'
#' # Comparing observed and predicted values
#' library(data.table)
#' library(ggplot2)
#' library(patchwork)
#' list_gg <- melt(
#'   data = rbindlist(
#'     l = lapply(
#'       X = (function(.x) `names<-`(.x, .x))(c("predicted", "observed")),
#'       FUN = compute_apar,
#'       fit = res
#'     ),
#'     idcol = "from"
#'   )[
#'     AP | AR
#'   ][
#'     j = what := fifelse(paste(AP, AR) %in% paste(FALSE, TRUE), "AR", "AP")
#'   ],
#'   id.vars = c("from", "egg_id", "what"),
#'   measure.vars = c("egg_ageyears", "egg_bmi")
#' )[
#'   j = list(gg = list({
#'     dt <- dcast(data = .SD, formula = egg_id + what ~ from)
#'     range_xy <- range(dt[, c("observed", "predicted")], na.rm = TRUE)
#'     ggplot(data = dt) +
#'       aes(x = observed, y = predicted, colour = what) +
#'       geom_abline(intercept = 0, slope = 1) +
#'       geom_segment(aes(xend = observed, yend = observed), alpha = 0.5) +
#'       geom_point() +
#'       scale_colour_manual(values = c("#E69F00FF", "#56B4E9FF")) +
#'       labs(
#'         x = sprintf("Observed: %s", sub(".*_", "", toupper(variable))),
#'         y = sprintf("Predicted: %s", sub(".*_", "", toupper(variable))),
#'         colour = NULL,
#'         title = sub(".*_", "", toupper(variable))
#'       ) +
#'       coord_cartesian(xlim = range_xy, ylim = range_xy)
#'   })),
#'   by = "variable"
#' ]
#' wrap_plots(list_gg[["gg"]], guides = "collect")
compute_apar <- function(fit, from = c("predicted", "observed"), start = 0.25, end = 10, step = 0.05, filter = NULL) {
  stopifnot(inherits(fit, "lme"))
  match.arg(from, c("predicted", "observed"))
  AP <- AR <- egg_ageyears <- egg_bmi <- egg_id <- apar <- NULL # no visible binding for global variable from data.table

  id_var <- names(fit[["groups"]])
  model_vars <- all.vars(fit[["terms"]])
  age_var <- grep("age", model_vars, value = TRUE, ignore.case = TRUE)
  bmi_var_pos <- grep("bmi", model_vars, ignore.case = TRUE)
  bmi_var <- model_vars[bmi_var_pos]
  covariates <- setdiff(model_vars, c(id_var, age_var, bmi_var))

  out <- data.table::setnames(
    x = switch(EXPR = from,
      "observed" = {
        data.table::as.data.table(fit[["data"]])[
          j = .SD,
          .SDcols = c(id_var, age_var, bmi_var)
        ]
      },
      "predicted" = {
        predict_bmi(fit = fit, start = start, end = end, step = step, filter = filter)
      }
    ),
    old = c(id_var, age_var, bmi_var, covariates),
    new = c("egg_id", "egg_ageyears", "egg_bmi", sprintf("egg_%s", covariates)),
    skip_absent = TRUE
  )[
    j = `:=`(
      AP = egg_ageyears %in% egg_ageyears[which(diff(sign(diff(egg_bmi))) == -2) + 1],
      AR = egg_ageyears %in% egg_ageyears[which(diff(sign(diff(egg_bmi))) == +2) + 1]
    ),
    by = "egg_id"
  ][
    order(egg_id, egg_ageyears)
  ][
    i = (AP),
    j = AP := AP & !duplicated(AP),
    by = "egg_id"
  ][
    i = (AR),
    j = AR := AR & !duplicated(AR),
    by = "egg_id"
  ][
    i = egg_ageyears >= 2 & AP,
    j = `:=`("AP" = FALSE)
  ]

  bad_estimated_individuals <- data.table::dcast(
    data = out[
      (AP | AR)
    ][
      j = apar := c("10" = "AP", "01" = "AR")[paste0(as.integer(AP), as.integer(AR))]
    ],
    formula = egg_id ~ apar,
    value.var = "egg_ageyears"
  )[AP >= AR, .SD, .SDcols = c("egg_id")]

  out[
    i = egg_id %in% bad_estimated_individuals,
    j = `:=`("AP" = FALSE, "AR" = FALSE)
  ]

  out
}
