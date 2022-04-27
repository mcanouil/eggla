#' Compute adiposity peak (AP) and adiposity rebound (AR).
#'
#' @param fit A model object from a statistical model
#'   such as from a call `nlme::lme()`, `time_model()` or `egg_model()`.
#' @param from A string indicating the type of data to be used for the AP and AR
#'   computation, either "predicted" or "observed". Default is "predicted".
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
#'       scale_colour_manual(values = c("#b22222", "#22b222")) +
#'       theme_minimal() +
#'       theme(plot.title.position = "plot") +
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
compute_apar <- function(fit, from = c("predicted", "observed"), start = 0.25, end = 10, step = 0.05) {
  stopifnot(inherits(fit, "lme"))
  match.arg(from, c("predicted", "observed"))
  AP <- AR <- bmi <- egg_ageyears <- egg_bmi <- egg_id <- NULL # no visible binding for global variable from data.table

  id_var <- names(fit[["groups"]])
  age_var <- grep("age", all.vars(fit[["terms"]]), value = TRUE, ignore.case = TRUE)
  bmi_var_pos <- grep("bmi", all.vars(fit[["terms"]]), ignore.case = TRUE)
  bmi_var <- all.vars(fit[["terms"]])[bmi_var_pos]

  if (grep("log", all.names(fit[["terms"]][[bmi_var_pos + 1]]))) {
    f <- exp
  } else {
    f <- identity
  }

  out <- switch(EXPR = from,
    "observed" = {
      data.table::as.data.table(fit[["data"]])[
        j = .SD,
        .SDcols = c(id_var, age_var, bmi_var)
      ]
    },
    "predicted" = {
      data.table::setnames(
        x = data.table::data.table(
          egg_id = unique(fit[["groups"]][[id_var]]),
          egg_ageyears = list(seq(from = start, to = end, by = step))
        ),
        old = c("egg_id", "egg_ageyears"),
        new = c(id_var, age_var)
      )[
        j = `names<-`(list(unlist(.SD)), age_var),
        .SDcols = c(age_var),
        by = c(id_var)
      ][
        j = bmi := f(stats::predict(
          object = fit,
          newdata = .SD,
          interval = "prediction"
        ))
      ]
    }
  )

  data.table::setnames(
    x = out,
    old = c(id_var, age_var, bmi_var),
    new = c("egg_id", "egg_ageyears", "egg_bmi")
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
  ]
}
