#' Compute area under the curves for several intervals using a model fitted by `time_model()`.
#'
#' Compute area under the curves for "clubic slope", "linear splines" and "cubic splines"
#' fitted using `time_model()`.
#'
#' @param fit A model object from a statistical model such as
#'   from a call to `time_model()`.
#' @param method The type of model provided in `fit`,
#'   _i.e._, one of `"cubic_slope"`, `"linear_splines"` or `"cubic_splines"`.
#' @param period The intervals knots on which AUCs are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#'
#' @return A `data.frame` with AUC for each individuals/samples.
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
#' head(compute_aucs(
#'   fit = ls_mod,
#'   method = "linear_splines",
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)#,
#'   # knots = list(
#'   #   "cubic_slope" = NULL,
#'   #   "linear_splines" = c(0.75, 5.5, 11),
#'   #   "cubic_splines" = c(1, 8, 12)
#'   # )[[method]]
#' ))
compute_aucs <- function(
  fit,
  method,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(0.75, 5.5, 11),
    "cubic_splines" = c(1, 8, 12)
  )[[method]]
) {
  stopifnot(inherits(fit, "lme"))
  id_var <- names(fit[["groups"]])

  pred_auc <- matrix(
    data = NA_real_,
    nrow = length(unique(fit[["data"]][[id_var]])),
    ncol = length(period) / 2,
    dimnames = list(
      as.character(unique(fit[["data"]][[id_var]])),
      paste0(
        "auc_",
        sapply(split(
          x = period,
          f = rep(
            x = seq(1, length(period), length(period) %/% 4),
            each = length(period) %/% 4
          )
        ), paste, collapse = "--")
      )
    )
  )

  fxef <- nlme::fixef(fit)
  fxef <- unname(fxef[grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(fxef))])
  rnef <- nlme::ranef(fit)
  rnef <- rnef[, grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(rnef))]

  switch(
    EXPR = as.character(method),
    "cubic_slope" = {
      y <- function(x, coeff, knots) {
        sapply(
          X = x,
          FUN = function(x) sum(coeff * mapply("^", rep(x, length(coeff)), seq_along(coeff) - 1))
        )
      }
      for (i in as.character(unique(fit[["data"]][[id_var]]))) {
        coeff <- fxef + as.numeric(rnef[i, ]) # this implies fixed = random
        for (j in seq_len(length(period) / 2)) {
          pred_auc[i, j] <- stats::integrate(
            f = y,
            coeff = coeff,
            lower = period[j * 2 - 1],
            upper = period[j * 2]
          )[["value"]]
        }
      }
    },
    "linear_splines" = {
      y <- function(x, coeff, knots) {
        sapply(
          X = x,
          FUN = function(x) {
            x_pos <- findInterval(x, knots, left.open = TRUE)
            sum(c(coeff * c(1, x - c(0, knots)))[seq_len(x_pos + 2)])
          }
        )
      }
      for (i in as.character(unique(fit[["data"]][[id_var]]))) {
        coeff <- fxef + as.numeric(rnef[i, ]) # this implies fixed = random
        for (j in seq_len(length(period) / 2)) {
           pred_auc[i, j] <- stats::integrate(
             f = y,
             coeff = coeff,
             knots = knots,
             lower = period[j * 2 - 1],
             upper = period[j * 2]
            )[["value"]]
        }
      }
    },
    "cubic_splines" = {
      y <- function(x, coeff, knots) {
        sapply(
          X = x,
          FUN = function(x) {
            y_tmp <- coeff * c(x^0, x^1, x^2, x^3, (x - knots)^3) / c(1, 1, 2, rep(6, length(knots) + 1))
            sum(y_tmp[seq_len(4 + findInterval(x, knots, left.open = TRUE))])
          }
        )
      }
      for (i in as.character(unique(fit[["data"]][[id_var]]))) {
        coeff <- fxef + as.numeric(rnef[i, ]) # this implies fixed = random
        for (j in seq_len(length(period) / 2)) {
          pred_auc[i, j] <- stats::integrate(
            f = y,
            coeff = coeff,
            knots = knots,
            lower = period[j * 2 - 1],
            upper = period[j * 2]
          )[["value"]]
        }
      }
    }
  )

  out <- cbind.data.frame(Row.names = rownames(pred_auc), pred_auc)
  out <- out[order(out[["Row.names"]]), ]
  names(out)[grepl("Row.names", names(out), fixed = TRUE)] <- id_var
  out
}
