#' Predict average slopes for several intervals using a model fitted by `time_model()`.
#'
#' Comoute average slopes for "clubic slope", "linear splines" and "cubic splines"
#' fitted using `time_model()`.
#'
#' @param fit A model object from a statistical model such as from a call to `time_model()`.
#' @param method The type of model provided in `fit`,
#'   _i.e._, one of `"cubic_slope"`, `"linear_splines"` or `"cubic_splines"`.
#' @param period The intervals knots on which slopes are to be computed.
#' @param knots The knots as defined `fit` and according to `method`.
#'
#' @return A `data.frame` with slopes for each individuals/samples.
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
#' head(compute_slopes(
#'   fit = ls_mod,
#'   method = "linear_splines",
#'   period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)#,
#'   # knots = list(
#'   #   "cubic_slope" = NULL,
#'   #   "linear_splines" = c(5.5, 11),
#'   #   "cubic_splines" = c(1, 8, 12)
#'   # )[[method]]
#' ))
compute_slopes <- function(
  fit,
  method,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(5.5, 11),
    "cubic_splines" = c(1, 8, 12)
  )[[method]]
) {
  stopifnot(inherits(fit, "lme"))
  id_var <- names(fit[["groups"]])
  slopes <- matrix(data = NA_real_, nrow = length(unique(fit$data[[id_var]])), ncol = length(period) / 2)
  colnames(slopes) <- paste0(
    "slope_",
    sapply(split(
      x = period,
      f = rep(
        x = seq(1, length(period), length(period) %/% 4),
        each = length(period) %/% 4
      )
    ), paste, collapse = "--")
  )
  pred <- matrix(data = NA_real_, nrow = length(unique(fit$data[[id_var]])), ncol = length(period))
  colnames(pred) <- paste0("pred_period_", round(period, digits = 1))

  fxef <- nlme::fixef(fit)
  fxef <- unname(fxef[grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(fxef))])
  rnef <- nlme::ranef(fit)
  rnef <- rnef[, grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(rnef))]

  out <- switch(
    EXPR = as.character(method),
    "cubic_slope" = {
      for (i in seq_along(unique(fit$data[[id_var]]))) {
        coeff <- fxef + as.numeric(rnef[i, ]) # this implies fixed = random
        for (j in 1:(length(period) / 2)) {
          x1 <- period[j * 2 - 1]
          y1 <- sum(coeff * mapply("^", rep(x1, length(coeff)), seq_along(coeff) - 1))

          x2 <- period[j * 2]
          y2 <- sum(coeff * mapply("^", rep(x2, length(coeff)), seq_along(coeff) - 1))

          pred[i, j * 2 - 1] <- y1
          pred[i, j * 2] <- y2
          slopes[i, j] <- (y2 - y1) / (x2 - x1)
        }
      }
      cbind.data.frame(ID = unique(fit$data[[id_var]]), pred, slopes)
    },
    "linear_splines" = {
      for (i in seq_along(unique(fit$data[[id_var]]))) {
        coeff <- fxef + as.numeric(rnef[i, ]) # this implies fixed = random
        for (j in 1:(length(period) / 2)) {
          x1 <- period[j * 2 - 1]
          x1_pos <- findInterval(x1, knots, left.open = TRUE)
          y1 <- sum(c(coeff * c(1, x1 - c(0, knots)))[1:(x1_pos + 2)])

          x2 <- period[j * 2]
          x2_pos <- findInterval(x2, knots, left.open = TRUE)
          y2 <- sum(c(coeff * c(1, x2 - c(0, knots)))[1:(x2_pos + 2)])

          pred[i, j * 2 - 1] <- y1
          pred[i, j * 2] <- y2
          slopes[i, j] <- (y2 - y1) / (x2 - x1)
        }
      }
      cbind.data.frame(ID = unique(fit$data[[id_var]]), pred, slopes)
    },
    "cubic_splines" = {
      for (i in seq_along(unique(fit$data[[id_var]]))) {
        coeff <- fxef + as.numeric(rnef[i, ]) # this implies fixed = random
        for (j in 1:(length(period) / 2)) {
          x1 <- period[j * 2 - 1]
          y1_tmp <- coeff * c(x1^0, x1^1, x1^2, x1^3, (x1 - knots)^3) / c(1, 1, 2, rep(6, 4))
          y1 <- sum(y1_tmp[1:(4 + findInterval(x1, knots, left.open = TRUE))])

          x2 <- period[j * 2]
          y2_tmp <- coeff * c(x2^0, x2^1, x2^2, x2^3, (x2 - knots)^3) / c(1, 1, 2, rep(6, 4))
          y2 <- sum(y2_tmp[1:(4 + findInterval(x2, knots, left.open = TRUE))])

          pred[i, j * 2 - 1] <- y1
          pred[i, j * 2] <- y2
          slopes[i, j] <- (y2 - y1) / (x2 - x1)
        }
      }
      cbind.data.frame(ID = unique(fit$data[[id_var]]), pred, slopes)
    }
  )

  names(out)[1] <- id_var
  out
}
