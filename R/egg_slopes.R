#' Derived slopes from a cubic splines mixed-effects model by `time_model()`.
#'
#' Derived slopes for different intervals based on a fitted
#' cubic splines mixed-effects model from `egg_model()`.
#' This function a specific version of `compute_slopes`
#' designed to work specifically on `egg_model()`.
#'
#' @param fit A model object from a statistical model
#'   such as from a call to `egg_model()`.
#' @param period The intervals knots on which slopes are to be computed.
#'
#' @return A `data.frame` with slopes for each individuals/samples.
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
#' head(
#'   egg_slopes(
#'     fit = res,
#'     period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
#'   )
#' )
egg_slopes <- function(
  fit,
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
) {
  stopifnot(inherits(fit, "lme"))
  knots <- c(2, 8, 12)
  id_var <- names(fit[["groups"]])

  slopes <- matrix(
    data = NA_real_,
    nrow = length(unique(fit$data[[id_var]])),
    ncol = length(period) / 2
  )
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
  pred <- matrix(
    data = NA_real_,
    nrow = length(unique(fit$data[[id_var]])),
    ncol = length(period)
  )
  colnames(pred) <- paste0("pred_period_", round(period, digits = 1))

  fxef <- nlme::fixef(fit)
  fxef <- unname(fxef[
    grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(fxef))
  ])
  rnef <- nlme::ranef(fit)
  rnef <- rnef[, grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(rnef))]

  rnef <- cbind.data.frame(
    as.matrix(rnef),
    matrix(
      data = rep(0, (length(fxef) - ncol(rnef)) * nrow(rnef)),
      nrow = nrow(rnef),
      ncol = length(fxef) - ncol(rnef)
    )
  )

  for (i in seq_along(unique(fit$data[[id_var]]))) {
     coeff <- fxef + as.numeric(rnef[i, ])
    for (j in 1:(length(period) / 2)) {
      x1 <- period[j * 2 - 1]
      y1_tmp <- coeff *
        c(x1^0, x1^1, x1^2, x1^3, (x1 - knots)^3) /
        c(1, 1, 2, rep(6, 4))
      y1 <- sum(y1_tmp[1:(4 + findInterval(x1, knots, left.open = TRUE))])

      x2 <- period[j * 2]
      y2_tmp <- coeff *
        c(x2^0, x2^1, x2^2, x2^3, (x2 - knots)^3) /
        c(1, 1, 2, rep(6, 4))
      y2 <- sum(y2_tmp[1:(4 + findInterval(x2, knots, left.open = TRUE))])

      pred[i, j * 2 - 1] <- y1
      pred[i, j * 2] <- y2
      slopes[i, j] <- (y2 - y1) / (x2 - x1)
    }
  }
  out <- cbind.data.frame(ID = unique(fit$data[[id_var]]), pred, slopes)
  names(out)[1] <- id_var
  out
}
