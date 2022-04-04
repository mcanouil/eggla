#' Derived areas under the curve from a cubic splines mixed-effects model by `time_model()`.
#'
#' Derived areas under the curve (AUCs) for differentintervals based
#' on a fitted cubic splines mixed-effects model from `egg_model()`. 
#' This function is a specific version of `compute_aucs`
#' designed to work specifically on `egg_model()`.
#'
#' @param fit A model object from a statistical model
#' such as from a call to `egg_model()`.
#' @param period The intervals knots on which AUCs are to be computed.
#'
#' @return A `data.frame` with AUC for each individuals/samples.
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
#'   egg_aucs(
#'     fit = res,
#'     period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
#'   )
#' )
egg_aucs <- function(
  fit,
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
) {
  stopifnot(inherits(fit, "lme4"))
  knots <- c(2, 8, 12)
  id_var <- names(fit[["groups"]])

  pred_auc <- matrix(
    data = NA_real_,
    nrow = length(unique(fit$data[[id_var]])),
    ncol = length(period) / 2
  )
  colnames(pred_auc) <- paste0(
    "auc_",
    sapply(split(
      x = period,
      f = rep(
        x = seq(1, length(period), length(period) %/% 4),
        each = length(period) %/% 4
      )
    ), paste, collapse = "--")
  )

  fxef <- lme4::fixef(fit)
  fxef <- unname(fxef[
    grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(fxef))
  ])
  rnef <- lme4::ranef(fit)
  rnef <- rnef[, grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(rnef))]

  rnef <- cbind(
    rnef,
    matrix(
      data = rep(0, (length(fxef) - ncol(rnef)) * nrow(rnef)),
      nrow = nrow(rnef),
      ncol = length(fxef) - ncol(rnef)
    )
  )

  y <- function(x, coeff, knots) {
    sapply(
      X = x,
      FUN = function(x) {
        y_tmp <- coeff *
          c(x^0, x^1, x^2, x^3, (x - knots)^3) /
          c(1, 1, 2, rep(6, 4))
        sum(y_tmp[1:(4 + findInterval(x, knots, left.open = TRUE))])
      }
    )
  }
  for (i in seq_along(unique(fit$data[[id_var]]))) {
    coeff <- fxef + as.numeric(rnef[i, ])
    for (j in 1:(length(period) / 2)) {
        pred_auc[i, j] <- stats::integrate(
          f = y,
          coeff = coeff,
          knots = knots,
          lower = period[j * 2 - 1],
          upper = period[j * 2]
        )$value
    }
  }
  out <- cbind.data.frame(ID = unique(fit$data[[id_var]]), pred_auc)
  names(out)[1] <- id_var
  out
}
