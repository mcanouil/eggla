#' compute_auc
#'
#' @param fit something
#' @param method something
#' @param period something
#' @param knots something
#'
#' @return something
#' @export
compute_auc <- function(
  fit,
  method,
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(5.5, 11),
    "cubic_splines" = c(2, 8, 12)
  )[[method]]
) {
  pred_auc <- matrix(data = NA_real_, nrow = length(unique(fit$data[["ID"]])), ncol = length(period) / 2)
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

  fxef <- as.numeric(fixef(fit))
  rnef <- ranef(fit)

  switch(
    EXPR = as.character(method),
    "cubic_slope" = {
      y <- function(x, coeff) {
        sapply(
          X = x,
          FUN = function(x) sum(coeff * mapply("^", rep(x, length(coeff)), seq_along(coeff) - 1))
        )
      }
      for (i in seq_along(unique(fit$data[["ID"]]))) {
        coeff <- fxef + as.numeric(rnef[i, ])
        for (j in 1:(length(period) / 2)) {
           pred_auc[i, j] <- integrate(f = y, coeff = coeff, lower = period[j * 2 - 1], upper = period[j * 2])$value
        }
      }
      cbind.data.frame(ID = unique(fit$data[["ID"]]), pred_auc)
    },
    "linear_splines" = {
      y <- function(x, coeff, knots) {
        sapply(
          X = x,
          FUN = function(x) {
            x_pos <- findInterval(x, knots, left.open = TRUE)
            sum(c(coeff * c(1, x_pos - c(0, knots)))[1:(x_pos + 2)])
          }
        )
      }
      for (i in seq_along(unique(fit$data[["ID"]]))) {
        coeff <- fxef + as.numeric(rnef[i, ])
        for (j in 1:(length(period) / 2)) {
           pred_auc[i, j] <- integrate(
             f = y,
             coeff = coeff,
             knots = knots,
             lower = period[j * 2 - 1],
             upper = period[j * 2]
            )$value
        }
      }
      cbind.data.frame(ID = unique(fit$data[["ID"]]), pred_auc)
    },
    "cubic_splines" = {
      y <- function(x, coeff, knots) {
        sapply(
          X = x,
          FUN = function(x) {
            y_tmp <- coeff * c(x^0, x^1, x^2, x^3, (x - knots)^3) / c(1, 1, 2, rep(6, 4))
            y <- sum(y_tmp[1:(4 + findInterval(x, knots, left.open = TRUE))])
          }
        )
      }
      for (i in seq_along(unique(fit$data[["ID"]]))) {
        coeff <- fxef + as.numeric(rnef[i, ])
        for (j in 1:(length(period) / 2)) {
           pred_auc[i, j] <- integrate(
             f = y,
             coeff = coeff,
             knots = knots,
             lower = period[j * 2 - 1],
             upper = period[j * 2]
            )$value
        }
      }
      cbind.data.frame(ID = unique(fit$data[["ID"]]), pred_auc)
    }
  )
}
