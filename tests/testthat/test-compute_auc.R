test_that("Cubic slope", {

  x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "cubic_slope")

  y1 <- compute_aucs(
    fit = x,
    method = "cubic_slope",
    period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
  )

  y2 <- local({
    suppressWarnings({
      pred_auc_lme <- data.frame()
      auc_lme <- function(fit, data, period) {
        fxef <- as.numeric(nlme::fixef(fit))
        for (j in 1:(length(period) / 2)) {
          for (i in seq_len(nrow(data))) {
            coeff <- fxef + c(as.numeric(nlme::ranef(fit)[i, ]))
            x1 <- period[j * 2 - 1]
            x2 <- period[j * 2]
            y <- function(x1) {
              coeff[1] + coeff[2] * x1 + coeff[3] * x1^2 + coeff[4] * x1^3
            }
            pred_auc_lme[i, j] <- stats::integrate(y, lower = x1, upper = x2)$value
          }
        }
        out <- data.frame(cbind(data[, 1], pred_auc_lme))
        out
      }
      auc_lme_female <- auc_lme(
        x,
        unique(bmigrowth[bmigrowth$sex == 0, "ID", drop = FALSE]),
        c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
      )
      names(auc_lme_female) <- c("ID", "AUC_Infancy", "AUC_Childhood", "AUC_PrePubertal", "AUC_PostPubertal")
      auc_lme_female
    })
  })

  expect_equal(y1, y2, ignore_attr = TRUE)
})

test_that("Linear Splines", {

  x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "linear_splines")

  y1 <- compute_aucs(
    fit = x,
    method = "linear_splines",
    period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
  )

  y2 <- local({
    suppressWarnings({
      k1 <- 5.5
      k2 <- 11
      pred_auc_lsplines <- data.frame()
      auc_lsplines <- function(fit, data, period) {
        fxef <- as.numeric(nlme::fixef(fit))
        for (j in seq_len(length(period) / 2)) {
          for (i in seq_len(nrow(data))) {
            coeff <- fxef + c(as.numeric(nlme::ranef(fit)[i, ]))
            x1 <- period[j * 2 - 1]
            x2 <- period[j * 2]
            if (x1 <= k1 & x2 <= k1) {
              y <- function(x1) {
                coeff[1] + coeff[2] * x1
              }
            } else {
              if ((x1 > k1 & x1 <= k2) & (x2 > k1 & x2 <= k2)) {
                y <- function(x1) {
                  coeff[1] + coeff[2] * x1 + coeff[3] * (x1 - k1)
                }
              } else {
                y <- function(x1) {
                  coeff[1] + coeff[2] * x1 + coeff[3] * (x1 - k1) + coeff[4] * (x1 - k2)
                }
              }
            }
            pred_auc_lsplines[i, j] <- stats::integrate(y, lower = x1, upper = x2)$value
          }
        }
        out <- data.frame(cbind(data[, 1], pred_auc_lsplines))
        out
      }
      auc_lsplines_female <- auc_lsplines(
        x,
        unique(bmigrowth[bmigrowth$sex == 0, "ID", drop = FALSE]),
        c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
      )
      names(auc_lsplines_female) <- c("ID", "AUC_Infancy", "AUC_Childhood", "AUC_PrePubertal", "AUC_PostPubertal")
      auc_lsplines_female
    })
  })

  expect_equal(y1, y2, ignore_attr = TRUE)
})

# test_that("Cubic Splines", {
#   x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "cubic_splines")
#
#   y1 <- compute_aucs(
#     fit = x,
#     method = "cubic_splines",
#     period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
#   )
#
#   y2 <- local({
#     suppressWarnings({
#       k1 <- 2
#       k2 <- 8
#       k3 <- 12
#       pred_auc_csplines <- data.frame()
#       auc_csplines <- function(fit, data, period) {
#         fxef <- as.numeric(nlme::fixef(fit))
#         for (j in 1:(length(period) / 2)) {
#           for (i in seq_len(nrow(data))) {
#             coeff <- fxef + c(as.numeric(nlme::ranef(fit)[i, ]), rep(0, 3))
#             x1 <- period[j * 2 - 1]
#             x2 <- period[j * 2]
#             if (x1 <= k1 & x2 <= k1) {
#               y <- function(x1) {
#                 coeff[1] + coeff[2] * x1 + (coeff[3] * x1^2) / 2 + (coeff[4] * x1^3) / 6
#               }
#               pred_auc_csplines[i, j] <- stats::integrate(y, lower = x1, upper = x2)$value
#             } else {
#               if ((x1 > k1 & x1 <= k2) & (x2 > k1 & x2 <= k2)) {
#                 y <- function(x1) {
#                   coeff[1] +
#                     coeff[2] * x1 +
#                     (coeff[3] * x1^2) / 2 +
#                     (coeff[4] * x1^3) / 6 +
#                     (coeff[5] * (x1 - k1)^3) / 6
#                 }
#                 pred_auc_csplines[i, j] <- stats::integrate(
#                   f = y,
#                   lower = x1,
#                   upper = x2
#                 )$value
#               } else {
#                 if ((x1 > k2 & x1 <= k3) & (x2 > k2 & x2 <= k3)) {
#                   y <- function(x1) {
#                     coeff[1] +
#                       coeff[2] * x1 +
#                       (coeff[3] * x1^2) / 2 +
#                       (coeff[4] * x1^3) / 6 +
#                       (coeff[5] * (x1 - k1)^3) / 6 +
#                       (coeff[6] * (x1 - k2)^3) / 6
#                   }
#                   pred_auc_csplines[i, j] <- stats::integrate(y, lower = x1, upper = x2)$value
#                 } else {
#                   if ((x1 > k3 & x2 > k3)) {
#                     y <- function(x1) {
#                       coeff[1] +
#                         coeff[2] * x1 +
#                         (coeff[3] * x1^2) / 2 +
#                         (coeff[4] * x1^3) / 6 +
#                         (coeff[5] * (x1 - k1)^3) / 6 +
#                         (coeff[6] * (x1 - k2)^3) / 6 +
#                         (coeff[7] * (x1 - k3)^3) / 6
#                     }
#                     pred_auc_csplines[i, j] <- stats::integrate(y, lower = x1, upper = x2)$value
#                   } else {
#                     if (x1 <= k1 & x2 > k1) {
#                       y1 <- function(x1) {
#                         coeff[1] + coeff[2] * x1 + (coeff[3] * x1^2) / 2 + (coeff[4] * x1^3) / 6
#                       }
#                       y2 <- function(x1) {
#                         coeff[1] +
#                           coeff[2] * x1 +
#                           (coeff[3] * x1^2) / 2 +
#                           (coeff[4] * x1^3) / 6 +
#                           (coeff[5] * (x1 - k1)^3) / 6
#                       }
#                       pred_auc_csplines[i, j] <- stats::integrate(
#                         f = y1,
#                         lower = x1,
#                         upper = k1
#                       )$value +
#                         stats::integrate(y2, lower = k1, upper = x2)$value
#                     } else {
#                       if (x1 <= k2 & x2 > k2) {
#                         y1 <- function(x1) {
#                           coeff[1] +
#                             coeff[2] * x1 +
#                             (coeff[3] * x1^2) / 2 +
#                             (coeff[4] * x1^3) / 6 +
#                             (coeff[5] * (x1 - k1)^3) / 6
#                         }
#                         y2 <- function(x1) {
#                           coeff[1] +
#                             coeff[2] * x1 +
#                             (coeff[3] * x1^2) / 2 +
#                             (coeff[4] * x1^3) / 6 +
#                             (coeff[5] * (x1 - k1)^3) / 6 +
#                             (coeff[6] * (x1 - k2)^3) / 6
#                         }
#                         pred_auc_csplines[i, j] <- stats::integrate(
#                           f = y1,
#                           lower = x1,
#                           upper = k2
#                         )$value +
#                           stats::integrate(y2, lower = k2, upper = x2)$value
#                       } else {
#                         y1 <- function(x1) {
#                           coeff[1] +
#                             coeff[2] * x1 +
#                             (coeff[3] * x1^2) / 2 +
#                             (coeff[4] * x1^3) / 6 +
#                             (coeff[5] * (x1 - k1)^3) / 6 +
#                             (coeff[6] * (x1 - k2)^3) / 6
#                         }
#                         y2 <- function(x1) {
#                           coeff[1] +
#                             coeff[2] * x1 +
#                             (coeff[3] * x1^2) / 2 +
#                             (coeff[4] * x1^3) / 6 +
#                             (coeff[5] * (x1 - k1)^3) / 6 +
#                             (coeff[6] * (x1 - k2)^3) / 6 +
#                             (coeff[7] * (x1 - k3)^3) / 6
#                         }
#                         pred_auc_csplines[i, j] <- stats::integrate(
#                           f = y1,
#                           lower = x1,
#                           upper = k3
#                         )$value +
#                           stats::integrate(y2, lower = k3, upper = x2)$value
#                       }
#                     }
#                   }
#                 }
#               }
#             }
#           }
#         }
#         out <- data.frame(cbind(data[, 1], pred_auc_csplines))
#         out
#       }
#       auc_csplines_female <- auc_csplines(
#         x,
#         unique(bmigrowth[bmigrowth$sex == 0, "ID", drop = FALSE]),
#         c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
#       )
#       names(auc_csplines_female) <- c(
#         "ID", "AUC_Infancy", "AUC_Childhood", "AUC_PrePubertal", "AUC_PostPubertal"
#       )
#       auc_csplines_female
#     })
#   })
#
#   expect_equal(y1, y2, ignore_attr = TRUE, tolerance = 1e-5)
# })
