test_that("Cubic slope", {

  x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "cubic_slope")

  y1 <- compute_slopes(
    fit = x,
    method = "cubic_slope",
    period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
  )
  y2 <- local({
    suppressWarnings({
      slopes_lme <- data.frame()
      pred_bmi_lme <- data.frame()
      ave_slope_lme <- function(fit, data, period) {
        fxef <- as.numeric(nlme::fixef(fit))
        for (j in 1:(length(period) / 2)) {
          for (i in seq_len(nrow(data))) {
            coeff <- fxef + c(as.numeric(nlme::ranef(fit)[i, ]))
            x1 <- period[j * 2 - 1]
            y1 <- coeff[1] + coeff[2] * x1 + coeff[3] * x1^2 + coeff[4] * x1^3
            x2 <- period[j * 2]
            y2 <- coeff[1] + coeff[2] * x2 + coeff[3] * x2^2 + coeff[4] * x2^3
            pred_bmi_lme[i, j * 2 - 1] <- y1
            pred_bmi_lme[i, j * 2] <- y2
            slopes_lme[i, j] <- (y2 - y1) / (x2 - x1)
          }
        }
        out <- data.frame(cbind(data[, 1], pred_bmi_lme, slopes_lme))
        out
      }
      slopes_lme_female <- ave_slope_lme(
        x,
        unique(bmigrowth[bmigrowth$sex == 0, "ID", drop = FALSE]),
        c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
      )
      names(slopes_lme_female) <- c(
        "ID", "BMI_0", "BMI_0.5", "BMI_1.5", "BMI_5", "BMI_6", "BMI_10", "BMI_12", "BMI_17",
        "Infancy", "Childhood", "PrePubertal", "PostPubertal"
      )
      slopes_lme_female
    })
  })

  expect_equivalent(y1, y2)
})

test_that("Linear Splines", {

  x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "linear_splines")

  y1 <- compute_slopes(
    fit = x,
    method = "linear_splines",
    period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
  )

  y2 <- local({
    suppressWarnings({
      k1 <- 5.5
      k2 <- 11
      slopes_lsplines <- data.frame()
      pred_bmi_lsplines <- data.frame()
      ave_slope_lsplines <- function(fit, data, period) {
        fxef <- as.numeric(nlme::fixef(fit))
        for (j in 1:(length(period) / 2)) {
          for (i in seq_len(nrow(data))) {
            coeff <- fxef + c(as.numeric(nlme::ranef(fit)[i, ]))
            x1 <- period[j * 2 - 1]
            if (x1 <= k1) {
              y1 <- coeff[1] + coeff[2] * x1
            } else {
              if (x1 > k1 & x1 <= k2) {
                y1 <- coeff[1] + coeff[2] * x1 + coeff[3] * (x1 - k1)
              } else {
                y1 <- coeff[1] + coeff[2] * x1 + coeff[3] * (x1 - k1) + coeff[4] * (x1 - k2)
              }
            }
            x2 <- period[j * 2]
            if (x2 <= k1) {
              y2 <- coeff[1] + coeff[2] * x2
            } else {
              if (x2 > k1 & x2 <= k2) {
                y2 <- coeff[1] + coeff[2] * x2 + coeff[3] * (x2 - k1)
              } else {
                y2 <- coeff[1] + coeff[2] * x2 + coeff[3] * (x2 - k1) + coeff[4] * (x2 - k2)
              }
            }
            pred_bmi_lsplines[i, j * 2 - 1] <- y1
            pred_bmi_lsplines[i, j * 2] <- y2
            slopes_lsplines[i, j] <- (y2 - y1) / (x2 - x1)
          }
        }
        out <- data.frame(cbind(data[, 1], pred_bmi_lsplines, slopes_lsplines))
        out
      }
      slopes_lsplines_female <- ave_slope_lsplines(
        x,
        unique(bmigrowth[bmigrowth$sex == 0, "ID", drop = FALSE]),
        c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
      )
      names(slopes_lsplines_female) <- c(
        "ID", "BMI_0", "BMI_0.5", "BMI_1.5", "BMI_5", "BMI_6", "BMI_10", "BMI_12", "BMI_17",
        "Infancy", "Childhood", "PrePubertal", "PostPubertal"
      )
      slopes_lsplines_female
    })
  })

  expect_equivalent(y1, y2)
})

test_that("Cubic Splines", {

  x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "cubic_splines")

  y1 <- compute_slopes(
    fit = x,
    method = "cubic_splines",
    period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
  )

  y2 <- local({
    suppressWarnings({
      k1 <- 2
      k2 <- 8
      k3 <- 12
      slopes_csplines <- data.frame()
      pred_bmi_csplines <- data.frame()
      ave_slope_csplines <- function(fit, data, period) {
        fxef <- as.numeric(nlme::fixef(fit))
        for (j in 1:(length(period) / 2)) {
          for (i in seq_len(nrow(data))) {
            coeff <- fxef + c(as.numeric(nlme::ranef(fit)[i, ]), rep(0, 3))
            x1 <- period[j * 2 - 1]
            if (x1 <= k1) {
              y1 <- coeff[1] + coeff[2] * x1 + (coeff[3] * x1^2) / 2 + (coeff[4] * x1^3) / 6
            } else {
              if (x1 > k1 & x1 <= k2) {
                y1 <- coeff[1] + coeff[2] * x1 + (coeff[3] * x1^2) / 2 + (coeff[4] * x1^3) / 6 +
                  (coeff[5] * (x1 - k1)^3) / 6
              } else {
                if (x1 > k2 & x1 <= k3) {
                  y1 <- coeff[1] + coeff[2] * x1 + (coeff[3] * x1^2) / 2 + (coeff[4] * x1^3) / 6 +
                    (coeff[5] * (x1 - k1)^3) / 6 + (coeff[6] * (x1 - k2)^3) / 6
                } else {
                  y1 <- coeff[1] + coeff[2] * x1 + (coeff[3] * x1^2) / 2 + (coeff[4] * x1^3) / 6 +
                    (coeff[5] * (x1 - k1)^3) / 6 + (coeff[6] * (x1 - k2)^3) / 6 + (coeff[7] * (x1 - k3)^3) / 6
                }
              }
            }
            x2 <- period[j * 2]
            if (x2 <= k1) {
              y2 <- coeff[1] + coeff[2] * x2 + (coeff[3] * (x2^2)) / 2 + (coeff[4] * (x2^3)) / 6
            } else {
              if (x2 > k1 & x2 <= k2) {
                y2 <- coeff[1] + coeff[2] * x2 + (coeff[3] * (x2^2)) / 2 + (coeff[4] * (x2^3)) / 6 +
                  (coeff[5] * ((x2 - k1)^3)) / 6
              } else {
                if (x2 > k2 & x2 <= k3) {
                  y2 <- coeff[1] + coeff[2] * x2 + (coeff[3] * x2^2) / 2 + (coeff[4] * x2^3) / 6 +
                    (coeff[5] * (x2 - k1)^3) / 6 + (coeff[6] * (x2 - k2)^3) / 6
                } else {
                  y2 <- coeff[1] + coeff[2] * x2 + (coeff[3] * x2^2) / 2 + (coeff[4] * x2^3) / 6 +
                    (coeff[5] * (x2 - k1)^3) / 6 + (coeff[6] * (x2 - k2)^3) / 6 + (coeff[7] * (x2 - k3)^3) / 6
                }
              }
            }
            pred_bmi_csplines[i, j * 2 - 1] <- y1
            pred_bmi_csplines[i, j * 2] <- y2
            slopes_csplines[i, j] <- (y2 - y1) / (x2 - x1)
          }
        }
        out <- data.frame(cbind(data[, 1], pred_bmi_csplines, slopes_csplines))
        out
      }
      slopes_csplines_female <- ave_slope_csplines(
        x,
        unique(bmigrowth[bmigrowth$sex == 0, "ID", drop = FALSE]),
        c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
      )
      names(slopes_csplines_female) <- c(
        "ID", "BMI_0", "BMI_0.5", "BMI_1.5", "BMI_5", "BMI_6", "BMI_10", "BMI_12", "BMI_17",
        "Infancy", "Childhood", "PrePubertal", "PostPubertal"
      )
      slopes_csplines_female
    })
  })

  expect_equivalent(y1, y2)
})
