set.seed(2705)

test_that("Cubic slope", {
  for (use_car1 in c(FALSE, TRUE)) {
    expect_snapshot(
      x <- time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth$sex == 0, ],
        method = "cubic_slope",
        use_car1 = use_car1,
        id_var = "ID",
        quiet = TRUE
      )
    )

    y <- nlme::lme(
      log(bmi) ~ stats::poly(age, 3),
      data = bmigrowth[bmigrowth$sex == 0, ],
      random = ~  stats::poly(age, 3) | ID,
      na.action = stats::na.omit,
      method = "ML",
      correlation = if (use_car1) nlme::corCAR1(form = ~ 1 | ID) else NULL,
      control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    )

    expect_equal(stats::coefficients(summary(x)), stats::coefficients(summary(y)), ignore_attr = TRUE)
  }
})

test_that("Linear Splines", {
  for (use_car1 in c(FALSE, TRUE)) {
    expect_snapshot(
      x <- time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth$sex == 0, ],
        method = "linear_splines",
        use_car1 = use_car1,
        id_var = "ID",
        quiet = TRUE
      )
    )

    y <- nlme::lme(
      log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)),
      data = bmigrowth[bmigrowth$sex == 0, ],
      random = ~ gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)) | ID,
      na.action = stats::na.omit,
      method = "ML",
      correlation = if (use_car1) nlme::corCAR1(form = ~ 1 | ID) else NULL,
      control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    )

    expect_equal(stats::coefficients(summary(x)), stats::coefficients(summary(y)), ignore_attr = TRUE)
  }
})

test_that("Cubic Splines", {
for (use_car1 in c(FALSE, TRUE)[2]) {
  expect_snapshot(
    x <- time_model(
      x = "age",
      y = "log(bmi)",
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_splines",
      use_car1 = use_car1,
      id_var = "ID",
      quiet = TRUE
    )
  )

  y <- nlme::lme(
    log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)),
    data = bmigrowth[bmigrowth$sex == 0, ],
    random = ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)) | ID,
    na.action = stats::na.omit,
    method = "ML",
    correlation = if (use_car1) nlme::corCAR1(form = ~ 1 | ID) else NULL,
    control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  )

  expect_equal(stats::coefficients(summary(x)), stats::coefficients(summary(y)), ignore_attr = TRUE)
}
})

test_that("Test wrong covariates", {
  expect_error(
    object = time_model(
      x = "age",
      y = "log(bmi)",
      cov = c("nothing"),
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines",
      id_var = "ID",
      use_car1 = TRUE,
      quiet = TRUE
    )
  )
})

test_that("Test covariates", {
  expect_snapshot(
    time_model(
      x = "age",
      y = "log(bmi)",
      cov = c("height"),
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines",
      id_var = "ID",
      use_car1 = TRUE,
      quiet = TRUE
    )
  )
})

test_that("Test covariates with transformation", {
  expect_snapshot(
    stats::coefficients(summary(time_model(
      x = "age",
      y = "log(bmi)",
      cov = c("log(height)"),
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines",
      id_var = "ID",
      use_car1 = TRUE,
      quiet = TRUE
    )))
  )
})

data("bmigrowth")

test_that("time_model", {
  res1 <- time_model(
    y = "log(bmi)",
    x = "age",
    data = bmigrowth[bmigrowth[["sex"]] == 0, ],
    method = "cubic_slope",
    knots = c(2, 8, 12),
    id_var = "ID",
    use_car1 = TRUE,
    quiet = TRUE
  )

  expect_snapshot(
    cor1 <- compute_correlations(
      fit = res1,
      method = "cubic_slope",
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )

  expect_snapshot(
    auc1 <- compute_aucs(
      fit = res1,
      method = "cubic_slope",
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )

  expect_snapshot(
    slopes1 <- compute_slopes(
      fit = res1,
      method = "cubic_slope",
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )

  for (s in c("predicted", "observed")) {
    expect_snapshot(compute_apar(fit = res1, from = s)[AP | AR])
  }

  expect_snapshot(
    outliers1 <- compute_outliers(
      fit = res1,
      method = "cubic_slope",
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )
})
