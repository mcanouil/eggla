test_that("Cubic slope", {
  for (use_ar1 in c(FALSE, TRUE)) {
    x <- time_model(
      x = "age",
      y = "log(bmi)",
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_slope",
      use_ar1 = use_ar1
    )

    y <- nlme::lme(
      log(bmi) ~ stats::poly(age, 3),
      data = bmigrowth[bmigrowth$sex == 0, ],
      random = ~  stats::poly(age, 3) | ID,
      na.action = stats::na.omit,
      method = "ML",
      correlation = if (use_ar1) nlme::corCAR1(form = ~ 1 | ID) else NULL,
      control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    )

    expect_equal(stats::coefficients(summary(x)), stats::coefficients(summary(y)), ignore_attr = TRUE)
  }
})

test_that("Linear Splines", {
  for (use_ar1 in c(FALSE, TRUE)) {
    x <- time_model(
      x = "age",
      y = "log(bmi)",
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines",
      use_ar1 = use_ar1
    )

    y <- nlme::lme(
      log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)),
      data = bmigrowth[bmigrowth$sex == 0, ],
      random = ~ gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)) | ID,
      na.action = stats::na.omit,
      method = "ML",
      correlation = if (use_ar1) nlme::corCAR1(form = ~ 1 | ID) else NULL,
      control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    )

    expect_equal(stats::coefficients(summary(x)), stats::coefficients(summary(y)), ignore_attr = TRUE)
  }
})

test_that("Cubic Splines", {
for (use_ar1 in c(FALSE, TRUE)[2]) {
  x <- time_model(
    x = "age",
    y = "log(bmi)",
    data = bmigrowth[bmigrowth$sex == 0, ],
    method = "cubic_splines",
      use_ar1 = use_ar1
  )

  y <- nlme::lme(
    log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)),
    data = bmigrowth[bmigrowth$sex == 0, ],
    random = ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)) | ID,
    na.action = stats::na.omit,
    method = "ML",
    correlation = if (use_ar1) nlme::corCAR1(form = ~ 1 | ID) else NULL,
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
      method = "linear_splines"
    )
  )
})

test_that("Test covariates", {
  expect_s3_class(
    object = time_model(
      x = "age",
      y = "log(bmi)",
      cov = c("height"),
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines"
    ),
    class = "lme"
  )
})

test_that("Test covariates with transformation", {
  expect_s3_class(
    object = time_model(
      x = "age",
      y = "log(bmi)",
      cov = c("log(height)"),
      data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines"
    ),
    class = "lme"
  )
})
