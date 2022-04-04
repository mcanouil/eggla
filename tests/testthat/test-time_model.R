test_that("Cubic slope", {

  x <- time_model(
    x = "age",
    y = "log(bmi)",
    data = bmigrowth[bmigrowth$sex == 0, ],
    method = "cubic_slope"
  )

  y <- lme4::lmer(
    formula = log(bmi) ~ stats::poly(age, 3) + (stats::poly(age, 3) | ID),
    data = bmigrowth[bmigrowth$sex == 0, ],
    na.action = stats::na.omit,
    REML = FALSE,
    control = lme4::lmerControl()
  )

  x <- stats::coefficients(summary(x))
  y <- stats::coefficients(summary(y))

  expect_equivalent(x, y)
})

test_that("Linear Splines", {

  x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "linear_splines")

  y <- lme4::lmer(
    formula = log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)) +
      (gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)) | ID),
    data = bmigrowth[bmigrowth$sex == 0, ],
    na.action = stats::na.omit,
    REML = FALSE,
    control = lme4::lmerControl()
  )

  x <- stats::coefficients(summary(x))
  y <- stats::coefficients(summary(y))

  expect_equivalent(x, y)
})

test_that("Cubic Splines", {

  x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ], method = "cubic_splines")

  y <- lme4::lmer(
    formula = log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)) +
      (gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)) | ID),
    data = bmigrowth[bmigrowth$sex == 0, ],
    na.action = stats::na.omit,
    REML = FALSE,
    control = lme4::lmerControl()
  )

  x <- stats::coefficients(summary(x))
  y <- stats::coefficients(summary(y))

  expect_equivalent(x, y)
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

test_that("Test covariates with trnasformation", {
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
