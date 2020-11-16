test_that("Cubic slope", {

  x <- time_model("age", "log(bmi)", bmigrowth[bmigrowth$sex == 0, ], "cubic_slope")

  y <- nlme::lme(
    log(bmi) ~ stats::poly(age, 3),
    data = bmigrowth[bmigrowth$sex == 0, ],
    random = ~  stats::poly(age, 3) | ID,
    na.action = stats::na.omit,
    method = "ML",
    correlation = nlme::corCAR1(form = ~ 1 | ID),
    control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  )

  x <- stats::coefficients(summary(x))
  y <- stats::coefficients(summary(y))

  expect_equivalent(x, y)
})

test_that("Linear Splines", {

  x <- time_model("age", "log(bmi)", bmigrowth[bmigrowth$sex == 0, ], "linear_splines")

  y <- nlme::lme(
    log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)),
    data = bmigrowth[bmigrowth$sex == 0, ],
    random = ~ gsp(age, knots = c(5.5, 11), degree = c(1, 1, 1), smooth = c(0, 0)) | ID,
    na.action = stats::na.omit,
    method = "ML",
    correlation = nlme::corCAR1(form = ~ 1 | ID),
    control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  )

  x <- stats::coefficients(summary(x))
  y <- stats::coefficients(summary(y))

  expect_equivalent(x, y)
})

test_that("Cubic Splines", {

  x <- time_model("age", "log(bmi)", bmigrowth[bmigrowth$sex == 0, ], "cubic_splines")

  y <- nlme::lme(
    log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)),
    data = bmigrowth[bmigrowth$sex == 0, ],
    random = ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)) | ID,
    na.action = stats::na.omit,
    method = "ML",
    correlation = nlme::corCAR1(form = ~ 1 | ID),
    control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  )

  x <- stats::coefficients(summary(x))
  y <- stats::coefficients(summary(y))

  expect_equivalent(x, y)
})
