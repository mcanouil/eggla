set.seed(2705)

test_that("Cubic slope", {
  for (use_car1 in c(FALSE, TRUE)) {
    expect_no_condition(
      x <- time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth[["sex"]] == 0, ],
        method = "cubic_slope",
        use_car1 = use_car1,
        id_var = "ID",
        quiet = TRUE
      )
    )

    y <- nlme::lme(
      log(bmi) ~ stats::poly(age, 3),
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
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
    expect_no_condition(
      x <- time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth[["sex"]] == 0, ],
        method = "linear_splines",
        knots = c(0.75, 5.5, 11),
        use_car1 = use_car1,
        id_var = "ID",
        quiet = TRUE
      )
    )

    y <- nlme::lme(
      log(bmi) ~ gsp(age, knots = c(0.75, 5.5, 11), degree = c(1, 1, 1, 1), smooth = c(0, 0, 0)),
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
      random = ~ gsp(age, knots = c(0.75, 5.5, 11), degree = c(1, 1, 1, 1), smooth = c(0, 0, 0)) | ID,
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
    expect_no_condition(
      x <- time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth[["sex"]] == 0, ],
        method = "cubic_splines",
        knots = c(2, 8, 12),
        use_car1 = use_car1,
        id_var = "ID",
        quiet = TRUE
      )
    )

    y <- nlme::lme(
      log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)),
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
      random = ~ gsp(age, knots = c(2, 8, 12), degree = c(3, 3, 3, 3), smooth = c(2, 2, 2)) | ID,
      na.action = stats::na.omit,
      method = "ML",
      correlation = if (use_car1) nlme::corCAR1(form = ~ 1 | ID) else NULL,
      control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    )

    expect_equal(stats::coefficients(summary(x)), stats::coefficients(summary(y)), ignore_attr = TRUE)

    expect_no_condition(
      compute_outliers(
        fit = x,
        method = "cubic_splines",
        period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
        knots = c(1, 8, 12)
      )
    )

    for (s in c("predicted", "observed")) {
      expect_no_condition(compute_apar(fit = x, from = s)[AP | AR])
    }
  }
})

test_that("Test wrong covariates", {
  expect_error(
    object = time_model(
      x = "age",
      y = "log(bmi)",
      cov = c("nothing"),
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
      method = "linear_splines",
      knots = c(5.5, 11),
      id_var = "ID",
      use_car1 = TRUE,
      quiet = TRUE
    )
  )
})

test_that("Test covariates", {
  expect_no_condition(
    time_model(
      x = "age",
      y = "log(bmi)",
      cov = c("height"),
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
      method = "linear_splines",
      knots = c(5.5, 11),
      id_var = "ID",
      use_car1 = TRUE,
      quiet = TRUE
    )
  )
})

data("bmigrowth")

test_that("time_model", {
  expect_no_condition(
    res <- list(
      "cubic_slope" = time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth[["sex"]] == 0, ],
        method = "cubic_slope",
        knots = c(1, 8, 12),
        id_var = "ID",
        use_car1 = TRUE,
        quiet = TRUE
      ),
      "linear_splines" = time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth[["sex"]] == 0, ],
        method = "linear_splines",
        knots = c(5.5, 11),
        use_car1 = TRUE,
        quiet = TRUE
      ),
      "cubic_splines" = time_model(
        x = "age",
        y = "log(bmi)",
        data = bmigrowth[bmigrowth[["sex"]] == 0, ],
        method = "cubic_splines",
        knots = c(2, 8, 12),
        use_car1 = TRUE,
        quiet = TRUE
      )
    )
  )

  for (i in names(res)) {
    expect_snapshot(compute_correlations(fit = res[[i]], method = i))
    expect_snapshot({
      out <- compute_aucs(fit = res[[i]], method = i)
      out[, names(out)[-1L]] <- round(out[, names(out)[-1L]], digits = 2L)
      out
    })
    expect_snapshot({
      out <- compute_slopes(fit = res[[i]], method = i)
      out[, names(out)[-1L]] <- round(out[, names(out)[-1L]], digits = 2L)
      out
    })
  }
})
