set.seed(2705)
options(digits = 4, scipen = 10)

data("bmigrowth")
ls_mod <- time_model(
  x = "age",
  y = "log(bmi)",
  cov = NULL,
  data = bmigrowth[bmigrowth[["sex"]] == 0, ],
  method = "linear_splines"
)

test_that("plot_aucs", {
  expect_s3_class(
    plot_aucs(
      fit = ls_mod,
      method = "linear_splines"
    ),
    "ggplot"
  )
})

test_that("plot_slopes", {
  expect_s3_class(
    plot_slopes(
      fit = ls_mod,
      method = "linear_splines"
    ),
    "ggplot"
  )
})


data("bmigrowth")
res <- egg_model(
  formula = log(bmi) ~ age,
  data = bmigrowth[bmigrowth[["sex"]] == 0, ],
  id_var = "ID",
  random_complexity = 1
)
test_that("plot_egg_aucs", {
  expect_s3_class(
    plot_egg_aucs(
      fit = res,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
    ),
    "ggplot"
  )
})

test_that("plot_egg_slopes", {
  expect_s3_class(
    plot_egg_slopes(
      fit = res,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
    ),
    "ggplot"
  )
})

test_that("plot_residuals", {
  expect_s3_class(
    plot_residuals(
      x = "age",
      y = "log(bmi)",
      fit = res
    ),
    "ggplot"
  )
})
