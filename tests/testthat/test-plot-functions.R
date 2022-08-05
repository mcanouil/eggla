set.seed(2705)
data("bmigrowth")
ls_mod <- time_model(
  x = "age",
  y = "log(bmi)",
  cov = NULL,
  data = bmigrowth[bmigrowth[["sex"]] == 0, ],
  method = "linear_splines"
)

test_that("plot_aucs", {
  expect_doppelganger("plot_aucs",
    plot_aucs(
      fit = ls_mod,
      method = "linear_splines"
    )
  )
})

test_that("plot_slopes", {
  expect_doppelganger("plot_slopes",
    plot_slopes(
      fit = ls_mod,
      method = "linear_splines"
    ) +
    scale_y_continuous(trans = "identity")
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
  expect_doppelganger("plot_egg_aucs",
    plot_egg_aucs(
      fit = res,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
    )
  )
})

test_that("plot_egg_slopes", {
  expect_doppelganger("plot_egg_slopes",
    plot_egg_slopes(
      fit = res,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
    ) +
    scale_y_continuous(trans = "identity")
  )
})
