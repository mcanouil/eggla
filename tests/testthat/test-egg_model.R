set.seed(2705)

data("bmigrowth")

test_that("egg_model", {
  expect_no_condition(
    res2 <- egg_model(
      formula = log(bmi) ~ age,
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
      id_var = "ID",
      random_complexity = 1,
      use_car1 = TRUE,
      quiet = TRUE
    )
  )

  expect_no_condition(
    egg_correlations(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    )
  )

  expect_no_condition(
    egg_aucs(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    )
  )

  expect_no_condition(
    egg_slopes(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    )
  )

  for (s in c("predicted", "observed")) {
    expect_no_condition(compute_apar(fit = res2, from = s)[AP | AR])
  }

  expect_no_condition(
    egg_outliers(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    )
  )

  set.seed(1234)
  dta <- bmigrowth[bmigrowth[["sex"]] == 0, ]
  dta[["source"]] <- c("A", "B")[rbinom(n = nrow(dta), size = 1, prob = 0.65) + 1]
  res <- egg_model(
    formula = log(bmi) ~ age + source,
    data = dta,
    id_var = "ID",
    random_complexity = 1
  )

  expect_warning(predict_bmi(res)[order(egg_id, egg_ageyears)])

  expect_no_condition(
    predict_bmi(
      fit = res,
      filter = "source == 'A'"
    )[
      order(egg_id, egg_ageyears),
      egg_bmi := round(egg_bmi, digits = 2)
    ]
  )
})
