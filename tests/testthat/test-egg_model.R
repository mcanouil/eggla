set.seed(2705)
options(digits = 4, scipen = 10)

data("bmigrowth")

test_that("egg_model", {
  expect_snapshot({
    res2 <- egg_model(
      formula = log(bmi) ~ age,
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
      id_var = "ID",
      random_complexity = 1,
      use_car1 = TRUE,
      quiet = TRUE
    )
    print(res2, digits = 4)
  })

  expect_snapshot(
    print(egg_correlations(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    ), digits = 4)
  )

  expect_snapshot(
    print(egg_aucs(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    ), digits = 4)
  )

  expect_snapshot(
    print(egg_slopes(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    ), digits = 4)
  )

  for (s in c("predicted", "observed")) {
    expect_snapshot(
      print(compute_apar(fit = res2, from = s)[AP | AR], digits = 4)
    )
  }

  expect_snapshot(
    print(egg_outliers(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12)
    ), digits = 4)
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

  expect_snapshot_warning(
    print(predict_bmi(res)[order(egg_id, egg_ageyears)], digits = 4)
  )

  expect_snapshot(
    print(predict_bmi(
      fit = res,
      filter = "source == 'A'"
    )[
      order(egg_id, egg_ageyears),
      egg_bmi := round(egg_bmi, digits = 2)
    ], digits = 4)
  )
})
