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

  expect_snapshot(head(egg_correlations(fit = res2), 10))
  expect_snapshot(head(egg_aucs(fit = res2), 10))
  expect_snapshot(head(egg_slopes(fit = res2), 10))

  for (s in c("predicted", "observed")) {
    expect_snapshot(head(compute_apar(fit = res2, from = s)[AP | AR], 10))
  }

  expect_snapshot(head(egg_outliers(fit = res2), 10))

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

  expect_snapshot(
    head(
      predict_bmi(
        fit = res,
        filter = "source == 'A'"
      )[
        order(egg_id, egg_ageyears),
        egg_bmi := round(egg_bmi, digits = 2)
      ],
      10
    )
  )
})
