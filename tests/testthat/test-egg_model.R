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

  expect_snapshot(egg_correlations(fit = res2)[, lapply(.SD, round, digits = 2L), .SDcols = is.numeric])
  expect_snapshot({
    out <- egg_aucs(fit = res2)
    out[, names(out)[-1L]] <- round(out[, names(out)[-1L]], digits = 2L)
    out
  })
  expect_snapshot({
    out <- egg_slopes(fit = res2)
    out[, names(out)[-1L]] <- round(out[, names(out)[-1L]], digits = 2L)
    out
  })

  for (s in c("predicted", "observed")) {
    expect_snapshot({
      out <- compute_apar(fit = res2, from = s)[AP | AR]
      out[, c(2L, 3L)] <- round(out[, c(2L, 3L)], digits = 2L)
      out
    })
  }

  expect_snapshot({
    out <- egg_outliers(fit = res2)
    out[, "Distance_IQR"] <- round(out[, "Distance_IQR"], digits = 2L)
    out
  })

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

  expect_snapshot({
    out <- predict_bmi(
      fit = res,
      filter = "source == 'A'"
    )[
      order(egg_id, egg_ageyears),
      egg_bmi := round(egg_bmi, digits = 2)
    ]
    out <- head(out, 50)
    out[, c(3L, 4L)] <- round(out[, c(3L, 4L)], digits = 2L)
    out
  })
})
