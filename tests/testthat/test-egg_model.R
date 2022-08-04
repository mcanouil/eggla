set.seed(2705)

data("bmigrowth")

test_that("egg_model", {
  expect_snapshot(
    res2 <- egg_model(
      formula = log(bmi) ~ age,
      data = bmigrowth[bmigrowth[["sex"]] == 0, ],
      id_var = "ID",
      random_complexity = 1,
      use_car1 = TRUE,
      quiet = TRUE
    )
  )

  expect_snapshot(
    cor2 <- egg_correlations(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )

  expect_snapshot(
    auc2 <- egg_aucs(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )

  expect_snapshot(
    slopes2 <- egg_slopes(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )

  for (s in c("predicted", "observed")) {
    expect_snapshot(compute_apar(fit = res2, from = s)[AP | AR])
  }

  expect_snapshot(
    outliers2 <- egg_outliers(
      fit = res2,
      period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12)
    )
  )

  expect_equal(
    sort(basename(run_eggla_lmm(
      data = bmigrowth,
      id_variable = "ID",
      age_days_variable = NULL,
      age_years_variable = "age",
      weight_kilograms_variable = "weight",
      height_centimetres_variable = "height",
      sex_variable = "sex",
      covariates = NULL,
      male_coded_zero = FALSE,
      random_complexity = "auto", # simple to speed-up test
      use_car1 = TRUE,
      parallel = FALSE, # to parallelise Daymont QC
      parallel_n_chunks = 1, # to parallelise Daymont QC
      working_directory = tempdir()
    ))),
    sort(c("female.zip", "male.zip"))
  )
})
