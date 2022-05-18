test_that("Adiposity Peak & Rebound", {
  data("bmigrowth")
  res <- egg_model(
    formula = log(bmi) ~ age,
    data = bmigrowth[bmigrowth[["sex"]] == 0, ],
    id_var = "ID",
    random_complexity = 1,
    use_ar1 = TRUE,
    quiet = TRUE
  )
  for (i in c("predicted", "observed")) {
    expect_snapshot(compute_apar(fit = res, from = i)[AP | AR])
  }
  data("bmigrowth")
  res_cov <- egg_model(
    formula = log(bmi) ~ age + sex,
    data = bmigrowth,
    id_var = "ID",
    random_complexity = 1,
    use_ar1 = TRUE, 
    quiet = TRUE
  )
  for (i in c("predicted", "observed")) {
    expect_snapshot(compute_apar(fit = res_cov, from = i)[AP | AR])
  }
})
