test_that("Adiposity Peak & Rebound", {
  for (i in c("predicted", "observed")) {
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
})
