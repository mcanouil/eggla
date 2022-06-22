data("bmigrowth")

test_that("egg_model", {
  for (i in c(FALSE, TRUE)) {
    expect_s3_class(
      egg_model(
        formula = log(bmi) ~ age,
        data = bmigrowth[bmigrowth[["sex"]] == 0, ],
        id_var = "ID",
        random_complexity = 1,
        use_car1 = TRUE,
        quiet = TRUE
      ),
      "lme"
    )
  }
})
