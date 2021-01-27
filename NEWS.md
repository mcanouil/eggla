# eggla (development version)

* In `R/time_model.R`,
    + Add fall back (simpler random effect) models for "cubic_splines", "cubic_slopes" and "linear_splines".
    + Fix typos in messages.
    + Model call is printed as message.

* In `inst/rmarkdown/templates/eggla/skeleton/skeleton.Rmd`,
    + Add Daymont flag exclusion argument and control before modelling step.
    + Param `covariates` is used for summary table and models.
    + Uses "sex" coded as `1` for male and `0` for female.
    + Small code refactoring.
    + Use `performance` to compare models.

# eggla 0.2.0

* In `R/time_model.R`,
    + Fix output when using `as_text = TRUE`.
    + Add `cov` to allow additional covariates.
  
* In `inst/rmarkdown/templates/eggla/skeleton/skeleton.Rmd`,
    + Add `cov` to allow additional covariates.

# eggla 0.1.0

* Initial version.
