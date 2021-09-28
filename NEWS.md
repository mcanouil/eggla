# eggla (development)

* In `inst/models-diagnostics`,
    + `models-diagnostics.R`, R script to compute different models and extract performance metrics.
    + `models-best.R`, R script to compare models performance from `models-diagnostics.R`.
    + `models-adiposity.R`, R script to compare predicted BMI values to check for the "adiposity peak".

# eggla 0.3.0

* In `R/time_model.R`,
    + Add fall back (simpler random effect) models for "cubic_splines", "cubic_slopes" and "linear_splines".
    + Fix typos in messages.
    + Model call is printed as message.

* In `inst/rmarkdown/templates/eggla/skeleton/skeleton.Rmd`,
    + Add Daymont flag exclusion argument and control before modelling step.
    + Summary table and models use `covariates` parameter.
    + Uses "sex" coded as `1` for male and `0` for female.
    + Small code refactoring.
    + Use `performance` to compare models.
    + Add `summary_variables` parameter to show in the summary table.
    + Decrease font size in Daymont's QC summary table .
    + Fix numbers in the summary table.

# eggla 0.2.0

* In `R/time_model.R`,
    + Fix output when using `as_text = TRUE`.
    + Add `cov` to allow additional covariates.

* In `inst/rmarkdown/templates/eggla/skeleton/skeleton.Rmd`,
    + Add `cov` to allow additional covariates.

# eggla 0.1.0

* Initial version.
