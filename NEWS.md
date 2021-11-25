# eggla 0.4.2

* In `README.(R)md`,
    + Fix typo in `renv::init` calls.

# eggla 0.4.1

* In `README.(R)md`,
    + Add `renv` cleaning in bash example script.
    + Add setup for both interactive and non-interactive use.

* In `R/run_eggla.R`,
    + Switch from `svglite` to `ragg` to avoid issues with high number of elements.
    + Fix `id_var` which was not set with `id_var = "egg_id"` internally.

* In `DESCRIPTION`,
    + Switch from `svglite` to `ragg` to avoid issues with high number of elements.

# eggla 0.4.0

* In `README.(R)md`,
    + Add table of content.
    + Add Bash script to run everything.

* In `inst/setup`,
    + Add `renv.lock` file to list dependencies with version and help install or restore them.

* In `R/egg_model.R`,
    + Mixed model selected inside a helper/wrapper function.

* In `R/run_eggla.R`,
    + Compute quaility-control for BMI.
    + Compute modelling.
    + Write model object.
    + Write derived parameters.
    + Generate residuals plot.

* In `R/time_model.R`,
    + Fix model specification where additional covariates were not properly added to the model formula.

* In `R/plot_residuals.R`,
    + Fix hardcoded variable, i.e., "age".

* In `inst/models-diagnostics`,
    + `models-diagnostics.R`, R script to compute different models and extract performance metrics.
    + `models-best.R`, R script to compare models performance from `models-diagnostics.R`.
    + `models-adiposity.R`, R script to compare predicted BMI values to check for the "adiposity peak".

* In `R`,
    + Remove trailing spaces.
    + Comment code not used.
    + Add missing documentation.

* In `LICENSE`,
    + Add Nicole Warrington.

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
