# eggla 0.5.1

- No longer uses `nlme::corCAR1(form = ~ 1 | ID)` in models.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.5.0...v0.5.1>

# eggla 0.5.0

- Improve/Polish documentation,
  - Add title.
  - Add/tweak description.
  - Add code examples.
- In `R/egg_*.R`,
  - Add `random_complexity` parameter.
  - Remove _var arguments.
- In `R/compute_apar.R`,
  - New function to compute adiposity peak and adiposity rebound from a `lme` model object, such as `egg_model`.
- In `R/compute_aucs.R`,
  - Rename function for consistency, previously known as `compute_auc`.
- In `R/compute_slopess.R`,
  - Rename function for consistency, previously known as `predict_average_slopes`.
- In `vignettes/articles`,
  - Add `01-models-diagnostics.Rmd`, to describe models testing and diagnostics.
  - Add `02-models-selection.Rmd`, to show how to perform models selection based on results from `models-diagnostics.Rmd`.
  - Add `03-run-cubic-splines.Rmd`, to show how to run cubic splines models on the example dataset (previously in `README.Rmd`).
  - Add `04-adiposity-peak-rebound.Rmd`, to identify/caracterise adiposity peak and adiposity rebound from `models-diagnostics.Rmd`.
  - Add `99-run-eggla.Rmd`, to show how to run analysis interactively or not.
- In `R/plot_*.R`,
  - New functions to plot area under the curves and slopes.
- In `pkgdown/_pkgdown.yml`,
  - Polish website.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.4.4...v0.5.0>

# eggla 0.4.4

- In `R/egg_model.R`,
  - Three cublic spline random effects formula are tested in the following order:
    1. `~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3)) | ID`
    2. `~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))[,1:3] | ID`
    3. `~ gsp(age, knots = c(2, 8, 12), degree = rep(1, 4), smooth = rep(2, 3)) | ID`
  - Add message about which model is currently being computed.
- Add `pkgdown` website.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.4.3...v0.4.4>

# eggla 0.4.3

- In `README.(R)md`,
  - Disable `renv` caching.
- In `inst/model-diagnostics`,
  - Remove `ragg` dependency, uses `grDevices` instead.
- In `inst/setup`,
  - Add Dockerfile.
  - Use `pak` in addition to `renv`.
- In `R/run_eggla.R`,
  - Remove `ragg` dependency, uses `grDevices` instead.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.4.2...v0.4.3>

# eggla 0.4.2

- In `README.(R)md`,
  - Fix typo in `renv::init` calls.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.4.1...v0.4.2>

# eggla 0.4.1

- In `README.(R)md`,
  - Add `renv` cleaning in bash example script.
  - Add setup for both interactive and non-interactive use.
- In `R/run_eggla.R`,
  - Switch from `svglite` to `ragg` to avoid issues with high number of elements.
  - Fix `id_var` which was not set with `id_var = "egg_id"` internally.
- In `DESCRIPTION`,
  - Switch from `svglite` to `ragg` to avoid issues with high number of elements.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.4.0...v0.4.1>

# eggla 0.4.0

- In `README.(R)md`,
  - Add table of content.
  - Add Bash script to run everything.
- In `inst/setup`,
  - Add `renv.lock` file to list dependencies with version and help install or restore them.
- In `R/egg_model.R`,
  - Mixed model selected inside a helper/wrapper function.
- In `R/run_eggla.R`,
  - Compute quaility-control for BMI.
  - Compute modelling.
  - Write model object.
  - Write derived parameters.
  - Generate residuals plot.
- In `R/time_model.R`,
  - Fix model specification where additional covariates were not properly added to the model formula.
- In `R/plot_residuals.R`,
  - Fix hardcoded variable, _i.e._, "age".
- In `inst/models-diagnostics`,
  - `models-diagnostics.R`, R script to compute different models and extract performance metrics.
  - `models-best.R`, R script to compare models performance from `models-diagnostics.R`.
  - `models-adiposity.R`, R script to compare predicted BMI values to check for the "adiposity peak".
- In `R`,
  - Remove trailing spaces.
  - Comment code not used.
  - Add missing documentation.
- In `LICENSE`,
  - Add Nicole Warrington.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.3.0...v0.4.0>

# eggla 0.3.0

- In `R/time_model.R`,
  - Add fall back (simpler random effect) models for "cubic_splines", "cubic_slopes" and "linear_splines".
  - Fix typos in messages.
  - Model call is printed as message.
- In `inst/rmarkdown/templates/eggla/skeleton/skeleton.Rmd`,
  - Add Daymont flag exclusion argument and control before modelling step.
  - Summary table and models use `covariates` parameter.
  - Uses "sex" coded as `1` for male and `0` for female.
  - Small code refactoring.
  - Use `performance` to compare models.
  - Add `summary_variables` parameter to show in the summary table.
  - Decrease font size in Daymont's QC summary table .
  - Fix numbers in the summary table.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.2.0...v0.3.0>

# eggla 0.2.0

- In `R/time_model.R`,
  - Fix output when using `as_text = TRUE`.
  - Add `cov` to allow additional covariates.
- In `inst/rmarkdown/templates/eggla/skeleton/skeleton.Rmd`,
  - Add `cov` to allow additional covariates.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.1.0...v0.2.0>

# eggla 0.1.0

- Initial version.
