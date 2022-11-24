# eggla 0.18.4

## Chores

- In `DESCRIPTION`,
  - chore: update email address.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.18.3...v0.18.4>

# eggla 0.18.3

## Fixes

- In `R/run_eggla_gwas.R`,
  - fix: HWE_P should be NA for imputed variants.
  
**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.18.2...v0.18.3>

# eggla 0.18.2

## Fixes

- In `R/egg_correlations.R` and `R/run_eggla_lmm.R`,
  - fix: parameters table not properly filtered and omitting missing values.
  
**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.18.1...v0.18.2>

# eggla 0.18.1

## Fixes

- In `R/run_eggla_gwas.R`,
  - fix: info_type description given to genotyped variants.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.18.0...v0.18.1>

# eggla 0.18.0

## Features

- In `R/run_eggla_lmm.R` and `R/egg_correlations.R`,
  - feat: perform correlations between all parameters.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.17.5...v0.18.0>

# eggla 0.17.5

## Style

- In `vignettes/articles/adiposity-peak-rebound.Rmd`,
  - style: remove top ticks for AP/AR.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.17.4...v0.17.5>

# eggla 0.17.4

## Fixes

- In `R/run_eggla_gwas.R`,
  - fix: rename R2 to INFO if exists.
- In `vignettes/articles/models-diagnostics.Rmd`,
  - revert: set back knots to original values for diagnostics.
  - fix: hardcode performance metrics to avoid changes due to `performance` updates.
  - fix: eval to FALSE to set w/o AR1.

## Docs

- In `vignettes/articles/models-diagnostics.Rmd`,
  - docs: new section to update and tidy perf data.

## Style

- In `vignettes/articles/adiposity-peak-rebound.Rmd`, `vignettes/articles/run-cubic-splines.Rmd`, and `vignettes/articles/models-diagnostics.Rmd`,
  - style: homogenise all figures with the new theme.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.17.3...v0.17.4>

# eggla 0.17.3

## Fixes

- In `R/run_eggla_lmm.R`,
  - fix: order derived parameter for outlier figure.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.17.2...v0.17.3>

# eggla 0.17.2

## Fixes

- In `R/compute_apar.R`,
  - fix: update filter that removed all AR estimates and add a control for AR > AP.
- In `vignettes/articles/adiposity-peak-rebound.Rmd`,
  - fix: missplaced parenthesis.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.17.1...v0.17.2>

# eggla 0.17.1

## Fixes

- In `R/compute_outliers.R`, `R/egg_outliers.R`, and `R/run_eggla_lmm.R`,
  - fix: only allow one value for `outlier_method`.
- In `R/plot_egg_slopes.R` and `R/plot_slopes.R`,
  - fix: uses GAM with cubic splines every time.

## Style

- style: set default `ggplot2` theme to a modified version of `theme_minimal()` and colour/fill scales to "viridis".
- style: use Okabe Ito colour palette.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.17.0...v0.17.1>

# eggla 0.17.0

## Features

- In `R/run_eggla_lmm.R`, `R/compute_outliers.R`, and `R/egg_outliers.R`,
  - feat: new arg to pass on the outlier method.
  - feat: new arg to pass on the outlier method threshold.
  - feat: now exclude flagged outliers by setting their values to NA.

## Fixes

- In `R/compute_outliers.R`,
  - fix: replace AP greater than 2 by NA.

## Tests

- In `tests`,
  - test: no longer uses snapshots.

## Chores

- In `pkgdown/_pkgdown.yml`,
  - style: separate the `run_eggl_*()` functions from the rest.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.16.0...v0.17.0>

# eggla 0.16.0

## Features

- In `R/run_eggla_lmm.R`,
  - feat: draw outlier figure for all derived parameters.

## Fixes

- In `R/compute_outliers.R` and `R/egg_outliers.R`,
  - fix: id column matching across parameters now properly works.
- In `R/run_eggla_gwas.R`,
  - fix: CRAN checks notes.
- In `R/compute_apar.R`, `R/plot_aucs.R`, `R/plot_egg_aucs.R`, `R/plot_egg_slopes.R`, `R/plot_residuals.R`, `R/plot_slopes.R`,
  `vignettes/articles/adiposity-peak-rebound.Rmd`, `vignettes/articles/model-selection.Rmd`,
  `vignettes/articles/models-diagnostics.Rmd`, and `vignettes/articles/run-cubic-splines.Rmd`,
  - fix: standardise `ggplot` default theme and fix `gt` tables.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.15.0...v0.16.0>

# eggla 0.15.0

## Features

- In `R/compute_outliers.R` and `R/egg_outliers.R`,
  - feat: now detects outliers for all slopes together, all AUCs together and for individual slopes, AUCs, AP (BMI and age), and AR (BMI and age).

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.14.0...v0.15.0>

# eggla 0.14.0

## Breaking changes

- refactor!: no longer rely on `renv`.

## Features/Fixes

- In `R/run_eggla_lmm.R`,
  - feat: add diagnostics output to a "to-send" folder to avoid mistakenly send individual-level data.
- In `R/run_eggla_gwas.R`,
  - fix: now uses the filled INFO field of VCF and compute required columns for meta-analysis.

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.13.1...v0.14.0>

# eggla 0.13.1

## Chores

- In `pkgdown/_pkgdown.yml`,
  - chore: update template to "minty".

**Full Changelog**: <https://github.com/mcanouil/eggla/compare/v0.13.0...v0.13.1>

# eggla 0.13.0

## Breaking changes

- In `R/*.R`,
  - fix: `knots` default value is now `c(1, 8, 12)`.
- In `R/run_eggla_gwas.R`,
  - refactor: `path` argument has been renamed `working_directory` for consistency with `run_eggla_lmm()`.

## Features

- In `R/compute_apar.R` and `R/predict_bmi.R`,
  - feat: `filter` argument filter predicted values table in case of multiple measurements.
- In `R/run_eggla_lmm.R`,
  - feat: `filter_apar` argument to be passed to `compute_apar()` and `predict_bmi()`.
- In `R/run_eggla_gwas.R`,
  - feat: now performs GWAS of BMI and age at adiposity peak and rebound in addition to slopes and AUCs.
  - feat: now has a `clean` argument which clean intermediary files stored in `working_directory`.
  - feat: new `use_info` argument to indicate if INFO field should be extracted (default is `FALSE`).

## Fixes

- In `R/run_eggla_lmm.R`,
  - fix: now checks if IDs are unique.
  - fix: properly export BMI and age at adiposity peak and rebound.
  - fix: PLINK2 and BCFTools binary path checks.
- In `R/run_eggla_gwas.R`,
  - fix: now check for individuals in common between phenotypes and genotypes before processing the VCF files.
- In `DESCRIPTION`,
  - fix: move some packages from `suggests` to `imports` (_e.g._, `ggtext`, `ggdist`, _etc._)
  - fix: now uses `growthcleaner` from CRAN.

## Chores

- In `.devcontainer` and `inst/setup`,
  - refactor: improve Docker build process and add `devcontainer` ability.
- In `pkgdown/_pkgdown.yml`,
  - refactor: reorder sections to focus on the main functions first.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.12.3...v0.13.0>>

# eggla 0.12.3

## Breaking changes

- In `DESCRIPTION`,
  - fix: depends on R >= 4.2.0.

## Fixes

- In `R/run_eggla_gwas.R`,
  - fix: BCFtools filters as parameter with non by default.
  - fix: ensure 'id_column' is of type character.

## Chores

- In `insta/vcf` and `data-raw/bmigrowth.R`,
  - chore: add VCF files to test `run_eggla_gwas()`.

## Tests

- In `inst/setup/eggla-example.R`,
  - test: add run_eggla_gwas in docker build test example.
- In `tests/testthat/test-run_eggla.R`,
  - test: new tests for run_eggla functions.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.12.2...v0.12.3>>

# eggla 0.12.2

## Fixes

- In `R/run_eggla_gwas.R`,
  - fix: the condition has length > 1.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.12.1...v0.12.2>>

# eggla 0.12.1

## Fixes

- In `R/run_eggla_gwas.R`,
  - fix: wrong variable in "results" input check condition.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.12.0...v0.12.1>>

# eggla 0.12.0

## Fixes

- In `R/run_eggla_lmm.R`,
  - fix: improve data transformation/cleaning (#38).
  - fix: allow to output directories path and no zip archives when `zip` is not available (#41).
  - fix: remove dates from model object filename.

- In `R/run_eggla_gwas.R`,
  - fix: allow to use directories path or zip archives as results (#41).
  - fix: rename `results_zip` argument to `results`.

## Features

- In `R/run_eggla_lmm.R`,
  - feat: add knots and period parameters (#37).
  - feat: add clean parameter to clean directories when archives are successfully created (#41).

## Chores

- chore: remove version from CFF file.

## Style

- style: remove break line in badges section.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.11.2...v0.12.0>>

# eggla 0.11.2

## Fixes

- fix: uses general Zenodo DOI.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.11.1...v0.11.2>>

# eggla 0.11.1

## Chores

- chore: add DOI and citation files.

## Fixes

- In `vinettes/eggla.Rmd`,
  - fix: uses `latest` moving tag.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.11.0...v0.11.1>>

# eggla 0.11.0

## Breaking changes

- `run_eggla()` has been renamed to `run_eggla_lmm()`.
- `do_eggla_gwas()` has been renamed to `run_eggla_gwas()`.

## Fixes

- In `R/run_eggla_lmm.R`,
  - fix: exclude rds from being included in archive.
- In `R/run_eggla_gwas.R`,
  - fix: improve and add messages.
  - fix: ensure "data" is a data.frame or a path read in a data.frame.
  - fix: allows binary path to be an actual binary path.
  - fix: now returns results files path, input parameters zip file, and text file with software versions.
  - fix: collapse vector of covariates into an atomic string to be added in the results files.

## Features

- In `inst/bin`,
  - feat: add PLINK2 binary internally.
- In `R/run_eggla_lmm.R`,
  - feat: eggla model call is now written in a file.
- In `R/egg_correlations()` and `R/compute_correlations()`,
  - feat: add correlation figure/table. (#22)
- In `R/egg_outliers()` and `R/compute_outliers()`,
  - feat: add function to check. (#22)

## Tests

- In `tests/testhat`:
  - test: add tests for plot functions.
  - test: add more tests for `time_model()` and `egg_model()`.
  - test: add tests for `*_correlations()` and `*_outliers()` functions.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.10.4...v0.11.0>>

# eggla 0.10.4

## Fixes

- In `R/do_eggla_gwas.R`,
  - Fix: plink2 binary path modified outside download if statement.
  - Fix: merge not cartesian by keeping only first measures of each individuals (#20).
- In `inst/setup/Dockerfile`, `inst/setup/pkg.lock`, `inst/setup/renv.lock`, and`data-raw/lock.R`,
  - Fix: add all `data.table` dependencies.
- In `R/`, `tests/`, and `vignettes/`,
  - fix: documentation consistency with code, i.e., replaced "AR1" with "CAR1" (#21).

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.10.3...v0.10.4>>

# eggla 0.10.3

## Fixes

- In `inst/Dockerfile`,
  - Build: add PLINK2 and install all eggla's dependencies, including `suggests`.
- In `R/do_eggla_gwas.R`,
  - Fix: no longer relies on URL for PLINK2 since only `alpha` dynamic URL remains.
- In `vignettes/eggla.Rmd`:
  - Chore: remove unused `eval = FALSE` chunk option.
  - Fix: remove `renv` code in the Docker section.
  - Fix: set properly the working directory.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.10.2...v0.10.3>>

# eggla 0.10.2

## Fixes

- In `R/egg_model.R`,
  - Fix: try without CAR1 before reducing random effect. (#18)

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.10.1...v0.10.2>>

# eggla 0.10.1

## Fixes

- In `R/egg_slopes.R`, `R/plot_egg_slopes.R`, `R/egg_aucs.R`, and `R/plot_egg_aucs.R`,
  - Fix: hardcoded knots, now as an argument. (#16, #17)

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.10.0...v0.10.1>>

# eggla 0.10.0

## Features

- In `R/predict_bmi.R` ([#14](https://github.com/mcanouil/eggla/issues/14)),
  - Feat: new function to predict BMI from a `lme` model object, from `egg_model()` or `time_model()`.

## Improvements

- Change default period to the following time windows for predicting 'linear' slopes from the model:
  - 0-0.5 years (Infancy – same as previously).
  - 1.5-3.5 years (Childhood – reduced the upper bound from 5 years).
  - 6.5-10 years (Pre-pubertal – increased the lower bound from 6 years).
  - 12-17 years (adolescence – same as previously).

## Fixes

- In `R/compute_apar.R` ([#14](https://github.com/mcanouil/eggla/issues/14)),
  - Fix: `grep` instead of `any`/`grepl`.
  - Refactor: inherits params from `predict_bmi()`.
  - Fix: uses `predict_bmi()` internally.
- In `R/plot_slopes.R`, and `R/plot_egg_slopes.R`,
  - Fix: `grep` instead of `any`/`grepl`.
  - Fix: uses GAM 'y ~ s(x, bs = "cr")'.
- In `vignettes/articles/adiposity-peak-rebound.Rmd`; `vignettes/articles/models-diagnostics.Rmd`, and `vignettes/articles/run-cubic-splines.Rmd` ([#14](https://github.com/mcanouil/eggla/issues/14), [#15](https://github.com/mcanouil/eggla/issues/15)),
  - Fix: uses GAM 'y ~ s(x, bs = "cr")' and `predict_bmi()`.
- In `vignettes/eggla.Rmd`,
  - Fix: "-it" instead of "--detach" for Docker command.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.9.1...v0.10.0>>

# eggla 0.9.1

## Features

- In `R/egg_model.R`, `R/egg_model.R`, `R/run_eggla.R`, and `R/do_eggla_gwas`,
  - Feat: add `quiet` to silent the messages.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.9.0...v0.9.1>>

# eggla 0.9.0

## Features

- In `inst/setup/Dockerfile`,
  - Feat: add script example (`inst\setup\eggla-example.R`) to run on dataset included in `eggla`.
- In `R/egg_model.R`,
  - Feat: allow to specify the knots for the splines, i.e., no longer hardcoded.

## Fixes

- In `R/egg_model.R`,
  - Fix: hardcoded "ID" for individual specification in random effect.
- In `R/time_model.R`,
  - Fix: hardcoded "ID" for individual specification in random effect by adding `id_var` arg.
- In `vignettes/eggla.Rmd` (renamed/moved from `vignettes/articles/run-eggla.Rmd`),
  - Fix: use `Rscript` instead of `R --no-save --no-restore --quiet`.

## Documents

- In `vignettes/eggla.Rmd`,
  - Docs: complete GWAS sections.
  - Docs: add Docker section.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.8.1...v0.9.0>>

# eggla 0.8.1

## Features

- In `inst/setup/Dockerfile`,
  - Feat: Docker image built automatically "devel" from "main" and "tag" from "releases".

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.8.0...v0.8.1>>

# eggla 0.8.0

## Features

- In `R/compute_apar.R`,
  - Feat: allow to compute adiposity peak and adiposity rebound on raw data from a `"lme"` model object.
  - Refactor: rename arguments.
- In `tests`, `DESCRIPTION`,
  - Feat: now uses the third edition of `testthat`.

## Bug Fixes

- In `R/do_eggla_gwas.R`,
  - Fix: no visible global function definition for 'patterns'.

## Build

- In `inst/setup`,
  - build: update lock files from `renv` and `pak` based on local package.
- In `data-raw/lock.R`,
  - build: update script to work on local version for both `renv` and `pak`.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.7.0...v0.8.0>>

# eggla 0.7.0

## Features

- In `R/compute_apar.R`,
  - Feat: Allows to define the range in which to compute adiposity peak and adiposity rebound.
- In `R/egg_model.R`, `R/time_model.R`,
  - Feat: Allows to use (or not) AR(1) auto-correlation in the model, if `TRUE` (default: `FALSE`), the function will try without if all models fail.

## Fixes

- In `vignettes/articles/run-eggla.Rmd`, `vignettes/articles/models-diagnostics.Rmd`,
  - Fix: Uses CAR1 by default in vignettes, only to speed-up computation.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.6.1...v0.7.0>>

# eggla 0.6.1

## Fixes

- In `R/compute_apar.R`,
  - Fix: returns only the first local maxima (adiposity peak) and minima (adiposity rebound).

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.6.0...v0.6.1>>

# eggla 0.6.0

## New Feature

- In `R/do_eggla_gwas.R`:
  - Feat: Function to run GWAS on `eggla` results using PLINK2 (and BCFtools).
- In `vignettes/articles/run_eggla`,
  - Feat: Add "GWAS" step.
  - Refactor: use "R EOF" syntax.

## Fixes

- In `R/run_eggla.R`:
  - Fix: Rename (add "s") AUCs csv output file for consistency.
  - Docs: Add missing documentation for parameters.
- In `R/plot_slopes.R`:
  - Fix: hardcoded "age" variable.
  - Fix: ignore case in variable search.
- In `R/plot_egg_slopes.R`:
  - Fix: hardcoded "age" variable.
  - Fix: ignore case in variable search.
- In `R/compute_apar.R`:
  - Fix: ignore case in variable search.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.5.2...v0.6.0>>

# eggla 0.5.2

- Fix: `renv::restore` issue with `renv.lock` from GitHub.
- Fix: Update `pak` install command.
- Chore: Upgrade lock files.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.5.1...v0.5.2>>

# eggla 0.5.1

- No longer uses `nlme::corCAR1(form = ~ 1 | ID)` in models.
- Add Kimberley Burrows (@burrowsk) and Anni Heiskala as authors.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.5.0...v0.5.1>>

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

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.4.4...v0.5.0>>

# eggla 0.4.4

- In `R/egg_model.R`,
  - Three cublic spline random effects formula are tested in the following order:
    1. `~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3)) | ID`
    2. `~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))[,1:3] | ID`
    3. `~ gsp(age, knots = c(2, 8, 12), degree = rep(1, 4), smooth = rep(2, 3)) | ID`
  - Add message about which model is currently being computed.
- Add `pkgdown` website.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.4.3...v0.4.4>>

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

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.4.2...v0.4.3>>

# eggla 0.4.2

- In `README.(R)md`,
  - Fix typo in `renv::init` calls.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.4.1...v0.4.2>>

# eggla 0.4.1

- In `README.(R)md`,
  - Add `renv` cleaning in bash example script.
  - Add setup for both interactive and non-interactive use.
- In `R/run_eggla.R`,
  - Switch from `svglite` to `ragg` to avoid issues with high number of elements.
  - Fix `id_var` which was not set with `id_var = "egg_id"` internally.
- In `DESCRIPTION`,
  - Switch from `svglite` to `ragg` to avoid issues with high number of elements.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.4.0...v0.4.1>>

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

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.3.0...v0.4.0>>

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

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.2.0...v0.3.0>>

# eggla 0.2.0

- In `R/time_model.R`,
  - Fix output when using `as_text = TRUE`.
  - Add `cov` to allow additional covariates.
- In `inst/rmarkdown/templates/eggla/skeleton/skeleton.Rmd`,
  - Add `cov` to allow additional covariates.

**Full Changelog**: <<https://github.com/mcanouil/eggla/compare/v0.1.0...v0.2.0>>

# eggla 0.1.0

- Initial version.
