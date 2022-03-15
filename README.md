
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EGG Longitudinal Analysis <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/eggla.svg?label=latest%20tag&include_prereleases)](https://github.com/mcanouil/eggla)
[![R-CMD-check](https://github.com/mcanouil/eggla/workflows/R-CMD-check/badge.svg)](https://github.com/mcanouil/eggla/actions)
<!-- badges: end -->

Tools for longitudinal analysis within the EGG (Early Growth Genetics)
Consortium.

------------------------------------------------------------------------

-   [Installation](#installation)
-   [Run The Cubic Splines (Random Cubic/Linear Splines)
    Analysis](#run-the-cubic-splines-random-clubic-linear-splines-analysis)
    -   [Setup](#setup)
    -   [Data](#data)
    -   [Modelling Female](#modelling-female)
    -   [Predicted Values](#predicted-values)
    -   [Residuals](#residuals)
    -   [Predicted Average Slopes](#predicted-average-slopes)
    -   [Area Under The Curves](#area-under-the-curve)
-   [Run Non-Interactively](#run-non-interactively)
-   [Run Interactively](#run-interactively)
-   [License](#license)
-   [Code of Conduct](#code-of-conduct)

------------------------------------------------------------------------

## Installation

-   Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("mcanouil/eggla")
```

-   Install a particular version:

``` r
# install.packages("remotes")
remotes::install_github("mcanouil/eggla@v0.4.4")
```

## Run The Cubic Splines (Random Cubic/Linear Splines) Analysis

### Setup

``` r
# install.packages("remotes")
# remotes::install_github(c("carriedaymont/growthcleanr@v2.0.0", "mcanouil/eggla"))
library(eggla)
library(growthcleanr)
library(broom.mixed)
library(data.table, quietly = TRUE)
#> data.table 1.14.2 using 2 threads (see ?getDTthreads).  Latest news: r-datatable.com

# Setup for plots
library(ggplot2, quietly = TRUE)
library(patchwork)
library(ggbeeswarm) # remotes::install_github("eclarke/ggbeeswarm")
library(ggdist)
theme_set(theme_minimal())
options(
  ggplot2.discrete.colour = function(...) scale_colour_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.discrete.fill = function(...) scale_fill_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.colour = function(...) scale_colour_viridis_c(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.fill = function(...) scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
)
```

### Data

**eggla** includes a small dataset with 1050 rows and 6 variables.

-   `ID` (`character`): ID using three digits.

-   `age` (`numeric`): age in years.

-   `sex` (`integer`): sex with 1: male and 0: female.

-   `weight` (`numeric`): weight in kilograms.

-   `height` (`integer`): height in centimetres.

-   `bmi` (`numeric`): Body Mass Index in kilograms per quare metre.

    ``` r
    data("bmigrowth")
    bmigrowth <- as.data.table(bmigrowth)
    ```

    ``` r
    bmigrowth[bmigrowth[["ID"]] == "001"]
    #>     ID   age sex    weight height      bmi
    #> 1: 001  0.00   0  3.318845     47 15.02420
    #> 2: 001  0.25   0  6.585227     60 18.29230
    #> 3: 001  0.50   0  7.239751     64 17.67517
    #> 4: 001  0.75   0  9.474914     66 21.75141
    #> 5: 001  2.00   0 13.990333     79 22.41681
    #> 6: 001  4.00   0 25.909311    105 23.50051
    #> 7: 001  6.00   0 30.117745    106 26.80469
    #> 8: 001 12.00   0 77.958539    148 35.59101
    #> 9: 001 14.00   0 92.021767    156 37.81302
    ```

    ``` r
    ggplot(data = bmigrowth) +
      aes(x = age, y = bmi, colour = factor(ID)) +
      geom_path(na.rm = TRUE, alpha = 0.25) +
      geom_point(size = 0.5, na.rm = TRUE, alpha = 0.25) +
      stat_smooth(
        method = "loess",
        formula = y ~ x,
        linetype = 1,
        colour = "firebrick",
        se = FALSE
      ) +
      theme(legend.position = "none") +
      labs(x = "AGE (years)", y = "BMI (kg/m²)") +
      facet_grid(
        cols = vars(sex),
        margins = TRUE,
        labeller = labeller(
          .cols = function(x) {
            c("0" = "FEMALE", "1" = "MALE", "2" = "FEMALE", "(all)" = "ALL")[x]
          }
        )
      )
    ```

    <img src="man/figures/README-unnamed-chunk-7.svg" width="100%" />

#### With Daymont’s QC

``` r
pheno_dt <- bmigrowth[
  j = `:=`(
    "agedays" = floor(age * 365.25), # convert to age in days and as integers ...
    "WEIGHTKG" = as.numeric(weight),
    "HEIGHTCM" = as.numeric(height)
  )
][
  j = `:=`(# recode sex with Male = 0 and Female = 1...
    "sex_daymont" = c("0" = "1", "1" = "0")[as.character(sex)]
  )
]

pheno_dt_long <- melt(
  data = pheno_dt,
  id.vars = c("ID", "age", "sex", "agedays", "sex_daymont"),
  measure.vars = c("WEIGHTKG", "HEIGHTCM"),
  variable.name = "param",
  value.name = "measurement"
)[
  j = clean := cleangrowth(# Daymont's QC from 'growthcleanr'
    subjid = ID,
    param = param,
    agedays = agedays,
    sex = sex_daymont,
    measurement = measurement,
    quietly = TRUE
  )
]
pheno_dt_clean <- dcast(
  data = pheno_dt_long[clean %in% "Include"], # Exclude all flags
  formula = ... ~ param,
  value.var = "measurement"
)[
  j = "bmi" := WEIGHTKG / (HEIGHTCM / 100)^2 # recompute bmi based using QCed variables
][
  !is.na(bmi) # exclude missing BMI related to measurements exclusion
]
```

#### Without Daymont’s QC

``` r
pheno_dt_clean <- bmigrowth
```

### Modelling Female

``` r
pheno_dt_female <- pheno_dt_clean[sex_daymont == 1]
```

``` r
res <- egg_model(
  formula = log(bmi) ~ age, # + covariates, e.g., `log(bmi) ~ age + covariates`
  data = pheno_dt_female,
  id_var = "ID",
  random_complexity = 2 # "auto", 3, 2 or 1.
)
#> Fitting model:
#>   nlme::lme(
#>     fixed = log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3)),
#>     data = data,
#>     random = ~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))[,1:3] | ID,
#>     na.action = stats::na.omit,
#>     method = "ML",
#>     control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
#>   )
#> lme.formula
#> log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))
#> data
#> ~gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))[, 1:3] | ID
#> ML
#> stats::na.omit
#> nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
class(res)
#> [1] "lme"
sres <- tidy(res)
sres[["term"]] <- sub("gsp\\(.*\\)\\)", "gsp(...)", sres[["term"]]) # simplify output
sres
#> # A tibble: 18 x 8
#>    effect   group    term           estimate std.error    df statistic   p.value
#>    <chr>    <chr>    <chr>             <dbl>     <dbl> <dbl>     <dbl>     <dbl>
#>  1 fixed    fixed    (Intercept)     2.76e+0   0.0217    409    127.    0       
#>  2 fixed    fixed    gsp(...)D1(0)   2.45e-1   0.0433    409      5.67  2.75e- 8
#>  3 fixed    fixed    gsp(...)D2(0)  -3.13e-1   0.0528    409     -5.92  6.72e- 9
#>  4 fixed    fixed    gsp(...)D3(0)   1.80e-1   0.0286    409      6.31  7.37e-10
#>  5 fixed    fixed    gsp(...)C(2).3 -1.91e-1   0.0297    409     -6.43  3.46e-10
#>  6 fixed    fixed    gsp(...)C(8).3  1.98e-2   0.00441   409      4.49  9.41e- 6
#>  7 fixed    fixed    gsp(...)C(12)~ -2.94e-2   0.0109    409     -2.69  7.45e- 3
#>  8 ran_pars ID       sd_(Intercept)  9.13e-2  NA          NA     NA    NA       
#>  9 ran_pars ID       cor_gsp(...)[~ -4.58e-1  NA          NA     NA    NA       
#> 10 ran_pars ID       cor_gsp(...)[~  2.92e-1  NA          NA     NA    NA       
#> 11 ran_pars ID       cor_gsp(...)[~  3.63e-3  NA          NA     NA    NA       
#> 12 ran_pars ID       sd_gsp(...)[,~  3.43e-2  NA          NA     NA    NA       
#> 13 ran_pars ID       cor_gsp(...)[~ -9.22e-1  NA          NA     NA    NA       
#> 14 ran_pars ID       cor_gsp(...)[~ -4.18e-3  NA          NA     NA    NA       
#> 15 ran_pars ID       sd_gsp(...)[,~  4.33e-3  NA          NA     NA    NA       
#> 16 ran_pars ID       cor_gsp(...)[~  1.94e-4  NA          NA     NA    NA       
#> 17 ran_pars ID       sd_gsp(...)[,~  2.76e-6  NA          NA     NA    NA       
#> 18 ran_pars Residual sd_Observation  6.60e-2  NA          NA     NA    NA
```

### Predicted Values

``` r
ggplot() +
  aes(x = age, y = bmi) +
  stat_smooth(
    data = pheno_dt_female,
    mapping = aes(colour = "Loess"),
    method = "loess", formula = y ~ x, linetype = 2, se = FALSE
  ) +
  geom_path(
    data = data.table(
      age = seq(min(pheno_dt_female[["age"]]), max(pheno_dt_female[["age"]]), 0.1)
    )[
      j = bmi := exp(predict(res, .SD, level = 0)),
      .SDcols = "age"
    ],
    mapping = aes(colour = "Cubic Splines (Random Linear Splines)"),
  ) +
  labs(x = "AGE (years)", y = "BMI (kg/m²)", colour = "Model")
```

<img src="man/figures/README-unnamed-chunk-12.svg" width="100%" />

### Residuals

``` r
plot_residuals(
  x = "age",
  y = "log(bmi)",
  fit = res
) +
  plot_annotation(
    title = "Cubic Splines (Random Linear Splines) - BMI - Female",
    tag_levels = "A"
  )
```

<img src="man/figures/README-unnamed-chunk-13.svg" width="100%" />

### Predicted Average Slopes

``` r
res_pred_slopes <- egg_slopes(
  fit = res,
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
  id_var = "ID"
)
head(res_pred_slopes)
#>    ID pred_period_0 pred_period_0.5 pred_period_1.5 pred_period_5 pred_period_6
#> 1 001      2.856292        2.961108        3.022253      3.181718      3.267542
#> 2 004      2.749752        2.837953        2.868957      2.955361      3.029578
#> 3 005      2.729462        2.820443        2.858218      2.981043      3.069301
#> 4 006      2.627920        2.716995        2.750096      2.847498      2.925902
#> 5 007      2.847711        2.962818        3.042947      3.252084      3.347309
#> 6 009      2.748606        2.822970        2.830199      2.874348      2.948193
#>   pred_period_10 pred_period_12 pred_period_17 slope_0--0.5 slope_1.5--5
#> 1       3.509050       3.570135       3.460015    0.2096327   0.04556129
#> 2       3.265848       3.349027       3.366223    0.1764019   0.02468692
#> 3       3.377887       3.506917       3.667008    0.1819615   0.03509292
#> 4       3.183563       3.280224       3.339250    0.1781508   0.02782906
#> 5       3.605123       3.661583       3.502630    0.2302137   0.05975346
#> 6       3.234982       3.374625       3.623991    0.1487270   0.01261390
#>   slope_6--10 slope_12--17
#> 1  0.06037706 -0.022024093
#> 2  0.05906732  0.003439203
#> 3  0.07714657  0.032018278
#> 4  0.06441523  0.011805134
#> 5  0.06445348 -0.031790604
#> 6  0.07169729  0.049873220
```

``` r
wrap_plots(
  ggplot(
    data = melt(
      data = setDT(res_pred_slopes),
      id.vars = c("ID"),
      measure.vars = patterns("^slope_"),
      variable.name = "period_interval",
      value.name = "slope"
    )[
      j = period_interval := factor(
        x = gsub("slope_", "", period_interval),
        levels = gsub("slope_", "", unique(period_interval))
      )
    ]
  ) +
    aes(x = slope, y = period_interval) +
    stat_halfeye(
      mapping = aes(fill = period_interval),
      justification = -0.30,
      .width = 0,
      scale = 0.5
    ) +
    geom_boxplot(
      mapping = aes(colour = period_interval), width = 0.25, outlier.colour = NA
    ) +
    geom_quasirandom(
      mapping = aes(fill = period_interval, colour = period_interval),
      shape = 21,
      alpha = 0.25,
      groupOnX = FALSE,
      width = 0.15
    ) +
    labs(x = "Predicted Slope", y = "Period Interval (years)") +
    theme(legend.position = "none"),
  ggplot(
    data = melt(
      data = setDT(res_pred_slopes),
      id.vars = c("ID"),
      measure.vars = patterns("^pred_period_"),
      variable.name = "period",
      value.name = "pred"
    )[
      j = period := as.numeric(gsub("pred_period_", "", period))
    ]
  ) +
    aes(x = period, y = pred, colour = factor(ID)) +
    geom_path() +
    labs(x = "Age (years)", y = "Predicted Values") +
    theme(legend.position = "none"),
    ncol = 2
) +
  plot_annotation(tag_levels = "A")
```

<img src="man/figures/README-unnamed-chunk-15.svg" width="100%" />

### Area Under The Curve

``` r
res_auc <- egg_auc(
  fit = res,
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
  id_var = "ID"
)
head(res_auc)
#>    ID auc_0--0.5 auc_1.5--5 auc_6--10 auc_12--17
#> 1 001   1.457194  10.761594  13.64073   17.99617
#> 2 004   1.399728  10.082485  12.65643   17.16601
#> 3 005   1.390261  10.102862  12.95134   18.29588
#> 4 006   1.339025   9.684058  12.28203   16.92174
#> 5 007   1.455499  10.928559  14.00377   18.35351
#> 6 009   1.395641   9.854303  12.40419   17.82026
```

``` r
ggplot(
  data = melt(
    data = setDT(res_auc),
    id.vars = c("ID"),
    measure.vars = patterns("^auc_"),
    variable.name = "period_interval",
    value.name = "auc"
  )[
    j = period_interval := factor(
      x = gsub("auc_", "", period_interval),
      levels = gsub("auc_", "", unique(period_interval))
    )
  ]
) +
  aes(x = auc, y = period_interval) +
  stat_halfeye(
    mapping = aes(fill = period_interval),
    justification = -0.20,
    .width = 0,
    scale = 1
  ) +
  geom_boxplot(
    mapping = aes(colour = period_interval), width = 0.25, outlier.colour = NA
  ) +
  geom_quasirandom(
    mapping = aes(fill = period_interval, colour = period_interval),
    shape = 21,
    alpha = 0.25,
    groupOnX = FALSE,
    width = 0.15
  ) +
  labs(x = "Area Under The Curve (AUC)", y = "Period Interval (years)") +
  theme(legend.position = "none")
```

<img src="man/figures/README-unnamed-chunk-17.svg" width="100%" />

## Run Non-Interactively

1.  Copy and edit the following code to a new file (e.g.,
    `run_eggla.sh`) on the server that will run the analysis with the
    appropriate parameters.

-   `renv` (recommended):

    ``` bash
    #!/bin/bash

    home_analysis="/tmp/egg_analysis" # to be changed to the folder in which "egg_analysis" is to be performed

    mkdir $home_analysis 

    cd $home_analysis

    Rscript \
      -e 'wd <- "/tmp/egg_analysis"' \
      -e 'temp_library <- file.path(wd, "R")' \
      -e 'dir.create(temp_library, recursive = TRUE)' \
      -e '.libPaths(temp_library)' \
      -e 'install.packages("renv", lib = temp_library, repos = "http://cloud.r-project.org")' \
      -e 'library("renv")' \
      -e 'renv::init(bare = TRUE, settings = list(use.cache = FALSE))' \
      -e 'renv::restore(lockfile = "https://raw.githubusercontent.com/mcanouil/eggla/main/inst/setup/renv.lock")' \
      -e 'renv::install("mcanouil/eggla@v0.4.4")' \
      -e 'unlink(temp_library, recursive = TRUE)'

    Rscript \
      -e 'wd <- "/tmp/egg_analysis"' \
      -e 'setwd(wd)' \
      -e 'library(eggla)' \
      -e 'library(data.table)' \
      -e 'res <- try(run_eggla(
        data = fread("/tmp/bmigrowth.csv"), # to be changed with the path of the file containing the data
        id_variable = "ID",
        age_days_variable = NULL, # computed based on "age_years_variable" if not provided. Only used for QC.
        age_years_variable = "age", 
        weight_kilograms_variable = "weight",
        height_centimetres_variable = "height",
        sex_variable = "sex",
        covariates = NULL,
        male_coded_zero = FALSE,
        parallel = FALSE, # to parallelise Daymont QC
        parallel_n_chunks = 1, # to parallelise Daymont QC
        working_directory = wd # or in that case "/tmp/egg_analysis"
      ))' \
      -e 'if (inherits(res, "try-error")) unlink(wd, recursive = TRUE)'
    ```

-   `pak` (faster):

    ``` bash
    #!/bin/bash

    home_analysis="/tmp/egg_analysis" # to be changed to the folder in which "egg_analysis" is to be performed

    mkdir $home_analysis 

    cd $home_analysis

    Rscript \
      -e 'wd <- "/tmp/egg_analysis"' \
      -e 'temp_library <- file.path(wd, "R")' \
      -e 'dir.create(temp_library, recursive = TRUE)' \
      -e '.libPaths(temp_library)' \
      -e 'install.packages("pak", lib = temp_library, repos = "https://r-lib.github.io/p/pak/devel/")' \
      -e 'library(pak)' \
      -e 'lockfile_install(
        lockfile = "https://raw.githubusercontent.com/mcanouil/eggla/main/inst/setup/pkg.lock",
        lib = temp_library
      )' \
      -e 'pkg_install("mcanouil/eggla@0.4.4", lib = temp_library, upgrade = FALSE, dependencies = FALSE)'

    Rscript \
      -e 'wd <- "/tmp/egg_analysis"' \
      -e 'setwd(wd)' \
      -e 'library(eggla)' \
      -e 'library(data.table)' \
      -e 'res <- try(run_eggla(
        data = fread("/tmp/bmigrowth.csv"), # to be changed with the path of the file containing the data
        id_variable = "ID",
        age_days_variable = NULL, # computed based on "age_years_variable" if not provided. Only used for QC.
        age_years_variable = "age", 
        weight_kilograms_variable = "weight",
        height_centimetres_variable = "height",
        sex_variable = "sex",
        covariates = NULL,
        male_coded_zero = FALSE,
        parallel = FALSE, # to parallelise Daymont QC
        parallel_n_chunks = 1, # to parallelise Daymont QC
        working_directory = wd # or in that case "/tmp/egg_analysis"
      ))' \
      -e 'if (inherits(res, "try-error")) unlink(wd, recursive = TRUE)'
    ```

2.  Run the analysis in bash

    ``` bash
    bash run_eggla.sh
    ```

3.  Retrieve the two archives

        /tmp/egg_analysis/
        ├── 2021-11-23-female.zip
        └── 2021-11-23-male.zip

## Run Interactively

1.  Create the working directory

    ``` bash
    home_analysis="/tmp/egg_analysis" # to be changed to the folder in which "egg_analysis" is to be performed

    mkdir $home_analysis 

    cd $home_analysis
    ```

2.  Start R and setup working directory using `renv` (recommended) or
    `pak` (faster) to restore predefined version of packages

-   `renv` (recommended):

    ``` r
    wd <- "/tmp/egg_analysis"
    temp_library <- file.path(wd, "R")
    dir.create(temp_library, recursive = TRUE)
    .libPaths(temp_library)
    install.packages("renv", lib = temp_library, repos = "http://cloud.r-project.org")
    library("renv")
    renv::init(bare = TRUE, settings = list(use.cache = FALSE))
    renv::restore(lockfile = "https://raw.githubusercontent.com/mcanouil/eggla/main/inst/setup/renv.lock")
    renv::install("mcanouil/eggla@v0.4.4")
    unlink(temp_library, recursive = TRUE)
    ```

-   `pak` (faster):

    ``` r
    wd <- "/tmp/egg_analysis"
    temp_library <- file.path(wd, "R")
    dir.create(temp_library, recursive = TRUE)
    .libPaths(temp_library)
    install.packages("pak", lib = temp_library, repos = "https://r-lib.github.io/p/pak/devel/")
    library(pak)
    lockfile_install(
      lockfile = "https://raw.githubusercontent.com/mcanouil/eggla/main/inst/setup/pkg.lock",
      lib = temp_library
    )
    pkg_install("mcanouil/eggla@0.4.4", lib = temp_library, upgrade = FALSE, dependencies = FALSE)
    ```

3.  Restart R

4.  Run the analysis

    ``` r
    setwd(wd)
    library(eggla)
    library(data.table)
    res <- try(
      run_eggla(
        data = fread("/tmp/bmigrowth.csv"),
        id_variable = "ID",
        age_days_variable = NULL,
        age_years_variable = "age",
        weight_kilograms_variable = "weight",
        height_centimetres_variable = "height",
        sex_variable = "sex",
        covariates = NULL,
        male_coded_zero = FALSE,
        parallel = FALSE,
        parallel_n_chunks = 1,
        working_directory = wd
      )
    )
    if (inherits(res, "try-error")) unlink(wd, recursive = TRUE)
    ```

5.  Retrieve the two archives

        /tmp/egg_analysis/
        ├── 2021-11-23-female.zip
        └── 2021-11-23-male.zip

## License

MIT © [Mickaël Canouil](https://github.com/mcanouil), Nicole Warrington

## Code of Conduct

Please note that the `eggla` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).  
By contributing to this project, you agree to abide by its terms.
