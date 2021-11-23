
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Early Growth Genetics Longitudinal Analysis <img src="man/figures/eggla.png" align="right" width="120" />

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
-   [Run The Cubic Splines (Random Linear Splines)
    Analysis](#run-the-cubic-splines-random-linear-splines-analysis)
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
remotes::install_github("mcanouil/eggla@v0.4.0")
```

## Run The Cubic Splines (Random Linear Splines) Analysis

### Setup

``` r
# install.packages("remotes")
# remotes::install_github("mcanouil/eggla")
library(eggla)
# remotes::install_github("carriedaymont/growthcleanr@v2.0.0")
library(growthcleanr)
library(broom.mixed)
library(data.table, quietly = TRUE)

# Setup for plots
library(ggplot2, quietly = TRUE)
library(patchwork)
# remotes::install_github("eclarke/ggbeeswarm")
library(ggbeeswarm)
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
    ggplot(data = bmigrowth, mapping = aes(x = age, y = bmi, colour = factor(ID))) +
      geom_path(na.rm = TRUE, alpha = 0.25) +
      geom_point(size = 0.5, na.rm = TRUE, alpha = 0.25) +
      stat_smooth(method = "loess", formula = y ~ x, linetype = 1, colour = "firebrick", se = FALSE) +
      theme(legend.position = "none") +
      labs(x = "AGE (years)", y = "BMI (kg/m²)") +
      facet_grid(
        cols = vars(sex),
        margins = TRUE,
        labeller = labeller(
          .cols = function(x) c("0" = "FEMALE", "1" = "MALE", "2" = "FEMALE", "(all)" = "ALL")[x]
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
  j = clean := cleangrowth( # Daymont's QC from 'growthcleanr'
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
  formula = log(bmi) ~ age, # covariates can be added after the term used for the time component, e.g., `log(bmi) ~ age + covariate`
  data = pheno_dt_female
)
#> nlme::lme(
#>   fixed = log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3)),
#>   data = data,
#>   random = ~ gsp(age, knots = c(2, 8, 12), degree = rep(1, 4), smooth = rep(2, 3)) | ID,
#>   na.action = stats::na.omit,
#>   method = "ML",
#>   control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
#> )
class(res)
#> [1] "lme"
sres <- tidy(res)
sres[["term"]] <- sub("gsp\\(.*\\)\\)", "gsp(...)", sres[["term"]]) # simplify output
sres
#> # A tibble: 11 x 8
#>    effect   group    term          estimate std.error    df statistic    p.value
#>    <chr>    <chr>    <chr>            <dbl>     <dbl> <dbl>     <dbl>      <dbl>
#>  1 fixed    fixed    (Intercept)     2.76     0.0223    409    124.      0      
#>  2 fixed    fixed    gsp(...)D1(0)   0.240    0.0463    409      5.19    3.32e-7
#>  3 fixed    fixed    gsp(...)D2(0)  -0.305    0.0566    409     -5.39    1.21e-7
#>  4 fixed    fixed    gsp(...)D3(0)   0.175    0.0306    409      5.72    2.05e-8
#>  5 fixed    fixed    gsp(...)C(2)~  -0.185    0.0318    409     -5.82    1.21e-8
#>  6 fixed    fixed    gsp(...)C(8)~   0.0167   0.00452   409      3.70    2.44e-4
#>  7 fixed    fixed    gsp(...)C(12~  -0.0201   0.0107    409     -1.89    6.01e-2
#>  8 ran_pars ID       sd_(Intercep~   0.0861  NA          NA     NA      NA      
#>  9 ran_pars ID       cor_gsp(...)~  -0.387   NA          NA     NA      NA      
#> 10 ran_pars ID       sd_gsp(...)     0.0160  NA          NA     NA      NA      
#> 11 ran_pars Residual sd_Observati~   0.0714  NA          NA     NA      NA
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
    data = data.table(age = seq(min(pheno_dt_female[["age"]]), max(pheno_dt_female[["age"]]), 0.1))[
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
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
)
head(res_pred_slopes)
#>    ID pred_period_0 pred_period_0.5 pred_period_1.5 pred_period_5 pred_period_6
#> 1 001      2.896950        2.982138        3.011172      3.093593      3.168655
#> 2 004      2.758294        2.841876        2.867700      2.938884      3.010736
#> 3 005      2.731514        2.821683        2.860679      2.977967      3.062991
#> 4 006      2.645488        2.729430        2.755975      2.829683      2.902255
#> 5 007      2.914267        3.009222        3.057790      3.208579      3.303175
#> 6 009      2.726611        2.808914        2.832178      2.894402      2.963694
#>   pred_period_10 pred_period_12 pred_period_17 slope_0--0.5 slope_1.5--5
#> 1       3.443546       3.558354       3.764686    0.1703750   0.02354874
#> 2       3.272785       3.381172       3.571453    0.1671645   0.02033832
#> 3       3.377730       3.512462       3.768604    0.1803370   0.03351077
#> 4       3.167188       3.277017       3.470902    0.1678855   0.02105924
#> 5       3.656200       3.810076       4.114077    0.1899087   0.04308251
#> 6       3.215504       3.318771       3.496252    0.1646047   0.01777847
#>   slope_6--10 slope_12--17
#> 1  0.06872264   0.04126650
#> 2  0.06551222   0.03805608
#> 3  0.07868467   0.05122853
#> 4  0.06623314   0.03877700
#> 5  0.08825641   0.06080027
#> 6  0.06295237   0.03549623
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
    geom_boxplot(mapping = aes(colour = period_interval), width = 0.25, outlier.colour = NA) +
    geom_quasirandom(
      mapping = aes(fill = period_interval, colour = period_interval),
      # colour = "white",
      shape = 21,
      alpha = 0.25,
      groupOnX = FALSE,
      width = 0.15#,
      # side = -1,
      # cex = 0.5
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
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
)
head(res_auc)
#>    ID auc_0--0.5 auc_1.5--5 auc_6--10 auc_12--17
#> 1 001   1.472495  10.573508  13.26934   18.52345
#> 2 004   1.402765  10.051692  12.61198   17.59741
#> 3 005   1.391022  10.107799  12.92638   18.41851
#> 4 006   1.346452   9.665070  12.18383   17.08564
#> 5 007   1.483595  10.856314  13.96369   20.02623
#> 6 009   1.386604   9.911684  12.40334   17.25341
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
  geom_boxplot(mapping = aes(colour = period_interval), width = 0.25, outlier.colour = NA) +
  geom_quasirandom(
    mapping = aes(fill = period_interval, colour = period_interval),
    # colour = "white",
    shape = 21,
    alpha = 0.25,
    groupOnX = FALSE,
    width = 0.15#,
    # side = -1,
    # cex = 0.5
  ) +
  labs(x = "Area Under The Curve (AUC)", y = "Period Interval (years)") +
  theme(legend.position = "none")
```

<img src="man/figures/README-unnamed-chunk-17.svg" width="100%" />

## Run Non-Interactively

1.  Copy and edit the following code to a new file (e.g.,
    `run_eggla.sh`) on the server that will run the analysis with the
    appropriate parameters.

    ``` bash
    #!/bin/bash

    home_analysis="/tmp/egg_analysis" # to be changed to the folder in which "egg_analysis" is to be performed

    mkdir $home_analysis 

    cd $home_analysis

    Rscript \
      -e 'temp_library <- file.path(tempdir(), "R")' \
      -e 'dir.create(temp_library, recursive = TRUE)' \
      -e 'install.packages("renv", lib = temp_library, repos = "http://cloud.r-project.org")' \
      -e 'library("renv", lib.loc = temp_library)' \
      -e 'renv::init(base = TRUE)' \
      -e 'renv::install("mcanouil/eggla@v0.4.0")' \
      -e 'renv::restore(lockfile = system.file("setup", "renv.lock", package = "eggla"))' \
      -e 'unlink(temp_library, recursive = TRUE)'

    Rscript -e 'renv::activate()'

    Rscript \
      -e 'wd <- "/tmp/egg_analysis"' \
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
      -e 'if (inherits(res, "try-error")) { # cleanup
        unlink(wd, recursive = TRUE)
      } else {
        unlink(c(file.path(wd, "renv"), file.path(wd, "renv.lock")), recursive = TRUE)
      }'
    ```

2.  Run the analysis in bash

    ``` bash
    bash run_eggla.sh
    ```

3.  Retrieve the two archives

        /tmp/egg_analysis/
        ├── 2021-11-23-female.zip
        └── 2021-11-23-male.zip

## Run Non Interactively

1.  Setup working directory using `renv` to restore predefined version
    of packages

``` r
temp_library <- file.path(tempdir(), "R")
dir.create(temp_library, recursive = TRUE)
install.packages("renv", lib = temp_library, repos = "http://cloud.r-project.org")
library("renv", lib.loc = temp_library)
renv::init(base = TRUE)
renv::install("mcanouil/eggla@v0.4.1")
renv::restore(lockfile = system.file("setup", "renv.lock", package = "eggla"))
unlink(temp_library, recursive = TRUE)
```

2.  Restart R

3.  Run the analysis

    ``` r
    wd <- "/tmp/egg_analysis"
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
    if (inherits(res, "try-error")) { # cleanup
      unlink(wd, recursive = TRUE)
    } else {
      unlink(c(file.path(wd, "renv"), file.path(wd, "renv.lock")), recursive = TRUE)
    }
    ```

4.  Retrieve the two archives

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
