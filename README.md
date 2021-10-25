
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
    -   [Modelling female](#modelling-female)
    -   [Predicted values](#predicted-values)
    -   [Residuals](#residuals)
    -   [Predicted Average Slopes](#predicted-average-slopes)
    -   [Area Under The Curves](#area-under-the-curve)
-   [License](#license)
-   [Code of Conduct](#code-of-conduct)

------------------------------------------------------------------------

## Installation

``` r
# Install the development version from GitHub:
# install.packages("remotes")
remotes::install_github("mcanouil/eggla")

# Or a particular version:
remotes::install_github("mcanouil/eggla@v0.4.0")
```

## Run The Cubic Splines (Random Linear Splines) Analysis

### Setup

``` r
# install.packages("remotes")
# remotes::install_github("mcanouil/eggla@v0.4.0")
library(eggla)
# remotes::install_github("carriedaymont/growthcleanr@v2.0.0")
library(growthcleanr)
library(broom.mixed)
library(data.table)
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

    <img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

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

visits_long <- melt(
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
    quietly = FALSE
  )
]
#> [2021-10-25 16:43:39] Begin processing pediatric data...
#> [2021-10-25 16:43:39] Calculating z-scores...
#> [2021-10-25 16:43:40] Calculating SD-scores...
#> [2021-10-25 16:43:40] Re-centering data...
#> [2021-10-25 16:43:40] Using NHANES reference medians...
#> [2021-10-25 16:43:40] Note: input data has at least one age-year with < 100 subjects...
#> [2021-10-25 16:43:40] Cleaning growth data in 1 batch(es)...
#> [2021-10-25 16:43:40] Processing Batch #1...
#> [2021-10-25 16:43:40] Preliminarily identify potential extraneous...
#> [2021-10-25 16:43:40] Identify potentially swapped measurements...
#> [2021-10-25 16:43:40] Exclude measurements carried forward...
#> [2021-10-25 16:43:40] Exclude extreme measurements based on SD...
#> [2021-10-25 16:43:40] Exclude extreme measurements based on EWMA...
#> [2021-10-25 16:43:40] Exclude extraneous based on EWMA...
#> [2021-10-25 16:43:40] Exclude moderate errors based on EWMA...
#> [2021-10-25 16:43:44] Exclude heights based on growth velocity...
#> [2021-10-25 16:43:46] Exclude single measurements and pairs...
#> [2021-10-25 16:43:46] Exclude all measurements if maximum threshold of errors is exceeded...
#> [2021-10-25 16:43:47] Completed Batch #1...
#> [2021-10-25 16:43:47] Done with pediatric data!
#> [2021-10-25 16:43:47] No adult data. Moving to postprocessing...
visits_clean <- dcast(
  data = visits_long[clean %in% "Include"], # Exclude all flags
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
pheno_dt <- bmigrowth
```

### Modelling female

``` r
pheno_dt_female <- pheno_dt[sex_daymont == 1]
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
#>    effect   group    term                     estimate std.error    df statistic   p.value
#>    <chr>    <chr>    <chr>                       <dbl>     <dbl> <dbl>     <dbl>     <dbl>
#>  1 fixed    fixed    (Intercept)                2.65     0.0148    478    179.    0       
#>  2 fixed    fixed    gsp(...)D1(0)              0.500    0.0306    478     16.3   5.41e-48
#>  3 fixed    fixed    gsp(...)D2(0)             -0.603    0.0404    478    -14.9   1.47e-41
#>  4 fixed    fixed    gsp(...)D3(0)              0.331    0.0225    478     14.7   1.39e-40
#>  5 fixed    fixed    gsp(...)C(2).3            -0.345    0.0238    478    -14.5   1.47e-39
#>  6 fixed    fixed    gsp(...)C(8).3             0.0239   0.00499   478      4.78  2.33e- 6
#>  7 fixed    fixed    gsp(...)C(12).3           -0.0278   0.0121    478     -2.30  2.21e- 2
#>  8 ran_pars ID       sd_(Intercept)             0.0727  NA          NA     NA    NA       
#>  9 ran_pars ID       cor_gsp(...).(Intercept)  -0.376   NA          NA     NA    NA       
#> 10 ran_pars ID       sd_gsp(...)                0.0170  NA          NA     NA    NA       
#> 11 ran_pars Residual sd_Observation             0.0834  NA          NA     NA    NA
```

### Predicted values

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

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

### Residuals

    ```r
    plot_residuals(
      x = "age",
      y = "log(bmi)",
      fit = res,
      variables_unit = list(age = "years", bmi = "kg/m²")
    ) +
      plot_annotation(
        title = "Cubic Splines (Random Linear Splines) - BMI - Female",
        tag_levels = "A"
      )
    ```

    <img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

### Predicted average slopes

``` r
res_pred_slopes <- predict_average_slopes(
  fit = res,
  method = "cubic_splines",
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
)
#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length
head(res_pred_slopes)
#>    ID pred_period_0 pred_period_0.5 pred_period_1.5 pred_period_5 pred_period_6 pred_period_10 pred_period_12 pred_period_17 slope_0--0.5 slope_1.5--5 slope_6--10 slope_12--17
#> 1 082      2.778006        2.976054        3.182224     5.2907551     6.8812493     20.9519265      34.424008      97.919239    0.3960961  0.602437412  3.51766931   12.6990460
#> 2 083      2.617273        2.797292        2.848834     2.4998666     2.2791658     -0.2564255      -2.868036     -15.437107    0.3600383 -0.099705014 -0.63389782   -2.5138142
#> 3 080      2.634690        2.818808        2.893454     2.9086731     2.9508104      2.7747475       2.452312       0.788656    0.3682351  0.004348234 -0.04401571   -0.3327312
#> 4 031      2.540167        2.705490        2.664957     0.8611134    -0.4161378    -12.4969440     -24.364382     -80.930349    0.3306469 -0.515383889 -3.02020155  -11.3131932
#> 5 007      2.638875        2.841808        2.974785     3.8913017     4.5500573      9.5968827      14.366314      37.461554    0.4058660  0.261861938  1.26170633    4.6190480
#> 6 033      2.630668        2.804737        2.847105     2.3627886     2.0616905     -1.0360734      -4.205708     -19.689278    0.3481373 -0.138376036 -0.77444097   -3.0967139
```

``` r
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
  aes(x = slope) +
  geom_histogram(bins = 30) +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  facet_grid(cols = vars(period_interval), scales = "free") +
  labs(x = "Predicted Slope", y = "Count")
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

### Area under the curve

``` r
res_auc <- compute_auc(
  fit = res,
  method = "cubic_splines",
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
)
#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length

#> Warning in fxef + as.numeric(rnef[i, ]): longer object length is not a multiple of shorter object length
head(res_auc)
#>    ID auc_0--0.5 auc_1.5--5  auc_6--10  auc_12--17
#> 1 082   1.442592  13.643044  50.906453  309.546353
#> 2 083   1.359382   9.427155   5.109394  -41.152079
#> 3 080   1.368925  10.037208  11.700014    9.003379
#> 4 031   1.317984   6.972776 -21.449909 -243.650962
#> 5 007   1.375586  11.457524  26.817790  122.090360
#> 6 033   1.364491   9.246099   3.268964  -54.156793
```

``` r
ggplot(
  data = melt(
    data = setDT(res_auc),
    id.vars = "ID",
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
  aes(x = auc) +
  geom_histogram(bins = 30) +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  facet_grid(cols = vars(period_interval), scales = "free") +
  labs(x = "Area Under The Curve (AUC)", y = "Count")
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

## License

MIT © [Mickaël Canouil](https://github.com/mcanouil), Nicole Warrington

## Code of Conduct

Please note that the `eggla` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).  
By contributing to this project, you agree to abide by its terms.
