message(timestamp(quiet = TRUE))
### Project Setup ==================================================================================
library(here)
output_directory <- here("outputs", "models-diagnostics")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE, mode = "0775")

phenotype_csv <- here("phenotypes.csv")
is_male_zero <- FALSE # Is male coded 0?


### Load Packages ==================================================================================
#' # Setup
#'
#+ r-packages
eggla_version <- "0.3.0"
growthcleanr_version <- "2.0.0"
suppressPackageStartupMessages({
  if (nchar(system.file(package = "remotes")) == 0) {
    install.packages("remotes")
  }

  if (
    nchar(system.file(package = "eggla")) == 0 ||
      packageVersion("eggla") != eggla_version
  ) {
    remotes::install_github(sprintf("mcanouil/eggla@v%s", eggla_version))
  }
  library(eggla)


  if (
    nchar(system.file(package = "growthcleanr")) == 0 ||
      packageVersion("growthcleanr") != growthcleanr_version
  ) {
    remotes::install_github(sprintf("carriedaymont/growthcleanr@v%s", growthcleanr_version))
  }
  library(growthcleanr)

  library(data.table)
  library(nlme)
  library(stats)

  library(grDevices)
  library(ggplot2)
  library(ggtext)
  library(patchwork)

  library(gt)
  library(performance)

  library(future)
  library(future.apply)
  library(future.callr)
})


### project setup ==================================================================================
plan(future.callr::callr, workers = min(40, availableCores()))
message(sprintf("Number of workers: %d", future::nbrOfWorkers()))


### Tables and Figures Theme =======================================================================
options(
  ggplot2.discrete.colour = function(...) scale_colour_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.discrete.fill = function(...) scale_fill_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.colour = function(...) scale_colour_viridis_c(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.fill = function(...) scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
)
theme_set(theme_minimal(base_family = "Verdana"))
theme_update(
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_markdown(),
  plot.subtitle = element_markdown(face = "italic"),
  plot.caption = element_markdown(face = "italic"),
  axis.title.x = element_markdown(),
  axis.text.x = element_markdown(),
  axis.title.y = element_markdown(),
  axis.text.y = element_markdown()
)


### Analysis =======================================================================================
#' ## Data
#'
#' + `age` - age in years (double).
#' + `agedays` - age in days (integer).
#' + `WEIGHTKG` - weight in kilograms (double).
#' + `HEIGHTCM` - height in centimetres (double).
#'
#+ input-data
pheno_dt <- fread(phenotype_csv)

#+ format-data
pheno_dt[
  j = `:=`(
    "agedays" = floor(age * 365.25), # convert to age in days and as integers ...
    "WEIGHTKG" = as.numeric(weight),
    "HEIGHTCM" = as.numeric(height)
  )
]

pheno_dt[# recode sex with Male = 0 and Female = 1 (only for Daymont' QC procedure)
  j = `:=`(
    "sex_daymont" = {
      if (is_male_zero) {
        as.character(sex)
      } else {
        c("0" = "1", "1" = "0")[as.character(sex)]
      }
    }
  )
]

#' ### Add Daymont's QC tags
#'
#+ add-daymont-data
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

#' ### Compute BMI with raw measurements
#'
#+ visits-raw
visits_raw <- dcast(
  data = visits_long,
  formula = ... ~ param,
  value.var = "measurement"
)[
  j = "bmi" := WEIGHTKG / (HEIGHTCM / 100)^2 # recompute bmi based on QC variables
][
  !is.na(bmi) # exclude missing BMI related to measurements exclusion
]

#' ### Compute BMI with QCed measurements
#'
#+ visits-qc
visits_clean <- dcast(
  data = visits_long[clean %in% "Include"], # Exclude all flags
  formula = ... ~ param,
  value.var = "measurement"
)[
  j = "bmi" := WEIGHTKG / (HEIGHTCM / 100)^2 # recompute bmi based on QC variables
][
  !is.na(bmi) # exclude missing BMI related to measurements exclusion
]

#' # Model diagnostics (With Daymont's code for sex, Female: 1 and Male: 0)
#'
#+ models
fixed_effect <- c(
  rep("log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))", 2),
  rep("log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))", 4),
  rep("log(bmi) ~ poly(age, degree = 3)", 4)
)
random_effect <- c(
  "~ gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2)) | ID",
  "~ 1 | ID",
  "~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3)) | ID",
  "~ gsp(age, knots = c(2, 8, 12), degree = rep(2, 4), smooth = rep(2, 3)) | ID",
  "~ gsp(age, knots = c(2, 8, 12), degree = rep(1, 4), smooth = rep(2, 3)) | ID",
  "~ 1 | ID",
  "~ poly(age, degree = 3) | ID",
  "~ poly(age, degree = 2) | ID",
  "~ age | ID",
  "~ 1 | ID"
)

all_models <- setDT(expand.grid(
  sex = c(0, 1),
  dataset = c(
    "visits_raw",
    "visits_clean",
    # "visits_clean[age <= 11]",
    # "visits_clean[age <= 12]",
    # "visits_clean[age <= 13]",
    "visits_clean[between(age, 1, 18)]"
  ),
  correlation_structure = c("NULL", "corCAR1(form = ~ 1 | ID)"),
  models = sprintf("fixed = %s, random = %s", fixed_effect, random_effect),
  default = 'na.action = na.omit, method = "ML", control = lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)',
  lme = list(NULL),
  results = list(NULL),
  perf_check_model = list(NULL),
  perf_model_performance = list(NULL),
  stringsAsFactors = FALSE
))

all_models[["lme"]] <- future_mapply(
  FUN = function(models, dataset, sex, correlation_structure, default) {
    tryCatch(
      expr = eval(parse(text = sprintf(
        "list(lme(%s, data = %s[sex_daymont == %s], correlation = %s, %s))",
        models,
        dataset,
        sex,
        correlation_structure,
        default
      ))),
      error = function(e) sprintf("Error: %s", e$message),
      warning = function(w) sprintf("Warning: %s", w$message)
    )
  },
  all_models[["models"]],
  all_models[["dataset"]],
  all_models[["sex"]],
  all_models[["correlation_structure"]],
  all_models[["default"]],
  future.globals = FALSE,
  future.packages = c("nlme"),
  USE.NAMES = FALSE
)

all_models[["is_good"]] <- future_mapply(
  FUN = function(lme) inherits(summary(lme), "lme"),
  all_models[["lme"]],
  future.globals = FALSE,
  future.packages = c("nlme"),
  USE.NAMES = FALSE
)

all_models[j = results := fifelse(is_good, list("Good"), lme)]

all_models[
  i = (is_good),
  j = `:=`(
    perf_check_model = lapply(lme, check_model),
    perf_model_performance = lapply(lme, model_performance)
  )
]

saveRDS(object = all_models, file = file.path(output_directory, "summary_models.rds"), compress = FALSE)


### ======================================================================================
fmt_models <- all_models[
  j = list(
    sex = c("0" = "Male", "1" = "Female")[as.character(sex)],
    dataset = c(
      "visits_raw" = "Raw",
      "visits_clean" = "Clean",
      # "visits_clean[age <= 11]" = "Clean<br><i style='font-size: 6pt'>(0 ≤ age ≤ 11)</i>",
      # "visits_clean[age <= 12]" = "Clean<br><i style='font-size: 6pt'>(0 ≤ age ≤ 12)</i>",
      # "visits_clean[age <= 13]" = "Clean<br><i style='font-size: 6pt'>(0 ≤ age ≤ 13)</i>",
      "visits_clean[between(age, 1, 18)]" = "Clean<br><i style='font-size: 6pt'>(1 ≤ age ≤ 18)</i>"
    )[dataset],
    correlation_structure,
    model = setNames(
      object = c(
        "<b style='color: #463480FF'>Linear Splines</b> - Random Linear Splines",
        "<b style='color: #463480FF'>Linear Splines</b> - Random Intercepts",
        "<b style='color: #21908CFF'>Cubic Splines</b> - Random Cubic Splines",
        "<b style='color: #21908CFF'>Cubic Splines</b> - Random Quadratic Splines",
        "<b style='color: #21908CFF'>Cubic Splines</b> - Random Linear Splines",
        "<b style='color: #21908CFF'>Cubic Splines</b> - Random Intercepts",
        "<b style='color: #9AD93CFF'>Cubic Slope</b> - Random Cubic Slopes",
        "<b style='color: #9AD93CFF'>Cubic Slope</b> - Random Quadratic Slopes",
        "<b style='color: #9AD93CFF'>Cubic Slope</b> - Random Linear Slopes",
        "<b style='color: #9AD93CFF'>Cubic Slope</b> - Random Intercepts"
      ),
      nm = sprintf("fixed = %s, random = %s", fixed_effect, random_effect)
    )[models],
    results = sub(", block [0-9]+$| = [0-9.e-]+$", "", results),
    perf = perf_model_performance
  )
][
  j = c("model", "random") := tstrsplit(model, " - ")
][
  j = `:=`(
    y = paste0(
      model,
      fifelse(correlation_structure == "NULL", "<br>", " -- C-AR1<br>"),
      "<i style='font-size: 8pt'>", random, "</i>"
    )
  )
][
  j = `:=`(
    x = factor(x = dataset, levels = unique(dataset)),
    y = factor(x = y, levels = rev(unique(y))),
    fill = factor(
      x = sub(":.*", "", results),
      levels = c("Good", "Warning", "Error")
    )
  )
]

names_before_unlist <- names(fmt_models)

png(
  filename = file.path(output_directory, "models-diagnostics.png"),
  width = 16, height = 12, units = "cm", res = 300, scaling = 0.80
)
print(
  ggplot(data = fmt_models) +
    aes(x = x, y = y, fill = fill) +
    facet_grid(cols = vars(sex)) +
    geom_tile(colour = "white") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_manual(values = c("#00b200", "#0000b2", "#b20000")) +
    labs(x = NULL, y = NULL, fill = NULL, title = "OBE") +
    theme(legend.position = "top")
)
invisible(dev.off())

fwrite(
  x = fmt_models[
    j = unlist(perf, recursive = FALSE),
    by = list(
      sex,
      dataset = gsub("<b [^>]*>|</b>|<i [^>]*>|</i>", "", x),
      model = gsub("<br>", " -- ", gsub("<b [^>]*>|</b>|<i [^>]*>|</i>", "", y)),
      status = sub("TRUE", "Good", results)
    )
  ],
  file = file.path(output_directory, "models-performance.csv")
)

gt(
  data = fmt_models[
    i = fill != "Good",
    j = list(
      Model = gsub("<i [^>]*>|</i>", "", gsub("<br>", " -- ", gsub("8pt", "0.75em", as.character(y)))),
      Dataset = gsub("<br>", " ", gsub("|<i [^>]*>|</i>", "", gsub("6pt", "0.75em", as.character(x)))),
      Result = results
    )
  ]
) %>%
  fmt(columns = everything(), fns = identity) %>%
  tab_header(title = "OBE", subtitle = "Models Diagnostics") %>%
  data_color(
    columns = "Result",
    colors = function(x) {
      c(
        "Good" = "#00b200",
        "Warning" = "#0000b2",
        "Error" = "#b20000"
      )[sub("TRUE", "Good", sub(":.*", "", x))]
    },
    apply_to = "text"
  ) %>%
  tab_options(
    table.font.size = "10pt",
    data_row.padding = "2pt"
  ) %>%
  opt_all_caps() %>%
  opt_row_striping() %>%
  gtsave(file.path(output_directory, "models-diagnostics.html"))

gt(
  data = fmt_models[
    j = unlist(perf, recursive = FALSE),
    by = list(
      Sex = sex,
      Model = gsub("<i [^>]*>|</i>", "", gsub("<br>", " -- ", gsub("8pt", "0.75em", as.character(y)))),
      Dataset = gsub("<br>", " ", gsub("|<i [^>]*>|</i>", "", gsub("6pt", "0.75em", as.character(x))))
    )
  ],
  groupname_col = c("Sex", "Dataset")
) %>%
  fmt(columns = everything(), fns = identity) %>%
  fmt_number(
    columns = c("AIC", "BIC"),
    decimals = 1,
    drop_trailing_zeros = FALSE
  ) %>%
  fmt_number(
    columns = c("R2_conditional", "R2_marginal"),
    n_sigfig = 3,
    drop_trailing_zeros = FALSE
  ) %>%
  fmt_number(
    columns = c("RMSE", "Sigma"),
    n_sigfig = 3,
    drop_trailing_zeros = FALSE
  ) %>%
  fmt_number(
    columns = "ICC",
    rows = ICC >= 0.10,
    decimals = 3,
    n_sigfig = 3,
    drop_trailing_zeros = FALSE
  ) %>%
  fmt_scientific(
    columns = "ICC",
    rows = ICC < 0.10,
    decimals = 2,
    drop_trailing_zeros = FALSE
  ) %>%
  tab_header(title = "OBE", subtitle = "Models Performance") %>%
  data_color(
    columns = c("AIC", "BIC", "R2_conditional", "R2_marginal", "ICC", "RMSE", "Sigma"),
    colors = function(x) {
      sx <- scales::rescale(x, to = c(0, 1))
      scales::col_numeric(palette = "plasma", domain = c(0, 1))(sx)
    }
  ) %>%
  tab_options(
    table.font.size = "10pt",
    data_row.padding = "2pt"
  ) %>%
  opt_all_caps() %>%
  opt_row_striping() %>%
  gtsave(file.path(output_directory, "models-performance.html"))


### Complete =======================================================================================
message(timestamp(quiet = TRUE))
message("Success!", appendLF = TRUE)
