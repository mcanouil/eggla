#!/usr/bin/env Rscript

parent_directory <- getwd()

setwd(file.path(parent_directory, "egg_analysis"))
renv::activate()

# Setup
data_path <- ""
id_variable <- "ID"
age_days_variable <- NULL
age_years_variable <- "age"
weight_kilograms_variable <- "weight"
height_centimetres_variable <- "height"
sex_variable <- "sex"
covariates <- NULL # or c("cov1", "cov2")
male_coded_zero <- FALSE
parallel <- FALSE
parallel_n_chunks <- 1

library(data.table)
library(eggla)
library(growthcleanr)
library(broom.mixed)
library(patchwork)

dt <- fread(data_path)

# Data formatting
dt_long <- melt(
  data = dt[
    j = list(
      "egg_id" = get(id_variable),
      "egg_agedays" = if (is.null(age_days_variable)) {
        floor(get(age_years_variable) * 365.25) # convert age in years to age in days and as integers ...
      } else {
        get(age_days_variable)
      },
      "WEIGHTKG" = as.numeric(get(weight_kilograms_variable)),
      "HEIGHTCM" = as.numeric(get(height_centimetres_variable)),
      "egg_sex" = if (male_coded_zero) {
        as.character(get(sex_variable))
      } else {
        c("0" = "1", "1" = "0")[as.character(get(sex_variable))] # recode sex with Male = 0 and Female = 1 ...
      }
    )
  ],
  id.vars = c(sprintf("egg_%s", c("id", "agedays", "sex")), covariates),
  measure.vars = c("WEIGHTKG", "HEIGHTCM"),
  variable.name = "param",
  value.name = "measurement"
)

dt_long[
  j = clean := cleangrowth( # Daymont's QC from 'growthcleanr'
    subjid = egg_id,
    param = param,
    agedays = egg_agedays,
    sex = egg_sex,
    measurement = measurement,
    quietly = TRUE,
    parallel = parallel,
    num.batches = parallel_n_chunks
  )
]

dt_clean <- dcast(
  data = dt_long[clean %in% "Include"], # Exclude all flags
  formula = ... ~ param,
  value.var = "measurement"
)[
  j = "bmi" := WEIGHTKG / (HEIGHTCM / 100)^2 # recompute bmi based using QCed variables
][
  !is.na(bmi) # exclude missing BMI related to measurements exclusion
]

base_model <- log(bmi) ~ egg_agedays
if (!is.null(covariates)) {
  base_model <- update(
    base_model,
    as.formula(sprintf(". ~ . + %s", paste(covariates, collapse = " + ")))
  )
}

archives <- sapply(
  X = c(0, 1),
  FUN = function(isex) {
    sex_literal <- c("0" = "male", "1" = "female")[as.character(isex)]
    results_directory <- file.path(parent_directory, sex_literal)
    dir.create(results_directory, recursive = TRUE)
    on.exit(unlink(results_directory, recursive = TRUE))
    results <- egg_model(
      formula = base_model,
      data = dt_clean[egg_sex %in% isex],
      id_var = "egg_id"
    )

    saveRDS(
      object = results,
      file = file.path(results_directory, "model-object.rds")
    )

    fwrite(
      x = tidy(results),
      file = file.path(results_directory, "model-coefficients.csv")
    )

    fwrite(
      x = egg_slopes(
        fit = res,
        period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
      ),
      file = file.path(results_directory, "derived-slopes.csv")
    )

    fwrite(
      x = egg_auc(
        fit = res,
        period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17)
      ),
      file = file.path(results_directory, "derived-auc.csv")
    )

    svglite(
      filename = file.path(results_directory, "model-residuals.svg"),
      width = 10,
      height = 8
    )
    print(
      plot_residuals(
        x = "age",
        y = "log(bmi)",
        fit = res
      ) +
        plot_annotation(
          title = sprintf(
            "Cubic Splines (Random Linear Splines) - BMI - %s",
            c("0" = "Male", "1" = "Female")[as.character(isex)]
          ),
          tag_levels = "A"
        )
    )
    invisible(dev.off())

    owd <- getwd()
    on.exit(setwd(owd), add = TRUE)
    setwd(results_directory)
    archive_filename <- file.path(
      parent_directory,
      sprintf("%s-%s.zip", Sys.Date(), sex_literal)
    )
    zip(
      zipfile = archive_filename,
      files = list.files()
    )
    archive_filename
  }
)

message("Results available at:")
message(paste(sprintf("+ '%s'", archives), collapse = "\n"))
