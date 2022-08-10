#' Perform EGG longitudinal analysis and derived areas under the curves and slopes.
#'
#' Perform Daymont's quality-control for BMI,
#' fit a cubic splines mixed model regression
#' with linear splines as random effect,
#' save model object, generates residuals figures fot model validity,
#' derived area under the curve and slopes for male and femal.  
#' This function is a wrapper around `egg_model()`, `egg_slopes()` and `egg_aucs()`.
#'
#' @param data Phenotypes data that inherits from `data.frame` class.
#' @param id_variable Name of the column where sample/individual IDs are stored.
#' @param age_days_variable Name of the column where age in days is stored.
#' @param age_years_variable Name of the column where age in years is stored.
#' @param weight_kilograms_variable Name of the column where weight in kilograms is stored.
#' @param height_centimetres_variable Name of the column where height in centimetres is stored.
#' @param sex_variable Name of the column where sex is stored.
#' @param covariates A vector of columns' names to be used as covariates.
#' @param male_coded_zero Is male coded "0" (and female coded "1")?
#' @param random_complexity A numeric (1-3) indicating the complexity of the random effect term.
#'  Default, `"auto"` will try from the more complex to the less complex if no success.
#' @param use_car1 A logical indicating whether to use continuous auto-correlation,
#'   i.e., CAR(1) as correlation structure.
#' @param parallel Determines if `growthcleanr::cleangrowth()` function shoud be run in parallel. Defaults to `FALSE`.
#' @param parallel_n_chunks Specify the number of batches (in `growthcleanr::cleangrowth()`) to run in parallel.
#'   Only applies if parallel is set to TRUE.
#' Defaults to the number of workers returned by the getDoParWorkers function in the foreach package.
#' @param working_directory Directory in which computation will occur and where output files will be saved.
#' @param quiet A logical indicating whether to suppress the output.
#'
#' @return Path to zip archives.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   data("bmigrowth")
#'   fwrite(
#'     x = bmigrowth,
#'     file = file.path(tempdir(), "bmigrowth.csv")
#'   )
#'   res <- run_eggla_lmm(
#'     data = fread(file.path(tempdir(), "bmigrowth.csv")),
#'     id_variable = "ID",
#'     age_days_variable = NULL,
#'     age_years_variable = "age",
#'     weight_kilograms_variable = "weight",
#'     height_centimetres_variable = "height",
#'     sex_variable = "sex",
#'     covariates = NULL,
#'     male_coded_zero = FALSE,
#'     random_complexity = 1,
#'     use_car1 = FALSE,
#'     parallel = FALSE,
#'     parallel_n_chunks = 1,
#'     working_directory = tempdir()
#'   )
#' }
run_eggla_lmm <- function(
  data,
  id_variable,
  age_days_variable,
  age_years_variable,
  weight_kilograms_variable,
  height_centimetres_variable,
  sex_variable,
  covariates,
  male_coded_zero = FALSE,
  random_complexity = "auto",
  use_car1 = FALSE,
  parallel = FALSE,
  parallel_n_chunks = 1,
  working_directory = getwd(),
  quiet = FALSE
) {
  HEIGHTCM <- WEIGHTKG <- bmi <- clean <- NULL # no visible binding for global variable from data.table
  egg_agedays <- egg_id <- egg_sex <- NULL # no visible binding for global variable from data.table
  measurement <- param <- egg_ageyears <- NULL # no visible binding for global variable from data.table

  working_directory <- normalizePath(working_directory)

  period <- c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)
  knots <- c(2, 8, 12)

  data <- data.table::setnames(
    x = data.table::as.data.table(data),
    old = c(
      id_variable, age_years_variable, sex_variable,
      weight_kilograms_variable, height_centimetres_variable
    ),
    new = c("egg_id", "egg_ageyears", "egg_sex", "WEIGHTKG", "HEIGHTCM")
  )
  if (is.null(age_days_variable)) {
    data[
      j = egg_agedays := floor(egg_ageyears * 365.25)
    ]
  } else {
    data.table::setnames(
      x = data,
      old = age_days_variable,
      new = "egg_agedays"
    )
  }
  if (male_coded_zero) {
    data[
      j = egg_sex := as.integer(egg_sex)
    ]
  } else {
    # recode sex with Male = 0 and Female = 1 ...
    data[
      j = egg_sex := c("0" = 1L, "1" = 0L)[as.character(gegg_sex)]
    ]
  }

  required_id_variables <- c(
    sprintf("egg_%s", c("id", "ageyears", "agedays", "sex")),
    intersect(covariates, names(data))
  )

  dt_long <- data.table::melt(
    data = data.table::as.data.table(data)[
      j = `:=`(
        "egg_id" = as.character(egg_id),
        "egg_ageyears" = egg_ageyears,
        "egg_agedays" = egg_ageyears,
        "WEIGHTKG" = as.numeric(WEIGHTKG),
        "HEIGHTCM" = as.numeric(HEIGHTCM),
        "egg_sex" = as.integer(iegg_sex)
      )
    ][
      j = .SD,
      .SDcols = c(required_id_variables, "WEIGHTKG", "HEIGHTCM")
    ],
    id.vars = required_id_variables,
    measure.vars = c("WEIGHTKG", "HEIGHTCM"),
    variable.name = "param",
    value.name = "measurement",
    variable.factor = FALSE
  )

  dt_long[
    j = `:=`(
      "clean" = growthcleanr::cleangrowth(
        subjid = egg_id,
        param = param,
        agedays = egg_agedays,
        sex = egg_sex,
        measurement = measurement,
        quietly = quiet,
        parallel = parallel,
        num.batches = parallel_n_chunks
      )
    )
  ]

  dt_clean <- data.table::dcast(
    data = dt_long[clean %in% "Include"], # Exclude all flags
    formula = ... ~ param,
    value.var = "measurement"
  )[
    j = `:=`("bmi" = WEIGHTKG / (HEIGHTCM / 100)^2)
  ][
    !is.na(bmi) # exclude missing BMI related to measurements exclusion
  ]

  y_variable <- "log(bmi)"
  x_variable <- "egg_ageyears"
  base_model <- stats::as.formula(sprintf("%s ~ %s", y_variable, x_variable))
  if (!is.null(covariates)) {
    base_model <- stats::update(
      base_model,
      stats::as.formula(
        sprintf(". ~ . + %s", paste(covariates, collapse = " + "))
      )
    )
  }

  archives <- sapply(
    X = c(0, 1),
    FUN = function(isex) {
      sex_literal <- c("0" = "male", "1" = "female")[as.character(isex)]
      results_directory <- file.path(working_directory, sex_literal)
      dir.create(results_directory, recursive = TRUE)
      on.exit(unlink(results_directory, recursive = TRUE))
      results <- egg_model(
        formula = base_model,
        data = dt_clean[egg_sex %in% isex],
        id_var = "egg_id",
        random_complexity = random_complexity,
        use_car1 = use_car1,
        knots = knots,
        quiet = quiet
      )

      saveRDS(
        object = results,
        file = file.path(
          working_directory,
          sprintf("%s-%s-model-object.rds", Sys.Date(), sex_literal)
        )
      )

      writeLines(
        text = deparse1(results$call),
        con = file.path(results_directory, "model-call.txt")
      )

      data.table::fwrite(
        x = broom.mixed::tidy(results),
        file = file.path(results_directory, "model-coefficients.csv")
      )

      grDevices::png(
        filename = file.path(results_directory, "model-residuals.png"),
        width = 4 * 2.5,
        height = 3 * 2.5,
        units = "in",
        res = 120
      )
      print(
        plot_residuals(
          x = x_variable,
          y = y_variable,
          fit = results
        ) +
          patchwork::plot_annotation(
            title = sprintf(
              "Cubic Splines (Random Linear Splines) - BMI - %s",
              c("0" = "Male", "1" = "Female")[as.character(isex)]
            ),
            tag_levels = "A"
          )
      )
      invisible(grDevices::dev.off())

      data.table::fwrite(
        x = egg_slopes(
          fit = results,
          period = period,
          knots = knots
        ),
        file = file.path(results_directory, "derived-slopes.csv")
      )

      data.table::fwrite(
        x = egg_aucs(
          fit = results,
          period = period,
          knots = knots
        ),
        file = file.path(results_directory, "derived-aucs.csv")
      )

      data.table::fwrite(
        x = egg_outliers(
          fit = results,
          period = period,
          knots = knots
        ),
        file = file.path(results_directory, "derived-outliers.csv")
      )

      eggc <- egg_correlations(
        fit = results,
        period = period,
        knots = knots
      )

      data.table::fwrite(
        x = eggc[["AUC"]],
        file = file.path(results_directory, "derived-aucs-correlations.csv")
      )
      data.table::fwrite(
        x = eggc[["SLOPE"]],
        file = file.path(results_directory, "derived-slopes-correlations.csv")
      )

      owd <- getwd()
      on.exit(setwd(owd), add = TRUE)
      setwd(results_directory)
      archive_filename <- file.path(
        working_directory,
        sprintf("%s.zip", sex_literal)
      )
      utils::zip(
        zipfile = archive_filename,
        files = list.files()
      )
      archive_filename
    }
  )

  if (!quiet) {
    message("Results available at:")
    message(paste(sprintf("+ '%s'", archives), collapse = "\n"))
  }
  archives
}
