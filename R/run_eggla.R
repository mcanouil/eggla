#' Perform EGG longitudinal analysis and derived areas under the curves and slopes.
#'
#' Perform Daymont's quality-control for BMI,
#' fit a cubic splines mixed model regression
#' with linear splines as random effect,
#' save model object, generates residuals figures fot model validity,
#' derived area under the curve and slopes for male and femal.  
#' This function is a wrapper around `egg_model`, `egg_slopes` and `egg_auc`.
#'
#' @param data something
#' @param id_variable something
#' @param age_days_variable something
#' @param age_years_variable something
#' @param weight_kilograms_variable something
#' @param height_centimetres_variable something
#' @param sex_variable something
#' @param covariates something
#' @param male_coded_zero something
#' @param parallel something
#' @param parallel_n_chunks something
#' @param working_directory something
#'
#' @return Path to zip archives.
#' @import data.table
#'
#' @export
run_eggla <- function(
  data,
  id_variable,
  age_days_variable,
  age_years_variable,
  weight_kilograms_variable,
  height_centimetres_variable,
  sex_variable,
  covariates,
  male_coded_zero = FALSE,
  parallel = FALSE,
  parallel_n_chunks = 1,
  working_directory = getwd()
) {
  HEIGHTCM <- WEIGHTKG <- bmi <- clean <- NULL
  egg_agedays <- egg_id <- egg_sex <- NULL
  measurement <- param <- NULL

  dt_long <- data.table::melt(
    data = data.table::as.data.table(data)[
      j = list(
        "egg_id" = as.character(get(id_variable)),
        "egg_ageyears" = get(age_years_variable),
        "egg_agedays" = if (is.null(age_days_variable)) {
          # convert age in years to age in days and as integers ...
          floor(get(age_years_variable) * 365.25)
        } else {
          get(age_days_variable)
        },
        "WEIGHTKG" = as.numeric(get(weight_kilograms_variable)),
        "HEIGHTCM" = as.numeric(get(height_centimetres_variable)),
        "egg_sex" = if (male_coded_zero) {
          as.integer(get(sex_variable))
        } else {
          # recode sex with Male = 0 and Female = 1 ...
          c("0" = 1L, "1" = 0L)[as.character(get(sex_variable))]
        }
      )
    ],
    id.vars = c(
      sprintf("egg_%s", c("id", "ageyears", "agedays", "sex")),
      covariates
    ),
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
        quietly = TRUE,
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
        id_var = "egg_id"
      )

      saveRDS(
        object = results,
        file = file.path(results_directory, "model-object.rds")
      )

      data.table::fwrite(
        x = broom.mixed::tidy(results),
        file = file.path(results_directory, "model-coefficients.csv")
      )

      data.table::fwrite(
        x = egg_slopes(
          fit = results,
          period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
          id_var = "egg_id"
        ),
        file = file.path(results_directory, "derived-slopes.csv")
      )

      data.table::fwrite(
        x = egg_auc(
          fit = results,
          period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
          id_var = "egg_id"
        ),
        file = file.path(results_directory, "derived-auc.csv")
      )

      grDevices::png(
        filename = file.path(results_directory, "model-residuals.png"),
        width = 600,
        height = 480,
        res = 72
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

      owd <- getwd()
      on.exit(setwd(owd), add = TRUE)
      setwd(results_directory)
      archive_filename <- file.path(
        working_directory,
        sprintf("%s-%s.zip", Sys.Date(), sex_literal)
      )
      utils::zip(
        zipfile = archive_filename,
        files = list.files()
      )
      archive_filename
    }
  )

  message("Results available at:")
  message(paste(sprintf("+ '%s'", archives), collapse = "\n"))
  archives
}
