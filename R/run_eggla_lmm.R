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
#'   `NULL` if age in days is not available.
#' @param age_years_variable Name of the column where age in years is stored.
#'   `NULL` if age in years is not available.
#' @param weight_kilograms_variable Name of the column where weight in kilograms is stored.
#' @param height_centimetres_variable Name of the column where height in centimetres is stored.
#' @param sex_variable Name of the column where sex is stored.
#' @param covariates A vector of columns' names to be used as covariates.
#'   `NULL` if there are no covariates to add.
#' @param male_coded_zero Is male coded "0" (and female coded "1")?
#' @param random_complexity A numeric (1-3) indicating the complexity of the random effect term.
#'  Default, `"auto"` will try from the more complex to the less complex if no success.
#' @param use_car1 A logical indicating whether to use continuous auto-correlation,
#'   i.e., CAR(1) as correlation structure.
#' @param knots The knots defining the splines.
#' @param period The intervals knots on which slopes are to be computed.
#' @param filter_apar A string following `data.table` syntax for filtering on `"i"`
#'   (_i.e._, row elements), _e.g._, `filter = "source == 'A'"`.
#'   Argument pass through `compute_apar()` (see `predict_bmi()`).
#'   Default is `NULL`.
#' @param parallel Determines if `growthcleanr::cleangrowth()` function shoud be run in parallel. Defaults to `FALSE`.
#' @param parallel_n_chunks Specify the number of batches (in `growthcleanr::cleangrowth()`) to run in parallel.
#'   Only applies if parallel is set to TRUE.
#' Defaults to the number of workers returned by the getDoParWorkers function in the foreach package.
#' @param working_directory Directory in which computation will occur and where output files will be saved.
#' @param quiet A logical indicating whether to suppress the output.
#' @param clean A logical indicating whether to clean `working_directory` once the archives are created.
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
#'     random_complexity = 1,
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
  knots = c(1, 8, 12),
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  filter_apar = NULL,
  parallel = FALSE,
  parallel_n_chunks = 1,
  working_directory = getwd(),
  quiet = FALSE,
  clean = TRUE
) {
  HEIGHTCM <- WEIGHTKG <- bmi <- NULL # no visible binding for global variable from data.table
  egg_agedays <- egg_id <- egg_sex <- NULL # no visible binding for global variable from data.table
  measurement <- param <- egg_ageyears <- NULL # no visible binding for global variable from data.table
  AP <- AR <- what <- NULL # no visible binding for global variable from data.table
  Outlier <- outlier_colour <- Outlier_Zscore <- Outlier_IQR <- NULL # no visible binding for global variable from data.table

  working_directory <- normalizePath(working_directory)

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
      j = egg_sex := c("0" = 1L, "1" = 0L)[as.character(egg_sex)]
    ]
  }

  required_id_variables <- c(
    sprintf("egg_%s", c("id", "ageyears", "agedays", "sex")),
    intersect(covariates, names(data))
  )

  if (sum(
    data[
      j = list(id_not_unique = anyDuplicated(egg_agedays)),
      by = c("egg_id", "egg_sex")
    ][["id_not_unique"]]
  ) > 0) {
    stop(sprintf(
      paste(
        "It appears IDs provided by column '%s' are not unique.",
        "'id_variable' must be a column providing unique IDs!"
      ),
      id_variable
    ))
  }

  dt_long <- data.table::melt(
    data = data.table::as.data.table(data)[
      j = `:=`(
        "egg_id" = as.character(egg_id),
        "egg_ageyears" = egg_ageyears,
        "egg_agedays" = egg_agedays,
        "WEIGHTKG" = as.numeric(WEIGHTKG),
        "HEIGHTCM" = as.numeric(HEIGHTCM),
        "egg_sex" = as.integer(egg_sex)
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
      archive_filename <- sprintf("%s.zip", results_directory)
      try(unlink(results_directory, recursive = TRUE), silent = TRUE)
      dir.create(results_directory, recursive = TRUE)

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
          sprintf("%s-model-object.rds", sex_literal)
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

      slopes_dt <- egg_slopes(
        fit = results,
        period = period,
        knots = knots
      )
      data.table::fwrite(
        x = slopes_dt,
        file = file.path(results_directory, "derived-slopes.csv")
      )

      aucs_dt <- egg_aucs(
        fit = results,
        period = period,
        knots = knots
      )
      data.table::fwrite(
        x = aucs_dt,
        file = file.path(results_directory, "derived-aucs.csv")
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

      apar_dt <- data.table::setnames(
        x = data.table::dcast(
          data = compute_apar(
            fit = results,
            from = "predicted",
            start = 0.25,
            end = 10,
            step = 0.05,
            filter = filter_apar
          )[
            AP | AR
          ][
            j = what := data.table::fifelse(
              test = paste(AP, AR) %in% paste(FALSE, TRUE),
              yes = "AR",
              no = "AP"
            )
          ],
          formula = egg_id ~ what,
          value.var = c("egg_ageyears", "egg_bmi")
        ),
        old = function(x) {
          out <- sapply(strsplit(sub("^egg_", "", x), "_"), function(.x) {
            paste(rev(.x), collapse = "_")
          })
          out[grepl("^egg_id$", x)] <- "egg_id"
          out
        }
      )
      data.table::fwrite(
        x = apar_dt,
        file = file.path(results_directory, "derived-apar.csv")
      )

      outliers_dt <- egg_outliers(
        fit = results,
        period = period,
        knots = knots
      )
      data.table::fwrite(
        x = outliers_dt,
        file = file.path(results_directory, "derived-outliers.csv")
      )

      dt <- data.table::merge.data.table(
        x = data.table::rbindlist(
          l = lapply(
            X = list(
              apar_dt,
              aucs_dt,
              slopes_dt
            ),
            FUN = function(dt) {
              out <- data.table::setDT(dt)
              out <- out[
                j = .SD,
                .SDcols = c(grep(
                  pattern = paste(
                    c("^egg_id$", "slope_.*", "auc_.*", "^AP_.*", "^AR_.*"),
                    collapse = "|"
                  ),
                  x = colnames(dt),
                  value = TRUE
                ))
              ]
              data.table::melt(data = out, id.vars = "egg_id", variable.name = "parameter")
            }
          ),
          use.names = TRUE
        ),
        y = outliers_dt,
        by.x = c("parameter", "egg_id"),
        by.y = c("parameter", id_variable),
        all.x = TRUE
      )
      palette_okabe_ito <- c(
        "#e69f00", "#56B4E9", "#009E73", "#F0E442",
        "#0072B2", "#D55E00", "#CC79A7", "#999999"
      )
      dt <- dt[
        j = `:=`(
          Outlier_Zscore = data.table::fifelse(is.na(Outlier_Zscore), 0, Outlier_Zscore),
          Outlier_IQR = data.table::fifelse(is.na(Outlier_IQR), 0, Outlier_IQR)
        )
      ][
        j = outlier_colour := mapply(
          FUN = function(iqr, zs, pc) {
            if (iqr == 1 && zs == 1) {
              return(sprintf("<b style = 'color:%s;'>IQR & Z-score</b>", pc[1]))
            }
            if (iqr == 0 && zs == 1) {
              return(sprintf("<b style = 'color:%s;'>Z-score</b>", pc[2]))
            }
            if (iqr == 1 && zs == 0) {
              return(sprintf("<b style = 'color:%s;'>IQR</b>", pc[3]))
            }
            if (iqr == 0 && zs == 0) {
              return(NA_character_)
            }
          },
          iqr = Outlier_Zscore,
          zs = Outlier_IQR,
          MoreArgs = list(pc = palette_okabe_ito)
        )
      ][
        i = order(Outlier, Outlier_IQR)
      ][
        j = outlier_colour := factor(outlier_colour, levels = unique(outlier_colour))
      ]

      if (nzchar(system.file(package = "ggdist")) & nzchar(system.file(package = "ggbeeswarm"))) {
        gpl <- list(
          ggdist::stat_halfeye(
            mapping = ggplot2::aes(group = .data[["parameter"]]),
            justification = -0.20,
            .width = 0,
            scale = 1,
            na.rm = TRUE
          ),
          ggbeeswarm::geom_quasirandom(
            data = function(dt) dt[Outlier != 0],
            mapping = ggplot2::aes(group = .data[["parameter"]], colour = .data[["outlier_colour"]]),
            shape = 21,
            groupOnX = TRUE,
            width = 0.15,
            na.rm = TRUE
          )
        )
      } else {
        gpl <- list(
          ggplot2::geom_point(
            data = function(dt) dt[Outlier != 0],
            mapping = ggplot2::aes(group = .data[["parameter"]], colour = .data[["outlier_colour"]]),
            position = ggplot2::position_jitter(width = 0.15),
            shape = 21,
            na.rm = TRUE
          )
        )
      }
      grDevices::png(
        filename = file.path(results_directory, "derived-outliers.png"),
        width = 4 * 2.5,
        height = 3 * 2.5,
        units = "in",
        res = 120
      )
      print(
        ggplot2::ggplot(data = dt) +
          ggplot2::aes(x = .data[["parameter"]], y = .data[["value"]]) +
          ggplot2::geom_boxplot(
            mapping = ggplot2::aes(group = .data[["parameter"]]),
            width = 0.25,
            outlier.colour = NA,
            na.rm = TRUE
          ) +
          ggplot2::labs(
            x = "Parameter",
            y = "Values",
            colour = "Outlier Metrics"
          ) +
          ggplot2::facet_wrap(
            facets = ggplot2::vars(.data[["parameter"]]),
            scales = "free",
            ncol = 4
          ) +
          ggplot2::scale_colour_manual(values = palette_okabe_ito[c(3, 2, 1)]) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            legend.text = ggtext::element_markdown()
          ) +
          gpl
      )
      invisible(grDevices::dev.off())

      archive_tosend <- file.path(working_directory, "to-send")
      dir.create(archive_tosend, recursive = TRUE, showWarnings = FALSE)
      archive_tosend_zip <- sprintf(
        "%s/%s_to-send.zip",
        archive_tosend, sex_literal
      )
      utils::zip(
        zipfile = archive_tosend_zip,
        files = c(
          file.path(results_directory, "model-call.txt"),
          file.path(results_directory, "model-coefficients.csv"),
          file.path(results_directory, "model-residuals.png"),
          file.path(results_directory, "derived-aucs-correlations.csv"),
          file.path(results_directory, "derived-slopes-correlations.csv"),
          file.path(results_directory, "derived-outliers.png")
        ),
        flags = "-r9Xj"
      )
      if (!quiet) {
        message(sprintf(
          "Diagnostics to send available at: '%s'",
          archive_tosend_zip
        ))
      }

      utils::zip(
        zipfile = archive_filename,
        files = list.files(results_directory, full.names = TRUE),
        flags = "-r9Xj"
      )
      if (clean & file.exists(archive_filename)) {
        unlink(results_directory, recursive = TRUE)
      }
      archive_filename
    }
  )

  if (!all(file.exists(archives))) {
    archives <- sub("\\.zip$", "", archives)
  }

  if (!quiet) {
    message(sprintf(
      "Results%savailable at:",
      if (any(grepl("\\.zip$", archives))) " (zip archives) " else " "
    ))
    message(paste(sprintf("+ '%s'", archives), collapse = "\n"))
  }

  archives
}
