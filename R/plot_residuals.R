#' Plot several residuals plots for diagnostics.
#'
#' @param x A length one character vector with the main covariate name
#'   (_i.e._, right-hand side), as defined in `fit`.
#' @param y A length one character vector with the variable name to be explained
#'   (_i.e._, left-hand side), as defined in `fit`.
#' @param fit A model object from a statistical model
#'   such as from a call `time_model()` or `egg_model()`.
#'
#' @return A `patchwork` `ggplot2` object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(patchwork)
#' library(eggla)
#' data("bmigrowth")
#' res <- egg_model(
#'   formula = log(bmi) ~ age,
#'   data = bmigrowth[bmigrowth[["sex"]] == 0, ],
#'   id_var = "ID",
#'   random_complexity = 1
#' )
#' plot_residuals(
#'   x = "age",
#'   y = "log(bmi)",
#'   fit = res
#' ) +
#'   plot_annotation(
#'     title = "Cubic Splines (Random Linear Splines) - BMI - Female",
#'     tag_levels = "A"
#'   )
plot_residuals <- function(x, y, fit) {
  stopifnot(inherits(fit, "lme"))
  .data <- ggplot2::.data

  okabe_ito_palette <- c(
    "#E69F00FF", "#56B4E9FF", "#009E73FF", "#F0E442FF", "#0072B2FF",
    "#D55E00FF", "#CC79A7FF", "#999999FF"
  )

  revert_trans <- if (grepl("log", y)) exp else identity
  y <- sub("log\\((.*)\\)", "\\1", y)

  model_data <- as.data.frame(fit$data)
  model_data[["resid"]] <- stats::residuals(fit, level = 1, type = "p")
  model_data[["fitted"]] <- stats::fitted(fit, level = 1)

  plota <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(x = revert_trans(.data[["fitted"]]), y = .data[[y]]) +
    ggplot2::geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x, linetype = 1, colour = okabe_ito_palette[6], se = FALSE) +
    ggplot2::labs(
      x = paste0("Fitted ", toupper(y), " Values"),
      y = paste0("Observed ", toupper(y), " Values")
    )

  plotb <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(x = .data[["fitted"]], y = .data[["resid"]]) +
    ggplot2::geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_smooth(method = "loess", formula = y ~ x, linetype = 1, colour = okabe_ito_palette[6], se = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = okabe_ito_palette[2]) +
    ggplot2::labs(
      x = paste0("Fitted ", toupper(y), " Values"),
      y = "Marginal Residuals"
    )

  plotc <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(x = .data[[x]], y = .data[["resid"]]) +
    ggplot2::geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_smooth(method = "loess", formula = y ~ x, linetype = 1, colour = okabe_ito_palette[6], se = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = okabe_ito_palette[2]) +
    ggplot2::labs(
      x = paste0("Fitted ", toupper(x), " Values"),
      y = "Marginal Residuals"
    )

  plotd <- ggplot2::ggplot(
    data = (function(model) {
      out <- nlme::ACF(model, resType = "normal")
      out[["stdv"]] <- stats::qnorm(1 - 0.01 / 2) / sqrt(attr(out, "n.used"))
      out
    })(fit)
  ) +
    ggplot2::aes(x = .data[["lag"]], y = .data[["ACF"]], ymin = -.data[["stdv"]], ymax = .data[["stdv"]]) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(alpha = 0.10, fill = okabe_ito_palette[2]) +
    ggplot2::geom_line(y = 0, linetype = 2, colour = okabe_ito_palette[2]) +
    ggplot2::scale_y_continuous(labels = function(x) paste(format(x * 100, nsmall = 2, digits = 2), "%")) +
    ggplot2::labs(x = "Lag", y = "Correlation")

  plote <- ggplot2::ggplot(data = data.frame(sample = revert_trans(nlme::random.effects(fit)[["(Intercept)"]]))) +
    ggplot2::aes(sample = .data[["sample"]]) +
    ggplot2::stat_qq(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_qq_line(linetype = 2, colour = okabe_ito_palette[2]) +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  plotf <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(sample = .data[["resid"]]) +
    ggplot2::stat_qq(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_qq_line(linetype = 2, colour = okabe_ito_palette[2]) +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Residuals Quantiles")

  patchwork::wrap_plots(
    lapply(
      X = list(plota, plotb, plotc, plotd, plote, plotf),
      FUN = `+`,
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = ggplot2::rel(0.65)),
        axis.text = ggplot2::element_text(size = ggplot2::rel(0.50))
      )
    ),
    ncol = 3, nrow = 2
  )
}
