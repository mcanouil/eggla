#' plot_residuals
#'
#' @param x something
#' @param y something
#' @param fit something
#' @param variables_unit something
#'
#' @return something
#' @export
plot_residuals <- function(x, y, fit, variables_unit) {
  .data <- ggplot2::.data

  revert_trans <- if (grepl("log", y)) exp else identity
  y <- gsub("log\\((.*)\\)", "\\1", y)

  model_data <- as.data.frame(fit$data)
  model_data[["resid"]] <- stats::residuals(fit, level = 1, type = "p")
  model_data[["fitted"]] <- stats::fitted(fit, level = 1)

  plota <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(x = revert_trans(.data[["fitted"]]), y = .data[[y]]) +
    ggplot2::geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x, linetype = 1, colour = "firebrick", se = FALSE) +
    ggplot2::labs(
      x = paste0("Fitted ", toupper(y), " Values (", variables_unit[[y]], ")"),
      y = paste0("Observed ", toupper(y), " Values (", variables_unit[[y]], ")")
    )

  plotb <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(x = .data[["fitted"]], y = .data[["resid"]]) +
    ggplot2::geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_smooth(method = "loess", formula = y ~ x, linetype = 1, colour = "firebrick", se = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "dodgerblue") +
    ggplot2::labs(
      x = paste0("Fitted ", toupper(y), " Values (", variables_unit[[y]], ")"),
      y = "Marginal Residuals"
    )

  plotc <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(x = .data[["age"]], y = .data[["resid"]]) +
    ggplot2::geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_smooth(method = "loess", formula = y ~ x, linetype = 1, colour = "firebrick", se = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "dodgerblue") +
    ggplot2::labs(
      x = paste0("Fitted ", toupper(x), " Values (", variables_unit[[x]], ")"),
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
    ggplot2::geom_ribbon(alpha = 0.10, fill = "dodgerblue") +
    ggplot2::geom_line(y = 0, linetype = 2, colour = "dodgerblue") +
    ggplot2::scale_y_continuous(labels = function(x) paste(format(x * 100, nsmall = 2, digits = 2), "%")) +
    ggplot2::labs(x = "Lag", y = "Correlation")

  plote <- ggplot2::ggplot(data = data.frame(sample = revert_trans(nlme::random.effects(fit)[["(Intercept)"]]))) +
    ggplot2::aes(sample = .data[["sample"]]) +
    ggplot2::stat_qq(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_qq_line(linetype = 2, colour = "dodgerblue") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  plotf <- ggplot2::ggplot(data = model_data) +
    ggplot2::aes(sample = .data[["resid"]]) +
    ggplot2::stat_qq(size = 0.5, alpha = 0.25, shape = 1) +
    ggplot2::stat_qq_line(linetype = 2, colour = "dodgerblue") +
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
