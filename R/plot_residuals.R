#' plot_residuals
#'
#' @param x something
#' @param y something
#' @param fit something
#' @param variables_unit something
#'
#' @return something
#' @export
#' @import ggplot2
#' @import data.table
#' @importFrom stats residuals fitted qnorm
#' @importFrom nlme ACF random.effects
#' @importFrom scales percent_format
#' @importFrom patchwork wrap_plots
plot_residuals <- function(x, y, fit, variables_unit) {
  revert_trans <- if (grepl("log", y)) exp else identity
  y <- gsub("log\\((.*)\\)", "\\1", y)

  model_data <- as.data.table(fit$data)
  model_data[,
    c("resid", "fitted") := list(
      stats::residuals(fit, level = 1, type = "p"),
      stats::fitted(fit, level = 1)
    )
  ]

  plota <- ggplot(data = model_data) +
    aes(x = revert_trans(fitted), y = .data[[y]]) +
    geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    stat_smooth(method = "lm", formula = y ~ x, linetype = 1, colour = "firebrick", se = FALSE) +
    labs(
      x = paste0("Fitted ", toupper(y), " Values (", variables_unit[[y]], ")"),
      y = paste0("Observed ", toupper(y), " Values (", variables_unit[[y]], ")")
    )

  plotb <- ggplot(data = model_data) +
    aes(x = fitted, y = resid) +
    geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    stat_smooth(method = "loess", formula = y ~ x, linetype = 1, colour = "firebrick", se = FALSE) +
    geom_hline(yintercept = 0, linetype = 2, colour = "dodgerblue") +
    labs(
      x = paste0("Fitted ", toupper(y), " Values (", variables_unit[[y]], ")"),
      y = "Marginal Residuals"
    )

  plotc <- ggplot(data = model_data) +
    aes(x = age, y = resid) +
    geom_point(size = 0.5, alpha = 0.25, shape = 1) +
    geom_hline(yintercept = 0, linetype = 2, colour = "dodgerblue") +
    labs(
      x = paste0("Fitted ", toupper(x), " Values (", variables_unit[[x]], ")"),
      y = "Marginal Residuals"
    )

  plotd <- ggplot(
    data = (function(model) {
      out <- nlme::ACF(model, resType = "normal")
      out[["stdv"]] <- stats::qnorm(1 - 0.01 / 2) / sqrt(attr(out, "n.used"))
      out
    })(fit)
  ) +
    aes(x = lag, y = ACF, ymin = -stdv, ymax = stdv) +
    geom_line() +
    geom_point() +
    geom_ribbon(alpha = 0.10, fill = "dodgerblue") +
    geom_line(y = 0, linetype = 2, colour = "dodgerblue") +
    scale_y_continuous(labels = scales::percent_format(suffix = " %")) +
    labs(x = "Lag", y = "Correlation")

  plote <- ggplot(data = data.frame(sample = revert_trans(nlme::random.effects(fit)[["(Intercept)"]]))) +
    aes(sample = sample) +
    stat_qq(size = 0.5, alpha = 0.25, shape = 1) +
    stat_qq_line(linetype = 2, colour = "dodgerblue") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  plotf <- ggplot(data = model_data) +
    aes(sample = resid) +
    stat_qq(size = 0.5, alpha = 0.25, shape = 1) +
    stat_qq_line(linetype = 2, colour = "dodgerblue") +
    labs(x = "Theoretical Quantiles", y = "Residuals Quantiles")

  patchwork::wrap_plots(
    lapply(
      X = list(plota, plotb, plotc, plotd, plote, plotf),
      FUN = `+`,
      theme(
        axis.title = element_text(size = rel(0.65)),
        axis.text = element_text(size = rel(0.50))
      )
    ),
    ncol = 3, nrow = 2
  )
}
