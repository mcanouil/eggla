# Cubic slope

    Code
      y1 <- compute_aucs(fit = x, method = "cubic_slope", period = c(0, 0.5, 1.5, 3.5,
        6.5, 10, 12, 17))

# Linear Splines

    Code
      y1 <- compute_aucs(fit = x, method = "linear_splines", period = c(0, 0.5, 1.5,
        3.5, 6.5, 10, 12, 17))

# Cubic Splines

    Code
      y1 <- compute_aucs(fit = x, method = "cubic_splines", period = c(0, 0.5, 1.5,
        3.5, 6.5, 10, 12, 17), knots = c(2, 8, 12))

