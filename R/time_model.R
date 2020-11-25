#' time_model
#'
#' @param x something
#' @param y something
#' @param data something
#' @param method something
#' @param knots something
#' @param as_text something
#'
#' @return something
#' @export
time_model <- function(
  x,
  y,
  data,
  method = c("cubic_slope", "linear_splines", "cubic_splines"),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(5.5, 11),
    "cubic_splines" = c(2, 8, 12)
  )[[method]],
  as_text = FALSE
) {
  knots_fmt <- as.character(enquote(knots)[2])
  x_fmt <- switch(
    EXPR = method,
    "cubic_slope" = paste0("stats::poly(", x, ", degree = 3)"),
    "linear_splines" = paste0(
      "gsp(", x, ", knots = ", knots_fmt, ", degree = rep(1, ", length(knots) + 1, "), smooth = rep(0, ", length(knots), "))"
    ),
    "cubic_splines" = paste0(
      "gsp(", x, ", knots = ", knots_fmt, ", degree = rep(3, ", length(knots) + 1, "), smooth = rep(2, ", length(knots), "))"
    ),
    stop(paste0("'", method, "' not defined!"))
  )
  form_fixed <- paste0(y, " ~ ",  x_fmt)
  form_random <- paste0("~ ", x_fmt, " | ID")


  model_call <- c(
    'nlme::lme(',
    paste0('  fixed = ', form_fixed, ','),
    '  data = data,',
    paste0('  random = ', form_random, ','),
    '  na.action = stats::na.omit,',
    '  method = "ML",',
    '  correlation = nlme::corCAR1(form = ~ 1 | ID),',
    '  control = nlme::lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)',
    ')'
  )
  if (as_text) cat(model_call, sep = "\n") else eval(parse(text = paste(model_call, collapse = "")))
}
