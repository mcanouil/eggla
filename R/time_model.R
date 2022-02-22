#' time_model
#'
#' @param x A length one character vector with the main covariate name (_i.e._, right-hand side).
#' @param y A length one character vector with the variable name to be explained (_i.e._, left-hand side).
#' @param cov A vector of addtional/optional covariates names to included in the fixed effect part of the linear mixed-effects models.
#' @param data A data.frame containing the variables named in `x` and `y`.
#' @param method The type of model, _i.e._, one of `"cubic_slope"`, `"linear_splines"` or `"cubic_splines"`.
#' @param knots The knots defining the splines for `"linear_splines"` ànd `"cubic_splines"` methods.
#'
#' @return An object of class "lme" representing the linear mixed-effects model fit.
#' @export
time_model <- function(
  x,
  y,
  cov = NULL,
  data,
  method = c("cubic_slope", "linear_splines", "cubic_splines"),
  knots = list(
    "cubic_slope" = NULL,
    "linear_splines" = c(5.5, 11),
    "cubic_splines" = c(2, 8, 12)
  )[[method]]
) {
  knots_fmt <- as.character(enquote(knots)[2])
  x_fmt <- switch(
    EXPR = method,
    "cubic_slope" = paste0("stats::poly(", x, ", degree = 3)"),
    "linear_splines" = paste0(
      "gsp(", x, ", knots = ", knots_fmt,
      ", degree = rep(1, ", length(knots) + 1,
      "), smooth = rep(0, ", length(knots), "))"
    ),
    "cubic_splines" = paste0(
      "gsp(", x, ", knots = ", knots_fmt,
      ", degree = rep(3, ", length(knots) + 1,
      "), smooth = rep(2, ", length(knots), "))"
    ),
    stop(paste0("'", method, "' not defined!"))
  )
  form_fixed <- paste0(y, " ~ ",  x_fmt)
  form_random <- paste0("~ ", x_fmt, " | ID")

  if (!is.null(cov)) {
    form_fixed <- paste(form_fixed, "+", paste(cov, collapse = " + "))
  }

  vars_available <- all.vars(stats::as.formula(form_fixed)) %in% colnames(data)

  if (!all(vars_available)) {
    stop(paste0(
      c(
        "Some variables are not available in the dataset provided:",
        paste("  * ", all.vars(stats::as.formula(form_fixed))[!vars_available])
      ),
      collapse = "\n"
    ))
  }

  f_model_call <- function(form_fixed, form_random, n_iteration) {
    c(
      "nlme::lme(",
      paste0("  fixed = ", form_fixed, ","),
      "  data = data,",
      paste0("  random = ", form_random, ","),
      "  na.action = stats::na.omit,",
      "  method = \"ML\",",
      "  correlation = nlme::corCAR1(form = ~ 1 | ID),",
      paste0(
        "  control = nlme::lmeControl(opt = \"optim\", maxIter = ",
        n_iteration, ", msMaxIter = ", n_iteration, ")"
      ),
      ")"
    )
  }

  model_call <- f_model_call(
    form_fixed = form_fixed,
    form_random = form_random,
    n_iteration = 500
  )
  res_model <- try(eval(parse(text = paste(model_call, collapse = ""))), silent = TRUE)
  if (inherits(res_model, "try-error")) {
    message("Number of iteration has been increased from 500 to 1,000.", appendLF = TRUE)
    model_call <- f_model_call(
      form_fixed = form_fixed,
      form_random = form_random,
      n_iteration = 1000
    )
    res_model <- try(eval(parse(text = paste(model_call, collapse = ""))), silent = TRUE)
  }
  if (inherits(res_model, "try-error")) {
    model_call <- switch(EXPR = method,
      "cubic_slope" = {
        message("Polynom's degree was decreased from 3 to 1 in the random effect formula.", appendLF = TRUE)
        f_model_call(
          form_fixed = form_fixed,
          form_random = paste0("~ ", gsub("degree = 3", "degree = 1", x_fmt, fixed = TRUE), " | ID"),
          n_iteration = 1000
        )
      },
      "cubic_splines" = {
        message("Spline's degree was decreased from 3 to 1 in the random effect formula.", appendLF = TRUE)
        f_model_call(
          form_fixed = form_fixed,
          form_random = paste0("~ ", gsub("degree = rep(3", "degree = rep(1", x_fmt, fixed = TRUE), " | ID"),
          n_iteration = 1000
        )
      }
    )
    res_model <- try(eval(parse(text = paste(model_call, collapse = ""))), silent = TRUE)
  }

  if (inherits(res_model, "try-error")) {
    message('The random effect formula has been set to "~ 1 | ID".', appendLF = TRUE)
    model_call <- f_model_call(
      form_fixed = form_fixed,
      form_random = "~ 1 | ID",
      n_iteration = 1000
    )
    res_model <- try(eval(parse(text = paste(model_call, collapse = ""))), silent = TRUE)
  }

  message(paste(model_call, collapse = "\n"), appendLF = TRUE)

  if (inherits(res_model, "try-error")) {
    stop(gsub("Error in try\\([^:]*\\) : ", paste0('"', method, '": '), res_model))
  } else {
    res_model
  }
}
