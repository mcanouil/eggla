#' egg_model
#'
#' @param x A length one character vector with the main covariate name (_i.e._, right-hand side).
#' @param y A length one character vector with the variable name to be explained (_i.e._, left-hand side).
#' @param cov A vector of addtional/optional covariates names to included in the fixed effect part of the linear mixed-effects models.
#' @param data A data.frame containing the variables named in `x` and `y`.
#'
#' @return An object of class "lme" representing the linear mixed-effects model fit.
#' @export
egg_model <- function(formula, data) {
  y <- as.character(formula)[[2]]
  x_cov <- strsplit(as.character(formula)[[3]], " \\+ ")[[1]]

  knots <- c(2, 8, 12)
  knots_fmt <- as.character(enquote(knots)[2])
  x_fmt <- sub(
    x_cov[1],
    sprintf(
      "gsp(%s, knots = %s, degree = rep(3, %s), smooth = rep(2, %s))",
      x_cov[1], knots_fmt, length(knots) + 1, length(knots)
    ),
    x_cov
  )

  form_fixed <- sprintf(
    "%s ~ %s",
    y, paste(x_fmt, collapse = " + ")
  )
  form_random <- sprintf(
    "~ %s | ID",
    sub("degree = rep\\(3,", "degree = rep\\(1,", x_fmt[1])
  )

  if (!all(vars_available <- all.vars(formula) %in% colnames(data))) {
    stop(paste0(
      c(
        "Some variables are not available in the dataset provided:",
        paste("  * ", all.vars(formula)[!vars_available])
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

  message(paste(model_call, collapse = "\n"), appendLF = TRUE)

  if (inherits(res_model, "try-error")) {
    stop(attr(res_model, "condition")$message)
  } else {
    res_model
  }
}
