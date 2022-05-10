#' Fit a cubic splines mixed model.
#'
#' Fit a cubic splines mixed model regression
#' with three splines parametrisation as random effect.
#' This function a specific version of `time_model()`.
#'
#' @param formula An object of class "formula":
#'   a symbolic description of the model to be fitted with,
#'   time component as the first term in the right-hand side.
#' @param data A data.frame containing the variables defined in formula.
#' @param id_var A string indicating the name of the variable
#'   to be used as the individual identifier.
#' @param random_complexity A numeric (1-3) indicating the complexity of the random effect term.
#'  Default, `"auto"` will try from the more complex to the less complex if no success.
#' @param use_ar1 A logical indicating whether to use AR(1) for autocorrelation.
#' @param knots The knots defining the splines.
#' @param quiet A logical indicating whether to suppress the output.
#'
#' @return An object of class "lme" representing
#'   the linear mixed-effects model fit.
#'
#' @export
#'
#' @examples
#' data("bmigrowth")
#' res <- egg_model(
#'   formula = log(bmi) ~ age,
#'   data = bmigrowth[bmigrowth[["sex"]] == 0, ],
#'   id_var = "ID",
#'   random_complexity = 1
#' )
#' sres <- as.data.frame(summary(res)[["tTable"]])
#' rownames(sres) <- sub("gsp\\(.*\\)\\)", "gsp(...)", rownames(sres))
#' sres
egg_model <- function(
  formula,
  data,
  id_var,
  random_complexity = "auto",
  use_ar1 = FALSE,
  knots = c(2, 8, 12),
  quiet = FALSE
) {
  random_complexity <- match.arg(as.character(random_complexity), c("auto", 1, 2, 3))
  y <- as.character(formula)[[2]]
  x_cov <- strsplit(as.character(formula)[[3]], " \\+ ")[[1]]

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

  form_random <- c(
    "3" = sprintf(
      "~ %s | %s",
      x_fmt[1],
      id_var
    ),
    "2" = sprintf(
      "~ %s[,1:3] | %s",
      x_fmt[1],
      id_var
    ),
    "1" = sprintf(
      "~ %s | %s",
      sub("degree = rep\\(3,", "degree = rep\\(1,", x_fmt[1]),
      id_var
    )
  )

  if (random_complexity != "auto") {
    form_random <- form_random[random_complexity]
  }

  if (!all(vars_available <- all.vars(formula) %in% colnames(data))) {
    stop(paste0(
      c(
        "Some variables are not available in the dataset provided:",
        paste("  * ", all.vars(formula)[!vars_available])
      ),
      collapse = "\n"
    ))
  }

  f_model_call <- function(form_fixed, form_random, n_iteration, use_ar1) {
    c(
      "nlme::lme(",
      paste0("  fixed = ", form_fixed, ","),
      "  data = data,",
      paste0("  random = ", form_random, ","),
      "  na.action = stats::na.omit,",
      "  method = \"ML\",",
      if (use_ar1) sprintf("  correlation = nlme::corCAR1(form = ~ 1 | %s),", id_var) else NULL,
      paste0(
        "  control = nlme::lmeControl(opt = \"optim\", maxIter = ",
        n_iteration, ", msMaxIter = ", n_iteration, ")"
      ),
      ")"
    )
  }

  irandom <- 1
  res_model <- `class<-`(list(), "try-error")
  while (inherits(res_model, "try-error") & irandom <= length(form_random)) {
    model_call <- f_model_call(
      form_fixed = form_fixed,
      form_random = form_random[[irandom]],
      n_iteration = 500,
      use_ar1 = use_ar1
    )
    if (!quiet) {
      message(
        "Fitting model:\n",
        paste(
          paste0("  ", model_call),
          collapse = "\n"
        )
      )
    }
    res_model <- try(
      expr = eval(parse(text = paste(model_call, collapse = ""))),
      silent = TRUE
    )
    if (inherits(res_model, "try-error")) {
      if (!quiet) message("Number of iteration has been increased from 500 to 1,000.")
      model_call <- f_model_call(
        form_fixed = form_fixed,
        form_random = form_random[[irandom]],
        n_iteration = 1000,
        use_ar1 = use_ar1
      )
      res_model <- try(
        expr = eval(parse(text = paste(model_call, collapse = ""))),
        silent = TRUE
      )
    }

    irandom <- irandom + 1
    if (inherits(res_model, "try-error") & irandom > length(form_random) & use_ar1) {
      if (!quiet) message("Model with AR(1) auto-correlation failed, now trying without it ...")
      use_ar1 <- FALSE
      irandom <- 1
    } else {
      break
    }
  }

  if (inherits(res_model, "try-error")) {
    stop(attr(res_model, "condition")[["message"]])
  } else {
    res_model
  }
}
