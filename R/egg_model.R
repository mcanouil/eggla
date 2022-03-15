#' Fit a cubic splines mixed model.
#'
#' Fit a cubic splines mixed model regression
#' with three splines parametrisation as random effect.  
#' This function a specific version of `time_model`.
#'
#' @param formula An object of class "formula":
#'   a symbolic description of the model to be fitted with,
#'   time component as the first term in the right-hand side.
#' @param data A data.frame containing the variables defined in formula.
#' @param id_var A string indicating the name of the variable
#'   to be used as the individual identifier.
#' @param random_complexity A numeric (1-3) indicating the complexity of the random effect term.
#'  Default, `"auto"` will try from the more complex to the less complex if no success.
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
egg_model <- function(formula, data, id_var, random_complexity = "auto") {
  random_complexity <- match.arg(as.character(random_complexity), c("auto", 1, 2, 3))
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

  irandom <- 1
  res_model <- `class<-`(list(), "try-error")
  while (inherits(res_model, "try-error") & irandom <= length(form_random)) {
    model_call <- f_model_call(
      form_fixed = form_fixed,
      form_random = form_random[[irandom]],
      n_iteration = 500
    )
    message(
      "Fitting model:\n",
      paste(
        paste0("  ", model_call),
        collapse = "\n"
      )
    )
    res_model <- try(
      expr = eval(parse(text = paste(model_call, collapse = ""))),
      silent = TRUE
    )
    if (inherits(res_model, "try-error")) {
      message(
        "Number of iteration has been increased from 500 to 1,000.",
        appendLF = TRUE
      )
      model_call <- f_model_call(
        form_fixed = form_fixed,
        form_random = form_random[[irandom]],
        n_iteration = 1000
      )
      res_model <- try(
        expr = eval(parse(text = paste(model_call, collapse = ""))),
        silent = TRUE
      )
    }

    irandom <- irandom + 1
  }

  message(paste(res_model[["call"]], collapse = "\n"), appendLF = TRUE)

  if (inherits(res_model, "try-error")) {
    stop(attr(res_model, "condition")[["message"]])
  } else {
    res_model
  }
}

#' Derived slopes from a cubic splines mixed-effects model.
#'
#' Derived slopes for different intervals based on a fitted
#' cubic splines mixed-effects model from `egg_model`.  
#' This function a specific version of `predict_average_slopes`
#' designed to work specifically on `egg_model`.
#'
#' @param fit A model object from a statistical model
#'   such as from a call to `nlme::lme()` and `time_model()`.
#' @param period The intervals knots on which slopes are to be computed.
#' @param id_var A string indicating the name of the variable
#'   to be used as the individual identifier.
#'
#' @return A `data.frame` with slopes for each individuals/samples.
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
#' head(
#'   egg_slopes(
#'     fit = res,
#'     period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
#'     id_var = "ID"
#'   )
#' )
egg_slopes <- function(
  fit,
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
  id_var = "ID"
) {
  knots <- c(2, 8, 12)

  slopes <- matrix(
    data = NA_real_,
    nrow = length(unique(fit$data[[id_var]])),
    ncol = length(period) / 2
  )
  colnames(slopes) <- paste0(
    "slope_",
    sapply(split(
      x = period,
      f = rep(
        x = seq(1, length(period), length(period) %/% 4),
        each = length(period) %/% 4
      )
    ), paste, collapse = "--")
  )
  pred <- matrix(
    data = NA_real_,
    nrow = length(unique(fit$data[[id_var]])),
    ncol = length(period)
  )
  colnames(pred) <- paste0("pred_period_", round(period, digits = 1))

  fxef <- nlme::fixef(fit)
  fxef <- unname(fxef[
    grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(fxef))
  ])
  rnef <- nlme::ranef(fit)
  rnef <- rnef[, grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(rnef))]

  rnef <- cbind(
    rnef,
    matrix(
      data = rep(0, (length(fxef) - ncol(rnef)) * nrow(rnef)),
      nrow = nrow(rnef),
      ncol = length(fxef) - ncol(rnef)
    )
  )

  for (i in seq_along(unique(fit$data[[id_var]]))) {
     coeff <- fxef + as.numeric(rnef[i, ])
    for (j in 1:(length(period) / 2)) {
      x1 <- period[j * 2 - 1]
      y1_tmp <- coeff *
        c(x1^0, x1^1, x1^2, x1^3, (x1 - knots)^3) /
        c(1, 1, 2, rep(6, 4))
      y1 <- sum(y1_tmp[1:(4 + findInterval(x1, knots, left.open = TRUE))])

      x2 <- period[j * 2]
      y2_tmp <- coeff *
        c(x2^0, x2^1, x2^2, x2^3, (x2 - knots)^3) /
        c(1, 1, 2, rep(6, 4))
      y2 <- sum(y2_tmp[1:(4 + findInterval(x2, knots, left.open = TRUE))])

      pred[i, j * 2 - 1] <- y1
      pred[i, j * 2] <- y2
      slopes[i, j] <- (y2 - y1) / (x2 - x1)
    }
  }
  out <- cbind.data.frame(ID = unique(fit$data[[id_var]]), pred, slopes)
  names(out)[1] <- id_var
  out
}

#' Derived areas under the curve from a cubic splines mixed-effects model.
#'
#' Derived areas under the curve (AUCs) for differentintervals based
#' on a fitted cubic splines mixed-effects model from `egg_model`.  
#' This function is a specific version of `compute_auc`
#' designed to work specifically on `egg_model`.
#'
#' @param fit A model object from a statistical model
#' such as from a call to `nlme::lme()` and `time_model()`.
#' @param period The intervals knots on which AUCs are to be computed.
#' @param id_var A string indicating the name of the variable
#' to be used as the individual identifier.
#'
#' @return A `data.frame` with AUC for each individuals/samples.
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
#' head(
#'   egg_auc(
#'     fit = res,
#'     period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
#'     id_var = "ID"
#'   )
#' )
egg_auc <- function(
  fit,
  period = c(0, 0.5, 1.5, 5, 6, 10, 12, 17),
  id_var = "ID"
) {
  knots <- c(2, 8, 12)
  pred_auc <- matrix(
    data = NA_real_,
    nrow = length(unique(fit$data[[id_var]])),
    ncol = length(period) / 2
  )
  colnames(pred_auc) <- paste0(
    "auc_",
    sapply(split(
      x = period,
      f = rep(
        x = seq(1, length(period), length(period) %/% 4),
        each = length(period) %/% 4
      )
    ), paste, collapse = "--")
  )

  fxef <- nlme::fixef(fit)
  fxef <- unname(fxef[
    grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(fxef))
  ])
  rnef <- nlme::ranef(fit)
  rnef <- rnef[, grep("\\(Intercept\\)|gsp\\(.*\\)|poly\\(.*\\)", names(rnef))]

  rnef <- cbind(
    rnef,
    matrix(
      data = rep(0, (length(fxef) - ncol(rnef)) * nrow(rnef)),
      nrow = nrow(rnef),
      ncol = length(fxef) - ncol(rnef)
    )
  )

  y <- function(x, coeff, knots) {
    sapply(
      X = x,
      FUN = function(x) {
        y_tmp <- coeff *
          c(x^0, x^1, x^2, x^3, (x - knots)^3) /
          c(1, 1, 2, rep(6, 4))
        sum(y_tmp[1:(4 + findInterval(x, knots, left.open = TRUE))])
      }
    )
  }
  for (i in seq_along(unique(fit$data[[id_var]]))) {
    coeff <- fxef + as.numeric(rnef[i, ])
    for (j in 1:(length(period) / 2)) {
        pred_auc[i, j] <- stats::integrate(
          f = y,
          coeff = coeff,
          knots = knots,
          lower = period[j * 2 - 1],
          upper = period[j * 2]
        )$value
    }
  }
  out <- cbind.data.frame(ID = unique(fit$data[[id_var]]), pred_auc)
  names(out)[1] <- id_var
  out
}
