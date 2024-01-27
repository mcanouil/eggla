#' General regression splines with variable degrees and ness, smoothing splines.
#'
#' From https://github.com/gmonette/spida2 because namespace and dependencies are not properly listed.
#' Source: https://github.com/gmonette/spida2/blob/master/R/gsp.R,
#' https://github.com/gmonette/spida2/blob/master/R/gsp_util.R
#'
#' @author Monette, G. \email{georges@@yorku.ca}
#'
#' @param x value(s) where spline is evaluated.
#' @param knots vector of knots.
#' @param degree vector giving the degree of the spline in each interval. Note
#'     the number of intervals is equal to the number of knots + 1. A value of 0
#'     corresponds to a constant in the interval. If the spline should evaluate to
#'     0 in the interval, use the \code{intercept} argument to specify some value
#'     in the interval at which the spline must evaluate to 0.
#' @param smoothness vector with the degree of smoothness at each knot
#'     (0 = continuity, 1 = smoothness with continuous first derivative, 2 = continuous
#'     second derivative, etc. The value -1 allows a discontinuity at the knot.
#'     A scalar is recycled so its length equals the number of knots. Alternatively,
#'     a list of length equal to the number of knots. Each element of the list is a
#'     vector of the orders of derivatives which are required to be smooth. THis allows
#'     non-sequential constraints, _e.g._, to have the same first and second derivative
#'     on either side of a knot but a possible discontinuity and change in
#'     higher-order derivatives, the vector would be c(1,2). Note that if a list is used,
#'     all elements must provide all desired constraints. That is the list argument corresponding
#'     to `smoothness = c(1,2,-1)` is `smoothness=list(0:1, 0:2, -1)`.
#' @param lin provides a matrix specifying additional linear contraints on the
#'     'full' parametrization consisting of blocks of polynomials of degree equal
#'     to max(degree) in each of the length(knots)+1 intervals of the spline. See
#'     below for examples of a spline that is 0 outside of its boundary knots.
#' @param periodic if TRUE generates a period spline on the base interval (0,max(knots)).
#'     A constraint is generated so that the coefficients generate
#'     the same values to the right of max(knots) as they do to the right of 0.
#'     Note that all knots should be strictly positive.
#' @param intercept value(s) of x at which the spline has value 0, _i.e._, the
#'     value(s) of x for which yhat is estimated by the intercept term in the
#'     model. The default is 0. If NULL, the spline is not constrained to evaluate
#'     to 0 for any x.
#' @param signif number of significant digits used to label coefficients.
#'
#' @return \code{gsp} returns a matrix generating a spline.
#'
#' @export
#' 
#' @examples
#' simd <- data.frame(
#'   age = rep(1:50, 2),
#'   y = sin(2 * pi * (1:100) / 5) + rnorm(100),
#'   G = rep(c("male", "female"), c(50, 50))
#' )
#' sp <- function(x) {
#'   gsp(x, knots = c(10, 25, 40), degree = c(1, 2, 2, 1), smoothness = c(1, 1 ,1))
#' }
#'
#' summary(lm(formula = y ~ sp(age) * G, data = simd))
gsp <- function(
  x,
  knots,
  degree = 3,
  smoothness = pmax(pmin(degree[-1], degree[-length(degree)]) - 1, -1),
  lin = NULL,
  periodic = FALSE,
  intercept = 0,
  signif = 3
) {
  if (periodic) {
    maxd <- max(degree)
    maxk <- max(knots)
    mid <- matrix(0, maxd + 1, (maxd + 1) * (length(knots) - 1))
    const.per <- do.call(cbind, list(diag(maxd + 1), mid, -PolyShift(maxk, maxd + 1)))
    lin <- rbind(lin, const.per)
  }
  degree <- rep(degree, length = length(knots) + 1)
  smoothness <- rep(smoothness, length = length(knots))
  spline.attr <- list(
    knots = knots, degree = degree, smoothness = smoothness,
    lin = lin, intercept = intercept, signif = signif
  )
  if (is.null(x)) {
    return(spline.attr)
  }
  if (periodic) x <- x %% maxk
  ret <- Xf(x, knots, max(degree), signif = signif) %*%
    spline.E(knots, degree, smoothness, lin = lin, intercept = intercept, signif = signif)
  attr(ret, "spline.attr") <- spline.attr
  class(ret) <- "gsp"
  ret
}

#' @export
print.gsp <- function(x, strip.attributes = TRUE, ...) {
  nms <- colnames(x)
  ncol <- ncol(x)
  xx <- x
  if (strip.attributes) {
    attributes(xx) <- NULL
    xx <- matrix(xx, ncol = ncol)
    colnames(xx) <- nms
  } else {
    (xx <- unclass(xx))
  }
  print(zapsmall(xx), ...)
  invisible(x)
}

#' @keywords internal
#' @noRd
PolyShift <- function(a, n) {
  ret <- matrix(0, n, n)
  pow <- a^(col(ret) - row(ret))
  ret[col(ret) >= row(ret)] <- unlist(sapply(0:(n - 1), function(x) choose(a, 0:a)))
  ret * pow
}

#' @keywords internal
#' @noRd
basis <- function(X, coef = FALSE) {
  q <- qr(X)
  sel <- q$pivot[seq_len(q$rank)]
  ret <- X[, sel]
  attr(ret, "cols") <- sel
  if (coef) attr(ret, "R") <- qr.coef(qr(ret), X)
  colnames(ret) <- colnames(X)[sel]
  ret
}

#' @keywords internal
#' @noRd
spline.E <- function(knots, degree, smooth, lin = NULL, intercept = 0, signif = 3) {
  cmat <- Cmat(knots, degree, smooth, lin, intercept, signif) # constraint matrix
  emat <- Emat(knots, degree, smooth, !is.null(intercept), signif) # estimation matrix
  # disp( list(C= cmat, E=emat ) )
  nc <- nrow(cmat)
  # ne <- nrow(emat)
  basisT <- t(basis(cbind(t(cmat), t(emat))))
  cols <- attr(basisT, "cols")
  ncc <- sum(cols <= nc)
  Tmat <- solve(basisT)
  Tmat[, (ncc + 1):ncol(Tmat)]
}

#' @keywords internal
#' @noRd
Xf <- function(x, knots, degree = 3, D = 0, right = TRUE, signif = 3) {
  xmat <- Xmat(x, degree, D, signif)
  k <- sort(knots)
  g <- cut(x, c(-Inf, k, Inf), right = right)
  do.call("cbind", lapply(seq_along(levels(g)), function(i) (g == levels(g)[i]) * xmat))
}

#' @keywords internal
#' @noRd
Xmat <- function(x, degree, D = 0, signif = 3) {
  if (length(D) < length(x)) D <- rep(D, length.out = length(x))
  if (length(x) < length(D)) x <- rep(x, length.out = length(D))
  xmat <- matrix(x, nrow = length(x), ncol = degree + 1)
  expvec <- 0:degree
  coeffvec <- rep(1, degree + 1)
  expmat <- NULL
  coeffmat <- NULL

  for (i in 0:max(D)) {
    expmat <- rbind(expmat, expvec)
    coeffmat <- rbind(coeffmat, coeffvec)
    coeffvec <- coeffvec * expvec
    expvec <- ifelse(expvec > 0, expvec - 1, 0)
  }
  G <- coeffmat[D + 1, ] * xmat^expmat[D + 1, ]

  xlab <- signif(x, signif)
  rownames(G) <- ifelse(D == 0, paste0("f(", xlab, ")"), paste0("D", D, "(", xlab, ")"))
  colnames(G) <- paste0("X", 0:(ncol(G) - 1))
  G
}

#' @keywords internal
#' @noRd
Cmat <- function(knots, degree, smooth, lin = NULL, intercept = 0, signif = 3) {
  dm <- max(degree)
  cmat <- NULL
  if (!is.null(intercept)) cmat <- rbind(cmat, "Intercept" = Xf(intercept, knots, dm, D = 0))

  for (i in seq_along(knots)) {
    k <- knots[i]
    sm <- smooth[i]
    if (sm > -1) { # sm = -1 corresponds to discontinuity
      dmat <- Xf(k, knots, dm, D = seq(0, sm), FALSE) - Xf(k, knots, dm, D = seq(0, sm), TRUE)
      rownames(dmat) <- paste0("C(", signif(k, signif), ").", seq(0, sm))
      cmat <- rbind(cmat, dmat)
    }
  }

  degree <- rep(degree, length.out = length(knots) + 1)
  for (i in seq_along(degree)) {
    di <- degree[i]
    if (dm > di) {
      dmat <- diag((length(knots) + 1) * (dm + 1))[(i - 1) * (dm + 1) + 1 + seq(di + 1, dm), , drop = FALSE]
      rownames(dmat) <- paste0("I.", i, ".", seq(di + 1, dm))

      cmat <- rbind(cmat, dmat)
    }
  }

  if (!is.null(lin)) cmat <- rbind(cmat, lin) # GM:2013-06-13
  rk <- qr(cmat)$rank
  spline.rank <- ncol(cmat) - rk
  attr(cmat, "ranks") <- c(npar.full = ncol(cmat), C.n = nrow(cmat), C.rank = rk, spline.rank = spline.rank)
  attr(cmat, "d") <- svd(cmat)$ d
  cmat
}


#' @keywords internal
#' @noRd
Emat <- function(knots, degree, smooth, intercept = FALSE, signif = 3) {
  if (length(degree) < length(knots) + 1) degree <- rep(degree, length.out = length(knots) + 1)
  # dmin <- min(degree)
  dmax <- max(degree)
  # smin <- min(smooth)
  # smax <- max(smooth)
  imax <- length(degree)

  zeroi <- as.numeric(cut(0, c(-Inf, sort(knots), Inf)))
  dzero <- degree[zeroi]

  cmat <- Xf(0, knots, degree = dmax, D = seq(1, dzero))

  if (imax > zeroi) {
    for (i in (zeroi + 1):imax) {
      d.right <- degree[i]
      d.left <- degree[i - 1]
      k <- knots[i - 1]
      sm <- smooth[i - 1]
      if (d.right > sm) {
        dmat <- Xf(k, knots, dmax, D = seq(sm + 1, d.right), FALSE) -
          Xf(k, knots, dmax, D = seq(sm + 1, d.right), TRUE)
        rownames(dmat) <- paste0("C(", signif(k, signif), ").", seq(sm + 1, d.right))
        cmat <- rbind(cmat, dmat)
      }
    }
  }

  if (zeroi > 1) {
    for (i in zeroi:2) {
      d.right <- degree[i]
      d.left <- degree[i - 1]
      k <- knots[i - 1]
      sm <- smooth[i - 1]
      if (d.left > sm) {
        dmat <- Xf(k, knots, dmax, D = seq(sm + 1, d.left), FALSE) -
          Xf(k, knots, dmax, D = seq(sm + 1, d.left), TRUE)
        rownames(dmat) <- paste0("C(", signif(k, signif), ").", seq(sm + 1, d.left))
        cmat <- rbind(cmat, dmat)
      }
    }
  }
  cmat
}


#' Hypothesis matrix for general regression splines
#'
#' This function helps to build hypothesis matrices for general splines. See examples
#' in \code{\link[spida2]{gsp}}.
#'
#' @param sp a spline function generated by \code{\link[spida2]{gsp}}
#' @param x points where spline is tested
#' @param D (default 0) order of derivative to test, 0 is the value of the spline, 1 is the first derivative, etc.
#' @param type (default 1) if there is a discontinuity at a knot, type specifies whether
#'             to estimate the limit from the left (0), the limit from the right (1)
#'             or the saltus -- limit from the right minus the limit from left (2)
#' @seealso \code{\link[spida2]{wald}} \code{\link[spida2]{gsp}}
#' @keywords internal
#' @noRd
sc <- function(sp, x, D = 0, type = 1) {
  a <- sp(NULL)
  D <- rep(D, length.out = length(x))
  type <- rep(type, length.out = length(x))
  left <- Xf(x, knots = a$knots, degree = max(a$degree), D = D, right = TRUE)
  right <- Xf(x, knots = a$knots, degree = max(a$degree), D = D, right = FALSE)
  cleft <- c(1, 0, -1) [match(type, c(0, 1, 2))]
  cright <- c(0, 1, 1) [match(type, c(0, 1, 2))]
  raw <- left * cleft + right * cright
  nam <- rownames(raw)
  nam <- sub("^f", "g", nam)
  nam0 <- sub("\\)", "-)", nam)
  nam1 <- sub("\\)", "+)", nam)
  nam2 <- paste0(nam1, "-", nam0)
  rownames(raw) <- ifelse(match(x, a$knots, 0) > 0,
    cbind(nam0, nam1, nam2) [cbind(seq_along(type), type + 1)],
    ifelse(type != 2, nam, "0")
  )
  mod <- raw %*% spline.E(
    a$knots, a$degree, a$smoothness,
    lin = a$lin,
    intercept = a$intercept,
    signif = a$signif
  )
  mod
}
