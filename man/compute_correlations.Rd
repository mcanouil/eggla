% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_correlations.R
\name{compute_correlations}
\alias{compute_correlations}
\title{Compute the derived parameters correlations from a cubic splines mixed-effects model by \code{time_model()}.}
\usage{
compute_correlations(
  fit,
  method,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = list(cubic_slope = NULL, linear_splines = c(0.75, 5.5, 11), cubic_splines =
    c(1, 8, 12))[[method]]
)
}
\arguments{
\item{fit}{A model object from a statistical model such as
from a call to \code{time_model()}.}

\item{method}{The type of model provided in \code{fit},
\emph{i.e.}, one of \code{"cubic_slope"}, \code{"linear_splines"} or \code{"cubic_splines"}.}

\item{period}{The intervals knots on which AUCs are to be computed.}

\item{knots}{The knots as defined \code{fit} and according to \code{method}.}
}
\value{
A \code{list} object with correlations between each intervals derived parameters.
}
\description{
Based on computed area under the curves (\emph{i.e.}, \code{compute_aucs()})
and slopes (\emph{i.e.}, \code{compute_slopes()}) for several intervals using
a model fitted by \code{time_model()}, compute the correlations between
each intervals derived parameters.
}
\examples{
data("bmigrowth")
ls_mod <- time_model(
  x = "age",
  y = "log(bmi)",
  cov = NULL,
  data = bmigrowth[bmigrowth[["sex"]] == 0, ],
  method = "linear_splines"
)
compute_correlations(
  fit = ls_mod,
  method = "linear_splines",
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17)#,
  # knots = list(
  #   "cubic_slope" = NULL,
  #   "linear_splines" = c(0.75, 5.5, 11),
  #   "cubic_splines" = c(1, 8, 12)
  # )[[method]]
)
}
