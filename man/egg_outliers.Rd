% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/egg_outliers.R
\name{egg_outliers}
\alias{egg_outliers}
\title{Compute outliers detection in AUCs/Slopes derived parameters from a cubic splines mixed-effects model by \code{egg_model()}.}
\usage{
egg_outliers(
  fit,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = c(1, 8, 12),
  from = c("predicted", "observed"),
  start = 0.25,
  end = 10,
  step = 0.01,
  filter = NULL,
  outlier_method = "iqr",
  outlier_threshold = list(iqr = 2)
)
}
\arguments{
\item{fit}{A model object from a statistical model
such as from a call to \code{egg_model()}.}

\item{period}{The intervals knots on which slopes are to be computed.}

\item{knots}{The knots as defined \code{fit} and according to \code{method}.}

\item{from}{A string indicating the type of data to be used for the AP and AR
computation, either "predicted" or "observed". Default is "predicted".}

\item{start}{The start of the time window to compute AP and AR.}

\item{end}{The end of the time window to compute AP and AR.}

\item{step}{The step to increment the sequence.}

\item{filter}{A string following \code{data.table} syntax for filtering on \code{"i"}
(\emph{i.e.}, row elements), \emph{e.g.}, \code{filter = "source == 'A'"}.
Argument pass through \code{compute_apar()} (see \code{predict_bmi()}).
Default is \code{NULL}.}

\item{outlier_method}{The outlier detection method(s). Default is \code{"iqr"}. Can be
\code{"cook"}, \code{"pareto"}, \code{"zscore"}, \code{"zscore_robust"}, \code{"iqr"}, \code{"ci"}, \code{"eti"},
\code{"hdi"}, \code{"bci"}, \code{"mahalanobis"}, \code{"mahalanobis_robust"}, \code{"mcd"}, \code{"ics"},
\code{"optics"} or \code{"lof"}.
See \code{performance::check_outliers()} \url{https://easystats.github.io/performance/reference/check_outliers.html} for details.}

\item{outlier_threshold}{A list containing the threshold values for each method (\emph{e.g.},
\code{list('mahalanobis' = 7, 'cook' = 1)}), above which an observation is
considered as outlier. If \code{NULL}, default values will be used (see
'Details'). If a numeric value is given, it will be used as the threshold
for any of the method run.
See \code{performance::check_outliers()} \url{https://easystats.github.io/performance/reference/check_outliers.html} for details.}
}
\value{
A \code{data.frame} listing the individuals which are not outliers based on several criteria.
}
\description{
Based on computed area under the curves (\emph{i.e.}, \code{egg_aucs()})
and slopes (\emph{i.e.}, \code{egg_slopes()}) for several intervals using
a model fitted by \code{egg_model()}, compute an outlier detection.
For details, see methods \code{iqr} and \code{zscore} of \code{performance::check_outliers()}.
}
\examples{
data("bmigrowth")
res <- egg_model(
  formula = log(bmi) ~ age,
  data = bmigrowth[bmigrowth[["sex"]] == 0, ],
  id_var = "ID",
  random_complexity = 1
)
head(egg_outliers(
  fit = res,
  period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
  knots = c(1, 8, 12)
)[Outlier != 0])
}
