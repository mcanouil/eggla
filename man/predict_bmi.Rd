% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_bmi.R
\name{predict_bmi}
\alias{predict_bmi}
\title{Predict BMI for a range of ages from a model fit.}
\usage{
predict_bmi(fit, start = 0.25, end = 10, step = 0.01, filter = NULL)
}
\arguments{
\item{fit}{A model object from a statistical model
such as from a call \code{nlme::lme()}, \code{time_model()} or \code{egg_model()}.}

\item{start}{The start of the time window to compute AP and AR.}

\item{end}{The end of the time window to compute AP and AR.}

\item{step}{The step to increment the sequence.}

\item{filter}{A string following \code{data.table} syntax for filtering on \code{"i"}
(\emph{i.e.}, row elements), \emph{e.g.}, \code{filter = "source == 'A'"}.
Argument pass through \code{compute_apar()} (see \code{predict_bmi()}).
Default is \code{NULL}.}
}
\value{
A \code{data.table} object.
}
\description{
Predict BMI values a cubic splines mixed model regression
with three splines parametrisation as random effect.
This function also works for any model obtained using \code{time_model()}.
}
\examples{
data("bmigrowth")
res <- egg_model(
  formula = log(bmi) ~ age,
  data = bmigrowth[bmigrowth[["sex"]] == 0, ],
  id_var = "ID",
  random_complexity = 1
)

predict_bmi(res)[]

## For multiple sources of measures or multiple measures at one age
set.seed(1234)
dta <- bmigrowth[bmigrowth[["sex"]] == 0, ]
dta[["source"]] <- c("A", "B")[rbinom(n = nrow(dta), size = 1, prob = 0.65) + 1]

res <- egg_model(
  formula = log(bmi) ~ age + source,
  data = dta,
  id_var = "ID",
  random_complexity = 1
)

predict_bmi(res)[order(egg_id, egg_ageyears)]

predict_bmi(res, filter = "source == 'A'")[order(egg_id, egg_ageyears)]
}
