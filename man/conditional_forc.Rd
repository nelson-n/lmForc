% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditional_forc.R
\name{conditional_forc}
\alias{conditional_forc}
\title{Linear model forecast conditioned on an input forecast}
\usage{
conditional_forc(lm_call, time_vec, ...)
}
\arguments{
\item{lm_call}{Linear model call of the class lm.}

\item{time_vec}{Vector of any class that is equal in length to the data
in \code{lm_call}.}

\item{...}{One or more forecasts of class Forecast, one forecast for each
covariate in the linear model.}
}
\value{
\code{\link{Forecast}} object that contains the conditional forecast.
}
\description{
\code{conditional_forc} takes a linear model call, a vector of time data
associated with the linear model, and a forecast for each covariate in the
linear model. The linear model is estimated once over the entire sample
period and the coefficients are multiplied by the forecasts of each
covariate. Returns a forecast conditional on forecasts of each covariate.
Used to create a forecast for the present period or replicate a forecast made
at a specific period in the past.
}
\examples{

x1_forecast <- Forecast(
   origin   = as.Date(c("2012-06-30", "2012-06-30", "2012-06-30", "2012-06-30")),
   future   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
   forecast = c(4.14, 4.04, 4.97, 5.12),
   realized = NULL,
   h_ahead  = NULL
)

x2_forecast <- Forecast(
   origin   = as.Date(c("2012-06-30", "2012-06-30", "2012-06-30", "2012-06-30")),
   future   = as.Date(c("2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30")),
   forecast = c(6.01, 6.05, 6.55, 7.45),
   realized = NULL,
   h_ahead  = NULL
)

date <- as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                  "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", 
                  "2012-03-31", "2012-06-30"))
y  <- c(1.09, 1.71, 1.09, 2.46, 1.78, 1.35, 2.89, 2.11, 2.97, 0.99)
x1 <- c(4.22, 3.86, 4.27, 5.60, 5.11, 4.31, 4.92, 5.80, 6.30, 4.17)
x2 <- c(10.03, 10.49, 10.85, 10.47, 9.09, 10.91, 8.68, 9.91, 7.87, 6.63)
data <- data.frame(date, y, x1, x2)

conditional_forc(
  lm_call = lm(y ~ x1 + x2, data),
  time_vec = data$date,
  x1_forecast, x2_forecast
)

}
\seealso{
For a detailed example see the help vignette:
\code{vignette("lmForc", package = "lmForc")}
}
