% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Forecast.R
\name{mae}
\alias{mae}
\title{Calculate MAE of a Forecast object}
\usage{
mae(Forecast)
}
\arguments{
\item{Forecast}{Forecast object.}
}
\value{
MAE value.
}
\description{
\code{mae} takes a \code{\link{Forecast}} object and returns the MAE of the
forecast. MAE is calculated as:
\code{1/length(forecast) * sum(abs(forecast - realized))}
}
\examples{

my_forecast <- Forecast(
  origin   = as.Date(c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31")),
  future   = as.Date(c("2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31")),
  forecast = c(4.21, 4.27, 5.32, 5.11),
  realized = c(4.40, 4.45, 4.87, 4.77),
  h_ahead  = 4L
)

mae(my_forecast)

}
