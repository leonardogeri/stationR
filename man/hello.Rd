\name{stationary.matrix}
\alias{stationary.matrix()}
\title{Create a data.frame object with Unit Root Tests stats on Time Series}
\usage{
stationary.matrix(..., critical = TRUE)
}
\arguments{
\item{...}{time series to check the stationarity}

\item{critical}{boolean variable that determines wheter the critical values for the UR tests will be shown on the data frame}
}
\description{
Returns a data frame with the stats for Augmented Dickey-Fuller, Phillips-Perron and KPSS unit root tests and their critical values.
}
\examples{
#From the BECS github package
x <- BECS::RIsentiment$SentimentGI
stationary.matrix(x, critical = TRUE)
}

