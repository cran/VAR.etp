\name{VAR.FOR}
\alias{VAR.FOR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VAR Forecasting
}
\description{
Generate point forecasts and prediction intervals
}
\usage{
VAR.FOR(x, p, h, type = "const", alpha = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix}
  \item{p}{ VAR order }
  \item{h}{ Forecasting Periods}
  \item{type}{"const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend}
  \item{alpha}{ 100(1-alpha) percent prediction intervals}
}
\details{
Prediction Intervals are based on normal approximation
}
\value{
\item{Intervals }{ Prediction Intervals}
\item{Forecast }{ Point Forecasts}
\item{alpha }{ Probability Content of Prediction Interva;s}
}
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer
}
\author{
Jae H. Kim
}
\note{
See Chapter 3 of Lutkepohl (2005)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#replicating Section 3.5.3 of Lutkepohl (2005)
data(dat)
VAR.FOR(dat,p=2,h=10,type="const",alpha=0.95)
}
\keyword{ ts }
