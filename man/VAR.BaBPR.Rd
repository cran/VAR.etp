\name{VAR.BaBPR}
\alias{VAR.BaBPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap-after-Bootstrap Prediction Intervals for VAR(p) Model
}
\description{Bias-correction given with stationarity Correction
}
\usage{
VAR.BaBPR(x, p, h, nboot = 500, nb = 200, type = "const", alpha = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix}
 \item{p}{ AR order }
 \item{h}{ forecasting period }
 \item{nboot}{ number of 2nd-stage bootstrap iterations}
  \item{nb}{ number of 1st-stage bootstrap iterations}
 \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
 \item{alpha}{ 100(1-alpha) percent prediction intervals}

}
\details{
Bias-correction given with stationarity Correction
}
\value{
\item{Intervals }{ Prediction Intervals}
\item{Forecast }{ Point Forecasts}
\item{alpha }{ Probability Content of Prediction Intervals}
}
\references{
Kim, J. H. (2001). Bootstrap-after-bootstrap prediction intervals for autoregressive models, Journal of Business & Economic Statistics, 19, 117-128.
}
\author{
Jae H. Kim
}
\note{
Bias-correction given with stationarity Correction
}
\examples{
data(dat)
VAR.BaBPR(dat,p=2,h=10,nboot=200,nb=100,type="const",alpha=0.95)}
\keyword{ ts }
