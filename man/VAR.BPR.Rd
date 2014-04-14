\name{VAR.BPR}
\alias{VAR.BPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap Prediction Intervals for VAR(p) Model
}
\description{
No Bias-correction is given
}
\usage{
VAR.BPR(x, p, h, nboot = 500, type = "const", alpha = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{ data matrix}
 \item{p}{ AR order }
 \item{h}{ forecasting period }
 \item{nboot}{ number of bootstrap iterations}
 \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
 \item{alpha}{ 100(1-alpha) percent prediction intervals}
}
\details{
Bootstrap Prediction Intervals for VAR(p) Model
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
No Bias-correction is given
}

\examples{
data(dat)
VAR.BPR(dat,p=2,h=10,nboot=200,type="const",alpha=0.95)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
