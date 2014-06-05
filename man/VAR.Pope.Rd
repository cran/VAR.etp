\name{VAR.Pope}
\alias{VAR.Pope}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bias-correction for VAR parameter estimators based on Pope's formula
}
\description{
The function returns bias-corrected parmater estimators and Bias estimators based on the bootstrap
}
\usage{
VAR.Pope(x, p, type = "const")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{ data matrix}
 \item{p}{ AR order }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}

\details{
Kilian's (1998) stationarity-correction is used for bias-correction
}
\value{
\item{coef }{coefficient matrix}
  \item{resid }{matrix of residuals}
  \item{sigu }{residual covariance matrix}
  \item{Bias}{Bootstrap Bias Estimator}
}
\references{


Kim, J. H. 2004, Bias-corrected bootstrap prediction regions for Vector Autoregression, Journal of FOrecasting 23, 141-154.

Kilian, L. (1998). Small sample confidence intervals for impulse response functions, The Review of Economics and Statistics, 80, 218 - 230.

Nicholls DF, Pope AL. 1988, Bias in estimation of multivariate autoregression. Australian Journal of Statistics, 30A, 296-309.

Pope AL. 1990. Biases of estimators in multivariate non-Gaussian autoregression, Journal of Time Series Analysis 11, 249-258.

}
\author{
Jae H. Kim
}

\examples{
data(dat)
VAR.Pope(dat,p=2,type="const")
}
\keyword{ts }
