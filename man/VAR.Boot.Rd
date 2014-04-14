\name{VAR.Boot}
\alias{VAR.Boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrapping VAR(p) model
}
\description{
The function returns bias-corrected parmater estimators and Bias estimators based on the bootstrap
}
\usage{
VAR.Boot(x, p, nb = 200, type = "const")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{ data matrix}
 \item{p}{ AR order }
 \item{nb}{ number of bootstrap iterations}
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
Kilian, L. (1998). Small sample confidence intervals for impulse response functions, The Review of Economics and Statistics, 80, 218 - 230.
}
\author{
Jae H. Kim
}

\examples{
y=data(dat)
VAR.Boot(dat,p=2,nb=200,type="const")
}
\keyword{ts }
