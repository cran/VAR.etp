\name{VAR.est}
\alias{VAR.est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of unrestricted VAR(p) model
}
\description{
This function returns least-squares estimation results for VAR(p) model
}
\usage{
VAR.est(x, p, type = "const")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix}
  \item{p}{ AR order }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  }
\details{
VAR estimation
}
\value{
  \item{coef }{coefficient matrix}
  \item{resid }{matrix of residuals}
  \item{sigu }{residual covariance matrix}
  \item{zzmat}{data matrix}
  \item{zmat}{data matrix}
  \item{tratio}{matrix of tratio corresponding to coef matrix}
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

\examples{

#replicating Section 3.2.3 of Lutkepohl
data(dat)
M=VAR.est(dat,p=2,type="const")
print(M$coef)
print(M$tratio)
}

\keyword{ ts }
