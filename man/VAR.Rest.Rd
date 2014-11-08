\name{VAR.Rest}
\alias{VAR.Rest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VAR parameter estimation with parameter restrictions
}
\description{
Estimation of VAR with 0 restrictions on parameters
}
\usage{
VAR.Rest(x, p, restrict, type = "const", method = "gls")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix in column}
  \item{p}{ VAR order }
  \item{restrict}{ Restriction matrix under H0}
  \item{type}{"const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  \item{method}{"ols" for OLS estimation, "gls" for EGLS estimation }
}
\details{

Restriction matrix is of m by 3 matrix where m is the number of restrictions.
A typical row of this matrix (k,i,j), which means that (i,j) element of Ak matrix is set to 0.
Ak is a VAR coefficient matrix (k = 1,....p).

}
\value{
  \item{coef }{coefficient matrix}
  \item{resid }{matrix of residuals}
  \item{sigu }{residual covariance matrix}
  \item{zmat}{data matrix}
  \item{tstat}{matrix of tratio corresponding to coef matrix}
}
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer
}
\author{
Jae H. Kim
}
\note{
See Chapter 5 of Lutkepohl
}

\examples{
data(dat) 
#replicating Section 5.2.10 of Lutkepohl (2005)
restrict = rbind( c(1,1,2),c(1,1,3),c(1,2,1),c(1,2,2), c(1,3,1),
c(2,1,1), c(2,1,2),c(2,1,3), c(2,2,2), c(2,2,3),c(2,3,1), c(2,3,3),
c(3,1,1), c(3,1,2), c(3,1,3), c(3,2,1), c(3,2,2), c(3,2,3), c(3,3,1),c(3,3,3),
c(4,1,2), c(4,1,3), c(4,2,1), c(4,2,2), c(4,2,3), c(4,3,1),c(4,3,2),c(4,3,3))
M= VAR.Rest(dat,p=4,restrict,type="const",method="gls")
print(M$coef)
print(M$tstat)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
