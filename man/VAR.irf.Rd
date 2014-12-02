\name{VAR.irf}
\alias{VAR.irf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Orthogonalized impluse response functions from an estimated VAR(p) model
}
\description{
This function returns Orthogonalized impluse response functions
}
\usage{
VAR.irf(b, p, sigu, h=10,graphs=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ VAR coefficient matrix, from VAR.est or similar estimation function}
  \item{p}{ VAR order }
  \item{sigu}{ VAR residual covariance matrix, from VAR.est or similar estimation function }
  \item{h}{ response horizon, the default is set to 10 }
  \item{graphs}{ logical, if TRUE, show the impulse-response functions, the default is FALSE }
  }
\details{
VAR impulse response functions
}
\value{
  \item{impmat }{matrix that contains orthogonalized impulse-responses }
  }
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer
}
\author{
Jae H. Kim
}
\note{
See Lutkepohl (2005) for details
}

\examples{

#replicating Table 3.4 and Figure 3.11 Lutkepohl (2005)
data(dat)
M=VAR.est(dat,p=2,type="const")
b=M$coef; sigu=M$sigu
VAR.irf(b,p=2,sigu,graphs=TRUE)
}

\keyword{ ts }
