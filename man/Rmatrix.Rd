\name{Rmatrix}
\alias{Rmatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Improved Augmented Regression Method for Predictive Regression
}
\description{
Function to generate restriction matrices
}
\usage{
Rmatrix(p, k, type = 1, index = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{AR order }
  \item{k}{number of predictors} 
  \item{type}{ type = 1: H0: b1=b2=b3=0; type = 2: H0: b1+b2+b3=0 }
  \item{index}{ index=0 : H0 applies for all parameters; index=k : H0 applies for kth predictor }
}
\details{
Function to generate restriction matrices
}
\value{
 \item{Rmat }{this value should be passed to PR.IARM}
 \item{rvec }{this value should be passed to PR.IARM}
}
\references{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance
}
\author{
Jae H. Kim
}

\examples{
Rmat1=Rmatrix(p=1,k=1,type=2,index=1); Rmat=Rmat1$Rmat; rvec=Rmat1$rvec

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }

