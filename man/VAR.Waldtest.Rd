\name{VAR.Waldtest}
\alias{VAR.Waldtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wald test for parameter restrictions
}
\description{
The function returns the system Wald test
}
\usage{
VAR.Waldtest(x, p, restrict, type = "const")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix}
  \item{p}{ VAR order }
  \item{restrict}{ Restriction matrix under H0}
  \item{type}{"const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}
\details{

Restriction matrix is of m by 3 matrix where m is the number of restrictions.
A typical row of this matrix (k,i,j), which means that (i,j) element of Ak matrix is set to 0.
Ak is a VAR coefficient matrix (k = 1,....p).
Under H1, the model is full VAR.
}
\value{
\item{Fstat}{Wald test statistic}
\item{Fpval}{p-value of the test}
}
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer
}
\author{
Jae H. Kim
}
\note{
See Chapter 3 of Lutkepohl
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
data(dat)
#replicating Section 3.6.2
restrict = rbind( c(1,1,2),c(1,1,3), c(2,1,2),c(2,1,3))
VAR.Waldtest(dat,p=2,restrict,type="const")
}
\keyword{ ts }
