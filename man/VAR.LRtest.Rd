\name{VAR.LRtest}
\alias{VAR.LRtest}

\title{
The LR test for parameter restrictions}
\description{
Log-Likelihood test for parameter restrictions
}
\usage{
VAR.LRtest(x, p, restrict0, restrict1, type = "const")
}

\arguments{
  \item{x}{ data matrix}
  \item{p}{ VAR order }
  \item{restrict0}{ Restriction matrix under H0}
  \item{restrict1}{ Restriction matrix under H1, if "full", the full VAR is estimated under H1}
  \item{type}{"const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}
\details{
Restriction matrix is of m by 3 matrix where m is the number of restrictions.
A typical row of this matrix (k,i,j), which means that (i,j) element of Ak matrix is set to 0.
Ak is a VAR coefficient matrix (k = 1,....p).
}
\value{
\item{LRstat}{LR test statistic}
\item{LRpval}{p-value of the LR test}
\item{Fstat}{F version of the test}
\item{Fpval}{p-value of F test}
}
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer
}
\author{
Jae H. Kim
}
\note{
See Chapter 4 of Lutkepohl (2005)
}

\examples{
data(dat)
#replicating Table 4.4
restrict1="full";
restrict0 = rbind(c(4,1,1), c(4,1,2), c(4,1,3), c(4,2,1),
c(4,2,2),c(4,2,3),c(4,3,1),c(4,3,2),c(4,3,3))
VAR.LRtest(dat,p=4,restrict0,restrict1,type="const")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
