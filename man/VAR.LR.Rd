\name{VAR.LR}
\alias{VAR.LR}

\title{
The Likelihood Ratio test for parameter restrictions}
\description{
Likelihood Ratio test for zero parameter restrictions based on system VAR estimation

Bootstrap option is available: iid bootstrap or wild bootstrap

Bootstrap is conducted under the null hypothesis using estimated GLS estimation: see Kim (2014)
}
\usage{
VAR.LR(x, p, restrict0, restrict1, type = "const",bootstrap=0,nb=500)
}

\arguments{
  \item{x}{ data matrix in column}
  \item{p}{ VAR order }
  \item{restrict0}{ Restriction matrix under H0}
  \item{restrict1}{ Restriction matrix under H1, if "full", the full VAR is estimated under H1}
  \item{type}{"const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  \item{bootstrap}{ 0 for no bootstrap; 1 for iid bootstrap; 2 for wild bootstrap}
  \item{nb}{ the number of bootstrap iterations}
}
\details{
Restriction matrix is of m by 3 matrix where m is the number of restrictions.
A typical row of this matrix (k,i,j), which means that (i,j) element of Ak matrix is set to 0.
Ak is a VAR coefficient matrix (k = 1,....p).


The bootstrap test is conducted using the GLS estimation under the parameter restrictions implied by the null hypothesis: see Kim (2014) for details.

Kim (2014) found that the bootstrap based on OLS can show inferior small sample properties.

There are two versions of the bootstrap: the first is based on the iid resampling and the second based on wild bootstrapping. 

The Wild bootstrap is conducted with Mammen's two-point distribution.

}
\value{
\item{LRstat}{LR test statistic}
\item{pval}{p-value of the LR test}
\item{Boot.pval}{p-value of the test based on bootstrapping}
}
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer

Kim, J.H. 2014, Testing for parameter restrictions in a stationary VAR model: a bootstrap alternative. Economic Modelling, 41, 267-273.

}
\author{
Jae H. Kim
}
\note{
See Chapter 4 of Lutkepohl (2005)
}

\examples{
data(dat)
#replicating Table 4.4 of Lutkepohl (2005)
restrict1="full";
restrict0 = rbind(c(4,1,1), c(4,1,2), c(4,1,3), c(4,2,1),
c(4,2,2),c(4,2,3),c(4,3,1),c(4,3,2),c(4,3,3))
VAR.LR(dat,p=4,restrict0,restrict1,type="const")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
