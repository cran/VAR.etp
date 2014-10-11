\name{VAR.Wald}
\alias{VAR.Wald}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wald test for parameter restrictions
}
\description{
Wald test for zero parameter restrictions based on system VAR estimation

Bootstrap option is available: iid bootstrap or wild bootstrap

Bootstrap is conducted under the null hypothesis using estimated GLS estimation: see Kim (2014) 

}
\usage{
VAR.Wald(x, p, restrict, type = "const",bootstrap=0,nb=500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix}
  \item{p}{ VAR order }
  \item{restrict}{ Restriction matrix under H0}
  \item{type}{"const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  \item{bootstrap}{ 0 for no bootstrap; 1 for iid bootstrap; 2 for wild bootstrap}
  \item{nb}{ the number of bootstrap iterations}
}
\details{

Restriction matrix is of m by 3 matrix where m is the number of restrictions.
A typical row of this matrix (k,i,j), which means that (i,j) element of Ak matrix is set to 0.
Ak is a VAR coefficient matrix (k = 1,....p).
Under H1, the model is full VAR.

The bootstrap test is conducted using the GLS estimation under the parameter restrictions implied by the null hypothesis: see Kim (2014) for details.

Kim (2014) found that the bootstrap based on OLS can show inferior small sample properties.

There are two versions of the bootstrap: the first is based on the iid resampling and the second based on wild bootstrapping. 

The Wild bootstrap is conducted with Mammen's two-point distribution.

}
\value{
\item{Fstat}{Wald test statistic}
\item{pval}{p-value of the test based on F-distribution}
\item{Boot.pval}{p-value of the test based on bootstrapping}
}
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer.

Kim, J.H. 2014, Testing for parameter restrictions in a stationary VAR model: a bootstrap alternative. Economic Modelling, 41, 267-273.
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
#replicating Section 3.6.2 of Lutkepohl (2005)
restrict = rbind( c(1,1,2),c(1,1,3), c(2,1,2),c(2,1,3))
VAR.Wald(dat,p=2,restrict,type="const")
}
\keyword{ ts }
