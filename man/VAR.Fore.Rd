\name{VAR.Fore}
\alias{VAR.Fore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VAR Forecasting
}
\description{
Generate point forecasts using the estimated VAR coefficient matrix
}
\usage{
VAR.Fore(x, b, p, h, type = "const")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix}
  \item{b}{ matrix of coefficients from VAR.est or VAR.Rest}
  \item{p}{ VAR order }
  \item{h}{ Forecasting Periods}
  \item{type}{"const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend}
  }
\details{
Generate point forecasts using the estimated VAR coefficient matrix
}
\value{
\item{Fore }{ Point Forecasts, out-of-sample and dynamic}

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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#replicating Section 3.5.3 of Lutkepohl (2005)
data(dat)
b=VAR.est(dat,p=2,type="const")$coef
VAR.Fore(dat,b,p=2,h=10,type="const")
}
\keyword{ ts }
