\name{VAR.select}
\alias{VAR.select}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Order Selection for VAR models
}
\description{
AIC, HQ, or SC can be used
}
\usage{
VAR.select(x, type = "const", ic = "aic", pmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix}
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  \item{ic}{ choose one of "aic", "sc", "hq"}
  \item{pmax}{ the maximum VAR order }
}
\details{Order Section Criterion}
\value{
\item{IC }{Values of information criterion for VAR models}
\item{p }{ AR order selected}

}
\references{
Lutkepohl, H. 2005, New Introduction to Multiple Time Series Analysis, Springer
}
\author{
JAe H. Kim
}
\note{
See Chapter 4 of Lutkepohl
}

\examples{
data(dat)
#replicating Section 4.3.1 of Lutkepohl (2005)
VAR.select(dat,pmax=4,ic="aic")}

\keyword{ ts }
