\name{PR.order}
\alias{PR.order}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Improved Augmented Regression Method for Predictive Regression
}
\description{
Function to select the order p by AIC or BIC 
}
\usage{
PR.order(x, y, pmax = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{ predictor}
  \item{y}{ variable to be predicted, usually data1 return}
  \item{pmax}{ maximum order for order selection }
}
\details{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance
}
\value{
 \item{p.aic }{order chosen by AIC}
 \item{p.aic }{order chosen by BIC}

}
\references{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance
}
\author{
Jae H. Kim
}
\note{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

data(data1)
# Replicating Table 5 (excess return)
y=data1$ret.nyse.vw*100 -data1$tbill*100
x=cbind(log(data1$dy.nyse), data1$tbill*100); k=ncol(x) 

p=PR.order(x,y,pmax=10)$p.bic;  # AR(1) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts }

