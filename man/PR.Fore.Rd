\name{PR.Fore}
\alias{PR.Fore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Improved Augmented Regression Method for Predictive Regression
}
\description{
Function for forecasting based on Imporved ARM
}
\usage{
PR.Fore(x, y, M, h = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ predictor or matrix of predictors in column}
  \item{y}{ variable to be predicted, usually stock return}
  \item{M}{ Estimation results of the function PR.IARM}
  \item{h}{ forecasting period }
}
\details{
Function for forecasting based on Imporved ARM
}
\value{
\item{Fore }{Out-of sample and dynamic forecasts for y and x}
}
\references{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance 25, 13-15.
}
\author{
jae H. Kim
}
\note{Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance 25, 13-15.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(data1)
# Replicating Table 5 (excess return)
y=data1$ret.nyse.vw*100 -data1$tbill*100
x=cbind(log(data1$dy.nyse), data1$tbill*100); k=ncol(x) 
p=4
Rmat1=Rmatrix(p,k,type=1,index=1); Rmat=Rmat1$Rmat; rvec=Rmat1$rvec
M=PR.IARM(x,y,p,Rmat,rvec)
PRF=PR.Fore(x,y,M)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ys }

