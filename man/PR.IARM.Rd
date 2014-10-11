\name{PR.IARM}
\alias{PR.IARM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Improved Augmented Regression Method (IARM) for Predictive Regression
}
\description{
Function for Improved ARM (IARM) estimation and testing
}
\usage{
PR.IARM(x, y, p, Rmat = diag(k * p), rvec = matrix(0, nrow = k * p))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ predictor or a matrix of predictors in column}
  \item{y}{ variable to be predicted, usually data1 return}
  \item{p}{ AR order}
  \item{Rmat}{Restriction matrix, refer to function Rmatrix}
  \item{rvec}{Restriction matrix, refer to function Rmatrix
}
}
\details{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance,  26, 13-25.
}
\value{

 \item{LS }{Ordinary Least Squares Estimators}
 \item{IARM }{IARM Estimators}
 \item{AR }{ AR parameter estimators}
 \item{ARc }{ Bias-corrected AR parameter estimators}
 \item{Fstats }{ Fstats and their p-values}
 \item{Covbc }{ Covariance matrix of the IARM estimators (for the predictive coefficients only)}
}
\references{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance, 26, 13-25.
}

\author{
Jae H. Kim
}
\note{
Kim J.H., 2014, Predictive Regression: Improved Augmented Regression Method, Journal of Empirical Finance,  26, 13-25.
}

\examples{
data(data1)
# Replicating Table 5 (excess return) of Kim (2014)
y=data1$ret.nyse.vw*100 -data1$tbill*100
x=cbind(log(data1$dy.nyse), data1$tbill*100); k=ncol(x) 

M=PR.IARM(x,y,p=1)}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts }

