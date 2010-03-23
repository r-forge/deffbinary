\name{updateGaussianGaussian}
\alias{updateGaussianGaussian}
\title{Update Gaussian parameters with Gaussian observations}
\description{
	Generates a sample from the posterior of a Gaussian parameter 
	or random effect when observations are Gaussian
    }
\usage{
	updateGaussianGaussian(y, X=1, offsetY,precisionY, 
	meanCoef, precisionCoef=NULL, 
	varCoef=solve(precisionCoef) ...)
    }
\arguments{
     \item{y}{Vector of Gaussian distributed observations}
     \item{X}{Matrix or vector of covariates, defaults to intercept only}
     \item{offsetY}{offsets, defaults to zero}
     \item{precisionY}{Precision (scalar, vector or matrix) of observations.  
     	If scalar or vector, it is converted to a diagonal precision matrix.}
     \item{meanCoef}{marginal mean for coefficients}
     \item{precisionCoef}{marginal variance for coefficients.}  
	\item{varCoef}{variance matrix of coeficients.  
		One of varCoef and precisionCoef must be specified.}
	\item{..}{other parameters passed to the updater, currently none are used.}
     }
\details{
	The model used is the following:
	\deqn{	Y_i ~ N( offset_i + X_i coef , precisionY^(-1)}
	\deqn{	coef ~ N(meanCoef, precisionCoef^(-1))}
A simulation from [coef|Y] is returned, using
\deqn{coef | Y ~ N[meanCoef +  X precisionCoef^{-1} precisionY (Y - offest) +   ,}
\deqn{precisionCoef^(-1) - precisionCoef^(-1) X precisionY X' precisionCoef^(-1)]}
This is obtained by noting that 
\deqn{var(Y) = precisionY^(-1) + X' precisionCoef^(-1) X}
\deqn{cov(Y,coef) = cov(X coef, coef) = X precisionCoef^{-1}}
and applying the conditional distribution for the multivariate normal as found at 
\code{en.wikipedia.org/wiki/Multivariate\_normal\_distribution\#Conditional\_distributions}
gives 



}    
\seealso{
     \code{\link{updateBinaryGaussian}}.
     }
\examples{
 	# simulate some data
	Nobservations = 20
	X = cbind(1, 1:20)
	coef = c(0, 1)
	
	offset = seq(from=-5, len=Nobservations, by=1)
	
	meanObs = X %*% coef + offset
	
	precisionY <- 2
	
	y = rnorm(Nobservations, meanObs, precisionY^(-0.5))


	# generate posterior sample
	posteriorSample = updateGaussianGaussian(y, X, 
		offsetY=offset, precisionY=precisionY, 
		meanCoef=c(1,2), precisionCoef=c(0.01, 0.01))
	posteriorSample	 
    }
\keyword{}

