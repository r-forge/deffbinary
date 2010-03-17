\name{updateBinaryGaussian}
\alias{updateBinaryGaussian}
\title{Update Gaussian parameters with Binary observations}
\description{
	Generates a sample from the posterior of a Gaussian parameter 
	or random effect when observations are binary
     }
\usage{
	updateBinaryGaussian(y, X=NULL, coef, offsetY, meanCoef, precisionCoef, 
	niter=1, sdProposal=1, link=c("logit","cloglog"), ...)
     }
\arguments{
     \item{y}{Binary valued observations}
     \item{X}{Matrix or vector of covariates, defaults to intercept only}
     \item{coef}{starting values for coefficients to be updated}
     \item{offsetY}{offsets, defaults to zero}
     \item{meanCoef}{marginal mean for coefficients}
     \item{precisionCoef}{marginal variance for coefficients.  
     If \code{coef} is multivariate, precisionCoef can be either a matrix, 
     a vector, or a scalar.
     If vector or scaler, the precision is diagonal with the values
     supplied specifying the diagonals.} 
     \item{niter}{number of iterations, only the last sample is retained}
     \item{sdProposal}{Standard deviation of proposal distribution.  Similar to precisionCoef, 
     	can be vector or scalar.}
     \item{link}{link function, one of \code{logit} or \code{cloglog} for the complimentary log-log transform}
\item{..}{other parameters passed to the updater, currently none are used.}
}
\details{
The model used is the following:
	\deqn{	Y_i ~ Bern(p_i)}
	\deqn{	link(p_i) = offset_i + X_i coef }
	\deqn{	coef ~ N(meanX, precisionX^(-1))}
Currently all coefficients are updated simultaneously.	
} 
\value{
A vector of coefficients, with attributes specifying acceptance rates.
}	    
\seealso{
     \code{\link{}}.
     }
\examples{
	# simulate some data
	Nobservations = 20
	X = cbind(1, 1:20)
	coef = c(0, 1)
	
	offset = seq(from=-5, len=Nobservations, by=1)
	
	meanLink = X %*% coef + offset
	probs = exp(meanLink) / (1+exp(meanLink))
	
	y = rbinom(Nobservations, 1, probs)


	# generate posterior sample
	posteriorSample = updateBinaryGaussian(y, X, 
		coef=c(2,1), 
		offsetY=offset, meanCoef=c(1,2), precisionCoef=c(0.01, 0.01), 
		niter=10, sdProposal=c(0.2,0.1), link="logit")
	posteriorSample	 

}
\keyword{mcmc}

