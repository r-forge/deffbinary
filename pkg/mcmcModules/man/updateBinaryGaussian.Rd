\name{updateBinaryGaussian}
\alias{}
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
     \item{offsetY}{offsets}
     \item{meanCoef}{marginal mean for coefficients}
     \item{precisionCoef}{marginal variance for coefficients}
     \item{niter}{number of iterations, only the last sample is retained}
     \item{sdProposal}{Standard deviation of proposal distribution}
     \item{link}{link function, one of \code{logit} or \code{cloglog} for the complimentary log-log transform}
\item{..}{other parameters passed to the updater}
}
\details{
	\deqn{	Y_i \sim Bern(p_i)}
	\deqn{	link(p_i) = offset_i + X_i coef }
	\deqn{	coef \sim N(meanX, precisionX^(-1))}
} 
\value{
A vector of coefficients
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
	
	meanLink = X %*% coef + offest
	probs = exp(meanLink) / (1+exp(meanLink))
	
	y = rbinom(Nobservations, 1, probs)


	# generate posterior sample
	posteriorSample = updateBinaryGaussian(y, X, c(0,0), 
		offset, c(1,2), precisionCoef=c(0.01, 0.01), 
		niter=10, sdProposal=1, link="logit")

}
\keyword{}

