\name{updateGaussianGaussian}
\alias{updateGaussianGaussian}
\title{Update Gaussian parameters with Gaussian observations}
\description{
	Generates a sample from the posterior of a Gaussian parameter 
	or random effect when observations are Gaussian.  Uses direct simulation fromthe conditional distribution.
    }
\usage{
	updateGaussianGaussian(y, X=1, offsetY,
		precisionY=NULL, varY=NULL,
		meanCoef, precisionCoef=NULL, varCoef = NULL, ...)
    }
\arguments{
     \item{y}{Vector of Gaussian distributed observations.}
     \item{X}{Matrix or vector of covariates, defaults to intercept only, rows are observations and columns are variables.}
     \item{offsetY}{offsets, defaults to zero}
     \item{precisionY}{Precision (scalar, vector or matrix) of observations conditional on coefficients.  
     	If scalar or vector, it is converted to a diagonal precision matrix.}
     \item{varY}{Can be specified in place of precisionY}
     \item{meanCoef}{marginal mean for coefficients}
     \item{precisionCoef}{marginal (or prior) precision for coefficients.}  
	\item{varCoef}{variance matrix of coeficients.  
		One of varCoef and precisionCoef must be specified.}
	\item{..}{other parameters passed to the updater, currently none are used.}
     }
\details{
	If the Y are independent, precisionY or varY is a vector or scaler and the model used is the following:
	\deqn{	Y_i ~ N( offset_i + X[i,] coef , varY_i}
	\deqn{	coef ~ N(meanCoef, varCoef)}
If the Y are dependent, varY is an N by N joint variance matrix, where N is the number of observations.  	
A simulation from [coef|Y] is returned, using
the conditional distribution for the multivariate normal as found at 
\code{en.wikipedia.org/wiki/Multivariate\_normal\_distribution}
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
		meanCoef=c(1,2), precisionCoef=c(0.01, 0.01), varCoef=NULL)
	posteriorSample	 
    }
\keyword{}

             
	Nobservations = 20
	X = cbind(1, 1:20)
	coef = c(0, 1)
	
	offset = seq(from=-5, len=Nobservations, by=1)
	
	meanObs = X %*% coef + offset
	
	precisionY <- 2
	
	y1 = rnorm(Nobservations, meanObs, precisionY^(-0.5))
	y2=  rnorm(Nobservations, meanObs+1, precisionY^(-0.5))
  y=cbind(y1,y2)
                 