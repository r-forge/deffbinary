\name{updateGaussianWishart}
\alias{}
\title{Update Inverse Wishart distributed variances with Gaussian observations}
\description{Returns a posterior sample for variance (or precision) matrix for multivariate normal data with an inverse Wishart prior on the variance matrix.
     }
\usage{
     }
\arguments{
       \item{y}{A vector of observations, or a matrix with each row being observations and each column a variable.
       }
       \item{scaleVar}{scale parameter (a matrix) for prior's inverse Wishart distribution}
       \item{dfVar}{degrees of freedom (scalar) for prior's inverse Wishart distribution}
       \item{meanY}{Mean of y}
       \item{returnPrecision}{return a sample from the precision instead of variance, defaults to F}
     }
\details{
	\deqn{	Y_i ~ MVN( meany_i, varY}
	\deqn{	varY ~ iwish(dfVar , scaleVar)}
}     
\seealso{
     \code{\link{}}.
     }
\examples{
	
	N=2
	Ny = 10
	meanY = rep(0, N)
	varY = matrix(1, N, N)
	diag(varY) = 2
	
	y = rmvnorm(Ny, meanY, varY)
	
	priorMeanVar = diag(1:N)
	
	dfVar = N+2
	scaleVar = priorMeanVar *(dfVar - N - 1)
	
	meanY= matrix(meanY, Ny, N, byrow=T)

	updateGaussianWishart(y, scaleVar, dfVar, meanY)  

     }
\keyword{}

