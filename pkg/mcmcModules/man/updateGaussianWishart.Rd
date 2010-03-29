\name{updateGaussianWishart}
\alias{}
\title{Update Inverse Wishart distributed variances with Gaussian observations}
\description{
     }
\usage{
     }
\arguments{
       \item{y}{A vector of observations, or a matrix with each row being observations and each column a variable.
       }
     }
\details{
	\deqn{	Y_i ~ MVN( meany_i, precisionY^(-1)}
	\deqn{	precisionY ~ dwish(meanVar, shapeVar)}
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
	
	meanVar = diag(N)
	shapeVar = N+2
	
	meanY= matrix(meanY, Ny, N, byrow=T)

	updateGaussianWishart(y, meanVar, shapeVar, meanY)  

     }
\keyword{}

