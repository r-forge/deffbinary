updateGaussianWishart <- function(y, scaleVar, dfVar, meanY=0,
  returnPrecision=F, 
		 ...) {
	if(is.matrix(y) & is.vector(meanY)) {
	  meanY = matrix(meanY, byrow=T, nrow=nrow(y), ncol=ncol(y))
	}
	
	y = y - meanY
	 
	if(is.matrix(y)) {
		Ny = dim(y)[2]
	} else {
		Ny = length(y)
	}
	
	if(is.vector(y) & returnPrecision) {
	 result = 1/riwish(Ny + dfVar, scaleVar+sum(y^2))
  }	else if(is.vector(y) & !returnPrecision) { 
	 result <- riwish(Ny + dfVar, scaleVar+sum(y^2))		
	} else if(is.matrix(y) & !returnPrecision) {
	 result <- riwish(Ny + dfVar, scaleVar+t(y)%*%y)	
	} else if(is.matrix(y) & returnPrecision) {
	 result <- solve(riwish(Ny + dfVar, scaleVar+t(y)%*%y))	
	} else {
	   cat("arghhhhhh.....")
	}
	
	result
	
}

