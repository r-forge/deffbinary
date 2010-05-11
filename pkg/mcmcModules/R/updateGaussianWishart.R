updateGaussianWishart <- function(y, meanVar, shapeVar, meanY=mean(y),
  returnPrecision=F, 
		 ...) {
	
	y = y - meanY
	 
	if(is.matrix(y)) {
		Ny = dim(y)[1]
	} else {
		Ny = length(y)
	}
	
	if(returnPrecision) {
	 result <- 1/sqrt(riwish(Ny + shapeVar, meanVar+sum(y^2)))		
	} else {
	 result <- riwish(Ny + shapeVar, meanVar+sum(y^2))	
	}
	
	result
	
}