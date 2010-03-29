updateGaussianWishart <- function(y, meanVar, shapeVar, meanY=NULL, 
		 ...) {
	
	if(!is.null(meanY))
    	y = y - meanY
	 
	if(is.matrix(y)) {
		Ny = dim(y)[1]
	} else {
		Ny = length(y)
	}
	
	sampleCov = var(y)*Ny
	
	result <- riwish(Ny + shapeVar, meanVar+sampleCov)	
	
	result
	
}