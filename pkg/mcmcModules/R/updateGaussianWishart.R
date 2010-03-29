updateGaussianWishart <- function(y, var, meanVar, shapeVar, meany=NULL, 
		 ...) {
	
	if(!is.null(meany))
    	y = y - meany
	 
	y = as.matrix(y)
	Nvar = dim(y)[2]
	
	sampleCov = var(y)*(dim(y)[1])
	
	precisionCoef <- riwish(Nvar + shapevar, meanVar+sampleCov)	
	
	
	
}