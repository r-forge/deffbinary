updateGaussianGaussian<- function(y, X=1, offsetY, 
		precisionY=NULL, varY=NULL,
		meanCoef, precisionCoef=NULL, varCoef = NULL,
		...){
	if(is.matrix(X)){
		Ncoef =dim(X)[2]
	} else Ncoef = 1
	
	
	# create variance matrix of coefficients
	if(is.null(varCoef)){
		if(is.vector(precisionCoef)) {
			varCoef = 1/precisionCoef	
		} else {
			varCoef =solve(precisionCoef)	
		}
	}
	if(is.vector(varCoef)) {
		XvarCoef = X * matrix(varCoef, nrow=length(y),ncol=Ncoef) 
			
	} else {	
		XvarCoef =  X %*% varCoef   
	}	
	
	varYmarg = XvarCoef %*% t(X) 
	if(is.null(varY)){
		if(is.vector(precisionY)) {
			diag(varYmarg) = diag(varYmarg) + 1/precisionY	
		} else {
			varYmarg = varYmarg + solve(precisionY)	
		}
	} else {
		if(is.vector(varY)) {
			diag(varYmarg) = diag(varYmarg) + varY	
		} else {
			varYmarg = varYmarg + varY	
		}
		
	}
	
	precisionYmarg = chol2inv(chol(varYmarg))

	varCoefXY = t(varCoefX) %*% precisionYmarg

	CondVar <- varCoefXY %*% varCoefX
		
	postMean <-  meanCoef +  varCoefXY %*% (y-offsetY)
	
	
	if(is.vector(varCoef)){
		CondVar <-  - CondVar
		diag(CondVar) <- varCoef + diag(CondVar)
	} else {
		CondVar <- varCoef - CondVar
	}
	
	thechol = chol(CondVar) 
	
	as.vector( rnorm(Ncoef, postMean, 1) %*% thechol)	
}
