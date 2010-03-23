updateGaussianGaussian<- function(y, X=1, offsetY, 
		precisionY=NULL, varY=NULL,
		meanCoef, precisionCoef=NULL, varCoef = NULL,
		...){
	if(is.matrix(X)){
		Ncoef =dim(X)[2]
	} else Ncoef = 1
	
	
	# create variance matrix of coefficients
	# or vector of variances if coefficients are independent
	if(is.null(varCoef)){
		if(is.vector(precisionCoef)) {
			varCoef = 1/precisionCoef	
		} else {
			varCoef =chol2inv(chol(precisionCoef))	
		}
	}
	# compute X %*% varCoef
	# different methods depending on whether varCoef is diagonal matrix
	# stored as a vector
	if(is.vector(varCoef)) {
		XvarCoef = X * matrix(varCoef, nrow=length(y),ncol=Ncoef) 
	} else {	
		XvarCoef =  X %*% varCoef   
	}	
	
	# marginal variance of coefficients
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

	# conditional variance matrix of coefficients
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
	
	# generate random normals
	thechol = chol(CondVar) 
	as.vector( rnorm(Ncoef, postMean, 1) %*% thechol)	
}
