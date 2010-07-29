updateGaussianGaussian<- function(y, X=1, offsetY, 
		precisionY=NULL, varY=NULL,
		meanCoef, precisionCoef=NULL, varCoef = NULL, XvarCoef=NULL,
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
			varCoef =chol2inv(chol(precisionCoef))	###get the inverse of precision matrix
		}
	}
	# compute X %*% varCoef
	# different methods depending on whether varCoef is diagonal matrix
	# stored as a vector
	if (is.null(XvarCoef)){
	if(is.vector(varCoef)) {
		XvarCoef = X %*% diag(varCoef, nrow=Ncoef,ncol=Ncoef) 
	} else {	
		XvarCoef =  X %*% varCoef   
	}	
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
	if(is.matrix(varYmarg)) {
  	precisionYmarg = chol2inv(chol(varYmarg))
  } else {
    precisionYmarg = 1/varYmarg                         
  }
                                                            
  

	varCoefXY = t(XvarCoef) %*% precisionYmarg

	CondVar <- varCoefXY %*% XvarCoef
	
  if (Ncoef == 1){	  
	postMean <-  meanCoef +  varCoefXY %*% (y-offsetY-meanCoef * X)
	} else {
	postMean <-  meanCoef +  varCoefXY %*% (y-offsetY-as.matrix(X) %*% as.matrix(meanCoef))
  }
	if(any(is.na(postMean))) {
	aa = list(postMean, meanCoef, varCoefXY, y, offsetY)
	 save(aa, file="thisdoesntwork.RData")
	 warning("eeks, stuff happening,patrick is panic")
  }
	
	if(is.vector(varCoef)){

		diag(CondVar) <- varCoef - diag(CondVar)
	} else {
		CondVar <- varCoef - CondVar
	}
	
	# generate random normals
	thechol = chol(CondVar) 
	as.vector( rnorm(Ncoef, postMean, 1) %*% thechol)	
}
