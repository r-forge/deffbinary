updateGaussianGaussian<- function(y, X=1, coef, offsetY,precisionY, 
		meanCoef, precisionCoef, 
		niter=1, sdProposal=1 , ...){

	if(!is.matrix(sdProposal)){ 
		# sdProposal is a vector of standard deviations 
		# rather than a variance matrix
		sdProposal = diag(sdProposal*sdProposal, length(coef))
	}
	# funtion to simulate from the proposal distribution
	simProp <- function() as.vector(rmvnorm(1,mean=coef, sigma=sdProposal))

	if(is.matrix(precisionCoef)){
		priorDiff <- function() {
			coefDiff = proposedCoef - coef
			-0.5*(coefDiff %*% precisionCoef %*% coefDiff)
		}
	} else {
		# preceisionCoef is vector or scalar
		priorDiff <- function() {
			coefDiff = proposedCoef - coef
			sum((-0.5)*precisionCoef*coefDiff*coefDiff)
		}
	}	
	
	
	# conditional variance matrix, doesnt change with iterations
	varCoefCondnY <- 
	
	
	acceptRatio<-0
	
	for(Diter in 1:niter){	
		
		# simulate proposal	
		proposedCoef <- simProp()
		
		# calculate old and new probabilities
		probsOld <- linkFun(exp(offsetY + as.matrix(X) %*% coef) )
		probsNew <- linkFun(exp(offsetY + as.matrix(X) %*% proposedCoef) )
		
		# calculate old and new likelihoods
		onemy = 1-y
		probsDiff = probsNew - probsOld
		ratio <- exp(sum(y*probsDiff - onemy*probsDiff) + priorDiff())
		
		accept = runif(1)<ratio
		
		if(accept) {
			coef <- proposedCoef 
			acceptRatio <- acceptRatio + accept
		}
	} # end iteration loop
	
	attributes(coef)$mcmc <- c(acceptRatio=acceptRatio/niter,
			niter=niter)
	attributes(coef)$sdProposal = sdProposal	
	
	coef
	

}
