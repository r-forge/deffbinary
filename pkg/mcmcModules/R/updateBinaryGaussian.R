updateBinaryGaussian <- function(y, X, coef, offsetY=0, precisionCoef, 
		niter=1, sdProposal=1, link=c("logit","cloglog"), ...) {	
	
# function to calculate logs of probabilities
	# and logs of one minus probabilities, depends on the link function
link<-link[1]
if(link=="logit") {
	logProbs = function(qq) {
		log1pexpqq = -log(1+exp(qq))
		list(probs = qq + log1pexpqq,
			onemProbs =  log1pexpqq)
	}
} else if(link=="cloglog"){
	logProbs = function(qq) {
		minusexpqq = -exp(qq)
		list(probs = log(1-exp(minusexpqq)),
			onemProbs =  minusexpqq)
	}
} else {
	warning("link not found: ",link)
}	


# function to simulate from the proposal, depends on whether proposal is independent
if(is.vector(sdProposal)){
	# function to simulate from the proposal distribution
	simProp <- function() rnorm(Ncoef,mean=coef, sd=sdProposal)
} else {
	# assume sdProposal is a variance matrix
	simProp <- function() as.vector(rmvnorm(1,mean=coef, sigma=sdProposal))
}

# calculate the difference in log probabilities of prior for new and old coefficients
					# depends on whether prior is independent or not
if(is.vector(precisionCoef)){
	priorDiff <- function() {
		coefDiff = proposedCoef - coef
		-0.5*sum(coefDiff^2*precisionCoef)
	}
} else{
	priorDiff <- function() {
		-0.5*(proposedCoef %*% precisionCoef %*% proposedCoef  - coef %*% precisionCoef %*% coef)     
	}
}

acceptRatio<-0
f=3    #	f:degree of freedom 

for(Diter in 1:niter){	
	
  
	# simulate proposal	
	proposedCoef <- simProp()

	
	# calculate old and new probabilities
	logProbsOld <- logProbs(offsetY + as.matrix(X) %*% coef) 
	logProbsNew <- logProbs(offsetY + as.matrix(X) %*% proposedCoef) 

	# calculate old and new likelihoods
		# log likelihood is y*logProbsOld + (1-y)*(1-logProbsOld)
	onemy = 1-y

	ratio <- exp(sum(
					y*(logProbsNew$probs - logProbsOld$probs) - 
							onemy*(logProbsNew$onemProbs - logProbsOld$onemProbs)
					) + 
			priorDiff())

	accept = runif(1)<ratio
	
	if(accept) {
	  coefold <- coef
		coef <- proposedCoef 
		acceptRatio <- acceptRatio + accept
	}
} # end iteration loop

	#posterior distribution of the variance-covariance matrix 
   
	precisionCoef <- riwish(f+1, precisionCoef+cov((rbind(coef, proposedCoef))
))	
	attributes(coef)$mcmc <- c(acceptRatio=acceptRatio/niter,
				niter=niter)
	attributes(coef)$sdProposal = sdProposal	
	
	coef
}

