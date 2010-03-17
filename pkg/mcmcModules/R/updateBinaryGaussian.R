updateBinaryGaussian <- function(y, X=NULL, coef, offsetY, meanCoef, precisionCoef, 
		niter=1, sdProposal=1, link=c("logit","cloglog"), ...) {
	
# simulate proposal	
if(length(coef)==1)	{ # univariate normal
	proposedCoef <- rnorm(1,mean=coef, sd=sdProposal)	
} else {# multivariate
	if(!is.matrix(sdProposal)){ 
		# sdProposal is a vector of standard deviations rather than a variance matrix
		sdProposal = diag(sdProposal, length(coef))
	}
	proposedCoef = as.vector(rmvnorm(1,mean=coef, sigma=sdProposal))
}

# calculate expenontials of (means on link scale)
expMeanLinkOld = exp(offsetY + as.matrix(X) %*% coef)
expMeanLinkNew = exp(offsetY + as.matrix(X) %*% proposedCoef)

link<-link[1]
if(link=="logit") {
	probsOld = expMeanLinkOld/ (1+expMeanLinkOld)
	probsNew = expMeanLinkNew/ (1+expMeanLinkNew)
} else if(link=="cloglog"){
	probsOld = 1-exp(-expMeanLinkOld)
	probsNew = 1-exp(-expMeanLinkNew)
} else {
	warning("link not found: ",link)
}
	# calculate old and new likelihoods
	onemy = 1-y
#	logLikOld = sum(y*probsOld + onemy*(1-probsOld))  
#	logLikNew = sum(y*probsNew + onemy*(1-probsNew))
#	ratioSafe <- exp(logLikNew - logLikOld)
	probsDiff = probsNew - probsOld
	ratio <- exp(sum(y*probsDiff - onemy*probsDiff))

	accept = runif(1)<ratio
	if(accept) result <- 
}

