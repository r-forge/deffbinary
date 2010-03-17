updateBinaryGaussian <- function(y, X=NULL, coef, offsetY=0, meanCoef, precisionCoef, 
		niter=1, sdProposal=1, link=c("logit","cloglog"), ...) {

# create the link function
link<-link[1]
if(link=="logit") {
	linkFun = function(qq) qq/ (1+qq)
} else if(link=="cloglog"){
	linkFun = function(qq) 1-exp(-qq)
} else {
	warning("link not found: ",link)
}	

if(length(coef)==1)	{ # univariate normal
	# function to simulate from the proposal distribution
	simProp <- function() rnorm(1,mean=coef, sd=sdProposal)
	# function to calculate the log ratio of the 
	# priors of new and proposed coefficients
	priorDiff <- function() {
		coefDiff = proposedCoef - coef
		-0.5*(coefDiff*coefDiff*precisionCoef)
	}
} else {# multivariate
	if(!is.matrix(sdProposal)){ 
		# sdProposal is a vector of standard deviations 
			# rather than a variance matrix
		sdProposal = diag(sdProposal*sdProposal, length(coef))
	}
	if(!is.matrix(precisionCoef)){
		precisionCoef = diag(precisionCoef, length(coef))
	}
	simProp <- function() as.vector(rmvnorm(1,mean=coef, sigma=sdProposal))
	priorDiff <- function() {
		coefDiff = proposedCoef - coef
		-0.5*(coefDiff %*% precisionCoef %*% coefDiff)
	}
}

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

