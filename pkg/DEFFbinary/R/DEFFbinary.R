DEFFbinary = function(
    logitMean, sigma, 
    NperGroup=NULL,Ngroups=NULL,  Nsim = 1000) {

  # simulate some normals to find the moments of the logit-normal  
  theSim = rnorm(Nsim, logitMean, sigma)
  # transform to logit-normals
  theSim = exp(theSim) / (1 + exp(theSim))
  # compute the moments
  moment1 = mean(theSim)
  moment1sq = moment1^2
  moment2 = mean(theSim^2)
  
 # compute the ICC 
 result = c(ICC = (moment2 - moment1sq) / (moment1 - moment1sq) )
     
 # if NperGroup is provided, compute the design effect    
 if(!is.null(NperGroup))           
 	result["DEFF"] = 1+(NperGroup-1)*result["ICC"]


# if Ngroups is specified, compute the standard error
if(!is.null(Ngroups)) {
	totalSize = NperGroup*Ngroups
	result["SE"] = 1/sqrt( (totalSize/result["DEFF"]) * moment1*(1-moment1))	
}

# save the moments as attributes  
 attributes(result)$moments = c(first=moment1, second=moment2)

result
}