#' Calculate gamma
#'
#' For a given dataset, calculate the probability that the largest observations are artifactual, as described by eqn [10] in the main paper. 
#'
#' @param input.data the input data
#' @param Bayes flag to specify whether Bayesian or frequentist method is to be used
#'
#' @export
#'

gamma<-function(input.data,Bayes=TRUE){	
	
  k.hat<-max(input.data)
  theta.hat<-estimate.theta(input.data,n=k.hat,v=FALSE)
  A<-sum(input.data)
  B.k.hat<-sum((k.hat)-input.data) #compute parameters of conditional posterior
	
  K<-sum(input.data==k.hat) #how many instances of k.hat
		
  if (!Bayes){ # Non-Bayesian method	   
    prob.dens<-c(0)
    for (i in seq(1,K)){ #do this in a for loop because I'm not feeling creative at the moment
       prob.dens<-prob.dens + dbinom(i,length(input.data),(theta.hat^k.hat)) 	    # sum up probability mass up to K from sampling distr			       			    
	result <-prob.dens #return prob of observing K or fewer events of size k.hat
	}							
  }else{ # Bayesian method
    theta.samples<-rbeta(1000,A,B.k.hat) #samples from conditional posterior
    all.gamma<-c()
    for (j in seq(1,length(theta.samples))){  # Loop over theta samples				    
      prob.dens<-c(0)
	  for (i in seq(1,K)){ 
	      prob.dens<-prob.dens + dbinom(i,length(input.data),(theta.samples[j]^k.hat)) #compute total prob density for jth sample of theta
       			    }
        all.gamma[j]<-prob.dens #collect prob density estimate for all theta samples	    
        }
        result<-mean(all.gamma) # there's your answer
        
    }																							      
    return(result)
}