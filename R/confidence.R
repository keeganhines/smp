#' Estimate parameter confidence from binomial data.
#'
#' Use methods described in the accompanying paper in order to calculate parameter confidence when analyzing binomially distributed data. Can be used for frequentist esimate (eqn [4] in main paper) or Bayesian estimate (eqn [9] in main paper). 
#'
#' @param input.data input data drawn from binomial distribution
#' @param Bayes flag to specify whether Bayesian or frequentist methodology is to be used
#' @param Iterations number of samples to use for Monte Carlo integration for Bayesian method
#'
#' @export
confidence<-function(input.data,Bayes=TRUE,Iterations=100000) {
							      
  k.hat<-max(input.data)
  A<-sum(input.data)
  B.k.hat<-sum((k.hat)-input.data)
  B.k.hat.plus.one<-sum((k.hat+1)-input.data)
  
  if (!Bayes){
    
    theta.hat.k.hat.plus.one<-estimate.theta(input.data,n=(k.hat+1), v=FALSE)
    
    alpha<-1-(1-theta.hat.k.hat.plus.one^(k.hat+1))^length(input.data)	
    
    paste(c("Number of subunits is estimated to be"),c(k.hat),c('with confidence of'),c(round(alpha,4)),c('.'),sep=' ')
    }else{
	theta.samples<-rbeta(Iterations,A,B.k.hat.plus.one)
	alpha<-1 - mean((1-theta.samples^(k.hat+1))^length(input.data))					
	paste(c("Number of subunits is estimated to be"),c(k.hat),c('with confidence of'),c(round((alpha),4)),sep=' ')
	}									
}