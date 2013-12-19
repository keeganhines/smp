#' Calculate best estimate of theta.
#'
#' Returns the optimal point estimate of theta for the input data set. By default, theta is optimized with n set to the largest value of observed in the data. 
#'
#' @param in.data input data drawn from binomial distribution
#' @param n number of subunits, default is max(in.data)
#'
#' @export
#'
estimate.theta<-function(in.data,n=max(in.data),v=TRUE){
  if(n< max(in.data)){
	paste(c("Error, n must be at least as large as"),c(max(in.data)))
  }else{					
	a<-sum(in.data)
	b<-sum(n-in.data)
	theta_hat<-(a-1)/(a+b-2)
	if (!v){ # return value, or output a string
	  theta_hat										    
	  }else {
	  paste(c("For"),c(n),c("subunits, best estimate of theta is"),c(round(theta_hat,2)),sep=' ')			     }
	}
}
