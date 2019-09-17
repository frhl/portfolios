#' @description calculates the the variance of a equally weighted portfolio
#' @param n.assets the number of assets
#' @param correlation the correlation between each asset
#' @param project.sd the standard deviation of each project
#' @param weight deprecated.
#' @return the standard deviation of the portfolio in decimals (percentage)

portfolio.sd <- function(n.assets = 2, correlation = 0.5, project.sd = 4, weight = "equal", verbose = T){
  
  # Convert to covariance
  covariance = correlation*project.sd^2
  
  # Setup varaince matrix matrix
  sigma.mat = matrix(rep(covariance, n.assets), ncol=n.assets, nrow=n.assets)
  
  # Setup diagonal as variances of each project
  diag(sigma.mat) = rep(project.sd, length(diag(sigma.mat)))^2
  
  # equal weight portfolio
  x.vec = rep(1,n.assets)/ncol(sigma.mat)
  
  # Matrix multiplication
  sig2 = t(x.vec)%*%sigma.mat%*%x.vec 
  sig.p.x = sqrt(sig2)
  
  if (verbose) write(paste0("<projetcs=", n.assets, ". cor=", correlation ,". sd=", round(as.numeric(sig.p.x)*100,2),"%>"),stdout())
  
  return(as.numeric(sig.p.x))
}
