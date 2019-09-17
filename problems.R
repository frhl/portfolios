## unsorted 


calc.portfolio.pct <- function(n.assets = 2, correlation = 0.5, project.sd = 4, weight = "equal"){
  
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
  return(sig.p.x)
}


for (n in c(10, 100, 1000)){
  sd.pct = calc.portfolio.pct(n.assets = n, correlation = 0.5, project.sd = 4)
  print(paste0("projetcs=", n, ". cor = 0.5. sd=", round(sd.pct*100,2),"%"))
}


for (n in c(10, 100, 1000)){
  sd.pct = calc.portfolio.pct(n.assets = n, correlation = 0, project.sd = 4)
  print(paste0("projetcs=", n, ". cor = 0. sd=", round(sd.pct*100,2),"%"))
}


# Problem 4

## Expected Returns
ERa = 0.30*(0.50)+0.70*(-0.12); ERa # -2.5%
ERb = 0.30*(-0.25)+0.70*(0.05); ERb

## Variance
varRa = 0.30*((0.50-ERa)^2)+0.70*((-0.12-ERa)^2); varRa
varRb = 0.30*((-0.25-ERb)^2)+0.70*((0.05-ERb)^2); varRb

sdRa = sqrt(varRa)
sdRb = sqrt(varRb)

## Covariance and correlation
covar = 0.30*(0.50-ERa)*(-0.25-ERb)+0.70*(-0.12-ERa)*(0.05-ERb); covar
corr = covar/(sdRa*sdRb); corr

## Expected return 
ERpA = 0.5*0.50+0.5*(-0.25)
ERpD = 0.5*(-0.12)+0.5*(0.05) 
ERp = 0.3*ERpA + 0.7* ERpD

VarRp = 0.3*((ERpA-ERp)^2)+0.70*((ERpD-ERp)^2)
SdRP = sqrt(VarRp)


