
#' @description calculate the expected returns of a portfolio
#' @param expected.returns a vector of expected returns
#' @param weights either 'equal' or a vector of weights that has
#' the same length as expected.returns and sum up to one.
#' @returns the expected returns of the portfolio in percentage

porfolio.er <- function(expected.returns, weights = "equal"){
  n.assets = length(expected.returns)
  if (is.null(weights) || weights == "equal"){
    weights = rep(1,n.assets)/n.assets
  } 
  stopifnot(sum(weights) == 1 )
  stopifnot(length(weights) == n.assets)
  return(sum(expected.returns*weights))
}

calc.porfolio.e(10.79)
calc.portfolio.sd(n.assets = 8, correlation = 0, project.sd = 0.3134)













