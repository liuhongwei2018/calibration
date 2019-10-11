#' @title Platt scaling probability calibration
#' @description Performs an platt scaling calibration of posterior probability to minimize log loss.
#'
#' @param x Estimated probabilities from fit model 
#' @param y Binomial response variable used to fit model
#'
#' @return a vector of calibrated probabilities
#' @export


platt_calibration <- function(x, y){
  # Bayesian priors (see Platt end of section 2.2)
  # x and y all must be atomic vector
  F <- x
  prior0 <- sum(y <= 0)
  prior1 <- length(y) - prior0
  T <- vector(length = length(y))
  T[y > 0] <- (prior1 + 1) / (prior1 + 2)
  T[y <= 0] <- 1 / (prior0 + 2)
  
  # From Platt (beginning of Section 2.2)
  fn_objective <- function(AB){
    P <- 1 / (1 + exp(AB[1] * F + AB[2]))
    loss <- -sum(T * log(P) + (1 - T) * log(1 - P))
    return(loss)
  }
  
  fn_gradient <- function(AB){
    E <- exp(AB[1] * F + AB[2])
    P_ = 1 / (1 + E)
    TEP_minus_T1P <- P_ * (T * E - (1 - T))
    dA <- sum(TEP_minus_T1P * F)
    dB <- sum(TEP_minus_T1P)
    return(c(dA, dB))
  }
  ABO <- c(0, log((prior0+1)/(prior1+2)))
  AB_ <- optim(par = c(0,0.5), fn = fn_objective, gr = fn_gradient, method = "BFGS")
  
  return(1 / (1 + exp(AB_$par[1] * x + AB_$par[2])))
}
