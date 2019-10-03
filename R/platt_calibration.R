#' @title Platt scaling probability calibration
#' @description Performs an platt scaling calibration of posterior probability to minimize log loss.
#'
#' @param y                Binomial response variable used to fit model
#' @param p                Estimated probabilities from fit model
#'
#' @return a vector of calibrated probabilities





platt_calibration <- function(p,y){
  df <- as.data.frame(cbind(p,y))
  model_log<-glm(y~p,data = df,family = binomial)
  result_platt<-predict(model_log,df[-2],type = "response")
  return(result_platt)
}
