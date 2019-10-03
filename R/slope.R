#' @title Calculate the calibration slope
#' @description The calibration slope, which is calculated by regressing the observed outcome on the predicted probabilities.
#'
#' @param y Binomial response variable used to fit model
#' @param p Estimated probabilities from fit model
#'
#' @return the point estimate and 95\%ci of slope and intercept.


slope <- function(x,y){
  logitp <- log(x/(1-x))
  model <- glm(y~logitp,family = binomial(link = "logit"))

  df <- data.frame(c(NA,NA), c(NA,NA), c(NA,NA), c(NA,NA))

  df[,1] <- model$coefficients[1:2]
  df[,4] <- coef(summary(model))[,4]
  df[,2] <- confint(model)[,1]
  df[,3] <- confint(model)[,2]

  colnames(df) <- c("estimate","2.5%","97.5%","p-value")
  rownames(df) <- c("intercept","slope")
  return(df)
}
