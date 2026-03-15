#'@title   Assess potential driving factors of compound dry-hot events.
#'@description   Use logistic regression model to establish relationship between
#' climate indices (e.g., ENSO) and occurrences of compound dry-hot events.
#'@param Y Occurrence of compound dry-hot events (0-1 binary variable)
#'@param X Climate index  as the driving factor of compound events (e.g., ENSO)
#'@import stats
#'@references  Hao, Z. et al. (2019). A monitoring and prediction system for
#'             compound dry and hot events. Environ. Res. Lett., 14:114034.
#'@usage DriverLogit(Y,X)
#'@return slope parameter and associated p-value
#'@examples
#' X=c(-0.7,-1.2,1.3,0.7,-0.6,1.1,-0.5,0.8,0.5,
#' -0.5,1.6,-1.8,-0.5,-1.4,-0.1,2.2,-0.7,-1.1, 0.6, -1.7) # climate indices
#' Y=c(0,0,1,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0 )   # dry-hot occurrences
#' div<-DriverLogit(Y,X)  # div$slope and div$pvalue
#' @export

DriverLogit<- function(Y,X)
{

  # Built the logistic regression model Y=a+bX

  fit<-glm(formula =Y ~ X, family = "binomial")

  # Assess the significance of the regression coefficient

  # Get the parameter alpha and beta

  inter<-coef(fit)[1] # intercept
  slope<-coef(fit)[2] # slope

  # Get the significance of the parameter alpha and beta
  p1<-summary(fit)$coefficients[1,4]
  p2<-summary(fit)$coefficients[2,4]
  # Specify the value of the climate index (X)

  # x0<-matrix(data=seq(min(X)-0.5,max(X)+0.5,0.05),ncol=1)

  # Compute the occurrence probability P(Y=1)

  # py<-1/(1+exp(-(par1+ par2*x0)))

  return(data.frame(slope=slope,pvalue=p2))
}

