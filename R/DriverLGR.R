#'@title   Assess potential driving factors of compound dry-hot events.
#'@description   Use the logistic regression model to establish relationships between
#' climate indices (e.g., ENSO) and occurrences of compound dry-hot events.
#'@param Y Occurrence of compound dry-hot events (0-1 binary variable)
#'@param CI Climate index  as the driving factor of compound events (e.g., ENSO)
#'@import stats
#'@references  Hao, Z. et al. (2019). A monitoring and prediction system for compound dry and hot events. Environ. Res. Lett., 14:114034.
#'@usage DriverLGR(Y,CI)
#'@return slope parameter and associated p-value
#'@examples
#' CI=c(-0.7,-1.2,1.3,0.7,-0.6,1.1,-0.5,0.8,0.5,-0.5,1.6,-1.8,-0.5,-1.4,-0.1,2.2,-0.7,-1.1, 0.6, -1.7)
#' Y=c(0,0,1,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0 )
#' res<-DriverLGR(Y,CI)
#' @export

DriverLGR<- function(Y,CI)
{
  X=CI
  Y=Y
  # Built the logistic regression model

  logis.fit=glm(formula =Y ~ X, family = "binomial")

  # Assess the significance of the regression coefficient

  summary(logis.fit)

  # Get the parameter alpha and beta

  par1<-logis.fit$coefficients[1]
  par2<-logis.fit$coefficients[2]

  # Get the significance of the parameter alpha and beta
  p1<-summary(logis.fit)$coefficients[1,4]
  p2<-summary(logis.fit)$coefficients[2,4]
  # Specify the value of the climate index (X)

  x0<-matrix(data=seq(min(X)-0.5,max(X)+0.5,0.05),ncol=1)

  # Compute the occurrence probability P(Y=1)

  py=1/(1+exp(-(par1+ par2*x0)))


  z=cbind(par2,p2)
  colnames(z)<-c("Slope parameter","P-value")
  return(z)
}

