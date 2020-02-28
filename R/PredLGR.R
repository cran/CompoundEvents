#'@title   Prediction of compound event occurrences
#'@description Fit the logistic regression model (LGR) based on occurrences of compound events (Y) and climate index (CI).The output is the predicted probability of compound event occurrence for the given climate index value CI0
#'@param CI Climate index (CI) as the driving factor of compound events (e.g., ENSO)
#'@param Y Occurrences of compound dry-hot events (0-1 binary variable) (L lead time)
#'@param CI0 Specified CI value based on which the prediction is issued
#'@import stats
#'@usage  PredLGR(Y,CI,CI0)
#'@return Probability of occurrences estimated at CI0
#'@references Hao, Z. et al. (2019). Statistical prediction of the severity of compound dry-hot events based on ENSO . J. Hydrol., 572: 243-250.
#'@examples
#' CI=c(-0.7,-1.2,1.3,0.7,-0.6,1.1,-0.5,0.8,0.5,-0.5,1.6,-1.8,-0.5,-1.4,-0.1,2.2,-0.7,-1.1, 0.6, -1.7)
#' Y=c(0,0,1,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0 )
#' PredLGR(Y,CI,2)
#' @export

PredLGR<-function(Y,CI,CI0)
{
  X=CI
  Y=Y
  # Built the logistic regression model

  logis.fit=glm(formula =Y ~ X, family = "binomial")


  # Get the parameter alpha and beta

  par1<-as.numeric(logis.fit$coefficients[1])
  par2<-as.numeric(logis.fit$coefficients[2])

  # Specify the climate index (with certain lag L)


  py=1/(1+exp(-(par1+par2*CI0)))

  return(py)

}



