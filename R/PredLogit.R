#'@title   Prediction of compound event occurrences
#'@description Fit the logistic regression model (LGR) based on occurrences of
#'compound events (Y) and climate index (CI).The output is predicted probability
#'of compound event occurrence for the given climate index value CI0 or the
#'prediction performance (using AUC) and predicted values based on LOOCV
#'@param X Climate index (CI) as the driving factor (e.g., ENSO) with some lags
#'@param Y Occurrences of compound events (0-1 binary variable) (L lead time)
#'@param x0 Specified CI value based on which the prediction is issued
#'@param type type is the "performance" or "prediction"
#'@import stats
#'@usage  PredLogit(Y,X,type,x0=NULL)
#'@return (1) Probability of occurrences estimated at x0 if type="prediction",
#' shouLd input x0 (2) AUC values and predictions LOOCV if type="performance",
#'x0 is not needed
#'@references Hao, Z. et al. (2019). Statistical prediction of the severity
#'       of compound dry-hot events based on ENSO . J. Hydrol., 572: 243-250.
#'@examples
#' Y=as.matrix(c(0,0,1,1,0,0,0,0,0,0,1,0,1,0,0,1,0))
#' X=c(-0.7,-1.2,1.3,0.7,-0.6,1.1,-0.5,0.8,0.5,-0.5,
#'    1.6,-1.8,-0.5,-1.4,-0.1,2.2,-0.7) #  dry-hot (or other) 0-1 events
#' pred1<-PredLogit(Y,X,type="prediction",2) #pred1$predx0
#' pred2<-PredLogit(Y,X,type="performance") #pred2$aucvalue,pred2$predictions
#' @export

PredLogit<-function(Y,X,type=c("performance", "prediction"),x0=NULL)
{

  Y <- as.vector(Y)
  X <- as.vector(X)


  type <- match.arg(type)
  if (type == "performance") {
    if (!is.null(x0)) {
      warning("Argument 'x0' is ignored when type = 'performance'")
      x0 <- NULL
    }
  } else { # type == "prediction"
    if (is.null(x0)) {
      stop(" For type = 'prediction', argument 'x0' must be provided..")

    }
  }


  Y_data<-Y
  X_data<-X
  # Y is the predictand, X is the predictor
  # type is the "performance" or "prediction"
  # x0 is the specified value of X, for the type "prediction"

  n <- length(X_data)


  if (type == "performance") {
    # LOOCV to compute AUC
    #library(pROC)
    predictions <- numeric(n)


    for (i in 1:n) {
      #  LOOCV: remove the i sample
      train_X <- X_data[-i]
      train_Y <- Y_data[-i]

      # Train the logistic model
      model <- glm(train_Y ~ train_X,
                   family = binomial(link = "logit"))

      # Predict the left-out sample
      predictions[i] <- predict(model,
                                newdata = data.frame(train_X = X_data[i]),
                                type = "response")
    }

  auc_value <-pROC::auc(pROC::roc(Y_data, predictions))

  aucvalue<-as.numeric(auc_value)



  return(list(aucvalue = aucvalue,predictions=predictions))

  }

  else if (type == "prediction") {


  # Use all data to train model
  model <- glm(Y_data ~ X_data,
               family = binomial(link = "logit"))

  # Prediction at the specified value
  predictions <- predict(model,
                         newdata = data.frame(X_data = x0),
                         type = "response")


  # return the prediction value

  predx0=as.numeric(predictions)

  return(list(predx0 = predx0))
}

}



  # # Built the logistic regression model
  #
  # model=glm(formula =Y ~ X, family = "binomial")
  #
  # # get the predicted value at x0
  #
  # py <- predict(model,  x0, type = "response")
  #
  # # Get the parameter alpha and beta
  #
  # #par1<-as.numeric(logis.fit$coefficients[1])
  # #par2<-as.numeric(logis.fit$coefficients[2])
  #
  # # Specify the climate index (with certain lag L)
  #
  #
  # #py=1/(1+exp(-(par1+par2*CI0)))
  #
  # return(py)





