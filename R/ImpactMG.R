#'@title     Impacts  under droughts and hot extremes
#'@description    Use the meta-Gaussian model to construct  conditional distributions of
#' the impact variable (Y)  given drought and hot conditions P(Y|PRC,TEM).
#'@param PRC Precipitation or drought indicator corresponding to the impact variable Y
#'@param TEM Temperature or heat indicator  corresponding to the impact variable Y
#'@param Y Impact variable (e.g., Crop yield)
#'@param u0 Initial condition of (PRC,TEM)
#'@references Feng, S. et al. (2019). Probabilistic evaluation of the impact of compound dry-hot events on global maize yields. Sci. Total. Environ., 689: 1228-1234.
#'@references Hao, Z.  et al. (2018). A multivariate approach for statistical assessments of compound extremes. J. Hydrol., 565: 87-94.
#'@usage ImpactMG(PRC,TEM,Y,u0)
#'@return A vector of conditional mean and variance evaluated at u0
#'@examples
#'PRC=matrix(rnorm(60,0,1),ncol=1)
#'TEM=matrix(rnorm(60,0,1),ncol=1)
#'Y=matrix(rnorm(60,0,1),ncol=1)
#'u0=c(-1.2,1.2) # Speficify the compound dry-hot condition
#'ImpactMG(PRC,TEM,Y,u0)
#'@export
ImpactMG<-function(PRC,TEM,Y,u0)
{


  # Inputs
  AP=matrix(data=PRC, ncol = 1)
  AT=matrix(data=TEM, ncol = 1)
  AY=matrix(data=Y, ncol = 1)

  # Transform all variables into standardized variables

  n=length(AP);

  EP1<-ecdf(AP)  # Get the empirical probability
  P1=EP1(AP)*n/(n+1)  # Use the Weibull plotting position
  SPI<-qnorm(P1,0,1)

  EP2<-ecdf(AT)
  P2=EP2(AT)*n/(n+1)
  STI<-qnorm(P2,0,1)

  EP3<-ecdf(AY)
  P3=EP3(AY)*n/(n+1)
  SCI<-qnorm(P3,0,1)

  # Build the meta-Gaussian model

  xd=cbind(SPI,STI)
  yd=SCI;



  # Get the conditional mean and variance

    cv<-cov(cbind(xd,yd))

    n=3;

    sig12<-cv[1:n-1,n];
    sig21<-cv[n,1:n-1];

    sig12=matrix(data=sig12, nrow=2,ncol = 1)
    sig21=matrix(data=sig21, nrow=1,ncol = 2)


    mx=cbind(mean(xd[,1]),mean(xd[,2]))
    my=mean(yd)


    mu<-my+sig21%*%solve(cov(xd))%*%t((u0-mx))

    sig<-cv[n,n]-sig21%*%solve(cov(xd))%*%t(sig21)



  res<-cbind(mu,sig)

  colnames(res)<-c("Conditional mean","Conditional Variance")

  return (res)
}
