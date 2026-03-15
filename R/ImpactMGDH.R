#'@title     Impacts of concurrent droughts and hot extremes
#'@description    Use meta-Gaussian model to construct  conditional
#'distributions of the standardized impact variable (SII)  given SPI and STI
#'(represented by random variables of Z, X, and Y).
#'The output include (1) the conditional mean and variance of STI given SPI
#'and STI (e.g., X0=-1.2 and y0=1.2) (2)The conditional probability of
#' P(SII<=z0|SPI<=x0, STI>y0),  or lower GPP given dry-hot conditions
#'@param mp monthly precipitation of a month (e.g., August)
#'@param mt monthly temperature of a month (e.g., August)
#'@param mi Impact variable of a month (e.g., GPP, Crop yield)
#'@param x0 Initial condition of  SPI (e.g., -1.2, dry conditions)
#'@param y0 Initial condition of  STI (e.g., 1.2, hot conditions)
#'@param z0 Initial condition of standardized impact indicator(e.g.,0, low GPP)
#'@references Feng S.,Hao Z., et al. (2019). Probabilistic evaluation of the
#'           impact of compound dry-hot events on global maize yields.
#'           Sci. Total. Environ., 689: 1228-1234.
#'@references Hao Z., et al. (2018). A multivariate approach for statistical
#' assessments of compound extremes. J. Hydrol., 565: 87-94.
#'@usage ImpactMGDH(mp,mt,mi,x0,y0,z0=NULL)
#'@return (1) Conditional mean/variance  at SPI=x0, STI=y0, here z0=NULL
#' (2)conditional probability of lower SII given lower SPI and higher STI
#'or P(SZI<=z0|SPI<=x0, STI>y0), here z0 should be specified
#'@examples
#'mp=matrix(rnorm(60,0,1),ncol=1) # precipitation of a month (60 years)
#'mt=matrix(rnorm(60,0,1),ncol=1) # temperature of a month  (60 years)
#'mi=matrix(rnorm(60,0,1),ncol=1)   # impact of a month (60 years)
#'imp1<-ImpactMGDH(mp,mt,mi,-1.2,1.2,NULL) # imp1$mu, imp1$sig
#'imp2<-ImpactMGDH(mp,mt,mi,-1.2,1.2,0)# imp2$Condprob
#'@export
ImpactMGDH<-function(mp,mt,mi,x0,y0,z0=NULL)
{
  n<-length(mp)

  if (length(mp) != length(mt) || length(mp) != length(mi))

  {stop("Input vectors must have same length")}

  if (length(x0) != 1 || length(y0) != 1) {
    stop("x0 and y0 must be scalars (length 1)")
  }

  # Inputs
  SP<-matrix(data=mp, ncol = 1)
  ST<-matrix(data=mt, ncol = 1)
  SI<-matrix(data=mi, ncol = 1)

  # Transform all variables into standardized variables

  EP1<-ecdf(SP)  # Get the empirical probability
  P1<-EP1(SP)*n/(n+1)  # Use the Weibull plotting position
  SPI<-qnorm(P1,0,1)

  EP2<-ecdf(ST)
  P2<-EP2(ST)*n/(n+1)
  STI<-qnorm(P2,0,1)

  EP3<-ecdf(SI)
  P3<-EP3(SI)*n/(n+1)
  SII<-qnorm(P3,0,1)

  # (1) Compute the conditional mean and variance given [x0,y0]
  #  Build the meta-Gaussian model
  u0<-cbind(x0,y0)
  xd<-cbind(SPI,STI)
  yd<-SII

  if (is.null(z0))
  #---------- Get the conditional mean and variance----------
{
  nd<-3

    cv<-cov(cbind(xd,yd))

    sig12<-cv[1:(nd-1),nd]
    sig21<-cv[nd,1:(nd-1)]

    sig12<-matrix(data=sig12, nrow=2,ncol = 1)
    sig21<-matrix(data=sig21, nrow=1,ncol = 2)

    mx<-cbind(mean(xd[,1]),mean(xd[,2]))
    my<-mean(yd)

    mu<-my+sig21%*%solve(cov(xd))%*%t((u0-mx))  # "Conditional mean"
    sig<-cv[nd,nd]-sig21%*%solve(cov(xd))%*%t(sig21)# Conditional Variance

    return(list(mu=mu,sig=sig))

}
  else {

    # ------------(2)Compute the conditional probability of P(Z<=z0|X<=x0,Y>y0)
    # numerator: Fxz(x0,z0)-Fxyz(x0,y0,z0)
    # denominator: Fx(x0)-Fxy(x0,y0)
    # Get the joint probability based on normal distribution

    # construct the dataframe

    df <- data.frame(SPI, STI, SII)  # X,Y,Z

    # compute the covariance matrix

    Sigma <- cov(df)

    # compute the joint probability of numerator
    # P_XZ <- mvtnorm::pmvnorm(lower = c(-Inf, -Inf), upper = c(x0, z0),
    #sigma = Sigma[c(1,3), c(1,3)])
    #  P_XYZ <- mvtnorm::pmvnorm(lower = c(-Inf, -Inf, -Inf),
    #upper = c(x0, y0, z0), sigma = Sigma)
    # numerator <- as.numeric(P_XZ - P_XYZ)

    # compute the joint probability in the denominator
    # P_X <- pnorm(x0)   # x0 is already normalized
    # P_XY <- mvtnorm::pmvnorm(lower = c(-Inf, -Inf),
    #upper = c(x0, y0), sigma = Sigma[1:2, 1:2])
    # denominator <- as.numeric(P_X - P_XY)

    # 条件概率
    pnumerator<-mvtnorm::pmvnorm(lower = c(-Inf, y0, -Inf),
                                 upper = c(x0, Inf, z0),
                                 sigma = Sigma)

    pdenominator<-mvtnorm::pmvnorm(lower = c(-Inf, y0),
                                   upper = c(x0, Inf),
                                   sigma = Sigma[1:2, 1:2])

    condprob<-pnumerator/pdenominator


  return(list(condprob=condprob))
  }
}
