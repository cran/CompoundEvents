
#'@title   Likelihood multiplication factor (LMF) or probability multiplication factor (PMF) of compound dry-hot events
#'@description   Compute joint probabilities of compound dry-hot events and the independent case.
#'@param mp Precipitation
#'@param mt Temperature
#'@param threp Threshold of precipitation (e.g., 50th percentile)
#'@param thret Threshold of temperature
#'@references Zscheischler, J. and S. I. Seneviratne (2017). Dependence of drivers affects risks associated with compound events. Science Advances, 3(6): e1700263.
#'@usage LMFDH(mp,mt,threp,thret)
#'@return Joint probability of DH divided by that of independent case
#'@examples
#'mp=matrix(rnorm(120,0,1),ncol=1)
#'mt=matrix(rnorm(120,0,1),ncol=1)
#'threp=20
#'thret=80
#'res<-LMFDH(mp,mt,threp,thret)
#'@export
LMFDH<-function(mp,mt,threp,thret)
{

  n <- length(mp)
  y <- matrix(data=0, nrow = n, ncol = 1)
  # Define the matrix
  y1 <- matrix(data=0, nrow = n, ncol = 1)
  y2 <- matrix(data=0, nrow = n, ncol = 1)

  # Spefify the Quantile (from percentile threp, threp)

  p0 <- quantile(mp,threp/100)
  t0 <- quantile(mt,thret/100)

  for (i in 1:n)
  {
    if (mp[i]<=p0)
    {
      y1[i]= 1
    }

    if (mt[i]>t0)
    {
      y2[i]= 1
    }
  }

  y <- y1*y2


# The empirical joint probability of the dependence case
  pd=sum(y)/n
# The joint probability of the independence case
# For the dry-hot case (e.g., 20th for P and 80th for T gives a 0.04 probability)
  pi=threp/100*(100-thret)/100
# The likelihood multiplication factor

  lmf=pd/pi

  res<-cbind(pd,pi,lmf)

  return(res)
}
