#'@title   Occurrence of compound dry-hot events
#'@description  Extract compound dry-hot (DH) occurrences based on thresholds of precipitation and temperature. The binary variable of the DH (or dry-warm) event can be obtained.
#'
#'@param mp Precipitation
#'@param mt Temperature
#'@param threp Threshold of precipitation (e.g., 20th percentile)
#'@param thret Threshold of temperature (e.g., 80th percentile)
#'@usage GetDH(mp,mt,threp,thret)
#'@return The occurrence of compound dry-hot events (0-1 binary variable)
#'@references Hao, Z. et al. (2018). A multivariate approach for  statistical assessments of compound extremes. J. Hydrol., 565: 87-94.
#'@references  Hao, Z. et al. (2019). A monitoring and prediction system for compound dry and hot events. Environ. Res. Lett., 14:114034.
#'@examples
#'mp=matrix(rnorm(120,0,1),ncol=1)
#'mt=matrix(rnorm(120,0,1),ncol=1)
#'threp=20
#'thret=80
#'DH<-GetDH(mp,mt,threp,thret)
#'@export

GetDH<- function(mp,mt,threp,thret)
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
    if (mp[i]<= p0)
    {
      y1[i]= 1
    }

    if (mt[i]> t0)
    {
      y2[i]= 1
    }

  }

  y <- y1*y2

  return (y)

  }
