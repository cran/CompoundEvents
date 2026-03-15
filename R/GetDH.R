#'@title   Occurrence of compound dry-hot events
#'@description  Get compound dry-hot (DH) events based on thresholds
#' precipitation and temperature, resulting in a 0-1The binary variable
#'@param mp Precipitation
#'@param mt Temperature
#'@param threp Threshold of precipitation (e.g., 20th percentile)
#'@param thret Threshold of temperature (e.g., 80th percentile)
#'@usage GetDH(mp,mt,threp,thret)
#'@return The occurrence of compound dry-hot events (0-1 binary variable)
#'@references Hao, Z. et al. (2018). A multivariate approach for  statistical
#'assessments of compound extremes. J. Hydrol., 565: 87-94.
#'@references  Hao, Z. et al. (2019). A monitoring and prediction system for
#'compound dry and hot events. Environ. Res. Lett., 14:114034.
#'@examples
#'mp=matrix(rnorm(60,0,1),ncol=1) # prec. of a month (e.g., August,60 years)
#'mt=matrix(rnorm(60,0,1),ncol=1) # temp. of a  month (e.g., August,60 years)
#'threp=20 #Specify the percentile-based threshold
#'thret=80 #Specify the percentile-based threshold
#'DH<-GetDH(mp,mt,threp,thret) # 0-1 series

#'@export

GetDH<- function(mp,mt,threp,thret)
{
  n <- length(mp)
  y <- matrix(data=0, nrow = n, ncol = 1)
  # Define the matrix
  y1 <- matrix(data=0, nrow = n, ncol = 1)
  y2 <- matrix(data=0, nrow = n, ncol = 1)


  p0 <- quantile(mp,threp/100)  # compute the threshold
  t0 <- quantile(mt,thret/100)  # compute the threshold

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
