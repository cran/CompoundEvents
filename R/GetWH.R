#'@title  Occurrence of compound wet-hot events
#'
#'@description  Extract compound wet-hot (WH) occurrences based on thresholds of precipitation and temperature.The binary variable of the WH (or wet-warm,WW) event can be obtained.
#'
#'@param mp Precipitation
#'@param mt Temperature
#'@param threp Threshold of precipitation (e.g., 80th percentile)
#'@param thret Threshold of temperature (e.g., 80th percentile)
#'@usage GetWH(mp,mt,threp,thret)
#'@references Hao, Z. et al (2013). Changes in concurrent monthly precipitation and temperature extremes. Environ. Res. Lett., 8(3): 034014.
#'@return The occurrence of compound wet-hot events (0-1 binary variable)
#'@examples
#'mp=matrix(rnorm(120,0,1),ncol=1)
#'mt=matrix(rnorm(120,0,1),ncol=1)
#'threp=80
#'thret=80
#'WH<-GetWH(mp,mt,threp,thret)
#'@export

GetWH<- function(mp,mt,threp,thret)
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
    if (mp[i]>p0)
    {
      y1[i]= 1
    }

    if (mt[i]>t0)
    {
      y2[i]= 1
    }

  }

  y <- y1*y2

  return (y)

}
