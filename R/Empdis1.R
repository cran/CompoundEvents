#'@title  Univariate empirical probability
#'@description  Compute univariate empirical probability
#'@param mp monthly precipitation
#'@usage Empdis1(mp)
#'@references Hao, Z. et al., 2019a. Statistical prediction of the severity of compound dry-hot events based on El Ni??o-Southern Oscillation. J. Hydrol., 572, 243-250.
#'@return The empirical probability
#'@examples
#'mp=matrix(rnorm(120,0,1),ncol=1)
#'nd<-Empdis1(mp)
#'@export
Empdis1<-function(mp)
{
  d=mp
  n=length(d)
  bp=matrix(data=0, nrow = n, ncol = 1)
  for (i in 1:n)
  {
    s=0
    for (j in 1:n)
    {
      if (d[j] <= d[i])
      {s <- s+ 1
      }
    }
    bp[i,1]=s
  }
  # Weibull   y=(bp)/(n+1)
  # Gringorten plotting position
  y=(bp-0.44)/(n+0.12)
  return (y)
}
