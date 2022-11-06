#'@title  Bivariate empirical probability
#'@description  Compute bivariate empirical probability
#'@param mp monthly precipitation
#'@param mt monthly temperature
#'@usage Empdis2(mp,mt)
#'@references Hao, Z. et al., 2019a. Statistical prediction of the severity of compound dry-hot events based on El Ni??o-Southern Oscillation. J. Hydrol., 572, 243-250.
#'@return The bivariate empirical probability
#'@examples
#'mp=matrix(rnorm(120,0,1),ncol=1)
#'mt=matrix(rnorm(120,0,1),ncol=1)
#'nd<-Empdis2(mp,mt)
#'@export

Empdis2<-function(mp,mt)
{

  dp=mp
  dt=mt

  n=length(dp)

  bp=matrix(data=0, nrow = n, ncol = 1)

  for (i in 1:n)
  {
    td=matrix(data=0, nrow = n, ncol = 3)
    for (j in 1:n)
    {
      if (dp[j] <= dp[i])
      {td[j,1] = 1}
      if (dt[j] > dt[i])
      {td[j,2] = 1}
    }
    td[,3] = td[,1]*td[,2]
    bp[i,1]=sum(td[,3])

  }

  y=(bp-0.44)/(n+0.12)

  return (y)
}
