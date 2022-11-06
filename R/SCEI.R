#'@title  Standardized Compound Event Indicator (SCEI)
#'@description  Compute SCEI based on monthly  precipitation and temperature.
#'@param mp monthly precipitation
#'@param mt monthly temperature
#'@param ts time scale
#'@usage SCEI(mp,mt,ts)
#'@references Hao, Z. et al., 2019a. Statistical prediction of the severity of compound dry-hot events based on El Ni??o-Southern Oscillation. J. Hydrol., 572, 243-250.
#'@return The monthly SCEI series
#'@examples
#'mp=matrix(rnorm(120,0,1),ncol=1)
#'mt=matrix(rnorm(120,0,1),ncol=1)
#'ts=3; # ts<=12 otherwise you should revise line 98
#'nd<-SCEI(mp,mt,ts)
#'d=cbind(mp,mt,nd)
#'testd<-matrix(d, ncol=3,byrow=FALSE)
#write.table(testd,file="testd.txt", sep=" ",row.names=FALSE,col.names=FALSE,quote=FALSE)

#'@export
#Generate monthly data

SCEI<-function(mp,mt,ts)
{


ny = length(mp)/12 # number of year

#
APD <- matrix(data=NA, nrow = length(mp), ncol = 1)
ATD <- matrix(data=NA, nrow = length(mt), ncol = 1)

for (i in 1:(length(mp)-ts+1))
{
  APD[ts+i-1] <- mean(mp[i:(i+ts-1)]);
  ATD[ts+i-1] <- mean(mt[i:(i+ts-1)]);
}


AP12=matrix(APD,ncol=12,byrow=T)
AT12=matrix(ATD,ncol=12,byrow=T)


# Define SCEI as a matrix
SCEI0 <- matrix(data=NA, nrow =ny,ncol=12,byrow=T)

for  (k in 1:12)
{

  mpx=AP12[,k]
  mtx=AT12[,k]

  #  Exclude the first value to compute SCEI (if the first number is NA)

  if (is.na(mpx[1])==TRUE)
  {
    xd=mpx[2:ny]
    yd=mtx[2:ny]


   jp=CompoundEvents::Empdis2(xd,yd)
   SCEI0[2:ny,k]=CompoundEvents::Empdis1(jp)
  }

  else
    # Take the whole month to compute SCEI (if the first number is not NA)
  {

    xd=mpx
    yd=mtx


  jp=CompoundEvents::Empdis2(xd,yd)

  SCEI0[,k]=CompoundEvents::Empdis1(jp)
  }

}

SCEI0=stats::qnorm(SCEI0)

SCEI=matrix(t(SCEI0),ncol=1)

print(SCEI)

return(SCEI)
}

