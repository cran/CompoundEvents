#'@title  Standardized Compound Event Indicator (SCEI) for dry-hot events
#'@description  Compute SCEI based on monthly  precipitation and temperature.
#'@param pre monthly precipitation of several decades
#'@param tem monthly temperature of several decades
#'@param ts time scale
#'@usage SCEIDH(pre,tem,ts)
#'@references Hao, Z. et al., 2019. Statistical prediction of the severity of
#'             compound dry-hot events based on El Ni??o-Southern Oscillation.
#'              J. Hydrol., 572, 243-250.
#'@return The monthly SCEI series of several decades
#'@examples
#'pre=matrix(rnorm(120,0,1),ncol=1) # 10-year monthly precipitation
#'tem=matrix(rnorm(120,0,1),ncol=1) # 10-year monthly temperature
#'ts=1; # ts<=12 otherwise you should revise the function
#'SCEIDH(pre,tem,ts) #Generate monthly data
#'@export


SCEIDH<-function(pre,tem,ts)
{

ny<-length(pre)/12 # number of year

#
APD <- matrix(data=NA, nrow = length(pre), ncol = 1)
ATD <- matrix(data=NA, nrow = length(tem), ncol = 1)

for (i in 1:(length(pre)-ts+1))
{
  APD[ts+i-1] <- mean(pre[i:(i+ts-1)])
  ATD[ts+i-1] <- mean(tem[i:(i+ts-1)])
}

print("---apd -------atd---")
print(APD)
print(ATD)

AP12<-matrix(APD,ncol=12,byrow=T)
AT12<-matrix(ATD,ncol=12,byrow=T)


# Define SCEI as a matrix
SCEI0 <- matrix(data=NA, nrow =ny,ncol=12,byrow=T)

for  (k in 1:12)
{

  mpx<-AP12[,k]
  mtx<-AT12[,k]
  print("---apx -------atx---")
  print(mpx)
  print(mtx)
  #  Exclude the first value to compute SCEI (if the first number is NA)

  if (is.na(mpx[1])==TRUE)
  {
    xd<-mpx[2:ny]
    yd<-mtx[2:ny]


   jp<-CompoundEvents::Empdis2(xd,yd)
   SCEI0[2:ny,k]<-CompoundEvents::Empdis1(jp)
  }

  else
    # Take the whole month to compute SCEI (if the first number is not NA)
  {

    xd<-mpx
    yd<-mtx


  jp<-CompoundEvents::Empdis2(xd,yd)

  print("---JP")
  print(jp)

  SCEI0[,k]<-CompoundEvents::Empdis1(jp)
  }

}

print("---SCEI0----")

SCEI0<-stats::qnorm(SCEI0)

print(SCEI0)

print("---SCEI----")

SCEI=matrix(t(SCEI0),ncol=1)


return(SCEI)
}


