#'@title   Changes in compound dry-hot events
#'@description  Assess absolute/relative changes in compound dry-hot (DH) events
#' at the monthly scale and event occurrences at the annual scale.
#'@param pre monthly precipitation of several decades
#'@param tem monthly temperature of several decades
#'@param threp Threshold of precipitation (e.g., 20th percentile)
#'@param thret Threshold of temperature (e.g., 80th percentile)
#'@usage ChangeDH(pre,tem,threp,thret)
#'@return The absolute/relative change for each month and annual occurrence
#'@references Hao, Z. et al. (2018). A multivariate approach for  statistical
#'            assessments of compound extremes. J. Hydrol., 565: 87-94.
#'@references Hao, Z. et al. (2019). A monitoring and prediction system
#'            for compound dry and hot events. Environ. Res. Lett., 14:114034.
#'@examples
#'pre=runif(1200, min = 0, max = 100) # 100-year monthly precipitation
#'tem=runif(1200, min = 0, max = 100) # 100-year monthly temperature
#'threp=20 #Specify the percentile-based threshold
#'thret=80 # Specify the percentile-based threshold
#'chg<-ChangeDH(pre,tem,threp,thret) #  chg$abschg,chg$relchg, chg$yrtotal

#'@export

ChangeDH<- function(pre,tem,threp,thret)
{


  ny<-length(pre)/12   # Number of year

  # Define the matrix

  XP<-matrix(pre,nrow=ny,ncol=12,byrow=T)
  XT<-matrix(tem,nrow=ny,ncol=12,byrow=T)


  res_mono<-matrix(NA,nrow=ny,ncol=12,byrow=T)  # results of monthly occurrences

  # Specify the Quantile (from percentile threp, threp)

  for  (k in 1:12)
  {
    mdx<-XP[,k]
    mdy<-XT[,k]

    # if the first number is NA, exclude the first number and then compute SPI

    if (is.na(mdx[1])==TRUE)
    {
      xd<-mdx[2:ny]
      yd<-mdy[2:ny]

      res_mono[2:ny,k]<-CompoundEvents::GetDH(xd,yd,threp,thret)
    }

    else
      # if the first number is not NA, take the whole month to compute SPI
    {
      res_mono[,k]<-CompoundEvents::GetDH(mdx,mdy,threp,thret)
    }
  }

  # results of the sum for each month (12X1)
  month_total<-matrix(NA,nrow=1,ncol=12)
  month_total<-colSums(res_mono, na.rm = TRUE)

  #  output

  abschg<-matrix(NA,nrow=1,ncol=12)
  relchg<-matrix(NA,nrow=1,ncol=12)


  # results of sum for each year
  #year_total=matrix(NA,nrow=ny,ncol=1)
  year_total<-rowSums(res_mono, na.rm = TRUE)

  # results for the total occurrence

  # output for relative changes for two periods


  halfrow1 <- floor(ny/2)  # 向下取整，处理奇数行
  halfrow2 <- ceiling(ny/2)  # 向上取整，处理奇数行

  if (halfrow1!=halfrow2) {
    warning("Data are for odd years")
  }


  month_half1<-matrix(NA,nrow=1,ncol=12)
  month_half2<-matrix(NA,nrow=1,ncol=12)
  month_change<-matrix(NA,nrow=1,ncol=12)

  month_half1<-colSums(res_mono[1:halfrow1,]) # monthly sum for first period
  month_half2<-colSums(res_mono[halfrow2:ny,]) # monthly sum for second period

  month_abschg<-(month_half2-month_half1) # absolute change
  month_relchg<-(month_half2-month_half1)/month_half1 # relative change

  # year_half1<-sum(year_total[1:half_row1])  # anual sum for first period
  # year_half2<-sum(year_total[half_row2:ny])  # annual sum for second period
  # year_Relachange<-(year_half2-year_half1)/year_half1 # relative change

  result<-list(

    abschg=month_abschg,
    relchg=month_relchg*100,
    # month_total=month_total, # the total occurrence for each month
    # month_half1=month_half1, # occurrence of each month for first half period
    # month_half2=month_half2, # occurrence of each month for second half period
    # month_Relachange=month_Relachange, # Relative change for each month
    yrtotal=year_total # the total occurrence for each year
    # year_half1=year_half1, # occurrence for each year for first half period
    # year_half2=year_half2, # occurrence for each year for second half period
    # year_Relachange=year_Relachange # Relative change for annual scale
    )
  return (result)
}
