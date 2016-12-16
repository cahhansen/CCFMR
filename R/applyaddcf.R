#' Apply Additive Change Factor
#'
#' @param obsdata data.frame with the date and observed precipitation record
#' @param addcfdata data.frame with day of the year and change factor
#' @param period vector of start and end years for historical observed period (should be same as baseline period)
#' @param yearsdiff number of years between baseline and future periods
#' @return data.frame of scaled projection of precipitation
#' @export
#'
#'

applyaddcf=function(obsdata,addcfdata,period,yearsdiff){
  library(lubridate)
 obsdata$Date=as.Date(obsdata$Date)
 obsdata=obsdata[(obsdata$Date>=paste0(period[1],'-1-1') & obsdata$Date<=paste0(period[2],'-12-31')),]
 obsdata$DOY=yday(obsdata$Date)
 addproj=data.frame("Date"=obsdata$Date+(round(365.25*yearsdiff+1)))
 temp=merge(x=obsdata,y=addcfdata,by="DOY")
 temp=dplyr::arrange(temp,Date)
 addproj$scaledprecip=temp$Observed+temp$cf
 temp2=addproj$scaledprecip
 temp2[temp2<0]=0
 addproj$scaledprecip.modified=temp2
 return(addproj)
}
