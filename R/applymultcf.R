#' Apply Multiplicative Change Factor
#'
#' @param obsdata data.frame with the date and observed precipitation record
#' @param multcfdata data.frame with day of the year and change factor
#' @param period vector of start and end years for historical observed period (should be same as baseline period)
#' @param yearsdiff number of years between baseline and future periods
#' @return data.frame of scaled projection of precipitation
#' @export
#'
#'

applymultcf=function(obsdata,multcfdata,period,yearsdiff){
  library(lubridate)
 obsdata$Date=as.Date(obsdata$Date)
 obsdata=obsdata[(obsdata$Date>=paste0(period[1],'-1-1') & obsdata$Date<=paste0(period[2],'-12-31')),]
 obsdata$DOY=yday(obsdata$Date)
 multproj=data.frame("Date"=obsdata$Date+(round(365.25*yearsdiff+1)))
 temp=merge(x=obsdata,y=multcfdata,by="DOY")
 temp=dplyr::arrange(temp,Date)
 multproj$scaledprecip=temp$Observed*temp$cf
 return(multproj)
}
