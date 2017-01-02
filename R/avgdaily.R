#' Calculate Precipitation by Day of the Year
#'
#' @param data data.frame with the date and daily precipitation projections
#'
#' @return data.frame Data frame of averaged precipitation values
#' @export
#'
#'

avgdaily=function(data){
  library(lubridate)
  data$DOY=as.factor(yday(data$Date))
  avgdata=tapply(data$Precip,data$DOY,mean)
  avgdata=data.frame("DOY"=c(seq(1,366)),"Avg"=avgdata)
  return(avgdata)
}


