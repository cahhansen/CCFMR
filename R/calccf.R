#' Calculate Change Factor
#'
#' @param baselinedata data.frame with the day of year and daily average value for the baseline period
#' @param futuredata data.frame with the day of year and daily average value for the future period
#' @param type of change factor to be calculated
#' @return data.frame of additive or multiplicative change factor for each day of the year
#' @export
#'
#'

calccf=function(baselinedata,futuredata,cftype){
  if(cftype=="additive"){
  calccf=data.frame("DOY"=seq(1,366))
  calccf$cf=futuredata$Precip-baselinedata$Precip
  }
  else{
  calccf=data.frame("DOY"=seq(1,366))
  calccf$cf=futuredata$Precip/baselinedata$Precip
  }
  return(calccf)
}


