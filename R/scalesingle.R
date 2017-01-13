#' Applies single change factors to observed precipitation
#'
#' @param cf data.frame with calculated change factors by day of the year
#' @param observed, data.frame with observed time series (formatted to include day of the year)
#' @param cftype, string indicating 'additive' or 'multiplicative'
#'
#' @return data.frame Data frame of scaled projections
#' @export

scalesingle=function(cf,observed,cftype){
  if(cftype=="additive"){
    scaled=merge(cf,observed,by="DOY")
    scaled$scaled=scaled$Precip+scaled$cf

  }else{
    scaled=merge(cf,observed,by="DOY")
    scaled$scaled=scaled$Precip*scaled$cf
  }
  scaled=scaled[order(scaled$Date),]
  return(scaled)
}
