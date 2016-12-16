#' Calculate Multiplicative Change Factor
#'
#' @param baselinedata data.frame with the day of year and daily average value for the baseline period
#' @param futuredata data.frame with the day of year and daily average value for the future period
#' @return data.frame of multiplicative change factor for each day of the year
#' @export
#'
#'

multcf=function(baselinedata,futuredata){
  multcf=data.frame("DOY"=seq(1,366))
  multcf$cf=futuredata$Avg/baselinedata$Avg
  return(multcf)
}
