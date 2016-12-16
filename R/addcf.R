#' Calculate Additive Change Factor
#'
#' @param baselinedata data.frame with the day of year and daily average value for the baseline period
#' @param futuredata data.frame with the day of year and daily average value for the future period
#' @return data.frame of additive change factor for each day of the year
#' @export
#'
#'

addcf=function(baselinedata,futuredata){
  addcf=data.frame("DOY"=seq(1,366))
  addcf$cf=futuredata$Avg-baselinedata$Avg
  return(addcf)
}


