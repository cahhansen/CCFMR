#' PrecipSummary
#'
#'
#'
#' @param data, data.frame
#' @param precipcol string representing the column in data frame with precipitation (observed, modeled, or scaled) values
#' @export

precipsummary=function(data,precipcol){
  precip=data[,precipcol]
  psum=summary(precip)
  pevents=length(precip[precip>0])
  print(psum)
  print(quantile(precip,0.9))
  print(quantile(precip,0.95))
  print(quantile(precip,0.99))
  print(pevents)
}
