#' Calculate ECDF of precipitation values
#'
#' @param data.frame containing precipitation values
#' @param precipcol string representing the column in data frame with precipitation values
#' @return data.frame Data frame with percentiles (exact and rounded)
#' @export

calcecdf=function(data,precipcol){
  data$percentile=(rank(data[,precipcol])/length(data[,precipcol]))*100
  data$percentlow=floor(data$percentile)

  return(data)
}



