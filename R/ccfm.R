#' Apply CCFM
#'
#'
#'
#' @param data, data.frame
#' @param precipcol string representing the column in data frame with precipitation values
#' @param addcfcol string, name of column with additive change factors
#' @param multcfcol string, name of column with multiplicative change factors
#' @param upperboundadd numeric value (0-100) which is the upper bound percentile with which to apply the additive change factor
#' @param threshold numeric value (0-100) above which the additive change factor is applied
#'
#' @return data Data frame with percentiles (exact and rounded)
#' @export


ccfm=function(data,precipcol,addcfcol,multcfcol,upperboundadd,threshold){
  #Apply multiplicative cf as default
  data$scaled=data[,precipcol]*data[,multcfcol]

  #Apply additive over user specified range
  data[(data$percentile<=upperboundadd | data$percentile>=threshold),"scaled"]=data[(data$percentile<=upperboundadd | data$percentile>=threshold),precipcol]+data[(data$percentile<=upperboundadd | data$percentile>=threshold),addcfcol]

  #Apply multiplicative when additive produces a negative value
  data[(data$scaled<0),"scaled"]=data[(data$scaled<0),precipcol]*data[(data$scaled<0),multcfcol]

  #Remove extra precipitation events
  data[(data$Precip==0),"scaled"]=0

  return(data)
}
