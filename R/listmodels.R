#' Shows available models for a particular dataset
#'
#' @param data data.frame with formatted time series of projections for various models
#' @return list list of model names
#'
#' @export



listmodels=function(data){
  names=names(data)[2:length(names(data))]
  print(names)
}
