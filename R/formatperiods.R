#' Formats baseline and historic projections for a chosen model
#'
#' @param data data.frame with formatted time series of projections for various models
#' @param modelname Model and RCP
#' @param baseline vector, Bounding years for the baseline period, should be equal in length to the future period
#' @param future vector, Bounding years for the future period
#'
#' @export
#'
#'


formatperiods=function(modelname,baseline,future){
  data=data[,c("Date",modelname)]
  names(data)=c("Date","Model")
  basedata=data[(data$Date>=(paste0(baseline[1],'-01-01'))& data$Date<=(paste0(baseline[2],'-12-31'))),]
  futuredata=data[(data$Date>=(paste0(future[1],'-01-01'))& data$Date<=(paste0(future[2],'-12-31'))),]
  return(c(basedata,futuredata))
}
