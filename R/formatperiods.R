#' Formats baseline and historic projections for a chosen model
#'
#' @param data data.frame with formatted time series of projections for various models
#' @param modelname Model and RCP
#' @param period vector, Bounding years for the baseline or future period
#'
#' @return data.frame Data frame of projections for the specified period
#' @export
#'
#'


formatperiods=function(data,modelname,period){
  data=data[,c("Date",modelname)]
  names(data)=c("Date","Model")
  perioddata=data[(data$Date>=(paste0(period[1],'-01-01'))& data$Date<=(paste0(period[2],'-12-31'))),]

  return(perioddata)
}
