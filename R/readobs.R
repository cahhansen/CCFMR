#' Read Observed Data
#'
#' @param observed, data.frame
#' @param city
#' @return obsdata Data frame with observations for specified city
#' @export

readobs=function(observed,city){
  if(city=="slc"|city=="salt lake city"|city=="SLC"|city=="Salt Lake City"){
    obsdata=observed[(observed$StationName=="SALT LAKE CITY INTERNATIONAL AIRPORT UT US"),]
  }else if(city=="toledo"|city=="tol"|city=="Toledo"){
    obsdata=observed[(observed$StationName=="TOLEDO EXPRESS AIRPORT OH US"),]
  }else if(city=="Phoenix"|city=="phoenix"){
    obsdata=observed[(observed$StationName=="PHOENIX SKY HARBOR INTERNATIONAL AIRPORT AZ US"),]
  }else if(city=="seattle"|city=="Seattle"){
    obsdata=observed[(observed$StationName=="SEATTLE TACOMA INTERNATIONAL AIRPORT WA US"),]
  }else if(city=="miami"|city=="Miami"){
    obsdata=observed[(observed$StationName=="MIAMI INTERNATIONAL AIRPORT FL US"),]
  }else if(city=="Houston"|city=="houston"){
    obsdata=observed[(observed$StationName=="HOUSTON WILLIAM P HOBBY AIRPORT TX US"),]
  }
    obsdata$Date=as.Date(obsdata$Date,format="%m/%d/%Y")
    obsdata=obsdata[,c("Date","ObservedPrecip")]
    return(obsdata)
}
