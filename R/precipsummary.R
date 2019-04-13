#' PrecipSummary
#'
#'
#'
#' @param data, data.frame
#' @param precipcol string representing the column in data frame with precipitation (observed, modeled, or scaled) values
#' @param comparedata, data.frame of the comparison
#' @param compprecipcol string representing the column in data frame with precipitation (observed, modeled, or scaled) values
#' @param title, string used in plot title (such as the name of the CF approach)
#' @export

precipsummary=function(data,precipcol,comparedata,compprecipcol,title=NULL){
  resultsdf= data.frame(datasetA=NA,datasetB=NA,measure=c("Number of precip events","Min Precip","Mean Precip","75% Precip","90% Precip",
                                                          "95% Precip","99% Precip","Max Precip"))

  if(missing(comparedata)){
    precip=data[,precipcol]
    resultsdf$datasetA=round(c(length(precip[precip>0]),min(precip),mean(precip),quantile(precip,0.75),quantile(precip,0.9),quantile(precip,0.95),
    quantile(precip,0.99),max(precip)),digits=2)
    print(resultsdf[,c(1,3)])

     }else{
    compareprecip=comparedata[,compprecipcol]
    precip=data[,precipcol]
    resultsdf$datasetA=round(c(length(precip[precip>0]),min(precip),mean(precip),quantile(precip,0.75),quantile(precip,0.9),quantile(precip,0.95),
                         quantile(precip,0.99),max(precip)),digits=2)
    resultsdf$datasetB=round(c(length(compareprecip[compareprecip>0]),min(compareprecip),mean(compareprecip),quantile(compareprecip,0.75),quantile(compareprecip,0.9),quantile(compareprecip,0.95),
                         quantile(compareprecip,0.99),max(compareprecip)),digits=2)

    resultsdf$Diffs=resultsdf$datasetA-resultsdf$datasetB
    print(resultsdf)
    diffs=resultsdf$Diffs[4:7]

    plot(y=diffs,x=c(75,90,95,99),ylim=c(-75,75),xlim=c(60,100),main=title,ylab="Percent Difference from Observed",xlab="Percentile")
    text(y=diffs+5, x=c(75,90,95,99), labels=round(diffs,digits=2))

     }



}
