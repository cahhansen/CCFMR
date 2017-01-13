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

precipsummary=function(data,precipcol,comparedata,compprecipcol,title){
  if(missing(comparedata)){
    precip=data[,precipcol]
    psum=summary(precip)
    pevents=length(precip[precip>0])
    q.min=min(precip)
    q.mean=mean(precip)
    q.75=quantile(precip,0.75)
    q.9=quantile(precip,0.9)
    q.95=quantile(precip,0.95)
    q.99=quantile(precip,0.99)
    q.max=max(precip)
    diff.75="NA"
    diff.9="NA"
    diff.95="NA"
    diff.99="NA"
     }else{
    compareprecip=comparedata[,compprecipcol]
    precip=data[,precipcol]
    psum=summary(precip)
    pevents=length(precip[precip>0])
    q.min=min(precip)
    q.mean=mean(precip)
    q.75=quantile(precip,0.75)
    diff.75=100*(q.75-quantile(compareprecip,0.75))/quantile(compareprecip,0.75)
    q.9=quantile(precip,0.9)
    diff.9=100*(q.9-quantile(compareprecip,0.9))/quantile(compareprecip,0.9)
    q.95=quantile(precip,0.95)
    diff.95=100*(q.95-quantile(compareprecip,0.95))/quantile(compareprecip,0.95)
    q.99=quantile(precip,0.99)
    diff.99=100*(q.99-quantile(compareprecip,0.99))/quantile(compareprecip,0.99)
    q.max=max(precip)
     }
  diffs=c(diff.75,diff.9,diff.95,diff.99)
  results=paste(q.min,q.mean,q.75,diff.75,q.9,diff.9,q.95,diff.95,q.99,diff.99,q.max,pevents,sep=",")
  print(results)
  plot(y=diffs,x=c(75,90,95,99),ylim=c(-75,75),xlim=c(60,100),main=title,ylab="Percent Difference from Observed",xlab="Percentile")
  text(y=diffs, x=c(75,90,95,99), labels=diffs)


}
