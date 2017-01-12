#' PrecipSummary
#'
#'
#' @param comparedata, data.frame of the comparison
#' @param data, data.frame
#' @param precipcol string representing the column in data frame with precipitation (observed, modeled, or scaled) values
#' @export

precipsummary=function(data,precipcol,comparedata,compprecipcol){
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
  print(q.min)
  print(q.mean)
  print(q.75)
  print(diff.75)
  print(q.9)
  print(diff.9)
  print(q.95)
  print(diff.95)
  print(q.99)
  print(diff.99)
  print(q.max)
  print(pevents)

}
