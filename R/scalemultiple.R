#' Applies multiple change factors to observed precipitation
#'
#' @param observed, data.frame with observed time series (formatted to include day of the year)
#' @param cftype, string indicating 'additive' or 'multiplicative'
#' @param percentdf, data.frame with change factors by percentile
#' @return data.frame Data frame of scaled projections
#' @export

scalemultiple=function(observed,cftype,percentdf){

scaledmcfm=observed
scaledmcfm=merge(scaledmcfm,percentdf,by="percentlow")

  if(cftype=="additive"){
    scaledmcfm$addscaled=scaledmcfm$Precip+scaledmcfm$addcf
    scaledmcfm[(scaledmcfm$addscaled<0),"addscaled"]=0
  }else{
    scaledmcfm$multscaled=scaledmcfm$Precip*scaledmcfm$multcf
  }

  return(scaledmcfm)
}
