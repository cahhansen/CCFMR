#' Applies multiple change factors to observed precipitation
#'
#' @param observed, data.frame with observed time series (formatted to include day of the year)
#' @param cftype, string indicating 'additive' or 'multiplicative'
#'
#' @return data.frame Data frame of scaled projections
#' @export
#'

scalemultiple=function(observed,cftype){
  percent=data.frame(percentlow=seq(1,100))

  for (i in seq(0,99)){
    percent$base[i+1]=mean(base[(base$percentlow==i),"Precip"])
    percent$future[i+1]=mean(future[(future$percentlow==i),"Precip"])
  }
  percent$base[is.nan(percent$base)]=0
  percent$future[is.nan(percent$future)]=0

  percent$addcf=percent$future-percent$base
  percent$multcf=percent$future/percent$base
  percent$multcf[is.nan(percent$multcf)]=0

  scaledmcfm=observed
  scaledmcfm=merge(scaledmcfm,percent,by="percentlow")

  if(cftype=="additive"){
    scaledmcfm$addscaled=scaledmcfm$Precip+scaledmcfm$addcf
    scaledmcfm[(scaledmcfm$addscaled<0),"addscaled"]=0
  }else{
    scaledmcfm$multscaled=scaledmcfm$Precip*scaledmcfm$multcf
  }

  return(scaledmcfm)
}
