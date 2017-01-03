## ---- echo=FALSE---------------------------------------------------------
#Load Libraries
library(CCFMR)
library(lubridate)

#Read in data
slcbcca=slc_bcca

#Limit data to specified time period for a specified model
base=formatperiods(slcbcca,"gfdl-esm2g.1.rcp26",c(1950,1979))
future=formatperiods(slcbcca,"gfdl-esm2g.1.rcp26",c(1980,2009))
obs=formatperiods(slc_obs,"ObservedPrecip",c(1950,1979))

## ------------------------------------------------------------------------
#Find average precipitation volume for each day of the year
futureavg=avgdaily(future)
baseavg=avgdaily(base)
#Calculate additive and multiplicative change factors
add=calccf(baseavg,futureavg,cftype="additive")
mult=calccf(baseavg,futureavg,cftype="multiplicative")

## ------------------------------------------------------------------------
#Calculate the day of the year for observed data
obs$DOY=yday(obs$Date)

#Apply corresponding cf to observed dataset
scaledadd=scalesingle(add,obs,cftype="additive")
scaledmult=scalesingle(mult,obs,cftype="multiplicative")

## ------------------------------------------------------------------------
#Calculate the percentiles (ECDF) of modeled values
base=calcecdf(base,"Precip")
future=calcecdf(future,"Precip")

#Calculate the percentiles (ECDF) of the observed values
obs$percentile=(rank(obs$Precip)/length(obs$Precip))*100
obs$percentlow=floor(obs$percentile)

#Visualize Results by plotting precip against the percentiles
plot(x=base$Precip,y=base$percentile,ylab='Percentile',xlab='Precipitation (mm/day)',main='ECDF',col=2)
points(x=future$Precip,y=future$percentile,col=4)
points(x=obs$Precip,y=obs$percentile,col=3)


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


## ------------------------------------------------------------------------
#Create and Format the mcfm (multiple change factor methodology) dataframe
scaledmcfm=obs
scaledmcfm=merge(scaledmcfm,percent,by="percentlow")

#Apply multiple change factors (additive and multiplicative only)
scaledmcfm$addscaled=scaledmcfm$Precip+scaledmcfm$addcf
scaledmcfm[(scaledmcfm$addscaled<0),"addscaled"]=0
scaledmcfm$multscaled=scaledmcfm$Precip*scaledmcfm$multcf

## ----fig.show='hold'-----------------------------------------------------
#Calculate ratio and differences for each bin
diff=percent$future-percent$base
ratio=percent$future/percent$base

plot(x=diff,y=seq(1,100),xlab='Difference of Future-Baseline',ylab='Percentile')
plot(x=ratio,y=seq(1,100),xlab='Ratio of Future/Baseline',ylab='Percentile')

## ------------------------------------------------------------------------
#Format a dataframe for the combined change factor method
scaledccfms=merge(obs,add,by="DOY")
scaledccfms=merge(scaledccfms,mult,by="DOY")
names(scaledccfms)=c("DOY","Date","Precip","percentile","percentlow","addcf","multcf")


## ------------------------------------------------------------------------

scaledccfms=ccfm(scaledccfms,"Precip","addcf","multcf",60,95)

#Remove extra precipitation events
scaledccfms[(scaledccfms$Precip==0),"scaled"]=0


## ------------------------------------------------------------------------
scaledccfmm=scaledmcfm

#Apply the multiple change factors over the specified ranges
scaledccfmm=ccfm(scaledccfmm,"Precip","addcf","multcf",60,95)

#Remove extra precipitation events
scaledccfmm[(scaledccfmm$Precip==0),"scaled"]=0



## ------------------------------------------------------------------------
#Historical Observed
summary(scaledccfmm$Precip)
nrow(scaledccfmm[(scaledccfmm$Precip>0),])

#Future Observed
comparison=formatperiods(slc_obs,"ObservedPrecip",c(1980,2009))
summary(comparison$Precip)
nrow(comparison[(comparison$Precip>0),])

#Single Additive Change Factor
summary(scaledadd$scaled)
nrow(scaledadd[(scaledadd$scaled>0),])

#Single Multiplicative Change Factor
summary(scaledmult$scaled)
nrow(scaledmult[(scaledmult$scaled>0),])

#Combined Single Change Factors
summary(scaledccfms$scaled)
nrow(scaledccfms[(scaledccfms$scaled>0),])

#Multiple Additive Change Factors
summary(scaledccfmm$addscaled)
nrow(scaledccfmm[(scaledccfmm$addscaled>0),])

#Multiple Multiplicative Change Factors
summary(scaledccfmm$multscaled)
nrow(scaledccfmm[(scaledccfmm$multscaled>0),])

#Combined Multiple Change Factors
(summary(scaledccfmm$scaled))
nrow(scaledccfmm[(scaledccfmm$scaled>0),])


## ------------------------------------------------------------------------
scaledccfmm$FutureDate=scaledccfmm$Date+(round(365.25*40))



