## ------------------------------------------------------------------------
#Read in data
library(CCFMR)
slcbcca=slc_bcca

#Format data into future and baseline periods
base26=formatperiods(slcbcca,"gfdl-esm2g.1.rcp26",c(1960,1990))
future26=formatperiods(slcbcca,"gfdl-esm2g.1.rcp26",c(2010,2039))

#Find average daily (day of year) precipitation volume
future26avg=avgdaily(future26)
base26avg=avgdaily(base26)

## ------------------------------------------------------------------------
#Calculate additive and multiplicative change factors
add26=addcf(base26avg,future26avg)
mult26=multcf(base26avg,future26avg)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

