load("Data/L_60min_measurementsAndForecasts.rda")  ## Electricity demand measurements and forecasts 
Hour <- read.csv("Data/Markedsdata.csv",skip=1,sep=";")[,2]

MeanDemand <- tapply(L[[1]]$Pel,Hour[1:21144],mean,na.rm=TRUE)
for(i in 1:length(L)){
  for(j in 1:24){
    for(k in 1:(21144-24)){
      if(is.na(L[[i]][(k+j),(5+j)])!=1){
        L[[i]][k,(5+j)] <- L[[i]][(k+j),(5+j)]}else{
        L[[i]][k,(5+j)] <- MeanDemand[Hour[(k+j)]]
      }
    }
    for(k in (21144-23):21144){
      L[[i]][k,(5+j)] <- MeanDemand[Hour[(k+j)]]
    }
    }}

save(L,file="Data/Demand.RData")



load("Data/Xdmi_60min_forecasts.rda")
Xdmi <- Xdmi[-c(1:1417,21145),]
for(i in 2:8){
  for(j in 0:23){
    for(k in 1:(21144-24)){
      Xdmi[k,(i+7*j)] <- Xdmi[(k+j+1),(i+7*j)]
    }
    for(k in (21144-23):21144){
      Xdmi[k,(i+7*j)] <- NA
    }
  }}
save(Xdmi,file="Data/WeatherForecasts.RData")




load("Data/C_60min_climateMeasurements.rda")
#C[1416,1]
C <- C[-c(1:1416),]
C$sunx <- -cos(C$sunAzimuth) 
C$sunx[seq(24,21140,24)] <- (C$sunx[seq(23,21140,24)]+C$sunx[seq(25,21140,24)])/2 # Something strange has been done to the azimouth for midnight values
C$suny <- sin(C$sunAzimuth)
C$sunz <- sin(C$sunElevation)
save(C,file="Data/WeatherMeas.rda")





PVTraw <- read.csv("Data/PVT-data.csv",skip=1)[-8761,]

# We extract the parts that we need
THPS <- PVTraw[,c(11,15:25)] # The temperature of the cold fluid used by the heat pump in each of the 12 layers
THS <- PVTraw[,c(246,250:260)] # The temperature of the water used for heating in each of the 12 layers
TWWS <- PVTraw[,c(309,313:323)] # The temperature of the warm usage-water in each of the 12 layers

T2S <- function(x){ # function used to reduce the amout of layers from 12 to 6
  y <- numeric(6)
  for(k in 1:6){
    y[k] <- mean(x[c(2*k-1,2*k)])
  }
  return(y)
}
THPS <- t(apply(THPS,1,T2S))
THS <- t(apply(THS,1,T2S))
TWWS <- t(apply(TWWS,1,T2S))

TCHHPlb <- PVTraw[,38] # Temperature of bottom of lower coil heat exchanger for heat pump storage 
TCHHPlu <- PVTraw[,34] # Temperature of top of lower coil heat exchanger for heat pump storage 
TCHHPmb <- PVTraw[,43] # Temperature of bottom of middle coil heat exchanger for heat pump storage 
TCHHPmu <- PVTraw[,39] # Temperature of top of middle coil heat exchanger for heat pump storage 
TCHHPtb <- PVTraw[,48] # Temperature of bottom of upper coil heat exchanger for heat pump storage 
TCHHPtu <- PVTraw[,44] # Temperature of top of upper coil heat exchanger for heat pump storage 

## Uses water directly from PVT module
TCHHlb <- PVTraw[,269] # Temperature of bottom of lower coil heat exchanger for heat storage 
TCHHlu <- PVTraw[,273] # Temperature of top of lower coil heat exchanger for heat storage 

## Uses water from HP 
TCHHmb <- PVTraw[,275] # Temperature of bottom of middle coil heat exchanger for heat storage 
TCHHmu <- PVTraw[,279] # Temperature of top of middle coil heat exchanger for heat storage 
TCHHtb <- PVTraw[,284] # Temperature of bottom of upper coil heat exchanger for heat storage 
TCHHtu <- PVTraw[,280] # Temperature of top of upper coil heat exchanger for heat storage 

## Uses water directly from PVT module
TCHWWlb <- PVTraw[,336] # Temperature of bottom of lower coil heat exchanger for heat storage 
TCHWWlu <- PVTraw[,332] # Temperature of top of lower coil heat exchanger for heat storage 

## Uses water from HP 
TCHWWmb <- PVTraw[,341] # Temperature of bottom of middle coil heat exchanger for heat storage 
TCHWWmu <- PVTraw[,337] # Temperature of top of middle coil heat exchanger for heat storage 
TCHWWtb <- PVTraw[,346] # Temperature of bottom of upper coil heat exchanger for heat storage 
TCHWWtu <- PVTraw[,342] # Temperature of top of upper coil heat exchanger for heat storage 


PVT <- data.frame(THPS = THPS,THS = THS,TWWS = TWWS)



## Explorative analysis of HP

plot(PVTraw[,121] ~ PVTraw[,122],ylab='Supplied Heat',xlab='Consumed Electricity') 

plot(I(PVTraw[,121]/PVTraw[,122]) ~ PVTraw[,125],ylab='Efficiency',xlab='Cold water temperature') 



lmtom5 <- lm(I(PVTraw[PVTraw[,125]<(-5),121]/PVTraw[PVTraw[,125]<(-5),122]) ~ PVTraw[PVTraw[,125]<(-5),125])
lmm5to20 <- lm(I(PVTraw[PVTraw[,125]>(-5) & PVTraw[,125]<20,121]/PVTraw[PVTraw[,125]>(-5) & PVTraw[,125]<20,122]) ~ PVTraw[PVTraw[,125]>(-5) & PVTraw[,125]<20,125])
lm20to <- lm(I(PVTraw[PVTraw[,125]>20,121]/PVTraw[PVTraw[,125]>20,122]) ~ PVTraw[PVTraw[,125]>20,125])

limit <- 0
plot(PVTraw[PVTraw[,122]>limit,132] ~ I(PVTraw[PVTraw[,122]>limit,130]-PVTraw[PVTraw[,122]>limit,125]),ylab='Supplied temperature',xlab='Difference in temperature') 
plot(PVTraw[PVTraw[,122]>limit,122] ~ I(PVTraw[PVTraw[,122]>limit,130]-PVTraw[PVTraw[,122]>limit,125]),ylab='Electricity usage',xlab='Difference in temperature') 

# It seems like the supplied temperature is easily described by the temperature difference, but the needed electricty is very variable
# Put in another way the supplied temperature only seems to be a function of difference in temperature and on/off status
# Has the electricity been controlled to optimize the efficiency?

limit <- -8.5
plot(PVTraw[PVTraw[,125]>limit,132] ~ PVTraw[PVTraw[,125]>limit,127],ylab='Supplied Temperature',xlab='Taken temperature') # Is one of the straight lines due to phase change (ice)?
points(PVTraw[PVTraw[,125]<=limit,132] ~ PVTraw[PVTraw[,125]<=limit,127],col='blue') # Yes, seems to freeze at -8.5

plot(I(PVTraw[PVTraw[,125]>limit,132]/PVTraw[PVTraw[,125]>limit,127]) ~ PVTraw[PVTraw[,125]>limit,127],ylab='Supplied/Taken',xlab='Taken temperature',ylim=c(1,2.3)) # Non-constant ratio between heat capacities or non-constant heat losses?


plot(I(PVTraw[,121]/PVTraw[,122]) ~ PVTraw[,125],ylab='Efficiency',xlab='Cold water temperature') 
lines(predict(lmtom5) ~ PVTraw[PVTraw[,125]<(-5),125],col='red')
lines(predict(lmm5to20) ~ PVTraw[PVTraw[,125]>(-5) & PVTraw[,125]<20 & PVTraw[,122] !=0,125],col='red')
lines(predict(lm20to) ~ PVTraw[PVTraw[,125]>20,125],col='red')


plot(I(PVTraw[,121]/PVTraw[,122]) ~ PVTraw[,130],ylab='Supplied Heat',xlab='Consumed Electricity') 
plot(I(PVTraw[,121]/PVTraw[,122]) ~ I(PVTraw[,125]-PVTraw[,130]),ylab='Efficiency',xlab='Difference in cold and hot fluid temperature') 


plot(PVTraw[,131] ~ I((PVTraw[,253]+PVTraw[,316])/2-PVTraw[,25]),ylim=c(46,75))
