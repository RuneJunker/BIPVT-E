library(lpSolve)
library(zoo)


Spot <- read.csv("Data/Markedsdata.csv",skip=1,sep=";")[,3]
Spot <- na.approx(Spot)                            ## Electricity spot prices
Hour <- read.csv("Data/Markedsdata.csv",skip=1,sep=";")[,2]
load("Data/Demand.RData") # Electricity Demand measurements and forecasts

load("Data/WeatherMeas.rda")

load("Data/WeatherForecasts.rda")


## What to vary: bnmax/bpmax, bmax, cB, tau, gnmax/gpmax, PVAzi, PVEle, PVsize
bmaxG <-   c(0,6,9,13,17)
bnpmaxG <- 3.5 #c(0.5,1,2,3.5,5) #bmax=0 -> bnpmax constant
cBG <- c(0.75,0.9)
tauG <- c(0,0.65)
gnpmaxG <- c(10)
PVAziG <- seq(0,1.75,len=6)*pi
PVEleG <- seq(0,1/2,len=3)*pi
PVsizeG <- c(0,1,2,4)

TotalConfigs <- length(bmaxG)*length(bnpmaxG)*length(cBG)*length(tauG)*length(gnpmaxG)*length(PVAziG)*length(PVEleG)*length(PVsizeG)

ScaleBuy <- rep(1,24)  # For each hour the scaling of maximum rate of bought electricity. Actual limit is ScaleBuy*gnmax
ScaleSell <- rep(1,24) # For each hour the scaling of maximum rate of sold electricity. Actual limit is ScaleSell*gpmax

lambdaB <- quantile(Spot,0.4) # Value of each kwh in battery 
#lambdaB <- 0 # Value of each kwh in battery 
N <- 24


Tmax <- (dim(C)[1]-24)
Tmax <- 8760
Battery <- Buy <- Sell <- Unsatisfied <- array(dim=c(Tmax,length(bmaxG),length(bnpmaxG),length(cBG),
                                                     length(tauG),length(gnpmaxG),length(PVAziG),
                                                     length(PVEleG),length(PVsizeG)),dimnames = c("T","bmax","bnpmax","cB",
                                                                                                  "tau","gnpmax","PVAzi",
                                                                                                  "PVEle","PVsize"))
Battery[1,,,,,,,,] <- 0
Cost <- array(0,dim=c(length(bmaxG),length(bnpmaxG),length(cBG),
                      length(tauG),length(gnpmaxG),length(PVAziG),
                      length(PVEleG),length(PVsizeG)),dimnames = list(bmaxG,bnpmaxG,cBG,tauG,
                                                                      gnpmaxG,PVAziG,PVEleG,
                                                                      PVsizeG))

for(bmaxK in 1:length(bmaxG)){
  for(bnpmaxK in 1:length(bnpmaxG)){
    for(cBK in 1:length(cBG)){
      for(tauK in 1:length(tauG)){
        for(gnpmaxK in 1:length(gnpmaxG)){
          for(PVAziK in 1:length(PVAziG)){
            for(PVEleK in 1:length(PVEleG)){
              for(PVsizeK in 1:length(PVsizeG)){
                bmax <- bmaxG[bmaxK] # Maximum capacity of battery ((1-DOD)*max)    14 for TESLA, 14.4 in 
                bnmax <- bnpmaxG[bnpmaxK] # Maximum rate of discharge of battery (kwh/h) 5 (7) for TESLA, 3.5 in 
                bpmax <- bnpmaxG[bnpmaxK] # Maximum rate of charge of battery (kwh/h)    Unknown for TESLA, 3.5 in
                cB <- cBG[cBK]   # COP of battery                               0.9 for TESLA, 0.85 in
                tau <-  tauG[tauK] # sell_price = tau*buy_price                   Unknown in Denmark, 0.65 in
                gnmax <- gnpmaxG[gnpmaxK]   # Maximum rate of bought electricity
                gpmax <- gnpmaxG[gnpmaxK]   # Maximum rate of sold electricity
                
                PVAzi <- PVAziG[PVAziK]   # Direction of PV - 0: South, pi/2: east, pi: north, 3/2 pi: west
                PVEle <- PVEleG[PVEleK] # Direction of PV - 0: Horizontal, pi/2: vertical
                PVDirec <- c(cos(PVAzi),sin(PVAzi),sin(PVEle))
                PVDirec <- PVDirec/sum(PVDirec^2) # x,y,z - x for south/north, x>0: south, x<0: north, y for east/west, y>0: east, y<0: west, z for up/down, z>0 for up.
                
                PVsize <- PVsizeG[PVsizeK]  # Size (area) of PV
                PVCOP <- 0.4 # Coefficient of efficiency
                source("ControlPVB.R")                
                for(k in 1:(Tmax-1)){
                  PVAngle <- pmax(PVDirec[1]*C$sunx[k:(k+23)]+PVDirec[2]*C$suny[k:(k+23)]+PVDirec[3]*C$sunz[k:(k+23)],0) # Angle between PV direction and solar direction
                  Production <- PVCOP*PVsize*(PVAngle*Xdmi[k,seq(6,by=7,len=24)] + PVDirec[3]*Xdmi[k,seq(5,by=7,len=24)]) # first term is due to direct radiation while the second is due to diffusive radiation
                  Demand <- L[[1]][k,6:29]
                  Price <- Spot[k:(k+23)]
                  
                  DCBS <- ControlPVB(bnmax,bpmax,rep(gnmax,24),rep(gpmax,24),bmax,cB,tau,N,lambdaB,Battery[k,bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK],Hour[k],Price,Demand,Production)
                  
                  Battery[(k+1),bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK] <- Battery[k,bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK] - DCBS[1] + cB*DCBS[2]
                  Buy[k,bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK] <- DCBS[3]
                  Sell[k,bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK] <- DCBS[4]
                  Unsatisfied[k,bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK] <- DCBS[5]
                  Cost[bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK] <- Cost[bmaxK,bnpmaxK,cBK,tauK,gnpmaxK,PVAziK,PVEleK,PVsizeK] + DCBS[3]*Price[1] - DCBS[4]*tau*Price[1]
                  
                }
                
                }}}}}}}}
save.image(file="DimResults.RData")


