library(lpSolve)
library(zoo)


Spot <- read.csv("Data/Markedsdata.csv",skip=1,sep=";")[,3]
Spot <- na.approx(Spot)                            ## Electricity spot prices
Hour <- read.csv("Data/Markedsdata.csv",skip=1,sep=";")[,2]
load("Data/Demand.RData") # Electricity Demand measurements and forecasts

load("Data/WeatherMeas.rda")


load("Data/WeatherForecasts.rda")


#load("Data/L_60min_measurementsAndForecasts.rda")



bnmax <- 3.5 # Maximum rate of discharge of battery (kwh/h) 5 (7) for TESLA, 3.5 in 
bpmax <- 3.5 # Maximum rate of charge of battery (kwh/h)    Unknown for TESLA, 3.5 in
bmax <- 14.4 # Maximum capacity of battery ((1-DOD)*max)    14 for TESLA, 14.4 in 
cB <- 0.85   # COP of battery                               0.9 for TESLA, 0.85 in
tau <-  0.65 # sell_price = tau*buy_price                   Unknown in Denmark, 0.65 in
gnmax <- 4   # Maximum rate of bought electricity
gpmax <- 4   # Maximum rate of sold electricity

PVAzi <- 0   # Direction of PV - 0: South, pi/2: east, pi: north, 3/2 pi: west
PVEle <- pi/6 # Direction of PV - 0: Horizontal, pi/2: vertical
PVDirec <- c(cos(PVAzi),sin(PVAzi),sin(PVEle))
PVDirec <- PVDirec/sum(PVDirec^2) # x,y,z - x for south/north, x>0: south, x<0: north, y for east/west, y>0: east, y<0: west, z for up/down, z>0 for up.

PVsize <- 2  # Size (area) of PV
PVCOP <- 0.4 # Coefficient of efficiency

ScaleBuy <- rep(1,24)  # For each hour the scaling of maximum rate of bought electricity. Actual limit is ScaleBuy*gnmax
ScaleSell <- rep(1,24) # For each hour the scaling of maximum rate of sold electricity. Actual limit is ScaleSell*gpmax

lambdaB <- quantile(Spot,0.4) # Value of each kwh in battery 
#lambdaB <- 0 # Value of each kwh in battery 
N <- 24
source("ControlPVB.R")

Tmax <- (dim(C)[1]-24)
#Tmax <- 500
Battery <- Buy <- Sell <- Unsatisfied <- numeric(Tmax)
Battery[1] <- 7
Cost <- 0

for(k in 1:Tmax){
  PVAngle <- pmax(PVDirec[1]*C$sunx[k:(k+N-1)]+PVDirec[2]*C$suny[k:(k+N-1)]+PVDirec[3]*C$sunz[k:(k+N-1)],0) # Angle between PV direction and solar direction
  Production <- PVCOP*PVsize*(PVAngle*Xdmi[k,seq(6,by=7,len=N)] + PVDirec[3]*Xdmi[k,seq(5,by=7,len=N)]) # first term is due to direct radiation while the second is due to diffusive radiation
  Demand <- L[[1]][k,6:(6+N-1)]
  Price <- Spot[k:(k+N-1)]
  
  DCBS <- ControlPVB(bnmax,bpmax,rep(gnmax,24),rep(gpmax,24),bmax,cB,tau,N,lambdaB,Battery[k],Hour[k],Price,Demand,Production)
  Battery[(k+1)] <- Battery[k] - DCBS[1] + cB*DCBS[2]
  Buy[k] <- DCBS[3]
  Sell[k] <- DCBS[4]
  Unsatisfied[k] <- DCBS[5]
  Cost <- Cost + DCBS[3]*Price[1] - DCBS[4]*tau*Price[1]
  
  }


plot(Buy , type='s')
plot(Sell , type='s')
plot(Battery,type='s')

