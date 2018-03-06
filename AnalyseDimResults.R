load("DimResults.RData")
which.min(Cost)
dim(Cost)
Cost[1220]


Cost1 <- as.data.frame.table(Cost,stringsAsFactors = FALSE)

names(Cost1) <- c("bmax","bnpmax","cB","tau","gnpmax","PVAzi","PVEle","PVsize","Cost")

Cost1$bmax <- as.numeric(Cost1$bmax)
for(k in 1:length(names(Cost1))){
  Cost1[,k] <- as.numeric(Cost1[,k])
}



par(bg = "white")
par(bg = NA)
Interval <- (24*104+1):(24*107)
plot(Spot[Interval],type='s',ylab='Price (DKK/kWh)',xlab="Time (Hour)",axes=FALSE,ylim=c(120,340),xlim=c(0,3*24+6),cex.lab=1.3,lwd=2)
axis(side=1,at=c(0,12,24,36,48,60,72),labels=c("00:00","12:00","00:00","12:00","00:00","12:00","00:00"),cex.axis=1.3)
axis(side=2,at=c(120,230,340),cex.axis=1.3)

lines((Battery[,5,1,2,1,1,1,2,4][Interval]-12)*20+140,col='red',lwd=2)
axis(side=4,at=c(0,2,4,6,8)*20+140,labels=c(0,2,4,6,8),col='red',col.ticks=NULL,line=-1.1,cex.axis=1.3)
mtext("State of charge (kWh)", side=4,cex=1.3,col='red',line=1)

legend(55,200,c("Price","Battery"),col = c("black",'red'),lty=1,lwd=2,bty='n')

bat <- diff( Battery[,5,1,2,1,1,1,2,4][c(Interval,(Interval[length(Interval)]+1))])
Production <- L[[1]][Interval,6] + bat-Buy[,5,1,2,1,1,1,2,4][Interval]+Sell[,5,1,2,1,1,1,2,4][Interval]
Production[bat>0] <- (L[[1]][Interval,6] + bat/cB-Buy[,5,1,2,1,1,1,2,4][Interval]+Sell[,5,1,2,1,1,1,2,4][Interval])[bat>0]


Interval <- (24*104+1):(24*107)
plot(L[[1]][Interval,6],type='s',ylab='Demand/Production (kW)',xlab="Time (Hour)",axes=FALSE,ylim=c(0,1.25),xlim=c(0,3*24+6),cex.lab=1.3,lwd=2)
axis(side=1,at=c(0,12,24,36,48,60,72),labels=c("00:00","12:00","00:00","12:00","00:00","12:00","00:00"),cex.axis=1.3)
axis(side=2,cex.axis=1.3)

lines(Production,col='green',lwd=2)

lines((Battery[,5,1,2,1,1,1,2,4][Interval]-12)/10+0.2,col='red',lwd=2)
axis(side=4,at=c(0,2,4,6,8)/10+0.2,labels=c(0,2,4,6,8),col='red',col.ticks=NULL,line=-1.1,cex.axis=1.3)
mtext("State of charge (kWh)", side=4,cex=1.3,col='red',line=1)

legend(41.6,1.3,c("Demand","Production","Battery"),col = c("black",'green',"red"),lty=1,lwd=2,bty='n')


plot(Buy[,5,1,2,2,1,1,2,4][Interval],type='s')

plot(Sell[,5,1,1,2,1,1,1,4],type='s')


boxplot(Cost ~ bmax , data=subset(Cost1,PVsize==2))
#boxplot(Cost ~ bnpmax , data=Cost1)
boxplot(Cost ~ cB , data=Cost1)
boxplot(Cost ~ tau , data=Cost1)
#boxplot(Cost ~ bnpmax , data=Cost1)
boxplot(Cost ~ PVAzi , data=subset(Cost1,bmax == 9 & PVsize == 2 & tau ==0.65))
boxplot(Cost ~ cos(PVEle) , data=Cost1)
boxplot(Cost ~ PVsize , data=subset(Cost1,bmax == 17))


head(Cost1)
lm1 <- lm(Cost ~ bmax*cB*tau*cos(PVAzi)*PVEle*PVsize , data = Cost1)
lm2 <- step(lm1,direction="both",k=log(1440))

summary(lm2)

