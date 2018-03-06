I <- diag(rep(1,N))
LowTri <- lower.tri(matrix(1,ncol=N,nrow=N),diag=TRUE)
zeros <- diag(rep(0,N))
if(N==1){
  zeros=0
}
# x = (buy,sell,discharge,charge,slackbuy)

A <- rbind(rbind(cbind(cbind(cbind(cbind(-I,I),-I),I),-I),
                 cbind(cbind(cbind(cbind(zeros,zeros),-LowTri),cB*LowTri),zeros),
                 cbind(cbind(cbind(cbind(zeros,zeros),LowTri),-cB*LowTri),zeros),
                 cbind(cbind(cbind(cbind(I,zeros),zeros),zeros),zeros),
                 cbind(cbind(cbind(cbind(zeros,I),zeros),zeros),zeros),
                 cbind(cbind(cbind(cbind(zeros,zeros),I),zeros),zeros),
                 cbind(cbind(cbind(cbind(zeros,zeros),zeros),I),zeros),
                 cbind(cbind(cbind(cbind(I,zeros),zeros),zeros),zeros),
                 cbind(cbind(cbind(cbind(zeros,I),zeros),zeros),zeros),
                 cbind(cbind(cbind(cbind(zeros,zeros),I),zeros),zeros),
                 cbind(cbind(cbind(cbind(zeros,zeros),zeros),I),zeros),
                 cbind(cbind(cbind(cbind(zeros,zeros),zeros),zeros),I)))

ControlPVB <- function(bnmax,bpmax,buymax,sellmax,bmax,cB,tau,N,lambdaB,b0,Hour,Price,Demand,Production){
  
  Production <- Production/1000
  Demand <- Demand
  ####
  
  f <- matrix(c(Price,-tau*Price,rep(lambdaB,N),-cB*rep(lambdaB,N),Price+10^5),ncol=1)
  ####
  
  b <- matrix(c(Production-Demand,rep(bmax,N)-b0,numeric(N)+b0,numeric(2*N),rep(bnmax,N),rep(bpmax,N),rep(0,5*N)),ncol=1)
  
  if(Hour==1){
    b[(3*N+1):(4*N)] <- buymax
    b[(4*N+1):(5*N)] <- sellmax}else{
      b[(3*N+1):(4*N)] <- c(buymax[Hour:N],buymax[1:(Hour-1)])
      b[(4*N+1):(5*N)] <- c(sellmax[Hour:N],sellmax[1:(Hour-1)])}
  
  
  Solution <- lp("min",f,A,c(rep("<=",7*N),rep(">=",5*N)),b)
  
  DCBS <- c(Solution$solution[(2*N+1)],Solution$solution[(3*N+1)],Solution$solution[1],Solution$solution[(N+1)],Solution$solution[(4*N+1)])
  
  return(matrix(DCBS,nrow=1))
}
