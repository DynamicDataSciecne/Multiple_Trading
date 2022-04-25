#############################################################################################################################
# Function to implement the proposed robust trading strategy using DDVFI and the traditional one using KFVEI to training sample
# Outputs: this function will provide the graphs and results  (Table 2) presented in the manuscript
#############################################################################################################################

# data:  number of stocks used
# y: stock as response variable
# x: stocks as features
# delta: number to generate constant state covariance matrix
# Ve: constant innovation variance
# window.size: length of rolling window size to forecast innovation volatility

RATS_training<- function(data, y, x, delta = 0.0001, Ve = 0.001, window.size = 0 , L=L) {

data <- data    
# Implementation of maximum informative filter algorithm
res <- kalman_iteration(data = data, y=y, x=x, delta = delta, Ve= Ve) 
# Extract results
beta <- xts(res[[1]], order.by=index(data))

pdf(file="Hedge-ratios.pdf", height=8, width=12,onefile = FALSE, paper = "special")

if(ncol(beta==2)){col= c("green", "blue")
print(plot(beta[2:nrow(beta), 1:ncol(beta)], type='l', main = 'Dynamic hedge ratios', col = col))}

if(ncol(beta==3)){col= c("green", "blue", "red")
print(plot(beta[2:nrow(beta), 1:ncol(beta)], type='l', main = 'Dynamic hedge ratios', col = col))}

dev.off()

# DD-EWMA innovation volatility forecasts
vol <- NA # volatility
RMSE.algo<- NA 
nu <- res[[4]] # innovation
alpha<-seq(0.01, 0.5, 0.01) # range of alpha
for(i in 1: (nrow(data)-window.size)) {
  result <- DD_volatility (nu[i:(window.size+i-1)], cut_t=20, alpha = alpha)
  vol[i] <-result[1]
  RMSE.algo <- result[2]
}

# DDWvol innovation volatility (weighted volatility)
rho <- rho.cal(nu) # Sign correlation 

#df.fun <- function (x) rho*(x-1)*beta(x/2,1/2)-2*sqrt(x-2)
#df <- uniroot(df.fun, c(2, 15))$root
original.vol <- abs(nu - mean(nu))/rho # calculate observed volatility
# DDWvol
res_DDWvol <- ML_approach(data = original.vol, L = L, replication = 5000)

vol_DDWvol <- res_DDWvol$opt.volforecast # optimal DDW-volatility forecast

opt.wt <- res_DDWvol$opt.res[1:L]


# plot trade signals
nu <- xts(nu, order.by=index(data))
sqrtQ <- xts(sqrt(res[[3]]), order.by=index(data)) # KFVEI
sqrtQ[1:window.size] <- NA

vol <- xts(c(rep(NA, window.size), vol), order.by=index(data)) # DDVFI

##     
vol_DDWvol <- xts(c(rep(NA, L),vol_DDWvol), order.by=index(data)) # DDWvol
vol_DDWvol[1:(window.size)] <- NA 

#View(cbind(vol,vol_DDWvol))

#vol_combined <- merge(sqrtQ, vol, vol_DDWvol)
vol_combined <- merge(sqrtQ, vol_DDWvol)
colnames(vol_combined) <- c("KFVEI", "RWVFI")
#view(vol_combined)

#pdf(file="DDVFIvsKFVEIvsRWVFI.pdf", height=8, width=12,onefile = FALSE, paper = "special")
pdf(file="RWVFIvsKFVEI.pdf", height=8, width=10,onefile = FALSE, paper = "special")
#print(plot(vol_combined[3:length(index(vol_combined))], ylab='Volatlity', 
#           main = 'Innovation volatility', col=c('blue', 'red', "green"), lwd=c(2,2, 2)))

# first 100 values are NA
print(plot(vol_combined[3:length(index(vol_combined))], ylab='Volatlity', 
           main = 'Innovation volatility', col=c('blue', 'red'), lwd=c(2,2)))

dev.off()

## Proposed robust pairs trading strategy (DDVFI)

# Determining optimal p to maximize Sharpe ratio based on robust DDVFI method 
#nu <- xts(nu, order.by=index(data))
p <- seq(0.1, 2, 0.01)
SR<- NA
for(j in 1:length(p)){
  SR[j] <- SR.train(nu, volatility = vol, p[j], beta = beta, x = x, y = y)
}
ASR_DDVFI <- max(na.omit(SR))
ASR_DDVFI


## Calculate the optimal value of p by maximizing the SR
p.opt_DDVFI<- p[which.max(SR)]
p.opt_DDVFI

# create optimal trading signals based on robust DDVFI method 
p <- p.opt_DDVFI
signals_DDVFI <- merge(nu, p*vol, -p*vol)
colnames(signals_DDVFI) <- c("nu", "vol", "negvol")

pdf(file="DDVFI-signals.pdf", height=8, width=10,onefile = FALSE, paper = "special")

print(plot(signals_DDVFI[3:length(index(signals_DDVFI))], ylab='nu', main = 'Trading signals', 
     col=c('blue', 'red', 'red'), lwd=c(1,2,2)))
dev.off()

## Calculate the cumulative profit using optimal trading signals based on DDVFI

# Implementation of profit and loss function to calculate cumulative profit
profit.loss <- PnL(signals_DDVFI, nu, beta, x, y)
profit_DDVFI<- sum (na.omit(profit.loss))
profit_DDVFI



## Proposed data driven weigthed volatility (DDWvol) 

# Determining optimal p to maximize Sharpe ratio based on DDWvol method 
#nu <- xts(nu, order.by=index(data))
p<-seq(0.1, 2, 0.01)
SR<- NA
for(j in 1:length(p)){
  SR[j] <- SR.train(nu, volatility = vol_DDWvol, p[j], beta = beta, x = x, y = y)
}

ASR_DDWvol <- max(na.omit(SR))
ASR_DDWvol


## Calculate the optimal value of p by maximizing the SR
p.opt_DDWvol <- p[which.max(SR)]
p.opt_DDWvol

# create optimal trading signals based on robust DDVFI method 
p <- p.opt_DDWvol
signals_DDWvol <- merge(nu, p*vol_DDWvol, -p*vol_DDWvol)
colnames(signals_DDWvol) <- c("nu", "vol_DDWvol", "negvol_DDWvol")

pdf(file="RWVFI-signals.pdf", height=8, width=10,onefile = FALSE, paper = "special")

print(plot(signals_DDWvol[3:length(index(signals_DDWvol))], ylab='nu', main = 'Trading signals', 
           col=c('blue', 'red', 'red'), lwd=c(1,2,2))) #cex.lab=20, cex.axis=20, cex.main=20
dev.off()


## Calculate the cumulative profit using optimal trading signals based on DDVFI

# Implementation of profit and loss function to calculate cumulative profit
profit.loss <- PnL(signals_DDWvol, nu, beta, x, y)
profit_DDWvol <- sum (na.omit(profit.loss))
profit_DDWvol


## Traditional pairs trading strategy (KFVEI)

# Determining optimal p to maximize Sharpe ratio based on traditional KFVEI method 
#nu <- xts(nu, order.by=index(data))
p <- seq(0.1, 2, 0.01)
SR<- NA
for(j in 1:length(p)){
  SR[j] <- SR.train (nu, volatility=sqrtQ, p[j], beta=beta, x=x, y=y)
}

ASR_KFVEI<- max(na.omit(SR))
ASR_KFVEI

## Calculate the optimal value of p by maximizing the SR
p.opt_KFVEI<- p[which.max(SR)]
p.opt_KFVEI

# create optimal trading signals based on robust KFVEI method 
p <- p.opt_KFVEI
signals_KFVEI <- merge(nu, p*sqrtQ, -p*sqrtQ)
colnames(signals_KFVEI) <- c("nu", "sqrtQ", "negsqrtQ")

pdf(file="KFVEI-signals.pdf", height=8, width=10,onefile = FALSE, paper = "special")

print(plot(signals_KFVEI[3:length(index(signals_KFVEI))], ylab='nu', main = 'Trading signals', 
     col=c('blue', 'red', 'red'), lwd=c(1,2,2)))
dev.off()

## Calculate the cumulative profit using optimal trading signals based on KKVEI

# Implementation of profit and loss function to calculate cumulative profit
profit.loss<- PnL(signals_KFVEI, nu, beta, x, y)
profit_KFVEI<- sum (na.omit(profit.loss))
profit_KFVEI

## Save the results
res_DDVFI<- data.frame(popt_DDVFI = p.opt_DDVFI, ASR_DDVFI, profit_DDVFI)
res_DDWvol<- data.frame(popt_DDWvol = p.opt_DDWvol, ASR_DDWvol, profit_DDWvol)
res_KFVEI<- data.frame(popt_KFVEI =p.opt_KFVEI, ASR_KFVEI, profit_KFVEI)

results <- cbind(res_DDVFI, res_DDWvol, res_KFVEI)

return(list(results = results, res.optwt = opt.wt))

}





