######################################################################################
####### AUXILLARY FUNCTIONS FOR ANALYZING (ROBUST) MULTIPLE TRADING STRATEGIES ######     
######################################################################################


############################################################################
# Function to select cointegrated pairs 
############################################################################

# data: a number of stocks to select cointegrated pairs  

cointegrated_pairs <- function (data){
  n <- ncol (data)
  pvalue_matrix <- matrix(0, nrow=n, ncol=n)
  pairs <- list()
  m <- 1
  for (i in 1:n){
    for (j in 1:n){
      if(i>=j) { 
        next;
      } else{
        S1 <- data[, i]
        S2 <- data[, j]
        result <- coint.test (as.numeric(S1), as.numeric(S2), output = FALSE)
        pvalue_matrix [i, j] <- result[,3][[1]]
        if (result[,3][[1]] < 0.05){
          pairs [[m]] <- c (i, j)
          m <- m+1}
      }
    }
  }
  newlist <- list(pvalue_matrix, pairs)
  return (newlist)
}

####################################################################
# Function to calculate sign correlation
####################################################################
# X: data to calculate sign correlation
rho.cal<-function(X){
  rho.hat<- cor(sign(X-mean(X)), X-mean(X))
  return(rho.hat)
}

#Example
#data <- runif(100, 0,1)
#rho.cal(data)

#########################################################################
# Function to DD-EWMA innovation volatility forecasts
#########################################################################

# y: data to calculate DD-EWMA innovation volatility forecasts and the RMSE 
# cut_t: the number of observations to obtain initial smoothed statistic
# alpha: a set of smoothing constants, ranges from 0 to 1.

DD_volatility <- function(y, cut_t, alpha){
  t <-length(y)
  rho <- rho.cal(y) # calculate sample sign correlation
  vol <- abs(y-mean(y))/rho # calculate observed volatility
  MSE_alpha <- rep(0, length(alpha))
  sn <- rep(0, length(alpha)) # volatility
  for(a in 1:length(alpha)){
    s <- mean(vol[1:cut_t]) # initial smoothed statistic 
    error<-rep(0, t)
    for(i in 1:t){
      error[i] <- vol[i]-s
      s <- alpha[a]*vol[i]+(1-alpha[a])*s
    }
    MSE_alpha[a]<- mean(error[-(1:cut_t)]^2) # forecast error sum of squares (FESS)
    sn[a]<- s
  }
  vol.forecast <- sn[which.min(MSE_alpha)]
  RMSE <- sqrt(min(MSE_alpha))
  return(c(vol.forecast, RMSE))
}

# Example:
#data <- runif(100, 0,1)
#alpha <-seq(0.01, 0.5, 0.01)
#DD_volatility (data, 20, alpha = alpha)


####################################################################
# Function to implement the maximum informative filter algorithm
####################################################################

# stocks: a number of stocks used
# y: stock as response variable
# x: stocks as features
# delta: a number to genereate constant state covariance matrix
# Ve: constant innovation variance

kalman_iteration <- function(data, y, x, delta, Ve) {
  x<- x
  y<- y
  x$intercept <- rep(1, nrow(x)) # create intercept
  m<- ncol(data) # number of column
  # Initial parameters
  sigma_v <- delta/(1-delta)*diag(m) # constant state covariance matrix
  P_t <- 10^{-10}*diag(ncol(data)) 
  P <- matrix(rep(0, m^2), nrow=m) # zero initial state covariance matrix
  I_t <- matrix(rep(0, m^2), nrow=m) 
  beta <- matrix(rep(0, nrow(y)*m), ncol=m)
  y_fitted <- rep(0, nrow(y))
  nu <- rep(0, nrow(y))
  Q <- rep(0, nrow(y))
  
  for(i in 1:nrow(y)) {
    if(i > 1) {
      beta[i, ] <- beta[i-1, ] # state (hedge ratio) transition
      P_t <- P + sigma_v # updated state covariance matrix
    }
    y_fitted[i] <- x[i, ] %*% beta[i, ] # observation prediction
    Q[i] <- x[i, ] %*% P_t %*% t(x[i, ]) + Ve # observation variance prediction
    nu[i] <- y[i] - y_fitted[i] # prediction error
    K_gain <- P_t %*% t(x[i, ]) / Q[i] # information gain
    # updating the state
    beta[i, ] <- beta[i, ] + K_gain * nu[i]
    I_t <- inv(P_t)+ t(x[i, ])%*%x[i, ] / Ve #updated information matrix
    P <- inv(I_t)
  }
  return(list(beta, P, Q, nu))
}

# Example
#library('quantmod')
#start.date = '2017-2-1' # starting date of stock
#end.date = '2019-3-15' # ending date of stock
#Get stocks from Yahoo finance
#getSymbols(c('EWA','EWC'), src = "yahoo", from = start.date, to = end.date)
#stocks <-merge(EWA = EWA[, "EWA.Adjusted"], EWC = EWC[, "EWC.Adjusted"])
#pair.stock<- pair.stock <- merge(stocks[, 1], stocks[, 2], join="inner")
#x <- pair.stock[, 1]
#y <- pair.stock[, 2]
#res <- kalman_iteration(stocks=pair.stock, y, x, delta=0.0001, Ve=0.001) # Implementation of function
#plot(beta[2:nrow(beta), 1],type='l',main ='Dynamic hedge ratio',col = "blue")



########################################################################################
# # Function to generate trading signals and positions and calculate profit and loss for multiple stocks
########################################################################################

# signals: generated trading signals
# nu: innovations
# beta: hedge ratios 
# x: explanatory stocks
# y: response stock

PnL<-function(signals, nu, beta, x, y ){
  
  #method<- match.arg(method)
  colnames(signals) <- c("nu", "volatility", "negvolatility")
  len <- length(index(signals)) 
  vec.sig<- ifelse((signals[1:len]$nu > signals[1:len]$volatility) & 
                      (lag.xts(signals$nu, 1) < lag.xts(signals$volatility, 1)), -1, 
            ifelse((signals[1:len]$nu < signals[1:len]$negvolatility) & 
                      (lag.xts(signals$nu, 1) > lag.xts(signals$negvolatility, 1)), 1, 0))
                   
  # getting only the first signals
  vec.sig[vec.sig == 0] <- NA # replace 0 by NA
  vec.sig <- na.locf(vec.sig) # replace the missing values by last real observations
  vec.sig <- diff(vec.sig)/2
  # generate positions and calculate profit for two stocks
  if(ncol(beta)==2){
    sim <- merge(lag.xts(vec.sig,1), beta[, 1], x[, 1], y)
    colnames(sim) <- c("sig", "hedge", assets[1], assets[2])
    sim$posX <- sim$sig * -1000 * sim$hedge
    sim$posY <- sim$sig * 1000   
    sim$posX[sim$posX == 0] <- NA
    sim$posX <- na.locf(sim$posX)
    sim$posY[sim$posY == 0] <- NA
    sim$posY <- na.locf(sim$posY)
    PLX <- sim$posX * diff(sim[, assets[1]])
    PLY <- sim$posY * diff(sim[, assets[2]])
    profit_loss <- PLX + PLY
  }
  # generate positions and calculate profit for three stocks
  if(ncol(beta)==3){
    sim <- merge(lag.xts(vec.sig,1), beta[, 1], beta[, 2], x[, 1], x[, 2], y)
    colnames(sim) <- c("sig", "hedge1", "hedge2", assets[1], assets[2], assets[3])
    sim$posX1 <- sim$sig * -1000 * sim$hedge1
    sim$posX2 <- sim$sig * -1000 * sim$hedge2
    sim$posY <- sim$sig * 1000   
    sim$posX1[sim$posX1 == 0] <- NA
    sim$posX1 <- na.locf(sim$posX1)
    sim$posX2[sim$posX2 == 0] <- NA
    sim$posX2 <- na.locf(sim$posX2)
    sim$posY[sim$posY == 0] <- NA
    sim$posY <- na.locf(sim$posY)
    PLX <- sim$posX1 * diff(sim[, assets[1]]) + sim$posX2 * diff(sim[, assets[2]])
    PLY <- sim$posY * diff(sim[, assets[3]])
    profit_loss <- PLX + PLY
  }
  # generate positions and calculate profit for four stocks
  if(ncol(beta)==4){
    sim <- merge(lag.xts(vec.sig,1), beta[,1], beta[,2], beta[,3], x[,1], x[,2], x[,3], y)
    colnames(sim) <- c("sig", "hedge1", "hedge2", "hedge3",assets[1],assets[2], 
                       assets[3], assets[4])
    sim$posX1 <- sim$sig * -1000 * sim$hedge1
    sim$posX2 <- sim$sig * -1000 * sim$hedge2
    sim$posX3 <- sim$sig * -1000 * sim$hedge3
    sim$posY <- sim$sig * 1000   
    sim$posX1[sim$posX1 == 0] <- NA
    sim$posX1 <- na.locf(sim$posX1)
    sim$posX2[sim$posX2 == 0] <- NA
    sim$posX2 <- na.locf(sim$posX2)
    sim$posX3[sim$posX3 == 0] <- NA
    sim$posX3 <- na.locf(sim$posX3)
    sim$posY[sim$posY == 0] <- NA
    sim$posY <- na.locf(sim$posY)
    PLX <- sim$posX1 * diff(sim[, assets[1]]) + sim$posX2 * diff(sim[, assets[2]]) + 
      sim$posX3 * diff(sim[, assets[3]])
    PLY <- sim$posY * diff(sim[, assets[4]])
    profit_loss <- PLX + PLY
  }
  return(ProfitLoss=profit_loss)
}

####################################################################################################
# Functions to calculate the optimal value of threshold $p$, which maximizes the Sharpe ratio (SR)
####################################################################################################

# nu: innovations
# volatility: innovation volatility used to calculate the optimal value of p 
# beta: hedge ratios 
# x: features
# y: response stock


SR.train<-function(nu, volatility, p, beta, x, y){
  signals <- merge(nu, p*volatility, -p*volatility)
  colnames(signals) <- c("nu", "volatility", "negvolatility")
  # Implementation of profit
  profit.loss<- PnL(signals, nu, beta, x, y)
  st_p <- sqrt(252)*mean(na.omit(profit.loss))/sd(na.omit(profit.loss))
  return (st_p)
} 


