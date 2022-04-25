######################################################################################
####### AUXILLARY FUNCTIONS FOR ALGO TRADING ######     
######################################################################################

# required packages
require(timeSeries)
require(tibble)
require(dplyr) # to get lag values
library(TTR) # tp use "SMA" function
library(zoo) # to use "rollapply" function
library(fGarch) # fro student t distribution
library(forecast)



## sign correlation, X is a sequence of data or a data frame of data
rho.cal<-function(X){
  rho.hat<- cor(X-mean(X), sign(X-mean(X)))
  return(as.numeric(rho.hat))
}

## function to generate weight
#get_weights <- function(N){
#  return(diff(c(0, sort(runif(N-1, min = 0, max = 1)), 1)))
#}
get_weights <- function(N){
  w<- runif(N, min = 0, max = 1)
  return(w/sum(w))
}

#######################################################################
# Function to calculate lag L (L= 1, 2, 3, 4, 5,...) values
#######################################################################
lag_data <- function(data, L){
  data <- as.vector(data)
  lag.data <- sapply(1:L, function(l) dplyr::lag(data, l))
  return(lag.data)
}
# Calculate lag values
# lag.data <- lag_data(data = data, L = L)



#Function to calculate weighted forecast
ML_approach <- function(data, L = L, replication = 5000){
  L <- L
  # Calculate lag values
  lag.data <- lag_data(data = data, L = L)
  # Do the simulation for number of replication times
  # Creating a matrix to store the weights
  all_wts <- matrix(nrow = replication, ncol = ncol(lag.data))
  # Creating an empty vector to store demand forecast
  dem_fore <- matrix(0, nrow = nrow(lag.data) - L, ncol = replication)
  # Creating an empty vector to store forecasting error
  error <- matrix(0, nrow = nrow(lag.data) - L, ncol = replication)
  # Creating an empty vector to store MSE
  MSE <- vector('numeric', length = replication)
  
  # Do the simulation
  for (i in 1:replication) {
    set.seed(2021 + i)
    # Create random weights first.
    wts <- runif(n = L)
    wts <- wts/sum(wts) # standardized the weight to get sum equal 1
    all_wts[i,] <- wts
    # demand forecast (forecast is valid only when t >= L+1)
    demand_fore <- sapply((L+1):nrow(lag.data), function(l) sum(wts * lag.data[l,]))
    # Storing demand forecast values
    dem_fore[,i] <- demand_fore
    # Calculate the forecast error
    error[,i] <- (data[-c(1:L)] - dem_fore[,i])
    #MSE[i] <- sum(error[,i]^2) 
    MSE[i] <- mean(error[,i]^2) 
  }
  # Storing the values in the table
  res1 <- data.frame(Weight = all_wts, Forecast = as.vector(tail(dem_fore, 1)), MSE = MSE)

  opt <- res1 %>% slice(which.min(MSE))
  
  all.forecast <- dem_fore[,which.min(res1$MSE)]
  opt.volforecast <- all.forecast
  
  opt.res <- c(opt.wt = as.numeric(opt[1:L]), opt.MSE = opt$MSE,
               opt.forecast = opt$Forecast, ll.fore = opt$Forecast-1.96*sqrt(opt$MSE), 
               ul.fore = opt$Forecast+1.96*sqrt(opt$MSE))
  
  return(list(res1, opt.res = opt.res, opt.volforecast = opt.volforecast))
}


#Function to calculate weighted forecast for test data

ML_approachTest <- function(data, L = L, opt.wt, replication = 1){
  L <- L
  # Calculate lag values
  lag.data <- lag_data(data = data, L = L)
  # Creating an empty vector to store demand forecast
  dem_fore <- matrix(0, nrow = nrow(lag.data) - L, ncol = replication)
  error <- matrix(0, nrow = nrow(lag.data) - L, ncol = replication)
  # Creating an empty vector to store results
  MSE <- vector('numeric', length = replication)
  #all_wts <- vector('numeric', length = replication)
  
  # Do the simulation
  for (i in 1:replication) {
    set.seed(2021 + i)
    wts <- opt.wt
    # demand forecast (forecast is valid only when t >= L+1)
    demand_fore <- sapply((L+1):nrow(lag.data), function(l) sum(wts * lag.data[l,]))
    # Storing demand forecast values
    dem_fore[,i] <- demand_fore
    
    error[,i] <- (data[-c(1:L)] - dem_fore[,i])
    MSE[i] <- mean(error[,i]^2) 
    
  }
  # Storing the values in the table
  res1 <- data.frame(Weight = wts, Forecast = as.vector(tail(dem_fore, 1)), MSE = MSE)
  #opt <- res1 %>% slice(which.min(MSE))
  
  all.forecast <- dem_fore
  opt.volforecast <- all.forecast
  
  opt.res <- c(opt.wt = wts, opt.MSE = res1$MSE,
               opt.forecast = res1$Forecast, ll.fore = res1$Forecast-1.96*sqrt(res1$MSE), 
               ul.fore = res1$Forecast+1.96*sqrt(res1$MSE))
  
  return(list(res1, opt.res = opt.res, opt.volforecast = opt.volforecast))
}


