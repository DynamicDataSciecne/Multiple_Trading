#########################################################################
# Rcode to check the robustness based on Test sample
#########################################################################

# Load required packages
pkg_list = c('quantmod','zoo', 'tseries','aTSA', 'urca','fGarch','timeSeries','tibble',
             'dplyr','TTR','forecast')
# quantmod: get the quantitative financial and trading data
# zoo: deal with irregular time series data
# tseries: to do time series analysis and computational finance
# aSTA: performs Engle-Granger(or EG) cointegration test
# urca: conducts the Johansen procedure on a given data set to test multiple cointegration
# fGarch: to modeling heterskedasticity in time series and also get the inverse and student t distribution
# dplyr: to get lag values
# TTR: to use "SMA" function

# Function to install required packages if needed
for (pkg in pkg_list)
{
  # Loading the library
  if (!library(pkg, logical.return=TRUE, character.only=TRUE))
  {
    # If the library cannot be loaded, install first and then load.
    install.packages(pkg)
    library(pkg, character.only=TRUE)
  }
}

# Load all the required functions needed get the results
source("RATS.aux.R")
source("RATS_Training.R")
source("RATS_Test.R")
source("op_functions.R")


# Implementation of training and test functions to get the results 

# Load the required stocks to use as data-sets from Yahoo finance

start.date <- '2017-2-1' # starting date of stock
end.date <- '2020-3-15' # ending date of stock
# Download the selected stocks from Yahoo finance
getSymbols(c('EWA','EWC','IGE'), src = "yahoo", from = start.date, to = end.date)
stocks <- merge(EWA = EWA[, "EWA.Adjusted"], EWC = EWC[, "EWC.Adjusted"], 
                IGE = IGE[, "IGE.Adjusted"])
tradeday <- nrow(stocks)
tradeday

# Splitting the data-sets into training and test sample
train_sample <- stocks[1:532]
test_sample <- stocks[533:784]

# new set
#train_sample <- stocks[1:700]
#test_sample <- stocks[701:tradeday]



####################################################################################################
# Implementation of RATS_training function for two stocks
####################################################################################################

robust_test <- function(delta, Ve){
  
# Implementation of RATS_training function for two stocks
  
assets <- c("EWA", "EWC") # selected two assets  
pair.stock <- merge(train_sample[, 1], train_sample[, 2], join="inner")
colnames(pair.stock) <- assets  

x <- pair.stock[, 1]
y <- pair.stock[, 2]
res.training22 <- RATS_training(data = pair.stock, y = y, x = x, delta = delta, Ve=Ve, 
                                window.size = 100, L=7)

res.training <- res.training22[[1]]
res.training

opt.weight <- res.training22[[2]]
opt.weight

# store the optimal value of p to use in test function
p.optimal <- c(res.training$popt_DDVFI, res.training$popt_DDWvol, res.training$popt_KFVEI) 
p.optimal


## Part of the results presented in Table 2 and 3 of manuscript based on training sample including the result of DDWvol

res_DDVFI<- data.frame(Popt=res.training$popt_DDVFI, ASR=res.training$ASR_DDVFI, Profit=res.training$profit_DDVFI)
res_DDWvol<- data.frame(Popt=res.training$popt_DDWvol, ASR=res.training$ASR_DDWvol, Profit=res.training$profit_DDWvol)
res_KFVEI<- data.frame(Popt=res.training$popt_KFVEI, ASR=res.training$ASR_KFVEI, Profit=res.training$profit_KFVEI)
res_Table2<- rbind(res_DDVFI, res_DDWvol, res_KFVEI)
row.names(res_Table2)<- c("Robust multiple trading","DDWvol multiple trading",
                          "Traditional multiple trading")

res_Table2Train <- cbind(res_Table2[2,], res_Table2[3,])


# Implementation of RATS_test function for two stocks

####################################################################################################
# Implementation of RATS_test function for two stocks
####################################################################################################
pair.stock <- merge(test_sample[, 1], test_sample[, 2], join="inner")
colnames(pair.stock) <- assets
# Plot the test sample
# plot(pair.stock, legend.loc=1)
# Implementation of maximum informative filter algorithm for pairs trading
x <- pair.stock[, 1]
y <- pair.stock[, 2]
res.test <- RATS_test(data = pair.stock, y = y, x = x, delta = delta, Ve = Ve,
                      p.optimal =p.optimal, window.size = 40, L= 5, opt.wt = opt.weight)
#res.test


## Part of the results presented in supplementary of manuscript based on test sample including the result of DDWvol

res_DDVFI<- data.frame(Popt=res.test$popt_DDVFI, ASR=res.test$ASR_DDVFI, Profit=res.test$profit_DDVFI)
res_DDWvol<- data.frame(Popt=res.test$popt_DDWvol, ASR=res.test$ASR_DDWvol, Profit=res.test$profit_DDWvol)
res_KFVEI<- data.frame(Popt=res.test$popt_KFVEI, ASR=res.test$ASR_KFVEI, Profit=res.test$profit_KFVEI)
res_Table4<- rbind(res_DDVFI, res_DDWvol, res_KFVEI)
row.names(res_Table4)<- c("Robust multiple trading", "DDWvol multiple trading", 
                          "Traditional multiple trading")
#res_Table4

res_Table4Test <- cbind(res_Table4[2,], res_Table4[3,])

res_final <- rbind(res_Table2Train, res_Table4Test)

rownames(res_final)<- c("Train", "Test")

return(res_final)
}


# Implementation

delta <- c(0.0001, 0.001, 0.005, 0.01, 0.05, 
           0.0001, 0.001, 0.005, 0.01, 0.05,
           0.0001, 0.001, 0.005, 0.01, 0.05)
Ve <- c(rep(0.001,5), rep(0.01,5), rep(0.05, 5))


res_train <- matrix(NA, length(Ve), ncol=6)  
res_test <-  matrix(NA, length(Ve), ncol=6) 

for(i in 1:length(Ve)){
  res <- robust_test(delta = delta[i], Ve = Ve[i])
  res_train[i,] <- as.numeric(res[1,])
  res_test[i,] <- as.numeric(res[2,])
  colnames(res_train) <- c("Popt", "ASR", "Profit", "Popt", "ASR", "Profit")
  colnames(res_test) <- c("Popt", "ASR", "Profit", "Popt", "ASR", "Profit")
}

res_train

res_test




