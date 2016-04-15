#Recreating the research of Bialkowski et al. (2008)
#on VWAP strategies

#setwd("~/Documents/R/VWAP_data")
rm(list = ls(all=TRUE))
setwd("H:/VWAP")
library(stats)
library(tsDyn)

#See the Python code to obtain real-time intraday
#volume data from the Internet
data    <- read.csv("nasdaq_vols.csv",sep=",")
prices  <- read.csv("nasdaq_prices.csv",sep=",")
tickers <- colnames(data)

# day  <- 1:5
# mnth <- 4 
# 
# indvec <- data$DAY %in% day & data$MONTH %in% mnth
# 
# dataDay <- data[which(indvec == TRUE),]

#Data normalization
datamat  <- as.matrix(data)
pricemat <- as.matrix(prices)
dataorig <- as.matrix(data)
n        <- nrow(datamat)
k        <- ncol(datamat)
mvec     <- colMeans(datamat)
stdvec   <- sqrt(diag(cov(datamat)))
stdvec   <- as.matrix(stdvec)
normmat  <- diag(n) - (1/n)*(matrix(1,n,1)%*%matrix(1,1,n))
datadmd  <- normmat%*%datamat
datanrmd <- apply(datadmd,1,"/",t(stdvec))
datanrmd <- t(datanrmd)

datamat <- datanrmd
#datamat <- scale(datamat)

numfac <- 55

#Do the factor analysis on the volumes' data 
fa_rslts <- factanal(datamat, factors = numfac,scores = 'Bartlett',
                     lower = 1e-7)
print(fa_rslts)

#Remove data with very small uniquenesses 
fa_rslts$uniquenesses
inds <- which(fa_rslts$uniquenesses < 1e-05)
datamat <- datamat[,-inds]
dataorig <- dataorig[,-inds]


#Do the factor analysis AGAIN 
numfac <- 40 #to get a big enough p-value!
fa_rslts <- factanal(datamat, factors = numfac,scores = 'Bartlett',
                     lower = 1e-7)
print(fa_rslts)

ldgs   <- fa_rslts$loadings
scores <- fa_rslts$scores

days <- 51
dlgth <- 39

#"Index matrix" to get data for particular days
indmat = matrix(data = NA, nrow = days, ncol = dlgth)
seqce = seq(1,n,dlgth)
for (i in 1:days){
  if (i == days){
    indmat[i,] = seq(seqce[i],n)
  }else{
    indmat[i,] = seq(seqce[i],seqce[i+1]-1)
  }
}

#Compute the common component
comcomp <- scores%*%t(ldgs)
nrow(comcomp)
ncol(comcomp)

#Compute the specific component
speccomp <- datamat - comcomp

#Plot common component, specific component and the data together
day   <- 10 #take day 10
stock <- which(tickers == 'AMZN') #analyze AMAZON stock! 
plot(comcomp[indmat[day,],stock],type = 'l',col = 'red', ylim = c(-2,7),
     main = paste('Common component and data for',tickers[stock]),
     lwd = 2, xlab = "Time", ylab = "")
lines(1:39,datamat[indmat[day,],stock],type = 'l', lwd = 2, col = 'blue',
      lty=2)
legend("topright",c('Common cmpt', 'Data'),lty=c(1,2), lwd = c(2,2),
       col = c('red','blue'))

plot(speccomp[indmat[day,],stock],type = 'l', xlab = "Time", ylab = "",  
     main = paste('Specific component of',tickers[stock]), lwd = 2, 
     col = "magenta")

#Obtain the autocorrelation functions for original data, 
#common and specific components
ccompacf <- acf(comcomp[indmat[day,],stock],lag.max = 75,
                type = 'correlation', plot=FALSE)
plot(ccompacf,lwd=5,col='red',
     main = paste('Common component ACF for',tickers[stock]))

dataacf  <- acf(datamat[indmat[day,],stock],lag.max = 75,
                type = 'correlation', plot=FALSE)
plot(dataacf,lwd=5,col='red',
     main = paste('Data ACF for',tickers[stock]))

scompacf <- acf(speccomp[indmat[day,],stock],lag.max = 75,
                type = 'correlation', plot=FALSE)
plot(scompacf,lwd=5,col='red',
     main = paste('Specific component ACF for',tickers[stock]))

#SETAR estimation and forecast
setarmod <- selectSETAR(speccomp[indmat[day,],stock],m=2,d=1)
setarmod$th
setarmod$nthresh
setarmod$ML
setarmod$MM
setarmod$MH

setarEst <- setar(speccomp[1:(n-dlgth),stock],m=2,d=1,mL = setarmod$ML,
                  mM = setarmod$MM, mH = setarmod$MH, th=setarmod$th)
pred_setar <- predict(setarEst,n.ahead=39,type='MC',
                      ci = 0.95)
pred_setar$se
pred_setar$pred

#Plot the intraday forecast with confidence intervals 
plot(1:39,pred_setar$pred,col='red',lwd=2,type = 'l',ylim=c(-0.5,0.5),
     main = paste('1-day forecast for',tickers[stock]),xlab='Time',
     ylab='')
lines(1:39,pred_setar$se[,1],col='blue',lty=2)
lines(1:39,pred_setar$se[,2],col='blue',lty=2)

#Common component forecast

# L <- 10
# sumval <- 0
# day <- 50
# t <- indmat[day,dlgth]
# for (l in 1:L){
#   sumval = sumval + comcomp[t+1-dlgth*l,stock]
#   print(sumval)
# }
# comcomp_forec <- sumval/L

#Rolling-window forecast for the whole day
#for both common and specific components
L <- 10
comcomp_forec = numeric()
speccomp_forec = numeric()
shift <- 0

for (t in 1950:n-1){
  sumval <- 0
  for (l in 1:L){
    sumval = sumval + comcomp[t+1-dlgth*l,stock]
  }
  comcomp_f <- sumval/L
  comcomp_forec <- rbind(comcomp_forec,comcomp_f)
  
  setarmod <- selectSETAR(speccomp[1+shift:t,stock],m=2,d=1)
  setarEst <- setar(speccomp[1+shift:t,stock],m=2,d=1,
                    mL = setarmod$ML, mM = setarmod$MM, 
                    mH = setarmod$MH, th=setarmod$th)
  pred_setar <- predict(setarEst,n.ahead=dlgth,type='MC',
                        ci = 0.95)
  speccomp_f <- pred_setar$pred[1]
  speccomp_forec <- rbind(speccomp_forec,speccomp_f)
  
  shift <- shift+1
}

#Plot true and forecast common and specific components 
#common components
plot(comcomp_forec[1:39],type = 'l',col='red',lwd=2,ylim=c(-1,3),
     main = paste('True and forecast common component for',tickers[stock]),
     xlab='Time', ylab='')
lines(1:39,comcomp[1951:1989,stock],col = 'blue',lty=2,lwd=2)
legend("topleft",c('True', 'Forecast'),lty=c(2,1), lwd = c(2,2),
       col = c('blue','red'))

#specific components
plot(speccomp_forec[1:39],type = 'l',col='red',lwd=2,ylim=c(-1,1),
     main = paste('True and forecast specific component for',tickers[stock]),
     xlab='Time', ylab='')
lines(1:39,speccomp[1951:1989,stock],col = 'blue',lty=2,lwd=2)
legend("topleft",c('True', 'Forecast'),lty=c(2,1), lwd = c(2,2),
       col = c('blue','red'))

#Combined forecast for the original data
x_pred_trnsfd <- comcomp_forec + speccomp_forec
x_pred <- comcomp_forec + speccomp_forec
x_pred <- x_pred*stdvec[stock]+mvec[stock] #re-introduce scale and shift

#Plot true and forecast data
plot(x_pred[1:38],type = 'l',col='red',
     ylim=c(min(dataorig[1951:1988,stock]),max(dataorig[1951:1988,stock])),
     main = paste('True and predicted volume for',tickers[stock]),
     xlab = 'Time', ylab = "",lwd=2)
lines(1:38,dataorig[1951:1988,stock],col = 'blue',lty=2,lwd=2)

plot(x_pred_trnsfd[1:38],type = 'l',col='red',
     main = paste('True and predicted volume for',tickers[stock]),
     xlab = 'Time', ylab = "",lwd=2)
lines(1:38,datanrmd[1951:1988,stock],col = 'blue',lty=2,lwd=2)

#Dynamic VWAP execution (calculate VWAP for each intraday time t)
pricevec <- pricemat[indmat[day,]]
pricevec <- as.matrix(pricevec)
forec_vwap <- numeric()
x_pred <- x_pred[1:length(x_pred)-1]
x_pred <- as.matrix(x_pred)
for (t in 2:dlgth){
  summ = sum(x_pred[-(1:t-1)])
  forec_vwap[t] = sum(pricevec[-(1:t-1)]*x_pred[-(1:t-1)])/summ
}
forec_vwap <- forec_vwap[-1]

#Real VWAP (not predicted), to get the MAPE etc.
data_real <- dataorig[1951:1989,stock]
theor_vwap <- numeric()
for (t in 2:dlgth){
  summ = sum(data_real[-(1:t-1)])
  theor_vwap[t] = sum(pricevec[-(1:t-1)]*data_real[-(1:t-1)])/summ
}
theor_vwap <- theor_vwap[-1]

#Plot forecast VWAP against prices
plot(forec_vwap,type = 'l',lwd=2,col='blue', main='Close prices vs. forecast VWAP',
     xlab = 'Time',ylab = '',ylim=c(93,94.3))
lines(pricevec[-1],lty = 2,lwd=2,col='red')
legend("bottomright",c('Forecast VWAP','Close prices'),lty=c(1,2), lwd = c(2,2),
       col = c('blue','red'))

#Plot true VWAP against forecast VWAP
plot(theor_vwap,type = 'l',lwd=2,col='blue', main='True vs. forecast VWAP',
     xlab = 'Time',ylab = '',ylim=c(93.5,94))
lines(forec_vwap,lty = 2,lwd=2,col='red')
legend("topleft",c('True VWAP', 'Forecast VWAP'),lty=c(1,2), lwd = c(2,2),
       col = c('blue','red'))

#MAPE calculation (dynamic PCA-SETAR approach)
mape <- (1/length(forec_vwap))*sum(abs((theor_vwap-forec_vwap)/theor_vwap))

#MAPE calculation (classical approach)
dataorig_forec <- numeric()
for (t in 1950:n-1){
  sumval <- 0
  for (l in 1:L){
    sumval = sumval + dataorig[t+1-dlgth*l,stock]
  }
  dataorig_f <- sumval/L
  dataorig_forec <- rbind(dataorig_forec,dataorig_f)
}

#(Dynamic?)Classical VWAP execution (calculate VWAP 
#for each intraday time t)
pricevec <- pricemat[indmat[day,]]
pricevec <- as.matrix(pricevec)
forec_vwap_cl <- numeric()
xcl_pred <- dataorig_forec[1:length(dataorig_forec)-1]
xcl_pred <- as.matrix(xcl_pred)
for (t in 2:dlgth){
  summ = sum(xcl_pred[-(1:t-1)])
  forec_vwap_cl[t] = sum(pricevec[-(1:t-1)]*xcl_pred[-(1:t-1)])/summ
}
forec_vwap_cl <- forec_vwap_cl[-1]

plot(theor_vwap,type = 'l',lwd=2,col='blue', 
     main='True vs. forecast VWAP (classical approach)',
     xlab = 'Time',ylab = '',ylim=c(93.5,94))
lines(forec_vwap_cl,lty = 2,lwd=2,col='red')
legend("topleft",c('True VWAP', 'Forecast VWAP'),lty=c(1,2), lwd = c(2,2),
       col = c('blue','red'))

mape_cl <- (1/length(forec_vwap_cl))*sum(abs((theor_vwap-forec_vwap_cl)/theor_vwap))

#Mean average squared error
mase    <- sqrt( (1/length(forec_vwap))*sum((theor_vwap-forec_vwap)^2) )
mase_cl <- sqrt( (1/length(forec_vwap_cl))*sum((theor_vwap-forec_vwap_cl)^2) )
#mase should be smaller than mase_cl and it is!
#can also do this calculation for all days, 
#but it is time-consuming

