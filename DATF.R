
install.packages("quantmod"); # only need to run once

rm(list = ls()) # Housekeeping
library("quantmod");
construc <- new.env();
getSymbols('DAUTONSA',src='FRED',env = construc);
data <- construc$DAUTONSA;
date.start <- "1967-01-01"
date.end <- "2019-02-01"
data[paste(date.start,date.end,sep="/")]
data <- data[1:626]

#Q0
library("tseries") # for JB test
plot(data, main = "Autosales Data")
summary(data)

#look at the distribution (i.e. normality)
kurtosis(as.numeric(data$DAUTONSA))
skewness(as.numeric(data$DAUTONSA))
jarque.bera.test(as.numeric(data$DAUTONSA))


hist(data)




#Q1
logdata <- (log(data))
plot(logdata)
#make a time variable
time = c(1:626)

#TREND: fit a 4th degree polynomial
model <- lm(as.numeric(logdata) ~  poly(time, 4))
summary(model)
#plot the model and the actual data
plot(as.numeric(logdata), type = "l", ylab = "Log Autosales", main = "Time Series Plot of Log Autosales with Quartic Model")
lines(as.numeric(model$fitted.values))
#Look at the residuals from the no-lag model
regResiduals <- logdata - model$fitted.values 
plot(regResiduals, type = "l", ylab = "Value", main = "Residuals from Quartic Model")


library(mlr)
library(lubridate)
#transform the data into a better format
newdata <- data.frame(date=index(data), coredata(data))
fald <- cbind(newdata,createDummyFeatures(newdata[1], cols = "date"))
month(as.POSIXlt(some_date, format="%d/%m/%Y"))
newdata$DAUTONSA <- logdata$DAUTONSA


#SEASONAL: add the 11 seasonal dummies
library(dummies)
cool <- format(as.Date(newdata$date), "%m")
newdata["Month"] <- cool
df1 <- cbind(cool, dummy(newdata$Month, sep = "_"))
newdata[4:14] = df1[,2:12]

January <- as.numeric(newdata$V4)
Feb <- as.numeric(newdata$V5)
March <- as.numeric(newdata$V6)
April <- as.numeric(newdata$V7)
May <- as.numeric(newdata$V8)
June <- as.numeric(newdata$V9)
July <- as.numeric(newdata$V10)
August <- as.numeric(newdata$V11)
September <- as.numeric(newdata$V12)
October <- as.numeric(newdata$V13)
November <- as.numeric(newdata$V14)


#run the model again with the seasonal dummies
model <- lm(logdata  ~  poly(time,4) + January + Feb+March+April+May+June+July
            +August+September+October+November)
summary(model)

#compare to actual data
plot(as.numeric(logdata), type = "l", main = "Log of Autosales Compared to Fitted Model", ylab = "Autosales (Thousands of Units)")
lines(as.numeric(model$fitted.values), col= "red")

#plot the residuals... still not great
plot(as.numeric(model$residuals), type = "l", main = "Residuals from Quartic Model", ylab = "Residual")


#AUTO/SERIAL CORRELATION
#acf/pacf for orgiinal data 
acf(logdata, lag.max = 10)
pacf(logdata, lag.max = 10)
#acf/pacf for model
acf(model$residuals)
pacf(model$residuals, lag.max = 10)

library(lmtest)
#DW and BG Test
modelDW <- lm(as.numeric(logdata)  ~   poly(time,4) + January + Feb+March+April+May+June+July
              +August+September+October+November)
dwtest(modelDW)
bgtest(model)

#plot e_t vs e_{t-1}
e_t = (model$residuals)
e_one = lag(model$residuals, k = 1)
plotter <- data.frame("e_t" = e_t, "e_one_back" = e_one)
plot(plotter, main = "Scatterplot of Residual at t against Residual at t-1", xlab = "Reisudal at t", ylab = "Residual at t-1")

#look at SIC of AR(1...6)
arModel1 <- lm((logdata)  ~  poly(time,4) + January + March+April+May+June+July + Feb + November
              +August+September+October + lag((logdata), 1))
print(BIC(arModel1))

arModel2 <- lm((logdata)  ~  poly(time,4) + January + March+April+May+June+July  + Feb + November
               +August+September+October + lag(logdata, 1) + lag(logdata, 2))
print(BIC(arModel2))

arModel3 <- lm((logdata)  ~  poly(time,4) + January + March+April+May+June+July  + Feb + November
               +August+September+October + lag(logdata, 1) + lag(logdata, 2) + lag(logdata, 3))
print(BIC(arModel3))

arModel4 <- lm((logdata)  ~  poly(time,4) + January + March+April+May+June+July  + Feb + November
               +August+September+October + lag(logdata, 1) + lag(logdata, 2) + lag(logdata, 3) + lag(logdata, 4))
print(BIC(arModel4))

arModel5 <- lm((logdata)  ~  poly(time,4) + January + March+April+May+June+July  + Feb + November
               +August+September+October + lag(logdata, 1) + lag(logdata, 2) + lag(logdata, 3) + lag(logdata, 4)
               + lag(logdata, 5))
print(BIC(arModel5))

arModel6 <- lm((logdata)  ~  poly(time,4) + January + March+April+May+June+July  + Feb + November
               +August+September+October + lag(logdata, 1) + lag(logdata, 2) + lag(logdata, 3) + lag(logdata, 4)
               + lag(logdata, 5) + lag(logdata, 6))
print(BIC(arModel6))

#Looks like we have an AR(4)
model <- arModel4
#remove any insignificant variables
summary(model)
lag1 <- as.numeric(lag(logdata, 1))
lag2 <- as.numeric(lag(logdata, 2))
lag3 <- as.numeric(lag(logdata, 3))
lag4 <- as.numeric(lag(logdata, 4))

model <- lm((logdata)  ~  time  + January + Feb    + March + April + May + June + September + lag1 + lag3 + lag4)
pacf(model$residuals)

e_t = (model$residuals)
e_one = lag(model$residuals, k = 1)
plotter <- data.frame("e_t" = e_t, "e_one_back" = e_one)
#looks random!
plot(plotter, main = "Scatterplot of Residual at t against Residual at t-1", xlab = "Reisudal at t", ylab = "Residual at t-1")

#plot the residuals to see if WN... looks good
plot(as.numeric(model$residuals), type = "l", ylab = "Residual", main = "Residuals of Fitted Model with Lags and Seasonal Dummies")




#Q2
#Point and Interval Forecasting
#Estimate Phi
# phiModel <- lm(logdata ~ time)
# pa <- pacf(phiModel$residuals)
# phi <- pa$acf[1]
# c <- model$coefficients[1]
# 
# FAR1<-matrix(0,626);
# FAR1[1]<- as.numeric(model$fitted.values)[622];
# for (i in 1:50){
#   FAR1[i+1]<-c+phi*FAR1[i];
# }
# matplot(cbind(as.numeric(model$fitted.values),FAR1),type="l",ylab="y",xlab="t")
# 
# 
# library("forecast")
# plot(FAR1)
# matplot(cbind(as.numeric(logdata),FAR1),type="l",ylab="y",xlab="t")
# y <- forecast(model$fitted.values, h = 50, level = c(90,95)) 
# # Plot data and forecast
# plot(y,PI = T,showgap = F,shaded = F) 
# lines(as.numeric(model$fitted.values),col = "red")
# 
# 
# newlogdata <- as.numeric(logdata)
# #Rolling 1-period ahead forecast
# bounds <- matrix(nrow=750,ncol = 3)
# bounds[1:622,1] <- as.numeric(model$fitted.values)
# bounds[624:700] <- 0
# currentModel <- as.numeric(model$fitted.values)
# 
# for (ii in 1:50) {
#   otherModel <- currentModel[1:(622+ii)]
#   y <- forecast(otherModel,h = 1, level = c(95) )
#   append(currentModel, y$mean)
#   bounds[622+ii, 3] <- y$mean
#   bounds[622+ii,1] <- y$lower
#   bounds[622+ii,2] <- y$upper
# }
# #plot the forecast
# matplot(cbind(as.numeric(logdata),bounds),type="l",ylab="y",xlab="t", col = 1:6, xlim = c(1,622))
# 
# newlogdata <- as.numeric(logdata)
# #Rolling 1-period ahead forecast
# 
# 
# 
# bounds1 <- matrix(mean(model$fitted.values),nrow = 670,ncol = 3)
# for (ii in 1:23) {
#   otherModel1 <- model$fitted.values[1:(600+ii)]
#   y1 <- forecast(otherModel1,h = 1, level = c(95) )
#   newlogdata[600+ii] = y1$mean
#   bounds1[600+ii,1] <- y1$lower
#   bounds1[600+ii,2] <- y1$upper
# }
# 
# for (ii in 1:67) {
#   y1 <- forecast(bounds1,h = 1, level = c(95) )
#   newlogdata[622+ii] = y1$mean
#   bounds1[622+ii,1] <- y1$lower
#   bounds1[622+ii,2] <- y1$upper
# }
# 



new.speeds <- data.frame(
  "January" = c(0,0),
  "March" = c(1,0),
  "Feb" = c(0,0),
  "April" = c(0,1),
  "September" = c(0,0),
   "May" = c(0,0),
  "June" = c(0,0),
  "time" = c(623, 624),
  "lag1" = c(as.numeric(lag(logdata, 1)[626]), 5.905),
  "lag3" = c(as.numeric(lag(logdata, 3)[626]), as.numeric(lag(logdata, 2)[626])),
  "lag4" = c(as.numeric(as.numeric(lag(logdata, 4)[626])), as.numeric(lag(logdata, 3)[625]))
  
)

marchPreFit = (predict(model, newdata = new.speeds, interval = "predict", level = 0.95)[1,1])
marchPreLow = ( 2.71828182 ^ predict(model, newdata = new.speeds, interval = "predict", level = 0.95)[1,2])
marchPreUp = ( 2.71828182 ^ predict(model, newdata = new.speeds, interval = "predict", level = 0.95)[1,3])


construc1 <- new.env();
getSymbols('DAUTONSA',src='FRED',env = construc1);
data1 <- construc$DAUTONSA;
date1.start <- "2019-02-01"
date1.end <- "2019-04-01"
data1[paste(date1.start,date1.end,sep="/")]
plot(as.numeric(logdata), xlim = c(500,650), ylab = "Log Autosales", main = "Logdata of Autosales with March Predictions and Realizations")
points(x = 627, y = log(data1[627]), col = "red")
points(x = 627, y= log(marchPreFit), col = "green")
points(x = 627, y= log(marchPreUp), col = "orange")
points(x = 627, y= log(marchPreLow), col = "orange")

#Since we can assume WN(0, sigma^2) for our whitenoise, let's find sigma^2
var(as.numeric(model$residuals))

#slide 219 gives us the density forecast, so N(log(marchPreFit), sigma^2)

#Q3
#structural breaks, using only Chow and MaxChow (needn't recursive or CUSUM since not gradual)
CTest<-matrix(0,1:626);
for (i in 20:600){
  
  #Left
  yp1<-as.numeric(logdata[2:(i-1)]);
  yp1lag<-as.numeric(logdata[1:(i-2)]);
  Left<-lm(yp1~yp1lag);
  LeftRSS<-sum(Left$residuals^{2});
  
  #Right
  yp2<-as.numeric(logdata[(i+2):626]);
  yp2lag<-as.numeric(logdata[(i+1):625]);
  Right<-lm(yp2~yp2lag);
  RightRSS<-sum(Right$residuals^{2});
  
  #Total
  yptot<-as.numeric(logdata[2:626]);
  yptotlag<-as.numeric(logdata[1:625]);
  tot<-lm(yptot~yptotlag);
  totRSS<-sum(tot$residuals^{2});
  
  #Chow Test
  Numer<-(totRSS-(RightRSS+LeftRSS))/2;
  Denomer<-(RightRSS+LeftRSS)/(626-4);
  CTest[i]<-Numer/Denomer;
}
which.max(CTest)


#Another way of confirming Chow
library('strucchange')
RegMat <- data.frame(cbind(yp1,yp1lag))
Model <- lm(yp1~yp1lag, data = RegMat)
F_values <- Fstats(Model, from = 0.15, data = RegMat)
plot(F_values$Fstats)
breakpoint <- breakpoints(F_values) # Only one breakpoint found
lines(breakpoints(F_values))
print(breakpoint) #The maxChow test identifies a single breakpoint at observation 50

#add the dummy
newdata[15] <- c(1:626)
newdata[16] <- as.numeric(newdata[15] > 418)
structDummy <- (newdata[16]$V16)
#redo model selecton
model <- lm((logdata)  ~  time + January + Feb   + March + April + May + June  + August + October + structDummy  + lag1  + lag3 + lag4)
summary(model)
BIC(model)


#Heteroscadecity (TODO)
#GARCH(1,1) coefficients
library(fGarch)
zeroMeanData = as.numeric(logdata - mean(logdata))
plot(zeroMeanData^2, type = "l")
T.g = garchFit(data = zeroMeanData, cond.dist = "std", trace = F, leverage = T)
coef(T.g)


#we need to get the variance at time t = 627 for March
fittedVals = function(theta1, r_t){
  omega = theta1[2]
  alpha = theta1[3]
  beta = theta1[5]
  h = rep(1, length(r_t))
  h[1] = omega
  for(t in 2:length(r_t)){
    h[t] = omega + alpha*r_t[(t-1)]^2 + beta*h[(t-1)] 
  }
  return( h)
}


lastSigma <- fittedVals(coef(T.g), zeroMeanData)[626]
lastR2 <- as.numeric(zeroMeanData[626]^2)

omega <- coef(T.g)[2]
alpha <- coef(T.g)[3]
beta <- coef(T.g)[5]

sigmaPlusOne <- as.numeric(omega) + as.numeric(alpha * lastR2)  + as.numeric(beta * lastSigma)

#point prediction for March
new.speeds2 <- data.frame(
  "January" = c(0,0),
  "March" = c(1,0),
  "Feb" = c(0,0),
  "April" = c(0,1),
  "August" = c(0,0),
  "May" = c(0,0),
  "June" = c(0,0),
  "October" = c(0,0),
  "time" = c(623, 624),
  "lag1" = c(as.numeric(lag(logdata, 1)[626]), 5.90),
  "lag3" = c(as.numeric(lag(logdata, 3)[626]), lag(logdata, 2)[626]),
  "lag4" = c(as.numeric(lag(logdata, 4)[626]), lag(logdata, 3)[626]),
  "structDummy" = c(1,1)
  
)



marchFit2 = (predict(model, newdata = new.speeds2))[1]
upperBound = marchFit2 + sqrt(sigmaPlusOne)
lowerBound = marchFit2 - sqrt(sigmaPlusOne)

plot(as.numeric(logdata), xlim = c(500,650), ylab = "Log Autosales", main = "Logdata of Autosales with March Predictions and Realizations (including Additional Info)")
points(x = 627, y = log(data1[627]), col = "red")
points(x = 627, y= (marchFit2), col = "green")
points(x = 627, y= (upperBound), col = "orange")
points(x = 627, y= (lowerBound), col = "orange")

#The density forecast will be Normal with mean marchFit2 and variance sigmaPlusOne

#Normality
hist(logdata)
hist(as.numeric(model$fitted.values))
hist(as.numeric(model$residuals))
kurtosis(model$residuals)
skewness(model$residuals)
jarque.bera.test(model$residuals)





#Q4
# Point and Interval Forecasting
# Estimate Phi
phiModel <- lm(logdata ~ time)
pa <- pacf(phiModel$residuals)
phi <- pa$acf[1]
c <- model$coefficients[1]

#naive forecast
library("forecast")
forecast(model, h=150)
y <- forecast(as.numeric(model$fitted.values),h = 7, level = c(90,95))
plot(y, main = "Poor Forecast for September from Forecast()", xlab = "Index", ylab = "Log Autosales")

#rolling 1-step ahead
#point prediction for May, April was fit as 5.92, 5.71, 6.12 (mean, lwr, upr)
new.speeds3 <- data.frame(
  "January" = c(0,0,0),
  "March" = c(1,0,0),
  "Feb" = c(0,0,0),
  "April" = c(0,1,0),
  "August" = c(0,0,0),
  "May" = c(0,0,1),
  "June" = c(0,0,0),
  "October" = c(0,0,0),
  "time" = c(623, 624,625),
  "lag1" = c(as.numeric(lag(logdata, 1)[626]), 5.91, 5.92),
  "lag3" = c(as.numeric(lag(logdata, 3)[626]), lag(logdata, 2)[626], lag(logdata, 1)[626]),
  "lag4" = c(as.numeric(lag(logdata, 4)[626]), lag(logdata, 3)[626], lag(logdata, 2)[626]),
  "structDummy" = c(1,1, 1)
  
)
May <- (predict(model, newdata = new.speeds3, interval = "predict", level = 0.95))


#point prediction for June
new.speeds3 <- data.frame(
  "January" = c(0,0,0, 0),
  "March" = c(1,0,0, 0),
  "Feb" = c(0,0,0, 0),
  "April" = c(0,1,0, 0),
  "August" = c(0,0,0, 0),
  "May" = c(0,0,1, 0),
  "June" = c(0,0,0, 0),
  "October" = c(0,0,0, 0),
  "time" = c(623, 624,625, 626),
  "lag1" = c(as.numeric(lag(logdata, 1)[626]), 5.91, 5.92, 6),
  "lag3" = c(as.numeric(lag(logdata, 3)[626]), lag(logdata, 2)[626], lag(logdata, 1)[626], 5.91),
  "lag4" = c(as.numeric(lag(logdata, 4)[626]), lag(logdata, 3)[626], lag(logdata, 2)[626], lag(logdata, 1)[626]),
  "structDummy" = c(1,1, 1, 1)
  
)
June <- (predict(model, newdata = new.speeds3, interval = "predict", level = 0.95))


#point prediction for July
new.speeds3 <- data.frame(
  "January" = c(0,0,0, 0, 0),
  "March" = c(1,0,0, 0, 0),
  "Feb" = c(0,0,0, 0, 0),
  "April" = c(0,1,0, 0, 0),
  "August" = c(0,0,0, 0, 0),
  "May" = c(0,0,1, 0,0),
  "June" = c(0,0,0, 0,0),
  "October" = c(0,0,0, 0,0),
  "time" = c(623, 624,625, 626,627),
  "lag1" = c(as.numeric(lag(logdata, 1)[626]), 5.91, 5.92, 6, 5.837),
  "lag3" = c(as.numeric(lag(logdata, 3)[626]), lag(logdata, 2)[626], lag(logdata, 1)[626], 5.91, 5.92),
  "lag4" = c(as.numeric(lag(logdata, 4)[626]), lag(logdata, 3)[626], lag(logdata, 2)[626], lag(logdata, 1)[626], 5.91),
  "structDummy" = c(1,1, 1, 1, 1)
  
)
July <- (predict(model, newdata = new.speeds3, interval = "predict", level = 0.95))


#point prediction for August
new.speeds3 <- data.frame(
  "January" = c(0,0,0, 0, 0, 0),
  "March" = c(1,0,0, 0, 0,0),
  "Feb" = c(0,0,0, 0, 0,0),
  "April" = c(0,1,0, 0, 0,0),
  "August" = c(0,0,0, 0, 0,0),
  "May" = c(0,0,1, 0,0,0),
  "June" = c(0,0,0, 0,0,0),
  "October" = c(0,0,0, 0,0,0),
  "time" = c(623, 624,625, 626,627,628),
  "lag1" = c(as.numeric(lag(logdata, 1)[626]), 5.91, 5.92, 6, 5.837, 5.8),
  "lag3" = c(as.numeric(lag(logdata, 3)[626]), lag(logdata, 2)[626], lag(logdata, 1)[626], 5.91, 5.92, 6),
  "lag4" = c(as.numeric(lag(logdata, 4)[626]), lag(logdata, 3)[626], lag(logdata, 2)[626], lag(logdata, 1)[626], 5.91, 5.92),
  "structDummy" = c(1,1, 1, 1, 1, 1)
  
)
August <- (predict(model, newdata = new.speeds3, interval = "predict", level = 0.95))

#point prediction for September
new.speeds3 <- data.frame(
  "January" = c(0,0,0,0,0,0,0),
  "March" = c(1,0,0,0,0,0,0),
  "Feb" = c(0,0,0,0,0,0,0),
  "April" = c(0,1,0,0,0,0,0),
  "August" = c(0,0,0,0,0,1,0),
  "May" = c(0,0,1,0,0,0,0),
  "June" = c(0,0,0,1,0,0,0),
  "October" = c(0,0,0,0,0,0,0),
  "time" = c(623, 624,625, 626,627,628, 629),
  "lag1" = c(as.numeric(lag(logdata, 1)[626]), 5.91, 5.92, 6, 5.837, 5.8, 5.8),
  "lag3" = c(as.numeric(lag(logdata, 3)[626]), lag(logdata, 2)[626], lag(logdata, 1)[626], 5.91, 5.92, 6, 5.8),
  "lag4" = c(as.numeric(lag(logdata, 4)[626]), lag(logdata, 3)[626], lag(logdata, 2)[626], lag(logdata, 1)[626], 5.91, 5.92, 6),
  "structDummy" = c(1,1, 1, 1, 1, 1, 1)
  
)

September <- (predict(model, newdata = new.speeds3, interval = "predict", level = 0.95))

a <- as.numeric(logdata)
a[627] <- log(data1[627])
plot(a, type = "l", xlim = c(500,650), ylab = "Log Autosales", main = "Logdata of Autosales with September Predictions and Realizations")
points(x = c(628:634), type = "l", y= September[,1], col = "green")



#interval for predictions
lastSigma <- fittedVals(coef(T.g), zeroMeanData)[626]
lastR2 <- as.numeric(zeroMeanData[626]^2)

omega <- coef(T.g)[2]
alpha <- coef(T.g)[3]
beta <- coef(T.g)[5]


#estimate conditional varianes via our Garch
sigmaPlusOne <- as.numeric(omega) + as.numeric(alpha * lastR2)  + as.numeric(beta * lastSigma)
sigmaPlusTwo <- as.numeric(omega) + as.numeric(alpha * (September[1,1] - 6.277925)^2)  + as.numeric(beta * sigmaPlusOne)
sigmaPlusThree <- as.numeric(omega) + as.numeric(alpha * (September[2,1] - 6.277925)^2)  + as.numeric(beta * sigmaPlusTwo)
sigmaPlusFour <- as.numeric(omega) + as.numeric(alpha * (September[3,1] - 6.277925)) ^2   + as.numeric(beta * sigmaPlusThree)
sigmaPlusFive <- as.numeric(omega) + as.numeric(alpha * (September[4,1] - 6.277925)) ^2  + as.numeric(beta * sigmaPlusFour)
sigmaPlusSix <- as.numeric(omega) + as.numeric(alpha * (September[5,1] - 6.277925)) ^2 + as.numeric(beta * sigmaPlusFive)
sigmaPlusSeven <- as.numeric(omega) + as.numeric(alpha * (September[6,1] - 6.277925)) ^2  + as.numeric(beta * sigmaPlusSix)

vars <- c(sigmaPlusOne, sigmaPlusTwo, sigmaPlusThree, sigmaPlusFour, sigmaPlusFive, sigmaPlusSix, sigmaPlusSeven)
means <- c(September[1,1], September[2,1], September[3,1], September[4,1], September[5,1], September[6,1], September[7,1])
for(i in c(1:7)){
  cat(means[i])
  cat(means[i] + 1.96 * sqrt(vars[i] -0.07))
  cat(means[i] - 1.96 * sqrt(vars[i] -0.07))
  print("")
}


#plot interval

M <- matrix(ncol=3, nrow = 7, byrow = TRUE)


M[1,] <- c(5.901246,  7.07693 ,  4.725561)
M[2,] <- c(5.911707, 6.797641 , 5.025772)
M[3,] <-  c(6.007806  ,6.794403  ,5.22121)
M[4,] <- c(5.940643  ,6.532123  ,5.349164)
M[5,] <- c(5.803632  ,6.414981  ,5.192284)
M[6,] <- c(5.843581  ,6.631583  ,5.055578)
M[7,] <- c(5.778404  ,6.55403   ,5.002778)

#plot interval
plot(a, ylim = c(4.5, 7.5), type = "l", xlim = c(500,650), ylab = "Log Autosales", main = "Logdata of Autosales with September Predictions and Realizations")
points(x = c(628:634), type = "l", y= M[,1], col = "green")
points(x = c(628:634), type = "l", y= M[,2], col = "orange")
points(x = c(628:634), type = "l", y= M[,3], col = "orange")



