data("m.deciles08")

#This tells you that the data series is in a time series format
is.ts(m.deciles08)
## [1] FALSE
#We change the data to time series format
#STEP 2:
 # Now that we know that the data is time series we should do some data exploration. Functions print() and summary() are used to get the overview of the data. The start() and end() functions return the time index of the first and last observations, respectively. The time() function calculates a vector of time indices, with one element for each time index on which the series was observed. Finally, the frequency() function returns the number of observations per unit time.

#This will give us the structure of our data
print(m.deciles08)

summary(m.deciles08)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   104.0   180.0   265.5   280.3   360.5   622.0
#Starting index, end index
start(m.deciles08)
## [1] 1949    1
end(m.deciles08)

time(m.deciles08)

frequency(m.deciles08)


#Step 3:
 # It is essential to analyze the trends prior to building any kind of time series model. The details we are interested in pertains to any kind of trend, seasonality or random behaviour in the series. what better way to do so than visualize the Time Series.

#This will plot the time series
ts.plot(AirPassengers, xlab="v1", ylab="V3", main="Monthly totals of international airline passengers, 1949-1960")

# This will fit in a line
abline(reg=lm(m.deciles08~time(m.deciles08)))

#Auto correlation matrixx
acf(m.deciles08)

#Fit the AR model to the dataset
AR <- arima(m.deciles08, order = c(0,1,0))
print(AR)

#Plotting the AR model
ts.plot(m.deciles08)

#Fitting the model
AR_fit <- m.deciles08 - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)


#Using predict() to make a 1-step forecast
predict_AR <- predict(AR)

#Obtaining the 1-step forecast using $pred[1]
predict_AR$pred[1]

#ALternatively Using predict to make 1-step through 10-step forecasts
predict(AR, n.ahead = 10)

#plotting the data series plus the forecast and 95% prediction intervals
ts.plot(m.deciles08, xlim = c(1949, 1961))
AR_forecast <- predict(AR, n.ahead = 10)$pred
AR_forecast_se <- predict(AR, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)


#Fitting the MA model to the dataset
MA <- arima(m.deciles08, order = c(0,0,1))
print(MA)

#plotting the series along with the MA fitted values
ts.plot(m.deciles08)
MA_fit <- m.deciles08 - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)

#Making a 1-step forecast based on MA
predict_MA <- predict(MA)

#Obtaining the 1-step forecast using $pred[1]
predict_MA$pred[1]

#Alternately Making a 1-step through 10-step forecast based on MA
predict(MA,n.ahead=10)

#Plotting the m.deciles08 series plus the forecast and 95% prediction intervals
ts.plot(m.deciles08, xlim = c(1949, 1961))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)

#Choosing AR or MA: Exploiting ACF plots
# Find correlation between AR_fit and MA_fit
cor(AR_fit, MA_fit)

# Find AIC of AR
AIC(AR)
# Find AIC of MA
AIC(MA)
# Find BIC of AR
BIC(AR)

# Find BIC of MA
BIC(MA)