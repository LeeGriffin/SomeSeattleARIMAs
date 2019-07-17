#alright this is going to be my final
#up first is ARIMA model of housing index

#Packages 
library(tseries)
library(forecast)


#TS-data/Decomp
index <- c(SEXRNSA$SEXRNSA)
index.ts <- ts(index, frequency = 12)
plot(index.ts)
index.comp <- decompose(index.ts)
print(index.comp)

#you really only need this one
plot(index.comp)
#but to get each part
plot(index.comp$x)
plot(index.comp$seasonal)
plot(index.comp$trend)
plot(index.comp$random)

#ADFizzle test
adf.test(index.ts, alternative = "stationary")
#well would you look at that, non-stationary

acf(index.ts, main = "") # very slow linear decay
pacf(index.ts, main = "")

diff3.index.ts <- diff(index.ts, differences = 1)
diff3.index.ts <- na.omit(diff3.index.ts) 
plot(diff3.index.ts)
adf.test(diff3.index.ts, alternative = "stationary")
#alright, a p-value of less than 0.01 a first difference should wourk just fin

acf(diff3.index.ts, main="")
pacf(diff3.index.ts, main="")
#looks.......good...looking like some seasonal stuff

index.ts.arima <- auto.arima(index.ts, trace =TRUE)
print(index.ts.arima)
index.ts.forecast <- forecast(index.ts.arima, h=12)
plot(index.ts.forecast)

tsdisplay(residuals(index.ts.arima), lag.max=25, main='(2,1,2)(0,1,1)[12] Model Residuals')
#Just being honest here, not quite sure what I am looking for

#alright and that should be enough for my housing index
#lets try unemplyeeoment rate

unemp <- c(unempseattle$SEAT653URN)
plot(unemp, type = 'l')
unemp.ts <- ts(unemp, frequency = 12)
unemp.comp <- decompose(unemp.ts)
plot(unemp.comp)
#intresting, it seems that there is some pretty strong seasonal stuff 

adf.test(unemp.ts, alternative = "stationary")
#okay bot stationary but not far off 

acf(unemp.ts, main = "") #linear decay
pacf(unemp.ts, main = "")

diff1.unemp.ts <- diff(unemp.ts, differences = 1)

plot(diff1.unemp.ts)
adf.test(diff1.unemp.ts, alternative = "stationary")
#..... right under 0.1..... I guess we can call it stationary?

acf(diff1.unemp.ts, main = "") #maybe some slight seasonal stuff nothing linear
pacf(diff1.unemp.ts, main = "")

#lets try a secound difference 
diff2.unemp.ts <- diff(unemp.ts, differences = 2)
plot(diff2.unemp.ts)
adf.test(diff2.unemp.ts, alternative = "stationary") #below 0.01
acf(diff2.unemp.ts, main = "") #still seasonal, less (drastic) spikes 
pacf(diff2.unemp.ts, main = "")
#alright so it looks like 2nd difference is important 


unemp.ts.arima1 <- auto.arima(unemp.ts, trace =TRUE)
print(unemp.ts.arima1)
#hummm no differences
unemp.ts.forecast <- forecast(unemp.ts.arima, h=12)
plot(unemp.ts.forecast)
#so this is pretty good but maybe there is no differences ((2,0,2)(1,1,2)[12])

unemp.ts.arima2 <- auto.arima(unemp.ts, d=2, trace = TRUE)
print(unemp.ts.arima2)
unemp.ts.forecast2 <- forecast(unemp.ts.arima2, h=12)
plot(unemp.ts.forecast2)
#alright that loos a little better ((2,2,2)(1,1,1)[12])

tsdisplay(residuals(unemp.ts.arima2), lag.max=25, main='(2,2,2)(1,1,1)[12] Model Residuals')
#I still do not know what I am looking at, but these look better


#next time, finish this and then my last one will be CPI?
#alright lets do some CPI stuff 

#here is some cpi data --> this is nice and it gives me some good ideas
cpi <- c(Copy_of_Seattle_CPI_History_Bimonthly_013$Index)
cpi.ts <- ts(cpi, frequency = 6)
plot(cpi.ts)
cpi.comp <- decompose(cpi.ts)
plot(cpi.comp)
plot(cpi.comp$random)

adf.test(cpi.ts, alternative = 'stationary')
#not stationary
acf(cpi.ts, main = "")#linear slope 
pacf(cpi.ts, main = "")

diff1.cpi.ts <- diff(cpi.ts, differences = 1)
plot(diff1.cpi.ts)
adf.test(diff1.cpi.ts, alternative = "stationary")
#P-value 0.0618, yeah that should be fine....

acf(diff1.cpi.ts, main = "")
pacf(diff1.cpi.ts, main = "")
#Still going to play around with a secound difference

cpi.arima <- auto.arima(cpi.ts,  trace = TRUE)
print(cpi.arima)
cpi.forecast <- forecast(cpi.arima, h = 12)
plot(cpi.forecast)

tsdisplay(residuals(cpi.arima), lag.max=25, main='CPI (0,1,0)(2,0,0)[6] Model Residuals')



cpi.arima2 <- auto.arima(cpi.ts, d=1, D=1,  trace = TRUE)
print(cpi.arima2)
cpi.forecast2 <- forecast(cpi.arima2, h = 12)
plot(cpi.forecast2)
print(cpi.forecast2)
#Lower BIC,AIC,and Sigma^2

tsdisplay(residuals(cpi.arima2), lag.max=25, main='CPI (2,1,2)(1,1,1)[6] Model Residuals')
#really does not look all that difference than the one above
#I can test later


#then clean up --> I think it would be best to clean all of this up on the mac --> see if I can get it all right and stuff 
#adding dates --> yeah I can just do that later.... if I want to
