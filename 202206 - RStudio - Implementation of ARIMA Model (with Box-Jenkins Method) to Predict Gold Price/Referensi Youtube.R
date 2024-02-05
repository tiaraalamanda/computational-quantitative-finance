mydata <- XAU_USD_Historical_Data_ab
class(mydata)
str(mydata)

tsdata <- ts(mydata)
class(tsdata)
str(tsdata)

summary(tsdata)

XAU_USD_Historical_Data_ab <- tsdata[,2]

#plot gold
plot(XAU_USD_Historical_Data_ab)
ts.plot(XAU_USD_Historical_Data_ab)
plot(diff(XAU_USD_Historical_Data_ab), main="GOLD PRice D=1")

#uji Stasioner (Uji ADF)
install.packages("tseries")
library(tseries)
adf.test(XAU_USD_Historical_Data_ab) 
adf.test(diff(XAU_USD_Historical_Data_ab)) #ini yg differencing
   
# =================IDENTIFIKASI MODEL================= #

par(mfrow=c(2,1))
acf(diff(XAU_USD_Historical_Data_ab))
pacf(diff(XAU_USD_Historical_Data_ab))

# =================Estimasi Model================= #
#ordonya dicoba coba intinya ngikutin lag dari acf dan pacf
install.packages("forecast")
library(forecast)
Arima.1 <-arima(XAU_USD_Historical_Data_ab, order = c(5,0,1))
Arima.2 <-arima(XAU_USD_Historical_Data_ab, order = c(1,5,0))
Arima.3 <-arima(XAU_USD_Historical_Data_ab, order = c(5,1,0))
Arima.4 <-arima(XAU_USD_Historical_Data_ab, order = c(5,1,5))
Arima.5 <-arima(XAU_USD_Historical_Data_ab, order = c(5,0,1))
Arima.6 <-arima(XAU_USD_Historical_Data_ab, order = c(5,2,5))
summary(Arima.1)
summary(Arima.2)
summary(Arima.3)
summary(Arima.4)
summary(Arima.5)
summary(Arima.6)

install.packages("lmtest")
library(lmtest)
coeftest(Arima.1)
coeftest(Arima.2)
coeftest(Arima.3)
coeftest(Arima.4)
coeftest(Arima.5)
coeftest(Arima.6)

# =================Diasnostic Casting================= #
#Cek Autokorelasi, akan baik jika ACF residual tdak melebihi garis batas untuk lag>0
# dan p-value untuk Ljung-Boxdi atas garis (run satu satu)
tsdiag(Arima.1) #baik
tsdiag(Arima.2) #baik
tsdiag(Arima.3) #baik
tsdiag(Arima.4) #baik
tsdiag(Arima.5) #baik
tsdiag(Arima.6) #paling baik

#menyimpan residual/sisaan tiap model
resid1 = Arima.1$residuals
resid2 = Arima.2$residuals
resid3 = Arima.3$residuals
resid4 = Arima.4$residuals
resid5 = Arima.5$residuals
resid6 = Arima.6$residuals

#Uji nilai tengah Residual (bagus jika p-value>alpha)
t.test(resid1, mu=0, alternative = "two.sided") #ok
t.test(resid2, mu=0, alternative = "two.sided") #ok
t.test(resid3, mu=0, alternative = "two.sided") #ok
t.test(resid4, mu=0, alternative = "two.sided") #ok
t.test(resid5, mu=0, alternative = "two.sided") #ok
t.test(resid6, mu=0, alternative = "two.sided") #ok

# =================Accuracy================= #
#semakin kecil errornya semakin baik
accuracy(Arima.1)
accuracy(Arima.2)
accuracy(Arima.3)
accuracy(Arima.4)
accuracy(Arima.5)
accuracy(Arima.6)

#dapat terlihat dari semua aspek error Arima 6 adalah yang memiliki 
#nilai lebih kecil sehingga arima 6 atau Arima(5,2,5) merupakan yang 
#terbaik untuk data ini

# =================Prediksi / Forecasting================= #
pred.data = predict(Arima.6, n.ahead=6) #predict 6 periode ke depan
pred.data

#plot
fit.data = fitted(Arima.6)
par(mfrow=c(1,1))
ts.plot(XAU_USD_Historical_Data_ab)
lines(fit.data, col="red")