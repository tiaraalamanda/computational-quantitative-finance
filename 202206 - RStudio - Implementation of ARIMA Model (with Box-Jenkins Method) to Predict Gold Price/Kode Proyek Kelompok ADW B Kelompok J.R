#[PROYEK KELOMPOK J - ANALISIS DERET WAKTU KELAS B]
#6161901114 - Bilqis Aulia Rahma S
#6161901116 - Tiara Alamanda (Ketua)
#6161901120 - Abel Angelo
#6161901129 - Aspira Rahmadini Purnamasari
###############################################
# =================LANGKAH 1================= #
#Penggambaran Nilai "Price" dari 30/04/2019 - 29/04-2022
XAUUSD_data <- XAU_USD
Price <- ts(XAUUSD_data[,2])
plot(Price, main = "Harga Penutupan Emas pada Mei 2019 hingga April 2022", xlab = "Hari", ylab = "Harga Penutupan ($)")
#Dapat terlihat tampilan "Price" dari periode waktu tersebut diduga tidak stasioner

#Uji ADF
install.packages("tseries")
library(tseries)
adf.test(Price)
#Karena p-value hasil uji ADF = 0.8155 > 0.05, maka terima H0: Data tidak stasioner

# =================LANGKAH 2================= #
#Differencing Price
Price_diff <- diff(Price)
plot(Price_diff, main = "Price d=1", xlab = "Hari", ylab = "Harga Penutupan ($)")

#Uji ADF Price_Differencing
install.packages("tseries")
library(tseries)
adf.test(Price_diff)
#Karena p-value hasil uji ADF = 0.01 < 0.05, maka tolak H0: Data tidak stasioner dan terima H1: Data stasioner
#Dengan begitu, kita memilih nilai d = 1

###############################################

# =================LANGKAH 3================= #
#Identifikasi Model
par(mfrow=c(2,1))
acf(Price_diff)
pacf(Price_diff)
#Pada gambar ACF, dapat terlihat nilai pada lag 5, 18, dan 22 melebihi batas garis signifikansi.
#Dengan begitu, dapat disimpulkan Price_diff memiliki calon koefisien MA sebesar q = 5 atau 18 atau 22.
#Pada gambar PACF, dapat terlihat nilai pada lag 4, 5, 7, 18, dan 23 melebihi batas garis signifikansi.
#Dengan begitu, dapat disimpulkan Price_diff memiliki calon koefisien AR sebesar p = 4 atau 5 atau 7 atau 18 atau 23.

#Estimasi Model
install.packages("forecast")
library("forecast")
Price_model <- auto.arima(Price,trace=TRUE)

#Calon Model
Price.212 <- arima(Price, order=c(2,1,2))
Price.010 <- arima(Price, order=c(0,1,0))
Price.110 <- arima(Price, order=c(1,1,0))
Price.011 <- arima(Price, order=c(0,1,1))
Price.111 <- arima(Price, order=c(1,1,1))

#Test Signifikansi Koefisien Calon Model
install.packages("lmtest")
library(lmtest)
coeftest(Price.212)
coeftest(Price.010)
coeftest(Price.110)
coeftest(Price.011)
coeftest(Price.111)
#Karena p-value yang signifikan (<0.05) hanya p-value dari calon model ARIMA(2,1,2), maka model tersebut yang dipilih.

###############################################

# =================LANGKAH 4================= #
#Cek Diagnostik
tsdiag(Price.010)
win.graph(width=8,height=4)
par(mfrow=c(1,2))

tsdiag(Price.212)
win.graph(width=8,height=4)
par(mfrow=c(1,2))
#Pada grafik ACF of Residuals, ada beberapa nilai yang melebihi batas garis signifikansi yaitu di lag 4, 5, 18, dan 22.
#Karena pada grafik p-values for Ljung-Box Statistic seluruh nilainya berada di atas batas gatis signifikansi,
#maka beberapa nilai yang melebihi batas garis pada grafik ACF of Residuals dapat dianggap tidak ada.
#Berarti dapat disimpulkan bahwa tidak ada autokorelasi antar koefisien berdasarkan cek diagnostik 

#Uji Nilai Tengah Residual
resid.212 <- Price.212$residuals
t.test(resid.212, mu = 0, alternative = "two.sided")
#Karena p-value hasil uji nilai tengah residual ARIMA(2,1,2) sebesar = 0.2045 > 0.05, maka benar bahwa nilai tengah residual nya sudah = 0.
resid.010 <- Price.010$residuals
t.test(resid.010, mu = 0, alternative = "two.sided")
#Kelompok kami meelakukan uji nilai tengah residual untuk model ARIMA(0,1,0) karena berdasarkan bagian #Estimasi Model
#model ARIMA(0,1,0) diusulkan, walaupun saat di #Cek Diagnostik model ARIMA(0,1,0) kurang cocok, tetap perlu di inputkan
#sebagai pembanding dengan model ARIMA(2,1,2) yang dipilih.
#Karena p-value hasil uji nilai tengah residual ARIMA(0,1,0) sebesar = 0.1979 > 0.05, maka benar bahwa nilai tengah residual nya sudah = 0.

# =================LANGKAH 5================= #
#Perbandingan Akurasi Error

install.packages("forecast")
library(forecast)
accuracy(Price.212)
accuracy(Price.010)
#Karena ME, RMSE, dst (alias error secara statistik) ARIMA(2,1,2) lebih kecil, maka model itu yang dipilih.

#Histogram
hist(residuals(Price.212))
qqnorm(residuals(Price.212))
qqline(residuals(Price.212))
shapiro.test(residuals(Price.212))

###############################################

# =================LANGKAH 6================= #
#Forecasting
install.packages("forecast")
library("forecast")

Price_forecast<-forecast(arima(XAUUSD_data[,2], order=c(2,1,2), seasonal=list(order=c(2,1,2))),level=c(95),h=30)
Price_forecast
plot(Price_forecast)
