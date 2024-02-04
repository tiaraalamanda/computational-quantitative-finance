#Pilih ada brp byk ttk yg ingin kita sebar
runs=10^5 #Misal 100000 ttk #sdh dicoba 10^3, hasilnya 3.11sekian
xs=runif(runs, min=-0.5, max=0.5) #Kita sdg memilih suatu persegi yg pjg sisi 1
ys=runif(runs, min=-0.5, max=0.5) #Ttp gmbr persegi, t stengah di Kuadran I stengah di Kuadran II
#Tentukan ttk mana saja yg ada di dlm lingk
in.circle<-xs^2+ys^2<=0.5^2 #Psgan (x,y) di dlm lingk dgn d=1
mc.pi=(sum(in.circle)/runs)*4 #Byk ttk dlm lingk/byk ttk dlm persegi
mc.pi
plot(xs,ys,pch='.',col=ifelse(in.circle,"blue","grey"),xlab='',
     ylab='',asp=1, main=paste("MC Approximation of Pi=",mc.pi))

### CARA LAIN:
# Fungsi untuk menghitung hampiran dari Pi dengan Monte Carlo
hampiran.pi <- function(iterasi) {
  # Bangkitkan 2 vektor berdistribusi U(0,1) untuk menyatakan titik2 acak dalam 1/4 lingkaran
  posisi.x <- runif(iterasi, min=0, max=1)
  posisi.y <- runif(iterasi, min=0, max=1)
  # Uji apakah titik2 tsb di dalam / di luar 1/4 lingkaran
  posisi.titik <- ifelse(posisi.x^2 + posisi.y^2 <= 1, TRUE, FALSE)
  titik.masuk <- length(which(posisi.titik == TRUE))
  # Estimasi Pi
  return(4*(titik.masuk/iterasi))
}


################################################
#menghitung integral monte carlo

#hitung secara 'langsung'(bagaimanapun R sama sprt maple, kalo htg integral dihtg numerik=yg ditampilkan hampiran)
integrate(function(x)sin(x)^2 + log(x), lower=1, upper=10)
#nilai integralnya: 18.52494 

#menaksir nilai integral dengan monte carlo
monte_int<-function(f, a, b, n=1e6){ #1e6 adlh 1jt=10^6
  x<-runif(n, min=a, max=b)
  return((b-a)*sum(f(x))/n)
}
monte_int(function(x)sin(x)^2+log(x), a=1, b=10)
#nilai integralnya: 18.52765
#Drtd penggunaan dgn distribusi uniform

#menaksir nilai integral dengan dist normal
n=10^6
sims=rnorm(n, mean=1, sd=10)
mc.integral=sum(sims>=3 & sims<=6)/n
mc.integral
#untuk n=10^4, diperoleh nilai integralnya adalah 0.1109
#untuk n=10^6, diperoleh nilai integralnya adalah 0.112277


###############################
#menaksir harga saham
#single path
days <- 200
changes <- rnorm(days,mean=1.001,sd=0.005)
plot(cumprod(c(20,changes)),type='l',ylab="Price",xlab="day", main="ABCD closing price (sample path)")

#simulasi 100.000 path
runs <- 100000
#simulates future movements and returns the closing price on day 200
generate.path <- function(){
  days <- 200
  changes <- rnorm(200,mean=1.001,sd=0.005)
  sample.path <- cumprod(c(20,changes))
  closing.price <- sample.path[days+1] #+1 because we add the opening price
  return(closing.price)
}

mc.closing <- replicate(runs,generate.path())
median(mc.closing) #24.36932
mean(mc.closing) #24.4328
quantile(mc.closing, 0.95) #27.37 #batas atas diambil di 95%
quantile(mc.closing, 0.05) #21.71 #batas atas diambil di 5%
