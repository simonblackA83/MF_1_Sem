
#Script Exercises ACF/PACF MA(1) Process

len = 200
set.seed(100)
x1 = arima.sim(n = len, list(ma = 0.9, sd = 10))

a1<-0.9
y_1<-rep(0,len)
eps<-rnorm(len)
for (i in 2:len)
   {
     y_1[i]<-a1*y_1[i-1]+eps[i]
     }


par(mfrow=c(2,1))
acf(y_1); pacf(y_1)


###################### Exercise 2 ##########

estim<-function(parma,x)
{
  # Center the data
  x<-x-mean(x)
  # Determine length
  len<-length(x)
  # Define q
  q<-length(parma)
  b<-parma
  # Define a vector of epsilon's
  epsilon<-x
  # Compute epsilon_1
  epsilon[1]<-x[1]
  for (i in 2:q)
  {
    epsilon[i]<-x[i]-epsilon[(i-1):1]%*%b[1:(i-1)]
  }
  for (i in (q+1):len)
  {
    epsilon[i]<-x[i]-epsilon[(i-1):(i-q)]%*%b[1:q]
  }
  return(sum(epsilon^2))
}

######################  Data ######################################################

path = 'C:/Users/41799/OneDrive - ZHAW/Time_Series1/'
file = 'ma50.txt'
ma50 = read.table(paste(path,file, sep = ''))#[,3]
file = 'ma300.txt'
ma300 = read.table(paste(path,file, sep = ''))#[,2]

######################################################

ts.plot(ma50,lty=1:4)

par(mfrow=c(2,1))
acf(ma50)
acf(ma50,type="partial")

# init: white noise
maorder = 2

parma<-rep(0, maorder)
estim_obj<-nlminb(parma,estim,x= ma50)
round(estim_obj$par,4)

x_obj<-arima(ma50, order=c(0,0, maorder))
x_obj$coef

abs(estim_obj$par-x_obj$coef[1])<1.96*sqrt(diag(x_obj$var.coef)[1])

acf(x_obj$residuals)
tsdiag(x_obj,gof.lag=50)


##### ma50 X1####
acf(ma50$V1)#MA(2) or MA(1)
maorder = 1
x_obj<-arima(ma50$V1, order=c(0,0, maorder))
acf(x_obj$residuals)
tsdiag(x_obj,gof.lag=50)

##### ma50 X2####
acf(ma50$V2)#MA(1)
x_obj<-arima(ma50$V2, order=c(0,0, 1))
acf(x_obj$residuals)
tsdiag(x_obj,gof.lag=50)


##### ma50 X4####
acf(ma50$V4)#MA(1) or MA(2) or MA(3)

#MA(2) just works
x_obj<-arima(ma50$V4, order=c(0,0, 2))
acf(x_obj$residuals)
tsdiag(x_obj,gof.lag=50)

#MA(3) not much better
x_obj<-arima(ma50$V4, order=c(0,0, 3))
acf(x_obj$residuals)
tsdiag(x_obj,gof.lag=50)

#MA(4) safe
x_obj<-arima(ma50$V4, order=c(0,0, 4))
acf(x_obj$residuals)
tsdiag(x_obj,gof.lag=50)




