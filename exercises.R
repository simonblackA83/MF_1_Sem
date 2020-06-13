#Exercise 1
#X = -0.7et_1 +et
#X = 0.5et_5 +et

set.seed(71)
len = 100
x1 = arima.sim(list(ma=-0.7), n=len, sd = 1)
x5 = arima.sim(list(ma= 0.5), n=len, sd = 1)

ts.plot(x1); ts.plot(x5)

acf(x1, lag.max = len)
acf(x5, lag.max = len)

sigmas = c(1, 3, 10, 15, 20)


y_hat = function(x){
  return(mean(x))
}
y_hat(x1)

y_var = function(x){
  return(sapply(x, function(i) (i - y_hat(x))**2))
}
y_var(x1)

#ACF Formula -> Autocovariance Function at lag k:

       
       #x is ts and k is lag
auto_cov = function(x, k = 1){
  
  res = sapply(x, function(i) i - y_hat(x))
  M = matrix(, nrow = length(x))
  for (i in 1:length(x)){
    
    M[i] = (res[i]*res[i+k])
  }
  return(M)}

acf_x1 = mean(auto_cov(x1) / y_var(x1))

mean(acf(x1, lag.max = 100)$acf[2:100])


M = matrix(, ncol = length(sigmas), nrow = length(x1))
for (i in 1:length(sigmas)){
  #M[,i] = acf(arima.sim(list(ma = -0.7), n = len, sd = sigmas[i]))$acf
  set.seed(72)
  M[,i] = (acf(arima.sim(list(ma = -0.7), n = len, sd = sigmas[i]), lag.max = len)$acf)
}


  
test = arima.sim(list(ma = -0.7), n = 10**6, sd = 1)
mean(test)

#True ACF
ARMAacf( ma = -0.7, lag.max = 10, pacf = FALSE)

#Parameter estimation and model identification

df = read.table('C:/Users/41799/ZHAW/OEKO 1 - Kursmaterialien/16.02.2020/ma50.txt')
x = df[,3]

acf(x, lag.max =50)#suggest MA 2
pacf(x, lag.max = 50)#suggest AR 2

# Estimation


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


# initialization: white noise
maorder = 2

parma<-rep(0,maorder)
estim_obj<-nlminb(parma,estim,x=x)
round(estim_obj$par,4)

x_obj<-arima(x,order=c(0,0,maorder))

x_obj$coef

# Note that arima estimates the intercept too whereas we estimate the MA-parameters only: therefore
# both estimates will generally differ.
# We check that the differences are within the confidence interval of the estimates

abs(estim_obj$par-x_obj$coef[1])<1.96*sqrt(diag(x_obj$var.coef)[1])

####
acf(x_obj$residuals)

tsdiag(x_obj,gof.lag=50)


ARMAacf( ma = b_1, lag.max = 10, pacf = FALSE)

b_1/(1+b_1**2)



#2.2
#xt = et + 1/0.9 et

#1/.9 > 1 -> not invertible
#is always stationary
# mean is ZERO
#Var is: 
sigma = 1
(sigma**2) * (1 + (1/.9)**2)
#ACF:
b = 1/.9
b/(1 + b**2)

#2.4
sig_2 = 1
len = 100
set.seed(10)
b1_1 = -0.7
b1_5 = 0.5
x1 = arima.sim(list(ma = b1_1), n = len, sd = sqrt(sig_2))
x5 = arima.sim(list(ma = c(rep(0, 4), b1_5)), n=len, sd = sqrt(sig_2))

B_mat_1 = 0.7


B_mat_5 = matrix(nrow=5, ncol=5)
B_mat_5[1,] = -c(rep(0,4),-b1_5)
B_mat_5[2:dim(B_mat_5)[1],1:4] <- diag(rep(1, dim(B_mat_5)[1]-1))
B_mat_5[2:5,5]<-0
B_mat_5

# Invertibility
abs(eigen(B_mat_5)$values) < 1

#Model Order by looking at ACF?
acf(x5, max.lag = 50)# MA(3)
pacf(x5)#AR(2) or AR(4); usually less parms better

# We select 5: estimate empirical models

y_obj = arima(x5, order=c(0,0,5))
y_obj$coef
# Check diagnostics
tsdiag(y_obj)

# Define empirical matrix B
Bhat_mat_5 = B_mat_5
Bhat_mat_5[1, ] = -y_obj$coef[1:5]

# Check invertibility of empirical model
abs(eigen(Bhat_mat_5)$values) < 1

# Compute weights of AR-representation
B_k = diag(rep(1, dim(Bhat_mat_5)[1]))
Bk_11 = 0:110
for (i in 0:110)
{
  Bk_11[i+1] = B_k[1,1]
  B_k = B_k %*% Bhat_mat_5
}

ts.plot(Bk_11)

# Compute 1-10 steps ahead forecasts
h = 10
yhat = 1:h
# One-step ahead forecast
yhat[1] = -Bk_11[101:2] %*% (x5 - y_obj$coef[6])+y_obj$coef[6]
yfor <- x5
for (i in 2:h)
{
  # We append recursively the last forecast to y
  yfor = c(yfor, yhat[i-1])
  # we apply the multi-step ahead forecast function to the centered y
  yhat[i] = -Bk_11[(length(x5)+i):2] %*% (yfor - y_obj$coef[6]) + y_obj$coef[6]
  if (i==h)
    yfor = c(yfor, yhat[i])
  
}

# Compare forecasts: slight differences are due to the fact that predict
# relies on a state-space model representation of the process
yhat
predict(y_obj, n.ahead = 10)
 


#2.1
#x = e + 0.9e
var = 4; std = var**0.5; b1 = 0.9

#is invertible: Yes, Ib1I < 1
#all MA are stationary
#Mean: 0
#Variance:
R0 = var*(1 + b1**2)
#ACF:
b1/(1+b1**2)
ARMAacf( ma = b1, lag.max = 10, pacf = FALSE)

# Express process as AR(4)+rest

# AR-weights: convergent
-(-b1)^(1:4)
# Rest: weight of epsilon_{t-5} (converges to zero exponentially fast)
(b1)^5

# Variance of rest-term

std^2*(b1)^(10)

# Ratio of variances

std^2*(b1)^(10)/R0

# The size of the variance is determined by sigma^2, by b_1 and by the AR-order
# The ratio is determined by b_1 and by the AR-order only (sigma^2 cancels)


# By increasing the model order of the AR-approximation the ratio of variances
# can be made arbitrarily small: thus AR-processes can approximate the MA-process
# arbitrarily well. But one may need more or less large model-orders depending on abs(b_1) being close (or not) to 1.

#3.1 : xt = 6 + 1.6xt???1 ??? 1.4xt???2 + 0.5xt???3 + t

con = 6; b = c(1.6, -1.4, 0.5); var = 1; std = var**(1/2)
# Specify the system-matrix A_mat
A_mat = matrix(ncol = length(b), nrow = length(b))
A_mat[1, ] = b
A_mat[2:dim(A_mat)[1], 1:(dim(A_mat)[1] - 1)] = diag(rep(1, dim(A_mat)[1] - 1))
A_mat[2:dim(A_mat)[1], dim(A_mat)[1]] = 0
A_mat

#3.1.b Stationary
abs(eigen(A_mat)$values) < 1
abs(polyroot(c(-b[length(b):1], 1)))

#3.1c
mean_AR_p = function(constant, betas){return (constant / (1 - sum(betas)))}
mean_AR_p(con, b)

#3.1d

# MA-inversion
A_rec = diag(rep(1,length(b)))
len = 100
# a_weight collects the weights (1,1)-elements of A_mat^k
a_weight = A_rec[1,1]

for (i in 1:len) #i<-2
{
  
  # We compute A_mat^k
  A_rec = A_rec %*% A_mat
  # and store the weight of the MA-inversion
  a_weight = c(a_weight, A_rec[1,1])
}

ts.plot(a_weight)

# Exercise 1.e

# All weights of A_rec=A^{100} are close to vanishing. Therefore the rest-term
# must be vanishingly small too.
A_rec
abs(eigen(A_rec)$values)

#3.2
df = read.table('C:/Users/41799/OneDrive - ZHAW/Time_Series1/exercise_3.txt')

#Series 1
par(mfrow = c(2,1))
acf(df$V1, lag.max = 30); pacf(df$V1, lag.max = 30)#MA(3) , AR(6)

y_ma_3 = arima(df$V1, order=c(0,0,3))
tsdiag(y_ma_3, gof.lag=20)
tsdiag(y_ma_3, gof.lag=200)

# Diagnostics for an AR(6) are OK too.
y_ar<-arima(df$V1,order=c(6,0,0))
tsdiag(y_ar)


#3.3a xt = 0.95xt???1 + et
set.seed(10)
x1 = arima.sim(list(ar = 0.95), n = 100)
par(mfrow = c(2,1))
acf(x1, lag.max =  40);pacf(x1, lag.max = 40)#go with AR(1)

xar_obj = arima(x1, order=c(1,0,0))
tsdiag(xar_obj)

#3.3b) go with MA(6)
xma_obj = arima(x1, order=c(0,0,6))
tsdiag(xma_obj)


#3c) 10 step forecast with comparison of errors
sim_n = 100; h = 10
ar_err = matrix(nrow = h, ncol = sim_n); ma_err = ar_err
len = 100
set.seed(10)

for (i in 1:sim_n){
  
  x = arima.sim(list(order = c(1,0,0), ar=0.95), n = len+h)
  y_ar = arima(x[1:len], order = c(1, 0, 0))
  y_ma = arima(x[1:len], order = c(0, 0, 6))
  
  ar = predict(y_ar, n.ahead = h)$pred
  ma = predict(y_ma, n.ahead = h)$pred
  
  ar_err[,i] = (ar - x[(length(x)-h + 1):length(x)])**2
  ma_err[,i] = (ma - x[(length(x)-h + 1):length(x)])**2
  
}

ar_row_mean = apply(ar_err, 1, mean)
ma_row_mean = apply(ma_err, 1, mean)

ar_row_mean < ma_row_mean

#4.1 a) xt = 6 + 1.6xt???1 ??? 1.4xt???2 + 0.5xt???3 + et

c = 6; b = c(1.6, -1.4, 0.5); var = 1; std = sqrt(var)
M = matrix(nrow = length(b), ncol = length(b))
M[1,] = b
M[2:length(b), 1:length(b) - 1] = diag(rep(1, dim(M)[1] - 1))
M[2:length(b), length(b)] = 0

AkA = kronecker(M, M)
AkA
vec_sigma<-c(std^2, rep(0,dim(AkA)[2]-1))
vec_sigma
# Ricatti equation
R_mat_0_vec<-solve(diag(rep(1,dim(AkA)[2]))-AkA)%*%vec_sigma
R_mat_0<-matrix(R_mat_0_vec,ncol=dim(A_mat)[2])
R_mat_0

len_R<-100
R_mat_k<-R_mat_0
R_k<-rep(0,len_R)
for (i in 0:(len_R-1))
{
  R_k[i+1]<-R_mat_k[1,1]
  R_mat_k<-A_mat%*%R_mat_k
}

# Exercise 1.a
sigma<-1
a_vec<-c(1.6,-1.4,0.5)
# Specify the system-matrix A_mat
A_mat<-matrix(ncol=length(a_vec),nrow=length(a_vec))
A_mat[1,]<-a_vec
A_mat[2:length(a_vec),1:(length(a_vec)-1)]<-diag(rep(1,length(a_vec)-1))
A_mat[2:length(a_vec),length(a_vec)]<-0
A_mat

AkA<-kronecker(A_mat,A_mat)
AkA
vec_sigma<-c(sigma^2,rep(0,dim(AkA)[2]-1))
vec_sigma
# Ricatti equation
R_mat_0_vec<-solve(diag(rep(1,dim(AkA)[2]))-AkA)%*%vec_sigma
R_mat_0<-matrix(R_mat_0_vec,ncol=dim(A_mat)[2])
R_mat_0

len_R<-100
R_mat_k<-R_mat_0
R_k<-rep(0,len_R)
for (i in 0:(len_R-1))
{
  R_k[i+1]<-R_mat_k[1,1]
  R_mat_k<-A_mat%*%R_mat_k
}

# Exercise 1.b

len<-100000
x<-arima.sim(n=len,list(ar=a_vec))

# Exercise 1.c

acf_true<-R_k/R_mat_0[1,1]

ts.plot(acf(x,plot=F,lag.max=100)$acf,xlab="",ylab="")
lines(acf_true,col="blue")
lines(rep(2/sqrt(len),len),lty=2)
lines(rep(-2/sqrt(len),len),lty=2)
lines(rep(0,len))
mtext("Sample acf", side = 3, line = -1,at=length(acf(x,plot=F)$acf)/2,col="black")
mtext("True acf", side = 3, line = -2,at=length(acf(x,plot=F)$acf)/2,col="blue")


#4.3
#Generate a realization of length 100 of the above AR(3)-process
set.seed(10)
len = 100
x = arima.sim(n = len, list(ar = b))
par(mfrow = c(2,1))
acf(x, lag.max = 50); pacf(x, lag.max = 50)#AR(4) MA ???? no use
ar_4 = arima(x, order = c(4, 0, 0))
tsdiag(ar_4)
#AR 3 also works -> parsimony
ar_3 = arima(x, order = c(3, 0, 0))
tsdiag(ar_3) 

#MA inversion o: xt = 6 + 1.6xt???1 ??? 1.4xt???2 + 0.5xt???3 + et
con = 6; b = c(1.6, -1.4, 0.5); var = 1; std = var**(1/2)
# Specify the system-matrix A_mat
A_mat = matrix(ncol = length(b), nrow = length(b))
A_mat[1, ] = b
A_mat[2:dim(A_mat)[1], 1:(dim(A_mat)[1] - 1)] = diag(rep(1, dim(A_mat)[1] - 1))
A_mat[2:dim(A_mat)[1], dim(A_mat)[1]] = 0
A_mat

# Stationary
abs(eigen(A_mat)$values) < 1
abs(polyroot(c(-b[length(b):1], 1)))


# out-of-sample
simanz<-200
h<-10
forecast_error_ma_out<-matrix(nrow=h,ncol=simanz)
forecast_error_ar_out<-forecast_error_ma_out
forecast_error_ar6_out<-forecast_error_ma_out
set.seed(10)

for (i in 1:simanz)        #i<-1
{
  x<-arima.sim(list(ar=a_vec),n=len+h)
  y_ma<-arima(x[1:len],order=c(0,0,6))
  y_ar<-arima(x[1:len],order=c(3,0,0))
  y_ar6<-arima(x[1:len],order=c(6,0,0))
  forecast_error_ma_out[,i]<-(predict(y_ma,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error_ar_out[,i]<-(predict(y_ar,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error_ar6_out[,i]<-(predict(y_ar6,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  print(i)
}

out_of_sample<-cbind(apply(forecast_error_ma_out,1,mean),apply(forecast_error_ar_out,1,mean),apply(forecast_error_ar6_out,1,mean))
ts.plot(out_of_sample[,1],xlab="Forecast horizon",ylab="MSFE",main="Out-of-sample")
lines(out_of_sample[,2],col="blue")
lines(out_of_sample[,3],col="red")
mtext("MA(6)", side = 3, line = -1,at=h/2,col="black")
mtext("AR(3)", side = 3, line = -2,at=h/2,col="blue")
mtext("AR(6)", side = 3, line = -3,at=h/2,col="red")



# in sample
simanz<-200
h<-10
forecast_error_ma_in<-matrix(nrow=h,ncol=simanz)
forecast_error_ar_in<-forecast_error_ma_in
forecast_error_ar6_in<-forecast_error_ma_in
set.seed(10)

for (i in 1:simanz)        #i<-1
{
  x<-arima.sim(list(ar=a_vec),n=len+h)
  y_ma<-arima(x,order=c(0,0,6))
  y_ar<-arima(x,order=c(3,0,0))
  y_ar6<-arima(x,order=c(6,0,0))
  
  
  forecast_error_ma_in[,i]<-(predict(y_ma,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error_ar_in[,i]<-(predict(y_ar,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error_ar6_in[,i]<-(predict(y_ar6,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  print(i)
}

in_sample<-cbind(apply(forecast_error_ma_in,1,mean),apply(forecast_error_ar_in,1,mean),apply(forecast_error_ar6_in,1,mean))
ts.plot(in_sample[,1],xlab="Forecast horizon",ylab="MSFE",main="In-sample")
lines(in_sample[,2],col="blue")
lines(in_sample[,3],col="red")
mtext("MA(6)", side = 3, line = -1,at=h/2,col="black")
mtext("AR(3)", side = 3, line = -2,at=h/2,col="blue")
mtext("AR(6)", side = 3, line = -3,at=h/2,col="red")

y_ma$x

#########  Ex. 5.1 ARMA xt = 6 + 1.6xt???1 ??? 0.9xt???2 + et ??? 0.3et???1 + 0.4et???2 ######## 

#Mean (mu)
ar_vec = c(1.6, -0.9); ma_vec = c(-0.3, 0.4)
sigma = 1
mu = mean_AR_p(c, ar_vec)#20

r = max(c(length(ar_vec), length(ma_vec) + 1))
A = matrix(nrow = r, ncol = r)
A[,1] = c(ar_vec, 0)
A[1:dim(A)[1] - 1, 2:dim(A)[1]] = diag(rep(1,dim(A)[1]-1))
A[dim(A)[1], ] = 0

B = matrix(nrow = r, ncol = r)
B[,1] = c(-ma_vec, 0)
B[1:dim(B)[1] - 1, 2:dim(B)[1]] = diag(rep(1,dim(B)[1]-1))
B[dim(A)[1], ] = 0




arma_inv_matrix = function(ar_vector, ma_vector){
  
  r = max(c(length(ar_vector), length(ma_vector) + 1))
  
  #AR
  A = matrix(nrow = r, ncol = r)
  A[,1] = c(ar_vector, 0)
  A[1:dim(A)[1] - 1, 2:dim(A)[1]] = diag(rep(1,dim(A)[1]-1))
  A[dim(A)[1], ] = 0
  #MA
  B = matrix(nrow = r, ncol = r)
  B[,1] = c(-ma_vector, 0)
  B[1:dim(B)[1] - 1, 2:dim(B)[1]] = diag(rep(1,dim(B)[1]-1))
  B[dim(A)[1], ] = 0
  
  matrices = list('A' = A, 'B' = B)
  
  return(matrices)
}

matrices = arma_inv_matrix(c(1.6, -0.9), ma_vec = c(-0.3, 0.4))

#Stationary / Invertible
abs(eigen(matrices$B)$values) < 1#Stationary
abs(eigen(matrices$A)$values) < 1#Invertible

# Exercise 1.c

len<-100
d_k<-rep(1:len)
A_k<-matrices$A
for (i in 2:len)#i<-1
{
  d_k[i]<-(A_k%*%c(1, ma_vec))[1]
  A_k<-A_k%*%matrices$A
  
}

ts.plot(d_k)
# Check weights with function ARMAtoMA
ARMAtoMA(ar = ar_vec, ma = ma_vec, lag.max=10)
d_k[1:10]

Rk<-1:11

for (i in 1:length(Rk))#i<-1
{
  Rk[i]<-d_k[i:len]%*%d_k[1:(len-i+1)]
}

Rk/Rk[1]
ARMAacf(ar = -a_vec[2:3], ma = b_vec[2:3], lag.max = 10, pacf = FALSE)



#given Solution
# Exercise 1.a
sigma<-1
a_vec<-c(1,-1.6,0.9)


6/(sum(a_vec))

# Exercise 1.b
b_vec<-c(1,-0.3,0.4)
# Specify the system-matrices A_mat and B_mat
A_mat<-matrix(ncol=length(a_vec),nrow=length(a_vec))
A_mat[,1]<-c(-a_vec[2:3],0)
A_mat[1:(length(a_vec)-1),2:length(a_vec)]<-
  diag(rep(1,length(a_vec)-1))
A_mat[length(a_vec),2:length(a_vec)]<-0
A_mat

B_mat<-matrix(ncol=length(b_vec),nrow=length(b_vec))
B_mat[,1]<-c(-b_vec[2:3],0)
B_mat[1:(length(b_vec)-1),2:length(b_vec)]<-diag(rep(1,length(b_vec)-1))
B_mat[length(b_vec),2:length(b_vec)]<-0
B_mat



# Check stationarity and Invertibility

abs(eigen(A_mat)$values)
abs(eigen(B_mat)$values)

# Exercise 1.c

len<-100
d_k<-rep(1:len)
A_k<-A_mat
for (i in 2:len)#i<-1
{
  d_k[i]<-(A_k%*%b_vec)[1]
  A_k<-A_k%*%A_mat
  
}

ts.plot(d_k)
# Check weights with function ARMAtoMA
ARMAtoMA(ar = -a_vec[2:3], ma = b_vec[2:3], lag.max=10)
d_k[1:10]

Rk<-1:11

for (i in 1:length(Rk))#i<-1
{
  Rk[i]<-d_k[i:len]%*%d_k[1:(len-i+1)]
}

Rk/Rk[1]
ARMAacf(ar = -a_vec[2:3], ma = b_vec[2:3], lag.max = 10, pacf = FALSE)



######## Exercise 5.2 ######## 

y_data = read.table("C:/Users/41799/OneDrive - ZHAW/Time_Series1/y_data_exercise5.txt")
dim(y_data)
ts.plot(y_data)

x = y_data[,3]
par(mfrow = c(2,1))
acf(x, lag.max = 40)
pacf(x, lag.max = 40)

arorder<-1
maorder<-2

x_obj<-arima(x,order=c(arorder,0,maorder))

tsdiag(x_obj)

# question how ???????????

######### Ex. 5.3 Normal Form ? ######### 


polyroot(c(1, -1.2, 0.36))
polyroot(c(1, -0.5, -0.06))

# 1. Beispiel
# Der Operator (1-0.5B) kürzt sich auf beiden Seiten d.h. der Prozess ist
# x_t=C+epsilon_t
# Der Erwartungswert des Prozesses ist 2/(1-0.5)=4 d.h. C=4
# bzw. die Normalform lautet: X_t=4+epsilon_t

# 2. Beispiel
# Prozess ist bereits in Normalform


# 3. Beispiel
polyroot(c(0.2,-0.9,1))
# d.h. das Polynom ist (1-0.4B)(1-0.5B). Der Term 1-0.5B kürzt sich auf beiden Seiten
# und es bleibt (1-0.4B)X_t=C+epsilon_t
# Das C wird wie folgt berechnet:
#  -Der Erwartungswert des Prozesses ist 2/(1-0.9+0.2)=6.666...
#  -C ist dann: C/(1-0.4)=6.666 bzw. C=4

# 4. Beispiel
polyroot(c(0.25,-1,1))
polyroot(c(0.1,-0.7,1))
# d.h. das AR-Polynom ist (1-0.5B)(1-0.5B) und das MA-Polynom ist
# (1-0.2B)(1-0.5B)
# Nach kürzen von (1-0.5B) bleibt der ARMA(1,1) übrig:
# (1-0.5B)X_t=C+(1-0.2B)epsilon_t
# Dabei ergibt sich C wie folgt:
#  -der Erwartungswert des Prozesses ist  2/(1-1+0.25)=8
#  -Deshalb ist C/(1-0.5)=8 bzw. C=8*(1-0.5)=4




##### AIC Automation ###########

#AR(2) process: 10 + 1.7x_1 - 0.8x_2, var = 3
set.seed(11)
len<-100
sigma<-sqrt(3)
a_1<-1.7
a_2<--0.8
c<-10
mu<-c/(1-a_1-a_2)
y<-mu+arima.sim(n = len, list( ar = c(a_1,a_2)), sd = sigma)

acf(y,type="partial")#AR(2) or AR(3)

maxorder<-10
sigma_k<-rep(0,maxorder+1)
# Order zero
sigma_k[1]<-var(y)
for (k in 1:maxorder) #k<-10
  {
    AR_obj<-arima(y, order=c(k,0,0))
    sigma_k[k+1]<-mean(AR_obj$res^2)
    # This is the same as
    #sigma_k[k+1]<-AR_obj$sigma
    print(paste("AR-Order=",k," AIC=",AR_obj$aic,sep=""))
    }

y<-y_data[,3]
ts.plot(y)
len<-length(y)

par(mfrow=c(2,1))
acf(y)
acf(y,type="partial")


y_data = read.table("C:/Users/41799/OneDrive - ZHAW/Time_Series1/y_data_exercise5.txt")
y = y_data[,3]
maxarorder<-5
maxmaorder<-5
sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                 ncol=maxmaorder+1,nrow=maxarorder+1)
# Order (zero,zero)
sigma_jk[1,1]<-var(y)
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)         #k<-4  j<-4
  {
    ARMA_obj<-arima(y,order=c(j,0,k))
    sigma_jk[j+1,k+1]<-ARMA_obj$sigma
        print(paste("  AR-order=",j,"MA-Order=",k,"  AIC=",ARMA_obj$aic,sep=""))
  }
}
log_sigma_jk<-log(sigma_jk)
aic<-sigma_jk
dimnames(aic)[[2]]<-paste("MA-order",0:maxmaorder,sep=" ")
dimnames(aic)[[1]]<-paste("AR-order",0:maxarorder,sep=" ")

bic<-aic
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)
  {
    aic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+2*(j+k)/len
    bic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+log(len)*(j+k)/len
  }
}
aic
bic

which(aic == min(aic), arr.ind = TRUE) 
which(bic == min(bic), arr.ind = TRUE) 

# Model orders
which(aic == min(aic), arr.ind = TRUE)-1 
which(bic == min(bic), arr.ind = TRUE)-1 


# Diagnostics
arorder<-1
maorder<-2
y_obj<-arima(y,order=c(arorder,0,maorder))

tsdiag(y_obj)

#### Test Prüfung 3a) identify Process


#### Series 1: X1
df = read.csv("C:/Users/41799/OneDrive - ZHAW/Time_Series1/series.csv", header = T, sep = "")

ts.plot(df$x1)#scheint stationär
par(mfrow = c(2,1))
acf(df$x1, lag.max = 50); pacf(df$x1, lag.max = 50)#MA(3), AR(7) or AR(3) or maybe ARMA (mixed)

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

# initialization: white noise
maorder = 3

parma<-rep(0,maorder)
estim_obj<-nlminb(parma,estim,x= df$x1)
round(estim_obj$par,4)

x_obj<-arima(df$x1,order=c(0,0,maorder))
x_obj$coef




####
tsdiag(x_obj,gof.lag=50)

maxarorder<-7
maxmaorder<-7
sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                 ncol=maxmaorder+1,nrow=maxarorder+1)
# Order (zero,zero)
sigma_jk[1,1]<-var(df$x1)
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)         #k<-4  j<-4
  {
    ARMA_obj<-arima(y,order=c(j,0,k))
    sigma_jk[j+1,k+1]<-ARMA_obj$sigma
    print(paste("  AR-order=",j,"MA-Order=",k,"  AIC=",ARMA_obj$aic,sep=""))
  }
}
log_sigma_jk<-log(sigma_jk)
aic<-sigma_jk
dimnames(aic)[[2]]<-paste("MA-order",0:maxmaorder,sep=" ")
dimnames(aic)[[1]]<-paste("AR-order",0:maxarorder,sep=" ")

bic<-aic
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)
  {
    aic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+2*(j+k)/len
    bic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+log(len)*(j+k)/len
  }
}
aic
bic

which(aic == min(aic), arr.ind = TRUE) 
which(bic == min(bic), arr.ind = TRUE) 

# Model orders
which(aic == min(aic), arr.ind = TRUE)-1 
which(bic == min(bic), arr.ind = TRUE)-1 

#### Series 2: X2
ts.plot(df$x2)
par(mfrow = c(2,1))
acf(df$x2, lag.max = 50); pacf(df$x2, lag.max = 50)#MA(13)

ma_1 = arima(df$x2, order = c(0,0,2))
tsdiag(ma_1, gof.lag = 50)#does only work up until lag 10 approx


maxarorder<-10
maxmaorder<-10
sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                 ncol=maxmaorder+1,nrow=maxarorder+1)
# Order (zero,zero)
sigma_jk[1,1]<-var(df$x2)
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)         #k<-4  j<-4
  {
    ARMA_obj<-arima(df$x2, order=c(j,0,k))
    sigma_jk[j+1,k+1]<-ARMA_obj$sigma
    print(paste("  AR-order=",j,"MA-Order=",k,"  AIC=",ARMA_obj$aic,sep=""))
  }
}
log_sigma_jk<-log(sigma_jk)
aic<-sigma_jk
dimnames(aic)[[2]]<-paste("MA-order",0:maxmaorder,sep=" ")
dimnames(aic)[[1]]<-paste("AR-order",0:maxarorder,sep=" ")

bic<-aic
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)
  {
    aic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+2*(j+k)/len
    bic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+log(len)*(j+k)/len
  }
}
aic
bic

which(aic == min(aic), arr.ind = TRUE) 
which(bic == min(bic), arr.ind = TRUE) 

# Model orders
which(aic == min(aic), arr.ind = TRUE)-1 
which(bic == min(bic), arr.ind = TRUE)-1 


###must be MA process

x_obj<-arima(df$x2,order=c(0,0,12))#go for MA(12)
tsdiag(x_obj, gof.lag = 50)
x_obj$coef

c(x_obj$coef)

set.seed(40)
x2<-as.ts(arima.sim(list(order=c(0,0,12),ma = c(-0.04, 0.04, 0.02, -0.025, 0.05, -0.06, 0.048, 
                                                -0.02, -0.054, 0.075, -0.13, -0.9)), n=len))-5
acf(x2)
acf(x2,type="partial")
ts.plot(x2)


#####??? Series 3 ####
x = df$x3
par(mfrow = c(2,1))
acf(x); pacf(x)# mixed ARIMA

maxarorder = 5
maxmaorder = 5
sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                 ncol=maxmaorder+1,nrow=maxarorder+1)
# Order (zero,zero)
sigma_jk[1,1]<-var(x)
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)         #k<-4  j<-4
  {
    ARMA_obj<-arima(x, order=c(j,0,k))
    sigma_jk[j+1,k+1]<-ARMA_obj$sigma
    print(paste("  AR-order=",j,"MA-Order=",k,"  AIC=",ARMA_obj$aic,sep=""))
  }
}
log_sigma_jk<-log(sigma_jk)
aic<-sigma_jk
dimnames(aic)[[2]]<-paste("MA-order",0:maxmaorder,sep=" ")
dimnames(aic)[[1]]<-paste("AR-order",0:maxarorder,sep=" ")

bic<-aic
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)
  {
    aic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+2*(j+k)/len
    bic[j+1,k+1]<-log_sigma_jk[j+1,k+1]+log(len)*(j+k)/len
  }
}
aic
bic

which(aic == min(aic), arr.ind = TRUE) 
which(bic == min(bic), arr.ind = TRUE) 


t = arima(x, order=c(2,0,3))
set.seed(40)
x3<-as.ts(arima.sim(list(order=c(2,0,3),ar=c(-0.32,-0.53),ma=-c(-0.128,0.5843,-0.553)),n=len))-5.
acf(x3)
acf(x3,type="partial")
ts.plot(x3)











