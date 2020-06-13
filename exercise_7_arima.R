path = 'C:/Users/41799/OneDrive - ZHAW/Time_Series1/'
file = 'gdp.txt'
gdp = read.table(paste(path,file, sep = ''))

#stationary ARIMA
#arima(x, order = c(p, 0, q))

#NON-stationary ARIMA
#arima(x, order = c(p, d > 0, q))

#AIC/BIC Crit


info_crit = function(y, maxarorder, maxmaorder){
  
  
  sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                   ncol=maxmaorder+1,nrow=maxarorder+1)
  
  sigma_jk[1,1]<-var(y)
  for (j in 0:maxarorder)
  {
    
    for (k in 0:maxmaorder)         #k<-3  j<-4
    {
      # Avoid interrupt if optimization fails    
      try_obj<-try(arima(y,order=c(j,d,k)),silent=T)
      if (is.character(try_obj[1]))
      {
        # If interrupted: sigma is set to a large value (`bad' model)      
        sigma_jk[j+1,k+1]<-1.e+99      
      } else
      {
        ARMA_obj<-arima(y,order=c(j,d,k))
        print(c(j,k))
        sigma_jk[j+1,k+1]<-ARMA_obj$sigma
        print(paste("  AR-order=",j,"MA-Order=",k,"  AIC=",ARMA_obj$aic,sep=""))      
      }
    }
  }
  
  len<-length(y)
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
  results = list('aic' = aic, 'bic' = bic)
  return(results)
}

data = c(log(gdp$V1))
crit = info_crit(data, 5,5)
aic = crit$aic; bic = crit$bic

which(aic == min(aic), arr.ind = TRUE)-1 
which(bic == min(bic), arr.ind = TRUE)-1 
arorder<-which(bic == min(bic), arr.ind = TRUE)[1]-1 
maorder<-which(bic == min(bic), arr.ind = TRUE)[2]-1 

#stationary ARIMA d = 0
d = 0
y_obj<-arima(y,order=c(arorder,d,maorder))

tsdiag(y_obj)

