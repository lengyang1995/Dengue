library(stats)
library(lubridate)
library(scales)
library(ggplot2)
library(ggpubr)
library(mgcv)
library(tidyverse)
########################## extract dengue case count
########################## Please add all files into your R, using Costa Rica as an example.
crcase=SouthAmerica_case$Costa_rica[31:365]
lag1crcase=SouthAmerica_case$Costa_rica[30:364]
lag30crcase=SouthAmerica_case$Costa_rica[1:364]
######################### adjust under-reporting rate
crcase=crcase*25
lag1crcase=lag1crcase*25
lag30crcase=lag30crcase*25
############# set rain and temperature 
temp=Costarica_weather[185:2705,]$temp
rain=Costarica_weather[185:2705,]$rain

h=cumsum(temp)
h=c(0,h)
h1=cumsum(rain)
h1=c(0,h1)

temp_1=matrix(data = NA,nrow=180,ncol=length(crcase))
rain_1=matrix(data = NA,nrow=180,ncol=length(crcase))
for(j in 1:180)
{ 
  for(j1 in 1:length(crcase)){
    temp_1[j,j1]=h[181+7*(j1-1)]-h[181+7*(j1-1)-j]
    rain_1[j,j1]=h1[181+7*(j1-1)]-h1[181+7*(j1-1)-j]
  }
}
############### find the best parameters, K can be chosen from any value
library(stats)
library(doParallel)
library(parallel)
library(foreach)
num_cores = detectCores()
cl = makeCluster(num_cores-1)
registerDoParallel(cl)
K=50000
########################## find the best parameters and you can choose your own K
ress1 = foreach(iter = 1:K,.combine='rbind')%dopar%{
  k=sample(c(4:30),1)
  i4= sample(c(14:180),1)
  i5=sample(c(14:180),1)
  
  rangeupper=quantile(rain_1[i4,], probs = seq(0.7, 0.8, 0.1))[1]
  rangelower=quantile(rain_1[i4,], probs = seq(0, 0.1, 0.1))[2]
  thre=seq(rangelower, rangeupper, by=5)
  thre1=sample(thre,1)
  
  rangeupper1=quantile(temp_1[i5,], probs = seq(0.7, 0.8, 0.1))[1]
  rangelower1=quantile(temp_1[i5,], probs = seq(0.1, 0.2, 0.1))[2]
  tthre=seq(rangelower1, rangeupper1, by=2)
  tthre1=sample(tthre,1)
  
  
  
  summ=numeric()
  for(i in 1:length(crcase))
  {summ[i]=sum(lag30crcase[(30+i-k):(29+i)])}
  sum=summ*lag1crcase
  
  
  r= (rain_1[i4,])*lag1crcase
  t= (temp_1[i5,])*lag1crcase
  
  
  thre2=(rain_1[i4,]-thre1)
  thre3=thre2*(thre2>0)*lag1crcase
  
  tthre2=(temp_1[i5,]-tthre1)
  tthre3=tthre2*(tthre2>0)*lag1crcase
  
  fit2=lm(crcase~lag1crcase+sum+r+thre3+t+tthre3+0) ################ result based on 1-step prediction
  
  a=fit2$coefficients[1]
  b=fit2$coefficients[2]
  c=fit2$coefficients[3]
  d=fit2$coefficients[4]
  e=fit2$coefficients[5]
  f=fit2$coefficients[6]
  
  r1=(rain_1[i4,])
  t1=(temp_1[i5,])
  th1=thre2*(thre2>0)
  tth1=tthre2*(tthre2>0)
  ################### For all regions, we set the max fiited y smaller than 2*reported cases and min fiited y bigger than 1.
  beta=c(a,b,c,d,e,f)
  obj = function(beta)
  {
    err = 0
    y=crcase[1:k]
    for(i in (k+1):length(crcase))
    {
      y[i]=beta[1]*y[i-1]+beta[2]*sum(y[(i-k):(i-1)])*y[i-1]+ beta[3]*(r1)[i]*y[i-1]+beta[4]*(th1)[i]*y[i-1]+beta[5]*(t1)[i]*y[i-1]+beta[6]*(tth1)[i]*y[i-1]
      y[i]=max(1,y[i])
      y[i]=min(2*max(crcase),y[i])
    }
    err = err + sum((y[(k+1):length(y)]- crcase[(k+1):length(crcase)])^2)
    err=err/length(y[(k+1):length(y)])
    return(err)
  }
  est.b = optim(beta, obj)
  beta = est.b$par
  
  ############ re-estimated parameters based on multi-step prediction
  err = 0
  y=crcase[1:k]
  for(i in (k+1):length(crcase))
  {
    y[i]=beta[1]*y[i-1]+beta[2]*sum(y[(i-k):(i-1)])*y[i-1]+ beta[3]*(r1)[i]*y[i-1]+beta[4]*(th1)[i]*y[i-1]+beta[5]*(t1)[i]*y[i-1]+beta[6]*(tth1)[i]*y[i-1]
    y[i]=max(1,y[i])
    y[i]=min(2*max(crcase),y[i])
  }
  err = err + sum((y[(k+1):length(y)]- crcase[(k+1):length(crcase)])^2)
  c(err/length(y[(k+1):length(y)]),i4,i5,thre1,tthre1,k,beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5)
}
stopCluster(cl)
which.min(ress1[,1])

######## Those are the best parameters from our analysis
i4=   146 
i5=   172
thre1=    1.174138e+03
tthre1=  3.870000e+03 
k=    23


summ=numeric()
for(i in 1:length(crcase))
{summ[i]=sum(lag30crcase[(30+i-k):(29+i)])}
sum=summ*lag1crcase


r= (rain_1[i4,])*lag1crcase
t= (temp_1[i5,])*lag1crcase


thre2=(rain_1[i4,]-thre1)
thre3=thre2*(thre2>0)*lag1crcase

tthre2=(temp_1[i5,]-tthre1)
tthre3=tthre2*(tthre2>0)*lag1crcase

fit2=lm(crcase~lag1crcase+sum+r+thre3+t+tthre3+0) ################ result based on 1-step prediction

a=fit2$coefficients[1]
b=fit2$coefficients[2]
c=fit2$coefficients[3]
d=fit2$coefficients[4]
e=fit2$coefficients[5]
f=fit2$coefficients[6]

r1=(rain_1[i4,])
t1=(temp_1[i5,])
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

beta=c(a,b,c,d,e,f)
obj = function(beta)
{
  err = 0
  y=crcase[1:k]
  for(i in (k+1):length(crcase))
  {
    y[i]=beta[1]*y[i-1]+beta[2]*sum(y[(i-k):(i-1)])*y[i-1]+
      beta[3]*(r1)[i]*y[i-1]+
      beta[4]*(th1)[i]*y[i-1]+
      beta[5]*(t1)[i]*y[i-1]+
      beta[6]*(tth1)[i]*y[i-1]
    y[i]=max(1,y[i])
    y[i]=min(2*max(crcase),y[i])
  }
  err = err + sum((y[(k+1):length(y)]- crcase[(k+1):length(crcase)])^2)
  err=err/length(y[(k+1):length(y)])
  return(err)
}
est.b = optim(beta, obj)
beta = est.b$par

############ re-estimated parameters based on multi-step prediction
y=crcase[1:k]
for(i in (k+1):length(crcase))
{
  y[i]=beta[1]*y[i-1]+beta[2]*sum(y[(i-k):(i-1)])*y[i-1]+
    beta[3]*(r1)[i]*y[i-1]+
    beta[4]*(th1)[i]*y[i-1]+
    beta[5]*(t1)[i]*y[i-1]+
    beta[6]*(tth1)[i]*y[i-1]
  y[i]=max(1,y[i])
  y[i]=min(2*max(crcase),y[i])
}


####################### draw graph and see the fitting, which is pretty good.
plot(crcase)
lines(y,col='red')
####################### Those are p-value calculations
err1=fit2$residuals
########################## 
library(doParallel)
library(parallel)
library(foreach)
num_cores = detectCores()
cl = makeCluster(num_cores)
registerDoParallel(cl)
B=2000
####### parameters are fixed
resss1 = foreach(iter = 1:B,.combine='rbind')%dopar%{
  y=crcase[1:k]
  for(i in (k+1):length(crcase))
  {
    y[i]=beta[1]*y[i-1]+beta[2]*sum(y[(i-k):(i-1)])*y[i-1]+
      beta[3]*(r1)[i]*y[i-1]+
      beta[4]*(th1)[i]*y[i-1]+
      beta[5]*(t1)[i]*y[i-1]+
      beta[6]*(tth1)[i]*y[i-1]+err1[i]*rnorm(1)
    y[i]=max(1,y[i])
    y[i]=min(2*max(crcase),y[i])
  }
  
  
  beta1=beta
  obj = function(beta1)
  {
    err = 0
    y1=crcase[1:k]
    for(i in (k+1):length(crcase))
    {
      y1[i]=beta1[1]*y1[i-1]+beta1[2]*sum(y1[(i-k):(i-1)])*y1[i-1]+
        beta1[3]*(r1)[i]*y1[i-1]+
        beta1[4]*(th1)[i]*y1[i-1]+
        beta1[5]*(t1)[i]*y1[i-1]+
        beta1[6]*(tth1)[i]*y1[i-1]
      y1[i]=max(1,y1[i])
      y1[i]=min(2*max(crcase),y1[i])
    }
    err = err + sum((y1[(k+1):length(y)]- y[(k+1):length(crcase)])^2)
    return(err)
  }
  est.b = optim(beta1, obj)
  beta1 = est.b$par
  c(beta1[1],beta1[2],beta1[3],beta1[4],beta1[5],beta1[6])
}
stopCluster(cl)
a=resss1[,1]
b=resss1[,2]
c=resss1[,3]
d=resss1[,4]
e=resss1[,5]
f=resss1[,6]
#################### This is how we find the P-values (if the parameter is positive, p-value is 1- (ranking))
a=c(0,a)
((B+1)-rank(a)[1])/B
b=c(0,b)
((B+1)-rank(b)[1])/B
c=c(0,c)
((B+1)-rank(c)[1])/B
d=c(0,d)
((B+1)-rank(d)[1])/B
e=c(0,e)
((B+1)-rank(e)[1])/B
f=c(0,f)
((B+1)-rank(f)[1])/B