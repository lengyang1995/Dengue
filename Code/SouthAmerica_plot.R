library(stats)
library(lubridate)
library(scales)
library(ggplot2)
library(ggpubr)
########################## extract dengue case count
########################## Please add all files into your R
crcase=SouthAmerica_case$Costa_rica[31:365]
lag1crcase=SouthAmerica_case$Costa_rica[30:364]
lag30crcase=SouthAmerica_case$Costa_rica[1:364]

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

################################### Those are the best parameters estimated
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
  err = err + sum((y[(k+1):length(crcase)]- crcase[(k+1):length(crcase)])^2)
  err=err/length(y[(k+1):length(crcase)])
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


####################### draw graph and see the fitting
plot(crcase)
lines(y,col='red')
c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)




########################## Note that: to ignore the additive effect, the parameters for temperature should times i5
########################## and the parameters for rainfall should times i4
z=crcase
z=z[(k+1):length(z)]
y=y[(k+1):length(y)]
d=seq(as.Date("2015/1/10"), as.Date("2020/12/26"), by = "week")
data=cbind.data.frame(d,z)
data1=cbind.data.frame(d,y)
p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=15,face="bold"))
p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
p=p+ggtitle("Costa Rica")
p0=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))

###Dominican Republic
crcase=SouthAmerica_case$Dominican[31:365]
lag1crcase=SouthAmerica_case$Dominican[30:364]
lag30crcase=SouthAmerica_case$Dominican[1:364]

crcase=crcase*25
lag1crcase=lag1crcase*25
lag30crcase=lag30crcase*25
############# set rain and temperature 
temp=Dominican_Republic_weather[185:2705,]$temp
rain=Dominican_Republic_weather[185:2705,]$rain

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

################################### Those are the best parameters estimated
i4=   45
i5=   1.190000e+02
thre1=   2.392028e+02
tthre1=  3.229039e+03
k=   29


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
  err = err + sum((y[(k+1):length(crcase)]- crcase[(k+1):length(crcase)])^2)
  err=err/length(y[(k+1):length(crcase)])
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


####################### draw graph and see the fitting
plot(crcase)
lines(y,col='red')
c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)

z=crcase
z=z[(k+1):length(z)]
y=y[(k+1):length(y)]
d=seq(as.Date("2015/2/18"), as.Date("2020/12/26"), by = "week")
data=cbind.data.frame(d,z)
data1=cbind.data.frame(d,y)
p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=15,face="bold"))
p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
p=p+ggtitle("Dominican Republic")
p11=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))




#############Paraguay
crcase=SouthAmerica_case$Praguay[31:365]
lag1crcase=SouthAmerica_case$Praguay[30:364]
lag30crcase=SouthAmerica_case$Praguay[1:364]

crcase=crcase*25
lag1crcase=lag1crcase*25
lag30crcase=lag30crcase*25
############# set rain and temperature 
temp=Paraguay_weather[185:2703,]$temp
rain=Paraguay_weather[185:2703,]$rain

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

################################### Those are the best parameters estimated
i4=   166
i5=   108
thre1=  1.749076e+03
tthre1=  2.860430e+03
k=   2.800000e+01


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


####################### draw graph and see the fitting
plot(crcase)
lines(y,col='red')
c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)

####################
  z=crcase
  z=z[(k+1):length(z)]
  y=y[(k+1):length(y)]
  d=seq(as.Date("2015/2/9"), as.Date("2020/12/26"), by = "week")
  data=cbind.data.frame(d,z)
  data1=cbind.data.frame(d,y)
  p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
  p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
  p=p +theme_bw() +
    theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
          axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
          axis.title.y = element_text( size=15,face="bold"))
  p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
  p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
  p=p+ggtitle("Paraguay")
  p2=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
##################### Colombia
  crcase=SouthAmerica_case$Colombia[31:365]
  lag1crcase=SouthAmerica_case$Colombia[30:364]
  lag30crcase=SouthAmerica_case$Colombia[1:364]
  
  crcase=crcase*25
  lag1crcase=lag1crcase*25
  lag30crcase=lag30crcase*25
  ############# set rain and temperature 
  temp=Colombia_weather[185:2705,]$temp
  rain=Colombia_weather[185:2705,]$rain
  
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
  
  ################################### Those are the best parameters estimated
  i4=   1.700000e+02 
  i5=   1.790000e+02 
  thre1=   1.045729e+03 
  tthre1=  3.002980e+03
  k=   5
  
  
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
  
  
  ####################### draw graph and see the fitting
  plot(crcase)
  lines(y,col='red')
  c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
  ######################################
  
  z=crcase
  z=z[(k+1):length(z)]
  y=y[(k+1):length(y)]
  d=seq(as.Date("2014/9/1"), as.Date("2020/12/26"), by = "week")
  data=cbind.data.frame(d,z)
  data1=cbind.data.frame(d,y)
  p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
  p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
  p=p +theme_bw() +
    theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
          axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
          axis.title.y = element_text( size=15,face="bold"))
  p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
  p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
  p=p+ggtitle("Colombia")
  p3=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
  
########################### Bolivia
  crcase=SouthAmerica_case$Bolivia[31:365]
  lag1crcase=SouthAmerica_case$Bolivia[30:364]
  lag30crcase=SouthAmerica_case$Bolivia[1:364]
  
  crcase=crcase*25
  lag1crcase=lag1crcase*25
  lag30crcase=lag30crcase*25
  ############# set rain and temperature 
  temp=Bolivia_weather[185:2705,]$temp
  rain=Bolivia_weather[185:2705,]$rain
  
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
  
  ################################### Those are the best parameters estimated
  i4=   64
  i5=    86
  thre1=     5.025606e+02 
  tthre1= 2.038900e+03
  k=  2.600000e+01
  
  
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
  
  
####################### draw graph and see the fitting
  plot(crcase)
  lines(y,col='red')
  c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
######################################
  z=crcase
  z=z[(k+1):length(z)]
  y=y[(k+1):length(y)]
  d=seq(as.Date("2015/1/26"), as.Date("2020/12/26"), by = "week")
  data=cbind.data.frame(d,z)
  data1=cbind.data.frame(d,y)
  p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
  p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
  p=p +theme_bw() +
    theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
          axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
          axis.title.y = element_text( size=15,face="bold"))
  p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
  p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
  p=p+ggtitle("Bolivia")
  p5=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
  
#########################  Honduras

  crcase=SouthAmerica_case$Honduras[31:365]
  lag1crcase=SouthAmerica_case$Honduras[30:364]
  lag30crcase=SouthAmerica_case$Honduras[1:364]
  
  crcase=crcase*25
  lag1crcase=lag1crcase*25
  lag30crcase=lag30crcase*25
  ############# set rain and temperature 
  temp=Honduras_weather[185:2705,]$temp
  rain=Honduras_weather[185:2705,]$rain
  
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
  
  ################################### Those are the best parameters estimated
  i4=   151
  i5=    149
  thre1=   4.982078e+02
  tthre1= 4.138280e+03
  k=  1.200000e+01 
  
  
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
  
  
  ####################### draw graph and see the fitting
  plot(crcase)
  lines(y,col='red')
  c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
  ######################################
    
    ######################
    z=crcase
    z=z[(k+1):length(z)]
    y=y[(k+1):length(y)]
    d=seq(as.Date("2014/10/21"), as.Date("2020/12/26"), by = "week")
    data=cbind.data.frame(d,z)
    data1=cbind.data.frame(d,y)
    p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
    p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
    p=p +theme_bw() +
      theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
            axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.title.y = element_text( size=15,face="bold"))
    p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
    p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
    p=p+ggtitle("Honduras")
    p6=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
####################### Mexico
    crcase=SouthAmerica_case$Mexico[31:365]
    lag1crcase=SouthAmerica_case$Mexico[30:364]
    lag30crcase=SouthAmerica_case$Mexico[1:364]
    
    crcase=crcase*25
    lag1crcase=lag1crcase*25
    lag30crcase=lag30crcase*25
    ############# set rain and temperature 
    temp=Mexico_weather[185:2705,]$temp
    rain=Mexico_weather[185:2705,]$rain
    
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
    
    ################################### Those are the best parameters estimated
    i4=  45
    i5=    1.630000e+02 
    thre1=    7.528869e+01
    tthre1=  2.985475e+03
    k=  16
    
    
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
    
    
    ####################### draw graph and see the fitting
    plot(crcase)
    lines(y,col='red')
    c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
    ######################################   
      z=crcase
      z=z[(k+1):length(z)]
      y=y[(k+1):length(y)]
      d=seq(as.Date("2014/11/21"), as.Date("2020/12/26"), by = "week")
      data=cbind.data.frame(d,z)
      data1=cbind.data.frame(d,y)
      p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
      p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
      p=p +theme_bw() +
        theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
              axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
              axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
              axis.title.y = element_text( size=15,face="bold"))
      p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
      p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
      p=p+ggtitle("Mexico")
      p7=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
      
