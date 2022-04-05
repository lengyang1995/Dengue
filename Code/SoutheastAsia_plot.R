library(stats)
library(tidyverse)
################## Singapore (only 2014~2020)
Singapore_number=dengueCases_SG
Singapore_number$case[732:1044]=Singapore_number$case[732:1044]*6
Singapore_number$case[1045:1096]=Singapore_number$case[1045:1096]*3

crcase=Singapore_number$case[762:1096]
lag1crcase=Singapore_number$case[761:1095]
lag30crcase=Singapore_number$case[732:1095]

############## Climate factors
pm10=Singapore_weather$pm10[36:2554]
o3=Singapore_weather$o3[36:2554]
no2=Singapore_weather$no2[36:2554]
temp=Singapore_weather$temp[36:2554]
rain=Singapore_weather$rain[36:2554]


a=cumsum(pm10)
a=c(0,a)
b=cumsum(o3)
b=c(0,b)
c=cumsum(no2)
c=c(0,c)
h=cumsum(temp)
h=c(0,h)
h1=cumsum(rain)
h1=c(0,h1)


pm10_1=matrix(data = NA,nrow=180,ncol=length(crcase))
o3_1=matrix(data = NA,nrow=180,ncol=length(crcase))
no2_1=matrix(data = NA,nrow=180,ncol=length(crcase))
temp_1=matrix(data = NA,nrow=180,ncol=length(crcase))
rain_1=matrix(data = NA,nrow=180,ncol=length(crcase))
for(j in 1:180)
{ 
  for(j1 in 1:length(crcase)){
    pm10_1[j,j1]=a[181+7*(j1-1)]-a[181+7*(j1-1)-j]
    o3_1[j,j1]=b[181+7*(j1-1)]-b[181+7*(j1-1)-j]
    no2_1[j,j1]=c[181+7*(j1-1)]-c[181+7*(j1-1)-j]
    temp_1[j,j1]=h[181+7*(j1-1)]-h[181+7*(j1-1)-j]
    rain_1[j,j1]=h1[181+7*(j1-1)]-h1[181+7*(j1-1)-j]
  }
}





################################### Those are the best parameters estimated for considering both
################################### air-pollution and temperature
i1=23
i2=148
i3=124
i4=169
i5=58
thre1= 899.24
tthre1= 1637.98
k=17


summ=numeric()
for(i in 1:length(crcase))
{summ[i]=sum(lag30crcase[(30+i-k):(29+i)])}
sum=summ*lag1crcase

pm=pm10_1[i1,]*lag1crcase
o3=o3_1[i2,]*lag1crcase
no2=no2_1[i3,]*lag1crcase
r= (rain_1[i4,])*lag1crcase
t= (temp_1[i5,])*lag1crcase


thre2=(rain_1[i4,]-thre1)
thre3=thre2*(thre2>0)*lag1crcase

tthre2=(temp_1[i5,]-tthre1)
tthre3=tthre2*(tthre2>0)*lag1crcase

fit2=lm(crcase~lag1crcase+pm+o3+no2+r+thre3+t+tthre3+sum+0) ################ result based on 1-step prediction

a=fit2$coefficients[1]
b=fit2$coefficients[2]
c=fit2$coefficients[3]
d=fit2$coefficients[4]
e=fit2$coefficients[5]
f=fit2$coefficients[6]
g=fit2$coefficients[7]
h=fit2$coefficients[8]
I=fit2$coefficients[9]


p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=(rain_1[i4,])
t1=(temp_1[i5,])
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

beta=c(a,b,c,d,e,f,g,h,I)
obj = function(beta)
{
  err = 0
  y=crcase[1:k]
  for(i in (k+1):length(crcase))
  {
    y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
      beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
      beta[5]*(r1)[i]*y[i-1]+
      beta[6]*(th1)[i]*y[i-1]+
      beta[7]*(t1)[i]*y[i-1]+
      beta[8]*(tth1)[i]*y[i-1]+
      beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
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
  y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
    beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
    beta[5]*(r1)[i]*y[i-1]+
    beta[6]*(th1)[i]*y[i-1]+
    beta[7]*(t1)[i]*y[i-1]+
    beta[8]*(tth1)[i]*y[i-1]+
    beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
  y[i]=max(1,y[i])
  y[i]=min(2*max(crcase),y[i])
}
###################### see how's the fitting
c(beta[1],beta[2]*i1,beta[3]*i2,beta[4]*i3,beta[5]*i4,beta[6]*i4,beta[7]*i5,beta[8]*i5,beta[9],thre1/i4,tthre1/i5,k*7,i1,i2,i3,i4,i5)
plot(crcase)
lines(y,col='red')
  



#####
z=crcase
z=z[(k+1):length(z)]
y=y[(k+1):length(y)]
d=seq(as.Date("2014/11/30"), as.Date("2020/12/27"), by = "week")
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
p=p+ggtitle("Singapore")
p8=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))


###################################### only consider temperature and rainfall
i4= 149
i5= 131
thre1=  836.20
tthre1=   3664.80
k=      29
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
###################################### The fitting is ok since it still captures the main dynamics
c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
plot(crcase)
lines(y,col='red')
#####################################
#####################################      
z=crcase
z=z[(k+1):length(z)]
y=y[(k+1):length(y)]
d=seq(as.Date("2015/2/18"), as.Date("2020/12/27"), by = "week")
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
p=p+ggtitle("Singapore")
p9=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))


############## Bangkok consider all
############## Climate factors
Thai_Dengue_Week=read_csv('Thai_Dengue_Week.csv')
Thai_Dengue_Week[,2:5]=Thai_Dengue_Week[,2:5]*7
crcase=Thai_Dengue_Week$bk[31:339]
lag1crcase=Thai_Dengue_Week$bk[30:338]
lag30crcase=Thai_Dengue_Week$bk[1:338]


pm10=Bangkok_weather$pm10[38:2554]
o3=Bangkok_weather$o3[38:2554]
no2=Bangkok_weather$no2[38:2554]
temp=Bangkok_weather$temp[38:2554]
rain=Bangkok_weather$rain[38:2554]


a=cumsum(pm10)
a=c(0,a)
b=cumsum(o3)
b=c(0,b)
c=cumsum(no2)
c=c(0,c)
h=cumsum(temp)
h=c(0,h)
h1=cumsum(rain)
h1=c(0,h1)


pm10_1=matrix(data = NA,nrow=180,ncol=length(crcase))
o3_1=matrix(data = NA,nrow=180,ncol=length(crcase))
no2_1=matrix(data = NA,nrow=180,ncol=length(crcase))
temp_1=matrix(data = NA,nrow=180,ncol=length(crcase))
rain_1=matrix(data = NA,nrow=180,ncol=length(crcase))
for(j in 1:180)
{ 
  for(j1 in 1:length(crcase)){
    pm10_1[j,j1]=a[181+7*(j1-1)]-a[181+7*(j1-1)-j]
    o3_1[j,j1]=b[181+7*(j1-1)]-b[181+7*(j1-1)-j]
    no2_1[j,j1]=c[181+7*(j1-1)]-c[181+7*(j1-1)-j]
    temp_1[j,j1]=h[181+7*(j1-1)]-h[181+7*(j1-1)-j]
    rain_1[j,j1]=h1[181+7*(j1-1)]-h1[181+7*(j1-1)-j]
  }
}
################################### Those are the best parameters estimated for considering both
################################### air-pollution and temperature
i1=124
i2= 20
i3=178
i4=45
i5= 106
thre1= 508.4287
tthre1=   3046.520 
k=30

summ=numeric()
for(i in 1:length(crcase))
{summ[i]=sum(lag30crcase[(30+i-k):(29+i)])}
sum=summ*lag1crcase

pm=pm10_1[i1,]*lag1crcase
o3=o3_1[i2,]*lag1crcase
no2=no2_1[i3,]*lag1crcase
r= (rain_1[i4,])*lag1crcase
t= (temp_1[i5,])*lag1crcase


thre2=(rain_1[i4,]-thre1)
thre3=thre2*(thre2>0)*lag1crcase

tthre2=(temp_1[i5,]-tthre1)
tthre3=tthre2*(tthre2>0)*lag1crcase

fit2=lm(crcase~lag1crcase+pm+o3+no2+r+thre3+t+tthre3+sum+0) ################ result based on 1-step prediction

a=fit2$coefficients[1]
b=fit2$coefficients[2]
c=fit2$coefficients[3]
d=fit2$coefficients[4]
e=fit2$coefficients[5]
f=fit2$coefficients[6]
g=fit2$coefficients[7]
h=fit2$coefficients[8]
I=fit2$coefficients[9]


p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=(rain_1[i4,])
t1=(temp_1[i5,])
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

beta=c(a,b,c,d,e,f,g,h,I)
obj = function(beta)
{
  err = 0
  y=crcase[1:k]
  for(i in (k+1):length(crcase))
  {
    y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
      beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
      beta[5]*(r1)[i]*y[i-1]+
      beta[6]*(th1)[i]*y[i-1]+
      beta[7]*(t1)[i]*y[i-1]+
      beta[8]*(tth1)[i]*y[i-1]+
      beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
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
  y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
    beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
    beta[5]*(r1)[i]*y[i-1]+
    beta[6]*(th1)[i]*y[i-1]+
    beta[7]*(t1)[i]*y[i-1]+
    beta[8]*(tth1)[i]*y[i-1]+
    beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
  y[i]=max(1,y[i])
  y[i]=min(2*max(crcase),y[i])
}
###################### see how's the fitting
c(beta[1],beta[2]*i1,beta[3]*i2,beta[4]*i3,beta[5]*i4,beta[6]*i4,beta[7]*i5,beta[8]*i5,beta[9],thre1/i4,tthre1/i5,k*7,i1,i2,i3,i4,i5)
plot(crcase)
lines(y,col='red')
#######################
z=crcase
z=z[(k+1):length(z)]
y=y[(k+1):length(y)]
d=seq(as.Date("2015/3/1"), as.Date("2020/7/1"), by = "week")
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
p=p+ggtitle("Bangkok")
p10=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
########################################### Bangkok consider only temperature and rainfall
i4= 93
i5= 167
thre1=   934.6444 
tthre1= 4890.161
k=     30
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



###################################### The fitting is ok since it still captures the main dynamics
c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
plot(crcase)
lines(y,col='red')


#####################################
#####################################      
    z=crcase
    z=z[(k+1):length(z)]
    y=y[(k+1):length(y)]
    d=seq(as.Date("2015/2/28"), as.Date("2020/7/1"), by = "week")
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
    p=p+ggtitle("Bangkok")
    p11=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
    
################################# Chiangmai
    crcase=Thai_Dengue_Week$cm[31:339]
    lag1crcase=Thai_Dengue_Week$cm[30:338]
    lag30crcase=Thai_Dengue_Week$cm[1:338]
    
    
    pm10=Chiangmai_weather$pm10[38:2553]
    o3=Chiangmai_weather$o3[38:2553]
    no2=Chiangmai_weather$no2[38:2553]
    temp=Chiangmai_weather$temp[38:2553]
    rain=Chiangmai_weather$rain[38:2553]
    
    
    a=cumsum(pm10)
    a=c(0,a)
    b=cumsum(o3)
    b=c(0,b)
    c=cumsum(no2)
    c=c(0,c)
    h=cumsum(temp)
    h=c(0,h)
    h1=cumsum(rain)
    h1=c(0,h1)
    
    
    pm10_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    o3_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    no2_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    temp_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    rain_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    for(j in 1:180)
    { 
      for(j1 in 1:length(crcase)){
        pm10_1[j,j1]=a[181+7*(j1-1)]-a[181+7*(j1-1)-j]
        o3_1[j,j1]=b[181+7*(j1-1)]-b[181+7*(j1-1)-j]
        no2_1[j,j1]=c[181+7*(j1-1)]-c[181+7*(j1-1)-j]
        temp_1[j,j1]=h[181+7*(j1-1)]-h[181+7*(j1-1)-j]
        rain_1[j,j1]=h1[181+7*(j1-1)]-h1[181+7*(j1-1)-j]
      }
    }
    ################################### Those are the best parameters estimated for considering both
    ################################### air-pollution and temperature
    i1=19
    i2= 87
    i3=125
    i4=38
    i5= 25
    thre1= 2.284907e+02 
    tthre1=6.924000e+02
    k=18
    
    summ=numeric()
    for(i in 1:length(crcase))
    {summ[i]=sum(lag30crcase[(30+i-k):(29+i)])}
    sum=summ*lag1crcase
    
    pm=pm10_1[i1,]*lag1crcase
    o3=o3_1[i2,]*lag1crcase
    no2=no2_1[i3,]*lag1crcase
    r= (rain_1[i4,])*lag1crcase
    t= (temp_1[i5,])*lag1crcase
    
    
    thre2=(rain_1[i4,]-thre1)
    thre3=thre2*(thre2>0)*lag1crcase
    
    tthre2=(temp_1[i5,]-tthre1)
    tthre3=tthre2*(tthre2>0)*lag1crcase
    
    fit2=lm(crcase~lag1crcase+pm+o3+no2+r+thre3+t+tthre3+sum+0) ################ result based on 1-step prediction
    
    a=fit2$coefficients[1]
    b=fit2$coefficients[2]
    c=fit2$coefficients[3]
    d=fit2$coefficients[4]
    e=fit2$coefficients[5]
    f=fit2$coefficients[6]
    g=fit2$coefficients[7]
    h=fit2$coefficients[8]
    I=fit2$coefficients[9]
    
    
    p1=pm10_1[i1,]
    o1=o3_1[i2,]
    n1=no2_1[i3,]
    r1=(rain_1[i4,])
    t1=(temp_1[i5,])
    th1=thre2*(thre2>0)
    tth1=tthre2*(tthre2>0)
    
    beta=c(a,b,c,d,e,f,g,h,I)
    obj = function(beta)
    {
      err = 0
      y=crcase[1:k]
      for(i in (k+1):length(crcase))
      {
        y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
          beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
          beta[5]*(r1)[i]*y[i-1]+
          beta[6]*(th1)[i]*y[i-1]+
          beta[7]*(t1)[i]*y[i-1]+
          beta[8]*(tth1)[i]*y[i-1]+
          beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
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
      y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
        beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
        beta[5]*(r1)[i]*y[i-1]+
        beta[6]*(th1)[i]*y[i-1]+
        beta[7]*(t1)[i]*y[i-1]+
        beta[8]*(tth1)[i]*y[i-1]+
        beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
      y[i]=max(1,y[i])
      y[i]=min(2*max(crcase),y[i])
    }
    ###################### see how's the fitting
    c(beta[1],beta[2]*i1,beta[3]*i2,beta[4]*i3,beta[5]*i4,beta[6]*i4,beta[7]*i5,beta[8]*i5,beta[9],thre1/i4,tthre1/i5,k*7,i1,i2,i3,i4,i5)
    plot(crcase)
    lines(y,col='red')
############################################
    z=crcase
    z=z[(k+1):length(z)]
    y=y[(k+1):length(y)]
    d=seq(as.Date("2014/12/8"), as.Date("2020/7/1"), by = "week")
    data=cbind.data.frame(d,z)
    data1=cbind.data.frame(d,y)
    p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
    p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
    p=p +theme_bw() +
      theme(plot.title = element_text(size=20,hjust = 0.68,face="bold"),axis.title.x = element_text( size=15, face="bold"),
            axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.title.y = element_text( size=15,face="bold"))
    p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
    p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
    p=p+ggtitle("Chiangmai")
    p12=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))

    ############### consider only temperature and rainfall 
    i4= 152
    i5= 115
    thre1=  1.050563e+03  
    tthre1=  3.208900e+03 
    k=   15
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
    
    ###################################### The fitting is ok since it still captures the main dynamics
    c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
    plot(crcase)
    lines(y,col='red')
#################################################################
    z=crcase
    z=z[(k+1):length(z)]
    y=y[(k+1):length(y)]
    d=seq(as.Date("2014/11/14"), as.Date("2020/7/1"), by = "week")
    data=cbind.data.frame(d,z)
    data1=cbind.data.frame(d,y)
    p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
    p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
    p=p +theme_bw() +
      theme(plot.title = element_text(size=20,hjust = 0.68,face="bold"),axis.title.x = element_text( size=15, face="bold"),
            axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.title.y = element_text( size=15,face="bold"))
    p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
    p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
    p=p+ggtitle("Chiangmai")
    p13=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
################################################################# Chonburi
    crcase=Thai_Dengue_Week$chb[31:339]
    lag1crcase=Thai_Dengue_Week$chb[30:338]
    lag30crcase=Thai_Dengue_Week$chb[1:338]
    
    
    pm10=Chonburi_weather$pm10[38:2554]
    o3=Chonburi_weather$o3[38:2554]
    no2=Chonburi_weather$no2[38:2554]
    temp=Chonburi_weather$temp[38:2554]
    rain=Chonburi_weather$rain[38:2554]
    
    
    a=cumsum(pm10)
    a=c(0,a)
    b=cumsum(o3)
    b=c(0,b)
    c=cumsum(no2)
    c=c(0,c)
    h=cumsum(temp)
    h=c(0,h)
    h1=cumsum(rain)
    h1=c(0,h1)
    
    
    pm10_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    o3_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    no2_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    temp_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    rain_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    for(j in 1:180)
    { 
      for(j1 in 1:length(crcase)){
        pm10_1[j,j1]=a[181+7*(j1-1)]-a[181+7*(j1-1)-j]
        o3_1[j,j1]=b[181+7*(j1-1)]-b[181+7*(j1-1)-j]
        no2_1[j,j1]=c[181+7*(j1-1)]-c[181+7*(j1-1)-j]
        temp_1[j,j1]=h[181+7*(j1-1)]-h[181+7*(j1-1)-j]
        rain_1[j,j1]=h1[181+7*(j1-1)]-h1[181+7*(j1-1)-j]
      }
    }
    ################################### Those are the best parameters estimated for considering both
    ################################### air-pollution and temperature
    i1=168
    i2= 142
    i3=39
    i4=155
    i5= 120
    thre1= 1211.08170816
    tthre1=3419.66000000
    k=2.800000e+01
    
    summ=numeric()
    for(i in 1:length(crcase))
    {summ[i]=sum(lag30crcase[(30+i-k):(29+i)])}
    sum=summ*lag1crcase
    
    pm=pm10_1[i1,]*lag1crcase
    o3=o3_1[i2,]*lag1crcase
    no2=no2_1[i3,]*lag1crcase
    r= (rain_1[i4,])*lag1crcase
    t= (temp_1[i5,])*lag1crcase
    
    
    thre2=(rain_1[i4,]-thre1)
    thre3=thre2*(thre2>0)*lag1crcase
    
    tthre2=(temp_1[i5,]-tthre1)
    tthre3=tthre2*(tthre2>0)*lag1crcase
    
    fit2=lm(crcase~lag1crcase+pm+o3+no2+r+thre3+t+tthre3+sum+0) ################ result based on 1-step prediction
    
    a=fit2$coefficients[1]
    b=fit2$coefficients[2]
    c=fit2$coefficients[3]
    d=fit2$coefficients[4]
    e=fit2$coefficients[5]
    f=fit2$coefficients[6]
    g=fit2$coefficients[7]
    h=fit2$coefficients[8]
    I=fit2$coefficients[9]
    
    
    p1=pm10_1[i1,]
    o1=o3_1[i2,]
    n1=no2_1[i3,]
    r1=(rain_1[i4,])
    t1=(temp_1[i5,])
    th1=thre2*(thre2>0)
    tth1=tthre2*(tthre2>0)
    
    beta=c(a,b,c,d,e,f,g,h,I)
    obj = function(beta)
    {
      err = 0
      y=crcase[1:k]
      for(i in (k+1):length(crcase))
      {
        y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
          beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
          beta[5]*(r1)[i]*y[i-1]+
          beta[6]*(th1)[i]*y[i-1]+
          beta[7]*(t1)[i]*y[i-1]+
          beta[8]*(tth1)[i]*y[i-1]+
          beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
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
      y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
        beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
        beta[5]*(r1)[i]*y[i-1]+
        beta[6]*(th1)[i]*y[i-1]+
        beta[7]*(t1)[i]*y[i-1]+
        beta[8]*(tth1)[i]*y[i-1]+
        beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
      y[i]=max(1,y[i])
      y[i]=min(2*max(crcase),y[i])
    }
    ###################### see how's the fitting
    c(beta[1],beta[2]*i1,beta[3]*i2,beta[4]*i3,beta[5]*i4,beta[6]*i4,beta[7]*i5,beta[8]*i5,beta[9],thre1/i4,tthre1/i5,k*7,i1,i2,i3,i4,i5)
    plot(crcase)
    lines(y,col='red')
######################### 
    z=crcase
    z=z[(k+1):length(z)]
    y=y[(k+1):length(y)]
    d=seq(as.Date("2015/2/18"), as.Date("2020/7/1"), by = "week")
    data=cbind.data.frame(d,z)
    data1=cbind.data.frame(d,y)
    p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
    p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
    p=p +theme_bw() +
      theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
            axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.title.y = element_text( size=15,face="bold"))
    p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
    p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
    p=p+ggtitle("Chonburi")
    p14=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
    
    ############### consider only temperature and rainfall 
    i4= 178
    i5= 129
    thre1= 1402.38690389
    tthre1= 3699.72000000 
    k=   28
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
    ks=93
    yb=crcase[1:ks]
    for(i in (ks+1):length(crcase))
    {
      yb[i]=beta[1]*yb[i-1]+beta[2]*sum(yb[(i-k):(i-1)])*yb[i-1]+
        beta[3]*(r1)[i]*yb[i-1]+
        beta[4]*(th1)[i]*yb[i-1]+
        beta[5]*(t1)[i]*yb[i-1]+
        beta[6]*(tth1)[i]*yb[i-1]
      yb[i]=max(1,yb[i])
      yb[i]=min(2*max(crcase),yb[i])
    }
    
    ###################################### The fitting is ok since it still captures the main dynamics
    c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
    plot(crcase)
    lines(y,col='red')
    lines(yb,col='blue')
    #################################################################
    z=crcase
    z=z[(k+1):length(z)]
    y=y[(k+1):length(y)]
    yb=yb[(ks+1):309]
    d=seq(as.Date("2015/2/18"), as.Date("2020/7/1"), by = "week")
    d1=seq(as.Date("2016/5/18"), as.Date("2020/7/1"), by = "week")
    data=cbind.data.frame(d,z)
    data1=cbind.data.frame(d,y)
    data2=cbind.data.frame(d1,yb)
    p <- ggplot(data,  aes(d,z)) +geom_point(size=1.3)
    p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=data1, aes(d,y),col='red')
    p=p+geom_line(size=1.3,data=data2, aes(d1,yb),col='blue')
    p=p +theme_bw() +
      theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=15, face="bold"),
            axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
            axis.title.y = element_text( size=15,face="bold"))
    p=p+theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
    p =p+scale_x_date(date_breaks = "1 years", date_labels = "%Y")+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))
    p=p+ggtitle("Chonburi")
    p15=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))

################ Nakhon Sawan

    crcase=Thai_Dengue_Week$nks[31:339]
    lag1crcase=Thai_Dengue_Week$nks[30:338]
    lag30crcase=Thai_Dengue_Week$nks[1:338]
    
    
    pm10=Nks_weather$pm10[38:2553]
    o3=Nks_weather$o3[38:2553]
    no2=Nks_weather$no2[38:2553]
    temp=Nks_weather$temp[38:2553]
    rain=Nks_weather$rain[38:2553]
    
    
    a=cumsum(pm10)
    a=c(0,a)
    b=cumsum(o3)
    b=c(0,b)
    c=cumsum(no2)
    c=c(0,c)
    h=cumsum(temp)
    h=c(0,h)
    h1=cumsum(rain)
    h1=c(0,h1)
    
    
    pm10_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    o3_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    no2_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    temp_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    rain_1=matrix(data = NA,nrow=180,ncol=length(crcase))
    for(j in 1:180)
    { 
      for(j1 in 1:length(crcase)){
        pm10_1[j,j1]=a[181+7*(j1-1)]-a[181+7*(j1-1)-j]
        o3_1[j,j1]=b[181+7*(j1-1)]-b[181+7*(j1-1)-j]
        no2_1[j,j1]=c[181+7*(j1-1)]-c[181+7*(j1-1)-j]
        temp_1[j,j1]=h[181+7*(j1-1)]-h[181+7*(j1-1)-j]
        rain_1[j,j1]=h1[181+7*(j1-1)]-h1[181+7*(j1-1)-j]
      }
    }
    ################################### Those are the best parameters estimated for considering both
    ################################### air-pollution and temperature
    i1=129
    i2= 91.00000000
    i3= 110.00000000
    i4=143
    i5= 178
    thre1= 933.56888887
    tthre1=5112.88000000 
    k=19
    
    summ=numeric()
    for(i in 1:length(crcase))
    {summ[i]=sum(lag30crcase[(30+i-k):(29+i)])}
    sum=summ*lag1crcase
    
    pm=pm10_1[i1,]*lag1crcase
    o3=o3_1[i2,]*lag1crcase
    no2=no2_1[i3,]*lag1crcase
    r= (rain_1[i4,])*lag1crcase
    t= (temp_1[i5,])*lag1crcase
    
    
    thre2=(rain_1[i4,]-thre1)
    thre3=thre2*(thre2>0)*lag1crcase
    
    tthre2=(temp_1[i5,]-tthre1)
    tthre3=tthre2*(tthre2>0)*lag1crcase
    
    fit2=lm(crcase~lag1crcase+pm+o3+no2+r+thre3+t+tthre3+sum+0) ################ result based on 1-step prediction
    
    a=fit2$coefficients[1]
    b=fit2$coefficients[2]
    c=fit2$coefficients[3]
    d=fit2$coefficients[4]
    e=fit2$coefficients[5]
    f=fit2$coefficients[6]
    g=fit2$coefficients[7]
    h=fit2$coefficients[8]
    I=fit2$coefficients[9]
    
    
    p1=pm10_1[i1,]
    o1=o3_1[i2,]
    n1=no2_1[i3,]
    r1=(rain_1[i4,])
    t1=(temp_1[i5,])
    th1=thre2*(thre2>0)
    tth1=tthre2*(tthre2>0)
    
    beta=c(a,b,c,d,e,f,g,h,I)
    obj = function(beta)
    {
      err = 0
      y=crcase[1:k]
      for(i in (k+1):length(crcase))
      {
        y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
          beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
          beta[5]*(r1)[i]*y[i-1]+
          beta[6]*(th1)[i]*y[i-1]+
          beta[7]*(t1)[i]*y[i-1]+
          beta[8]*(tth1)[i]*y[i-1]+
          beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
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
      y[i]=beta[1]*y[i-1]+beta[2]*p1[i]*y[i-1]+
        beta[3]*o1[i]*y[i-1]+beta[4]*n1[i]*y[i-1]+
        beta[5]*(r1)[i]*y[i-1]+
        beta[6]*(th1)[i]*y[i-1]+
        beta[7]*(t1)[i]*y[i-1]+
        beta[8]*(tth1)[i]*y[i-1]+
        beta[9]*sum(y[(i-k):(i-1)])*y[i-1]
      y[i]=max(1,y[i])
      y[i]=min(2*max(crcase),y[i])
    }
    ###################### see how's the fitting
    c(beta[1],beta[2]*i1,beta[3]*i2,beta[4]*i3,beta[5]*i4,beta[6]*i4,beta[7]*i5,beta[8]*i5,beta[9],thre1/i4,tthre1/i5,k*7,i1,i2,i3,i4,i5)
    plot(crcase)
    lines(y,col='red')
##################################
    z=crcase
    z=z[(k+1):length(z)]
    y=y[(k+1):length(y)]
    d=seq(as.Date("2014/12/15"), as.Date("2020/7/1"), by = "week")
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
    p=p+ggtitle("Nakhon Sawan")
    p16=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
################################# consider only temperature and rainfall
    i4= 133
    i5= 127
    thre1=   8.031933e+02
    tthre1= 3.775400e+03 
    k=   25
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
  
    
    
    ###################################### The fitting is ok since it still captures the main dynamics
    c(beta[1],beta[2],beta[3]*i4,beta[4]*i4,beta[5]*i5,beta[6]*i5,thre1/i4,tthre1/i5,k*7,i4,i5)
    plot(crcase)
    lines(y,col='red')
      
     #####################################################     
          z=crcase
          z=z[(k+1):length(z)]
          y=y[(k+1):length(y)]
          d=seq(as.Date("2015/1/25"), as.Date("2020/7/1"), by = "week")
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
          p=p+ggtitle("Nakhon Sawan")
          p17=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
