################## fg3 and fg4 for sg
library(tidyverse)
################## parameters 
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
beta=c(-1.732347e+00 ,-3.019847e-06,  3.006458e-04 ,-2.226402e-03 , 7.379619e-04 ,-6.386484e-04 ) ## calculated before

#####################################
i4= 149
i5= 131
thre1=  836.20
tthre1=   3664.80
k=      29
##### Constant not in used anymore
sg14=Singapore_weather
pm10=o3=no2=t=r=numeric()
for( i in 1:365)
{
  t[i]=mean(sg14$temp)
  r[i]=mean(sg14$rain)
}
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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


r1=rain_1[i4,]
t1=temp_1[i5,]
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb=crcase[1:k]
for(i in (k+1):2200)
{
  yb[i]=beta[1]*yb[i-1]+beta[2]*sum(yb[(i-k):(i-1)])*yb[i-1]+
    beta[3]*(r1)[i]*yb[i-1]+
    beta[4]*(th1)[i]*yb[i-1]+
    beta[5]*(t1)[i]*yb[i-1]+
    beta[6]*(tth1)[i]*yb[i-1]
}
yb=cbind(c(1:313),yb[888:1200])


########################### both choose mean value
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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


r1=rain_1[i4,]
t1=temp_1[i5,]
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


yb1=crcase[1:k]
for(i in (k+1):1200)
{
  yb1[i]=beta[1]*yb1[i-1]+beta[2]*sum(yb1[(i-k):(i-1)])*yb1[i-1]+
    beta[3]*(r1)[i]*yb1[i-1]+
    beta[4]*(th1)[i]*yb1[i-1]+
    beta[5]*(t1)[i]*yb1[i-1]+
    beta[6]*(tth1)[i]*yb1[i-1]
  yb1[i]=max(yb1[i],1)
}
yb1=cbind(c(1:313),yb1[888:1200])


########################### rainfall mean value and temperature is 1 lower than mean
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
t=t-1
################################
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)

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


r1=rain_1[i4,]
t1=temp_1[i5,]
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)



yb2=crcase[1:k]
for(i in (k+1):1200)
{
  yb2[i]=beta[1]*yb2[i-1]+beta[2]*sum(yb2[(i-k):(i-1)])*yb2[i-1]+
    beta[3]*(r1)[i]*yb2[i-1]+
    beta[4]*(th1)[i]*yb2[i-1]+
    beta[5]*(t1)[i]*yb2[i-1]+
    beta[6]*(tth1)[i]*yb2[i-1]
  yb2[i]=max(yb2[i],1)
}
yb2=cbind(c(1:313),yb2[888:1200])

########################### rainfall mean value and temperature is highest
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = max(temp))
t=az$avg_value
################################
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)



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


r1=rain_1[i4,]
t1=temp_1[i5,]
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


yb4=crcase[1:k]
for(i in (k+1):1200)
{
  yb4[i]=beta[1]*yb4[i-1]+beta[2]*sum(yb4[(i-k):(i-1)])*yb4[i-1]+
    beta[3]*(r1)[i]*yb4[i-1]+
    beta[4]*(th1)[i]*yb4[i-1]+
    beta[5]*(t1)[i]*yb4[i-1]+
    beta[6]*(tth1)[i]*yb4[i-1]
  yb4[i]=max(yb4[i],1)
}
yb4=cbind(c(1:313),yb4[888:1200])



yb=as.data.frame(yb)
yb1=as.data.frame(yb1)
yb2=as.data.frame(yb2)
yb4=as.data.frame(yb4)
plot(yb4,ylim=c(0,8000),col='orange',type='l')
lines(yb,col='red')
lines(yb1,col='cyan')
lines(yb2,col='black')

sgtemp=rbind(yb1,yb2,yb4)
ps=read_csv('sgtemp.csv')


p=ggplot(ps, aes(V1, V2)) +
  geom_line(aes(group = group, color = group),lwd=1.2)
p <- p +labs(x = "Year", y = "Simulated weekly cases")
p=p+scale_color_manual(values=c("cyan",'orange','black'))
p=p+theme_bw()+theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
                     axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.title.y = element_text( size=25,face="bold"))+theme(legend.position = c(0.5, 0.85))
p=p+theme(legend.title = element_blank()) +theme(legend.text=element_text(size=20,face = "bold"))
p=p+scale_x_continuous(labels = c("0", "2", "4", "6"))+ylim(0,8000)
sgt=p









#################################  optimal rainfall and average temperature
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)
for (i in 1:length(r)) {
  if(r[i]<thre1/i4)
  {
    r[i]=r[i]+1
  }
}

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################

temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)



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


r1=rain_1[i4,]
t1=temp_1[i5,]
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


yb5=crcase[1:k]
for(i in (k+1):1200)
{
  yb5[i]=beta[1]*yb5[i-1]+beta[2]*sum(yb5[(i-k):(i-1)])*yb5[i-1]+
    beta[3]*(r1)[i]*yb5[i-1]+
    beta[4]*(th1)[i]*yb5[i-1]+
    beta[5]*(t1)[i]*yb5[i-1]+
    beta[6]*(tth1)[i]*yb5[i-1]
  yb5[i]=max(yb5[i],1)
}
yb5=cbind(c(1:313),yb5[788:1100])

################################# 0.8 of optimal rainfall and average temperature
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)
for (i in 1:length(r)) {
  if(r[i]<thre1/i4)
  {
    r[i]=r[i]+1
  }
}
r=r*0.8

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################

temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)


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


r1=rain_1[i4,]
t1=temp_1[i5,]
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


yb6=crcase[1:k]
for(i in (k+1):1200)
{
  yb6[i]=beta[1]*yb6[i-1]+beta[2]*sum(yb6[(i-k):(i-1)])*yb6[i-1]+
    beta[3]*(r1)[i]*yb6[i-1]+
    beta[4]*(th1)[i]*yb6[i-1]+
    beta[5]*(t1)[i]*yb6[i-1]+
    beta[6]*(tth1)[i]*yb6[i-1]
  yb6[i]=max(yb6[i],1)
}
yb6=cbind(c(1:313),yb6[788:1100])

################################# 1.2 of optimal rainfall and average temperature
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)
r=(az$avg_value)
for (i in 1:length(r)) {
  if(r[i]<thre1/i4)
  {
    r[i]=r[i]+1
  }
}
r=r*1.2

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################

temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)

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


r1=rain_1[i4,]
t1=temp_1[i5,]
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


yb7=crcase[1:k]
for(i in (k+1):1200)
{
  yb7[i]=beta[1]*yb7[i-1]+beta[2]*sum(yb7[(i-k):(i-1)])*yb7[i-1]+
    beta[3]*(r1)[i]*yb7[i-1]+
    beta[4]*(th1)[i]*yb7[i-1]+
    beta[5]*(t1)[i]*yb7[i-1]+
    beta[6]*(tth1)[i]*yb7[i-1]
  yb7[i]=max(yb7[i],1)
}
yb7=cbind(c(1:313),yb7[788:1100])



yb5=as.data.frame(yb5)
yb6=as.data.frame(yb6)
yb7=as.data.frame(yb7)

plot(yb5,col='orange',type='l',ylim=c(0,8000))
lines(yb6,col='blue')
lines(yb7,col='purple')

sgrain=rbind(yb5,yb6,yb7)
write_csv(sgrain,'sgrain.csv')
ps1=read_csv('sgrain.csv')


p=ggplot(ps1, aes(V1, V2)) +
  geom_line(aes(group = group, color = group),lwd=1.2)
p <- p +labs(x = "Year", y = "Simulated weekly cases")
p=p+scale_color_manual(values=c("purple", "blue",'coral'))
p=p+theme_bw()+theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
                     axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.title.y = element_text( size=25,face="bold"))+theme(legend.position = c(0.5, 0.85))
p=p+theme(legend.title = element_blank()) +theme(legend.text=element_text(size=20,face = "bold"))
p=p+scale_x_continuous(labels = c("0", "2", "4", "6"))+ylim(0,8000)
sgr=p


######################################################################################
############################## change temp
ss1=numeric()
ss2=numeric()
for (ii in 1:90) {
  
  df=sg14
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(rain))
  r=(az$avg_value)

  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(temp))
  t=az$avg_value
  
  t=t-1.8+(0.04*ii)
  
  temp=c(t,t,t,t,t,t,t)
  rain=c(r,r,r,r,r,r,r)
  
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
  
  
  r1=rain_1[i4,]
  t1=temp_1[i5,]
  t1=c(t1,t1,t1)
  t1=c(t1,t1,t1)
  r1=c(r1,r1,r1)
  r1=c(r1,r1,r1)
  
  thre2=(r1-thre1)
  tthre2=(t1-tthre1)
  
  th1=thre2*(thre2>0)
  tth1=tthre2*(tthre2>0)
  
  
  yb7=crcase[1:k]
  for(i in (k+1):2500)
  {
    yb7[i]=beta[1]*yb7[i-1]+beta[2]*sum(yb7[(i-k):(i-1)])*yb7[i-1]+
      beta[3]*(r1)[i]*yb7[i-1]+
      beta[4]*(th1)[i]*yb7[i-1]+
      beta[5]*(t1)[i]*yb7[i-1]+
      beta[6]*(tth1)[i]*yb7[i-1]
    yb7[i]=max(yb7[i],1)
  }
  
  ss1[ii]=mean(yb7[500:2500])
  ss2[ii]=mean(t1/i5)
}


data=cbind.data.frame(ss2,ss1)
p <- ggplot(data,  aes(ss2,ss1)) +geom_line(size=1.3)
p <- p +labs(x = "Average daily temperature (°C)", y = "Average simulated cases")
sgt1=p +theme_bw() +ylim(0,3000)+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=25,face="bold"))
### calculate the effect of temperature on simulated cases
(ss1[90]-ss1[44]) /(ss2[90]-ss2[44])/10#  +20
(ss1[44]-ss1[1]) /(ss2[44]-ss2[1])/10 #   +96
##################################### change precipitation
ss3=numeric()
ss4=numeric()
for (ii in 1:90) {
  
  df=sg14
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(rain))
  r=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(temp))
  t=az$avg_value
  
  
  r=r-5+(0.076*ii)
  r=r*(r>0)
  
  temp=c(t,t,t,t,t,t,t)
  rain=c(r,r,r,r,r,r,r)
  temp=c(t,t,t,t,t,t,t)
  rain=c(r,r,r,r,r,r,r)
  
  
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
  
  
  r1=rain_1[i4,]
  t1=temp_1[i5,]
  t1=c(t1,t1,t1)
  t1=c(t1,t1,t1)
  r1=c(r1,r1,r1)
  r1=c(r1,r1,r1)
  
  thre2=(r1-thre1)
  tthre2=(t1-tthre1)
  
  th1=thre2*(thre2>0)
  tth1=tthre2*(tthre2>0)
  
  
  yb7=crcase[1:k]
  for(i in (k+1):2500)
  {
    yb7[i]=beta[1]*yb7[i-1]+beta[2]*sum(yb7[(i-k):(i-1)])*yb7[i-1]+
      beta[3]*(r1)[i]*yb7[i-1]+
      beta[4]*(th1)[i]*yb7[i-1]+
      beta[5]*(t1)[i]*yb7[i-1]+
      beta[6]*(tth1)[i]*yb7[i-1]
    yb7[i]=max(yb7[i],1)
  }
  
  ss3[ii]=mean(yb7[500:2500])
  ss4[ii]=mean(r1/i4)
}
data=cbind.data.frame(ss4,ss3)
p <- ggplot(data,  aes(ss4,ss3)) +geom_line(size=1.3)
p <- p +labs(x = "Average daily precipitation  (mm)", y = "Average simulated cases")
sgr1=p +theme_bw() +ylim(0,3000)+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=25,face="bold"))
### calculate the effect of precipitation on simulated cases
(ss3[90]-ss3[66]) /(ss4[90]-ss4[66])/10 ## -99
(ss3[66]-ss3[1]) /(ss4[66]-ss4[1])/10   ## 48





######################################### consider all conditions, not in use anymore
library(tidyverse)
################## parameters 
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
beta=c(-3.185727e+00 ,-7.121491e-05,  1.617189e-04, -9.611759e-05,  3.441867e-04, -3.649091e-04,  1.369408e-03, -2.827601e-04, -4.553725e-06  ) ## calculated before
#####################################
i1=30
i2=108
i3=46
i4=88
i5=103
thre1= 383.5
tthre1=  2939.50
k=10
## Constant 
sg14=Singapore_weather
p=o=n=t=r=numeric()
for( i in 1:365)
{
  p[i]=mean(sg14$pm10)
  o[i]=mean(sg14$o3)
  n[i]=mean(sg14$no2)
  t[i]=mean(sg14$temp)
  r[i]=mean(sg14$rain)
}
################################
pm10=c(p,p,p,p,p,p,p)
o3=c(o,o,o,o,o,o,o)
no2=c(n,n,n,n,n,n,n)
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]


p1=c(p1,p1,p1)
p1=c(p1,p1,p1)
o1=c(o1,o1,o1)
o1=c(o1,o1,o1)
n1=c(n1,n1,n1)
n1=c(n1,n1,n1)
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb=crcase[1:k]
for(i in (k+1):1200)
{
  yb[i]=beta[1]*yb[i-1]+beta[2]*p1[i]*yb[i-1]+
    beta[3]*o1[i]*yb[i-1]+beta[4]*n1[i]*yb[i-1]+
    beta[5]*(r1)[i]*yb[i-1]+
    beta[6]*(th1)[i]*yb[i-1]+
    beta[7]*(t1)[i]*yb[i-1]+
    beta[8]*(tth1)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
}
yb=cbind(c(1:313),yb[888:1200])


########################### all choose mean value
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(pm10))
p=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(o3))
o=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(no2))
n=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################




pm10=c(p,p,p,p,p,p,p)
o3=c(o,o,o,o,o,o,o)
no2=c(n,n,n,n,n,n,n)
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]


p1=c(p1,p1,p1)
p1=c(p1,p1,p1)
o1=c(o1,o1,o1)
o1=c(o1,o1,o1)
n1=c(n1,n1,n1)
n1=c(n1,n1,n1)
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb1=crcase[1:k]
for(i in (k+1):1200)
{
  yb1[i]=beta[1]*yb1[i-1]+beta[2]*p1[i]*yb1[i-1]+
    beta[3]*o1[i]*yb1[i-1]+beta[4]*n1[i]*yb1[i-1]+
    beta[5]*(r1)[i]*yb1[i-1]+
    beta[6]*(th1)[i]*yb1[i-1]+
    beta[7]*(t1)[i]*yb1[i-1]+
    beta[8]*(tth1)[i]*yb1[i-1]+
    beta[9]*sum(yb1[(i-k):(i-1)])*yb1[i-1]
}


yb1=cbind(c(1:313),yb1[888:1200])


########################### rainfall mean value and temperature is 0.5 lower than mean
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(pm10))
p=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(o3))
o=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(no2))
n=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
t=t-0.25
################################




pm10=c(p,p,p,p,p,p,p)
o3=c(o,o,o,o,o,o,o)
no2=c(n,n,n,n,n,n,n)
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]


p1=c(p1,p1,p1)
p1=c(p1,p1,p1)
o1=c(o1,o1,o1)
o1=c(o1,o1,o1)
n1=c(n1,n1,n1)
n1=c(n1,n1,n1)
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb2=crcase[1:k]
for(i in (k+1):1200)
{
  yb2[i]=beta[1]*yb2[i-1]+beta[2]*p1[i]*yb2[i-1]+
    beta[3]*o1[i]*yb2[i-1]+beta[4]*n1[i]*yb2[i-1]+
    beta[5]*(r1)[i]*yb2[i-1]+
    beta[6]*(th1)[i]*yb2[i-1]+
    beta[7]*(t1)[i]*yb2[i-1]+
    beta[8]*(tth1)[i]*yb2[i-1]+
    beta[9]*sum(yb2[(i-k):(i-1)])*yb2[i-1]
}


yb2=cbind(c(1:313),yb2[888:1200])

########################### other mean value and temperature is high
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(pm10))
p=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(o3))
o=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(no2))
n=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
t=t+0.25
################################




pm10=c(p,p,p,p,p,p,p)
o3=c(o,o,o,o,o,o,o)
no2=c(n,n,n,n,n,n,n)
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]


p1=c(p1,p1,p1)
p1=c(p1,p1,p1)
o1=c(o1,o1,o1)
o1=c(o1,o1,o1)
n1=c(n1,n1,n1)
n1=c(n1,n1,n1)
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb4=crcase[1:k]
for(i in (k+1):1200)
{
  yb4[i]=beta[1]*yb4[i-1]+beta[2]*p1[i]*yb4[i-1]+
    beta[3]*o1[i]*yb4[i-1]+beta[4]*n1[i]*yb4[i-1]+
    beta[5]*(r1)[i]*yb4[i-1]+
    beta[6]*(th1)[i]*yb4[i-1]+
    beta[7]*(t1)[i]*yb4[i-1]+
    beta[8]*(tth1)[i]*yb4[i-1]+
    beta[9]*sum(yb4[(i-k):(i-1)])*yb4[i-1]
}


yb4=cbind(c(1:313),yb4[888:1200])





yb=as.data.frame(yb)
yb1=as.data.frame(yb1)
yb2=as.data.frame(yb2)
yb4=as.data.frame(yb4)
plot(yb4,ylim=c(0,8000),col='orange',type='l')
lines(yb,col='red')
lines(yb1,col='cyan')
lines(yb2,col='black')
ps=rbind(yb,yb1,yb2,yb4)
ps=read_csv('ps_all.csv')


p=ggplot(ps, aes(V1, V2)) +
  geom_line(aes(group = group, color = group),lwd=1.2)
p <- p +labs(x = "Year", y = "Simulated weekly cases")
p=p+scale_color_manual(values=c("cyan", "red",'orange','black'))
p=p+theme_bw()+theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
                     axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.title.y = element_text( size=25,face="bold"))+theme(legend.position = c(0.5, 0.85))
p=p+theme(legend.title = element_blank()) +theme(legend.text=element_text(size=20,face = "bold"))
p=p+scale_x_continuous(labels = c("0", "2", "4", "6"))+ylim(0,5000)
sgt=p



########################### optimal
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(pm10))
p=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(o3))
o=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(no2))
n=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(rain))
r=(az$avg_value)
for (i in 1:366) {
  if(r[i]>thre1/i4)
    {
    r[i]=r[i]-0.54
  }
  else
  {
    r[i]=r[i]+0.54
  }
}


az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################




pm10=c(p,p,p,p,p,p,p)
o3=c(o,o,o,o,o,o,o)
no2=c(n,n,n,n,n,n,n)
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]


p1=c(p1,p1,p1)
p1=c(p1,p1,p1)
o1=c(o1,o1,o1)
o1=c(o1,o1,o1)
n1=c(n1,n1,n1)
n1=c(n1,n1,n1)
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb=crcase[1:k]
for(i in (k+1):1200)
{
  yb[i]=beta[1]*yb[i-1]+beta[2]*p1[i]*yb[i-1]+
    beta[3]*o1[i]*yb[i-1]+beta[4]*n1[i]*yb[i-1]+
    beta[5]*(r1)[i]*yb[i-1]+
    beta[6]*(th1)[i]*yb[i-1]+
    beta[7]*(t1)[i]*yb[i-1]+
    beta[8]*(tth1)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
}


yb=cbind(c(1:313),yb[888:1200])







################################# low rainfall and average temperature
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(pm10))
p=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(o3))
o=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(no2))
n=(az$avg_value)

#az=df %>%
  #group_by(month = month(date), day = day(date)) %>%
  #summarize(avg_value = mean(rain))
#r=(az$avg_value)
#r=r-0.940745
r=sg14$rain[(2+365):(366+366)]


az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################




pm10=c(p,p,p,p,p,p,p)
o3=c(o,o,o,o,o,o,o)
no2=c(n,n,n,n,n,n,n)
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]


p1=c(p1,p1,p1)
p1=c(p1,p1,p1)
o1=c(o1,o1,o1)
o1=c(o1,o1,o1)
n1=c(n1,n1,n1)
n1=c(n1,n1,n1)
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb1=crcase[1:k]
for(i in (k+1):1200)
{
  yb1[i]=beta[1]*yb1[i-1]+beta[2]*p1[i]*yb1[i-1]+
    beta[3]*o1[i]*yb1[i-1]+beta[4]*n1[i]*yb1[i-1]+
    beta[5]*(r1)[i]*yb1[i-1]+
    beta[6]*(th1)[i]*yb1[i-1]+
    beta[7]*(t1)[i]*yb1[i-1]+
    beta[8]*(tth1)[i]*yb1[i-1]+
    beta[9]*sum(yb1[(i-k):(i-1)])*yb1[i-1]
}


yb1=cbind(c(1:313),yb1[888:1200])


########################### high rainfall and other average
df=sg14
az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(pm10))
p=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(o3))
o=(az$avg_value)

az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(no2))
n=(az$avg_value)

#az=df %>%
  #group_by(month = month(date), day = day(date)) %>%
  #summarize(avg_value = mean(rain))
#r=(az$avg_value)
#r=r+0.83
r=sg14$rain[(2+365+365+366):(366+366+365+366)]


az=df %>%
  group_by(month = month(date), day = day(date)) %>%
  summarize(avg_value = mean(temp))
t=az$avg_value
################################




pm10=c(p,p,p,p,p,p,p)
o3=c(o,o,o,o,o,o,o)
no2=c(n,n,n,n,n,n,n)
temp=c(t,t,t,t,t,t,t)
rain=c(r,r,r,r,r,r,r)
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
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]


p1=c(p1,p1,p1)
p1=c(p1,p1,p1)
o1=c(o1,o1,o1)
o1=c(o1,o1,o1)
n1=c(n1,n1,n1)
n1=c(n1,n1,n1)
t1=c(t1,t1,t1)
t1=c(t1,t1,t1)
r1=c(r1,r1,r1)
r1=c(r1,r1,r1)

thre2=(r1-thre1)
tthre2=(t1-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

yb2=crcase[1:k]
for(i in (k+1):1200)
{
  yb2[i]=beta[1]*yb2[i-1]+beta[2]*p1[i]*yb2[i-1]+
    beta[3]*o1[i]*yb2[i-1]+beta[4]*n1[i]*yb2[i-1]+
    beta[5]*(r1)[i]*yb2[i-1]+
    beta[6]*(th1)[i]*yb2[i-1]+
    beta[7]*(t1)[i]*yb2[i-1]+
    beta[8]*(tth1)[i]*yb2[i-1]+
    beta[9]*sum(yb2[(i-k):(i-1)])*yb2[i-1]
}


yb2=cbind(c(1:313),yb2[888:1200])



yb=as.data.frame(yb)
yb1=as.data.frame(yb1)
yb2=as.data.frame(yb2)

plot(yb,ylim=c(0,3000),col='orange',type='l')
lines(yb1,col='blue')
lines(yb2,col='purple')

ps1=rbind(yb,yb1,yb2)
write_csv(ps1,'ps1_all.csv')
ps1=read_csv('ps1_all.csv')


p=ggplot(ps1, aes(V1, V2)) +
  geom_line(aes(group = group, color = group),lwd=1.2)
p <- p +labs(x = "Year", y = "Simulated weekly cases")
p=p+scale_color_manual(values=c("purple", "blue",'coral'))
p=p+theme_bw()+theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
                     axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
                     axis.title.y = element_text( size=25,face="bold"))+theme(legend.position = c(0.5, 0.85))
p=p+theme(legend.title = element_blank()) +theme(legend.text=element_text(size=20,face = "bold"))
p=p+scale_x_continuous(labels = c("0", "2", "4", "6"))+ylim(0,5000)
sgr=p


###################################
############################## change temp
ss1=numeric()
ss2=numeric()
for (ii in 1:90) {
  
  df=sg14
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(pm10))
  p=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(o3))
  o=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(no2))
  n=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(rain))
  r=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(temp))
  t=az$avg_value
  ################################
  t=t-0.4+(0.02*ii)
  
  
  
  pm10=c(p,p,p,p,p,p,p)
  o3=c(o,o,o,o,o,o,o)
  no2=c(n,n,n,n,n,n,n)
  temp=c(t,t,t,t,t,t,t)
  rain=c(r,r,r,r,r,r,r)
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
  p1=pm10_1[i1,]
  o1=o3_1[i2,]
  n1=no2_1[i3,]
  r1=rain_1[i4,]
  t1=temp_1[i5,]
  
  
  p1=c(p1,p1,p1)
  p1=c(p1,p1,p1)
  o1=c(o1,o1,o1)
  o1=c(o1,o1,o1)
  n1=c(n1,n1,n1)
  n1=c(n1,n1,n1)
  t1=c(t1,t1,t1)
  t1=c(t1,t1,t1)
  r1=c(r1,r1,r1)
  r1=c(r1,r1,r1)
  
  thre2=(r1-thre1)
  tthre2=(t1-tthre1)
  
  th1=thre2*(thre2>0)
  tth1=tthre2*(tthre2>0)
  
  yb1=crcase[1:k]
  for(i in (k+1):3000)
  {
    yb1[i]=beta[1]*yb1[i-1]+beta[2]*p1[i]*yb1[i-1]+
      beta[3]*o1[i]*yb1[i-1]+beta[4]*n1[i]*yb1[i-1]+
      beta[5]*(r1)[i]*yb1[i-1]+
      beta[6]*(th1)[i]*yb1[i-1]+
      beta[7]*(t1)[i]*yb1[i-1]+
      beta[8]*(tth1)[i]*yb1[i-1]+
      beta[9]*sum(yb1[(i-k):(i-1)])*yb1[i-1]
    yb1[i]=max(yb1[i],10)
  }
  
  ss1[ii]=mean(yb1[500:2500])
  ss2[ii]=mean(t1/i5)
}


data=cbind.data.frame(ss2,ss1)
p <- ggplot(data,  aes(ss2,ss1)) +geom_line(size=1.3)
p <- p +labs(x = "Average daily temperature (°C)", y = "Average simulated cases")
sgt1=p +theme_bw() +
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=25,face="bold"))
#### calculate the effect of temperature on simulated cases
(ss1[90]-ss1[19]) /(ss2[90]-ss2[19])/10# 263
(ss1[19]-ss1[1]) /(ss2[19]-ss2[1])/10 #  303

ss3=numeric()
ss4=numeric()
for (ii in 1:90) {
  
  df=sg14
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(pm10))
  p=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(o3))
  o=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(no2))
  n=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(rain))
  r=(az$avg_value)
  
  az=df %>%
    group_by(month = month(date), day = day(date)) %>%
    summarize(avg_value = mean(temp))
  t=az$avg_value
  
  
  r=r-3.5+(0.065*ii)
  r=r*(r>0)
  
  pm10=c(p,p,p,p,p,p,p)
  o3=c(o,o,o,o,o,o,o)
  no2=c(n,n,n,n,n,n,n)
  temp=c(t,t,t,t,t,t,t)
  rain=c(r,r,r,r,r,r,r)
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
  p1=pm10_1[i1,]
  o1=o3_1[i2,]
  n1=no2_1[i3,]
  r1=rain_1[i4,]
  t1=temp_1[i5,]
  
  
  p1=c(p1,p1,p1)
  p1=c(p1,p1,p1)
  o1=c(o1,o1,o1)
  o1=c(o1,o1,o1)
  n1=c(n1,n1,n1)
  n1=c(n1,n1,n1)
  t1=c(t1,t1,t1)
  t1=c(t1,t1,t1)
  r1=c(r1,r1,r1)
  r1=c(r1,r1,r1)
  
  thre2=(r1-thre1)
  tthre2=(t1-tthre1)
  
  th1=thre2*(thre2>0)
  tth1=tthre2*(tthre2>0)
  
  yb1=crcase[1:k]
  for(i in (k+1):3000)
  {
    yb1[i]=beta[1]*yb1[i-1]+beta[2]*p1[i]*yb1[i-1]+
      beta[3]*o1[i]*yb1[i-1]+beta[4]*n1[i]*yb1[i-1]+
      beta[5]*(r1)[i]*yb1[i-1]+
      beta[6]*(th1)[i]*yb1[i-1]+
      beta[7]*(t1)[i]*yb1[i-1]+
      beta[8]*(tth1)[i]*yb1[i-1]+
      beta[9]*sum(yb1[(i-k):(i-1)])*yb1[i-1]
    yb1[i]=max(yb1[i],10)
  }
  
  ss3[ii]=mean(yb1[500:2500])
  ss4[ii]=mean(r1/i4)
}
data=cbind.data.frame(ss4,ss3)
p <- ggplot(data,  aes(ss4,ss3)) +geom_line(size=1.3)
p <- p +labs(x = "Average daily precipitation  (mm)", y = "Average simulated cases")
sgr1=p +theme_bw() +ylim(0,3000)+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=25, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=25,face="bold"))
### calculate the effect of precipitation on simulated cases
(ss3[90]-ss3[50]) /(ss4[90]-ss4[50])/10 ## -7.900948
(ss3[50]-ss3[1]) /(ss4[50]-ss4[1])/10   ## +52.29145