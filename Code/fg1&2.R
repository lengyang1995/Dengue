##########################################Singapore first
library(lubridate)
library(scales)
library(ggplot2)
library(tidyverse)
#### SG
i1=23
i2=148
i3=124
i4=169
i5=58
thre1= 899.24
tthre1= 1637.98
k=17

############ parameters for rainfall
z=numeric()
y=numeric()
for(i in 1:550)
{
  t=thre1
  x=(507+i)
  x1=(x)/i4
  z[i]=x1
  y[i]= 2.384436e-04 *x -3.989025e-04*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily precipitation (mm)", y = "Effect of precipitation on dengue cases")+
  theme_bw() +ggtitle("Singapore ")+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
######################### lower bound
z=numeric()
y=numeric()
for(i in 1:550)
{
  t=thre1
  x=(507+i)
  x1=(x)/i4
  z[i]=x1
  y[i]=0.0001598005   *x -0.0003115514    *(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))

######################### upper bound
z=numeric()
y=numeric()
for(i in 1:550)
{
  t=thre1
  x=(507+i)
  x1=(x)/i4
  z[i]=x1
  y[i]= 0.0003033481    *x  -0.0004799231      *(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))+ylim(0.08,0.3)
sg4=p






######### For temperature
z=numeric()
y=numeric()
for(i in 1:200)
{
  t=tthre1/i5
  x=(1500+i)/i5
  z[i]=x
  y[i]=  2.150551e-03   *i5*x -1.611625e-03      *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily temperature (°C)", y = "Effect of temperature on dengue cases")+
  theme_bw() +ggtitle('Singapore')+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
############### lower bound
z=numeric()
y=numeric()
for(i in 1:200)
{
  t=tthre1/i5
  x=(1500+i)/i5
  z[i]=x
  y[i]=0.002090389 *i5*x-0.0009306322    *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
############### upper bound
z=numeric()
y=numeric()
for(i in 1:200)
{
  t=tthre1/i5
  x=(1500+i)/i5
  z[i]=x
  y[i]=  0.002214325   *i5*x-0.0022282569    *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))+ylim(3,3.8)
sg5=p

############################################################# Chonburi 
i1=168
i2= 142
i3=39
i4=155
i5= 120
thre1= 1211.08170816
tthre1=3419.66000000
k=2.800000e+01

z=numeric()
y=numeric()
for(i in 1:900)
{
  t=thre1
  x=i+500
  x1=(x)/i4
  z[i]=x1
  y[i]=(1.166799e-03   *x  -1.720569e-03  *(x-t)*I(x>t))
}
data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily precipitation (mm)", y = "Effect of precipitation on dengue cases")+
  theme_bw() +ggtitle("Chonburi ")+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
############## lower bound
z=numeric()
y=numeric()
for(i in 1:900)
{
  t=thre1
  x=i+500
  x1=(x)/i4
  z[i]=x1
  y[i]=(0.001076925*x-0.001461155     *(x-t)*I(x>t))
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))

############## upper bound
z=numeric()
y=numeric()
for(i in 1:900)
{
  t=thre1
  x=i+500
  x1=(x)/i4
  z[i]=x1
  y[i]= (0.001278365     *x-0.001944803 *(x-t)*I(x>t))
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
bk3=p
################################## temperature 
z=numeric()
y=numeric()
for(i in 1:700)
{
  t=tthre1/i5
  x=(2900+i)/i5
  z[i]=x
  y[i]=  6.264258e-04  *i5*x -1.424048e-04     *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily temperature (°C)", y = "Effect of temperature on dengue cases")+
  theme_bw() +ggtitle('Chonburi')+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
############################## lower bound
z=numeric()
y=numeric()
for(i in 1:700)
{
  t=tthre1/i5
  x=(2900+i)/i5
  z[i]=x
  y[i]=0.0005792628    *i5*x-0.0000556118      *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
################################# upper bound
z=numeric()
y=numeric()
for(i in 1:700)
{
  t=tthre1/i5
  x=(2900+i)/i5
  z[i]=x
  y[i]=0.0007032168 *i5*x-0.0002395671    *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
bk4=p

########################## Plot for Fg2 in the main context, see files 'SouthAmerica_plot' and 'SoutheastAsia.plot'







###################################### Not in use anymore
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

i4= 93
i5= 167
thre1=   934.6444 
tthre1= 4890.161
k=     30
z=numeric()
y=numeric()
for(i in 1:700)
{
  t=thre1
  x=i+400
  x1=(x)/i4
  z[i]=x1
  y[i]=(1.505757e-03   *x  -1.840155e-03  *(x-t)*I(x>t))
}
data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily precipitation (mm)", y = "Effect of precipitation on dengue cases")+
  theme_bw() +ggtitle("Bangkok ")+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
############## lower bound
z=numeric()
y=numeric()
for(i in 1:700)
{
  t=thre1
  x=i+400
  x1=(x)/i4
  z[i]=x1
  y[i]=(0.0008757038*x-0.001288361    *(x-t)*I(x>t))
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))

############## upper bound
z=numeric()
y=numeric()
for(i in 1:700)
{
  t=thre1
  x=i+400
  x1=(x)/i4
  z[i]=x1
  y[i]= (0.0022633350   *x-0.002653064 *(x-t)*I(x>t))
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
bk1=p
################################## temperature 
z=numeric()
y=numeric()
for(i in 1:1100)
{
  t=tthre1/i5
  x=(4100+i)/i5
  z[i]=x
  y[i]=  1.161452e-03   *i5*x  -1.066791e-03      *i5*(x-t)*I(x>t)
}

data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily temperature (°C)", y = "Effect of temperature on dengue cases")+
  theme_bw() +ggtitle('Bangkok')+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
############################## lower bound
z=numeric()
y=numeric()
for(i in 1:1100)
{
  t=tthre1/i5
  x=(4100+i)/i5
  z[i]=x
  y[i]=0.001012435     *i5*x-0.0006234596      *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
################################# upper bound
z=numeric()
y=numeric()
for(i in 1:1100)
{
  t=tthre1/i5
  x=(4100+i)/i5
  z[i]=x
  y[i]=0.001296374        *i5*x-0.0016540220   *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))+ylim(4,7)
bk2=p

################################################ SG 
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
i4= 149
i5= 131
thre1=  836.20
tthre1=   3664.80
k=      29

############ parameters for rainfall
z=numeric()
y=numeric()
for(i in 1:900)
{
  t=thre1
  x=(100+i)
  x1=(x)/i4
  z[i]=x1
  y[i]=3.006458e-04 *x -2.226402e-03   *(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily precipitation (mm)", y = "Effect of precipitation on dengue cases")+
  theme_bw() +ggtitle("Singapore ")+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
######################### lower bound
z=numeric()
y=numeric()
for(i in 1:900)
{
  t=thre1
  x=(100+i)
  x1=(x)/i4
  z[i]=x1
  y[i]=7.726771e-05   *x-0.001120765  *(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))

######################### upper bound
z=numeric()
y=numeric()
for(i in 1:900)
{
  t=thre1
  x=(100+i)
  x1=(x)/i4
  z[i]=x1
  y[i]= 7.634168e-04     *x   -0.003326652      *(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
sg1=p



######### For temperature
z=numeric()
y=numeric()
for(i in 1:600)
{
  t=tthre1/i5
  x=(3200+i)/i5
  z[i]=x
  y[i]=  7.379619e-04   *i5*x -6.386484e-04       *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- ggplot(data,  aes(z,y)) +geom_line(size=1.3)+labs(x = "Average daily temperature (°C)", y = "Effect of temperature on dengue cases")+
  theme_bw() +ggtitle('Singapore')+
  theme(plot.title = element_text(size=25,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold"))
p=p+theme(axis.text.x = element_text( hjust = 1, vjust = 0.5))
p=p + theme(plot.title = element_text(margin = margin(t = 10, b = -30)))
############### lower bound
z=numeric()
y=numeric()
for(i in 1:600)
{
  t=tthre1/i5
  x=(3200+i)/i5
  z[i]=x
  y[i]=0.0006777477 *i5*x-7.096956e-05*i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))
############### upper bound
z=numeric()
y=numeric()
for(i in 1:600)
{
  t=tthre1/i5
  x=(3200+i)/i5
  z[i]=x
  y[i]=  0.0008374304   *i5*x-1.675279e-03      *i5*(x-t)*I(x>t)
}
data=cbind(z,y)
data=as.data.frame(data)
p <- p+geom_line(size=1.3,lty=2,data=data,  aes(z,y))+ylim(2,3.5)
sg2=p