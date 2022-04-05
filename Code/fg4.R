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
###################### Switch both 2015-2016 with 2017-2018

r1=(rain_1[i4,])
t1=(temp_1[i5,])


thre2=(rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


tt1=t1
rr1=r1
th11=th1
tth11=tth1

##############################
rr1[129:233]=r1[24:128]
rr1[24:128]= r1[129:233]

th11[129:233]=th1[24:128]
th11[24:128]=th1[129:233]

tt1[129:233]=t1[24:128]
tt1[24:128]= t1[129:233]

tth11[129:233]=tth1[24:128]
tth11[24:128]=tth1[129:233]

yb=crcase[1:k]
for(i in (k+1):length(crcase))
{
  yb[i]=beta[1]*yb[i-1]+beta[2]*sum(yb[(i-k):(i-1)])*yb[i-1]+
    beta[3]*(rr1)[i]*yb[i-1]+ 
    beta[4]*(th11)[i]*yb[i-1]+ 
    beta[5]*(tt1)[i]*yb[i-1]+ 
    beta[6]*(tth11)[i]*yb[i-1] 
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}
lines(yb,col='blue')


y=y[(k+1):length(y)]
z=crcase
z=z[(k+1):length(z)]
yb=yb[(k+1):length(yb)]


d=seq(as.Date("2015/1/10"), as.Date("2020/12/26"), by = "week")

dd=d[(24-k):length(d)]
yy=yb[(24-k):length(yb)]

y1=cbind.data.frame(d,y)
d3=cbind.data.frame(dd,yy)

p <- ggplot(d3,  aes(dd,yy)) +geom_line(size=1.3,col='blue')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=(p+ggtitle("Costa Rica (switch all)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y")
)
cr1=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))



################### Switch temp
r1=(rain_1[i4,])
t1=(temp_1[i5,])


thre2=(rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


tt1=t1
rr1=r1
th11=th1
tth11=tth1

tt1[129:233]=t1[24:128]
tt1[24:128]= t1[129:233]

tth11[129:233]=tth1[24:128]
tth11[24:128]=tth1[129:233]

yb=crcase[1:k]
for(i in (k+1):length(crcase))
{
  yb[i]=beta[1]*yb[i-1]+beta[2]*sum(yb[(i-k):(i-1)])*yb[i-1]+
    beta[3]*(r1)[i]*yb[i-1]+ 
    beta[4]*(th1)[i]*yb[i-1]+ 
    beta[5]*(tt1)[i]*yb[i-1]+ 
    beta[6]*(tth11)[i]*yb[i-1] 
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}



z=crcase
z=z[(k+1):length(z)]
yb=yb[(k+1):length(yb)]


d=seq(as.Date("2015/1/10"), as.Date("2020/12/26"), by = "week")

dd=d[(24-k):length(d)]
yy=yb[(24-k):length(yb)]

y1=cbind.data.frame(d,y)
d3=cbind.data.frame(dd,yy)

p <- ggplot(d3,  aes(dd,yy)) +geom_line(size=1.3,col='blue')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=(p+ggtitle("Costa Rica (switch temperature)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y"))

cr2=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))



################### Switch rainfall
r1=(rain_1[i4,])
t1=(temp_1[i5,])


thre2=(rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)

th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)


tt1=t1
rr1=r1
th11=th1
tth11=tth1


rr1[129:233]=r1[24:128]
rr1[24:128]= r1[129:233]

th11[129:233]=th1[24:128]
th11[24:128]=th1[129:233]




yb=crcase[1:k]
for(i in (k+1):length(crcase))
{
  yb[i]=beta[1]*yb[i-1]+beta[2]*sum(yb[(i-k):(i-1)])*yb[i-1]+
    beta[3]*(rr1)[i]*yb[i-1]+ 
    beta[4]*(th11)[i]*yb[i-1]+ 
    beta[5]*(t1)[i]*yb[i-1]+ 
    beta[6]*(tth1)[i]*yb[i-1] 
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}


z=crcase
z=z[(k+1):length(z)]
yb=yb[(k+1):length(yb)]


d=seq(as.Date("2015/1/10"), as.Date("2020/12/26"), by = "week")

dd=d[(24-k):length(d)]
yy=yb[(24-k):length(yb)]

y1=cbind.data.frame(d,y)
d3=cbind.data.frame(dd,yy)

p <- ggplot(d3,  aes(dd,yy)) +geom_line(size=1.3,col='blue')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=(p+ggtitle("Costa Rica (switch precipitation)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y"))

cr3=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))




library(stats)
################## Singapore (only 2014~2020)
Singapore_number=dengueCases_SG
Singapore_number$case[732:1044]=Singapore_number$case[732:1044]*6
Singapore_number$case[1045:1096]=Singapore_number$case[1045:1096]*3

crcase=Singapore_number$case[762:1096]
lag1crcase=Singapore_number$case[761:1095]
lag30crcase=Singapore_number$case[732:1095]

############## Climate factors
pm10=Singapore_weather$pm10[36:2553]
o3=Singapore_weather$o3[36:2553]
no2=Singapore_weather$no2[36:2553]
temp=Singapore_weather$temp[36:2553]
rain=Singapore_weather$rain[36:2553]


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
plot(crcase)
lines(y,col='red')
###################### Switch all
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]

thre2= (rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

tt1=t1
rr1=r1
th11=th1
tth11=tth1
pp1=p1
oo1=o1
nn1=n1
################################


pp1[129:233]=p1[24:128]
pp1[24:128]= p1[129:233]
oo1[129:233]=o1[24:128]
oo1[24:128]= o1[129:233]
nn1[129:233]=n1[24:128]
nn1[24:128]= n1[129:233]

rr1[129:233]=r1[24:128]
rr1[24:128]= r1[129:233]

th11[129:233]=th1[24:128]
th11[24:128]=th1[129:233]

tt1[129:233]=t1[24:128]
tt1[24:128]= t1[129:233]

tth11[129:233]=tth1[24:128]
tth11[24:128]=tth1[129:233]

yb=crcase[1:k]
for(i in (k+1):length(crcase))
{
  yb[i]=beta[1]*yb[i-1]+
    beta[2]*(pp1)[i]*yb[i-1]+
    beta[3]*(oo1)[i]*yb[i-1]+
    beta[4]*(nn1)[i]*yb[i-1]+
    beta[5]*(rr1)[i]*yb[i-1]+ 
    beta[6]*(th11)[i]*yb[i-1]+ 
    beta[7]*(tt1)[i]*yb[i-1]+
    beta[8]*(tth11)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}
plot(y)
lines(yb,col='red')

y=y[(k+1):length(y)]
z=crcase
z=z[(k+1):length(z)]
yb=yb[(k+1):length(yb)]


d=seq(as.Date("2014/11/28"), as.Date("2020/12/27"), by = "week")

dd=d[(1):length(d)]
yy=yb[(1):length(yb)]

y1=cbind.data.frame(d,y)
d3=cbind.data.frame(dd,yy)

p <- ggplot(d3,  aes(dd,yy)) +geom_line(size=1.3,col='blue')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=(p+ggtitle("Singapore (switch all)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y")
)
sg1=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))+ylim(0,6000)


###################### Switch temp
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]

thre2= (rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

tt1=t1
rr1=r1
th11=th1
tth11=tth1

pp1=p1
oo1=o1
nn1=n1


########################
tt1[129:233]=t1[24:128]
tt1[24:128]= t1[129:233]

tth11[129:233]=tth1[24:128]
tth11[24:128]=tth1[129:233]

yb=crcase[1:k]
for(i in (k+1):length(crcase))
{
  yb[i]=beta[1]*yb[i-1]+
    beta[2]*(p1)[i]*yb[i-1]+
    beta[3]*(o1)[i]*yb[i-1]+
    beta[4]*(n1)[i]*yb[i-1]+
    beta[5]*(r1)[i]*yb[i-1]+ 
    beta[6]*(th1)[i]*yb[i-1]+ 
    beta[7]*(tt1)[i]*yb[i-1]+
    beta[8]*(tth11)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}



z=crcase
z=z[(k+1):length(z)]
yb=yb[(k+1):length(yb)]


d=seq(as.Date("2014/11/28"), as.Date("2020/12/27"), by = "week")

dd=d[(1):length(d)]
yy=yb[(1):length(yb)]

y1=cbind.data.frame(d,y)
d3=cbind.data.frame(dd,yy)

p <- ggplot(d3,  aes(dd,yy)) +geom_line(size=1.3,col='blue')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=(p+ggtitle("Singapore (switch temperature)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y")
)
sg2=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))+ylim(0,6000)

#################### Switch rain
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]

thre2= (rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

tt1=t1
rr1=r1
th11=th1
tth11=tth1

pp1=p1
oo1=o1
nn1=n1



rr1[129:233]=r1[24:128]
rr1[24:128]= r1[129:233]

th11[129:233]=th1[24:128]
th11[24:128]=th1[129:233]


yb=crcase[1:k]
for(i in (k+1):length(crcase))
{
  yb[i]=beta[1]*yb[i-1]+
    beta[2]*(p1)[i]*yb[i-1]+
    beta[3]*(o1)[i]*yb[i-1]+
    beta[4]*(n1)[i]*yb[i-1]+
    beta[5]*(rr1)[i]*yb[i-1]+ 
    beta[6]*(th11)[i]*yb[i-1]+ 
    beta[7]*(t1)[i]*yb[i-1]+
    beta[8]*(tth1)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}

z=crcase
z=z[(k+1):length(z)]
yb=yb[(k+1):length(yb)]


d=seq(as.Date("2014/11/28"), as.Date("2020/12/27"), by = "week")

dd=d[(1):length(d)]
yy=yb[(1):length(yb)]

y1=cbind.data.frame(d,y)
d3=cbind.data.frame(dd,yy)

p <- ggplot(d3,  aes(dd,yy)) +geom_line(size=1.3,col='blue')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=(p+ggtitle("Singapore (switch precipitation)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y")
)
sg3=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))+ylim(0,6000)



######################## consider all for Singapore in 2021, not in use anymore
df=Singapore_weather_2021
df$rain[is.na(df$rain)] <- mean(df$rain, na.rm = TRUE)
df$temp[is.na(df$temp)] <- mean(df$temp, na.rm = TRUE)



pm10=df$pm10[36:2923]
o3=df$o3[36:2923]
no2=df$no2[36:2923]
temp=df$temp[36:2923]
rain=df$rain[36:2923]


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


pm10_1=matrix(data = NA,nrow=180,ncol=386)
o3_1=matrix(data = NA,nrow=180,ncol=386)
no2_1=matrix(data = NA,nrow=180,ncol=386)
temp_1=matrix(data = NA,nrow=180,ncol=386)
rain_1=matrix(data = NA,nrow=180,ncol=386)
for(j in 1:180)
{ 
  for(j1 in 1:386){
    pm10_1[j,j1]=a[181+7*(j1-1)]-a[181+7*(j1-1)-j]
    o3_1[j,j1]=b[181+7*(j1-1)]-b[181+7*(j1-1)-j]
    no2_1[j,j1]=c[181+7*(j1-1)]-c[181+7*(j1-1)-j]
    temp_1[j,j1]=h[181+7*(j1-1)]-h[181+7*(j1-1)-j]
    rain_1[j,j1]=h1[181+7*(j1-1)]-h1[181+7*(j1-1)-j]
  }
}
i1=23
i2=148
i3=124
i4=169
i5=58
thre1= 899.24
tthre1= 1637.98
k=17


thre2=(rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)

p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=(rain_1[i4,])
t1=(temp_1[i5,])
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)
y=crcase[1:k]
for(i in (k+1):386)
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
###################### Switch all
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]

thre2= (rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

tt1=t1
rr1=r1
th11=th1
tth11=tth1
pp1=p1
oo1=o1
nn1=n1

pp1[336:386]=p1[285:335]
pp1[285:335]= p1[336:386]
oo1[336:386]=o1[285:335]
oo1[285:335]= o1[336:386]
nn1[336:386]=n1[285:335]
nn1[285:335]= n1[336:386]

rr1[336:386]=r1[285:335]
rr1[285:335]= r1[336:386]


tt1[336:386]=t1[285:335]
tt1[285:335]= t1[336:386]

th11[336:386]=th1[285:335]
th11[285:335]=th1[336:386]

tth11[336:386]=tth1[285:335]
tth11[285:335]=tth1[336:386]


yb=crcase[1:k]
for(i in (k+1):386)
{
  yb[i]=beta[1]*yb[i-1]+
    beta[2]*(pp1)[i]*yb[i-1]+
    beta[3]*(oo1)[i]*yb[i-1]+
    beta[4]*(nn1)[i]*yb[i-1]+
    beta[5]*(rr1)[i]*yb[i-1]+ 
    beta[6]*(th11)[i]*yb[i-1]+ 
    beta[7]*(tt1)[i]*yb[i-1]+
    beta[8]*(tth11)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}

plot(y,ylim=c(0,5000))
lines(yb,col='red')

y=y[(k+1):length(y)]
yb=yb[(k+1):length(yb)]
d=seq(as.Date("2014/11/30"), as.Date("2021/12/24"), by = "week")

dd=d[(285-k-1):(386-k-1)]
yy=yb[(285-k-1):(386-k-1)]
d1=d[(386-k-1):length(d)]
yb=yb[(386-k-1):length(yb)]

y1=cbind.data.frame(d,y)
d2=cbind.data.frame(d1,yb)
d3=cbind.data.frame(dd,yy)
p <- ggplot(d2,  aes(d1,yb)) +geom_line(size=1.3,col='cyan')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=p+geom_line(size=1.3,data=d3, aes(dd,yy),col='cyan')
p=(p+ggtitle("Singapore (switch all)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y")
)
sg4=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))+ylim(0,5500)
################################# temp
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]

thre2= (rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

tt1=t1
rr1=r1
th11=th1
tth11=tth1
pp1=p1
oo1=o1
nn1=n1



tt1[336:386]=t1[285:335]
tt1[285:335]= t1[336:386]
tth11[336:386]=tth1[285:335]
tth11[285:335]=tth1[336:386]


yb=crcase[1:k]
for(i in (k+1):386)
{
  yb[i]=beta[1]*yb[i-1]+
    beta[2]*(p1)[i]*yb[i-1]+
    beta[3]*(o1)[i]*yb[i-1]+
    beta[4]*(n1)[i]*yb[i-1]+
    beta[5]*(r1)[i]*yb[i-1]+ 
    beta[6]*(th1)[i]*yb[i-1]+ 
    beta[7]*(tt1)[i]*yb[i-1]+
    beta[8]*(tth11)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}

y=y[(k+1):length(y)]
yb=yb[(k+1):length(yb)]
d=seq(as.Date("2014/11/30"), as.Date("2021/12/24"), by = "week")

dd=d[(285-k-1):(386-k-1)]
yy=yb[(285-k-1):(386-k-1)]
d1=d[(386-k-1):length(d)]
yb=yb[(386-k-1):length(yb)]

y1=cbind.data.frame(d,y)
d2=cbind.data.frame(d1,yb)
d3=cbind.data.frame(dd,yy)
p <- ggplot(d2,  aes(d1,yb)) +geom_line(size=1.3,col='cyan')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=p+geom_line(size=1.3,data=d3, aes(dd,yy),col='cyan')
p=(p+ggtitle("Singapore (switch temperature)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y")
)
sg5=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))+ylim(0,5500)
################################# percipitation
p1=pm10_1[i1,]
o1=o3_1[i2,]
n1=no2_1[i3,]
r1=rain_1[i4,]
t1=temp_1[i5,]

thre2= (rain_1[i4,]-thre1)
tthre2=(temp_1[i5,]-tthre1)
th1=thre2*(thre2>0)
tth1=tthre2*(tthre2>0)

tt1=t1
rr1=r1
th11=th1
tth11=tth1
pp1=p1
oo1=o1
nn1=n1




rr1[336:386]=r1[285:335]
rr1[285:335]= r1[336:386]



th11[336:386]=th1[285:335]
th11[285:335]=th1[336:386]


yb=crcase[1:k]
for(i in (k+1):386)
{
  yb[i]=beta[1]*yb[i-1]+
    beta[2]*(p1)[i]*yb[i-1]+
    beta[3]*(o1)[i]*yb[i-1]+
    beta[4]*(n1)[i]*yb[i-1]+
    beta[5]*(rr1)[i]*yb[i-1]+ 
    beta[6]*(th11)[i]*yb[i-1]+ 
    beta[7]*(t1)[i]*yb[i-1]+
    beta[8]*(tth1)[i]*yb[i-1]+
    beta[9]*sum(yb[(i-k):(i-1)])*yb[i-1]
  yb[i]=max(1,yb[i])
  yb[i]=min(2*max(crcase),yb[i])
}

y=y[(k+1):length(y)]
yb=yb[(k+1):length(yb)]
d=seq(as.Date("2014/11/30"), as.Date("2021/12/24"), by = "week")

dd=d[(285-k-1):(386-k-1)]
yy=yb[(285-k-1):(386-k-1)]
d1=d[(386-k-1):length(d)]
yb=yb[(386-k-1):length(yb)]

y1=cbind.data.frame(d,y)
d2=cbind.data.frame(d1,yb)
d3=cbind.data.frame(dd,yy)
p <- ggplot(d2,  aes(d1,yb)) +geom_line(size=1.3,col='cyan')
p <- p +labs(x = "", y = "Weekly dengue cases")+geom_line(size=1.3,data=y1, aes(d,y),col='red')
p=p+geom_line(size=1.3,data=d3, aes(dd,yy),col='cyan')
p=(p+ggtitle("Singapore (switch precipitation)") +theme_bw() +
     theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
           axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
           axis.title.y = element_text( size=18,face="bold") )+
     scale_x_date(date_breaks = "1 years", date_labels = "%Y")
)
sg6=p+ theme(plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title.x = element_text( size=18, face="bold"),
             axis.text.x = element_text(color = "grey20", size =13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.text.y = element_text(color = "grey20", size = 13, angle = 0, hjust = 1, vjust = 0, face = "bold"),
             axis.title.y = element_text( size=18,face="bold") )+theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5))+ylim(0,5500)

