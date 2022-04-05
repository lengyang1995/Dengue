library(mgcv)
#################### fg5 and fg6
#################### get dengue and GAI data as well as weather factors
sg_mos=Singapore_gai
crcase=sg_mos$number
d=seq(as.Date("2014-01-05 "), as.Date("2016-09-11"), by = "week")
fr1=cbind.data.frame(d,crcase)
Singapore_number=dengueCases_SG
Singapore_number$case[732:1044]=Singapore_number$case[732:1044]*6
Singapore_number$case[1045:1096]=Singapore_number$case[1045:1096]*3

crcase1=Singapore_number$case[732:872]
label=c(1:141)

TempRain=Singapore_weather_all
temp=TempRain$temp[5304:6463]
rain=TempRain$rainfall[5304:6463]


#crcase=lowess(crcase, f = 0.05)
#crcase1=lowess(crcase1, f = 0.05)

#crcase=crcase$y
#crcase1=crcase1$y
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





temp=temp_1[60,]/60
rain=rain_1[21,]/21
fr=data.frame(crcase,crcase1,temp,rain)
fr= fr[order(fr[,1]),]

################## make group
i1=45
fr2=fr[1:i1,]
fr3=fr[(i1+1):127,]
fr4=fr[(128):141,]
######### remove outlier 
fr2=fr2[(fr2$temp<28.6),]


temp=temp_1[120,]/120
temphat=temp
h1=crcase
fit=gam(h1~s(temp))
data=cbind.data.frame(temphat,h1)
#################### effect of temperature on GAI
p <- ggplot(data,  aes(temphat,h1)) +
  geom_smooth(size=1.3,span = 3,level=0.95)
#p=p+scale_x_continuous(breaks=seq(26.5,30.5,1))
p <- p +labs(x = "Average daily temperature (°C)", y = "Weekly GAI index")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))

pz1=p


################ fg6, effect of temperature on dengue for each group
temphat=fr2$temp
h1=fr2$crcase1 
data=cbind.data.frame(temphat,h1)
p <- ggplot(data,  aes(temphat,h1)) +ylim(0,650*8)+
  geom_smooth(size=1.3,span = 0.7,level=0.95)
p=p+scale_x_continuous(breaks=seq(26.5,29.5,1))
p <- p +labs(x = "Average daily temperature (°C)", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("Low GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p11=p

temphat=fr3$temp
h1=fr3$crcase1 
data=cbind.data.frame(temphat,h1)
p <- ggplot(data,  aes(temphat,h1)) +ylim(0,650*8)+
  geom_smooth(size=1.3,span = 2,level=0.95)
p=p+scale_x_continuous(breaks=seq(26.5,30,1))
p <- p +labs(x = "Average daily temperature (°C)", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("Medium GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p12=p

temphat=fr4$temp
h1=fr4$crcase1 
data=cbind.data.frame(temphat,h1)
p <- ggplot(data,  aes(temphat,h1)) +ylim(0,650*8)+
  geom_smooth(size=1.3,span = 3,level=0.95)
p=p+scale_x_continuous(breaks=seq(26.5,29.5,1))
p <- p +labs(x = "Average daily temperature (°C)", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("High GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p13=p

################### Check the p-values of spline regression,crcase is gai index and crcase1 is dengue number.
fit1=gam(fr2$crcase1~s(fr2$temp))
fit2=gam(fr3$crcase1~s(fr3$temp))
fit3=gam(fr4$crcase1~s(fr4$temp))

fit5=gam(fr2$crcase1~s(fr2$rain))
fit6=gam(fr3$crcase1~s(fr3$rain))
fit7=gam(fr4$crcase1~s(fr4$rain))

fit8=gam(fr2$crcase1~s(fr2$crcase))
fit9=gam(fr3$crcase1~s(fr3$crcase))
fit10=gam(fr4$crcase1~s(fr4$crcase))
#################### The plots in SI
############################################# Effect of gai on Dengue
fr=rbind(fr2,fr3,fr4)

z=fr$crcase1
d=fr$crcase
data=cbind.data.frame(d,z)
p <- ggplot(data,  aes(d,z)) +geom_smooth(size=1.3,span = 1.2,level=0.95)+ylim(0,5000)
p <- p +labs(x = "GAI index", y = "Weekly dengue cases")
p20=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))


temphat=fr2$crcase
h1=fr2$crcase1 
data=cbind.data.frame(temphat,h1)


p <- ggplot(data,  aes(temphat,h1)) +
  geom_smooth(size=1.3,span = 2,level=0.95)
p=p+ylim(0,5000)
p <- p +labs(x = "GAI index", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("Low GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p21=p

temphat=fr3$crcase
h1=fr3$crcase1 
data=cbind.data.frame(temphat,h1)


p <- ggplot(data,  aes(temphat,h1)) +
  geom_smooth(size=1.3,span = 2,level=0.95)
p=p+ylim(0,5000)
p <- p +labs(x = "GAI index", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("Medium GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p22=p



temphat=fr4$crcase
h1=fr4$crcase1 
data=cbind.data.frame(temphat,h1)
p <- ggplot(data,  aes(temphat,h1)) +geom_smooth(size=1.3,span = 2,level=0.95)
p=p+ylim(0,5000)
p <- p +labs(x = "GAI index", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("High GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p23=p

############################################## rainfall
z=fr$crcase
d=fr$rain

data=cbind.data.frame(d,z)
p <- ggplot(data,  aes(d,z)) +geom_smooth(size=1.3,span = 1,level=0.95)+ylim(0,0.3)
p <- p +labs(x = "Average daily Precipitation (mm)", y = "Weekly GAI index")
p1=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))



temphat=fr2$rain
h1=fr2$crcase1 
fit=gam(h1~s(temphat))

data=cbind.data.frame(temphat,h1)
p <- ggplot(data,  aes(temphat,h1)) +ylim(0,5000)+
  geom_smooth(size=1.3,span =1,level=0.95)
p <- p +labs(x = "Average daily Precipitation (mm)", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("Low GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p11=p



temphat=fr3$rain
h1=fr3$crcase1 
fit=gam(h1~s(temphat))

data=cbind.data.frame(temphat,h1)
p <- ggplot(data,  aes(temphat,h1)) +ylim(0,5000)+
  geom_smooth(size=1.3,span =1,level=0.95)
p <- p +labs(x = "Average daily Precipitation (mm)", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("Medium GAI group ")
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p12=p



temphat=fr4$rain
h1=fr4$crcase1 
fit=gam(h1~s(temphat))
data=cbind.data.frame(temphat,h1)
p <- ggplot(data,  aes(temphat,h1)) +ylim(0,5000)+
  geom_smooth(size=1.3,span = 1,level=0.95)

p <- p +labs(x = "Average daily Precipitation (mm)", y = "Weekly dengue cases")
p=p +theme_bw() +
  theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"),axis.title.x = element_text( size=20, face="bold"),
        axis.text.x = element_text(color = "grey20", size =15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),
        axis.title.y = element_text( size=20,face="bold", vjust = 1.5))
p=p=p+ggtitle("High GAI group ")
p=p+scale_x_continuous(breaks=seq(0.0,10.0,2.5))
p=p+theme(plot.title = element_text(margin = margin(t = 10, b = -25)))
p13=p





############## fg5, wavelent graph for dengue and GAI
library(WaveletComp)
x=sg_mos$number
y=crcase1
my.data <- data.frame(x = x, y = y)
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           dt = 1, dj = 1/100,upperPeriod = 32,
                           loess.span = 0,
                           make.pval = TRUE, n.sim = 10)

par(new = TRUE,cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2)
wc.image(my.wc, n.levels = 250,timelab = "Time",
         legend.params = list(width=1.2, shrink = 0.9, mar = 5.1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2,
                              n.ticks =6, 
                              label.digits = 1, label.format = "f", lab = "cross-wavelet power levels"),
         spec.time.axis = list(at = NULL, labels = TRUE, 
                               las = 2, hadj = NA, padj = NA), periodlab = "period (weeks)")


my.wc3 <- analyze.coherency(my.data, my.pair = c("x","y"),
                            loess.span = 0, dt = 1, dj = 1/100,
                            window.type.t = 3, window.type.s = 3,
                            window.size.t = 5, window.size.s = 1,
                            make.pval = TRUE, n.sim = 10)

wc.image(my.wc3, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "wavelet coherence levels"),
         timelab = "")

wc.sel.phases(my.wc, sel.period = 26,
              only.sig = TRUE,
              which.sig = "wc",
              siglvl = 0.1,
              phaselim = c(-pi,+pi), ## default if legend.horiz = FALSE
              legend.coords = c(1,95), legend.horiz = FALSE,
              main = "", sub = "", timelab = "Time")
legend('left', legend=c("GAI", "Dengue", "Phase-difference"),
       col=c("red", "blue",'black'), lty=c(1,1,2), lwd=1.35,cex=1.03,y.intersp = 0.75,pt.cex = 2,bty='n')


