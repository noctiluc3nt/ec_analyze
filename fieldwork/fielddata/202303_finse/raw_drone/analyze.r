
dat=read.csv("DATA0016.TXT",header=F)


mpcT=dat[dat[,3]=="mpcT",4]
scdT=dat[dat[,3]=="scdT",4]
scdH=dat[dat[,3]=="scdH",4]
bmeP=dat[dat[,3]=="bmeP",4]
bmeT=dat[dat[,3]=="bmeT",4]
bmeH=dat[dat[,3]=="bmeH",4]
Ts=dat[dat[,3]=="mlxObjT",4]

pottemp=(bmeT+273.15)*(100000/bmeP)^(287/1005)

#flight profile
plot(bmeP/100,ylim=c(87400,86500)/100,type="l",ylab="pressure [hPa]",main="Flight Profile")

#plot(pottemp)

pal<<-colorRampPalette(c("blue4","blue2","yellow","orange","red"), space = "Lab")

#flight profile
plot(bmeP/100,ylim=c(87400,86500)/100,type="l",ylab="pressure [hPa]",main="Flight Profile")


par(mfrow=c(2,2))
off=1100
n=length(pottemp)
plot(bmeT[off:n],type="l",col=2,lwd=2,main="temperature",ylab="",xlab="")
par(new=T)
plot(bmeP[off:n]/100,ylim=c(87400,86500)/100,type="l",ylab="",main="",xlab="",xaxt="n",yaxt="n")


off=1100
n=length(pottemp)
plot(pottemp[off:n],type="l",col=2,lwd=2,main="potential temperature",ylab="",xlab="")
par(new=T)
plot(bmeP[off:n]/100,ylim=c(87400,86500)/100,type="l",ylab="",main="",xlab="",xaxt="n",yaxt="n")


off=1100
n=length(pottemp)
plot(bmeH[off:n],type="l",col=2,lwd=2,main="relative humidity",ylab="",xlab="")
par(new=T)
plot(bmeP[off:n]/100,ylim=c(87400,86500)/100,type="l",ylab="",main="",xlab="",xaxt="n",yaxt="n")

#Ts / flight profile
plot(Ts,type="l",col=2,lwd=2,main="surface temperature",ylab="",xlab="")
par(new=T)
plot(bmeP/100,ylim=c(87400,86500)/100,type="l",ylab="",main="",xlab="",xaxt="n",yaxt="n")

