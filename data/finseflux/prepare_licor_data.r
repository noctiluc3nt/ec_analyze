
dir_in="/home/lauracma/Documents/teaching/finse_excursion/2025/tower/finse_excursion2025/raw/licor"
dir_out="/home/lauracma/Documents/teaching/finse_excursion/2025/tower/finse_excursion2025/temp/"


files = list.files(dir_in,recursive=T,pattern="dat.xz",full.name=T)
filenames = list.files(dir_in,recursive=T,pattern="dat.xz")

for (i in 1:length(files)) {
	print(i)
	filename=as.character(filenames[i])
	#dat=read.table(files[i],sep="\t",skip=7,header=T)
	dat=read.table(files[i],sep=",",skip=4,fill=T)
	head=names(read.table(files[i],skip=1,sep=",",header=T))
	colnames(dat)=head
	dat_new=data.frame("time"=dat$TIMESTAMP,"u_m/s"=dat$Ux,
		"v_m/s"=dat$Uy,
		"w_m/s"=dat$Uz,"T_degC"=dat$SOS,
		"CO2_ppm"=dat$CO2_dry,"H2O_ppt"=dat$H2O_dry)
	dat_new$T_degC=(dat_new$T_degC)^2/1.4/287.04-273.15
	write.csv(file=paste0(dir_out,substr(filename,1,nchar(filename)-7),".csv"), 			x=dat_new,quote=F,row.names=F)
}


#tar everything
files_out=list.files(dir_out,full.names=T)
tar(tarfile=paste0(dir_out,"raw_data_20250423_prelim.tar"),files=files_out)

#remove the rest
file.remove(files_out)

#########################################################################################
### calc composite spectra

library(Reddy)

nf=length(files)
ux_spectrum=array(NA,dim=c(nf,250))
uana_spectrum=array(NA,dim=c(nf,250))
uy_spectrum=array(NA,dim=c(nf,250))
vana_spectrum=array(NA,dim=c(nf,250))
uz_spectrum=array(NA,dim=c(nf,250))
wana_spectrum=array(NA,dim=c(nf,250))
sos_spectrum=array(NA,dim=c(nf,250))
sosana_spectrum=array(NA,dim=c(nf,250))

w_mean=array(NA,dim=c(nf))
temp_mean=array(NA,dim=c(nf))
u_mean=array(NA,dim=c(nf))
uana_mean=array(NA,dim=c(nf))
cov_wT_mean=array(NA,dim=c(nf))
wana_mean=array(NA,dim=c(nf))
tempana_mean=array(NA,dim=c(nf))
cov_wT_ana_mean=array(NA,dim=c(nf))

for (i in 1:nf) {
	filename=as.character(filenames[i])
	#dat=read.table(files[i],sep="\t",skip=7,header=T)
	dat=read.table(files[i],sep=",",skip=4,fill=T)
	head=names(read.table(files[i],skip=1,sep=",",header=T))
	colnames(dat)=head
	dat$Ts=sos2Ts(dat$SOS)
	dat$Ts_ana=sos2Ts(dat$SOS_ana)
	###
	s=calc_spectrum1D(dat$Ux,nbins=500,plot=F)
	ux_spectrum[i,]=s[251:500,2]
	s=calc_spectrum1D(dat$U_ana,nbins=500,plot=F)
	uana_spectrum[i,]=s[251:500,2]
	s=calc_spectrum1D(dat$Uy,nbins=500,plot=F)
	uy_spectrum[i,]=s[251:500,2]
	s=calc_spectrum1D(dat$V_ana,nbins=500,plot=F)
	vana_spectrum[i,]=s[251:500,2]
	s=calc_spectrum1D(dat$Uz,nbins=500,plot=F)
	uz_spectrum[i,]=s[251:500,2]
	s=calc_spectrum1D(dat$W_ana,nbins=500,plot=F)
	wana_spectrum[i,]=s[251:500,2]
	s=calc_spectrum1D(dat$SOS,nbins=500,plot=F)
	sos_spectrum[i,]=s[251:500,2]
	s=calc_spectrum1D(dat$SOS_ana,nbins=500,plot=F)
	sosana_spectrum[i,]=s[251:500,2]
	w_mean[i]=mean(dat$Uz,na.rm=T)
	wana_mean[i]=mean(dat$W_ana,na.rm=T)
	u_mean[i]=mean(dat$Ux,na.rm=T)
	uana_mean[i]=mean(dat$U_ana,na.rm=T)
	temp_mean[i]=mean(dat$Ts,na.rm=T)
	tempana_mean[i]=mean(dat$Ts_ana,na.rm=T)
	cov_wT_mean[i]=cov(dat$Uz,dat$Ts,use="pairwise.complete.obs")
	cov_wT_ana_mean[i]=cov(dat$W_ana,dat$Ts_ana,use="pairwise.complete.obs")
}
freq=s[251:500,1]

par(mfrow=c(1,2))
plot(freq,apply(wx_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy")
#points(freq,apply(wx_spectrum,2,quantile,0.25,na.rm=T),type="l")
#points(freq,apply(wx_spectrum,2,quantile,0.75,na.rm=T),type="l")
for (i in 1:nf) {
	points(freq,wx_spectrum[i,],type="l",col=rgb(0,0,0,0.02))
}

plot(freq,apply(wana_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy")
for (i in 1:nf) {
	points(freq,wana_spectrum[i,],type="l",col=rgb(0,0,0,0.02))
}

par(mfrow=c(2,2))

#spectra
plot(freq,apply(uana_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy")
points(freq,apply(ux_spectrum,2,mean,na.rm=T),lwd=2,type="l",col=2)

plot(freq,apply(vana_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy")
points(freq,apply(uy_spectrum,2,mean,na.rm=T),lwd=2,type="l",col=2)

plot(freq,apply(wana_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy")
points(freq,apply(uz_spectrum,2,mean,na.rm=T),lwd=2,type="l",col=2)

plot(freq,apply(sosana_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy")
points(freq,apply(sos_spectrum,2,mean,na.rm=T),lwd=2,type="l",col=2)

#co spectra
plot(freq,apply(wana_spectrum*sosana_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy")
points(freq,apply(uz_spectrum*sos_spectrum,2,mean,na.rm=T),lwd=2,type="l",col=2)



plot(freq[200:250],apply(uana_spectrum,2,median,na.rm=T)[200:250],lwd=2,type="l",log="xy")
points(freq[200:250],apply(ux_spectrum,2,median,na.rm=T)[200:250],lwd=2,type="l",col=2)
points(freq,apply(uana_spectrum,2,quantile,0.25,na.rm=T),type="l")
points(freq,apply(uana_spectrum,2,quantile,0.75,na.rm=T),type="l")
points(freq,apply(ux_spectrum,2,quantile,0.25,na.rm=T),type="l",col=2)
points(freq,apply(ux_spectrum,2,quantile,0.75,na.rm=T),type="l",col=2)

##################
#HFdata

#record offset
keep=!is.na(dat$SOS)&!is.na(dat$SOS_ana)
ccf=ccf(dat$SOS[keep],dat$SOS_ana[keep])
ccf$lag[which(ccf$acf==max(ccf$acf))]
xdig=1:(12000-3)
xana=4:12000
ccf=ccf(dat$SOS[keep][xdig],dat$SOS_ana[keep][xana])
ccf$lag[which(ccf$acf==max(ccf$acf))]


par(mfrow=c(2,2))
plot(dat$SOS_ana[xana],type="l",xlab="index",ylab="SOS [m/s]",main="SOS in HFdata",col=rgb(0,0,0,0.5))
points(dat$SOS[xdig],type="l",col=rgb(0,0,0.8,0.5))
legend("bottomleft",legend=c("analog","digital"),col=c(1,"blue3"),lty=1)

#spectrum
plot(freq,apply(wana_spectrum,2,mean,na.rm=T),lwd=2,type="l",log="xy",main="spectrum w", xlab="",ylab="")
points(freq,apply(uz_spectrum,2,mean,na.rm=T),lwd=2,type="l",col="blue3")
legend("bottomleft",legend=c("analog","digital"),col=c(1,"blue3"),lty=1,lwd=2)

par(mfrow=c(1,2))
#scatterplots
plot(dat$SOS_ana[xana],dat$SOS[xdig],pch=20,xlab="SOS analog [m/s]",ylab="SOS digital [m/s]",main="SOS in HFdata",col=rgb(0,0,0,0.1))
abline(0,1,col="red3",lty=2,lwd=2)
fit=lm(dat$SOS[xdig] ~ dat$SOS_ana[xana])
abline(fit,col="blue3")
text(332.1,332.7,TeX("linreg: 0.0.06845 + 1.00002 x, $R^2$=0.9983"),col="blue3")

plot(dat$W_ana[xana],dat$Uz[xdig],pch=20,xlab="w analog [m/s]",ylab="w digital [m/s]",main="W in HFdata",col=rgb(0,0,0,0.1))
abline(0,1,col="red3",lty=2,lwd=2)
fit=lm(dat$Uz[xdig] ~ dat$W_ana[xana])
abline(fit,col="blue3")
text(-0.1,0.4,TeX("linreg: 0.02341 + 1.51375 x, $R^2$=0.9983"),col="blue3")


plot(dat$U_ana[xana],dat$Ux[xdig],pch=20,xlab="SOS analog [m/s]",ylab="SOS digital [m/s]",main="SOS in HFdata",col=rgb(0,0,0,0.1))
abline(0,1,col="red3",lty=2,lwd=2)
fit=lm(dat$Ux[xdig] ~ dat$U_ana[xana])
abline(fit,col="blue3")
text(332.1,332.7,TeX("linreg: 0.0.06845 + 1.00002 x, $R^2$=0.9983"),col="blue3")


#in mean
par(mfrow=c(2,2))

plot(wana_mean,type="l",xlim=c(0,104),xlab="index",ylab="w [m/s]",main="w-wind (10 min average)",ylim=c(-0.12,0.2))
points(w_mean,col="blue3",type="l")
legend("bottomright",legend=c("analog","digital"),col=c(1,"blue3"),lty=1,lwd=2)
abline(h=0,lty=2)

plot(uana_mean,type="l",xlim=c(0,104),xlab="index",ylab="Ts [K]",main="u-wind (10 min average)")
points(u_mean,col="blue3",type="l")
legend("bottomright",legend=c("analog","digital"),col=c(1,"blue3"),lty=1,lwd=2)
abline(h=0,lty=2)


plot(tempana_mean,type="l",xlim=c(0,104),xlab="index",ylab="Ts [K]",main="sonic temperature (10 min average)")
points(temp_mean,col="blue3",type="l")
legend("bottomright",legend=c("analog","digital"),col=c(1,"blue3"),lty=1,lwd=2)

plot(cov_wT_ana_mean,type="l",xlim=c(0,104),xlab="index",ylab="Ts [K]",main="cov(w,Ts) (10 min average)",ylim=c(-0.03,0.035))
points(cov_wT_mean,col="blue3",type="l")
abline(h=0,lty=2)
legend("bottomright",legend=c("analog","digital"),col=c(1,"blue3"),lty=1,lwd=2)


#das analoge signal ist niedriger als das digitale und diskret?, w hat diese systematische abweichung von 1:1 und dadurch hat cov(w,Ts) ne höhere amplitude im digitalen 


#record delay: 3
#from csat manual for analog output

#data gaps: e.g. in DB 18.05.2025 13:30-13:42 is missing, but the data exist on the VM -- it seems that the data copied to the DB is from the truncated files



###################################
### test for completeness

files=list.files("./",full.name=T)

#filename=as.character(filenames[i])
#dat=read.table(files[i],sep="\t",skip=7,header=T)
dat=read.table(files[i],sep=",",skip=4,fill=T)
head=names(read.table(files[i],skip=1,sep=",",header=T))
colnames(dat)=head


#list of gaps

18.04.: some weird measuremetns

25.04.
	08:10 - ... -> gap series starts

...

29.04.
	07:00-11:39 ... dann nur analog

... 
06.05.:
	09:10

07.05.:
	00:40-01:03

	01:10-01:38

	05:50-06:24*


	14:00-14:08
	14:10-14:18
	14:20-14:28
	14:30-14:47
	14:50-15:09*
	15:10-15:17
	15:20-15:24
	15:30-15:39
	15:40-16:07*
	16:10-16:18
	16:20-16:37*
	16:50-16:57
	17:00-17:18*
	17:20-17:28
	17:30-17:33
	19:40-19:42
	20:00-20:07
	20:10-20:18
	20:20-20:30
	20:40-20:42
	20:50-20:57
	21:00-21:02
	21:10-21:37*
	22:00-22:38*
	23:20-23:24

08.05.:
	00:10-00:17
	00:30-00:47*
	01:20-01-27
	02:10-02:27*
	02:30-02:47*
	02:50-02:52
	03:40-03:52*
	05:20-05:23
	05:50-05:53
	06:00-06:08
	06:10-06:12
	07:00-07:08
	07:10-07:18
	07:30-07:32
	09:50-09:58
	10:00-10:03
	13:30-13:42
	15:40-15:43
	18:00-18:02
	18:10-18:17
	18:19-18:27
	20:40-20:42
	23:00-23:03
	23:30-23:32
	23:50-23:52

09.05.:
	00:10-00:12
	00:20-00:27
	03:20-03:22
	03:30-03:32
	12:00-12:02
	17:50-17:52
	18:00-18:02
	18:10-18:18
	22:30-22:32

	... 11.05. 08:10
	... 14.05


that's a list of percentange of available data per day


from 25.04. to 07.05. is a lot of missing data, e.g. 07.05.2025 05:50-06:24 (maybe we could check if this is still on logger?)


sometimes, there is analog only in DB

and sometimes digital only


and sometimes some spikes (*today)