dir_in=paste0("./biomet/")
dir_out=paste0("./")

avgtime=30
ncum=180

aggregate = function(vec,nr=18,type="mean") {
	n=length(vec)
	new=numeric(as.integer(n/nr))
	for (i in 1:n) {
		if (!is.na(vec[i])) {
			new[(i-1)%/%nr+1] = new[(i-1)%/%nr+1] + vec[i]
		} else {
			nr=nr-1
		}
	}
	if (type=="mean") {
		new=new/nr
		return(new)
	} else {
		return(new)
	}
}


swin_agg=c()
swout_agg=c()
lwin_agg=c()
lwout_agg=c()
soil_agg=c()
snow_agg=c()
ta_agg=c()
rh_agg=c()
time=c()

files=list.files(dir_in,full.name=T)
nf=length(files)

for ( i in 1:nf ) {
	dat=read.table(files[i],sep=",",skip=4,fill=T)
	head=names(read.table(files[i],skip=1,sep=",",header=T))
	colnames(dat)=head
	dat=data.frame("TIMESTAMP"=as.character(dat$TIMESTAMP),
		"SWIN_6_10_1_1_1"=dat$SWIN_6_10_1_1_1,
		"SWOUT_6_11_1_1_1"= dat$SWOUT_6_11_1_1_1,
		"LWIN_6_14_1_1_1"=dat$LWIN_6_14_1_1_1,
		"LWOUT_6_15_1_1_1"=dat$LWOUT_6_15_1_1_1,
		"SHF_6_37_1_1_1"=dat$SHF_6_37_1_1_1,
		"RH_19_3_1_1_1"=dat$RH_19_3_1_1_1,
		"TA_2_1_1_1_1"=dat$TA_2_1_1_1_1)
	swin_agg=c(swin_agg,aggregate(as.numeric(dat$SWIN_6_10_1_1_1),ncum))
	swout_agg=c(swout_agg,aggregate(as.numeric(dat$SWOUT_6_11_1_1_1),ncum))
	lwin_agg=c(lwin_agg,aggregate(as.numeric(dat$LWIN_6_14_1_1_1),ncum))
	lwout_agg=c(lwout_agg,aggregate(as.numeric(dat$LWOUT_6_15_1_1_1),ncum))
	soil_agg=c(soil_agg,aggregate(as.numeric(dat$SHF_6_37_1_1_1),ncum))
	ta_agg=c(ta_agg,aggregate(as.numeric(dat$TA_2_1_1_1_1),ncum))
	rh_agg=c(rh_agg,aggregate(as.numeric(dat$RH_19_3_1_1_1),ncum))
	#snow_agg=c(snow_agg,aggregate(as.numeric(dat$METNOS_99_99_1_1_1),ncum))
	time=c(time,as.character(dat$TIMESTAMP[1]))
}

dat=data.frame("TIMESTAMP"=as.character(time),
			"SWIN_6_10_1_1_1"=swin_agg,
			"SWOUT_6_11_1_1_1"= swout_agg,
			"LWIN_6_14_1_1_1"=lwin_agg,
			"LWOUT_6_15_1_1_1"=lwout_agg,
			"SHF_6_37_1_1_1"=soil_agg)
			#"TA_2_1_1_1_1"=ta_agg,
			#"RH_19_3_1_1_1"=rh_agg)

write.csv(x=dat,file=paste0(dir_out,"biomet_data.csv"),quote=F,row.names=F)


