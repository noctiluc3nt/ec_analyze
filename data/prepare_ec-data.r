### preparation of the ec data

#unzip .ghg data
#path = "./data/licor/"
#files=list.files(path,pattern=".ghg",full.name=T)
#for (i in 1:length(files)) unzip(files[i])

date=c("2022-06-05","2022-06-06","2022-06-07")

dir_out="./prepared_raw_data/"
dir_in = paste0("./data/licor/")

do_timeformatting=function(times) {
	times_formatted=times
	for (i in 1:length(times)) {
		time=times[i]
		times_formatted[i]=paste0(substr(time,1,8),".",substr(time,10,nchar(time)))
	}
	return(times_formatted)
}

for (j in 1:length(date)) {
	files = list.files(dir_in,pattern=date[j],recursive=T,full.name=T)
	filenames = list.files(dir_in,pattern=date[j],recursive=T)
	for (i in 1:length(files)) {
		print(i)
		filename=as.character(filenames[i])
		dat=read.table(files[i],sep="\t",skip=7,header=T)
		time=dat$Time
		time=do_timeformatting(time)
		dat_new=data.frame("time"=paste(dat$Date,time),"u_m/s"=dat$Aux.1...U..m.s.,
			"v_m/s"=dat$Aux.2...V..m.s.,
			"w_m/s"=dat$Aux.3...W..m.s.,"T_degC"=dat$Aux.4...SOS..m.s.,
			"CO2_ppm"=dat$CO2.dry.umol.mol.,"H2O_ppt"=dat$H2O.dry.mmol.mol.)
		dat_new$T_degC=(dat_new$T_degC)^2/1.4/287.04-273.15
		write.csv(file=paste0(dir_out,substr(filename,1,nchar(filename)-5),".csv"), x=dat_new,quote=F,row.names=F)
	}
}


#tar everything
files_out=list.files(dir_out,full.names=T)
tar(tarfile=paste0(dir_out,"prepared_raw_data.tar"),files=dir_out)

#remove the rest
file.remove(files_out)
