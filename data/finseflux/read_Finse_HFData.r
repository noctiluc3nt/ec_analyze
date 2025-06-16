library(Reddy)

#for clickhouse db
#library(RClickhouse)
#con <- DBI::dbConnect(RClickhouse::clickhouse(), host="wsn.latice.eu") #create connection

read_file=function(file,write=FALSE) {
	dat=read.table(file,sep=",",skip=4,fill=T)
	head=names(read.table(file,skip=1,sep=",",header=T))
	colnames(dat)=head
	dat_new=data.frame("time"=dat$TIMESTAMP,"u_m/s"=dat$Ux,
		"v_m/s"=dat$Uy,
		"w_m/s"=dat$Uz,"T_degC"=dat$SOS,
		"CO2_ppm"=dat$CO2_dry,"H2O_ppt"=dat$H2O_dry)
	dat_new$T_degC=(dat_new$T_degC)^2/1.4/287.04-273.15
    if (write ==    TRUE) {
	    write.csv(file=paste0(dir_out,substr(filename,1,nchar(filename)-7),".csv"), 			x=dat_new,quote=F,row.names=F)
    }
    return(dat_new)
}

#################################################################

#file=list.files(".",pattern="dat.xz",full.name=TRUE)
dat=read_file(file)

#--- high-resolution ----
#double rotation
wind_rotated=rotate_double(dat$u_m.s,dat$v_m.s,dat$w_m.s)
dat$u=wind_rotated$u
dat$v=wind_rotated$v
dat$w=wind_rotated$w

#unit conversions
dat$h2o=ppt2rho(dat$H2O_ppt,gas="H2O")
dat$co2=ppt2rho(dat$CO2_ppm/1000,gas="CO2")

#quadrant analysis
#par(bg="gray40")
#par(col="white")
#par(col.lab="white",col.axis="white",col.main="white")
par(mfrow=c(1,4))
calc_quadrant_analysis(dat$T_degC,dat$w,xlim=c(-4,4),ylim=c(-4,4),xlab="T`",ylab="w`",main="Quadrant Analysis: (T, w)")
calc_quadrant_analysis(dat$h2o,dat$w,xlim=c(-4,4),ylim=c(-4,4),xlab="q`",ylab="w`",main="Quadrant Analysis: (H2O, w)")
calc_quadrant_analysis(dat$co2,dat$w,xlim=c(-4,4),ylim=c(-4,4),xlab="c`",ylab="w`",main="Quadrant Analysis: (CO2, w)")
calc_quadrant_analysis(dat$u,dat$w,xlim=c(-4,4),ylim=c(-4,4),xlab="u`",ylab="w`",main="Quadrant Analysis: (u, w)")

#mrd
par(mfrow=c(1,4))
#par(bg="gray40")
#par(col="white")
#par(col.lab="white",col.axis="white",col.main="white")
calc_mrd(dat$T_degC,dat$w,main="MRD: Cospectrum (T, w) ")
calc_mrd(dat$h2o,dat$w,main="MRD: Cospectrum (H2O, w) ")
calc_mrd(dat$co2,dat$w,main="MRD: Cospectrum (CO2, w) ")
calc_mrd(dat$u,dat$w,main="MRD: Cospectrum (u, w) ")

#--- post-processed ----
#post-processing
out=EC_processing_realtime(dat$u_m.s,dat$v_m.s,dat$w_m.s,dat$T_degC+273.15,dat$H2O_ppt,dat$CO2_ppm,time_averaging=10)
#footprint
ffp=calc_flux_footprint(4.4,out$ws,out$wd,300,out$L,out$v_sd,out$ustar,0.001)
lon1=7.527061462
lat1=60.59384155
ffp=locate_flux_footprint(ffp,lon1,lat1)


### ffp on map

par(mfrow=c(1,2))
colramp<<-colorRampPalette(c("forestgreen","yellow2","brown","white"))
x=1:1550
y=2950:5041
image.plot(coord$longitude[x,y],coord$latitude[x,y],t(elev)[x,y],zlim=c(1100,1500),xlim=c(7.51,7.545),ylim=c(60.585,60.602),col=colramp(40),
    xlab="lon",ylab="lat",main="Flux Footprint")
points(lon1,lat1,pch=3,lwd=3,cex=1.3)
text(lon1,lat1,"Finse1",pos=3)
points(ffp$xcontour_earth[[1]],ffp$ycontour_earth[[1]],type="l")
points(ffp$xcontour_earth[[2]],ffp$ycontour_earth[[2]],type="l")
points(ffp$xcontour_earth[[3]],ffp$ycontour_earth[[3]],type="l")

image.plot(coord$longitude[x,y],coord$latitude[x,y],t(elev)[x,y],zlim=c(1200,1260),xlim=c(7.521,7.53),ylim=c(60.591,60.597),col=colramp(40),
    xlab="lon",ylab="lat",main="Flux Footprint -- zoom in")
points(lon1,lat1,pch=3,lwd=3,cex=1.3)
legend("bottom","flux footprint contours: 50%, 60%, 70%, 80%, 90%",bty="n")
points(ffp$xcontour_earth[[1]],ffp$ycontour_earth[[1]],type="l")
points(ffp$xcontour_earth[[2]],ffp$ycontour_earth[[2]],type="l")
points(ffp$xcontour_earth[[3]],ffp$ycontour_earth[[3]],type="l")
points(ffp$xcontour_earth[[4]],ffp$ycontour_earth[[4]],type="l")
points(ffp$xcontour_earth[[5]],ffp$ycontour_earth[[5]],type="l")
