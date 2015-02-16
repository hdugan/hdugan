plotDZT <- function(file,heading='ELB_____') {
name = paste(heading,file,".DZT",sep="")
a=readgssi(name)

data = a$gpr
data = t(data)
filtdata=data-32768L #keep as integer
depths = seq(from=a$top,to=a$depth,length.out=a$nsamp)

#set filename, size and resolution of output
png(paste(heading,file,'.png', sep=''),
    width=10, height=4, units="in", res=200)

#### PLOT RADAR IMAGE ###
par(mar = c(3,3,0.2,1),mgp=c(1,0.2,0),tck=-0.02,cex=0.8,
    cex.axis=0.8,cex.lab=0.8)
image(x=seq(1:nrow(data)+1),y=depths,z=filtdata[1:nrow(data),],
      col = colRamp(20),ylim=rev(range(depths)),
      ylab='depth (m)', xlab='distance')

dev.off() 

}