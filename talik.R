### Returns temperature profile beneath lake ###
# author: Hilary Dugan, Feb 16th, 2015

# radius = radius of the lake (m)
# maat = mean annual air temperature (degC)
# mabt = mean annual bottom temperature (degC)
# geoZ = geothermal gradient with depth (m/degC)
# maxdepth = maximum depth of investigation (m) 

talik <- function(radius, maat = -19, mabt = 25, geoZ = 50, maxdepth = 2000) {

  depths = seq(0,maxdepth,by=maxdepth/100)

  talikT <- function(z) { #temperature function
    temp = maat + (z/geoZ) + (mabt-maat)*(1-(z/(sqrt((z^2)+(radius^2)))))
    return(temp)
  }  

  temps = sapply(depths,talikT)
  talik = data.frame(depth=depths,temp=temps)

  return(talik)
}

plotTalik <- function(talik) {
  par(mar = c(3,3,0.2,1),mgp=c(1.5,0.8,0),tck=-0.02,cex=0.8,
      cex.axis=0.8,cex.lab=0.8)
  plot(talik$temp,talik$depth,ylim=rev(range(talik$depth)),
       pch=16, type='o',
       ylab='depth (m)',xlab='temperature (degC) ')
}

