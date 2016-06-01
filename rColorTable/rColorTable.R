# R colors minus 100 shades of grey 
cols = colors()[c(2:151,362:657)]

# Duplicate colors
dupCols = list()
for (i in 1:4){
  dupCols[[i]] <- cols[grep(pattern = i,x = cols)]
}
dupNames = gsub(pattern = '1',replacement = '',x = dupCols[[1]])

# All Color Names 
allNames = a[!grepl('1',a) & !grepl('2',a) & !grepl('3',a) & !grepl('4',a)]
indx = match(dupNames,allNames)

# Output color table 
output = data.frame(col = c,c1=NA,c2=NA,c3=NA,c4=NA,stringsAsFactors = F)
for (i in 1:4){
  output[,i+1][indx] = dupCols[[i]]
}

# Set rectangles x start and end points 
x1 = c(0,3,3.5,4,4.5)
x2 = c(3,3.5,4,4.5,5)

# Color chart
pdf('~/Downloads/rColor.pdf')
par(mar=c(0,6,0,6))
plot(0, type="n", ylab="", xlab="",axes=FALSE, ylim=c(138,0), xlim=c(1,5))
axis(2,at = c(1:138),labels = c,las=1,cex.axis=0.35)
axis(4,at= indx,labels = b1names,cex.axis=0.35,las=1)
for (j in 1:(138-1)) {
  for (i in 1:5) {
    #k = j*5 + i
    if (!is.na(output[j,i])){
      rect(x1[i],j-0.5, x2[i],j+0.5, border="black", col=output[j,i])
    }
  }
}
dev.off()

# 2 page color chart 
pdf('~/Dropbox/Scripts/hdugan/rColorTable/rColorTable.pdf',height = 10)
par(mar=c(0,6,0,6))
# First page
plot(0, type="n", ylab="", xlab="",axes=FALSE, ylim=c(69,0), xlim=c(1,5))
axis(2,at = allNames(1:69),labels = allNames[1:69],las=1,cex.axis=0.6)
axis(4,at= indx[indx<70],labels = allNames[indx[indx<70]],cex.axis=0.6,las=1)
for (j in 1:69) {
  for (i in 1:5) {
    #k = j*5 + i
    if (!is.na(output[j,i])){
      rect(x1[i],j-0.5, x2[i],j+0.5, border="black", col=output[j,i])
    }
  }
}
# Second Page 
plot(0, type="n", ylab="", xlab="",axes=FALSE, ylim=c(138,70),xlim=c(1,5))
axis(2,at = allNames(70:138),labels = allNames[70:138],las=1,cex.axis=0.6)
axis(4,at= indx[indx>=70],labels = allNames[indx[indx>=70]],cex.axis=0.6,las=1)
for (j in 70:138) {
  for (i in 1:5) {
    #k = j*5 + i
    if (!is.na(output[j,i])){
      rect(x1[i],j-0.5, x2[i],j+0.5, border="black", col=output[j,i])
    }
  }
}
dev.off()
