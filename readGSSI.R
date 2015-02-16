#this script was translated from readgssi.m, a matlab script 
#found available online
#R code by H. Dugan June 7, 2014
#Need R version R 3.0.0 or higher

#The function of this script is to obatin the header and data 
#form a .DZT file. This is the output of radar files associated
#with GSSI

#Input: name: file name. ex) "GPR01.DZT"
#Output: list containing all header info
#object name $gpr contains the data

readgssi <- function(name) {

  fid = file(name,'rb') #connect to file
  
  rh = list()
  
  rh[['tag']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['data']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['nsamp']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['bits']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['zero']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['sps']] = readBin(fid,double(),size=4)
  rh[['spm']] = readBin(fid,double(),size=4)
  rh[['mpm']] = readBin(fid,double(),size=4)
  rh[['position']] = readBin(fid,double(),size=4)
  rh[['range']] = readBin(fid,double(),size=4)
  rh[['npass']] = readBin(fid,integer(),size=2,signed="FALSE")
  
  createTime = readBin(fid,integer(),size=4,n=1)
  
  #bitint function supplied by Matthew Lundberg
  #http://stackoverflow.com/questions/24101150/read-integers-from-binary-8-bits-in-r
  bitint <- function(x, bitlens) {
    result <- integer(length(bitlens))
    for (i in seq_along(bitlens)) {
      result[i] <- bitwAnd(x, (2^bitlens[i])-1)
      x <- bitwShiftR(x, bitlens[i])
    }
    return(result)
  }
  
  bitlens <- c(5,6,5,5,4,7) #bit length of date stamps
  create = c(sapply(createTime, function(i) bitint(i, bitlens)))
  
  rh[['fileCreate']] = strptime(paste(create[3],":",create[2],":",create[1]," ",
          create[5],"-",create[4],"-",create[6],sep=""),"%H:%M:%S %m-%d-%y",tz="UTC")
  
    modTime = readBin(fid,integer(),size=4,n=1) 
  mod = c(sapply(modTime, function(i) bitint(i, bitlens)))
  
  rh[['fileMod']] = strptime(paste(mod[3],":",mod[2],":",mod[1]," ",
          mod[5],"-",mod[4],"-",mod[6],sep=""),"%H:%M:%S %m-%d-%y",tz="UTC")
    
  rh[['rgain']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['nrgain']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['text']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['ntext']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['proc']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['nproc']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['nchan']] = readBin(fid,integer(),size=2,signed="FALSE")
  
  rh[['epsr']] = readBin(fid,double(),size=4)
  rh[['top']] = readBin(fid,double(),size=4)
  rh[['depth']] = readBin(fid,double(),size=4)
  
  reserved = readBin(fid,integer(),n=31,size=1)
  rh[['dtype']] = readBin(fid,integer(),n=1,size=1)
  rh[['antname']] = readBin(fid,integer(),n=14,size=1)
  rh[['chanmask']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['name']] = readBin(fid,integer(),n=12,size=1)
  rh[['chksum']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['Gain']] = readBin(fid,integer(),size=2,signed="FALSE")
  rh[['Gainpoints']] = readBin(fid,double(),n=rh$Gain,size=4)
  rh[['comments']] = readBin(fid,character(),n=rh$ntext)
  rh[['proccessing']] = readBin(fid,raw(),n=rh$nproc)
  rh[['proccessing']] = as.integer(paste("0x",rh$proccessing,sep=""))
  
  seek(fid,where=0)
  temp = length(readLines(fid)) * rh$nsamp / rh$bits #for length 
  #of vector in d
  seek(fid,where=1024)
  
  options(warn=-1) #ignore warning that final line of file is incomplete
  d = readBin(fid,integer(),size=2,n=temp,signed="FALSE")
  close(fid) #close connection to file
  
  d = matrix(d,nrow = rh$nsamp)
  d[1,] = d[3,]
  d[2,] = d[3,]
  d = d+rh$zero
  
  rh[['gpr']] = d
  
  return (rh)
}
