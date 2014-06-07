#this script was translated from readgssi.m, a matlab script 
#found available online
#R code by H. Dugan June 7, 2014

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
  
  create = list()
  create[['sec2']] = readBin(fid,integer(),n=4,size=2)
  create[['min']]
  create[['hour']]
  create[['day']]
  create[['month']]
  create[['year']]
  
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
  close(fid)
  
  d = matrix(d,nrow = rh$nsamp)
  d[1,] = d[3,]
  d[2,] = d[3,]
  d = d+rh$zero
  
  rh[['gpr']] = d
  
  return (rh)
}
