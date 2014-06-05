DDF <- function(DATE_TIME,temp,threshold=0) {
library ('zoo') #NEED ZOO FUNCTIONS

#Default of threshold is 0*C
  
temp[temp<threshold] <- NA #DELETE DATA <0*C
#take absolute value of temperatures
temp <- abs(temp*(1/24)) #FOR HOURLY TIME STAMPS

DDF <- data.frame(year1=NA,year2=NA,DDF=NA)
r=1
for (i in min(year(DATE_TIME)):max(year(DATE_TIME))) {
  DDF[r,1] = i
  DDF[r,2] = i+1
  firstyear <- temp[year(DATE_TIME)==i & month(DATE_TIME)>6] #SECOND HALF OF 1st YEAR
  secondyear <- temp[year(DATE_TIME)==(i+1) & month(DATE_TIME)<6] #FIRST HALF OF 2nd YEAR
  DDF[r,3] = sum(firstyear,na.rm=TRUE) + sum(secondyear,na.rm=TRUE) #ADD DFFs TOGETHER
  r = r+1
}

return (DDF)
}
