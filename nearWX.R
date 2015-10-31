# Function to find the closest NOAA weather stations in county given lat/long (*must be in USA)
# Coords is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
# NOAA Key must be requested from http://www.ncdc.noaa.gov/cdo-web/token
# noaakey: character string

nearWX <- function(coords,noaakey){
  county = latlong2county(coords)
  state = latlong2state(coords)
  
  # Get state fipscode
  # fps = unique(fipscodes[fipscodes$state == 'Wisconsin',]$fips_state) #should return only 1 value
  # Get county fipscode
  fps = unique(fipscodes[fipscodes$state == capwords(state,space=T) & 
                           fipscodes$county == capwords(county,space=T),]$fips) #should return only 1 value
  
  stations = ncdc_stations(datasetid='GHCND', locationid=paste("FIPS:",fps,sep=''),limit = 400, token=noaakey,
                           enddate = '1980-01-01',sortfield = "name", sortorder = "asc")
  
  stData = as.data.frame(stations$data)
  stData$maxdate = strptime(stData$maxdate,'%Y-%m-%d')
  stData$mindate = strptime(stData$mindate,'%Y-%m-%d')
  stData = stData[stData$maxdate > as.POSIXlt('2015-01-10'),]
  
  # Order closest weather stations in county 
  indx = order(sqrt((coords[1,2]-stData$latitude)^2 + (coords[1,1]-stData$longitude)^2))
  station = stData[indx,]
  
  return(station)
}

