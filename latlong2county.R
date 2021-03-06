#Function that returns US county from lat/long data
#from http://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
latlong2county <- function(pointsDF) {
  require(sp)
  require(maps)
  require(maptools)
  require(rgdal)
  if (!complete.cases(pointsDF)){
    countyNames = 'unknown'
  } else {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per county
    counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
    counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                       proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, counties_sp)
    
    # Return the county names of the Polygons object containing each point
    countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
    county = unlist(strsplit(countyNames[indices], ","))[2]
    return(county)
  }
}


#Function that returns US state from lat/long data
#from http://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
latlong2state <- function(pointsDF) {
  require(sp)
  require(maps)
  require(maptools)
  require(rgdal)
  if (!complete.cases(pointsDF)){
    countyNames = 'unknown'
  } else {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per county
    counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
    counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                       proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, counties_sp)
    
    # Return the county names of the Polygons object containing each point
    countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
    state = unlist(strsplit(countyNames[indices], ","))[1]
    return(state)
  }
}
