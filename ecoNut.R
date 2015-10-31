# Function to get aggregated nutrient ecoregion based on lat/long
# pointsDF is a data.frame, where col1: longitude (negative in US)
# col2 contains the latitude in degrees
ecoNut <- function(pointsDF){
  require(rgdal)
  require(sp)
  epa = readOGR(dsn='/Users/hilarydugan/Downloads/SALT/us_nut_agg',
                layer='us_nutr_agg',stringsAsFactors=FALSE,verbose=F)
  epaTrans = spTransform(epa,CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  o = over(pointsSP,epaTrans) #overlay points on polygons
  code = o$CODE
  return(code)
}

