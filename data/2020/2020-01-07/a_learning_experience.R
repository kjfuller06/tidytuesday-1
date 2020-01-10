# Mapping NSW Current Incidents in R -------------------------------------------

#' Current Incidents Feed (GeoJSON)
#' This feed contains a list of current incidents from the NSW RFS, 
#' and includes location data and Major Fire Update summary information where available. 
#' Click through from the feed to the NSW RFS website for full details of the update. 
#' GeoJSON is a lightweight data standard that has emerged to support the sharing of 
#' information with location or geospatial data. 
#' It is widely supported by modern applications and mobile devices.

url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"

url %>% 
  jsonlite::write_json("2020-01-10_incidents.json")

fires <- st_read(url)
fires3<-st_cast(fires,to="")
# st_write(fires, dsn = "fires1.shp", layer = "fires.shp", driver = "ESRI Shapefile")

# fires<-st_cast(fires,to=sfg)

fires2<-fires %>% 
  separate(description,c("Alert","Location","Council","Status","Type","Fire","Size","Agency","Updated"),sep=" <br />",convert=TRUE) %>% 
  separate(Size,c("garbage", "Size"),sep=": ",remove=TRUE) %>% 
  separate(Size,c("Size", "garbage"),sep=" ",remove=TRUE) %>% 
  mutate(Size=as.numeric(Size)) %>% 
  select(-garbage) %>% 
  filter(Size>0) %>% 
  mutate(point = map(geometry,
                   ~ if(length(..1[[1]]) == 1 ) {
                     pluck(..1)
                   } else {
                     pluck(..1, 1)
                   })) %>% 
  mutate(polygons = map(geometry,
                   ~ if(length(..1[[1]]) == 1 ) {
                     NA
                   } else {
                     pluck(..1, 2)
                   })) %>% 
  select(-point)


#separate the string of description into columns (still contains the header within the observations)
#separate size into the actual size and all the extra text and turn the character into a numeric
#remove all observations with zero area
#help from Desi: dealing with geometry- which has the structure of a vector with one or two lists. If one list, it is only a point. If two lists, the second contains a nested list with unlimited lists within (a list of polygons)
#extract (pluck) just the first list from every entry (map) in geometry, taking the whole list if there is only one (pluck(..1) indicates the whole element- the point coordinates) or just the first list if there are two (pluck(..1,1) indicates the first list within a list- the first list being the point coordinates and the second being the list of polygons)



fires2$geometry<-NULL
st_geometry(fires2) <- "polygons"

ggplot(data = fires2) +
  geom_sf() 

mapview(fires2,zcol="Alert",col.regions = c("yellow", "red", "forestgreen"))


#' Hacky way to get rid of points within geometry collections
fire_poly <- fires %>% 
  st_buffer(dist = 0) %>% 
  st_union(by_feature = TRUE)

mapview(fire_poly)

fire_poly %>% 
  mutate(pubdate = as.character(pubDate),
         pubdate = as.Date(pubdate))

mapview(fire_poly)
