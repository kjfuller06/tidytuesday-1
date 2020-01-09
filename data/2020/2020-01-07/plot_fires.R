# Mapping NSW Current Incidents in R -------------------------------------------

library(sf)
library(mapview)
library(tidyverse)

#' Current Incidents Feed (GeoJSON)
#' This feed contains a list of current incidents from the NSW RFS, 
#' and includes location data and Major Fire Update summary information where available. 
#' Click through from the feed to the NSW RFS website for full details of the update. 
#' GeoJSON is a lightweight data standard that has emerged to support the sharing of 
#' information with location or geospatial data. 
#' It is widely supported by modern applications and mobile devices.

url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"

url %>% 
  jsonlite::write_json("2020-01-09_incidents.json")

fires <- st_read(url)

fires

fires2<-fires %>% 
  separate(description,c("Alert","Location","Council","Status","Type","Fire","Size","Agency","Updated"),sep=" <br />",convert=TRUE) %>% 
  separate(Size,c("garbage", "Size"),sep=": ",remove=TRUE) %>% 
  separate(Size,c("Size", "garbage"),sep=" ",remove=TRUE) %>% 
  mutate(Size=as.numeric(Size)) %>% 
  select(-garbage) %>% 
  filter(Size>0)

# fires2$Size<-grep(": ",x=fires2$Size,value=TRUE)
# fires2$Size<-grep(" ",x=fires2$Size,value=TRUE)
# fires2<-fires2[fires2$Size>0,]

mapview(fires2,zcol="Status",col.regions = c("yellow", "red", "forestgreen"))

#' Hacky way to get rid of points within geometry collections
fire_poly <- fires %>% 
  st_buffer(dist = 0) %>% 
  st_union(by_feature = TRUE)

mapview(fire_poly)

fires %>% 
  mutate(pubdate = as.character(pubDate),
         pubdate = as.Date(pubdate))
