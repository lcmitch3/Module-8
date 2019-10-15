# Data aquired through tidytyesday/data/2019/2019-06-11

rm(list = ls(all = TRUE))
library(tidyverse)
library(lubridate)

#the following are two funtions created to turn long and lat cordinants into countries and continents

coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  # convert our list of points to a SpatialPoints object
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

#read in Data
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")
#check what the data is 
str(meteorites)
#remove need vectors to convert
latlong <- select(meteorites, long, lat, id) %>%
  drop_na()
  # this turns my lat and long coridinates and tells which country and continent they are on.
location <- latlong%>%
  mutate(country = coords2country(latlong)) %>%
  mutate(continents = coords2continent(latlong))%>%
  drop_na()
#plot1 is bar chart of how many meteorites per continent. 
plot1 <-
  ggplot(location, aes(continents))+
  geom_bar(fill = "Brown")+
  theme_minimal()+
  labs(title = "Meteors per Continent", 
       x = "Continent", 
       y = "Meteors",
       caption = "Data Source: tidytyesday/data/2019/2019-06-11")+
  theme(plot.title = element_text(hjust = 0.5)) #centers the title on the plot.

plot1

meteor <- meteorites %>% 
  dplyr::arrange(year)
meteor$num <- seq.int(nrow(meteor)) # adds a row that index the number of meteors

plot2 <- 
  ggplot(meteor, mapping = aes(year, num))+
  geom_smooth(color = "red")+
  theme_minimal()+
  xlim(1850,2050)+#change scale to only view important range.
  labs(title = "Number of Meteors Identified",
       x = "Year",
       y = "Meteors",
       caption = "Data Source: tidytyesday/data/2019/2019-06-11")+
  theme(plot.title = element_text(hjust = 0.5)) #centers the title on the plot.

plot2

