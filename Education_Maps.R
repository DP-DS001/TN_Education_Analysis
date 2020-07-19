library(tidyverse)
library(maps)
library(mapdata)
library(ggplot2)
library(readxl)
library(stringr)
library(dplyr)
library(tigris)
library(leaflet)
library(scales)



#'MAP TN'
usa <- map_data("usa")
states <- map_data("state")
TN <- subset(states, region %in% c("tennessee"))
ggplot(data = TN) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

#'ADD COUNTIES'
counties <- map_data("county")
tn_county <- subset(counties, region == "tennessee")
tn_outline <- ggplot(data = TN, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
tn_outline

tn_outline + 
  geom_polygon(data = tn_county, fill = "blue", color = "white") +
  geom_polygon(color = "black", fill = NA)


schools <- school_districts("Tennessee")
leaflet(schools) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5)


#ADD DATA
tvaas <- read_csv('data/tvaas.csv')
crosswalk <- read_excel('data/data_district_to_county_crosswalk.xls')
pop <- read_csv('data/Pop.csv')
pop

names(pop) <- c("rank","subregion", "population")
pop$population <- as.numeric(pop$population)

pop_map <- inner_join(tn_county, pop, by = "subregion")

pop_county <- tn_outline + 
  geom_polygon(data = pop_map, aes(fill = population), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() + scale_fill_gradientn(colors = rev(rainbow(10)),
                                    breaks = c(5000,20000,70000,500000, 900000), 
                                    trans = "log10", labels = comma) + ggtitle("Population by County")
                                    
pop_county + clean

clean <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
