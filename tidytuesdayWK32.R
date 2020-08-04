library(ggplot2)
library(ggrepel)
require(psych)
library(tidyverse)
library(lubridate)
library(dplyr)
library(formattable)
library(tidyr)
library(readr)

#loading the data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

#Creating the datasets
energy_types <- tuesdata$energy_types %>% 
  filter(country_name != "NA")

country_tots <- tuesdata$country_totals

#Map View
world_map <- map_data("world")

country_name <- unique(energy_types$country_name) 



eu_maps <- map_data("world", region = country_name)

# Label coordinates for country names
country_names <- eu_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>% 
  rename(country_name = region)

#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/

#Add Lat Long Coordinates to the energy data
energy_latlong <- left_join(energy_primary,world_map_gp,by="country_name")

#Plot the graph
ggplot(energy_latlong, aes(x = long, y = lat))+
  geom_polygon(aes( group = group, fill = type))+
  geom_text(aes(label = country_name), data = country_names,size = 3, hjust = 0.5)+
  theme_minimal()+
  ggtitle("How European Countries Predominantly Generated Electricity in 2018")+
  guides(fill=guide_legend(title=NULL))+  
  scale_fill_discrete(labels=c("Thermal", "Hydroelectric", "Nuclear","Wind"))+
  theme(legend.position="bottom")
