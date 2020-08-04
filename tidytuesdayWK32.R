library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)

#loading the data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

#Cleaning the data
energy_types <- tuesdata$energy_types %>% 
  filter(country_name != "NA")


#Creating map data
world_map <- map_data("world")

country_name <- unique(energy_types$country_name) 

eu_maps <- map_data("world", region = country_name)

# Create Label coordinates for country names
country_names <- eu_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>% 
  rename(country_name = region)

#Original code for label creation: https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/

#Add Lat Long Coordinates to the energy data
energy_latlong <- left_join(energy_primary,world_map_gp,by="country_name")

#Plot the graph
ggplot(energy_latlong, aes(x = long, y = lat))+
  geom_polygon(aes( group = group, fill = type))+
  geom_text(aes(label = country_name), data = country_names,size = 3, hjust = 0.5)+
  theme_minimal()+
  ggtitle("How European Countries Predominantly Generated Electricity in 2018")+
  guides(fill=guide_legend(title=NULL))+  
  scale_fill_brewer(labels=c("Thermal", "Nuclear", "Hydroelectric","Wind"),palette = "Spectral")+
  theme(legend.position="bottom")
