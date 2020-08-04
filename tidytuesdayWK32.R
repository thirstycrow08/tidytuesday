library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)

#Loading the data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

#Cleaning the data
energy_types <- tuesdata$energy_types %>% 
  filter(country_name != "NA")


#Creating map data and renaming region to country_name to make joins easier later
world_map <- map_data("world") %>% 
  rename(country_name = region)

#Getting unique country names from the Tidy Tuesday dataset
country_name <- unique(energy_types$country_name) 

#Building the EU map by subestting the world map using EU country names
eu_maps <- map_data("world", region = country_name)

# Creating label coordinates for country names 
# Original code for label creation: https://bit.ly/2DwIn8s
country_names <- eu_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>% 
  rename(country_name = region)

# Adding Lat Long coordinates to the energy data
energy_latlong <- left_join(energy_primary,world_map_gp,by="country_name")

# Plotting the graph
ggplot(energy_latlong, aes(x = long, y = lat))+
  geom_polygon(aes( group = group, fill = type))+
  geom_text(aes(label = country_name), data = country_names,size = 3, hjust = 0.5)+
  theme_minimal()+
  labs(
    title = paste(
      "European Energy"
    ),
    subtitle = paste(
      "How European Countries Predominantly Generated Electricity in 2018"
    ),
    caption = "Data from https://bit.ly/3a0Pjac"
  )+
  guides(fill=guide_legend(title=NULL))+                                
  scale_fill_brewer(labels=c("Thermal", "Nuclear", "Hydroelectric","Wind"),palette = "Spectral")+
  theme(legend.position="bottom")
