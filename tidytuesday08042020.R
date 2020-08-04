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
energy_types <- tuesdata$energy_types
country_tots <- tuesdata$country_totals

#Gathering Data
energy_types_narrow <- energy_types %>% 
  gather(`2016`,`2017`,`2018`,key = "Year",value = "Energy")

#Faceted Plot of Energy Gen by Year for Each Country
ggplot(energy_types_narrow,aes(x=Year,y=Energy,fill=type))+
  geom_bar(stat = "Identity")+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~country_name)

ggplot(energy_types_narrow,aes(x=reorder(country_name,Energy),y=Energy,order=desc(Energy)))+
  geom_bar(stat = "Identity",position = "dodge")+
  scale_fill_brewer(palette = "Set2")+
  # theme_classic()+
  coord_flip()+
  facet_wrap(Year~type)


ggplot(energy_types,aes(x=country_name,y=`2018`,fill=type))+
  geom_bar(stat = "Identity")+
  coord_flip()  

ggplot(energy_types_narrow,aes(y=Energy,x=type))+
  geom_bar(stat = "Identity")+
  scale_fill_brewer(palette = "Set2")+
  facet_grid(country_name~Year)


#Map View
# world_map <- map_data("world")
# ggplot(world_map, aes(x = long, y = lat, group = group)) +
#   geom_polygon(fill="white", colour = "black")

country_name <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic","Bulgaria","Estonia","Ireland","Spain",
  "France","Croatia","Cyprus","Latvia","Lithuania","Luxembourg",
  "Hungary","Malta","Netherlands","Portugal","Austria","Poland",
  "Romania","Slovenia","Slovakia","Czechia"
  
)
eu_maps <- map_data("world", region = country_name)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- eu_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>% 
  rename(country_name = region)

ggplot(energy_latlong, aes(x = long, y = lat))+
  geom_polygon(aes( group = group, fill = type))+
  geom_text(aes(label = country_name), data = region.lab.data,size = 3, hjust = 0.5)+
  theme_classic()
  facet_wrap(.~type)

ggplot(energy_types_narrow,aes(x=country_name,y=Energy))+
  geom_bar(stat = "identity")

world_map_gp <- world_map %>% 
  rename(country_name = region)

energy_latlong <- left_join(energy_primary,world_map_gp,by="country_name")

energy_types_summary <- summarize(energy_types,MaxEn=max(`2018`))


library(dplyr)

energy_primary <- energy_types %>%
  select(country_name,type,`2018`) %>% 
  group_by(country_name) %>%
  slice_max(`2018`) %>%

slice_max(energy_types,order_by = "country_name")

