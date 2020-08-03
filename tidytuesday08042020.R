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

energy_types <- tuesdata$energy_types
country_tots <- tuesdata$country_totals

View(energy_types)
formattable(energy_types)

energy_types_narrow <- energy_types %>% 
  gather(`2016`,`2017`,`2018`,key = "Year",value = "Energy")

#Faceted Plot of Energy Gen by Year for Each Country
ggplot(energy_types_narrow,aes(x=Year,y=Energy,fill=type))+
  geom_bar(stat = "Identity")+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~country_name)

ggplot(energy_types,aes(x=country_name,y=`2018`,fill=type))+
  geom_bar(stat = "Identity")+
  coord_flip()

ggplot(energy_types_narrow,aes(y=Energy,x=type))+
  geom_bar(stat = "Identity")+
  scale_fill_brewer(palette = "Set2")+
  facet_grid(country_name~Year)


view(country_tots)
