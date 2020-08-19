library(tidyverse)
library(tidytext)
library(tidytuesdayR)
library(formattable)
library(ggthemes)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
plants <- tuesdata$plants
threats <- tuesdata$threats
actions <- tuesdata$actions

#Basic Cleaning
threat_filtered <- threats %>% 
  filter(threatened == 1) 

#Create dataset for plotting
threat_by_continent <- threat_filtered %>% 
  mutate(year_last_seen = fct_relevel(year_last_seen,"Before 1900")) %>% 
  filter(!is.na(year_last_seen)) %>% 
  count(year_last_seen,continent,group,threat_type, red_list_category) %>% 
  mutate(threat_type = fct_reorder(threat_type,n,sum))

#Plot the data
threat_by_continent %>% 
  ggplot(aes(year_last_seen,n,col = continent,shape = group,size=n))+
  geom_point(stat = "identity")+
  scale_shape_manual(values=c(13,1,15,16,17,18))+
  coord_flip()+
  labs(
    y = "# of Plant Groups",
    x = "Years Observed",
    col = "Continent",
    shape = "Plant Type",
    title = "Plants: A Century of Disaster",
    size="# of Plant Groups"
  )+
  facet_grid(red_list_category~threat_type, scales = "free")+
  theme_light()

threat_by_continent %>% 
  count(group,red_list_category)
