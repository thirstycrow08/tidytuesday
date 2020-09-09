library(tidyverse)
library(tidytext)
library(tidytuesdayR)
library(formattable)
library(lubridate)
library(forcats)


#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

keycropyields <- tuesdata$key_crop_yields

#249 countries featured
#58 Years - 1961-2018
#All Continents
#11 Crops

arable_land <- tuesdata$arable_land_pin
#209 countries
#54 years - 1961 - 2014

fertilizeruse <- tuesdata$cereal_crop_yield_vs_fertilizer_application
#58 Years - 1961-2018
#240 countries and all continents

landuse <- tuesdata$land_use_vs_yield_change_in_cereal_production
#258 Years - 9000BCE-2019. Need to filter out those.
#286 countries and all continents

tractoruse <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture
#258 Years - 9000BCE-2019. Need to filter out those.
#287 countries and all continents

#Tidy Tuesday Theme
tt_theme <- theme(
  plot.caption = element_text(hjust = 0), 
  plot.title = element_text(family = "Helvetica",face = "bold", color = "grey20"),
  # strip.background = element_blank(),
  # strip.text = element_text(hjust = 0),
  panel.grid.major.y = element_line(colour="#f0f3f4",size=1),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_line(colour="#34495e", size=1),
  strip.background = element_rect(fill="#5f6a6a"),
  # strip.text.y = element_text(size=14, angle=-90, face="bold"),
  # panel.border = element_rect(colour="blue", fill=NA, size=.5)
  # panel.grid.minor = element_blank(),
  # plot.margin=unit(c(0,1,0,1),"cm"),
  # # legend.position="right",
  # plot.caption=element_text(hjust=1,size=9,colour="grey30"),
  plot.subtitle=element_text(face="italic",size=12,colour="grey40")
  # plot.title=element_text(size=18,face="bold"
)

#Defining Colors

continents_color <- c(
  "Asia" = "#27ae60",
  "Africa" = "#f39c12",
  "Europe" = "#3498db",
  "Oceania" = "#85929e",
  "Americas" = "#e74c3c",
  "North America" = "#e74c3c"
)

all_crop_col <- c(
  "bananas" = "#f4d03f",
  "potatoes" = "#ca6f1e", 
  "cassava" = "#dc7633", 
  "maize" = "#52be80",
  "barley" = "#f0b27a",
  "beans"="#b03a2e",
  "cocoa_beans"="#6e2c00",
  "peas"="#27ae60",
  "rice"="#7b7d7d",
  "soybeans"="#f8c471",
  "wheat"="#f9e79f"
)

pal <- c(
  "bananas" = "#f4d03f",
  "potatoes" = "#ca6f1e", 
  "cassava" = "#9b59b6", 
  "maize" = "#52be80",
  "barley" = "#f4f6f7",
  "beans"="#f4f6f7",
  "cocoa_beans"="#f4f6f7",
  "peas"="#f4f6f7",
  "rice"="#f4f6f7",
  "soybeans"="#f4f6f7",
  "wheat"="#f4f6f7"
)

contyield_caption <- paste(strwrap("For the last 25 years, Europe has outpaced the world in agricultural
                                   yield, far outpacing Africa and Asia. No wonder Europe exports 
                                   so much food.", 
                                   50,
                                   indent = 2), 
                           collapse = "\n")

keycropyieldsclean %>% #Overall yields by continent
  filter(entity %in% c("Asia","Africa","Europe","Americas","Oceania") &year %in% c(1990:2015)) %>% 
  gather(wheat:bananas,
         key = "crop",value = "yield") %>% 
  filter(!is.na(yield)) %>%
  mutate(crop = factor(crop,levels = unique(crop))) %>% 
  mutate(crop = fct_recode(crop,
                           "wheat" = "wheat")) %>% 
  mutate(year = factor(year,levels = unique(year))) %>% 
  mutate(code = factor(code,levels = unique(code))) %>% 
  mutate(entity = factor(entity,levels = unique(entity))) %>% 
  group_by(entity) %>% 
  summarise(meanyield = mean(yield)) %>% 
  mutate(meanyield=round(meanyield,2)) %>% 
  # ggplot(aes(fct_reorder(crop,yield)))+
  ggplot(aes(fct_reorder(entity,-meanyield),meanyield,fill=entity))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = continents_color)+
  geom_text(aes(label=meanyield), vjust=1.5, colour="white")+
  tt_theme+
  guides(fill=FALSE)+
  annotate(geom = "text", 
           x=4.5, 
           y=8, 
           label=contyield_caption,
           family="sans serif",
           colour="#21618c", 
           size=3) +
  labs(
    fill = "Continents",
    x="",
    y="",
    title = "Mean Crop Yields by Continent",
    subtitle = "Mean Crop Yields in tonnes per hectare from 1990 to 2015",
    caption = "Source - Hannah Ritchie and Max Roser (2013) - Crop Yields"
  )

cropyield_caption <- paste(strwrap("The world is going bananas (and potatoes). 
                                   In the last 25 years, the world has learned to produce bananas and potatoes very efficiently. 
                                   This could be because the world has been producing grains for millennia and has figured out ways to maximize yield.", 
                               40,
                               indent = 2), 
                       collapse = "\n")
keycropyieldsclean %>% #Overall yields by crop
  filter(entity %in% c("Asia","Africa","Europe","Americas","Oceania") &year %in% c(1990:2015)) %>% 
  gather(wheat:bananas,
         key = "crop",value = "yield") %>% 
  filter(!is.na(yield)) %>%
  mutate(crop = factor(crop,levels = unique(crop))) %>% 
  # mutate(crop = fct_collapse(crop,
  #                          Fruits = c("bananas"),
  #                          Grains = c("rice","maize","wheat","barley","soybeans"),
  #                          Tubers = c("potatoes","cassava"),
  #                          Beans = c("peas","beans","cocoa_beans"))) %>%
  mutate(year = factor(year,levels = unique(year))) %>% 
  mutate(code = factor(code,levels = unique(code))) %>% 
  mutate(entity = factor(entity,levels = unique(entity))) %>% 
  group_by(crop) %>% 
  summarise(meanyield = mean(yield)) %>% 
  # ggplot(aes(fct_reorder(crop,yield)))+
  ggplot(aes(fct_reorder(crop,meanyield),meanyield,fill=crop))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = all_crop_col)+
  coord_flip()+
  annotate(geom = "text", 
           x=7, 
           y=16, 
           label=cropyield_caption,
           family="sans serif",
           colour="#21618c", 
           size=3) +
  tt_theme+
  guides(fill=FALSE)+
  labs(
    fill = "Crop Names",
    x="",
    y="",
    title = "Mean Crop Yields",
    subtitle = "Mean Crop Yields in tonnes per hectare from 1990 to 2015",
    caption = "source - Hannah Ritchie and Max Roser (2013) - Crop Yields"
  )



yield_caption <- paste(strwrap("Asia has seen the fastest growth in yield part due to the need to feed it's growing population and part due to improvements in technology.", 
                             40,
                             indent = 2), 
                     collapse = "\n")

keycropyieldsclean %>% #Mean annual yield for 25 years
  filter(entity %in% c("Asia","Africa","Europe","Americas","Oceania") &year %in% c(1990:2015)) %>% 
  gather(wheat:bananas,
         key = "crop",value = "yield") %>% 
  filter(!is.na(yield)) %>%
  mutate(crop = factor(crop,levels = unique(crop))) %>% 
  mutate(year = factor(year,levels = unique(year))) %>% 
  mutate(code = factor(code,levels = unique(code))) %>% 
  mutate(entity = factor(entity,levels = unique(entity))) %>% 
  group_by(entity,year) %>% 
  summarise(meanyield = mean(yield)) %>%
  # ggplot(aes(year,meanyield,color=entity))+
  ggplot(aes(year,meanyield,color=fct_reorder2(entity,year,meanyield)))+
  geom_line(aes(group=entity),size=1)+
  geom_point(aes(group=entity),size=1)+
  scale_x_discrete(breaks=c(1990, 1991, 1992, 1993, 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015),
                     labels=c("1990","91","92","93","94","95","96","97","98","99","2000","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15"))+
  scale_color_manual(values = continents_color)+
  annotate(geom = "text", 
           x=20, 
           y=6, 
           label=yield_caption,
           family="sans serif",
           colour="#21618c", 
           size=3) +
  # scale_color_manual(values=tt_palette)
  tt_theme+
  labs(
    fill = "Crop Names",
    x="",
    y="Tonnes per Hectare",
    color = "Continent",
    title = "Mean Crop Yields by Continent",
    subtitle = "Variation in crop yields (tonnes per hectare) from 1990 to 2015 for each continent",
    caption = "Source - Hannah Ritchie and Max Roser (2013) - Crop Yields"
  )
  
pop_caption <- paste(strwrap("Asia has continued to lead in both total population and population growth rate.", 
                             20,
                             indent = 2), 
                     collapse = "\n")

tractoruse %>%  #Population growth
  clean_names() %>% 
  rename_all(str_remove, "_.*") %>% 
  mutate(year = factor(year,levels = unique(year))) %>% 
  mutate(code = factor(code,levels = unique(code))) %>% 
  mutate(entity = factor(entity,levels = unique(entity))) %>% 
  filter(entity %in% c("Asia","Africa","Europe","North America","Oceania") &
           year %in% c(1990:2015)) %>% 
  ggplot(aes(year,total/1000000,color=fct_reorder2(entity,year,total)))+
  geom_line(aes(group=entity),size=1)+
  geom_point(aes(group=entity),size=1)+
  scale_x_discrete(breaks=c(1990, 1991, 1992, 1993, 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015),
                   labels=c("1990","91","92","93","94","95","96","97","98","99","2000","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15"))+
  
  annotate(geom = "text", 
           x=20, 
           y=3000, 
           label=pop_caption,
           family="sans serif",
           colour="#21618c", 
           size=3) +
  labs(
    color = "Continents",
    title = "Population (in Millions) Growth Across Continents",
    x="",
    y="",
    subtitle = "Population growth from 1990 to 2015 for each continent",
    caption = "Source - Hannah Ritchie and Max Roser (2013) - Crop Yields"
  )+
  scale_color_manual(values = continents_color)+
  tt_theme

keycropyieldsclean %>% #Yields and pop growth
  filter(entity %in% c("Asia","Africa","Europe","Americas","Oceania") &year %in% c(1990:2015)) %>% 
  gather(wheat:bananas,
         key = "crop",value = "yield") %>% 
  filter(!is.na(yield)) %>% 
  group_by(crop) %>% 
  ggplot(aes(crop))+
  geom_boxplot(aes(y=yield,fill=crop))+
  scale_fill_manual(values = pal)+
  coord_flip()+
  facet_wrap(entity~.)+
  labs(
    fill = "Crops",
    title = "Crop Yields Across Continents",
    x="",
    y="",
    subtitle = "Boxplots of crop yields (tonnes per hectare) across continents.",
    caption = "Source - Hannah Ritchie and Max Roser (2013) - Crop Yields"
  )+
  scale_color_manual(values = continents_color)+
  tt_theme
    
