library(tidyverse)
library(tidytext)
library(tidytuesdayR)
library(formattable)
library(lubridate)


#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 35)
chopped <- tuesdata$chopped

#Subset of episode rating by year by judge
year_judge_rating <- chopped %>% #Judges with top ratings
  gather(judge1,judge2,judge3,key=judge,value = judgeName) %>% 
  count(year(mdy(air_date)),episode_rating,judgeName) %>% 
  rename(Year = "year(mdy(air_date))") %>% 
  group_by(Year,judgeName) %>% 
  # top_n(5,episode_rating) %>% view()
  summarise(avgRating = mean(episode_rating))

#Plotting the data
chopped %>% 
  gather(judge1,judge2,judge3,key=judge,value = judgeName) %>% 
  count(year(mdy(air_date)),judgeName,) %>% 
  rename(Year = "year(mdy(air_date))",Appearences = n) %>% 
  group_by(Year) %>% 
  top_n(5,Appearences) %>% 
  mutate(avgRating = ifelse(Year %in% year_judge_rating$Year &
                              judgeName %in% year_judge_rating$judgeName,
                            year_judge_rating$avgRating,"NA")) %>% 
  ggplot(aes(x=Year)) +
  geom_point(aes(y=judgeName,color=avgRating,size=Appearences))+
  geom_text(aes(y=judgeName,label=format(round(avgRating,1))),size=2.5,vjust = -2)+
  scale_colour_distiller(palette = "Oranges",direction = 1)+
  theme(axis.line = element_line(colour = "#7a2307", 
                                         size = 1, linetype = "solid"),
        panel.grid.major = element_line(colour="#d1800e"),
        panel.grid.minor = element_line(colour="#d1800e", linetype="dashed", size=0.2),
        panel.background = element_rect(fill="#fffeef"),
        plot.title = element_text(colour="#7a2307", size=12, face="bold"))+
  labs(
    title = "Chopped: Judge Appearences and Ratings",
    subtitle = "5 judges with most appearences in a year and average episode ratings when they appeared.",
    x = "Year",
    y="Judge Name",
    color = "Avg. Episode Rating",
    size = "Appearences in a Year"
  )
