library(tidyverse)
library(tidytext)
library(tidytuesdayR)
library(formattable)
library(lubridate)

tt_theme <- theme(
  plot.caption = element_text(hjust = 0), 
  plot.title = element_text(family = "Helvetica",face = "bold", color = "grey20"),
  # strip.background = element_blank(),
  # strip.text = element_text(hjust = 0),
  panel.grid.major.y = element_line(colour="#f0f3f4",size=.5),
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
  legend.position="bottom",
  # plot.caption=element_text(hjust=1,size=9,colour="grey30"),
  plot.subtitle=element_text(face="italic",size=12,colour="grey40")
  # plot.title=element_text(size=18,face="bold"
)

colors <- c(
  "#e67e22",
  "#c0392b",
  "#bdc3c7",
  "#3498db",
  "#884ea0",
  "#34495e",
  "#f1c40f")

#Get the data
tt <- tt_load("2020-09-08")
tuesdata <- tidytuesdayR::tt_load(2020, week = 37)

fr_dialogs <- tuesdata$friends
fr_info <- tuesdata$friends_info #traditional dataset
fr_emotions <- tuesdata$friends_emotions

fr_info_summary <- fr_info %>% #rating and views by seasons
  filter(season %in% c("1","2","3","4")) %>% 
  mutate(season = factor(season,levels = unique(season))) %>%
  mutate(season = fct_recode(season,
                             "Season1" = "1",
                             "Season2" = "2",
                             "Season3" = "3",
                             "Season4" = "4",
                             "Season5" = "5",
                             "Season6" = "6",
                             "Season7" = "7",
                             "Season8" = "8",
                             "Season9" = "9",
                             "Season10" = "10",
  )) %>%
  group_by(season) %>% 
  summarise(tot_views = sum(us_views_millions),avg_rating=mean(imdb_rating)) %>% 
  mutate(tot_views_hmil = tot_views/100)

fr_emotions %>% #Emotions per season
  filter(season %in% c("1","2","3","4")) %>% 
  mutate(emotion = factor(emotion,levels = unique(emotion))) %>% 
  mutate(season = factor(season,levels = unique(season))) %>%
  mutate(season = fct_recode(season,
                             "Season1" = "1",
                             "Season2" = "2",
                             "Season3" = "3",
                             "Season4" = "4",
                             "Season5" = "5",
                             "Season6" = "6",
                             "Season7" = "7",
                             "Season8" = "8",
                             "Season9" = "9",
                             "Season10" = "10",
  )) %>%
  group_by(season,emotion) %>% 
  summarise(utterances = n()) %>% 
  ggplot(aes(season))+
  geom_col(aes(y=utterances,fill=emotion))


fr_emotions %>% #Emotions per season
  filter(season %in% c("1","2","3","4")) %>% 
  mutate(emotion = factor(emotion,levels = unique(emotion))) %>% 
  mutate(season = factor(season,levels = unique(season))) %>%
  mutate(season = fct_recode(season,
                             "Season1" = "1",
                             "Season2" = "2",
                             "Season3" = "3",
                             "Season4" = "4"
  )) %>%
  group_by(season,emotion) %>%
  summarise(utterances = n()) %>% 
  spread(key = emotion, value = utterances) %>% 
  left_join(fr_info_summary,by="season") %>% 
  clean_names() %>% 
  mutate(mad=mad/500) %>% 
  mutate(neutral=neutral/500) %>% 
  mutate(joyful=joyful/500) %>% 
  mutate(scared=scared/500) %>% 
  mutate(sad=sad/500) %>% 
  mutate(powerful=powerful/500) %>% 
  mutate(peaceful=peaceful/500) %>% 
  mutate(avg_rating = round(avg_rating,2)) %>% 
  pivot_longer(mad:peaceful, names_to = "emotions", values_to = "utterrances") %>% 
  ggplot(aes(season))+
  geom_point(aes(y=avg_rating,size=tot_views),col="#239b56")+
  geom_text(aes(y=avg_rating,label=avg_rating), vjust=2.5,size=4,col="#239b56")+
  geom_line(aes(y=avg_rating),group="season",col="#239b56",size=1)+
  # scale_size_area(max_size=5)+
  geom_col(aes(y=utterrances,fill=emotions))+
  scale_y_continuous(sec.axis = sec_axis(~ . *5,name = "Utterances (Thousands)"))+
  scale_fill_manual(values = colors)+
  labs(
    title = "Friends: Ratings, Views and Emotions",
    subtitle = "Did emotions impact the ratings and viewership?",
    x="",
    y="Average IMDB Rating",
    credit = "Friends",
    size = "Views in Millions",
    fill = "Emotions",
    caption = "Tidy Tuesday Week 37                                Mathew Varghese                                   @thirsty_crow"
  )+
  tt_theme


###################################################Other Stuff#########################

#Friends Info Analysis
fr_info %>% #rating and views by seasons
  group_by(season) %>% 
  summarise(tot_views = sum(us_views_millions),avg_rating=mean(imdb_rating)) %>% 
  mutate(tot_views_hmil = tot_views/100) %>% 
  ggplot(aes(season))+
  geom_col(aes(y=tot_views_hmil),fill="blue")+
  geom_line(aes(y=avg_rating))+
  geom_point(aes(y=avg_rating),col="maroon")+
  labs(
    title = "Views and Ratings",
    x="",
    y="",
    credit = "Friends"
  )+
  tt_theme


fr_info %>% #rating and views by director
  group_by(season,directed_by) %>%
  mutate(season = factor(season,levels = unique(season))) %>%
  mutate(directed_by = factor(directed_by,levels = unique(directed_by))) %>%
  mutate(season = fct_recode(season,
                             "Season1" = "1",
                             "Season2" = "2",
                             "Season3" = "3",
                             "Season4" = "4",
                             "Season5" = "5",
                             "Season6" = "6",
                             "Season7" = "7",
                             "Season8" = "8",
                             "Season9" = "9",
                             "Season10" = "10",
  )) %>%
  summarise(episodes = n()) %>% 
  ggplot(aes(season))+
  geom_col(aes(y=episodes,fill=directed_by))

fr_info %>% #rating and views by director
  group_by(directed_by) %>% 
  summarise(tot_views = sum(us_views_millions),avg_rating=mean(imdb_rating),total=n(),avg_views=mean(us_views_millions)) %>% 
  arrange(desc(total)) %>% 
  mutate(tot_views_hmil = tot_views/100, avg_views_scaled = avg_views*.5) %>% 
  ggplot(aes(directed_by))+
  geom_col(aes(y=tot_views_hmil),fill="blue")+
  geom_col(aes(y=-total),fill="orange")+
  geom_line(aes(y=avg_rating),group="directed_by")+
  geom_point(aes(y=avg_rating),col="maroon")+
  geom_line(aes(y=avg_views),group="directed_by")+
  geom_point(aes(y=avg_views),col="green")+
  coord_flip()+
  tt_theme





ggplot(aes(season))+
  geom_col(aes(y=tot_views_hmil),fill="lightblue")+
  geom_line(aes(y=avg_rating),group="season")+
  geom_point(aes(y=avg_rating,size=tot_views_hmil),col="red")
