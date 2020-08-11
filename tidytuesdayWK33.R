library(tidyverse)
library(tidytext)
library(stringr)
library(textdata)

#loading the data
tuesdata <- tidytuesdayR::tt_load('2020-08-11')

avatar <- tuesdata$avatar
scene_description <- tuesdata$scene_description

#Common words by book
data(stop_words)

#Tokenizing the text and removing stop words
avatar_text <- avatar %>% 
  select(book,chapter,character, character_words) %>% 
  filter(character != "Scene Description") %>% 
  group_by(book,character) %>% 
  mutate(line_num = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, character_words) %>%
  anti_join(stop_words)

#Getting sentiment data. I decided to use NRC lexicon
sentiment_nrc <- get_sentiments("nrc")

#Joining words to sentiment
avatar_sentiment <- avatar_text %>%
  inner_join(sentiment_nrc) %>%
  count(book, index = line_num %/% 80, sentiment)
  
#Faceted plot of sentiment by book
ggplot(avatar_sentiment, aes(sentiment,n)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")+
  coord_flip()