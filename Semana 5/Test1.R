require(rtweet)
require(tidyverse)
#Nube de palabras
library(wordcloud)
#Colores
library(RColorBrewer)

tweet_df <- search_tweets("#LaUno", n = 100, include_rts = FALSE)

names(tweet_df)

tweets <- tweet_df %>% 
  select(text, retweet_count, favorite_count) %>% 
    mutate(tweets = str_remove_all(tweets$text, "#LaUno|que| que|que | qué"))



wordcloud(words = tweets$tweets)
