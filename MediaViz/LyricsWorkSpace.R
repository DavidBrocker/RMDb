library(tidyverse)
library(genius)
library(stringr)
library(ggplot2)
library(tidytext)
library(forcats)

top_words <- function(artist,song){
lyr <- genius_lyrics(artist=artist,song=song)
lyr %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  count(sort=T) %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  ggplot(aes(fct_reorder(word,n),n,fill=word)) +
    geom_bar(stat="identity",show.legend = F)+
    coord_flip()+
    theme_classic()+
    labs(x="", y="Word Frequency \n",
         title=paste0("Top Words in ",song, " by ", artist))
}

d <- "Bones by Beast Coast"

d_splt <- strsplit(d,"by")

bns <- genius_lyrics(d_splt[[1]][2],d_splt[[1]][1])

bns_sent <- bns %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(sentiment) %>% 
  count() %>% 
  ungroup()
           
bns_sent$fraction <- bns_sent$n/sum(bns_sent$n)
bns_sent$ymax <- cumsum(bns_sent$fraction)
bns_sent$ymin = c(0, head(bns_sent$ymax, n=-1))   
bns_sent$labelPosition <- (bns_sent$ymax + bns_sent$ymin) / 2
bns_sent$label <- paste0(bns_sent$sentiment, "\n value: ", bns_sent$n)
ggplot(bns_sent, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=sentiment)) +
  geom_rect(show.legend = F) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart 
  theme_void() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_manual(values=c("#C71E1D","#6BCBB6"))
  

overall_sent <- function(artist,song){
  lyr <- genius_lyrics(artist=artist,song=song)
  lyr_sent<- lyr %>% 
    unnest_tokens(word,lyric) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(sentiment) %>% 
    count()
  lyr_sent$fraction <- lyr_sent$n/sum(lyr_sent$n)
  lyr_sent$ymax <- cumsum(lyr_sent$fraction)
  lyr_sent$ymin = c(0, head(lyr_sent$ymax, n=-1))   
  lyr_sent$labelPosition <- (lyr_sent$ymax + lyr_sent$ymin) / 2
  lyr_sent$label <- paste0(lyr_sent$sentiment, "\n value: ", lyr_sent$n)
  ggplot(lyr_sent, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=sentiment)) +
    geom_rect(show.legend = F) +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart 
    theme_void() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
    scale_fill_manual(values=c("#C71E1D","#6BCBB6"))
}

overall_sent("beast coast","left hand")

# Slider Control Function
bns_s <- bns %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(word,sentiment)
# Separate into positive
bns_s_p <- bns_s %>% 
  filter(sentiment=="positive") %>% 
  distinct(word,.keep_all = T)
# Separate into negative
bns_s_n <- bns_s %>% 
  filter(sentiment=="negative") %>% 
  distinct(word,.keep_all = T)
# Let user control
sent_num <- function(n){
sample(bns_s_p$word,n)
sample(bns_s_n$word,n)
}

sent_num(10)


faceless <- genius_album("The Faceless","Akeldama")

fa <- "Akeldama by the Faceless"

fa_splt <- strsplit(fa,"by")

fa_splt[[1]][2]

faceless %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  group_by(word,track_title) %>% 
  count(sort=T) %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  ggplot(aes(fct_reorder(word,n),n,fill=word)) +
  geom_bar(stat="identity",show.legend = F)+
  coord_flip()+
  theme_classic()



unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  count(sort=T) %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  ggplot(aes(fct_reorder(word,n),n,fill=word)) +
  geom_bar(stat="identity",show.legend = F)+
  coord_flip()+
  theme_classic()+
  labs(x="", y="Word Frequency \n",
       title=paste0("Top Words in ",song, " by ", artist))














