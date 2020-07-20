

###################################IMDb Tree############################################################
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(htmlwidgets)
library(ggplot2)
rating_show_url <- "https://www.imdb.com/title/tt0397306/"
num_seasons <- rating_show_url %>% 
  read_html() %>% 
  html_nodes(".clear+ div a:nth-child(1)") %>% 
  html_text(trim=T) %>% 
  as.numeric()
rating_season_urls <- paste0("https://www.imdb.com/title/tt0397306/episodes?season=",1:num_seasons)

get_episode_titles <- function(urls){
  urls %>% 
    read_html() %>% 
    html_nodes("#episodes_content strong a") %>% 
    html_text(trim=T)
}
episode_titles <- unlist(map(rating_season_urls,get_episode_titles))

get_episode_ratings <- function(urls){
  urls %>% 
    read_html() %>% 
    html_nodes(".ipl-rating-star.small .ipl-rating-star__rating") %>% 
    html_text(trim = T) %>% 
    as.numeric()
}
episode_ratings <- unlist(map(rating_season_urls,get_episode_ratings))

get_season <- function(urls){
  urls %>% 
    read_html() %>% 
    html_nodes("#episode_top") %>% 
    html_text(trim=T) 
}
season <- unlist(map(rating_season_urls,get_season))
season <- unlist(str_remove_all(season,"Season|\\s")) %>% 
  as.numeric()

get_episode_number <- function(urls){
  urls %>% 
    read_html() %>% 
    html_nodes(".zero-z-index div") %>% 
    html_text(trim=T)
}

season_ep_rating <- unlist(map(rating_season_urls,get_episode_number))

szn <- unlist(strsplit(season_ep_rating,","))
szn_list <- as.numeric(unlist(str_match_all(szn,"(?<=S)(\\d+)")))
ep_list <- as.numeric(unlist(str_match_all(szn,"(?<=Ep)(\\d+)")))


season_num <- str_match(season_ep_rating,"S\\d+")
season_num <- str_remove_all(season_num,"S")
season_num <- as.numeric(season_num)
episode_num <- str_match(season_ep_rating,"Ep\\d+")
episode_num <- as.numeric(str_remove_all(episode_num,"Ep"))


show_df <- data.frame(
  season_num=season_num[1:290],
  episode=episode_num[1:290],
  titles=episode_titles[1:290],
  ratings = episode_ratings
)

show_df <- show_df %>% 
  mutate(quality=case_when(
    ratings< 5.0 ~ "Garbage", 
    ratings< 6.5 ~ "Bad",
    ratings< 7.5 ~ "Regular",
    ratings< 8.5 ~ "Good", 
    ratings< 10 ~ "Great")) 

cols=c("Bad"="red2",
       "Garbage" ="dodgerblue2",
       "Great" ="greenyellow",
       "Regular"="darkorange1", 
       "Good" ="gold1")

ggplot(show_df,aes(season_num,episode,fill=quality)) +
  geom_tile(colour="black") +
  geom_text(aes(label=ratings)) +
  scale_fill_manual(values = cols) +
  theme_classic()

