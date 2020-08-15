###################################IMDb Tile############################################################
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(htmlwidgets)
library(ggplot2)
library(ggthemes)

imdb_tile <- function(x){
# x <- "Dexter"
x <- ifelse(
  str_detect(x, " "),
  paste0("https://www.imdb.com/find?q=",
         str_replace_all(x, " ", "+")),
  paste0("https://www.imdb.com/find?q=",x)
)
imdb_link <- x
# Get Link containing tt ID
imdb_tt <- imdb_link %>%
  read_html() %>%
  html_nodes("#findSubHeader+ .findSection .result_text a") %>%
  html_attr("href") %>%
  .[[1]]
# Final Link
new_link <- paste0("https://www.imdb.com",imdb_tt)
# Get Name
show_name <- new_link %>% 
  read_html() %>% 
  html_nodes("h1") %>% 
  html_text(trim = T)
# Isolate tt
tt_isolate <- unlist(str_match(imdb_tt,
                            "(?<=\\/)(tt[0-9]+)(?<!\\/)"))
tt_isolate <- tt_isolate[[1]][1]

rating_show_url <- new_link
num_seasons <- rating_show_url %>%
  read_html() %>%
  html_nodes(".clear+ div a:nth-child(1)") %>%
  html_text(trim = T) %>%
  as.numeric()

rating_season_urls <-
  paste0("https://www.imdb.com/title/",tt_isolate,"/episodes?season=",
         1:num_seasons)

get_episode_titles <- function(urls) {
  urls %>%
    read_html() %>%
    html_nodes("#episodes_content strong a") %>%
    html_text(trim = T)
}
episode_titles <- unlist(map(rating_season_urls, get_episode_titles))

get_episode_ratings <- function(urls) {
  urls %>%
    read_html() %>%
    html_nodes(".ipl-rating-star.small .ipl-rating-star__rating") %>%
    html_text(trim = T) %>%
    as.numeric()
}
episode_ratings <-
  unlist(map(rating_season_urls, get_episode_ratings))

get_season <- function(urls) {
  urls %>%
    read_html() %>%
    html_nodes("#episode_top") %>%
    html_text(trim = T)
}
season <- unlist(map(rating_season_urls, get_season))
season <- unlist(str_remove_all(season, "Season|\\s")) %>%
  as.numeric()

get_episode_number <- function(urls) {
  urls %>%
    read_html() %>%
    html_nodes(".zero-z-index div") %>%
    html_text(trim = T)
}

season_ep_rating <-
  unlist(map(rating_season_urls, get_episode_number))

szn <- unlist(strsplit(season_ep_rating, ","))
szn_list <- as.numeric(unlist(str_match_all(szn, "(?<=S)(\\d+)")))
ep_list <- as.numeric(unlist(str_match_all(szn, "(?<=Ep)(\\d+)")))


season_num <- str_match(season_ep_rating, "S\\d+")
season_num <- str_remove_all(season_num, "S")
season_num <- as.numeric(season_num)

episode_num <- str_match(season_ep_rating, "Ep\\d+")
episode_num <- as.numeric(str_remove_all(episode_num, "Ep"))


show_df <- data.frame(
  season_num,
  episode = episode_num,
  titles = episode_titles,
  ratings = episode_ratings
)

show_df <- show_df %>%
  mutate(
    quality = case_when(
      ratings < 5.0 ~ "Garbage",
      ratings < 6.5 ~ "Bad",
      ratings < 7.5 ~ "Regular",
      ratings < 8.5 ~ "Good",
      ratings < 10 ~ "Great"
    )
  )

cols = c(
  "Bad" = "red2",
  "Garbage" = "dodgerblue2",
  "Great" = "greenyellow",
  "Regular" = "darkorange1",
  "Good" = "gold1"
)

ggplot(show_df, aes(as.factor(season_num), as.factor(episode), fill = quality)) +
  geom_tile(colour = "black") +
  geom_text(aes(label = ratings)) +
  scale_fill_manual(values = cols) +
  theme_clean() +
  theme(axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NULL,linetype = "blank"),
        plot.caption = element_text(face="italic"))+
labs(x="Season",y="Episode Number",
     title=paste0("Series Ratings for ",show_name),
     caption = "All information sourced from imdb.com")

}
