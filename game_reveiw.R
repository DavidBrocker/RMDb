library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(collapsibleTree)
#############Function: Get URLS########################
get_recommended_games_link <- function(urls){
  url_end <- urls %>% 
    read_html() %>% 
    html_nodes("#related-tab-games-1 .wiki-relation a") %>% 
    html_attr("href")
    paste0("https://www.giantbomb.com",url_end)
}

#################Function: Get Name####################
get_recommended_games_name <- function(urls){
  # Gets Game Name
    newrl <- urls %>% 
    read_html() %>% 
    html_nodes("#related-tab-games-1 .wiki-relation a") %>%
    html_text(trim=T)
  # Gets parent Game Name 
    game_name <- urls %>% 
      read_html() %>% 
      html_nodes(".wiki-title") %>% 
      html_text(trim = T)
  # Makes temporary dataframe
    newrl %>% 
      data.frame(parent=rep(game_name))
}

# In practice: 
###########################################################
# 1
game_url <- "https://www.giantbomb.com/celeste/3030-58634/"
# 2
related_games_url <- 
  game_url %>% 
  read_html() %>% 
  html_nodes("#related-tab-games-1 .wiki-relation a") %>% 
  html_attr("href")  
# 3
related_games_urls <- paste0("https://www.giantbomb.com",related_games_url)
# 4
xx <- map(related_games_urls,get_recommended_games_name)
# 5
collapsibleTree(xx,
                hierarchy = c("parent",".")
                )



grc <- function(gb_link){
  ###########Function: Get URLS################
  get_recommended_games_link <- function(urls){
    url_end <- urls %>% 
      read_html() %>% 
      html_nodes("#related-tab-games-1 .wiki-relation a") %>% 
      html_attr("href")
    paste0("https://www.giantbomb.com",url_end)
  }
  #############Function: Get Name################
  get_recommended_games_name <- function(urls){
    # Gets Game Names
    newrl <- urls %>% 
      read_html() %>% 
      html_nodes("#related-tab-games-1 .wiki-relation a") %>%
      html_text(trim=T)
    game_name <- urls %>% 
      read_html() %>% 
      html_nodes(".wiki-title") %>% 
      html_text(trim = T)
    # Makes temporary dataframe
    newrl %>% 
      data.frame(parent=rep(game_name))
  }
  ############Function: Gets Game Title###########
  get_game_name <-function(urls){
    urls %>% 
    read_html() %>% 
    html_nodes(".wiki-title") %>% 
    html_text(trim = T)
  }
  # Determine what to search
  gb_search_url <- ifelse(
    str_detect(gb_link, " "),
    paste0(
      "https://www.giantbomb.com/search/?i=&q=",
      str_replace_all(gb_link, " ", "+")
      ),
  # Create search link
    paste0("https://www.giantbomb.com/search/?i=&q=", gb_link)
    )
  # Get results of first search
  gb_rec_link <- gb_search_url %>% 
    read_html() %>% 
    html_nodes("#js-sort-filter-results > li:nth-child(1) > a") %>% 
    html_attr("href")
  # Save it as the link
  gb_rec_link <- paste0("https://www.giantbomb.com",gb_rec_link)
  # Get (grand)parent title
  gp_game_name <- get_game_name(gb_rec_link)
  # Get (grand)parent recommendations as links
  gb_recs_link <- get_recommended_games_link(gb_rec_link)
  # Get (grand)parent recommendations as names
  gb_recs_names <- map_df(gb_recs_link,get_recommended_games_name)
  # Make Tree
  collapsibleTree(gb_recs_names,
                  hierarchy = c("parent","."),
                  root=paste0(gp_game_name),
                  fill=c(
                    "black",
                    rep("#B0B8B4FF",length(unique(gb_recs_names$parent))),
                    rep("#184A45FF",length(unique(paste(gb_recs_names$parent,gb_recs_names$.))))
                  )
  )
}















gr_link <- "https://www.goodreads.com/book/show/264946.Hominids?from_search=true&from_srp=true&qid=2McFHTGsRA&rank=1"

gr_link <- ifelse(
  str_detect(book_link, " "),
  paste0(
    "https://www.goodreads.com/search?q=",
    str_replace_all(book_link, " ", "+")
  ),
  paste0("https://www.goodreads.com/search?q=", book_link)
)
# Get Link to Book
gr_link <- gr_link %>%
  read_html() %>%
  html_nodes("table a") %>%
  html_attr("href") %>%
  .[1]
# Final Link
gr_link <- paste0("https://goodreads.com", gr_link)
# FUNCTION: Get recommendations
get_all_recs <- function(i) {
  read_html(i) %>%
    html_nodes(".cover a") %>%
    html_attr("href") %>%
    .[1:18]
}
# Get 18 Parent Recommendations
rec_books <- get_all_recs(gr_link)
# get other recs name
get_all_clean <- function(urls){
  url_cln <- urls %>% 
    read_html() %>% 
    html_nodes("bookTitle") %>% 
    html_text(trim=T)
  
  book_name <-read_html(i) %>%
    html_nodes(".cover a") %>%
    html_attr("href") %>%
    .[1:18]
  
  data.frame(url_cln,book_name)
}




# Get 324 child recommendations
recs <- unlist(map(rec_books, get_all_clean))
# FUNCTION: Clean Book Links Up
get_clean_recs <- function(x) {
  x <- str_remove_all(x, "https://www.goodreads.com/book/show/\\d+.")
  x <- str_replace_all(x, "-|_", " ")
  x <- str_to_title(x)
}
# Clean parent recommendations
rec_books_cleaned <- get_clean_recs(rec_books)
# Clean child recommendations
recs_cleaned <- get_clean_recs(recs)
# Create vector of original 18 recommendations repeating 18 times each
books_like <- 0
for (i in 1:18) {
  books_like <- rep(rec_books_cleaned, each = 18)
}
# Create dataframe with original parent recommendations and child reccommendations
book_tree <- data.frame(books_like,
                        book_recs = recs_cleaned)
# Create Tree
collapsibleTree(
  book_tree,
  hierarchy = c("books_like", "book_recs"),
  root = paste0(x_book_title()),
  fill=c(
    "black",
    rep("#B0B8B4FF",length(unique(book_tree$books_like))),
    rep("#184A45FF",length(unique(paste(book_tree$books_like,book_tree$book_recs))))
  )
)



