library(rvest)
library(stringr)

better_tree <- function(x) {
  # Find URL of search term
  show_term <- function(x) {
    ifelse(
      str_detect(x, " "),
      paste0(
        "https://www.imdb.com/find?q=",
        str_replace_all(x, " ", "_")
      ),
      paste0("https://www.imdb.com/find?q=", x)
    )
  }
  imdb_link <- show_term(x)
  # Get Link containing tt ID
  imdb_tt <- imdb_link %>%
    read_html() %>%
    html_nodes("#findSubHeader+ .findSection .result_text a") %>%
    html_attr("href") %>%
    .[[1]]
  # Final Link
  new_link <- paste0("https://www.imdb.com", imdb_tt)
  # Get show name
  show_name <-  new_link %>%
    read_html() %>%
    html_nodes("h1") %>%
    html_text()
  # Get recs
  show_tree_show <- new_link %>%
    read_html() %>%
    html_nodes(".rec-title b") %>%
    html_text(trim = T)
  # This extracts the tt from the shows saved in the previous step
  show_morelike <- new_link %>%
    read_html() %>%
    html_nodes(".rec_item,.rec_selected") %>%
    html_attr("data-tconst")
  # This makes a vector of all (12) of the URLs
  yourls <- paste0("https://www.imdb.com/title/", show_morelike, "/")
  # Since we are working with 12 URLs we need to use the map function to apply this function over a list.
  doThis <- function(i) {
    show_tree <- read_html(i) %>%
      html_nodes(".rec-title b") %>%
      html_text(trim = T)
  }
  # This runs the function with each URL in the vector
  shows_tree_like_chr <- unlist(map(yourls, doThis))
  # This sets up the data how we need it by repeating each show title 12 times
  show_tree_showlist <- NULL
  for (j in 1:12) {
    show_tree_showlist <- rep(show_tree_show, each = 12)
  }
  # The name of the dataframe should be changed to reflect the show you are working with.
  tree <- data.frame(show = show_tree_showlist,
                     shows_like_shows = shows_tree_like_chr)
  # This builds the collapisble tree
  collapsibleTree(tree, c("show", "shows_like_shows"),
                  root = paste0(show_name))
}

#######################################################################

# Top-Rated Episode
show_term <- function(x) {
  tt <- ifelse(
    str_detect(x, " "),
    paste0("https://www.imdb.com/find?q=", str_replace_all(x, " ", "+")),
    paste0("https://www.imdb.com/find?q=", x)
  ) %>%
    read_html() %>%
    html_nodes("#findSubHeader+ .findSection .result_text a") %>%
    html_attr("href") %>%
    .[[1]]
  tt <- paste0("https://www.imdb.com", tt)
  
  tt_top <- tt %>%
    read_html() %>%
    html_nodes("a+ .episode-container a") %>%
    html_text(trim = T) %>%
    .[1]
  
  tt_rate <- tt %>%
    read_html() %>%
    html_nodes("a+ .episode-container p+ .ipl-rating-star .ipl-rating-star__rating") %>%
    html_text() %>%
    as.numeric()
  
  tt_ssn_rate <- tt %>%
    read_html() %>%
    html_nodes("strong span") %>%
    html_text()
  
  tt_summary <- tt %>%
    read_html() %>%
    html_nodes(".summary_text")
  
  paste0("Highest Rated Episode: ", tt_top, "- ", tt_rate)
  paste0("Series Rating: ", tt_ssn_rate)
}

show_term("The Originals")
