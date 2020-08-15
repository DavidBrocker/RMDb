library(shiny)
library(tidyverse)
library(stringr)
library(stringi)
library(rvest)
library(purrr)
library(collapsibleTree)
library(shinycssloaders)
library(dplyr)
library(forcats)

search_this <- function(x){
    # Fake a search query 
    x <- 
        ifelse(
            str_detect(x, " "),
            paste0("https://www.goodreads.com/search?q=",
                   str_replace_all(x, " ", "+")),
            paste0("https://www.goodreads.com/search?q=", 
                   x)
        )
    x <- 
        x %>% 
        read_html() %>% 
        html_nodes("table a") %>% 
        html_attr("href") %>% 
        .[1]
    # get link to book title
    x <- paste0("https://www.goodreads.com",x)
    x
}

##################FUNCTION NEW#####################
get_gp_link <- function(url){
    # get recommendation links
    gp_rec_links <- 
        url %>% 
        read_html() %>% 
        html_nodes(".cover a") %>% 
        html_attr("href")
} 
################FUNCTION NEW#####################
get_p_rec_name <- function(urls){
    # get names of recommendations
    book_rec_names <- 
        urls %>% 
        read_html() %>% 
        html_nodes(".cover img") %>% 
        html_attr("alt")
    book_parent_title <- 
    # get titles of parent books
        urls %>% 
        read_html() %>% 
        html_nodes("#bookTitle") %>% 
        html_text(trim = T)
    data.frame(book_parent_title,
               book_rec_names)
}
ui <- 
    fluidPage(
    titlePanel("GoodReads: Readers Also Liked",
               windowTitle = "BookRecs"),
    p("A tool for you to find your next book!"),
    tags$a(href="https://www.goodreads.com/","All reccomendations sourced from GoodReads."),
    br(),
    hr(),
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "book_title",
                label = "Name of Book",
                placeholder = NULL,
                width = "325px"
            ),
            actionButton("search_book_title",
                         "Search Book Title",
                         icon = icon("book")),
            br(),
            br(),
            em("Note: This will take around 1 minute to process."),
        ),
        mainPanel(
            collapsibleTreeOutput("book_text")
        ),
        position = c("left", "right")
    )
)
server <- function(input, output) {
######################Book Recommendations#################
x_book_title <- eventReactive(input$search_book_title, {
    input$book_title
})
# Create function for text to render
output$book_text <- renderCollapsibleTree({
    # Replace " " with "+"
    gr_link <- 
        search_this(x_book_title())
    book_name <-   
        gr_link %>% 
        read_html() %>% 
        html_nodes("#bookTitle") %>% 
        html_text(trim=T)

    #
    new_links <- get_gp_link(gr_link)
    book_tree <- map_df(new_links,get_p_rec_name)
    #Create Tree
    collapsibleTree(
        book_tree,
        hierarchy = c("book_parent_title",
                      "book_rec_names"),
        root = paste0(book_name)
    )
    })
}

shinyApp(ui, server)