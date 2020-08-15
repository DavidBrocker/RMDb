# Special thanks to User nirgrahamuk and Hadley!
# Load packages
library(collapsibleTree)
library(htmlwidgets)
library(dplyr)
library(shinycssloaders)


ui <- fluidPage(
    titlePanel("IMDb: More Like This",windowTitle = "RMDb"),
    sidebarLayout(
        sidebarPanel(
            textInput("title", "Show Title", "Black Mirror"),
            br(),
        ),
        mainPanel(
            collapsibleTreeOutput("nText") %>% withSpinner(),
        ),
        position = c("left","right")
    )
)
server <- function(input, output){
    # Create function for text to render
    output$nText <- renderCollapsibleTree({ 
        withProgress(message = 'Fetching URLS ', value = 0, {
            # Get IMDb Search Term Link
            imdb_link <- ifelse(str_detect(input$title," "),
                   paste0("https://www.imdb.com/find?q=",str_replace_all(input$title," ","_")),
                   paste0("https://www.imdb.com/find?q=",input$title))
            # imdb_link <- show_term(title)
            imdb_tt <- imdb_link %>%
                read_html() %>%
                html_nodes("#findSubHeader+ .findSection .result_text a") %>%
                html_attr("href") %>%
                .[[1]]
            incProgress(amount=1/13)
           #  Final Link
            new_link <- paste0("https://www.imdb.com",imdb_tt)
        # Show Name for later
            show_name <-  new_link %>%
                read_html() %>%
                html_nodes("h1") %>%
                html_text()
        # This extracts the titles of the shows that are similar to the one you searched.
            show_tree_show <-new_link %>%
                read_html() %>% 
                html_nodes(".rec-title b") %>%
                html_text(trim=T)
        # This extracts the tt from the shows saved in the previous step
            show_morelike <-new_link %>%
                read_html() %>% 
                html_nodes(".rec_item,.rec_selected") %>%
                html_attr("data-tconst")
            # This makes a vector of all (12) of the URLs
            urls <- paste0("https://www.imdb.com/title/",show_morelike,"/")
            # Since we are working with 12 URLs we need to use the map function to apply this function over a list.
            doThis <- function(i){
                show_tree <- read_html(i) %>%
                    html_nodes(".rec-title b") %>%
                    html_text(trim=T)
                incProgress(amount=1/13)
                show_tree
            }
            # This runs the function with each URL in the vector
            shows_tree_like_chr <- unlist(map(urls,doThis))
            # This sets up the data how we need it by repeating each show title 12 times
            show_tree_showlist <- NULL
            for (i in 1:12){
                show_tree_showlist <- rep(show_tree_show,each=12)
            }
            # The name of the dataframe should be changed to reflect the show you are working with.
            show_tree <- data.frame(
                show=show_tree_showlist,
                shows_like_shows=shows_tree_like_chr
            )
        })
        collapsibleTree(
            show_tree,
            hierarchy=c("show","shows_like_shows"),
            root=paste0(show_name)
            )
        # End function
    })
}
shinyApp(ui, server)