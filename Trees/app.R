
# Load packages
library(collapsibleTree)
library(dplyr)
library(htmlwidgets)
library(ggplot2)
library(plotly)
library(purrr)
library(rvest)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(stringi)
library(stringr)
library(spotifyr)
# Set API Access for Spotify Section
# Set Environment
Sys.setenv(SPOTIFY_CLIENT_ID = '1f6c9bc7d5314065bb3467e9ff984084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b1b24f99d46541769013cf20539b191e')
access_token <- get_spotify_access_token()
# Setup UI
ui <-
    navbarPage(
        "RecommendMe",
        selected = "TV/Movies",
        collapsible = TRUE,
        inverse = TRUE,
        # First Tab: RMDb
        tabPanel(
            "TV/Movies",icon =icon("tv"),
            fluidPage(
                titlePanel("IMDb: More Like This",
                           windowTitle = "TV Recs"),
                p("A tool for you to find your next show!"),
                tags$a(href="https://www.imdb.com","All reccomendations sourced from IMDb."),
                br(),
                hr(),
                sidebarLayout(
                    sidebarPanel(
                        textInput(
                            inputId = "title",
                            label = "Name of Show",
                            placeholder = NULL,
                            width = "325px"
                        ),
                        actionButton("search_title",
                                     "Search Title",
                                     icon = icon("tv")),
                        br(),
                        br(),
                        textInput(
                            inputId = "tt",
                            label = "TT ID of Show",
                            placeholder = NULL,
                            width = "100px"
                        ),
                        actionButton("search_tt",
                                     "Search TT",
                                     icon = icon("tv")),
                        br(),
                        br(),
                        em("Note: This will take around 1 minute to process."),
                        br(),
                        br(),
                    ),
                    mainPanel(
                        collapsibleTreeOutput("nText") %>% withSpinner(),
                        collapsibleTreeOutput("nTText") %>% withSpinner(),
                    ),
                    position = c("left", "right")
                )
            )
        ),
    # # Second Tab: Good Treeads
    # tabPanel(
    #     "Books",icon=icon("book"),
    #     fluidPage(
    #         titlePanel("GoodReads: Readers Also Liked",
    #                    windowTitle = "BookRecs"),
    #         p("A tool for you to find your next book!"),
    #         tags$a(href="https://www.goodreads.com/","All reccomendations sourced from GoodReads."),
    #         br(),
    #         hr(),
    #         sidebarLayout(
    #             sidebarPanel(
    #                 textInput(
    #                     inputId = "book_title",
    #                     label = "Name of Book",
    #                     placeholder = NULL,
    #                     width = "325px"
    #                 ),
    #                 actionButton("search_book_title",
    #                              "Search Book Title",
    #                              icon = icon("book")),
    #                 br(),
    #                 br(),
    #                 em("Note: This will take around 1 minute to process."),
    #                 br(),
    #                 br(),
    #                 verbatimTextOutput("quote"),
    #             ),
    #             mainPanel(
    #                 collapsibleTreeOutput("book_text") %>% withSpinner(),
    #                       ),
    #             position = c("left", "right")
    #         ),
    #         tags$style(
    #             type = "text/css",
    #             "#value{ height: 100px; font-family: monospace;font-style:italic;background:#ffffff;}"
    #         ),
    #         textOutput("value")
    #     ),
    # ),
    # Third Panel: Games
    tabPanel(
        "Games",icon=icon("gamepad"),
        fluidPage(
            titlePanel("GiantBomb: Similar Games",
                       windowTitle = "GameTree"),
            sidebarLayout(
                sidebarPanel(
                    p("A tool for you to find your next game!"),
                    tags$a(href="https://www.giantbomb.com","All reccomendations sourced from giantbomb."),
                    br(),
                    hr(),
                    textInput(
                        inputId = "game_title",
                        label = "Name of Game",
                        placeholder = NULL,
                        width = "325px"
                    ),
                    actionButton("search_game",
                                 "Search Game",
                                 icon = icon("game")),
                    br(),
                    br(),
                    strong("Instructions: "),
                    br(),br(),
                    p("Try to make your search as specific as possible"),
                    p("For example: If you were to search the following:"),tags$a(href="https://www.giantbomb.com/skyrim/3035-684/","Skyrim"),
                    br(), br(),
                    p("You would most likley be accessing a link to the series"),
                    p("rather than the standalone game"),
                    tags$a(href="https://www.giantbomb.com/the-elder-scrolls-v-skyrim/3030-33394/","The Elder Scrolls V: Skyrim"),
                    br(),br(),
                    p("Also keep in mind that not every game will have suggestions!")
                ),
                mainPanel(
                    collapsibleTreeOutput("gametree") %>% withSpinner(type=7),
                ),
                position = c("left", "right")
            )
        )
    ),
    # Fourth Tab: Music
    tabPanel(
        "Music",icon=icon("music"),
        fluidPage(
            titlePanel("Spotify: Similar Artists",
                       windowTitle = "Music"),
            hr(),
            p("A tool for you to find your next favorite artist!"),
            tags$a(href="https://www.spotify.com","All reccomendations sourced from Spotify."),
            br(),
            hr(),
            
            sidebarLayout(
                sidebarPanel(
                    textInput("artist", "Enter Artist Name", placeholder = NULL),
                    actionButton("search_artist", "Search Artist"),
                    br(),br(),
                    em("Note: This will take around 30 seconds to process.")
                ),
                mainPanel(
                    collapsibleTreeOutput("musictree") %>% withSpinner()
                )
            )
        )
    )
)
server <- function(input, output) {
    ###########TV/Movie Recommendations############
    x_title <- eventReactive(input$search_title, {
        input$title
    })
    x_tt <- eventReactive(input$search_tt, {
        input$tt
    })
        # Create function for text to render
        output$nText <- renderCollapsibleTree({
            withProgress(message = 'Fetching URLS ', value = 0, {
                # Get IMDb Search Term Link
                imdb_link <-
                    ifelse(
                        str_detect(x_title(), " "),
                        paste0(
                            "https://www.imdb.com/find?q=",
                            str_replace_all(x_title(), " ", "+")
                        ),
                        paste0("https://www.imdb.com/find?q=", x_title())
                    )
                # imdb_link <- show_term(title)
                imdb_tt <- imdb_link %>%
                    read_html() %>%
                    html_nodes("#findSubHeader+ .findSection .result_text a") %>%
                    html_attr("href") %>%
                    .[[1]]
                incProgress(amount = 1 / 13)
                #  Final Link
                new_link <-
                    paste0("https://www.imdb.com", imdb_tt)
                # Show Name for later
                show_name <-  new_link %>%
                    read_html() %>%
                    html_nodes("h1") %>%
                    html_text()
                # This extracts the titles of the shows that are similar to the one you searched.
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
                urls <-
                    paste0("https://www.imdb.com/title/",
                           show_morelike,
                           "/")
                # Since we are working with 12 URLs we need to use the map function to apply this function over a list.
                doThis <- function(i) {
                    show_tree <- read_html(i) %>%
                        html_nodes(".rec-title b") %>%
                        html_text(trim = T)
                    incProgress(amount = 1 / 13)
                    show_tree
                }
                # This runs the function with each URL in the vector
                shows_tree_like_chr <-
                    unlist(map(urls, doThis))
                # This sets up the data how we need it by repeating each show title 12 times
                show_tree_showlist <- NULL
                for (i in 1:12) {
                    show_tree_showlist <- rep(show_tree_show, each = 12)
                }
                # The name of the dataframe should be changed to reflect the show you are working with.
                show_tree <-
                    data.frame(show = show_tree_showlist,
                               shows_like_shows = shows_tree_like_chr)
            })
            collapsibleTree(
                show_tree,
                hierarchy = c("show", "shows_like_shows"),
                root = paste0(show_name),
                fill=c(
                    "black",
                  rep("#B0B8B4FF",length(unique(show_tree$show))),
                  rep("#184A45FF",length(unique(paste(show_tree$show,show_tree$shows_like_shows))))
            )
        )
    })
    # Create function for text to render
    output$nTText <- renderCollapsibleTree({
        withProgress(message = 'Fetching URLS ', value = 0, {
            #  Final Link
            new_link <-
                paste0("https://www.imdb.com/title/", x_tt(), "/")
            # Show Name for later
            show_name <-  new_link %>%
                read_html() %>%
                html_nodes("h1") %>%
                html_text()
            # This extracts the titles of the shows that are similar to the one you searched.
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
            urls <-
                paste0("https://www.imdb.com/title/",
                       show_morelike,
                       "/")
            # Since we are working with 12 URLs we need to use the map function to apply this function over a list.
            doThis <- function(i) {
                show_tree <- read_html(i) %>%
                    html_nodes(".rec-title b") %>%
                    html_text(trim = T)
                incProgress(amount = 1 / 13)
                show_tree
            }
            # This runs the function with each URL in the vector
            shows_tree_like_chr <-
                unlist(map(urls, doThis))
            # This sets up the data how we need it by repeating each show title 12 times
            show_tree_showlist <- NULL
            for (i in 1:12) {
                show_tree_showlist <- rep(show_tree_show, each = 12)
            }
            # The name of the dataframe should be changed to reflect the show you are working with.
            show_tree <-
                data.frame(show = show_tree_showlist,
                           shows_like_shows = shows_tree_like_chr)
        })
        collapsibleTree(
            show_tree,
            hierarchy = c("show", "shows_like_shows"),
            root = paste0(show_name),
            fill=c(
                "black",
                rep("#B0B8B4FF",length(unique(show_tree$show))),
                rep("#184A45FF",length(unique(paste(show_tree$show,show_tree$shows_like_shows))))
        )
    )
})
######################Book Recommendations##################
#                           Need to Fix                    #
############################################################
    # ########### Search on Button Press #####################
    # x_book_title <- eventReactive(input$search_book_title, {
    #     input$book_title
    # })
    # # Create function for text to render
    # output$book_text <- renderCollapsibleTree({
    #     # Replace " " with "+"
    #     search_this <- function(x){
    #         x <- 
    #             ifelse(
    #                 str_detect(x, " "),
    #                 paste0("https://www.goodreads.com/search?q=",
    #                        str_replace_all(x, " ", "+")),
    #                 paste0("https://www.goodreads.com/search?q=", 
    #                        x)
    #             )
    #         x <- 
    #             x %>% 
    #             read_html() %>% 
    #             html_nodes("table a") %>% 
    #             html_attr("href") %>% 
    #             .[1]
    #         
    #         x <- paste0("https://www.goodreads.com",x)
    #         x
    #     }
    #     
    #     gr_link <- 
    #         search_this(x_book_title())
    #     withProgress(message = "Building Recommendations",value=0, {
    #         incProgress(1/4)
    #     book_name <-   
    #         gr_link %>% 
    #         read_html() %>% 
    #         html_nodes("#bookTitle") %>% 
    #         html_text(trim=T)
    #         incProgress(1/4)
    #     ##################FUNCTION NEW#####################
    #     get_gp_link <- function(url){
    #         gp_rec_links <- url %>% 
    #             read_html() %>% 
    #             html_nodes(".cover a") %>% 
    #             html_attr("href")
    #     } 
    #     ################FUNCTION NEW#####################
    #     get_p_rec_name <- function(urls){
    #         book_rec_names <- 
    #             urls %>% 
    #             read_html() %>% 
    #             html_nodes(".cover img") %>% 
    #             html_attr("alt")
    #         book_parent_title <- 
    #             urls %>% 
    #             read_html() %>% 
    #             html_nodes("#bookTitle") %>% 
    #             html_text(trim = T)
    #         data.frame(book_parent_title,
    #                    book_rec_names)
    #     }
    #     #
    #     new_links <- get_gp_link(gr_link)
    #     incProgress(1/4)
    #     book_tree <- map_df(new_links,get_p_rec_name)
    #     incProgress(1/4)
    #     
    #     
    #     #Create Tree
    #     collapsibleTree(
    #         book_tree,
    #         hierarchy = c("book_parent_title",
    #                       "book_rec_names"),
    #         root = paste0(book_name),
    #         # fill=c(
    #         #     "black",
    #         #     rep("grey",length(unique(book_tree$book_parent_title))),
    #         #     rep("maroon",length(unique(paste(book_tree$book_parent_title,book_tree$book_rec_names))))
    #         #     )
    #     )
    #     })
    # })
    # ############### Quotes ###########################
    # output$value <- renderText({
    #     gr_link <- ifelse(
    #         str_detect(x_book_title(), " "),
    #         paste0(
    #             "https://www.goodreads.com/search?q=",
    #             str_replace_all(x_book_title(), " ", "+")
    #         ),
    #         paste0("https://www.goodreads.com/search?q=", x_book_title())
    #     )
    #     # Get Link to Book
    #     gr_link <- gr_link %>%
    #         read_html() %>%
    #         html_nodes("table a") %>%
    #         html_attr("href") %>%
    #         .[1]
    #     # Final Link
    #     gr_link <- paste0("https://goodreads.com", gr_link)
    #     # Book Quote
    #     gr_link <- gr_link %>%
    #         read_html() %>%
    #         html_nodes(".rightContainer .readable") %>%
    #         html_text() %>%
    #         .[1]
    # })
    ################Spotify##############################
    # Make the search button reactive
    x_artist <- eventReactive(input$search_artist, {
        input$artist
    })
    output$musictree <- renderCollapsibleTree({
        withProgress(message = 'Fetching URLS ', value = 0, {
            # Function to get similar artists
            get_similar_artists <- function(artist){
                # Artist IDs are nested inside of the audio_features
                art_st <- get_artist_audio_features(artist)
                # Use the id variable to get related artists
                related_artists <- get_related_artists(art_st$artist_id[1])
                # Only keep two variables
                related_artists <- related_artists %>% 
                    select(name,id)
                related_artists
            }
            # Get the 20 recommendations from the input artist
            artist_parent <- get_similar_artists(x_artist())
            incProgress(amount = 1 / 2)
            # Get the 400 child recommendations from the previous 20 (20*20)
            artist_child <- map_df(artist_parent$id,get_related_artists)
            incProgress(amount = 1 / 2)
        })
        
        family_df <- data.frame(
            parent=rep(artist_parent$name,each=20),
            child=artist_child$name
        )
        collapsibleTree(
            family_df,
            hierarchy = c("parent","child"),
            root=paste0(x_artist()),
            fill=c(
                "black",
                rep("grey",length(unique(family_df$parent))),
                rep("maroon",length(unique(paste(family_df$parent,family_df$child))))
            )
        )
    })
    #################Video Games#####################
    x_game <- eventReactive(input$search_game, {
        input$game_title
    })
    
    output$gametree <- renderCollapsibleTree({
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
            str_detect(x_game(), " "),
            paste0(
                "https://www.giantbomb.com/search/?i=&q=",
                str_replace_all(x_game(), " ", "+")
            ),
            # Create search link
            paste0("https://www.giantbomb.com/search/?i=&q=", x_game())
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
    })
}
shinyApp(ui, server)
