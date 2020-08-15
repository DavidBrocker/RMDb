library(collapsibleTree)
library(htmlwidgets)
library(dplyr)
library(shinycssloaders)
library(stringr)
library(stringi)
library(shiny)
library(rvest)
library(purrr)
library(shinythemes)
library(ggthemes)
library(ggplot2)
library(plotly)
library(spotifyr)

# Set Environment
Sys.setenv(SPOTIFY_CLIENT_ID = '1f6c9bc7d5314065bb3467e9ff984084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b1b24f99d46541769013cf20539b191e')

access_token <- get_spotify_access_token()

# Define UI for application that draws a histogram
ui <- fluidPage(
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    x_artist <- eventReactive(input$search_artist, {
        input$artist
    })
    output$musictree <- renderCollapsibleTree({
        withProgress(message = 'Fetching URLS ', value = 0, {
        # Function to get similar artists
        get_similar_artists <- function(artist){
            art_st <- get_artist_audio_features(artist)
            related_artists <- get_related_artists(art_st$artist_id[1])
            related_artists <- related_artists %>% 
                select(name,id)
            related_artists
        }
        artist_parent <- get_similar_artists(x_artist())
        incProgress(amount = 1 / 2)
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
}


shinyApp(ui = ui, server = server)
