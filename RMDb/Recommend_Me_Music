# Load packages ---------------------------------------------------------
library(collapsibleTree)
library(dplyr)
library(htmlwidgets)
library(ggplot2)
library(plotly)
library(purrr)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(spotifyr)
library(shinydashboard)
library(shinybrowser)
library(stringr)
library(purrr)
# Set Environment ---------------------------------------------------------
# Set API Access for Spotify Section
# Set Environment
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxxx')
access_token <- get_spotify_access_token()
# Artist Tree Function ----------------------------------------------------
artist_tree <- function(artist) {
  get_similar_artists <- function(artist) {
    artist_audfeat <-
      get_artist_audio_features(artist,
                                include_groups = c("album", "single"))
    related_artists <-
      get_related_artists(artist_audfeat$artist_id[1])
    related_artists <-
      related_artists |>
      select(name, id) |>
      slice(1:10)
  }
  artist_sim <- get_similar_artists(artist)
  artist_sim2 <- map_df(artist_sim$id, get_similar_artists)
  artist_dfT <-
    data.frame(parent = rep(artist_sim$name, each = 10),
               child = artist_sim2$name)
# Set Colors
  node_color <- c("darkgreen",
                  rep("maroon", 10),
                  rep("steelblue", 10 ^ 2))
# Build Tree  
  collapsibleTree(
    artist_dfT,
    hierarchy = c("parent","child"),
    root = artist,
    fill = node_color,
    fillByLevel = T,
    width = 1000,
    height = 500,
    zoomable = F,
    inputId = "node"
  )
}

# Overall Stats ---------------------------------------------------------------
artist_stats <- function(artist) {
  mb <-
    get_artist_audio_features(artist) |>
    summarize(
      Danceability = mean(danceability),
      Energy = mean(energy),
      Loudness = mean(loudness),
      Acousticness = mean(acousticness),
      Valence = mean(valence),
      Speechiness = mean(speechiness),
      Tempo = mean(tempo),
      Duration = mean(duration_ms) / 1000 / 60
    ) |>
    mutate_if(is.numeric, round, digits = 2)
}
# Most Common Key ---------------------------------------------------------
artist_key <- function(artist) {
  key <- get_artist_audio_features(artist)
  key <-
    key |>
    group_by(key_mode) |>
    count(sort = T) |>
    ungroup() |>
    slice(1) |>
    select(key_mode)
}

# Get Track ---------------------------------------------------------------
get_track <- function(artist) {
  track <- get_artist_audio_features(artist)
  track <-
    track |>
    select(external_urls.spotify) |>
    slice(1) |>
    str_extract("(?<=track\\/)(.*)")
}
# Get Genre ---------------------------------------------------------------
get_genre <- function(artist){
  get_artist_audio_features(artist) |> 
    select(artist_id) |> 
    unique() |> 
    get_artist() |> 
    purrr::pluck("genres") |> 
    sample(1)
}
# Get Followers -----------------------------------------------------------
get_followers <- function(artist){
  get_artist_audio_features(artist) |> 
    select(artist_id) |> 
    unique() |>
    get_artist() |> 
    pluck("followers") |> 
    pluck("total")
}
# UI ---------------------------------------------------------------------------
ui <-
  dashboardPage(
    skin = "green",
    dashboardHeader(titleWidth = "300px",
                    title = span("Recommend Me: Music",
                    tags$img(src = "https://www.citypng.com/public/uploads/small/11661570388xlqve2emckykh8duxvsgpvh7twc500yxmhrxeqceos5tlsy69cafnjapavvuls7qozpoi4rz8u97zecjlqnva0yy38a7xxuxbu2r.png",
                             height = '30', 
                             width ='30'))),
    dashboardSidebar(
      collapsed = FALSE,
      width = "300",
      sidebarMenu(
        sidebarSearchForm(
          textId = "artist",
          buttonId = "search_artist",
          label = "Enter Artist"),
        menuItem("Recommendation Tree", tabName = "main", icon = icon('music')),
        menuItemOutput("genre"),
        menuItemOutput("followers"),
        menuItem("Source Code", tabName = "other", icon = icon('code')))
    ),
    dashboardBody(
      tabItems(
        tabItem("main",
                fluidRow(
                  collapsibleTreeOutput("musictree")),
                fluidRow(
                  htmlOutput("frame"))),
        tabItem("other",
                fluidRow(
                  valueBoxOutput("tempo"),
                  valueBoxOutput("dance"),
                  valueBoxOutput("loudness"),
                  infoBoxOutput("key"),
                  valueBoxOutput("valence")))),
      ),
      shinybrowser::detect()
    )
# Server -----------------------------------------------------------------------
server <- function(input, output) {
  # Make the search button reactive
  x_artist <- eventReactive(input$search_artist, {
    input$artist
  })
  
  # Render Collapsible Tree
  output$musictree <- renderCollapsibleTree({
    artist_tree(x_artist())
  })
  
  # HTML iframe
  output$frame <- renderUI({
    track <- get_track(x_artist())
    tags$iframe(
      src = paste0(
        "https://open.spotify.com/embed/track/",
        track,
        "?utm_source=generator"
      ),
      seamless = "seamless",
      width = "373px",
      height = "90px",
      frameBorder = "none"
    )
  })
  
  # Genre
  output$genre <- renderMenu({
     gen <- get_genre(x_artist())
     sidebarMenu(
       menuItem(
         paste0("
                Genre: ", 
                gen), 
                icon = icon("list"))
     )
  })

# Followers
  output$followers <- renderMenu({
    follow <- get_followers(x_artist())
    sidebarMenu(
      menuItem(
        paste0(
          "Followers: ",
          follow),
        icon=icon("thumbs-up")
        )
      )
  })
  
  device_reactive <- reactive({
    if (as.numeric(shinybrowser::get_width()) <= 480) {
      "mobile"
    } else {
      "computer"
    }
  })
  
  output$more <-renderPrint(input$node)
}
shinyApp(ui, server)
