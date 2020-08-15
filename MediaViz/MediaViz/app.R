library(tidyverse)
library(genius)
library(stringr)
library(ggplot2)
library(rvest)
library(plotly)
library(tidytext)
library(forcats)
library(shiny)
library(shinycssloaders)

top_words_song <- function(artist,song){
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

top_words_album <- function(artist,song){
    lyr <- genius_lyrics(artist=artist,album=album)
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
             title=paste0("Top Words in ",album, " by ", artist))
}
    
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
            coord_polar(theta="y") + 
            xlim(c(2, 4)) + 
            theme_void() +
            geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
            scale_fill_manual(values=c("#C71E1D","#6BCBB6"))
}



ui <- navbarPage(
    "MediaViz",
    selected = "Lyrics",
    collapsible = TRUE,
    inverse = TRUE,
    # First Tab: RMDb
    tabPanel(
        "Lyrics",icon =icon("music"),
        fluidPage(
            # Application title
            titlePanel("Lyrics Visualizer"),
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    strong("Note:"),
                    br(),
                    p("Format your search as follows:"),
                    em("Song by Artist"),
                    br(),br(),
                    textInput("song",
                              "Song",
                              placeholder = NULL),
                    actionButton("search_song","Search Song"),
                    #actionButton("search_album","Search Album"),
                    br(),br(),
                    textInput("sent",
                              "Sentiment",
                              placeholder = NULL),
                    actionButton("search_sentiment","Search Sentiment")

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("wordPlot") %>% withSpinner(type=5),
           plotOutput("sentPlot") %>% withSpinner(type=5)
        )
    )
    )
    ),
    tabPanel("TV Tiles",icon=icon("tv"),
             fluidPage(
                 titlePanel("TV Tiles", windowTitle = "TV Tiles"),
                 hr(),
                 p("Generate a tile view of your favorite show"),
                 p("Note: This will currently",
                   em("only"),
                   "work for shows that have ended!"),
                 hr(),
                 sidebarLayout(
                     sidebarPanel(
                         textInput("show", "Enter Show Name", placeholder = NULL),
                         actionButton("search", "Search")
                     ),
                     mainPanel(plotlyOutput("show_tile") %>% withSpinner(type = 4))
                 )
                 )
             )
)
# Define server logic required to draw lyric plot
server <- function(input, output) {
    ############### Song Details ####################
     x_song <- eventReactive(input$search_song, {
        input$song
    })
    output$wordPlot <- renderPlot({
        withProgress(message = 'Fetching URLS ', value = 0, {
        splt <- strsplit(x_song(),"by")
        
        incProgress(1/1)
        })
        top_words_song(splt[[1]][2],splt[[1]][1])

    })
    
#####################Search Album##############################
#                                                            #
##############################################################  
    # x_album <- eventReactive(input$search_album, {
    #     input$song
    # })
    # 
    # output$wordPlot <- renderPlot({
    #     splt <- strsplit(x_album(),"by")
    #     top_words_album(splt[[1]][2],splt[[1]][1])
    #     
    # })
    
    ############### Sentiment Information ################
    x_sent <- eventReactive(input$search_sentiment, {
        input$sent
    })
    
    output$sentPlot <- renderPlot({
        splt <- strsplit(x_sent(),"by")
        overall_sent(splt[[1]][2],splt[[1]][1])
    })

    
#######################TV Tiles################################
#                                                             #        
############################################################### 
       # Make button press launch plot
    x_tile <- eventReactive(input$search, {
        input$show
    })
    output$show_tile <- renderPlotly({
        # Search IMDb with input, replace " " with "+"   
        tile <- ifelse(
            str_detect(x_tile(), " "),
            paste0(
                "https://www.imdb.com/find?q=",
                str_replace_all(x_tile(), " ", "+")
            ),
            paste0("https://www.imdb.com/find?q=", x_tile())
        )
        # Get the /tt##### of the show
        imdb_tt <- tile %>%
            read_html() %>%
            html_nodes("#findSubHeader+ .findSection .result_text a") %>%
            html_attr("href") %>%
            .[[1]]
        # Final Link
        new_link <- paste0("https://www.imdb.com", imdb_tt)
        # Get Name
        show_name <- new_link %>%
            read_html() %>%
            html_nodes("h1") %>%
            html_text(trim = T)
        # Isolate tt
        tt_isolate <- unlist(str_match(imdb_tt,
                                       "(?<=\\/)(tt[0-9]+)(?<!\\/)"))
        # Regular Expression keeps two entries, keep the first
        tt_isolate <- tt_isolate[[1]][1]
        
        # Get the number of seasons and make numeric
        rating_show_url <- new_link
        num_seasons <- rating_show_url %>%
            read_html() %>%
            html_nodes(".clear+ div a:nth-child(1)") %>%
            html_text(trim = T) %>%
            as.numeric()
        # Get the URL's for each season
        rating_season_urls <-
            paste0(
                "https://www.imdb.com/title/",
                tt_isolate,
                "/episodes?season=",
                1:num_seasons
            )
        # Function for getting titles
        get_episode_titles <- function(urls) {
            urls %>%
                read_html() %>%
                html_nodes("#episodes_content strong a") %>%
                html_text(trim = T)
        }
        # Apply the function to all of the URLs
        episode_titles <-
            unlist(map(rating_season_urls, get_episode_titles))
        # Function for getting ratings
        get_episode_ratings <- function(urls) {
            urls %>%
                read_html() %>%
                html_nodes(".ipl-rating-star.small .ipl-rating-star__rating") %>%
                html_text(trim = T) %>%
                as.numeric()
        }
        # Apply the function to all of the URL's
        episode_ratings <-
            unlist(map(rating_season_urls, get_episode_ratings))
        # Function for getting the Season Number
        get_season <- function(urls) {
            urls %>%
                read_html() %>%
                html_nodes("#episode_top") %>%
                html_text(trim = T)
        }
        # Apply the function to all of the urls
        season <- unlist(map(rating_season_urls, get_season))
        season <- unlist(str_remove_all(season, "Season|\\s")) %>%
            as.numeric()
        # Function for getting the episode number
        get_episode_number <- function(urls) {
            urls %>%
                read_html() %>%
                html_nodes(".zero-z-index div") %>%
                html_text(trim = T)
        }
        # Apply the function to all of the URLs
        season_ep_rating <-
            unlist(map(rating_season_urls, get_episode_number))
        # Remove commas
        szn <- unlist(strsplit(season_ep_rating, ","))
        # Remove everything in betwen 'S' and any numbers
        szn_list <-
            as.numeric(unlist(str_match_all(szn, "(?<=S)(\\d+)")))
        # Remove everything in between 'Ep' and any numbers
        ep_list <-
            as.numeric(unlist(str_match_all(szn, "(?<=Ep)(\\d+)")))
        # Match any instance of 'S#'
        season_num <- str_match(season_ep_rating, "S\\d+")
        # Remove 'S'
        season_num <- str_remove_all(season_num, "S")
        # Convert to numeric
        season_num <- as.numeric(season_num)
        # Match any instance of 'Ep##'
        episode_num <- str_match(season_ep_rating, "Ep\\d+")
        # Remove 'Ep' and make numeric
        episode_num <- as.numeric(str_remove_all(episode_num, "Ep"))
        # Combine all of this into a dataframe
        show_df <- data.frame(
            season_num,
            episode = episode_num,
            titles = episode_titles,
            ratings = episode_ratings
        )
        # Add quality column and text column
        show_df <- show_df %>%
            mutate(
                quality = case_when(
                    ratings < 5.0 ~ "Garbage",
                    ratings < 6.5 ~ "Bad",
                    ratings < 7.5 ~ "Regular",
                    ratings < 8.5 ~ "Good",
                    ratings < 10 ~ "Great"
                ),
                text = paste0(
                    "Season: ",
                    season_num,
                    "\n",
                    "Episode: ",
                    episode,
                    "\n",
                    "Title: ",
                    titles
                )
            )
        # Add coloring scheme
        cols = c(
            "Bad" = "red2",
            "Garbage" = "dodgerblue2",
            "Great" = "greenyellow",
            "Regular" = "darkorange1",
            "Good" = "gold1"
        )
        # Plot 
        show_df_plot <-
            ggplot(show_df,
                   aes(
                       # Make season a factor
                       as.factor(season_num),
                       # Make episode number a factor
                       as.factor(episode),
                       # Hover-over text 
                       text = text
                   )) +
            # Use the tile mapping, size for uniformity
            geom_tile(aes(fill=quality,size=1,width=.9,height=.9))+
            # Have the tiles 'filled' with the rating
            geom_text(aes(label = ratings)) +
            # Apply colors to the fill argument
            scale_fill_manual(values = cols) +
            # Nice theme
            theme_classic() +
            theme(
                # Remove axis ticks
                axis.ticks = element_blank(),
                # Remove legend title
                legend.title = element_blank(),
                # Try to remove plot border
                legend.background = element_rect(fill = NULL, linetype = "blank"),
                # Format caption
                plot.caption = element_text(face = "italic")
            ) +
            # Axis titles, title, caption
            labs(
                x = "Season",
                y = "Episode Number",
                title = paste0("Series Ratings for ", show_name),
                caption = "All information sourced from imdb.com"
            )
        # Plotly call
        ggplotly(show_df_plot, tooltip = "text")
        
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
