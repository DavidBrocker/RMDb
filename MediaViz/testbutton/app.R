library(shiny)
library(stringr)
library(dplyr)
library(tidytext)


x <- data.frame(
    lines=1:length(words),
    words
)

x1 <- 
    x %>% 
    unnest_tokens(word,words) %>% 
    anti_join(stop_words) %>% 
    group_by(word) %>% 
    count(sort=T) %>% 
    ungroup() %>% 
    slice(1:10)

x2 <- 
    x %>% 
    unnest_tokens(word,words) %>% 
    group_by(word) %>% 
    count(sort=T) %>% 
    ungroup() %>% 
    slice(1:10)


ui <- fluidPage(
    radioButtons("radio",label=h3("Text Choices"),
                 choices = list("TRUE" = 1, "FALSE" = 2),
                 selected = 1),
    hr(),
    fluidRow(column(3, tableOutput("value")))
)

server <- function(input,output) {

    output$value <- renderPrint({
    if(input$radio==1){
        return(x1)
        }
        else{
            return(x2)
            }
    })
    

    
}

shinyApp(ui,server)