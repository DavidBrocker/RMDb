#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("PSY 101: Grade Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("quiz_1",
                         "Quiz 1",
                         5),
             numericInput("quiz_2",
                          "Quiz 2",
                          5),
             numericInput("quiz_3",
                          "Quiz 3",
                          5),
             numericInput("quiz_4",
                          "Quiz 4",
                          5),
             numericInput("quiz_5",
                          "Quiz 5",
                          5),
             numericInput("quiz_6",
                          "Quiz 6",
                          5),
             numericInput("quiz_7",
                          "Quiz 7",
                          5),
             numericInput("quiz_8",
                          "Quiz 8",
                          5),
             numericInput("quiz_9",
                          "Quiz 9",
                          5),
             numericInput("quiz_10",
                          "Quiz 10",
                          5),
             numericInput("exam_1",
                          "Exam 1",
                          100),
             numericInput("exam_2",
                          "Exam 2",
                          100),
             numericInput("exam_3",
                          "Exam 3",
                          100),
             numericInput("paper",
                          "Paper",
                          150),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot") %>% withSpinner()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        grades <- data.frame(
            items=c(
                "quiz_1",
                "quiz_2",
                "quiz_3",
                "quiz_4",
                "quiz_5",
                "quiz_6",
                "quiz_7",
                "quiz_8",
                "quiz_9",
                "quiz_10",
                "exam_1",
                "exam_2",
                "exam_3",
                "paper"
                ),
            grade=c(
                input$quiz_1,
                input$quiz_2,
                input$quiz_3,
                input$quiz_4,
                input$quiz_5,
                input$quiz_6,
                input$quiz_7,
                input$quiz_8,
                input$quiz_9,
                input$quiz_10,
                input$exam_1,
                input$exam_2,
                input$exam_3,
                input$paper
                    )
        )
        withProgress(message = 'Recalculating ', value = 0, {
        grades$m_grade <- sum(grades$grade)/500*100
        incProgress(1/1)
        ggplot(grades,aes(items,grade))+
            geom_point() +
            theme_classic() +
            geom_hline(aes(yintercept = m_grade,color="red"))
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
