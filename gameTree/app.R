#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("GoodReads: Readers Also Liked",
               windowTitle = "BookRecs"),
    p("A tool for you to find your next book!"),
    tags$a(href = "https://www.goodreads.com/", "All reccomendations sourced from GoodReads."),
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
            br(),
            br(),
            verbatimTextOutput("quote"),
        ),
        mainPanel(collapsibleTreeOutput("book_text") %>% withSpinner(),),
        position = c("left", "right")
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    ########Book Recommendations#######################
    x_book_title <- eventReactive(input$search_book_title, {
        input$book_title
    })
    # Create function for text to render
    output$book_text <- renderCollapsibleTree({
        # Replace " " with "+"
        gr_link <- ifelse(
            str_detect(x_book_title(), " "),
            paste0(
                "https://www.goodreads.com/search?q=",
                str_replace_all(x_book_title(), " ", "+")
            ),
            paste0("https://www.goodreads.com/search?q=", x_book_title())
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
        # Get 324 child recommendations
        recs <- unlist(map(rec_books, get_all_recs))
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
