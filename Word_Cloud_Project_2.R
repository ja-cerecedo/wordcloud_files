library(shiny)
library(wordcloud2)
library(tm)
library(colourpicker)

ui <- fluidPage(
  h1("Word Cloud"),
  tabsetPanel(
    tabPanel(
      title = "Word cloud",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "source",
            label = "Word source",
            choices = c(
              "Upload a file" = "file"
            )
          ),
          hr(),
          conditionalPanel(
            condition = "input.source == 'file'",
            fileInput("file", "Select a file")
          ),
          hr(),
          numericInput("num", "Maximum number of words",
                       value = 100, min = 5
          ),
          hr(),
          colourInput("col", "Background color", value = "white"),
          hr(),
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          br(),
          br()
        )
      )
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  create_wordcloud <- function(data, num_words = 100, background = "white") {

    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }

    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }

    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background)
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
}

shinyApp(ui = ui, server = server)