library(shiny)
# install.packages("wordcloud2")
library(wordcloud2)
# install.packages("tm")
library(tm)
# install.packages("colourpicker")
library(colourpicker)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- read.csv("ihaveadream.csv",
                       sep = "&",
                       stringsAsFactors = FALSE
      )
      data <- data[, 1]
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
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
    
    # If text is provided, convert it to a dataframe of word frequencies
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
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
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