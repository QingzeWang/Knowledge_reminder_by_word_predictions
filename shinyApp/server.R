library(shiny)
library(RSQLite)
library(stringr)

source('predict.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # input$text and input$action are available
  # output$sentence and output$predicted should be made available
  db <- dbConnect(SQLite(), dbname="train.db")
  dbout <- reactive({ngram_backoff(input$text, db)})
  
  output$sentence <- renderText({input$text})
  output$predicted <- renderText({
    out <- dbout()
    return(unlist(out)[1])
    })
  output$otherp <- renderText({
    out <- dbout()
    return(paste(unlist(out)[2:5],","))
    })
  output$pred_plot <- renderPlot({
    out <- dbout()
    freq = unlist(out[[2]])
    names(freq) <- unlist(strsplit(out[[1]]," "))
#     out2 <- data.table(word = names(freq),freq = freq)
#     g <- ggplot(out2, aes(x= word, y= freq))+ geom_bar()
#     g <- g + ylab("Frequency") + ggtitle("Adjusted word frequency")
#     print(g)
    barplot(freq,main="Adjusted word frequency",ylab="Frequency", col=("Green"))
  })
})