library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Knowledge reminder based on a single word"),
  h4("Expand your horizon by word predictions"),
  hr(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h3("Please type a word"), value = "quantum"),
      helpText("hit enter or press the button"),
      submitButton("Predict"),
      hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      br(),
      h3("The relevant word is most likey to be:"),
      h1(textOutput("predicted"), align="center", style="color:blue"),
      hr(),
      # Create a spot for the barplot
      h3("Possible candidates:"),
      plotOutput("pred_plot")  
    )
  )
  ))