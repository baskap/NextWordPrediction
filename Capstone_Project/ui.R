
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(h2("Data Science Capstone - Natural Language Prediction"),
  

      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(

          textInput("phrase", 
                   "Write input phrase",
                   "Predict next word"
          ),
       
         sliderInput("maxWordsCloud", 
           h5("Number of words in word cloud:"),  min = 0,  max = 50,  value = 25),
           hr(),
           h5("This application was built for"),
           h5("Data Science Capstone Project"),
           h5("by Barbara Poszewiecka")
        ),
    
        # Show a plot of the generated distribution
        mainPanel(
            h3("Predicted Words"),
            hr(),
            fixedRow(
              column(width = 6,
                     h4("You typed"),
                     wellPanel(textOutput('phrase')),
                     h4("Next word prediction:"),
                     wellPanel(textOutput('pred1'))),
              column(width = 6, 
                     h4("Other predictions:"),
                     wellPanel(span(h4(textOutput('pred2'))),
                               span(h4(textOutput('pred3'))),
                               span(h4(textOutput('pred4'))),
                               span(h4(textOutput('pred5')))
                     )
              )
            ),
            h3("Word Cloud"), 
            hr(),
            plotOutput("wordCloud"),
            h5("Visualization of most probable next word was done using word clouds plot.  This type of plot shows words in size proportional to theirs probability.")
        )  
      )
  )
)