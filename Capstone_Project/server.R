
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library(data.table)
library(tm)
library(sqldf)


preprocessTexts <- function(texts) {
  texts <- iconv(texts, "latin1", "ASCII", sub=" ")
  corpus <- Corpus(VectorSource(texts), readerControl = list(language="en"))
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  corpus_df <- data.frame(text = lapply(corpus, '[',"content"), stringsAsFactors = FALSE)
  corpus_df <- sapply(corpus_df , "[[", 1)
}

predictNGram <- function(lastNGram, nGramTable, n) {
  
  if (n == 1) dt <- nGramTable[J(lastNGram[1])]
  if (n == 2) dt <- nGramTable[J(lastNGram[1])]
  if (n == 3) dt <- nGramTable[J(lastNGram[1], lastNGram[2])]
  if (n == 4) dt <- nGramTable[J(lastNGram[1], lastNGram[2], lastNGram[3])]

  dt$n <- n
  dt <- dt[order(freq,decreasing=TRUE),]
  dt <- as.data.frame(dt)[, seq(n, n + 2)]
  colnames(dt) <- c("word", "freq", "n")

  if (is.na(dt[1,"freq"])) return(NULL)
  else return(dt)
}

predictNextWord <- function(input, nGrams){
  input_length <- length(input)
  i <- min(4, input_length + 1)

  dt <- matrix(nrow = 0, ncol = 3)

  while (i > 1) {

    prediction <- predictNGram(input[seq( input_length - i + 2 , input_length)], nGrams[[i]], i)

    if (!is.null(prediction)) {
      dt <- rbind(dt, prediction)
    }

    i <- i - 1
  }

  dt1 <- nGrams[[1]]
  dt1 <- dt1[order(freq, decreasing=TRUE),][1:200]
  dt1$n <- 1
  colnames(dt1) <- c("word", "freq", "n")
  dt1 <- as.data.frame(dt1)

  dt <- rbind(dt, dt1)[1:100,]

  dt <- sqldf('select word, max(n) n, (select max(freq) from dt dtf where  dtf.word = dt.word and dtf.n = max(dt.n)
              
              ) freq from dt group by word order by 2 desc, 3 desc')
  
  return (dt)
}  

wordCloudGen <- function(nGramTable, n) {
  colors <- brewer.pal(11, "PiYG")
  colors <- rep(colors, 20)
  
  n <-  min(n, nrow(nGramTable))
  
  wordcloud(words = nGramTable[1:n, "word"], 
            freq = nGramTable[1:n, "freq"] *  100 ** nGramTable[1:n, "n"] , 
            scale = c(5, 1), 
            colors = colors[1:n],
            ordered.colors = TRUE,
            min.freq = 1)              
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  nGrams <- readRDS("nGrams.rds")
   
  predicts <- function(text) {
    prepr_text <- preprocessTexts(text)
    input <-  strsplit(prepr_text, ' ')[[1]]
    return(predictNextWord(input,  nGrams))
  }
  
  output$phrase <- renderText({
    input$phrase
  })
  
  preds <- reactive(predicts(input$phrase))
  
  output$pred1 = renderText(paste(preds()[1, "word"]))
  output$pred2 = renderText(paste("Rank 2: ", preds()[2, "word"]))
  output$pred3 = renderText(paste("Rank 3: ", preds()[3, "word"]))
  output$pred4 = renderText(paste("Rank 4: ", preds()[4, "word"]))
  output$pred5 = renderText(paste("Rank 5: ", preds()[5, "word"]))
  
  output$wordCloud <- renderPlot({
      wordCloudGen(preds(), input$maxWordsCloud)
  })
  
})


