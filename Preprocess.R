
# Loading all necessery libraries
library(NLP)
library(openNLP)
library(RWeka)
library(tm)
library(qdap)
library(wordcloud)
library(knitr)
library(htmlTable)
library(stringi)
library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)
library(data.table)

set.seed(123456)

setwd('C:/Users/Basia/Statistics/CAPSTONE/')

readLinesFromFile <- function(path) {
    conn  <- file(path, "rb")
    lines <- readLines(conn, encoding="UTF-8", skipNul = TRUE) 
    close(conn)
    lines
}

blogs <- readLinesFromFile("final/en_US/en_US.blogs.txt") 
news <- readLinesFromFile("final/en_US/en_US.news.txt")
tweets <- readLinesFromFile("final/en_US/en_US.twitter.txt")

texts <- c(blogs, news, tweets)

texts <- sample(texts, 0.1 * length(texts))

preprocessTexts <- function(texts) {
  texts <- iconv(texts, "latin1", "ASCII", sub=" ")
  corpus <- Corpus(VectorSource(texts), readerControl = list(language="en"))
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  
  corpus <- tm_map(corpus, stripWhitespace)

  corpus_df <- data.frame(text = lapply(corpus, '[',"content"), stringsAsFactors = FALSE)
  corpus_df <- sapply(corpus_df , "[[", 1)
  
  return(corpus_df)
}

corpus_df <- preprocessTexts(texts)

ng <- function(n, corpus_df) {
  
  dt <- matrix(nrow = 0, ncol = n)
  
  w <-  matrix(nrow = 0, ncol = n)
  
  l <- 1
  
  for (i in seq(length(corpus_df))) {
  
      sp <- strsplit(corpus_df[[i]], ' ')[[1]]

      if (i %% 500 == 0) {
        dt <- rbind(dt, w)
        w <-  matrix(nrow = 0, ncol = n)
        
      }
      
      if (length(sp) > n) {
        for (j in seq(length(sp) - n )) {
          
          w <- rbind(w, sp[seq(j,j + n - 1)])

          l <- l + 1
        }
        
      }
      print(paste0(i,' ',n))
  }
  
  colnames(dt) <- c(paste0("w", c(1:n)))
  
  dt <- as.data.frame(dt, stringAsFactor = FALSE)

  if (n == 1) dt <- dt %>% group_by(w1) %>% summarise(freq = n())
  if (n == 2) dt <- dt %>% group_by(w1, w2) %>% summarise(freq = n())
  if (n == 3) dt <- dt %>% group_by(w1, w2, w3) %>% summarise(freq = n())
  if (n == 4) dt <- dt %>% group_by(w1, w2, w3, w4) %>% summarise(freq = n())
  
  #dt <-  dt[order(dt$freq,decreasing = TRUE),]
  dt <- as.data.table(dt)
  
  if (n == 1) setkey(dt, w1)
  if (n == 2) setkey(dt, w1)
  if (n == 3) setkey(dt, w1, w2)
  if (n == 4) setkey(dt, w1, w2, w3)
  
  dt
}

n1GramTable <- ng(1, corpus_df)
n2GramTable <- ng(2, corpus_df)
n3GramTable <- ng(3, corpus_df)
n4GramTable <- ng(4, corpus_df)

nGrams <- list()

nGrams[[1]] <- n1GramTable
nGrams[[2]] <- n2GramTable
nGrams[[3]] <- n3GramTable
nGrams[[4]] <- n4GramTable

saveRDS(nGrams, "nGrams.rds")