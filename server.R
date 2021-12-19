# Example: Shiny app that search Wikipedia web pages
# File: server.R 
library(shiny)
library(tm)
library(stringi)
library(proxy)
source("WikiSearch.R")
library(wordcloud)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({ 
    result <- SearchWiki(input$select)
   # plot(result, labels = input$select, sub = "",main="Wikipedia Search")
    freq <- colSums(as.matrix(result)) # Term frequencies
    ord <- order(freq, decreasing = TRUE) # Ordering the frequencies (ord contains the indices))
    top_50 <- freq[ord[1:50]] # Most frequent terms & their frequency (most frequent term "the" appearing 85 times)
    
    set.seed(123)
    wordcloud(names(top_50), freq, min.freq=5, colors=brewer.pal(6, "Dark2"))
    
  })
})
