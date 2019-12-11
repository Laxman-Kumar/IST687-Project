install.packages("wordcloud", dependencies = TRUE)
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes


library(wordcloud)
library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(tidyverse)


textualReviews <- fall %>%
  select(freeText) %>%
  drop_na(freeText) 

row.names(textualReviews) <- NULL

reviewsCorpus <- Corpus(VectorSource(textualReviews))
reviewsCorpus <- tm_map(reviewsCorpus,content_transformer(tolower))
reviewsCorpus <- tm_map(reviewsCorpus, removeNumbers)
reviewsCorpus <- tm_map(reviewsCorpus, removePunctuation)
reviewsCorpus <- tm_map(reviewsCorpus, removeWords,stopwords("English"))
reviewsCorpus <- tm_map(reviewsCorpus, stemDocument)

reviewMatrix <- TermDocumentMatrix(reviewsCorpus)

tdmMatrix <- as.matrix(reviewMatrix)
freq <- sort(rowSums(tdmMatrix),decreasing=TRUE)
tdmDat <- data.frame(word = names(freq),freq=freq)
rownames(tdmDat) <- NULL

wordcloud(tdmDat$word,tdmDat$freq,rot.per=.15,min.freq=15,random.order = FALSE,random.color = TRUE,colors = brewer.pal(8, "Dark2"))

