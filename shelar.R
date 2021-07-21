library(stringr)
library(dplyr)
library(stringi)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(NLP)
library(topicmodels)
library(tidytext)
library(reshape2)
library(ggplot2)
library(pals)

install.packages('dplyr')
install.packages('tidytext')
install.packages('pals')
install.packages('reshape2')
install.packages('tm')

extract <- function(filename) {
  dataset = read.delim(filename, 
                       header = FALSE, 
                       sep = '\t', 
                       quote = '', 
                       dec = ' ', 
                       stringsAsFactors = FALSE)
  
  dataset <- as.data.frame(dataset)
  row_index <- seq_len(nrow(dataset)) %% 2
  details <- dataset[row_index == 1,]
  abstract <- dataset[row_index == 0,]
  abstract <- as.data.frame(abstract)
  details <- as.data.frame(details)
  
  doi <- extract_doi(details)
  author <- extract_author(details)
  title <- extract_title(details)
  
  article <- data.frame(doi, title, author, abstract)
  colnames(article) <- c("DOI Link", "Title", "Author", "Abstract")
  
  return(article)
}
extract('citations1.txt')

extract_doi <- function(details_col){
  pattern_doi <- "(\\w+):\\s(\\d+)[^ab  c](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
  doi <- str_match(details_col[, 1],pattern_doi)
  doi <- as.data.frame(doi[, 1])
  return(doi)
  
}
#extract_doi()

extract_author <- function(details_col) {
  pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"
  author_name <- str_match(details_col[,1], pattern_author)
  author <- as.data.frame(author_name[, 1])
  return(author)
}

extract_title <- function(details_col){
  pattern_title <- "(?<=\")(.*?)(?=\")"
  title <- str_match(details_col[, 1], pattern_title)
  title <- as.data.frame(title[, 1])
  return(title)
  
}

dfn <-extract('citations1.txt')

load("data_common_words.RData")

calculateDTM <- function(dataframe) {
  Corpus <- VCorpus(VectorSource(dataframe))   
  toSpace <- content_transformer(
    function (x, pattern)
      gsub(pattern, " ", x))
  processedCorpus <- tm_map(Corpus, toSpace, "/")
  processedCorpus <- tm_map(Corpus, toSpace, "@")
  processedCorpus <- tm_map(Corpus, toSpace, "#")
  
  
  processedCorpus <- tm_map(processedCorpus, content_transformer(tolower))
  processedCorpus <- tm_map(processedCorpus, removeNumbers)
  processedCorpus <- tm_map(processedCorpus, removeWords, stopwords("english"))
  processedCorpus <- tm_map(processedCorpus, removeWords, data_common_words)
  processedCorpus <- tm_map(processedCorpus, stripWhitespace)
  
  
  dtm <- TermDocumentMatrix(processedCorpus)
  return(dtm)
}

-------------------------------------------------------------------------

dtm <- calculateDTM(dfn$abstract)

VCorpus <- VCorpus(VectorSource(df$Abstract))   
VCorpus <- Corpus(DataframeSource(df$Abstract))
processedCorpus <- tm_map(VCorpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes <- TRUE)
processedCorpus <- tm_map(processedCorpus, removeWords, stopwords("english"))
processedCorpus <- tm_map(processedCorpus, removeWords, data_common_words)
processedCorpus <- tm_map(processedCorpus, stripWhitespace)
minimumFrequency <- 5
---------------------------------------------------------------
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(DTM)


sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

K <- 10
topicModel <- LDA(DTM, K)


tmResult <- posterior(topicModel)
attributes(tmResult)
nTerms(DTM)   
betas <- tmResult$terms  
dim(beta)  
thetas <- tmResult$topics 

rowSums(theta)[1:10] 
terms(topicModel, 10)

topics <- tidy(topicModel, matrix = "beta")
topics

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()  


------------------------------------------------
  source_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  #join to the tidy form to get the genre field
  inner_join(source_tidy, by = "document") %>%
  select(genre, topic, gamma) %>%
  group_by(genre, topic) %>%
  #avg gamma (document) probability per genre/topic
  mutate(mean = mean(gamma)) %>%
  select(genre, topic, mean) %>%
  ungroup() %>%
  #re-label topics
  mutate(topic = paste("Topic", topic, sep = " ")) %>%
  distinct()

circos.clear() #very important! Reset the circular layout parameters
#this is the long form of grid.col just to show you what I'm doing
#you can also assign the genre names individual colors as well
grid.col = c("Topic 1" = "grey", "Topic 2" = "grey", "Topic 3" = "grey",
             "Topic 4" = "grey", "Topic 5" = "grey", "Topic 6" = "grey",
             "Topic 7" = "grey", "Topic 8" = "grey")

#set the gap size between top and bottom halves set gap size to 15
circos.par(gap.after = c(rep(5, length(unique(source_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(source_topic_relationship[[2]])) - 1), 15))
chordDiagram(source_topic_relationship,  grid.col = grid.col, annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(source_topic_relationship))))))
#go back to the first track and customize sector labels
#use niceFacing to pivot the label names to be perpendicular
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important
title("Relationship Between Topic and Genre")
