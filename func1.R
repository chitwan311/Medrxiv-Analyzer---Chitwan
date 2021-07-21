
library(stringr)
library(dplyr)
#install.packages('stringi')
library(stringi)

read_dataset <- function(filename){
dataset = read.delim(filename, 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

dataset <- as.data.frame(dataset)
odd_row <- seq_len(nrow(dataset)) %% 2
details <- dataset[odd_row == 1,]
abstract <- dataset[odd_row == 0,]
abstract <- as.data.frame(abstract)
details <<- as.data.frame(details)

#abstract <- abstract_fun(details)
#doi <- doi_fun(details)
#author <- author_fun(details)
#title <- title_fun(details)

article <- data.frame(doi, title, author, abstract)
colnames(article) <- c("DOI Link", "Title", "Author", "Abstract")

return(article)
}

#read_dataset('citations1.txt')

-------------------------------------------------------------
-----------------------------------------------------

doi_fun <- function(details){
pattern_doi <- "(\\w+):\\s(\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
doi <- str_match(details[, 1], pattern_doi)
doi <- as.data.frame(doi[, 1])
return(doi)
}
#doi_fun()

--------------------------------------------------------------------
  
author_fun <- function(details_col){
#(\\w+)\\s(\\w+)\\s(\\w+)\\s[a-z][^abc]"
pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"
#pattern_author <- c("(\\w+)\\s(\\w+)\\s", "al$")
author_name <- str_match(details_col[,1], pattern_author)
#author_name <- str_match(dataset[,1], regex("[a-z]", multiline = TRUE))
author <- as.data.frame(author_name[, 1])
return(author)
}
#uthor_fun()
---------------------------------------------------------------------

title_fun <- function(details_col){
pattern_title <- "(?<=\")(.*?)(?=\")"
title <- str_match(details_col[, 1], pattern_title)
title <- as.data.frame(title[, 1])
return(title)
}
#title_fun()

read_dataset('citations1.txt')
---------------------------------------------------------------------


----------------------------------------------------------------------
...........................................................

#install.packages("tm")           # for text mining
#install.packages("SnowballC")    # for text stemming
#install.packages("wordcloud")    # word-cloud generator
#install.packages("RColorBrewer") # color palettes

abstract_scraped <- function(x, npar=TRUE, print=TRUE){
  
# Load the packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# To choose the text file
#text = readLines(file.choose())

# VectorSource() function 
# creates a corpus of 
# character vectors
docs <<- VCorpus(VectorSource(article_df$Abstract))   

# Text transformation
toSpace <<- content_transformer(
  function (x, pattern)
    gsub(pattern, " ", x))
docs1 <<- tm_map(docs, toSpace, "/")
docs1 <<- tm_map(docs, toSpace, "@")
docs1 <<- tm_map(docs, toSpace, "#")
abstract_list <<- data.frame(strwrap(docs1))
return(abstract_list)
}
abstract_scraped()

common_words <- load("data_common_words.RData")
# Cleaning the Text
docs1 = tm_map(docs1, content_transformer(tolower))
docs1 = tm_map(docs1, removeNumbers)
docs1 = tm_map(docs1, removeWords, stopwords("english"))
docs1 = tm_map(docs1, removeWords, common_words)
docs1 = tm_map(docs1, stripWhitespace)

# Build a term-document matrix
dtm <<- TermDocumentMatrix(docs1)
m <<- as.matrix(dtm)
v <<- sort(rowSums(m), 
         decreasing = TRUE)
d <<- data.frame(word = names(v),
               freq = v)
return(head(d, 10))
#return(d)
}
head(d, 10)
word_matrix()

d2 <- data.frame(d)
--------------------------------------------------------------------

# Generate the Word cloud
wordcld <- function(x, npar=TRUE, print=TRUE){
wordcloud(words = d$word, 
          freq = d$freq,
          min.freq = 1, 
          max.words = 250,
          random.order = FALSE, 
          rot.per = 0.3, 
          colors = brewer.pal(8, "Dark2"))
}
wordcld()
-----------------------------------------------------------------------

pieplot <- function(x, npar=TRUE, print=TRUE){
library(ggplot2)
ggplot(data = d[1:20, ], aes(x=d[1:20,]$word, y=d[1:20,]$freq, fill=d[1:20, ]$word))+
  geom_bar(position ='dodge', stat="identity")+
  theme_minimal()+
  labs(fill="Words")+
  ggtitle('Top 20 Most frequent words')+
  coord_polar()+
  xlab('Words')+
  ylab('Frequency')
}
pieplot()

#install.packages("ggpubr")
library(ggpubr)
ggplot(d[1:20, ], aes(x=d[1:20,]$word, y=d[1:20, ]$freq)) +
  geom_linerange(
    aes(x = d[1:20,]$word, ymin = 0, ymax = 10, color = d[1:20,]$word), 
    size = 3)+
  theme_minimal()+
  ggtitle("Top 20 most frequent words")+
  coord_flip()+
  labs(color=element_blank())+
  geom_point(aes(size = 8, color=d[1:20,]$word))+
  xlab('Words')+
  ylab('Frequency')

#medical subject heading (mesh) -> apply filter for word cloud


