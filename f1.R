dataset = read.delim('citations1.txt', 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

library(stringr)
library(dplyr)
#install.packages('stringi')
library(stringi)

dataset2 <- as.data.frame(dataset)
odd_row <- seq_len(nrow(dataset2)) %% 2
odd_col <- dataset2[odd_row == 1,]
abstract1 <- dataset2[odd_row == 0,]
abstract <- as.data.frame(abstract1)
odd <- as.data.frame(odd_col)
View(odd)
View(abstract)


pattern_doi <- "(\\w+):\\s(\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
doi <- str_match(odd[, 1],pattern_doi)
doi2 <- as.data.frame(doi[, 1])
View(doi2)


#(\\w+)\\s(\\w+)\\s(\\w+)\\s[a-z][^abc]"
pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"
#pattern_author <- c("(\\w+)\\s(\\w+)\\s", "al$")
author_name <- str_match(odd[,1], pattern_author)
#author_name <- str_match(dataset[,1], regex("[a-z]", multiline = TRUE))
author <- as.data.frame(author_name[, 1])
View(author)

pattern_title <- "(?<=\")(.*?)(?=\")"
title <- str_match(odd[, 1], pattern_title)
title <- as.data.frame(title[, 1])
View(title)


article <- data.frame(doi2, title, author, abstract)
colnames(article) <- c("DOI Link", "Title", "Author", "Abstract")
View(article)

...........................................................
#install.packages("tm")           # for text mining
#install.packages("SnowballC")    # for text stemming
#install.packages("wordcloud")    # word-cloud generator
#install.packages("RColorBrewer") # color palettes

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
docs = VCorpus(VectorSource(article$Abstract))   

# Text transformation
toSpace = content_transformer(
  function (x, pattern)
    gsub(pattern, " ", x))
docs1 = tm_map(docs, toSpace, "/")
docs1 = tm_map(docs, toSpace, "@")
docs1 = tm_map(docs, toSpace, "#")
strwrap(docs1)

# Cleaning the Text
docs1 = tm_map(docs1, content_transformer(tolower))
docs1 = tm_map(docs1, removeNumbers)
docs1 = tm_map(docs1, removeWords, stopwords("english"))
docs1 = tm_map(docs1, removeWords, c("also", "the", "interest"))
docs1 = tm_map(docs1, stemDocument)
docs1 = tm_map(docs1, stripWhitespace)


# Build a term-document matrix
dtm = TermDocumentMatrix(docs1)
m = as.matrix(dtm)
v = sort(rowSums(m), 
         decreasing = TRUE)
d = data.frame(word = names(v),
               freq = v)
head(d, 10)

# Generate the Word cloud
wordcloud(words = d$word, 
          freq = d$freq,
          min.freq = 1, 
          max.words = 250,
          random.order = FALSE, 
          rot.per = 0.3, 
          colors = brewer.pal(8, "Dark2"))

barplot(d[1:10, ]$freq, names.arg = d[1:10, ]$word,
        col ="lightgreen", main ="Top 10 most frequent words",
        ylab = "Word frequencies", xlab="Words")
