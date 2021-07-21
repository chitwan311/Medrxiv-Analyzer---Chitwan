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
View(article$Abstract)


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


common_words <- load("data_common_words.RData")
# Cleaning the Text
docs1 = tm_map(docs1, content_transformer(tolower))
docs1 = tm_map(docs1, removeNumbers)
docs1 = tm_map(docs1, removeWords, stopwords("english"))
docs1 = tm_map(docs1, removeWords, common_words)
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



library(ggplot2)
ggplot(data = d[1:20, ], aes(x=d[1:20,]$word, y=d[1:20,]$freq, fill=d[1:20, ]$word))+
  geom_bar(position ='dodge', stat="identity")+
  theme_minimal()+
  labs(fill="Words")+
  ggtitle('Top 20 Most frequent words')+
  coord_polar()+
  xlab('Words')+
  ylab('Frequency')

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


#pattern_mutation <- "(\\w+):\\s(\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
pattern_mutation <- "([a-z]{2})\\d+"
mutation <- str_match("rs773866720", pattern_mutation)
View(mutation)

pattern_celline <- "(CVCL_)(\\d+)"
cell_line <- str_match(article$Abstract, pattern_celline)
View(cell_line)

#for chemical also same code
#for bioconcepts also same code
pattern_disease <- "(MESH:D)(\\d+)"
disease <- str_match(article$Abstract, pattern_disease)
View(disease)

pattern_species <- "(\\d+){4}"
species <- str_match(article$Abstract, pattern_species)
View(species)


----------------------------------------------------------
#Topic Modelling
install.packages('topicmodels')
library(topicmodels)

data_tm <- article$Abstract

install.packages('tidytext')
library(tidytext)

DTM <- DocumentTermMatrix(docs1, control = list(bounds = list(global = c(5, Inf))))
dim(DTM)


sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
#textdata <- textdata[sel_idx, ]

K <- 10
topicModel <- LDA(DTM, K)


tmResult <- posterior(topicModel)
attributes(tmResult)
nTerms(DTM)   
betas <- tmResult$terms  
dim(beta)  
thetas <- tmResult$topics 

rowSums(thetas)[1:10] 
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

-----------------------------------------------------------------

--------------------------------------------------------------------

  
  install.packages('igraph')
  library(igraph)

(bigram_graph <- series %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE) %>%
    unite("bigram", c(word1, word2), sep = " ") %>%
    filter(n > 20) %>%
    graph_from_data_frame()
)
------------------------------------------------------------
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
count_bigrams(DTM)
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
-----------------------------------------------------------------
  ------------------------------------------------------------
  ----------------------------------------------------------
library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams