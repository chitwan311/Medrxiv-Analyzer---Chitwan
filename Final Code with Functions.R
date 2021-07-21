data <- function(filename){
  dataset <<- read.delim(filename, 
                         header = FALSE, 
                         sep = '\t', 
                         quote = '', 
                         dec = ' ', 
                         stringsAsFactors = FALSE)
  return(dataset)
}

data('citations1.txt')


abstract <- function(x, npar=TRUE, print=TRUE){
  library(stringr)
  library(dplyr)
  #install.packages('stringi')
  library(stringi)
  dataset2 <- as.data.frame(dataset)
  odd_row <- seq_len(nrow(dataset2)) %% 2
  odd_col <- dataset2[odd_row == 1,]
  abstract1 <- dataset2[odd_row == 0,]
  abstract2 <<- as.data.frame(abstract1)
  odd <<- as.data.frame(odd_col)
  #View(odd)
  return(abstract2)
}

abstract()




doi <- function(x, npar=TRUE, print=TRUE){
  pattern_doi <- "(\\w+):\\s(\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
  doi <- str_match(odd[, 1],pattern_doi)
  doi <<- as.data.frame(doi[, 1])
  return(doi)
}
doi()




author <- function(x, npar=TRUE, print=TRUE){
  #(\\w+)\\s(\\w+)\\s(\\w+)\\s[a-z][^abc]"
  pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"
  #pattern_author <- c("(\\w+)\\s(\\w+)\\s", "al$")
  author_name <- str_match(odd[,1], pattern_author)
  #author_name <- str_match(dataset[,1], regex("[a-z]", multiline = TRUE))
  author1 <<- as.data.frame(author_name[, 1])
  return(author1)
}
author()




title <- function(x, npar=TRUE, print=TRUE){
  pattern_title <- "(?<=\")(.*?)(?=\")"
  title1 <<- str_match(odd[, 1], pattern_title)
  title1 <<- as.data.frame(title1[, 1])
  #return(View(title1))
  return(title1)
}
title()




article <- function(x, npar=TRUE, print=TRUE){
  article_df <<- data.frame(doi, title1, author1, abstract2)
  colnames(article_df) <- c("DOI Link", "Title", "Author", "Abstract")
  #View(article_df$Abstract)
  return(article_df)
}
article()




#install.packages("tm")           # for text mining
#install.packages("SnowballC")    # for text stemming
#install.packages("wordcloud")    # word-cloud generator
#install.packages("RColorBrewer") # color palettes
df <- data('citations1.txt')
DTM <- function(dataframe){
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
  docs <<- VCorpus(VectorSource(dataframe))   
  
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

list_words <- DTM(article_df$abstract1)

word_matrix <- function(list_words){
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
  word_mat <<- data.frame(word = names(v),
                          freq = v)
  return(head(word_mat, 10))
  #return(d)
}
#head(d, 10)

matrix <- word_matrix()




# Generate the Word cloud
wordcld <- function(matrix){
  wordcloud(words = word_mat$word, 
            freq = word_mat$freq,
            min.freq = 1, 
            max.words = 250,
            random.order = FALSE, 
            rot.per = 0.3, 
            colors = brewer.pal(8, "Dark2"))
}




pieplot <- function(matrix)
{
  library(ggplot2)
  ggplot(data = word_mat[1:20, ], aes(x=word_mat[1:20,]$word, y=word_mat[1:20,]$freq, fill=word_mat[1:20, ]$word))+
    geom_bar(position ='dodge', stat="identity")+
    theme_minimal()+
    labs(fill="Words")+
    ggtitle('Top 20 Most frequent words')+
    coord_polar()+
    xlab('Words')+
    ylab('Frequency')
}
pieplot()




barplot <- function(matrix)
{
  library(ggplot2)
  ggplot(data = word_mat[1:20, ], aes(x=word_mat[1:20,]$word, y=word_mat[1:20,]$freq, fill=word_mat[1:20, ]$word))+
    geom_bar(position ='dodge', stat="identity")+
    theme_minimal()+
    labs(fill="Words")+
    ggtitle('Top 20 Most frequent words')+
    xlab('Words')+
    ylab('Frequency')
}
barplot()





#install.packages("ggpubr")
dotline <- function(matrix){
  library(ggpubr)
  ggplot(word_mat[1:20, ], aes(x=word_mat[1:20,]$word, y=word_mat[1:20, ]$freq)) +
    geom_linerange(
      aes(x = word_mat[1:20,]$word, ymin = 0, ymax = 10, color = word_mat[1:20,]$word), 
      size = 3)+
    theme_minimal()+
    ggtitle("Top 20 most frequent words")+
    coord_flip()+
    labs(color=element_blank())+
    geom_point(aes(size = 8, color=word_mat[1:20,]$word))+
    xlab('Words')+
    ylab('Frequency')
}
dotline()




mutation_fun <- function(dataset){  
  pattern_mutation <- "([a-z]{2})\\d+"
  mutation <- str_match("rs773866720", pattern_mutation)
  #View(mutation)
  return(mutation)
}
mutation_fun()


#medical subject heading (mesh) -> apply filter for word cloud




NLP_Model <- function(x, npar=TRUE, print=TRUE){
  library(topicmodels)
  
  data_tm <- article$Abstract
  
  #install.packages('tidytext')
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
  
  topics <<- tidy(topicModel, matrix = "beta")
  topics
  text_network1(topics)
  
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
}



#install.packages('dplyr')
library(dplyr)
#install.packages('igraph')

text_network1 <- function(topics){
  my_adj_list <- topics %>% filter(beta > 0.025)
  names(my_adj_list) <- c('from', 'to', 'weight')
  class(my_adj_list)
  dim(my_adj_list)
  library(igraph)
  # create igraph S3 object
  net <- graph.data.frame(my_adj_list, directed = FALSE)
  # store original margins
  orig_mar <- par()$mar
  # set new margins to limit whitespace in plot
  par(mar=rep(.1, 4))
  set.seed(123)
  
  plot(net, layout = layout_components(net), edge.width = E(net)$weight)
  
}


text_network2 <- function(x, npar=TRUE, print=TRUE){
  data.frame(my_adj_list) <- data.frame(topics) %>% filter(beta > 0.025)
  names(my_adj_list) <- c('from', 'to', 'weight')
  class(my_adj_list)
  dim(my_adj_list)
  library(igraph)
  
  # create igraph S3 object
  net <- graph.data.frame(my_adj_list, directed = FALSE)
  
  # store original margins
  orig_mar <- par()$mar
  
  # set new margins to limit whitespace in plot
  par(mar=rep(.1, 4))
  set.seed(123)
  plot(net, layout = layout_components(net), edge.width = E(net)$weight, vertex.shape="none")
}

text_network3 <- function(x, npar=TRUE, print=TRUE){
  my_adj_list <- topics %>% filter(beta > 0.025)
  names(my_adj_list) <- c('from', 'to', 'weight')
  class(my_adj_list)
  dim(my_adj_list)
  library(igraph)
  
  # create igraph S3 object
  net <- graph.data.frame(my_adj_list, directed = FALSE)
  
  # store original margins
  orig_mar <- par()$mar
  
  # set new margins to limit whitespace in plot
  par(mar=rep(.1, 4))
  set.seed(123)
  
  set.seed(123)
  ceb <- cluster_edge_betweenness(net)
  class(ceb)
  set.seed(123)
  plot(ceb, net)
}


text_network4 <- function(x, npar=TRUE, print=TRUE){
  my_adj_list <- topics %>% filter(beta > 0.025)
  names(my_adj_list) <- c('from', 'to', 'weight')
  
  class(my_adj_list)
  
  dim(my_adj_list)
  library(igraph)
  
  # create igraph S3 object
  net <- graph.data.frame(my_adj_list, directed = FALSE)
  
  # store original margins
  orig_mar <- par()$mar
  
  # set new margins to limit whitespace in plot
  par(mar=rep(.1, 4))
  set.seed(123)
  
  # community membership for each node
  membership(ceb)
  
  
  par(mar=orig_mar)
  dendPlot(ceb, mode="hclust")
  
}
----------------------------
  text_network5 <- function(x, npar=TRUE, print=TRUE){
    my_adj_list <- topics %>% filter(beta > 0.025)
    names(my_adj_list) <- c('from', 'to', 'weight')
    
    class(my_adj_list)
    
    dim(my_adj_list)
    library(igraph)
    
    # create igraph S3 object
    net <- graph.data.frame(my_adj_list, directed = FALSE)
    
    # store original margins
    orig_mar <- par()$mar
    
    # set new margins to limit whitespace in plot
    par(mar=rep(.1, 4))
    set.seed(123)
    # not much difference in the edge width given the values
    # but I included it for reference's sake
    set.seed(123)
    plot(net, edge.arrow.size = 0.2,
         layout = layout_with_graphopt,
         vertex.color = rgb(1,0.8,0.4,0),
         #vertex.color = 'grey50',          # Node color
         vertex.shape2="square",
         vertex.shape="sphere", # One of "none", "circle", "square", "csquare", "rectangle" "crectangle", "vrectangle", "pie", "raster", or "sphere"
         vertex.size=17,                               # Size of the node (default is 15)
         vertex.size2=15,                              # The second size of the node (e.g. for a rectangle)
         vertex.label.family="Aerial",
         vertex.label.color="darkblue", # Font family of the label (e.g."Times", "Helvetica")
         vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
         vertex.label.cex=0.9,                           # Font size (multiplication factor, device-dependent)
         vertex.label.dist=0,                          # Distance between the label and the vertex
         vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
         edge.width=3,                                 # Edge width, defaults to 1
         edge.arrow.size=1,                            # Arrow size, defaults to 1
         edge.arrow.width=1,                           # Arrow width, defaults to 1
         edge.lty="solid",                             # Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed", 3 or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
         edge.curved=0.1,
         edge.color="grey70",
         arrow.mode="all")
    
  }


-----------------------------------------------------------
  
  