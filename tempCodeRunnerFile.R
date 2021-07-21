data <- function(filename){
  dataset <<- read.delim(filename, 
                         header = FALSE, 
                         sep = '\t', 
                         quote = '', 
                         dec = ' ', 
                         stringsAsFactors = FALSE)
  return(dataset)
}