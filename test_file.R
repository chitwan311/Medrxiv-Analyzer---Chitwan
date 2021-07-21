dataset = read.delim('species2pubtatorcentral.txt', 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

dataset3 <- as.data.frame(dataset)
id <- as.data.frame(dataset3[, 1])
mut_name <- as.data.frame(dataset3[, 3])

saveRDS(dataset, file='mutation2pubtatorcentral.txt')
dfnew <- readRDS('mutation2pubtatorcentral.txt')
