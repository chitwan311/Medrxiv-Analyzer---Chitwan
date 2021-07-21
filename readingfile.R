dataset = scan(file='mutation2pubtatorcentral.txt',
               what = "character",
                nmax=1000,
               skipNul=FALSE)
#               sep = '\t', 
#               quote = '', 
#               dec = ' ',
#               skipNul = FALSE,
#               multi.line = TRUE)

dataset = read.delim('mutation2pubtatorcentral.txt', 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

install.packages("data.table")
library(data.table)
try <- fread("mutation2pubtatorcentral.txt", sep = ",", header= FALSE,
             nrows=100)

install.packages("readr")
library(readr)
my_data <- read_lines('mutation2pubtatorcentral.txt', skip=0, n_max=10)
View(my_data)

my_data2 <- read_tsv('mutation2pubtatorcentral.txt', col_names=FALSE)

my <- read_table("mutation2pubtatorcentral.txt", n_max=10,
                  col_names=FALSE, skip=0, skip_empty_rows = FALSE,
                  col_types = NULL, locale = default_locale())

data <- fread("mutation2pubtatorcentral.txt")
#looking at top 5000 rows
data <- data %>% select=c(2:3) %>% head(5000)



audfeed <- read.table("mutation2pubtatorcentral.txt", header =FALSE,
                      fill=T,na.strings="NA", 
                      sep ='\t',stringsAsFactors=FALSE)