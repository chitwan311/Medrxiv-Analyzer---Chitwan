Download the main_package.R file on your local machines and open it in RStudio.

------------------------------------------------------------------------

Installation

Install the following packages using install.packages() function in R and then load these respective librariees given below:

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
library(Rcpp)
library(igraph)

---------------------------------------------------------------------------

Data Sources

Download the citations from the medrxiv website or can download citations from the github folder which are named as "citations1.txt" and "citations2.txt".
These citations which are already there in the folder are downloaded from the Addictions Medicine category and HIV-AIDS category from the medrxiv website.

After downloading the ".txt" files on local machines, for reading the files in RStudio:
Click on Files in the bottom right corner on RStudio and set the required ".txt" file as the working directory by selecting the option "More" and choosing "set as working directory".
If the user is getting the output in the console as "setwd("~/PS-1 Documents")", then the data is ready.


------------------------------------------------------------------------------

Running the Code

1. Reading the File
Run the data() function code by pressing "ctrl+enter" on the first line where the function is defined.
For reading the file, use the function "data()" and pass the filename in ".txt" format to this function. This would read the file and give the output which further would be used in other functions to create a dataframe out of it. This is a parametrized function, so passing the argument as ".txt" file is required by the user.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: data(".txt")

2. Extracting the Abstract from the .txt file
Run the abstract() function code by pressing "ctrl+enter" on the first line where the function is defined.
For extracting the abstract, the user can just type "abstract()" and press enter to get the output containing only the abstract from the citations. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: abstract()

3. Extracting the DOI Link from the .txt file
Run the doi() function code by pressing "ctrl+enter" on the first line where the function is defined.
For extracting the DOI Link of the corresponding article, the user can just type "doi()" and press enter to get the output containing only the doi from the citations. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: doi()

4. Extracting the Author Name from the .txt file
Run the author() function code by pressing "ctrl+enter" on the first line where the function is defined.
For extracting the Author name of the corresponding article, the user can just type "author()" and press enter to get the output containing only the author name from the citations. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: author()

5. Extracting the Journal Title from the .txt file
Run the title() function code by pressing "ctrl+enter" on the first line where the function is defined.
For extracting the Title of the corresponding article, the user can just type "title()" and press enter to get the output containing only the title from the citations. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: title()

6. Creating a Data Frame of the four elements which are extracted before
Run the article() function code by pressing "ctrl+enter" on the first line where the function is defined.
For creating a dataframe of the four elements abstract, doi, author name and journal title, the function "article()" should be used. This would give the output as a dataframe containing all the four elements as the output. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: article()

7. Building a Document Term Matrix (DTM) 
Run the DTM() function code by pressing "ctrl+enter" on the first line where the function is defined.
For building a document term matrix which would be used further for creating a word cloud and various plots. The user should run this function first and since, its a parametrized function, so argument is required. The parameter which is to be passed in this function is "article_df$abstract1".
Step1: "ctrl+enter" to run function definition.
After, running the function definition, write this in console and press enter:
Step2: list_words <- DTM(article_df$abstract1)

8. Building a word matrix
Run the word_matrix() function code by pressing "ctrl+enter" on the first line where the function is defined.
This function would give a word matrix as output containing the number of words in one column and their occurence in terms of frequency in the 2nd column.
Step1: "ctrl+enter" to run function definition.
After, running the function definition, write this in console and press enter:
Step2: matrix <- word_matrix()

9. Creating the word cloud
Run the word_cld() function code by pressing "ctrl+enter" on the first line where the function is defined.
For creating a word cloud from the word matrix which has been created the user can just make use of the wordcld() function. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: word_cld()

10. Creating the Bar Plot
Run the barplot() function code by pressing "ctrl+enter" on the first line where the function is defined.
For creating a word cloud from the word matrix which has been created the user can just make use of the barplot() function. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: barplot()

11. Creating the Pie Plot
Run the pieplot() function code by pressing "ctrl+enter" on the first line where the function is defined.
For creating a word cloud from the word matrix which has been created the user can just make use of the pieplot() function. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: pieplot()

12. Creating the Dot Line Graph
Run the dotline() function code by pressing "ctrl+enter" on the first line where the function is defined.
For creating a word cloud from the word matrix which has been created the user can just make use of the dotline() function. This is a non-parametrized function, so the user should not pass any argument.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: dotline()

13. Extracting the mutations type from abstract
Run the mutation_fun() function code by pressing "ctrl+enter" on the first line where the function is defined.
For extracting the type of mutations from an abstract of a citation, the user can just enter the name of the function "mutation_fun()" after running the function definition. This is a parametrized function. The parameter which is to be passed in this function is "article_df$abstract1".
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: mutation_fun(article_df$abstract1)

14. Making a Topic Model
Run the maketopicmodel() function code by pressing "ctrl+enter" on the first line where the function is defined.
This creates a topic model from the different types of words being used in the abstract and this is a parameterized function. The parameter which is to be passed in this function is "article_df$abstract1".
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: maketopicmodel(article_df$abstract1)

15. Running the createtopics() function
Run the createtopics() function code by pressing "ctrl+enter" on the first line where the function is defined.
This function is required for creating the text graphical networks later. The user should run this function for making beautiful graphical networks from abstract.
Step1: "ctrl+enter" to run function definition.
After running the function definition, its important to store the output in another variable which would be used as an argument for rest of the functions used for creating the text graphical network. Here, the output will be stored in a function named "ttopics".
Write this in console and press enter.
Step2: topic_matrix <- createtopics(article_df$abstract1)

16. Creating the Text Graphical Network of first type
Run the text_network1() function code by pressing "ctrl+enter" on the first line where the function is defined.
This function would create the graphical network of words from the abstract of the article. Since, this is a parameterized function, so the user should pass the argument which is obtained as the output from the createtopics() function. Here, the argument "topic_matrix" is passed.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: text_network1(topic_matrix)

17. Creating the Text Graphical Network of second type
Run the text_network2() function code by pressing "ctrl+enter" on the first line where the function is defined.
This function would create the graphical network of words from the abstract of the article. Since, this is a parameterized function, so the user should pass the argument which is obtained as the output from the createtopics() function. Here, the argument "topic_matrix" is passed.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: text_network2(topic_matrix)

17. Creating the Text Graphical Network of third type i.e. a Dendrogram
Run the text_network3() function code by pressing "ctrl+enter" on the first line where the function is defined.
This function would create the graphical network of words from the abstract of the article. Since, this is a parameterized function, so the user should pass the argument which is obtained as the output from the createtopics() function. Here, the argument "topic_matrix" is passed.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: text_network3(topic_matrix)

18. Creating the Text Graphical Network of fourth type
Run the text_network4() function code by pressing "ctrl+enter" on the first line where the function is defined.
This function would create the graphical network of words from the abstract of the article. Since, this is a parameterized function, so the user should pass the argument which is obtained as the output from the createtopics() function. Here, the argument "topic_matrix" is passed.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: text_network4(topic_matrix)

17. Creating the Text Graphical Network of fifth type
Run the text_network5() function code by pressing "ctrl+enter" on the first line where the function is defined.
This function would create the graphical network of words from the abstract of the article. Since, this is a parameterized function, so the user should pass the argument which is obtained as the output from the createtopics() function. Here, the argument "topic_matrix" is passed.
Step1: "ctrl+enter" to run function definition.
Write this in console and press enter.
Step2: text_network5(topic_matrix)


