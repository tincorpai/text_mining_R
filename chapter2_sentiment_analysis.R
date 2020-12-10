#From chapter 1, we examined which words are use most
#frequently and to compare documents
#In this chapter, we examine what is called 
#opinion mining, sometime called sentiment analysis.
#Here, we want to use test mining tool to examine
#emotional context of a text.
#To analyze a sentiment of a text, we need to consider a text
#as a component of its singular word.
#The sentiment content of a text as a sum of all the 
#sentiment of individual word. There are many 
#tools that helps to evaluate opinion in a text.
#tidytext contains several sentiment lexicon
#in the sentiment data set.
#import the tidytext using library() function
#import sentiment data set as well
#There are three lexicons: they contain unigrams
#The lexicon are afinn, bing and nrc
#There are also domian specific sentiment analysis 
#as well.
#Performing sentiment connection is an inner join operation.
library(tidytext)
library(textdata)
library(janeaustenr)
library(dplyr) 
library(stringr)
library(ggplot2)

sentiments
unique(sentiments$sentiment)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


#create an object called tidy_book
tidy_book <- austen_books() %>%
  group_by(book)%>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",                                                 
                                                 ignore_case = TRUE)))) %>%
  ungroup()%>%
  unnest_tokens(word, text)


#Let's create a sentiment called "joy"
nrcjoy <- get_sentiments("nrc") %>%  
  filter(sentiment == "joy")

#Filter out emma and inner_join to nrcjoy 
tidy_book %>%  
  filter(book == "Emma") %>%  
  inner_join(nrcjoy) %>%  
  count(word, sort = TRUE)

#We could examine how sentiment changes throughout
#the each novel
#Firstly, we find sentiment score for each word 
#using Bing lexicon and inner_join()
#Next we count up how many negative and positive 
#word are there in defined sections of each word.
library(tidyr)
janeaustensentiment <- tidy_book %>%  
  inner_join(get_sentiments("bing")) %>%  
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%  
  mutate(sentiment = positive - negative) 

#Now, plot the sentiment analysis across each novel
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +  
  geom_col(show.legend = FALSE) +  
  facet_wrap(~book, ncol = 2, scales = "free_x")



############################################
#Comparing The Three Sentiment Dictionaries#
############################################

