#Turn this text into a tidy text dataset, we need to 
#turn to a dataframe.
library(dplyr)
text <- c("Because I could not stop for Death -",          
          "He kindly stopped for me -",          
          "The Carriage held but just Ourselves -",          
          "and Immortality")

text_df <- data.frame(line = 1:4, text = text)

#We need to ensure that this data frame is compatible
#for tidy text analysis.
#it means we can not filter out words and we cannot
#count how many times words occur.
#We need to convert one token per document per row.
#A token is a meaninful unit of words that we are interested in 
#using for further analysis.
#Tokenization is the process of splitting text into words
#We need to break the text into individual token
#and transform the text to individual tidy text
#import tidyr library
library(tidyr)
library(tidytext)
text_df <- text_df%>%
  unnest_tokens(word, text)


#This gives frequencies of each word.
#having dataset in this form helps us to
#manipulate to do some data manipulation
text_df %>% 
  ggplot(aes(x = word, y = line)) +
           geom_bar(stat = "identity")

#Tidying text using stringr
#We will use the work of jane austene six in our
#analysis. Before then, we need to import the book
library(janeaustenr)
library(stringr)

original_book <- austen_books() %>% 
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
  
#To work with original_book as a data set, we need to
#use to transform all text to a token
tidy_books <- original_book %>% 
  unnest_tokens(word, text)
  

#The data is in one word per row format
#The data is in one word per row format, we 
#need to maipulate to manipulate it with tidy
#tools like dplyr
#Also, eliminate stop words like "i", "of" etc. 
#These words are unnecessary in data analysis.
#remove stop words using anti_joins() on stop words
data("stop_words")
tidy_books <- tidy_books %>%
  anti_join(stop_words)
         

#count words
tidy_books%>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL)+
  coord_flip()


#We want to access data set library
install.packages("gutenbergr")
library(gutenbergr)

#download some datasets
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

#remove stop_words by using anti_joins() function.
tidy_hgwells <- hgwells %>%  
  unnest_tokens(word, text) %>%  
  anti_join(stop_words)

#Find common words
tidy_hgwells %>% 
  count(word, sort = TRUE)


#let's get some books through the id
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- tidy_bronte%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
  

#Give the most common words
tidy_bronte %>%
  count(word, sort = TRUE)

#let calculate the frequency of each author's word
#by binding all the three books together
#tidy_books, tidy_bronte and tidy_hgwells
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),                       
                       mutate(tidy_hgwells, author = "H.G. Wells"),                       
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%  
  count(author, word) %>%  
  group_by(author) %>%  
  mutate(proportion = n / sum(n)) %>%  
  select(-n) %>%  
  spread(author, proportion) %>%  
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`) 


library(ggplot2)
library(scales)
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,                      
                      color = abs(`Jane Austen` - proportion))) +  
  geom_abline(color = "gray40", lty = 2) +  
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +  
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +  
  scale_x_log10(labels = percent_format()) +  
  scale_y_log10(labels = percent_format()) +  
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +  
  facet_wrap(~author, ncol = 2) +  
  theme(legend.position="none") +  
  labs(y = "Jane Austen", x = NULL)


#Now let us now examine how correlated are words 
#between austen and Bronte and also between 
#Austen and H.G Wells
#correlation between Austen and Bronte
cor.test(data = frequency[frequency$author == "Brontë Sisters",],         
         ~ proportion + `Jane Austen`)

#Correlation between Austen and H.G Wells
cor.test(data = frequency[frequency$author == "H.G. Wells",],         
         ~ proportion + `Jane Austen`)

#Just as we saw in the scatter plot, the word are
#more correlated between Austen and Bronte than
#between Austen and H.G wells.
