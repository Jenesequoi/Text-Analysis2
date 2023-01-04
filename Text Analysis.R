###Text Analysis2

install.packages("tidytext")
library(tidytext)
get_sentiments("nrc")
install.packages("textdata")
library(textdata)
sentiments<- get_sentiments("nrc")
library(tidyverse)
library(readr)
library(tidytext)
library(textdata)
book_df<- readLines("Women Who Run with the Wolves.txt")

#create a tibble
review<- tibble(line= 1:length(book_df), text= book_df)

#tidy text
sent<- review %>% unnest_tokens(word, text)
sent%>% count(word, sort = TRUE)

#remove stop words using antijoin function
senti<- sent%>% anti_join(stop_words)
senti%>% count(word, sort= TRUE)

#All together now
senti<- review%>% unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  filter(n >100)

#sentiments
book_sentiments<- senti %>% inner_join(sentiments)
book_sentiments %>% 
  filter(sentiment== "positive")%>%
  count(word, sort = TRUE)
book_sentiments%>%
  count(sentiment)

#Plot
senti%>% mutate(word= reorder(word, n))%>%
  top_n((15))%>%
  ggplot(aes(n, word, fill= "brown"))+ 
  geom_col()+ labs(y=NULL)

book_sentiments%>%
  count(sentiment)%>%
  mutate(sentiment= reorder(sentiment, n))%>%
  ggplot(aes(n, sentiment))+
  geom_col()+labs(y= NULL, fill= NULL)

#More sentiment plots
review_sentiments<- review%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  inner_join(sentiments)

#block text
blocks<- review_sentiments%>%
  count(block= line %/% 150, sentiment)%>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0)%>%
  mutate(sentiment= positive- negative)

#plot review sentiments
blocks%>%
  ggplot(aes(block, sentiment))+
  geom_col()

blocks%>%
  ggplot(aes(block, joy))+
  geom_col()

###End of Text Analysis2
  