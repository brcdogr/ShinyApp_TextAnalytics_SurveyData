##########################################################
################ Importing Libraries######################
##########################################################
library(textreadr) # load documents
library(textdata)  # provide access to text-related data sets
library(tidyr)     # tidy the data
library(dplyr)     # data manipulation
library(stringr)   # extract words from a sentence
library(tidytext)  # text mining
library(tm)        # text mining
library(scales)    # scaling
library(reshape2)  # transform data between wide and long formats
library(wordcloud) # visualize the keyword as a word cloud
library(ggplot2)   # visualization tool
library(igraph)    # for bigram network
library(ggraph)    # for bigram network
library(widyr)

# Loading the Txt Files
getwd()
setwd("/Users/burcudogru/Dropbox/My Mac (Burcus-MacBook-Pro.local)/Desktop/Survey")
nm <- list.files(path="/Users/burcudogru/Dropbox/My Mac (Burcus-MacBook-Pro.local)/Desktop/Survey")

business_success <- read_document('Survery App_  Sentiment Analysis - Yes.txt')
business_failure <- read_document('Survery App_  Sentiment Analysis - No.txt')

business_success_df <- tibble(line=1:46, text = business_success, stringsAsFactors=FALSE)
business_failure_df <- tibble(line=1:26, text = business_failure, stringsAsFactors=FALSE)

## Removing Numeric
business_success_df$text <- gsub("[0-9]+", "", business_success_df$text)
business_failure_df$text <- gsub("[0-9]+", "", business_failure_df$text)

####### Custom###########
custom <- data_frame(word=c('watch', 'watching', 'super', 'bowl','?'), lexicon=c('cust', 'cust', 'cust', 'cust','cust'))

##########################################################
################Tokenization#############################
##########################################################

#Business Success Tokens
business_success_tokens<- business_success_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  anti_join(custom, by="word") %>% 
  count(line, word, sort=TRUE)
  ungroup()
business_success_tokens

#Business Failure Tokens
business_failure_tokens<- business_failure_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  anti_join(custom, by="word") %>% 
  count(line, word, sort=TRUE)
  ungroup()
business_failure_tokens

##########################################################
################ Frequency Histograms#####################
##########################################################

business_success_df$superbowl <- 'Yes'
business_failure_df$superbowl <- 'No'

all_df <- rbind(business_success_df, business_failure_df)

##Frequency histogram for business success
tidy_success <- business_success_df %>%
  group_by(superbowl) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  anti_join(custom, by="word") %>%
  count(word, sort = TRUE)

tidy_success

tidy_success %>%
  filter(n > 7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="orange") +
  xlab(NULL) +
  coord_flip()

##Frequency histogram for business failure
tidy_failure <- business_failure_df %>%
  group_by(superbowl) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  anti_join(custom, by="word") %>%
  count(word, sort = TRUE)

tidy_failure

tidy_failure %>%
  filter(n > 4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="cyan4") +
  xlab(NULL) +
  coord_flip()

##########################################################
################ Correlogram #####################
##########################################################
frequency <- bind_rows(mutate(business_success_tokens, superbowl="Yes"),
                       mutate(business_failure_tokens, superbowl="No")
)%>% #closing bind_rows
  
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(superbowl, word) %>%
  group_by(superbowl) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(superbowl, proportion) %>%
  gather(superbowl, proportion, Yes)

library(scales)
ggplot(frequency, aes(x=proportion, y=`No`, 
                      color = abs(`No`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~superbowl, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "No", x=NULL)

##########################################################
################ tf_idf Analysis#########################
##########################################################

all_business_idf <- bind_rows(mutate(business_success_tokens, superbowl="Yes"),
                              mutate(business_failure_tokens, superbowl="No")
)%>% #closing bind_rows
  count(superbowl, word, sort=TRUE) %>%
  ungroup()

total_words <- all_business_idf %>%
  group_by(superbowl) %>%
  summarize(total=sum(n))

superbowl_words <- left_join(all_business_idf, total_words)

print(superbowl_words)

ggplot(superbowl_words, aes(n/total, fill = superbowl))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.1) +
  facet_wrap(~superbowl, ncol=2, scales="free_y")


##################################################
############### Sentiment Analysis ###############
##################################################

##### Word Cloud for Business Success ######

business_success_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("bing"), by="word") %>% # binary
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "darkgreen"),
                   max.words=100)

business_success_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("nrc"), by="word") %>% # flavor
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale=c(1.2, 0.8),
                   fixed.asp=TRUE, title.size=0.9)

###Business Failure#####

business_failure_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("bing"), by="word") %>% # binary
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

business_failure_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("nrc"), by="word") %>% # flavor
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale=c(1.2, 0,8),
                   fixed.asp=TRUE, title.size=0.9)

# positive and negative sentiments among Success and Failure of super Bowl Promotion
tidy_superbowl <- all_df %>%
  group_by(superbowl) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word")

# using bing
superbowl_sentiment <- tidy_superbowl %>%
  inner_join(get_sentiments("bing"), by="word") %>%  # no positive sentiment
  count(superbowl, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(superbowl_sentiment, aes(index, sentiment, fill = Airlines)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~superbowl, ncol = 2, scales = "free_x")

# using nrc
superbowl_sentiment <- tidy_superbowl %>%
  inner_join(get_sentiments("nrc"), by="word") %>%  
  count(superbowl, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(superbowl_sentiment, aes(index, sentiment, fill = superbowl)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~superbowl, ncol = 2, scales = "free_x")

# comparing three sentiment lexicons in each Airlines report seperately

# Sentiment for Business success- 

Yes <- tidy_superbowl %>% 
  filter(superbowl == "Yes")

afinn <- Yes %>% 
  inner_join(get_sentiments("afinn"), by="word") %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(Yes %>% 
                            inner_join(get_sentiments("bing"), by="word") %>%
                            mutate(method = "Bing et al."),
                          Yes  %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative")), by="word") %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "fixed")

####### Business Failure######

No <- tidy_superbowl %>% 
  filter(superbowl == "No")

afinn <- No %>% 
  inner_join(get_sentiments("afinn"), by="word") %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(No %>% 
                            inner_join(get_sentiments("bing"), by="word") %>%
                            mutate(method = "Bing et al."),
                          No  %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative")), by="word") %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "fixed")

##Most common positive and negative words in Business and Failure

##### Business Success ##########

Success_bing_counts <- business_success_tokens %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Success_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

Success_nrc_counts <- business_success_tokens %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Success_nrc_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

### Business Failure####

##### Business Success ##########

Failure_bing_counts <- business_failure_tokens %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Failure_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

Failure_nrc_counts <- business_failure_tokens %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Failure_nrc_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

all_tokens <- bind_rows(mutate(business_success_tokens, superbowl="Yes"),
                        mutate(business_failure_tokens, superbowl="No"))


freq_by_rank <- all_tokens %>%
  group_by(superbowl) %>%
  mutate(rank = row_number(),
         `term frequency` = n/sum(n))

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=superbowl))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

all_words <- all_tokens %>%
  bind_tf_idf(word, line, n)

all_words %>%
  arrange(desc(tf_idf))

#############################################
###### N-grams and tokenizing ###############
############################################# 

business__success_bigrams <- business_success_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)


business_failure_bigrams <- business_failure_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

all_bigrams <- all_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)


######Count#########

business__success_bigrams%>%
  count(bigram, sort = TRUE)

business__failure_bigrams%>%
  count(bigram, sort = TRUE)

all_bigrams%>%
  count(bigram, sort = TRUE) 

######Seperation######

business_success_bigrams_separated <- business__success_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

business_failure_bigrams_seperated <- business_failure_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

all_bigrams_separated <- all_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

######Filter Bi Gram ########

business_success_bigrams_filtered <- business_success_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


business_failure_bigrams_filtered <- business_failure_bigrams_seperated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

all_bigrams_filtered <- all_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":

business_sucesss_bigram_counts <- business_success_bigrams_filtered%>%
  count(word1, word2, sort = TRUE)

business_failure_bigram_counts <- business_failure_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

all_bigram_counts <- all_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing a bigram network

#Business Success Network

business_success_bigram_graph <- business_sucesss_bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(business_success_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Business Failure Network

business_failure_bigram_graph <- business_failure_bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(business_success_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##########################################################
#########Word Correlation Network#########################
##########################################################

#Business_success_count pair
business_success_count <-business_success_tokens %>%
  pairwise_count(word, line,sort = TRUE)

business_success_count

#Business_Success_cor
business_success_corrs <- business_success_tokens %>%
  group_by(word) %>%
  filter(n() >= 1) %>%
  pairwise_cor(word, line, sort = TRUE)

business_success_corrs

#Business Success_network
business_success_corrs %>%
  filter(correlation >0.7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

#Business_failure_count pair
business_failure_count <-business_failure_tokens %>%
  pairwise_count(word, line,sort = TRUE)

business_failure_count

#Business_Success_cor
business_failure_corrs <- business_failure_tokens %>%
  group_by(word) %>%
  filter(n() >= 1) %>%
  pairwise_cor(word, line, sort = TRUE)

business_failure_corrs

#Business Success_network
business_failure_corrs %>%
  filter(correlation >0.9) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=8)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()
