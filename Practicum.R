# Compare texts classified as spam or not spam to determine if there is a 
# measurable and meaningful difference in average number of positive and 
# negative words per text. 

library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

df = read_csv("spam_ham.csv") %>% mutate(text=row.names(.))
bing <- get_sentiments("bing")

data = df %>%
  unnest_tokens(word, Message, token = "words")

data_lemma <- data %>% mutate(word=lemmatize_words(word), .keep = "all")
bing_lemma <- bing %>% mutate(word=lemmatize_words(word))
data_lemma <- data_lemma %>% left_join(bing_lemma)

data %>% regex_left_join(bing, by = NULL, ignore_case = TRUE)

#Aggregate the data
bing_analysis <- data_lemma %>%
  count(text,sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from=n) %>% 
  mutate(positive = replace_na(positive,0), negative = replace_na(negative,0)) %>% 
  mutate(sent_score = positive + negative*-1) %>%
  left_join(df)
  
neg_analysis <- bing_analysis %>%
  group_by(Type) %>%
  summarise(neg_avg = mean(negative))

posi_analysis <- bing_analysis %>%
  group_by(Type) %>%
  summarise(posi_avg = mean(positive))

# Dabest for negative

neg_dabest <- bing_analysis %>% select(Type, negative) %>% 
  dabest(x = Type,
         y= negative,
         idx= c("spam", "ham"),
         paired = FALSE)

neg_dabest %>% mean_diff()
neg_dabest %>% mean_diff() %>% plot()

posi_dabest <- bing_analysis %>% select(Type, positive) %>% 
  dabest(x = Type,
         y= positive,
         idx= c("spam", "ham"),
         paired = FALSE)

posi_dabest %>% mean_diff() 
posi_dabest %>% mean_diff() %>% plot()
