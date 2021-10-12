# Select the first 100 entries for each class in the AG News dataset. 
# Within this data subset, is there a measurable and meaningful difference in 
# average description sentiment by article class?


library(textdata)
library(tidyverse)
library(tidytext)
library(dabestr)


bing <- get_sentiments("bing")
df <- dataset_ag_news()

df <- df %>%
  cbind(article = rownames(df))

df <- df %>%
  group_by(class) %>%
  slice(1:100) %>%
  ungroup()

df_lemma <- df %>%
  unnest_tokens(word, description, token = "words", drop = TRUE) %>%
  mutate(word=lemmatize_words(word)) %>%
  left_join(df) %>%
  select(-title, -description)
bing_lemma  <- bing %>% mutate(word=lemmatize_words(word))

bing_analysis <- df_lemma %>%
  inner_join(bing_lemma) %>%
  count(article, sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from=n) %>% 
  mutate(positive = replace_na(positive,0), negative = replace_na(negative,0)) %>% 
  mutate(sent_score = positive + negative*-1) %>%
  left_join(df_lemma)

bing_analysis_n <- bing_analysis %>% 
  group_by(article) %>% 
  summarise(sentiment = mean(sent_score), class = unique(class))
  

bing_dabest <- bing_analysis_n %>% select(class, sentiment) %>%
  dabest(x= class,
         y= sentiment,
         idx= c("Sports", "World", "Business", "Sci/Tech"),
         paired = FALSE)

bing_dabest %>% mean_diff() %>% plot
