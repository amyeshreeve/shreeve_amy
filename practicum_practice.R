# Select the first 100 entries for each class in the AG News dataset. 
# Within this data subset, is there a measurable and meaningful difference in 
# average description sentiment by article class?


#Loading in libraries and data
library(textdata)
library(tidyverse)
library(tidytext)
library(dabestr)


bing <- get_sentiments("bing")
df <- dataset_ag_news()

# Making a column to distinguish articles
df <- df %>%
  cbind(article = rownames(df))

# Getting the first 100 entries for each class
df <- df %>%
  group_by(class) %>%
  slice(1:100) %>%
  ungroup()

# Lemmatizing/unnesting tokens
df_lemma <- df %>%
  unnest_tokens(word, description, token = "words", drop = TRUE) %>%
  mutate(word=lemmatize_words(word)) %>%
  left_join(df) %>%
  select(-title, -description)
bing_lemma  <- bing %>% mutate(word=lemmatize_words(word))

# bing analysis with lemmatized data
bing_analysis <- df_lemma %>%
  inner_join(bing_lemma)%>%
  count(article, sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from=n) %>% 
  mutate(positive = replace_na(positive,0), negative = replace_na(negative,0)) %>% 
  mutate(sent_score = positive + negative*-1) %>%
  left_join(df_lemma)

# Getting the aggregate score per article
bing_analysis_n <- bing_analysis %>% 
  group_by(article) %>% 
  summarise(sentiment = sum(sent_score), class = unique(class))
  
# Piping into dabest by class
bing_dabest <- bing_analysis_n %>% select(class, sentiment) %>%
  dabest(x= class,
         y= sentiment,
         idx= c("Business", "Sci/Tech", "Sports", "World"),
         paired = FALSE)


# Getting mean difference figures
bing_dabest %>% mean_diff() 

# Visualizing mean difference
bing_dabest %>% mean_diff() %>% plot
