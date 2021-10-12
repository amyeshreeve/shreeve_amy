# Select the first 100 entries for each class in the AG News dataset. 
# Within this data subset, is there a measurable and meaningful difference in 
# average description sentiment by article class?


#Loading in libraries and data
library(textdata)
library(tidyverse)
library(tidytext)
library(dabestr)


afin <- get_sentiments("afinn")
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
afin_lemma  <- afin %>% mutate(word=lemmatize_words(word))

# Afinn analysis with lemmatized data + getting aggregate score
afin_analysis <- df_lemma %>%
  inner_join(afin_lemma) %>%
  group_by(article) %>% 
  summarise(sentiment = sum(value), class = unique(class))

# Piping into dabest by class
afin_dabest <- afin_analysis %>% select(class, sentiment) %>%
  dabest(x= class,
         y= sentiment,
         idx= c("Sci/Tech", "Sports", "World", "Business"),
         paired = FALSE)


# Getting mean difference figures
afin_dabest %>% mean_diff() 

# Visualizing mean difference
afin_dabest %>% mean_diff() %>% plot
