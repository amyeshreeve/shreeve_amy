# Importing libraries

library(readxl)
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(textstem)
library(dplyr)

library(RedditExtractoR)
library(openxlsx)

# Skill 1 -- Social Media Scraping
# This works, but it takes a long time to run
# I saved it to an excel and then import it later

excontent <- get_reddit(
  subreddit = "exmormon",
  search_terms = "gay",
  page_threshold = 10,
  cn_threshold = 50
)


ldscontent <- get_reddit(
  subreddit = "latterdaysaints",
  search_terms = "gay",
  page_threshold = 10,
  cn_threshold = 50
)

write.xlsx(excontent, 'ExMormonData.xlsx')
write.xlsx(ldscontent, 'LDSData.xlsx')

# Importing datasets

ex = read_excel("ExMormonData.xlsx")
lds = read_excel("LDSData.xlsx")

# Skill 2 -- Framegrams
# Lemmatizing

ex$comment = lemmatize_words(ex$comment)

ex_word_n <- ex %>%
  unnest_tokens(word, comment) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# Get total words for tf/if calc

ex_total_words <- ex_word_n %>% 
  summarize(total = sum(n))

ex_word_data <- left_join(ex_word_n, ex_total_words)

# Save word data with previous data and tf_idf

ex_word_data <- ex_word_data %>%
  bind_tf_idf(word, date, n)

# Print word data

ex_word_data %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Skill 3 -- Visualizing with ggplot
# Visualizes the top 10 most common terms.

ex_word_data %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  slice(1:10) %>% 
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 2, scales = "free") +
  coord_flip()

# Now, let's do it all again for the other subreddit

# Lemmatizing

lds$comment = lemmatize_words(lds$comment)

lds_word_n <- lds %>%
  unnest_tokens(word, comment) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# Get total words for tf/if calc

lds_total_words <- lds_word_n %>% 
  summarize(total = sum(n))

lds_word_data <- left_join(lds_word_n, lds_total_words)

# Save word data with previous data and tf_idf

lds_word_data <- lds_word_data %>%
  bind_tf_idf(word, date, n)

# Print word data

lds_word_data %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Visualizes the top 10 most common terms. 

lds_word_data %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  slice(1:10) %>% 
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 2, scales = "free") +
  coord_flip()

# Skill 4 -- ML Classification

# More libraries

library(stringr)
library(tidytext)
library(caret)
library(pROC)

df = read_csv("spam_ham.csv") %>% mutate(number=row.names(.))
df_labels <- df %>% mutate(spam = ifelse(Type=="spam","yes","no")) %>% select(number, spam)

# Feature Engineering
# TF-IDF is appropriate because we don't already know the content categories.

df_counts <- map_df(1:2, 
                    ~ unnest_tokens(df, word, Message, 
                                    token = "ngrams", n = .x)) %>%
  anti_join(stop_words, by = "word") %>%
  count(number, word, sort = TRUE)

# finding only words used more than 10x

words_10 <- df_counts %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 10) %>%
  select(word) %>%
  na.omit()

# making the document term matrix

df_dtm <- df_counts %>%
  right_join(words_10, by = "word") %>%
  bind_tf_idf(word, number, n) %>%
  cast_dtm(number, word, tf_idf)

# Engineered data with labels

df_engineered <- df_dtm %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(number = dimnames(df_dtm)[[1]]) %>% 
  right_join(df_labels) %>% 
  filter(complete.cases(.))

# Training and test sets

training_set <- df_engineered %>% slice_sample(prop =.8)
test_set <- df_engineered %>% 
  anti_join(training_set, by="number") %>% 
  select(-number)
training_set_no_id <- df_engineered %>% 
  slice_sample(prop =.8) %>% 
  select(-number)

# KNN Modelling

knnfit <- train(spam ~ ., 
                data = training_set_no_id,
                method = "knn",
                tuneLength = 7)
knn_pred <- test_set %>% select(-spam) %>% predict(knnfit, newdata = ., type = 'prob')

# NB Modelling

fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)
nb_mod = train(spam ~ ., 
               data = training_set_no_id, 
               method="naive_bayes", 
               trControl = fitControl, 
               tuneGrid = expand.grid(usekernel=TRUE,laplace=0,adjust=1))

nb_pred <- test_set %>% select(-spam) %>% predict(nb_mod, newdata = ., type = 'prob')

# NN Modelling

nnetFit <- train(spam ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 verbose = FALSE)

nn_pred <- test_set %>% select(-spam) %>% predict(nnetFit, newdata = ., type = 'prob')

# ROCs

knn_roc <- roc(test_set$spam,knn_pred$yes)
nb_roc <- roc(test_set$spam,nb_pred$yes)
nn_roc <- roc(test_set$spam,nn_pred$yes)

# Compare visually

ggroc(list(knn=knn_roc,nb=nb_roc,nnet=nn_roc), legacy.axes = TRUE)