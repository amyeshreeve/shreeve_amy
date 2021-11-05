# Load libraries

library(tidyverse)
library(stringr)
library(tidytext)

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

knnfit <- train(ut ~ ., 
                data = training_set_no_id,
                method = "knn",
                tuneLength = 7)
knn_pred <- test_set %>% select(-ut) %>% predict(knnfit, newdata = ., type = 'prob')

# NB Modelling

fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)
nb_mod = train(ut ~ ., 
               data = training_set_no_id, 
               method="naive_bayes", 
               trControl = fitControl, 
               tuneGrid = expand.grid(usekernel=TRUE,laplace=0,adjust=1))

nb_pred <- test_set %>% select(-ut) %>% predict(nb_mod, newdata = ., type = 'prob')

# NN Modelling

nnetFit <- train(ut ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 verbose = FALSE)

nn_pred <- test_set %>% select(-ut) %>% predict(nnetFit, newdata = ., type = 'prob')

# ROCs
knn_roc <- roc(test_set$ut,knn_pred$yes)
nb_roc <- roc(test_set$ut,nb_pred$yes)
nn_roc <- roc(test_set$ut,nn_pred$yes)

# Compare
ggroc(list(knn=knn_roc,nb=nb_roc,nnet=nn_roc), legacy.axes = TRUE)