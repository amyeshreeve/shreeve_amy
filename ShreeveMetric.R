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

ex = read_excel("ExMormonData.xlsx") %>% na.omit()
lds = read_excel("LDSData.xlsx")  %>% na.omit()

# Skill 2 & 3 -- Framegrams & ggplot

# Lemmatizing

lemmatize_strings("gay lesbian bisexual transgender queer lgbt")
lgbt <- "gay|lesbian|bisexual|transgender|queer|lgbt"

# Bound stopwords
stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")

ex %>%
  unnest_tokens(trigram, comment, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,lgbt)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:15) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

lds %>%
  unnest_tokens(trigram, comment, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,lgbt)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:15) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Skill 4 -- ML Classification

# More libraries

library(stringr)
library(tidytext)
library(caret)
library(pROC)

# Had to simplify the data because the size kept crashing r lol

lds2 <- lds %>% select(c("post_text", "subreddit")) %>% unique()
lds2 <- lds2[1:50, ]
ex2 <- ex %>% select(c("post_text", "subreddit")) %>% unique()
ex2 <- ex2[1:50, ]

df <- rbind(lds2, ex2) %>% 
  select(c("post_text", "subreddit")) %>% 
  mutate(number=row.names(.))

# Making labels to save for later

df_labels <- df %>% mutate(type = ifelse(subreddit=="latterdaysaints","yes","no")) %>%
  select(number, type)

# Feature Engineering
# TF-IDF is appropriate because we don't already know the content categories.

df_counts <- map_df(1:2, 
                    ~ unnest_tokens(df, word, post_text, 
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
  right_join(df_labels, by = "number") %>% 
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

knnfit <- train(type ~ ., 
                data = training_set_no_id,
                method = "knn",
                tuneLength = 7)
knn_pred <- test_set %>% select(-type) %>% predict(knnfit, newdata = ., type = 'prob')

# NB Modelling

fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 3, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)
nb_mod = train(type ~ ., 
               data = training_set_no_id, 
               method="naive_bayes", 
               trControl = fitControl, 
               tuneGrid = expand.grid(usekernel=TRUE,laplace=0,adjust=1))

nb_pred <- test_set %>% select(-type) %>% predict(nb_mod, newdata = ., type = 'prob')

# NN Modelling

nnetFit <- train(type ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 MaxNWts = 9352,
                 verbose = FALSE)

nn_pred <- test_set %>% select(-type) %>% predict(nnetFit, newdata = ., type = 'prob')

# ROCs

knn_roc <- roc(test_set$type,knn_pred$yes)
nb_roc <- roc(test_set$type,nb_pred$yes)
nn_roc <- roc(test_set$type,nn_pred$yes)

# Compare numerically
knn_roc
nb_roc
nn_roc

# Compare visually

ggroc(list(knn=knn_roc,nb=nb_roc,nnet=nn_roc), legacy.axes = TRUE)
