# Is there a meaningful difference in the frequency of fear words used per 
# talking turn when comparing Bush, Cruz, and Fiorina to Trump? 

# Loading in libraries and data
library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

df = read_csv("gop_debates.csv")
nrc <- get_sentiments("nrc")

# Removing speaker names, filtering speakers, and unnesting tokens
data = df %>%
  separate(text, c(NA,"text"), sep = ":") %>%
  filter(who == "BUSH" | who == "CRUZ"|who == "FIORINA"|who == "TRUMP") %>%
  unnest_tokens(word, text, token = "words")

# Making the index into a column
data <- cbind(turn = rownames(data), data)

# Lemmatizing

## df_stemmed <- df %>% mutate(word=stem_words(word))
## nrc_stemmed <- nrc %>% mutate(word=stem_words(word))
## data1 = df_stemmed %>% left_join(nrc_stemmed)

data_lemma <- data %>% mutate(word=lemmatize_words(word))
nrc_lemma  <- nrc %>% mutate(word=lemmatize_words(word))

# data3 = df %>% regex_left_join(nrc) %>% group_by(word.x)

# Getting the n of each word, getting total words

data_n <- data_lemma %>%
  count(turn, word, sort = TRUE)

data_n <- data_n %>% left_join(data_lemma)

total_words <- data_n %>% 
  group_by(turn) %>% 
  summarize(total = sum(n))

data_n <- left_join(data_n, total_words)

# Doing the tf-idf

data_n <- data_n %>%
  bind_tf_idf(word, turn, n)

data2 <- data_n %>%
  inner_join(nrc)

# Adding term frequency by term

data2 <- data2 %>%
  filter(sentiment=="fear")

fear_data_n <- data2 %>% 
  filter(sentiment=="fear") %>%
  group_by(turn) %>% 
  summarise(fear_use = sum(tf))

fear_data_n <- inner_join(data2, total_words)

# Average fear frequency grouped by candidate

candidate_n <- data %>%
  count(who, turn, sort = TRUE) %>%
  group_by(who) %>%
  summarise(turns = sum(n))

candidate_analysis <- fear_data_n %>%
  group_by(who) %>%
  summarise(value = sum(tf))

candidate_analysis = candidate_analysis %>% left_join(candidate_n)

# candidate_analysis <- cbind(times = (div(value) by = turns), data)


#---------------------------------------------------------------------QUES

#What constitutes a turn?
#Maybe I shouldn't group by fear too early because then the tf will be out of 1
#Then I need to average across all turns by candidate
#I need to find a visualization that shows the differences based on Trump
#It doesn't matter *what* the fear words are; it just matters their frequency.