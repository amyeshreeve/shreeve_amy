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

# Removing speaker names and filtering speakers
data = df %>%
  separate(text, c(NA,"text"), sep = ":") %>%
  filter(who == "BUSH" | who == "CRUZ"|who == "FIORINA"|who == "TRUMP")

# Making a column of row names and unnesting tokens.
data <- cbind(turn = rownames(data), data) %>%
  unnest_tokens(word, text, token = "words")

# Lemmatizing (decided on this method after comparing to stem/regex)

## df_stemmed <- df %>% mutate(word=stem_words(word))
## nrc_stemmed <- nrc %>% mutate(word=stem_words(word))
## data1 = df_stemmed %>% left_join(nrc_stemmed)

# data3 = df %>% regex_left_join(nrc) %>% group_by(word.x)

# Lemmatizes nrc & data, gets TF, combines nrc and data

data_lemma <- data %>% 
  mutate(word=lemmatize_words(word), .keep = "all" )  

data_count <- data_lemma %>% count(turn, word, sort = TRUE)

data_lemma <- data_lemma %>% left_join(data_count)

nrc_lemma  <- nrc %>% mutate(word=lemmatize_words(word))

data2 <- data_lemma %>%
  inner_join(nrc_lemma)

# Filtering term frequency by fear

data2 <- data2 %>%
  filter(sentiment=="fear", .preserve = TRUE)

fear_data_n <- data2 %>% 
  group_by(turn) %>% 
  mutate(fear_use = sum(n), .keep = "all") %>%
  ungroup()

# Bootstrapping larger sample of term frequency per turn

fear_dabest <- fear_data_n %>% select(who, fear_use) %>% 
  dabest(x = who,
         y= fear_use,
         idx= c("TRUMP", "BUSH", "CRUZ", "FIORINA"),
         paired = FALSE)

# Visualizing average mean difference between the percent

fear_dabest %>% mean_diff() %>% plot()

fear_dabest %>% mean_diff
