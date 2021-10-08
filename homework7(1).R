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
nrc <- nrc %>% filter(sentiment=="fear")

# Removing speaker names, filtering speakers, and unnesting tokens
data = df %>%
  separate(text, c(NA,"text"), sep = ":") %>%
  filter(who == "BUSH" | who == "CRUZ"|who == "FIORINA"|who == "TRUMP") %>%
  unnest_tokens(word, text, token = "words")

data <- cbind(turn = rownames(data), data)

# Here, I  tested all three methods & preferred the lemmatization results

# df_stemmed <- df %>% mutate(word=stem_words(word))
# nrc_stemmed <- nrc %>% mutate(word=stem_words(word))
# data1 = df_stemmed %>% left_join(nrc_stemmed)

data_lemma <- data %>% mutate(word=lemmatize_words(word))
nrc_lemma  <- nrc %>% mutate(word=lemmatize_words(word))
data2 = data_lemma  %>% left_join(nrc_lemma)

# data3 = df %>% regex_left_join(nrc) %>% group_by(word.x)

nrc_analysis <- data2 %>%
  inner_join(nrc)

# Filtering for fear data, getting counts, calculating TF-IDF

fear_data = nrc_analysis

fear_data_n <- fear_data %>%
  count(turn, word, sort = TRUE)

fear_data_n <- fear_data_n %>% left_join(fear_data)

total_words <- fear_data_n %>% 
  group_by(turn) %>% 
  summarize(total = sum(n))

fear_data_n <- left_join(fear_data_n, total_words)

fear_data_n <- fear_data_n %>%
  bind_tf_idf(word, turn, n)

fear_data_n <- fear_data_n %>% 
  group_by(turn) %>% 
  summarize(freq = sum(tf))

# sum term frequency by speaking turn

data %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

# average frequency across speaking turns by candidate

# display/show meaningful difference

# Creating visualization

fear_data_n %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(who) %>% 
  slice(1:15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf, fill = who)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~who, ncol = 2, scales = "free") +
  coord_flip()


#---------------------------------------------------------------------NOTES

#Maybe I shouldn't group by fear too early because then the tf will be out of 1
#Then I need to average across all turns by candidate
#I need to find a visualization that shows the differences based on Trump
#It doesn't matter *what* the fear words are; it just matters their frequency.