## Imports the appropriate data from the shared datasets. 

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(textstem)
df = read_csv("datasets/gop_debates.csv")

# Removing speaker name; sorting for 2015 debates

df = df %>%
  separate(text, c(NA,"text"), sep = ":") %>%
  separate(date, "year", sep = "-", remove = FALSE) %>%
  filter(year < 2016)

# TK -- how to properly lemmatize

df$text = lemmatize_words(df$text)

df_word_n <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(date, word, sort = TRUE)

total_words <- df_word_n %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

df_word_data <- left_join(df_word_n, total_words)

df_word_data <- df_word_data %>%
  bind_tf_idf(word, date, n)

df_word_data %>%
  select(-total) %>%
  arrange(desc(tf_idf))

## Visualizes the top 10 most common terms for each of the 2015 debates. 

df_word_data %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(date) %>% 
  slice(1:10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 2, scales = "free") +
  coord_flip()
