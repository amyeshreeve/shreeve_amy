## Imports the appropriate data from the shared datasets. 

library(tidyverse)
library(tidytext)
library(textstem)
df = read_csv("datasets/gop_debates.csv")

# Getting terms

stem_strings("immigration border")
lemmatize_strings("immigration border")
im <- "immigr|border"

## Conducts a lemmatized framegram analysis comparing each of the candidates.  

df$text = lemmatize_strings(df$text)

df = df %>%
  separate(text, c(NA,"text"), sep = ":") %>%
  filter(who == "TRUMP" | who == "RUBIO"|who == "CRUZ")

# Stopwords

stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")

## Visualizes the top 10 most frames for each of the above named candidates. 

df %>%
  filter(who == "CRUZ") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  group_by(who) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  filter(str_detect(trigram,im)) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
  
df %>%
  filter(who == "RUBIO") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  group_by(who) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  filter(str_detect(trigram,im)) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
  
df %>%
  filter(who == "TRUMP") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  group_by(who) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  filter(str_detect(trigram,im)) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
