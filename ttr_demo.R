
# Lexical Diversity Demo --------------------------------------------------
# This is my reconstruction of TTR and LogTTR

#Loading in libraries 
library(tidyverse)
library(textstem)
library(ngram)

# Reading text file in
article <- readChar("npr_article.txt", nchars=50000)

# Counting unique stems (new verb)
unique_stems <- function(x){
  temp <- stem_strings(x)
  temp <- as.list(strsplit(temp, '\\s+')[[1]])
  temp %>% unique() %>% length()}

# Applying to article
unique_stems(article)

# Counting number of words (verb)
words <- function(x){
  as.list(strsplit(x, '\\s+')[[1]]) %>% length()
}

# Getting article TTR
unique_stems(article)/words(article)


# Get article' Herdan's C's LogTTR
log(unique_stems(article))/log(words(article))
