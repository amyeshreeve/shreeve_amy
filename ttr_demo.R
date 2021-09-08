
# Lexical Diversity Demo --------------------------------------------------
# This is my reconstruction of TTR and LogTTR

#Load libraries 
library(tidyverse)
library(textstem)
library(ngram)

# Read text file in
article <- readChar("cnn_article.txt", nchars=50000)

# Create a verb that counts the unique number of stems in a string
unique_stems <- function(x){
  temp <- stem_strings(x)
  temp <- as.list(strsplit(temp, '\\s+')[[1]])
  temp %>% unique() %>% length()}

# Apply to sentence 
unique_stems(article)

# Create a verb to get the number of words (even though ngram does this)
words <- function(x){
  as.list(strsplit(x, '\\s+')[[1]]) %>% length()
}

# Get article TTR
unique_stems(article)/words(article)


# Get article Herdan's C
log(unique_stems(article))/log(words(article))
