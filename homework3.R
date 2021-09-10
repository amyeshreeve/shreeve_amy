# Homework 3 ------------------------------------------------------------------

## Importing the appropriate data from the shared datasets. 
# Opening up libraries & getting into the correct directory

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
setwd("datasets/gop_frags/")


# Listing all of the files in /gopfrags in a chr value

files <- list.files()

# Mapping the files' data into a list

data <- map(files,function(x) read_csv(x))

# Binding data and the names of their respective files

gop <- map2(files,data, function(x,y) cbind(x,y))

# Putting data and file names in one dataframe

df <- do.call(rbind,gop)

## Analyzes the speaking complexity of each speaking turn using an appropriate 
##complexity metric. 
# Making two new dataframe columns: speaker and Flesch reading ease score.

df2 <- df %>%
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  bind_cols(textstat_readability(df$text,measure = c("Flesch")))

## Aggregates the data by candidate and returns a two column data frame: speaker
## | ave_complexity in descending order of complexity.

df2 %>% 
  group_by(speaker) %>% 
  summarise(ave_complexity = mean(Flesch)) %>%
  arrange(desc(ave_complexity))