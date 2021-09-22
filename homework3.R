# Homework 3 ------------------------------------------------------------------

## Importing the appropriate data from the shared datasets. 
# Opening up libraries & getting into the correct directory

# hi

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
setwd("datasets/gop_frags/")


# Listing all of the files in /gopfrags in a chr value

files <- list.files()

# Mapping the files' data into a list

data <- map(files,function(x) read_csv(x))

# Binding data and the names of their respective files
data2 <- map2(files,data, function(x,y) cbind(x,y))

# Putting data and file names in one dataframe so that each fragment is attached
# to a corresponding file name.

df <- do.call(rbind, data2)

## Analyzes the speaking complexity of each speaking turn using an appropriate 
## complexity metric. 
# Making two new dataframe columns: speaker and Flesch reading ease score.
# I am removing the "speaker" name from the txt col so that the syllables in a
# name don't accidentally contribute complexity.

df2 <- df %>%
  separate(text, c("speaker", "text"), sep = ":") %>% 
  bind_cols(textstat_readability(df$text,measure = c("FOG")))

## Aggregates the data by candidate and returns a two column data frame: speaker
## | ave_complexity in descending order of complexity.

df2 %>% 
  group_by(speaker) %>% 
  summarise(ave_complexity = mean(FOG)) %>%
  arrange(desc(ave_complexity))
