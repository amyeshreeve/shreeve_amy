# Homework 3 ------------------------------------------------------------------


library(tidyverse)
library(quanteda)
library(quanteda.textstats)
setwd("datasets/gop_frags/")


files <- list.files()


data <- map(files,function(x) read_csv(x))


gop <- map2(files,data, function(x,y) cbind(x,y))


df <- do.call(rbind,gop)


df2 <- df %>%
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  bind_cols(textstat_readability(df$text,measure = c("Flesch")))


df2 %>% 
  group_by(speaker) %>% 
  summarise(ave_complexity = mean(Flesch)) %>%
  arrange(desc(ave_complexity))