
## Imports the appropriate data from the shared datasets. 

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
setwd("datasets/gop_frags/")

# Making the filenames into a df, names are bound to data from files

files <- list.files()
data <- map(files,function(x) read_csv(x))
data2 <- map2(files,data, function(x,y) cbind(x,y))
df <- do.call(rbind, data2)

# Analyzes the speaking complexity of each speaking turn using an appropriate 
# complexity metric. 

# Taking the speaker out of the text; using the fog index for each turn

df2 <- df %>%
  separate(text, c("speaker", "text"), sep = ":") %>% 
  bind_cols(textstat_readability(df$text,measure = c("FOG")))

# Separating out by speaker

bush_tweets = filter(df2, speaker == "BUSH")
cruz_tweets = filter(df2, speaker == "CRUZ")
trump_tweets = filter(df2, speaker == "TRUMP")
walker_tweets = filter(df2, speaker == "WALKER")
bush = bush_tweets$FOG %>% as.factor()
cruz = cruz_tweets$FOG %>% as.factor()
trump = trump_tweets$FOG %>% as.factor()
walker = walker_tweets$FOG %>% as.factor()


# Getting more representative samples by bootstrapping data for each speaker

bushsample = replicate(100, sample(bush, 10)) %>% 
  table() %>% data.frame()
names(bushsample)
summary(bushsample)
hist(bushsample)

cruzsample = replicate(100, sample(cruz, 10)) %>% 
  table() %>% data.frame()
names(cruzsample)[1] <- "cruz_fog"
summary(cruzsample)
hist(as.numeric(cruzsample))

trumpsample = replicate(100, sample(trump, 10)) %>% 
  table() %>% data.frame()
names(trumpsample)[1] <- "trump_fog"
summary(trumpsample)
hist(trumpsample$trump_fog)

walkersample = replicate(100, sample(walker, 10)) %>% 
  table() %>% data.frame()
names(walkersample)[1] <- "walker_fog"
summary(walkersample)
hist(walkersample$walker_fog)


# Calculates and visualizes the mean difference in speaking complexity for 
# each of the above named candidates.

red_blue_mean_diff <- samples_long %>%
  dabest(name, value, 
         idx = c("percent_blue", "percent_red"), 
         paired = FALSE) %>% mean_diff()
