
# Imports the appropriate data from the shared datasets. 

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
setwd("datasets/gop_frags/")

files <- list.files()
data <- map(files,function(x) read_csv(x))
data2 <- map2(files,data, function(x,y) cbind(x,y))
df <- do.call(rbind, data2)

# Analyzes the speaking complexity of each speaking turn using an appropriate 
# complexity metric. 

df2 <- df %>%
  separate(text, c("speaker", "text"), sep = ":") %>% 
  bind_cols(textstat_readability(df$text,measure = c("FOG")))

target <- c("BUSH", "CRUZ", "WALKER")

trump_tweets = filter(df2, speaker == "TRUMP")
bcw_tweets = filter(df2, speaker == target)

jar = c(trump_tweets, bcw_tweets) %>% as.factor()
jar = jar$FOG

jarsample = replicate(100, sample(jar, 10)) %>% 
  table() %>% data.frame()


bcwsample = replicate(100, sample(bcw_tweets$FOG, 10)) %>% 
  table() %>% data.frame()





# Calculates and visualizes the mean difference in speaking complexity for 
# each of the above named candidates.

mean_diff <- trumpsample %>%
  dabest(trump_freq, trump_fog, 
         idx = c(trump_fog), 
         paired = FALSE) %>% mean_diff()



# just in case

names(trumpsample)[1] <- "trump_fog"
names(trumpsample)[2] <- "trump_freq"
names(bcwsample)[1] <- "bcw_fog"
names(bcwsample)[2] <- "bcw_freq"