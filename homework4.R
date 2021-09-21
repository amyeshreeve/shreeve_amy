## Imports the appropriate data from the shared datasets. 

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
setwd("datasets/gop_frags/")

# Making the filenames into a df, names are bound to data from files

files <- list.files()
data <- map(files,function(x) read_csv(x))
data2 <- map2(files,data, function(x,y) cbind(x,y))
df <- do.call(rbind,data2)
names(df)[1] <- "date"

# Analyzes the speaking complexity of each speaking turn using an appropriate 
# complexity metric. 

# Taking the speaker out of the text; using the fog index for each turn

df2 <- df %>% 
  separate(text, "speaker", sep = ":", remove = FALSE) %>%
  bind_cols(textstat_readability(df$text,measure = c("ELF")))

# Calculates and visualizes the mean difference in speaking complexity for 
# each of the above named candidates.

mean_diff_speakers <- df2 %>%
  dabest(speaker, ELF, 
         idx = c("TRUMP", "BUSH", "WALKER", "CRUZ"), 
         paired = FALSE) %>% mean_diff()

# Seeing the figures for each speaker and their mean differences

mean_diff_speakers

# Plotting the unpaired mean difference

mean_diff_speakers %>% plot()
