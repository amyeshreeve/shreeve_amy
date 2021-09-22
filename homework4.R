## Imports the appropriate data from the shared datasets. 

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
df = read_csv("datasets/gop_debates.csv")

# Analyzes the speaking complexity of each speaking turn using an appropriate 
# complexity metric. 

# Taking the speaker out of the text; using the ELF for each turn
df2 <- df %>% 
  bind_cols(textstat_readability(df$text,measure = c("ELF")))

# Calculates and visualizes the mean difference in speaking complexity for 
# each of the above named candidates.

mean_diff_speakers <- df2 %>%
  dabest(who, ELF, 
         idx = c("TRUMP", "BUSH", "WALKER", "CRUZ"), 
         paired = FALSE) %>% mean_diff()

# Seeing the figures for each speaker and their mean differences

mean_diff_speakers

# Plotting the unpaired mean difference

mean_diff_speakers %>% plot()
