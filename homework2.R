
# Homework #2 -------------------------------------------------------------

# Opening up the tidyverse package & getting in the right directory
library(tidyverse)
setwd("datasets/gop_frags/")

# Making a new variable called files (a list of all file names in gop_frags).
files <- list.files()

# This makes a list named "data." Unlike the previous list, this one has the
# text from the files, not just the file names.
data <- map(files,function(x) read_csv(x))

# This binds the previous list ("data") with the file names from "files."
gop_data <- map2(files,data, function(x,y) cbind(x,y))

# Making a df of all fragments, not separated by file or speaker.
gop_df <- do.call(rbind,gop_data)

# Naming the "Date" column date in gop_df from the default name
names(gop_df)[1] <- "date"

# Here we are making a new dataframe that separates the date from anything after
# the "." (meaning ".csv"). Then we separate anything after the colon in the
# text column. We name anything before the colon "speaker" and don't delete it
# from the text column. Lastly, we count the characters in the text column.
df1 <- gop_df %>% 
  separate(date,"date",sep = "\\.") %>% 
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  mutate(text_length = nchar(text))

# Here we make a second new df. We group by the speaker. Talking turns indicates
# the number of different fragments have that speakers' name attached.
# Each speaker gets a value of total length that is all of their lengths added
# together. They also get an average length, which is, well, the average.
# In the last line, we rename the columns to "variable" and "value."
df2 <- df1 %>% 
  group_by(speaker) %>% 
  summarise(talking_turns = n(), 
            total_length = sum(text_length),
            ave_length = mean(text_length)) %>% 
  pivot_longer(-speaker,names_to = "variable", values_to = "value")
