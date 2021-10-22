# Write an R script that performs an automated summative content analysis.

library(tidyverse)
library(tidytext)
library(stringr)
library(textstem)
library(irr)


reddit <- read_csv("utreddit.csv")

sched_words <- ("course|professor|schedule|time|flag|easy|grades|fail|
                instructor|conflict|tips|core|credit|register")
major_words <- ("major|accepted|department|program|transfer|internal")
policy_words <- ("claiming|wifi|syllabus|gpa|waitlist|AP|IB")
finaid_words <- ("money|pell|federal|bill|fafsa|federal|grant|loan|
                  scholarship|debt|cost|expensive|tuition")
housing_words <- ("dorm|roommate|hall|housing|occupancy|room|bedroom| 
                  apartment|rent|maintenance")
food_words <- ("eat|food|kins|jester|dining|meal|grub|ate|lunch|breakfast|
               |dessert|dinner|snack")
entertain_words <- ("game|football|gym|play|union|club|party|friend|fun")
humor_words <- ("domino|lol|lmao|fun|meme")
other_words <- ("sell|work|job|technology|canvas")

regexify <- function(x){
  stems <- stem_words(x)
  lemmas <- lemmatize_words(x)
  c(stems,lemmas,x) %>% unique %>% 
    paste0(.,collapse = "|")
}

# Apply regexify function to terms
sched_words_regex <- regexify(sched_words)
major_words_regex <- regexify(major_words)
policy_words_regex <- regexify(policy_words)
finaid_words_regex <- regexify(finaid_words)
housing_words_regex <- regexify(housing_words)
food_words_regex <- regexify(food_words)
entertain_words_regex <- regexify(entertain_words)
humor_words_regex <- regexify(humor_words)
other_words <- regexify(other_words)

# Apply to all data
bot_reddit <- reddit %>%
  filter(!is.na(post_text)) %>%
  mutate(post_text = tolower(post_text),
         bot_sched = ifelse(str_detect(post_text, sched_words_regex),1,0),
         bot_major = ifelse(str_detect(post_text, major_words_regex),1,0),
         bot_policy = ifelse(str_detect(post_text, policy_words_regex),1,0),
         bot_finaid = ifelse(str_detect(post_text, finaid_words_regex),1,0),
         bot_housing = ifelse(str_detect(post_text, housing_words_regex),1,0),
         bot_food = ifelse(str_detect(post_text, food_words_regex),1,0),
         bot_entertain = ifelse(str_detect(post_text, entertain_words_regex),1,0),
         bot_humor = ifelse(str_detect(post_text, humor_words_regex),1,0),
         bot_other = ifelse(bot_sched + bot_major + bot_policy + bot_finaid +
                              bot_housing + bot_food + bot_entertain +
                              bot_humor == 0), 1,0)

# I understand that the bot_other is making it harder on myself