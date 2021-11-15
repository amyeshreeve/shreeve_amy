library(RedditExtractoR)
library(openxlsx)

lgbtwords <- "lgbt, gay, lesbian, bisexual, trans, queer, same-sex, gender, attraction"


excontent <- get_reddit(
  subreddit = "exmormon",
  search_terms = "gay",
  cn_threshold = 50
)


ldscontent <- get_reddit(
  subreddit = "latterdaysaints",
  search_terms = "gay",
  page_threshold = 10,
  cn_threshold = 50
)

write.xlsx(excontent, 'ExMormonData.xlsx')
write.xlsx(ldscontent, 'LDSData.xlsx')