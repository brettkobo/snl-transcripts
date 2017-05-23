library(pageviews)
library(lubridate)

dsc <- article_pageviews(article = "Dollar Shave Club", start = "2008010100", end = pageview_timestamps(Sys.Date()), granularity = "monthly") %>% 
  select(article, date, views)
harry <- article_pageviews(article = "Harry's", start = "2008010100", end = pageview_timestamps(Sys.Date()), granularity = "monthly") %>% 
  select(article, date, views)

rbind(dsc, harry) %>% filter(views < 5000) %>% ggplot() + geom_line(aes(date, views, color = article))

article_pageviews()


article_pageviews(article = "Dollar Shave Club", start = "2008010100", end = pageview_timestamps(Sys.Date()), granularity = "monthly") %>% 
  select(article, date, views) %>% 
  group_by(article, week = week(date)) %>% 
  summarise(views = sum(views)) %>% 
  ggplot() + geom_line(aes(week, views, color = article))


group_by(week = week(time)) %>% summarise(value = mean(values))
