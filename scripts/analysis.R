library(rvest)
library(magrittr)
library(dplyr)
library(stringr)
library(stringi)
library(tm)
library(tidyverse)
library(feather)
library(tidytext)
library(purrr)
devtools::install_github("dgrtwo/widyr")
devtools::install_github("bmschmidt/wordVectors")
library(widyr)
library(wordVectors)
library(tsne)

#top words over time

snl_words <- tran_data %>% 
  mutate_each(funs(as.character), speakers, episode_num, line) %>%
  filter(line != "") %>% 
  unnest_tokens(word, line)

word_by_episode <- snl_words %>%
  group_by(episode_num, season, word) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) #%>%
  #top_n(n = 10, n)

#sentiment analysis of episodes by season

senti_by_word <- snl_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarise(count = n(), contributions = sum(score), mult = count*contributions)
  
senti_by_season <- word_by_episode %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(season) %>%
  summarize(contribution = sum(score)) %>%
  mutate(season_num = as.numeric(str_extract(season, "([0-9]+).*$"))) %>%
  mutate(season = reorder(season, season_num))

senti_by_season %>% 
  ggplot() + 
  geom_col(aes(season, contribution, group = 1, fill = contribution > 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#searching for curse words throgh time

#seaching for ass
a_word <- snl_words %>% 
  filter(word %in% c("ass", "asses", "dumbass", "dumb ass")) %>% 
  group_by(season, word) %>%
  count(word,sort = TRUE) %>% 
  mutate(curse_word = "ass") %>% 
  group_by(season, curse_word) %>%
  summarise(count = sum(n)) 
  
#searching for damn
d_word <- snl_words %>% 
  filter(grepl("damn", word)) %>% 
  group_by(season, word) %>%
  count(word,sort = TRUE) %>% 
  mutate(curse_word = "damn") %>% 
  group_by(season, curse_word) %>%
  summarise(count = sum(n)) 

#searching for bitch
b_word <- snl_words %>% 
  filter(grepl("bitch", word)) %>% 
  group_by(season, word) %>% 
  count(word, sort = TRUE) %>% 
  mutate(curse_word = "bitch") %>% 
  group_by(season, curse_word) %>%
  summarise(count = sum(n)) 

curse_words <- rbind(a_word, d_word, b_word) %>%
  ungroup() %>%
  mutate(season_num = as.numeric(str_extract(season, "([0-9]+).*$"))) %>%
  mutate(season = reorder(season, season_num)) 

curse_words %>%  ggplot(aes(season, count, group = curse_word, color = curse_word)) +
  geom_line() +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#word2vec models

write.table(tran_data$line, "input_data/snl_lines.txt", row.names = FALSE, col.names = FALSE)

prep_word2vec(origin = "input_data/snl_lines.txt" , destination="data/word2vec_snl.txt", lowercase = T, bundle_ngrams = 2)

snl_model <- train_word2vec("data/word2vec_snl.txt","data/snl_vector.bin", vectors = 200, threads = 4, window = 12, iter = 5, negative_samples = 0)

plot(snl_model, perplexity = 50)

similar_words <- snl_model %>% closest_to("joke")
fishy <- snl_model[[similar_words$word,average=F]]
plot(fishy,method="pca")


set.seed(10)
centers <- 150
clustering <- kmeans(snl_model, centers=centers, iter.max = 40)

sapply(sample(1:centers,20),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

#bag of words

