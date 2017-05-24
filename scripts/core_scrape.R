library(rvest)
library(magrittr)
library(dplyr)
library(stringr)
library(stringi)
library(tm)
library(tidyverse)
library(feather)
library(lubridate)

#list of all the seasons.
season_list <- read_html("http://snltranscripts.jt.org/") %>% 
  html_nodes(".c1+ .c1 .c1 > a") %>% 
  html_attr("href") %>% data.frame() %>% 
  set_names(c("season")) %>% 
  slice(4:43) %>% 
  mutate_all(funs(season = as.character))

#pulls a list of all episodes
episdoes <- data.frame()
for(i in 1:40) {
  data <- read_html(season_list$season[i]) %>%
    html_nodes("tr+ tr a") %>% 
    html_attr("href") %>% 
    str_replace_all("http://snltranscripts.jt.org/", "") %>%
    data.frame() %>% 
    set_names(c("episode")) %>%
    mutate_all(funs(episode = as.character)) %>% 
    filter(!grepl("cast", episode)) %>%
    filter(grepl("^[0-9]", episode))
  
  if(grepl("07", data$episode[1])) {
    data <- data %>% slice(2:13)
  }
  
  date_title <- read_html(season_list$season[i]) %>%
    html_nodes(".c1 .c1 .c1 li") %>% 
    html_text() %>%
    data.frame() %>%
    set_names(c("date")) %>%
    filter(grepl("^[0-9]", date)) %>%
    separate(date, c("date", "title"), sep = ":")
    
  data <- cbind(data, date_title)
  
  episdoes <- rbind(episdoes, data)
  print(i)
}

#cleans the urls structures up a bit
episdoes$episode[1:24] <- paste("75/", episdoes$episode[1:24], sep = "")
episdoes$episode[197:214] <- paste("85/", episdoes$episode[197:214], sep = "")


#creates a list of transcripts from each episode
l <- nrow(episdoes)
tran_list <- data.frame()
for(p in 130:l) {
  
  url_data <-  tryCatch({paste0("http://snltranscripts.jt.org/", episdoes$episode[p]) %>% read_html()}, error=function(err) "Error 404")
  if(url == "Error 404") {
    next
  }
  
  new_data <- url_data %>%
    html_nodes("br+ a") %>% html_attr("href") %>%
    data.frame() %>%
    set_names(c("transcript")) %>% 
    filter(grepl("snltranscripts\\.jt\\.org", transcript))
  
  new_data_other<- url_data %>%
    html_nodes("b a") %>% html_attr("href") %>%
    data.frame() %>%
    set_names(c("transcript")) %>% 
    filter(grepl("snltranscripts\\.jt\\.org", transcript))
  
  new_data_extra <- url_data %>%
    html_nodes(".c1 .c1 .c1 a") %>% html_attr("href") %>%
    data.frame() %>%
    set_names(c("transcript")) %>% 
    filter(grepl("snltranscripts\\.jt\\.org", transcript))
  
  if((nrow(new_data) + nrow(new_data_other) + nrow(new_data_extra)) == 0) {
    next
  }
  
  new_data <- rbind(new_data, new_data_other, new_data_extra) %>% unique() %>% cbind(episdoes[p,])

  tran_list <- rbind(tran_list, new_data)
  print(p)
  Sys.sleep(.5)
}

#writing data in feather to local disc incase I accedently delete it
write_feather(tran_list, "data/snl_trans_list.feather")

#loops through the list of every transcript on site and strucutres it in a dataframe
l <- nrow(tran_list)
tran_data <- data.frame()
for(m in 1:l) {
  url <- tryCatch({read_html(paste0(as.character(tran_list$transcript[m])))}, error=function(err) "Error 404")
  if(url == "Error 404") {
    print(paste("skip", m))
    Sys.sleep(10)
    next
  } else {
    
    episode_num <- url %>% html_node("font") %>% html_text()
    
    if(is.na(episode_num)) {
      
    strip_pos <- url %>% html_nodes(".c1 .c1") %>% as.character() %>% str_locate(., "</center>\n<br>")
    
    tran_pipe_other <- url %>% html_nodes(".c1 .c1") %>% 
      as.character() %>% 
      iconv("ASCII", "UTF-8", sub="") %>%
      substr(strip_pos[2]+1, nchar(.)) %>%
      strsplit("<br><br>") %>% 
      unlist() %>%
      str_replace_all("[\n]", "") %>%
      str_replace_all("&#13;|<p>|</p>|<br/>", "") %>%
      iconv("ASCII", "UTF-8", sub="")
    
      if(length(tran_pipe_other) == 0) {
        next
      }
  
    speakers_other <- lapply(tran_pipe_other, contain_speaker) %>% unlist()
    tran_pipe_other <-  str_replace_all(tran_pipe_other, "<b>.*</b>:", "")
    
    speaking_line <- data.frame(speakers = speakers_other, line = tran_pipe_other, episode_num = episode_num, url = as.character(tran_list$transcript[m])) %>% slice(1:(length(tran_pipe_other)-2))
    tran_data <- rbind(tran_data, speaking_line)
    
      next
    }

    tran_pipe <- url %>% html_nodes("center+ p") %>%
      as.character() %>% 
      strsplit("<br><br>") %>% 
      unlist() %>%
      str_replace_all("[\n]", "") %>%
      str_replace_all("&#13;|<p>|</p>|<br/>", "") %>%
      iconv("ASCII", "UTF-8", sub="")
    
    if(length(tran_pipe) == 0) {
      next
    }

    speakers <- lapply(tran_pipe, contain_speaker) %>% unlist()

    tran_pipe <-  str_replace_all(tran_pipe, "<b>.*</b>:", "")
    speaking_line <- data.frame(speakers = speakers, line = tran_pipe, episode_num = episode_num, url = as.character(tran_list$transcript[m]))
    tran_data <- rbind(tran_data, speaking_line)
    
    
    Sys.sleep(1)
    cat(paste0('\r', round((m/l)*100, 2)), "-", m)
    flush.console()
  }
}

#extracting some addition information out of transcript data
tran_data$year <- str_extract(tran_data$url, "[0-9][0-9]")
tran_data$format_year <- lapply(tran_data$year, snl_date_formating) %>% unlist()
tran_data$season <- str_extract(as.character(tran_data$episode_num), ".*:") %>% str_replace(":", "")

#saving dataset before modifying it with joins
write_feather(tran_data, "data/snl_transcript_two.feather")


#downloading rating data from hhllcks github - he previous scrapped IMBD and another SNL website
download.file("https://raw.githubusercontent.com/hhllcks/snldb/master/db/snl_rating.csv", "data/snl_ratings.csv")
download.file("https://raw.githubusercontent.com/hhllcks/snldb/master/db/snl_episode.csv", "data/snl_episdoes_git.csv")
snl_ratings <- read.csv("data/snl_ratings.csv") %>% 
  select(sid, eid, IMDb.users, IMDb.users_avg) %>% 
  arrange(sid, eid) %>%
  mutate(epi_key = paste0(sid,"-",eid))
snl_episodes_git <- read.csv("data/snl_episdoes_git.csv", stringsAsFactors = FALSE) %>% 
  mutate(epi_key = paste0(sid,"-",eid))
snl_ratings <- left_join(snl_ratings, snl_episodes_git, by = c("epi_key" = "epi_key"))

#joining transcript data with list of transcripts for episode meta information
tran_data_join <- left_join(tran_data, tran_list, by = c("url" = "transcript"))

#creating unique key to combine episode ratings and transcript data
snl_ratings$date_key <- mdy(snl_ratings$aired)
tran_data_join$date_key <- mdy(tran_data_join$date)

#join worked great!
tran_data_ratings <- left_join(tran_data_join, snl_ratings, by = "date_key")


#final dataset
write_feather(tran_data_ratings, "data/tran_plus_ratings.feather")

