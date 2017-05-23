contain_speaker <- function(x){
  if(grepl("<b>.*</b>:", x)){
    return(str_extract(x, "<b>.*</b>:") %>% str_replace_all("<b>|</b>:", ""))
  } else {
    return("null")
  }
}

snl_date_formating <- function(x){
  x <- as.numeric(x)
  if(x > 70) {
    return(1900 + x) 
  } else {
    return(2000 + x)
  }
}

contains_word <- function(phrase, word) {
  x <- grepl(word, phrase, ignore.case = TRUE)
  return(x)
}
