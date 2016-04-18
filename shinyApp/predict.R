# Predict, using N-Grams and Stupid Backoff
library(magrittr)
library(stringr)
library(RSQLite)
library(tm)

ngram_backoff <- function(raw, words_connection) {
 
  max = 2  # max n-gram - 1
  
  # process sentence, don't remove stopwords
  sentence <- tolower(raw) %>%
    removePunctuation %>%
    removeNumbers %>%
    stripWhitespace %>%
    str_trim %>%
    strsplit(split=" ") %>%
    unlist
  
  for (i in min(length(sentence), max):1) {
    gram <- paste(tail(sentence, i), collapse=" ")
    sql <- paste("SELECT word, count FROM NGram WHERE ", 
                 " pre=='", paste(gram), "'",
                 " AND n==", i + 1, " LIMIT 5", sep="")
    res <- dbSendQuery(conn=words_connection, sql)
    predicted <- dbFetch(res, n=-1)
    names(predicted) <- c("word", "freq")
#    print(predicted)

    if (nrow(predicted) > 0) return(predicted)
  }
  
  if (nrow(predicted) < 1){
    sql <- paste("SELECT word, count FROM NGram WHERE n==", 1, " LIMIT 5", sep="")
    res <- dbSendQuery(conn=words_connection, sql)
    predicted1 <- dbFetch(res, n=-1)
    names(predicted) <- c("word", "freq")
    #    print(predicted1)
    return(predicted1)}

}