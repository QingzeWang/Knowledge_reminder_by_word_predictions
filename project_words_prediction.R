##Get and Sample the data
library(stringi) #install.packages("stringi")
library(doParallel) # chooseCRANmirror() //  install.packages("doParallel", dependencies = c("Depends", "Imports")) 
library(ggplot2)
library(tm)
library(magrittr)
library(RWeka)
library(data.table)
library(slam)
library(googleVis)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
getDoParWorkers()  #for parallel computation
#start time
strt<-Sys.time()

for (i in 1999:2003){
    file_name <- paste('hep-th-',toString(i),'.tar.gz', sep = "")
    if(!file.exists(file_name)){ #check whether the files exist. if not, download the files
      url_name <- paste('http://www.cs.cornell.edu/projects/kddcup/download/',file_name, sep="")}
      
    if(!dir.exists(toString(i))){#check whether the files are untared. if not, untar the files
      untar(file_name, exdir=".")
    }
}

#input the data into workspace
data_arxiv <- vector()
data_arxiv <- foreach (i = 1999:2003,.combine='c') %dopar%{
  folder = paste("./",toString(i),'/', sep = "")
  filelist <- as.character(list.files(path = folder, all.files = FALSE))
  for (f in filelist) {
    con <- file(paste(folder,f,sep=""), open="r")
    data_arxiv <- c(data_arxiv,readLines(con)) 
    close(con)
  } 
  data_arxiv
}

print(Sys.time()-strt) #time used

save(data_arxiv, file= "data_arxiv.RData")

load("data_arxiv.RData")
data_arxiv  <- sample(data_arxiv, 1e+5) # sample data
save(data_arxiv, file= "sample_arxiv.RData")

#Clean the data and build a function to get a corpus
getCorpus <- function(v) {
  # Processes a vector of documents into a tm Corpus
  corpus <- VCorpus(VectorSource(v))
  corpus <- tm_map(corpus, stripWhitespace)  # remove whitespace
  corpus <- tm_map(corpus, content_transformer(tolower))  # lowercase all
  corpus <- tm_map(corpus, removeWords, stopwords("english"))  # rm stopwords
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("bf" , "the", "a", "is", "we", "author", "date", "theta", "alpha", "otimes",
                                          "bf b", "nucl phys", "phys rev", "phys bf", "citation hepth", "nucl phys bf",
                                          "phys bf b", "emhfill hbox emhfill", "hbox", " lett", "beginequation", "endequation",
                                          "x x x", "cal n", "kcal", "g", "ph", "hepth", "z", "dl", "hfil", "d x", "phys b"))
  corpus 
}

arxiv_Corp <- getCorpus(data_arxiv)

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))


tdm1 <- TermDocumentMatrix(arxiv_Corp, control = list(tokenize = UnigramTokenizer,
                                                                  delimiters = " \\r\\n\\t.,;:\"()?!"))
tdm2 <- TermDocumentMatrix(arxiv_Corp, control = list(tokenize = BigramTokenizer,
                                                                  delimiters = " \\r\\n\\t.,;:\"()?!")) 
tdm3 <- TermDocumentMatrix(arxiv_Corp, control = list(tokenize = TrigramTokenizer,
                                                                  delimiters = " \\r\\n\\t.,;:\"()?!"))
tdm4 <- TermDocumentMatrix(arxiv_Corp, control = list(tokenize = QuadgramTokenizer,
                                                                  delimiters = " \\r\\n\\t.,;:\"()?!"))

# Get word frequencies
tdmToFreq <- function(tdm) {
  # Takes tm TermDocumentMatrix and processes into frequency data.table
  freq <- sort(row_sums(tdm, na.rm=TRUE), decreasing=TRUE)
  word <- names(freq)
  data.table(string=word, count=freq)
}

freq4 <- tdmToFreq(tdm4)
freq3 <- tdmToFreq(tdm3)
freq2 <- tdmToFreq(tdm2)
freq1 <- tdmToFreq(tdm1)
save(freq1,freq2,freq3,freq4,file="freqData_arxiv.RData")


# Process with pre and current word
load("freqData_arxiv.RData")
processGram <- function(dt) {
  # Add to n-gram data.table pre (previous word[s]) and cur (current word itself)
  dt[, c("pre", "cur"):=list(unlist(strsplit(string, "[ ]+?[a-z]+$")), 
                             unlist(strsplit(string, "^([a-z]+[ ])+"))[2]), 
     by=string]
}

freq444 <- processGram(freq4)
freq333 <- processGram(freq3)
freq222 <- processGram(freq2)
freq111 <- freq1[, c("pre", "cur"):=list(string,string),by=string]


save(freq444,freq333,freq222, freq111,file="FinalData_arxiv.RData")

# Write to a database
library(RSQLite)
library(data.table)
load("FinalData_arxiv.RData")
#load("FinalData.RData") ##This is obtained from news, blogs and twitter

words_connection <- dbConnect(SQLite(), dbname="shinyApp/train.db")
dbSendQuery(conn=words_connection,
            "CREATE TABLE NGram
            (pre TEXT,
            word TEXT,
            count INTEGER,
            n INTEGER)")  # ROWID automatically created with SQLite dialect


# Insert into SQLite 
dbsql_insert <- function(sql, key_counts)
{
  dbBegin(words_connection)
  dbGetPreparedQuery(words_connection, sql, bind.data = key_counts)
  dbCommit(words_connection)
}

sql4 <- "INSERT INTO NGram VALUES ($pre, $cur, $count, 4)"
dbsql_insert(sql4, freq444)
dbsql_insert(sql4, freq44)

sql3 <- "INSERT INTO NGram VALUES ($pre, $cur, $count, 3)"
dbsql_insert(sql3, freq333)
dbsql_insert(sql3, freq33)

sql2 <- "INSERT INTO NGram VALUES ($pre, $cur, $count, 2)"
dbsql_insert(sql2, freq222)
dbsql_insert(sql2, freq22)

sql1 <- "INSERT INTO NGram VALUES ($pre, $cur, $count, 1)"
dbsql_insert(sql1, freq111)
dbsql_insert(sql1, freq11)


dbDisconnect(words_connection)

# Explotary data analytics 
#words_arxiv   <- stri_count_words(data_arxiv)
#qplot(words_arxiv)

## unigram plot
#unigramPlot <- gvisColumnChart(freq1[1:15,], "string", "count",                  
#                               options=list(width=750, height=300))
unigramPlot <- gvisPieChart(freq1[1:10,], "string", "count",                  
                            options=list(width=1500, height=600))
plot(unigramPlot)
## bigram plot
bigramPlot <- gvisColumnChart(freq2[1:12,], "string", "count",                  
                              options=list(width=1000, height=400))
## trigram plot
trigramPlot <- gvisColumnChart(freq3[1:12,], "string", "count",                  
                               options=list(width=1000, height = 400))
## quadgram plot
#quadgramPlot <- gvisColumnChart(freq4[1:15,], "string", "count",                  
#                                options=list(legend="none", width=750, height=300))
GT <- gvisMerge(bigramPlot, trigramPlot,horizontal=FALSE) 
plot(GT)

#Gauge <-  gvisGauge(freq1[1:10,], "string", "count",
#                    options=list(min=0, max=800, greenFrom=500,
#                                 greenTo=800, yellowFrom=300, yellowTo=500,
#                                 redFrom=0, redTo=300, width=400, height=300))

#plot(Gauge)

stopCluster(cl)
