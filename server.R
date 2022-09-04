# Downloading Ngram Models
url <- "https://github.com/milikest/Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project/blob/main/Ngram%20Models/Bigrams/total.summary.Bigrams.train.zip"
download.file(url, destfile = "./total.summary.Bigrams.train.zip")
unzip("./total.summary.Bigrams.train.zip")

url <- "https://github.com/milikest/Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project/blob/main/Ngram%20Models/Trigrams/total.summary.Trigrams.train.zip"
download.file(url, destfile = "./total.summary.Trigrams.train.zip")
unzip("./total.summary.Trigrams.train.zip")

url <- "https://github.com/milikest/Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project/blob/main/Ngram%20Models/Fourgrams/total.summary.Fourgrams.train.zip"
download.file(url, destfile = "./total.summary.Fourgrams.train.zip")
unzip("./total.summary.Fourgrams.train.zip")

url <- "https://github.com/milikest/Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project/blob/main/Ngram%20Models/Fivegrams/total.summary.Fivegrams.train.zip"
download.file(url, destfile = "./total.summary.Fivegrams.train.zip")
unzip("./total.summary.Fivegrams.train.zip")

url <- "https://github.com/milikest/Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project/blob/main/Ngram%20Models/Sixgrams/total.summary.Sixgrams.train.zip"
download.file(url, destfile = "./total.summary.Sixgrams.train.zip")
unzip("./total.summary.Sixgrams.train.zip")

library(shiny)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(tibble)


bigrams   <- read.csv('./total.summary.Bigrams.train.csv')
trigrams  <- read.csv('./total.summary.Trigrams.train.csv')
fourgrams <- read.csv('./total.summary.Fourgrams.train.csv')
fivegrams <- read.csv('./total.summary.Fivegrams.train.csv')
sixgrams  <- read.csv('./total.summary.Sixgrams.train.csv')

remove_chars_symbols <- function(x){
  x <- gsub("[^ ]{1,}@[^ ]{1,}"," ",x)
  x <- gsub(" @[^ ]{1,}"," ", x)
  x <- gsub("[^ ]{1,}://[^ ]{1,}"," ",x)
  x <- gsub("['??????]","'",x)
  x <- gsub("^a-z']"," ", x)
  x <- gsub("'{2,}", " '", x)
  x <- gsub("' ", " ", x)
  x <- gsub(" '"," ", x)
  x <- gsub("^'","",x)
  x <- gsub("'$","",x)
  x <- gsub("[^\x01-\x7F]+","",x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:digit:]]+","",x)
  x <- tolower(x)
  x
}

predictor <- function(text){
  text_input <- remove_chars_symbols(text)
  tb_sample <- tibble(text_input)
  word_count <- str_count(tb_sample, '\\w+')
  if(word_count >= 5){
    seperated_words <- strsplit(text_input, split = " ")[[1]]
    if(length(seperated_words)==5){ 
    seperated_words <- data.frame(rbind(seperated_words))}
    if(length(seperated_words)>5){
      seperated_words <- seperated_words[(length(seperated_words)-4):length(seperated_words)]
      seperated_words <- data.frame(rbind(seperated_words))
    }
    colnames(seperated_words)<- c("word1","word2","word3","word4","word5")
  }
  if(word_count == 4){
    seperated_words <- strsplit(text_input, split = " ")[[1]]
    seperated_words <- data.frame(rbind(seperated_words))
    colnames(seperated_words)<- c("word1","word2","word3","word4")
  }
  if(word_count == 3){
    seperated_words <- strsplit(text_input, split = " ")[[1]]
    seperated_words <- data.frame(rbind(seperated_words))
    colnames(seperated_words)<- c("word1","word2","word3")
  }
  if(word_count == 2){
    seperated_words <- strsplit(text_input, split = " ")[[1]]
    seperated_words <- data.frame(rbind(seperated_words))
    colnames(seperated_words)<- c("word1","word2")
  }
  if(word_count == 1){
    seperated_words <- strsplit(text_input, split = " ")[[1]]
    seperated_words <- data.frame(rbind(seperated_words))
    colnames(seperated_words)<- c("word1")
  }
  suggested_word_1 <- ""
  suggested_word_2 <- ""
  suggested_word_3 <- ""
  
  if(word_count == 1){
    suggestions <- filter(bigrams, word1 == seperated_words$word1)
    suggested_word_1 <- suggestions$word2[1]
    suggested_word_2 <- ""
    suggested_word_3 <- ""
  }
  if(word_count == 2){
    suggestions <- filter(bigrams, word1 == seperated_words$word2)
    suggested_word_1 <- suggestions$word2[1]
    suggestions_2 <- filter(trigrams, word1 == seperated_words$word1, word2 == seperated_words$word2)
    suggested_word_2 <- suggestions_2$word3[1]
    suggested_word_3 <- ""
  }
  if(word_count == 3){
    suggestions <- filter(bigrams, word1 == seperated_words$word3)
    suggested_word_1 <- suggestions$word2[1]
    suggestions_2 <- filter(trigrams, word1 == seperated_words$word2, word2 == seperated_words$word3)
    suggested_word_2 <- suggestions_2$word3[1]
    suggestions_3 <- filter(fourgrams, word1 == seperated_words$word1, word2 == seperated_words$word2, word3 == seperated_words$word3)
    suggested_word_3 <- suggestions_3$word4[1]
  }
  if(word_count == 4){
    suggestions <- filter(trigrams, word1 == seperated_words$word3, word2 == seperated_words$word4)
    suggested_word_1 <- suggestions$word3[1]
    suggestions_2 <- filter(fourgrams, word1 == seperated_words$word2, word2 == seperated_words$word3, word3 == seperated_words$word4)
    suggested_word_ <- suggestions_2$word4[1]
    suggestions_3 <- filter(fivegrams, word1 == seperated_words$word1, word2 == seperated_words$word2, word3 == seperated_words$word3, 
                            word4 == seperated_words$word4)
    suggested_word_3 <- suggestions_3$word5[1]
  }
  if(word_count >= 5){
    suggestions <- filter(fourgrams, word1 == seperated_words$word3, word2 == seperated_words$word4, word3 == seperated_words$word5)
    suggested_word_1 <- suggestions$word4[1]
    suggestions_2 <- filter(fivegrams, word1 == seperated_words$word2, word2 == seperated_words$word3, word3 == seperated_words$word4, 
                            word4 == seperated_words$word5)
    suggested_word_2 <- suggestions_2$word5[1]
    suggestions_3 <- filter(sixgrams, word1 == seperated_words$word1, word2 == seperated_words$word2, word3 == seperated_words$word3, 
                            word4 == seperated_words$word4, word5 == seperated_words$word5)
    suggested_word_3 <- suggestions_3$word6[1]
    
    if(is.na(suggested_word_1)){
      suggestions <- filter(trigrams, word1 == seperated_words$word4, word2 == seperated_words$word5)
      suggested_word_1 <- suggestions$word3[1]
    }  
    if(is.na(suggested_word_1)){
      suggestions <- filter(bigrams, word1 == seperated_words$word5)
      suggested_word_1 <- suggestions$word2[1]
    }
    if(is.na(suggested_word_2)){
      suggestions <- filter(fourgrams, word1 == seperated_words$word3, word2 == seperated_words$word4, word3 == seperated_words$word5)
      suggested_word_2 <- suggestions$word4[1]
    }
    if(is.na(suggested_word_2)){
      suggestions <- filter(trigrams, word1 == seperated_words$word4, word2 == seperated_words$word5)
      suggested_word_2 <- suggestions$word3[1]
    }
  }
  
  if(is.na(suggested_word_1)){
    suggested_word_1 = ""
  }
  if(is.na(suggested_word_2)){
    suggested_word_2 = ""
  }
  if(is.na(suggested_word_3)){
    suggested_word_3 = ""
  }
  return(list(suggested_word_1, suggested_word_2, suggested_word_3))
}


shinyServer(function(input, output) {
  

    output$predicted_output <-reactive({predictor(input$text_input)}) 


})
