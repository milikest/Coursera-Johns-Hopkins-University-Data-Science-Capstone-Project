# This is a script written in R language for creating ngram models for the Data Science Capstone Project.
# The purpose of this script is to create a final dataframe to be used by the text predictor application. 
# The dataset has been investigated in the Milestone Report on R Markdown file format.
# Our data is a very large corpus. The first issue will be memory usage. 
# We will make smaller parts of the corpus and clean them and create ngram dataframes
# and save all parts and finally merge all ngram dataframes to create the predictor of text.
# Script will have three parts. At first part we will load libraries and define functions.
# In the second part we will create our ngram dataframes and merge them.
# At the final part we will create a predictor function to predict the next word with ngram models. 

# PART 1
# Loading necessary libraries.

library(tm)
library(readr)
library(stringi)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)

# Creating a function which cleans a string input.

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
  x
}

# Since we have a large corpus, we need to increase the size of connection.

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 10000)

# We are going to create a function that reads the corpus and make a bigram, trigram, fourgram, fivegram and sixgram dataframes.
# Function will read the requested number and index lines of corpus, and make a tibble df.
# Then make this tibble turn into separated sentences.
# And for each sentences it will clear the symbols and chars and etc.
# After cleaning function will crate a ngram dataframe and save it in csv format.
# Function will do it for all ngram dataframes and save it. 

ngram_creater <- function(start, end, file_name, text_path, max_sent = 100){
  for(j in start:end){
    connection <- file(file_name)
    sample_lines <- read_lines(connection, skip = j*100, n_max = max_sent)
    tb_sample <- tibble(sample_lines)
    sample_sentences <- tb_sample %>% unnest_tokens(input = sample_lines, output = "sentences", token = "sentences")
    z = 1
    for(i in 1:dim(sample_sentences)[1]){
      sample_sentences[i,] <- remove_chars_symbols(sample_sentences[i,])
      if(str_count(sample_sentences[i,], '\\w+')>1){
        if(z == 1){
          temp_bigrams <- sample_sentences[i,] %>% unnest_tokens(bigram, sentences, token = "ngrams", n = 2)
          temp_bigrams <- temp_bigrams %>% count(bigram, sort=TRUE)
          seperated_bigrams <- temp_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
          seperated_bigrams$n <- as.integer(seperated_bigrams$n)
        }
        else{
          temp_bigrams <- sample_sentences[i,] %>% unnest_tokens(bigram, sentences, token = "ngrams", n = 2)
          temp_bigrams <- temp_bigrams %>% count(bigram, sort=TRUE)
          temp_bigrams_seperated <- temp_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
          if(!is.na(temp_bigrams_seperated$word1) || !is.na((temp_bigrams_seperated$word2))){
            seperated_bigrams <- seperated_bigrams %>% full_join(temp_bigrams_seperated, by=c("word1","word2"))
            seperated_bigrams[is.na(seperated_bigrams)] = 0
            seperated_bigrams$count <- seperated_bigrams[,c(3)]+ seperated_bigrams[,c(4)]
            seperated_bigrams$count <- as.integer(unlist(seperated_bigrams$count))
            seperated_bigrams <- seperated_bigrams[c("word1","word2","count")]
          }
        }
        z = z + 1
      }
    }
    write.csv(seperated_bigrams,paste0(paste0(paste0(paste0(paste0("./en_US/Bigrams/",text_path),j*100),"."), j*100 + max_sent),".csv"))
    
    z = 1
    for(i in 1:dim(sample_sentences)[1]){
      if(str_count(sample_sentences[i,], '\\w+')>2){
        if(z == 1){
          temp_trigrams <- sample_sentences[i,] %>% unnest_tokens(trigram, sentences, token = "ngrams", n = 3)
          temp_trigrams <- temp_trigrams %>% count(trigram, sort=TRUE)
          seperated_trigrams <- temp_trigrams %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
          seperated_trigrams$n <- as.integer(seperated_trigrams$n)
        }
        else{
          temp_trigrams <- sample_sentences[i,] %>% unnest_tokens(trigram, sentences, token = "ngrams", n = 3)
          temp_trigrams <- temp_trigrams %>% count(trigram, sort=TRUE)
          temp_trigrams_seperated <- temp_trigrams %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
          if(!is.na(temp_trigrams_seperated$word1) || !is.na(temp_trigrams_seperated$word2) || !is.na(temp_trigrams_seperated$word3)){
            seperated_trigrams <- seperated_trigrams %>% full_join(temp_trigrams_seperated, by=c("word1", "word2", "word3"))
            seperated_trigrams[is.na(seperated_trigrams)] = 0
            seperated_trigrams$count <- seperated_trigrams[,c(4)]+ seperated_trigrams[,c(5)]
            seperated_trigrams$count <- as.integer(unlist(seperated_trigrams$count))
            seperated_trigrams <- seperated_trigrams[c("word1","word2","word3","count")]
          }
        }
        z = z + 1
      }
    }
    write.csv(seperated_trigrams,paste0(paste0(paste0(paste0(paste0("./en_US/Trigrams/",text_path),j*100),"."), j*100 + max_sent),".csv"))
    
    z = 1
    for(i in 1:dim(sample_sentences)[1]){
      if(str_count(sample_sentences[i,], '\\w+')>3){
        if(z == 1){
          temp_fourgrams <- sample_sentences[i,] %>% unnest_tokens(fourgram, sentences, token = "ngrams", n = 4)
          temp_fourgrams <- temp_fourgrams %>% count(fourgram, sort=TRUE)
          seperated_fourgrams <- temp_fourgrams %>% separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ")
          seperated_fourgrams$n <- as.integer(seperated_fourgrams$n)
        }
        else{
          temp_fourgrams <- sample_sentences[i,] %>% unnest_tokens(fourgram, sentences, token = "ngrams", n = 4)
          temp_fourgrams <- temp_fourgrams %>% count(fourgram, sort=TRUE)
          temp_fourgrams_seperated <- temp_fourgrams %>% separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ")
          if(!is.na(temp_fourgrams_seperated$word1) || !is.na(temp_fourgrams_seperated$word2) || !is.na(temp_fourgrams_seperated$word3) || 
             !is.na(temp_fourgrams_seperated$word4)){
            seperated_fourgrams <- seperated_fourgrams %>% full_join(temp_fourgrams_seperated, by=c("word1", "word2", "word3", "word4"))
            seperated_fourgrams[is.na(seperated_fourgrams)] = 0
            seperated_fourgrams$count <- seperated_fourgrams[,c(5)]+ seperated_fourgrams[,c(6)]
            seperated_fourgrams$count <- as.integer(unlist(seperated_fourgrams$count))
            seperated_fourgrams <- seperated_fourgrams[c("word1","word2","word3","word4","count")]
          }
        }
        z = z + 1
      }
    }
    write.csv(seperated_fourgrams,paste0(paste0(paste0(paste0(paste0("./en_US/Fourgrams/",text_path),j*100),"."),j*100 + max_sent),".csv"))
    
    z = 1
    for(i in 1:dim(sample_sentences)[1]){
      if(str_count(sample_sentences[i,], '\\w+')>4){
        if(z == 1){
          temp_fivegrams <- sample_sentences[i,] %>% unnest_tokens(fivegram, sentences, token = "ngrams", n = 5)
          temp_fivegrams <- temp_fivegrams %>% count(fivegram, sort=TRUE)
          seperated_fivegrams <- temp_fivegrams %>% separate(fivegram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
          seperated_fivegrams$n <- as.integer(seperated_fivegrams$n)
        }
        else{
          temp_fivegrams <- sample_sentences[i,] %>% unnest_tokens(fivegram, sentences, token = "ngrams", n = 5)
          temp_fivegrams <- temp_fivegrams %>% count(fivegram, sort=TRUE)
          temp_fivegrams_seperated <- temp_fivegrams %>% separate(fivegram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
          if(!is.na(temp_fivegrams_seperated$word1) || !is.na(temp_fivegrams_seperated$word2) || !is.na(temp_fivegrams_seperated$word3) || 
             !is.na(temp_fivegrams_seperated$word4)|| !is.na(temp_fivegrams_seperated$word5)){
            seperated_fivegrams <- seperated_fivegrams %>% full_join(temp_fivegrams_seperated, by=c("word1", "word2", "word3", "word4", "word5"))
            seperated_fivegrams[is.na(seperated_fivegrams)] = 0
            seperated_fivegrams$count <- seperated_fivegrams[,c(6)]+ seperated_fivegrams[,c(7)]
            seperated_fivegrams$count <- as.integer(unlist(seperated_fivegrams$count))
            seperated_fivegrams <- seperated_fivegrams[c("word1","word2","word3","word4","word5","count")]
          }
        }
        z = z + 1
      }
    }
    write.csv(seperated_fivegrams,paste0(paste0(paste0(paste0(paste0("./en_US/Fivegrams/",text_path),j*100),"."),j*100 + max_sent),".csv"))
    
    z = 1
    for(i in 1:dim(sample_sentences)[1]){
      if(str_count(sample_sentences[i,], '\\w+')>5){
        if(z == 1){
          temp_sixgrams <- sample_sentences[i,] %>% unnest_tokens(sixgram, sentences, token = "ngrams", n = 6)
          temp_sixgrams <- temp_sixgrams %>% count(sixgram, sort=TRUE)
          seperated_sixgrams <- temp_sixgrams %>% separate(sixgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
          seperated_sixgrams$n <- as.integer(seperated_sixgrams$n)
        }
        else{
          temp_sixgrams <- sample_sentences[i,] %>% unnest_tokens(sixgram, sentences, token = "ngrams", n = 6)
          temp_sixgrams <- temp_sixgrams %>% count(sixgram, sort=TRUE)
          temp_sixgrams_seperated <- temp_sixgrams %>% separate(sixgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
          if(!is.na(temp_sixgrams_seperated$word1) || !is.na(temp_sixgrams_seperated$word2) || !is.na(temp_sixgrams_seperated$word3) ||  
             !is.na(temp_sixgrams_seperated$word4)|| !is.na(temp_sixgrams_seperated$word5) || !is.na(temp_sixgrams_seperated$word6)){
            seperated_sixgrams <- seperated_sixgrams %>% full_join(temp_sixgrams_seperated, by=c("word1", "word2", "word3", "word4", "word5" ,"word6"))
            seperated_sixgrams[is.na(seperated_sixgrams)] = 0
            seperated_sixgrams$count <- seperated_sixgrams[,c(7)]+ seperated_sixgrams[,c(8)]
            seperated_sixgrams$count <- as.integer(unlist(seperated_sixgrams$count))
            seperated_sixgrams <- seperated_sixgrams[c("word1","word2","word3","word4","word5","word6","count")]
          }
        }
        z = z + 1
      }
    }
    write.csv(seperated_sixgrams,paste0(paste0(paste0(paste0(paste0("./en_US/Sixgrams/", text_path),j*100),"."), j*100 + max_sent),".csv"))
  }
}
cleaner <- function(n=1) {for (i in 1:n) gc()}

# PART 2
# We defined our functions and now we will consider memory usage and our script will read corpus.
# It will read the corpus and make ngram models and save them as dataframes. 
# Creating such big dataframes will make memory decrease in the computer. 
# So we are going to run the ngram_creater function in a loop and after completion it will remove
# the memory of the computer by garbage collector function gc.
# Last sentences will be added to dataframes outside of the loop.
# Although we used garbage collector, R will create temporary files for this large corpus.
# And R will not delete them although they will not be used no longer.
# If we run the script as in below, in less than 100 iterate these temporary files will be larger 
# than 100 gb. So it is not suggested to use it in this way. 
# I had to write another script when this R script was working.
# That script (Temp_File_Delete.ipynb) is going to find these temporary files and delete them.
# And it should delete not the last created file (which in case R is still using it).
# It will delete temp files which are created 5 files before.
# Even though these scripts, these calculations took more than 1 week for my computer.
# My device has i5 10400F 2.90 GHz CPU and 8 GB Ram.
# So please use the script wisely.


for(i in 0:23601){
  ngram_creater(start = i,end = i+1, file_name = './en_US/en_US.twitter.txt', text_path = "Twitter/tweet." )
  cleaner()
}

for(i in 0:772){
  ngram_creater(start = i,end = i+1, file_name = './en_US/en_US.news.txt', text_path = "News/news." )
  cleaner()
}

for(i in 0:8992){
  ngram_creater(start = i,end = i+1, file_name = './en_US/en_US.blogs.txt' , text_path = "Blog/blog." )
  cleaner()
}

# Last Parts

ngram_creater(start = 23601, end = 23601, file_name = './en_US/en_US.twitter.txt', text_path = "Twitter/tweet.", max_sent = 148)

ngram_creater(start = 772, end = 772, file_name = './en_US/en_US.news.txt', text_path = "News/news.", max_sent = 59)

ngram_creater(start = 8992, end = 8992, file_name = './en_US/en_US.blogs.txt', text_path = "Blog/blog.", max_sent = 88)


# Merging All Files
# We will merge files by types of corpus and ngram models.

csv_merger <- function(location, ngram, type){
  directory <- dir(location)
  csv_files <- directory[grepl(".csv",directory)]
  train_set <- ceiling(length(csv_files)*7/10)
  test_set <- length(csv_files) - train_set
  total_set <- c(train_set, test_set)
  for(j in 1:2){
    for(i in 1:total_set[j]){
      if(i == 1){
        df <- read.csv(paste0(location, csv_files[i]))
        df <- df[-c(1)] 
      }
      else{
        df2 <- read.csv(paste0(location, csv_files[i]))
        df2 <- df2[-c(1)]
        if(ngram == "Bigrams"){
          df <- df %>% full_join(df2, by=c("word1","word2"))
          df[is.na(df)] = 0 
          df$count <- df[,c(3)]+ df[,c(4)]
          df <- df[-c(3,4)]
        }
        if(ngram == "Trigrams"){
          df <- df %>% full_join(df2, by=c("word1","word2","word3"))
          df[is.na(df)] = 0
          df$count <- df[,c(4)]+ df[,c(5)]
          df <- df[-c(4,5)]
        }
        if(ngram == "Fourgrams"){
          df <- df %>% full_join(df2, by=c("word1","word2","word3","word4"))
          df[is.na(df)] = 0
          df$count <- df[,c(5)]+ df[,c(6)]
          df <- df[-c(5,6)]
        }
        if(ngram == "Fivegrams"){
          df <-  df %>% full_join(df2, by=c("word1","word2","word3","word4","word5"))
          df[is.na(df)] = 0
          df$count <- df[,c(6)]+ df[,c(7)]
          df <- df[-c(6,7)]
        }
        if(ngram == "Sixgrams"){
          df <-  df %>% full_join(df2, by=c("word1","word2","word3","word4","word5","word6"))
          df[is.na(df)] = 0
          df$count <- df[,c(7)]+ df[,c(8)]
          df <- df[-c(7,8)]
        }
      }
    }
  if(j == 1){
    df <- df[order(-df$count),]
    write.csv(df,paste0(paste0(paste0(paste0(paste0(location, "train."), type),"."), ngram), ".csv"))
  }
  if (j == 2){
    df <- df[order(-df$count),]
    write.csv(df,paste0(paste0(paste0(paste0(paste0(location, "test."), type),"."), ngram), ".csv"))
  }
  }
}

ngrams <- c("Bigrams","Trigrams","Fourgrams","Fivegrams","Sixgrams")
types  <- c("Blog","News","Twitter")

for(i in ngrams){
  for(j in types){
    csv_merger(location = paste0(paste0(paste0(paste0("./en_US/",i),"/"),j),"/"), ngram = i, type=j)
  }
}
partition <- c('train.','test.')

# We can have a look at the final csv files.

for(i in ngrams){
  for(k in partition){
    for(j in types){
        df <- read.csv(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),j),'/'),k),j),'.'),i),'.csv'))
        cat(j,i,k, 'Volume of the file:',dim(df)[1],'\t', 'Sum of the frequencies:', sum(df$count),'\n')
        df <- filter(df, count != 1)
        cat(j,i,k, 'Volume of the file:',dim(df)[1],'\t', 'Sum of the frequencies:', sum(df$count),'\n')
    }
  }
}

# Creating smaller dataframes
# Since our dataframes have too many rows, and considering the words that have only 1 frequencies will not be used,
# we can make our dataframes smaller, by eliminating rows that have only 1 frequency.

ngrams <- c("Bigrams","Trigrams","Fourgrams","Fivegrams","Sixgrams")
types  <- c("Blog","News","Twitter")
partition <- c('train.','test.')

for(i in ngrams){
  for(k in partition){
    for(j in types){
        df <- read.csv(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),j),'/'),k),j),'.'),i),'.csv'))
        if(i == "Bigrams")
          df <- filter(df, count > 2)
        else{
          df <- filter(df, count != 1)
        }
        write.csv(df, paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),j),'/'),k),j),'.summary'),'.'),i),'.csv'))
    }
  }
}

# Merging the trains and tests of same ngrams

for(i in ngrams){
  for(k in partition){
      for(j in types){
        if(j == 'Blog'){
        df <- read.csv(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),j),'/'),k),j),'.summary'),'.'),i),'.csv'))
        df <- df[-c(1,2)]
        }
        if(j == 'News'){
          df2 <- read.csv(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),j),'/'),k),j),'.summary'),'.'),i),'.csv'))
          df2 <- df2[-c(1,2)]
          colons <- colnames(df2)
          df <-  df %>% full_join(df2, by=c(colons))
          df[is.na(df)] = 0
          df <- filter(df, count != 1)
        }
        if(j == 'Twitter'){
          df2 <- read.csv(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),j),'/'),k),j),'.summary'),'.'),i),'.csv'))
          df2 <- df2[-c(1,2)]
          colons <- colnames(df2)
          df <-  df %>% full_join(df2, by=c(colons))
          df[is.na(df)] = 0
          df <- filter(df, count != 1)
          write.csv(df, paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),'total.summary.'),i),'.'),k),"csv"))
        }
    }
  }
}

# PART 3
# We will define a prediction function. The function will take a sentence as an input.
# It will clean the sentence and separate the words and it will filter dataframes
# and look for a suggestion word. If there is no suggestion then it will return NA.
# It will return a dataframe that shows which ngram model offers which word
# and shows the ratio of the word's usage in the dataframe.

predictor <- function(sentence){
  ngrams <- c("Sixgrams","Fivegrams","Fourgrams","Trigrams","Bigrams")
  types  <- c("Blog","News","Twitter")
  partition <- c('train.')
  sentence <- remove_chars_symbols(sentence)
  tb_sample <- tibble(sentence)
  word_count <- str_count(tb_sample, '\\w+')
  if(word_count >= 6){
    ngram <- unnest_tokens(tb_sample, fivegram, sentence, token = "ngrams", n = 5)
    last_words <- ngram[dim(ngram)[1],]
    seperated_words <- last_words %>% separate(fivegram, c("word1", "word2","word3","word4","word5"), sep = " ")
  }
  if(word_count == 5){
    ngram <- unnest_tokens(tb_sample, fourgram, sentence, token = "ngrams", n = 4)
    last_words <- ngram[dim(ngram)[1],]
    seperated_words <- last_words %>% separate(fourgram, c("word1", "word2","word3","word4"), sep = " ")
    ngrams <- c("Fivegrams","Fourgrams","Trigrams","Bigrams")
  }
  if(word_count == 4){
    ngram <- unnest_tokens(tb_sample, trigram, sentence, token = "ngrams", n = 3)
    last_words <- ngram[dim(ngram)[1],]
    seperated_words <- last_words %>% separate(trigram, c("word1", "word2","word3"), sep = " ")
    ngrams <- c("Fourgrams","Trigrams","Bigrams")
  }
  if(word_count == 3){
    ngram <- unnest_tokens(tb_sample, bigram, sentence, token = "ngrams", n = 2)
    last_words <- ngram[dim(ngram)[1],]
    seperated_words <- last_words %>% separate(bigram, c("word1", "word2"), sep = " ")
    ngrams <- c("Trigrams","Bigrams")
  }
  if(word_count == 2){
    ngram <- unnest_tokens(tb_sample, unigram, sentence, token = "ngrams", n = 1)
    last_words <- ngram[dim(ngram)[1],]
    seperated_words <- last_words %>% separate(unigram, c("word1"), sep = " ")
    ngrams <- c("Bigrams")
  }
  x = 1
  for(i in ngrams){
    for(k in partition){
      for(j in types){
        
  ngram_data <- read.csv(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0(paste0('./en_US/',i),'/'),j),'/'),k),j),'.summary'),'.'),i),'.csv'))
  if(i == "Sixgrams"){
  suggestions <- filter(ngram_data, word1 == seperated_words$word1, word2 == seperated_words$word2, word3 == seperated_words$word3, 
         word4 == seperated_words$word4, word5 == seperated_words$word5)
  suggested_word <- suggestions$word6[1]
  }
  if(i == "Fivegrams"){
    suggestions <- filter(ngram_data, word1 == seperated_words$word1, word2 == seperated_words$word2, word3 == seperated_words$word3, 
                           word4 == seperated_words$word4)
    suggested_word <- suggestions$word5[1]
  }
  if(i == "Fourgrams"){
    suggestions <- filter(ngram_data, word1 == seperated_words$word1, word2 == seperated_words$word2, word3 == seperated_words$word3)
    suggested_word <- suggestions$word4[1]
  }
  if(i == "Trigrams"){
    suggestions <- filter(ngram_data, word1 == seperated_words$word1, word2 == seperated_words$word2)
    suggested_word <- suggestions$word3[1]
  }
  if(i == "Bigrams"){
    suggestions <- filter(ngram_data, word1 == seperated_words$word1)
    suggested_word <- suggestions$word2[1]
  }
  if(x == 1){
    ngram_model <- c(i)
    type_of_model <- c(j)
    predicted_word <- c(suggested_word)
    ratio_of_the_prediction <- suggestions$count[1]/sum(ngram_data$count)
  }
  if(x != 1){
    ngram_model <- append(ngram_model, i)
    type_of_model <- append(type_of_model, j)
    predicted_word <- append(predicted_word, suggested_word)
    ratio_of_the_prediction <- append(ratio_of_the_prediction, suggestions$count[1]/sum(ngram_data$count))
  }
  x = x + 1
      }
    }
  }
  prediction <- data.frame(ngram_model = ngram_model, type_of_model =type_of_model,predicted_word = predicted_word, ratio_of_the_prediction =ratio_of_the_prediction )
  return(prediction)
}


sample <- "how dare you to"
pred_df <- predictor(sample)
pred_df
