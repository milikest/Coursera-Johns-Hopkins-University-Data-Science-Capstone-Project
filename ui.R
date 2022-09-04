

library(shiny)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(tibble)


shinyUI(fluidPage(

    sidebarLayout(
        sidebarPanel(
        ),

        mainPanel(
          h3("TEXT PREDICTION:"),
          h4("Welcome to text prediction app! This is the capstone project of the Johns Hopkins University Data Science Specialization by Coursera."),
          h4("When you fill the box with at least one word, predictions will be shown downside of the box."),
          h4("Application will try to predict next word based on previous word or words you've typed. This application uses ngram models."),
          h4("Ngram models are created from Swiftkey Dataset. Dataset has 4 langauge sets. We used the English language."),
          h4("And three kind of resources in the dataset (Blogs, News, Twitter) are combined."),
          h4("Up to sixgram models are being used to predict the word. After you have typed more than 5 words, last 5 words will be considered."),
          h4("There are 3 predictions from different n gram models.But sometimes some models will not produce a prediction."),
          h4("First prediction can take time but after that predictions will be much more faster."),
          textInput("text_input",label = "Please type here", width = 800),
          h4('Predictions are:'),
          verbatimTextOutput("predicted_output"),
          h4('Source Code for this project: '),
          tags$a(href="https://github.com/milikest/Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project", "https://github.com/milikest/Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project"),
          h4("Mehmet İLİK"),
          h4("04/09/2022")
        )
    )
))
