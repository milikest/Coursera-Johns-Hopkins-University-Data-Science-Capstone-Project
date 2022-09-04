# Coursera-Johns-Hopkins-University-Data-Science-Capstone-Project
## Text Prediction with NGram Models (Markov Chain)

## Brief
Welcome to text prediction app! This is the capstone project of the Johns Hopkins University Data Science Specialization by Coursera.
Application is about predicting the next word based on the words user typed.
When you run the app and fill the box with at least one word, predictions will be shown downside of the box.
Application will try to predict next word based on previous word or words you've typed. This application uses ngram models.
Ngram models are created from Swiftkey Dataset. Dataset has 4 langauge sets. We used the English language. 
And three kind of resources in the dataset (Blogs, News, Twitter) are combined.
Up to sixgram models are being used to predict the word. After you have typed more than 5 words, only last 5 words will be considered.
There are 3 predictions from different n gram models. But sometimes some models will not produce a prediction.
First prediction can take time but after that predictions will be much more faster.

## Dependencies
- shiny
- dplyr
- tidyr
- tidytext
- stringr
- tibble

## Resources

Swiftkey data set: 

https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

Shiny app link for the application:

https://c94mnx-mehmet-0l0k.shinyapps.io/Text_Prediction/

## Screenshots
![Screenshot 2022-09-04 at 23-17-42 https __c94mnx-mehmet-0l0k shinyapps io](https://user-images.githubusercontent.com/30753164/188331935-8ce29281-014e-4656-9072-32faa76f74ad.png)

![Screenshot 2022-09-04 at 23-18-58 https __c94mnx-mehmet-0l0k shinyapps io](https://user-images.githubusercontent.com/30753164/188331963-539321cd-f0c3-4726-bdfd-69dd5ba3005f.png)
