install.packages("readr")
library(readr)
sms_raw <- read.csv("C:/Users/Susheel/Desktop/NB/sms_raw.csv")
View(sms_raw)
str(sms_raw)
str(sms_raw$type)
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
prop.table(table(sms_raw$type))
install.packages(tm)
library(tm)
str(sms_raw$text)
Corpus_clean<-tm_map(sms_corpus,tolower)
Corpus_clean<-tm_map(Corpus_clean,removePunctuation)
Corpus_clean<-tm_map(Corpus_clean,removeNumbers)
Corpus_clean<-tm_map(Corpus_clean, removeWords, stopwords())
Corpus_clean<-tm_map(Corpus_clean, stripWhitespace)
library(SnowballC)
inspect(sms_corpus_clean[1:2])
lapply(sms_corpus[1:4], as.character)

as.character(sms_corpus[1:3])
as.character(sms_corpus_clean[1:3])
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
install.packages("wordcloud")
library(wordcloud)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE, 
  stopwords = TRUE, 
  removePunctuation = TRUE, 
  stemming = TRUE))
sms_dtm
sms_dtm2
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
install.packages("klaR")
library(klaR)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
