#Set Working Directory
setwd("~/TwitterSentiment")
#Import datasets
train<-read.csv("~/TwitterSentiment/train_E6oV3lV.csv", stringsAsFactors = FALSE)
test<-read.csv("~/TwitterSentiment/test_tweets_anuFYb8.csv", stringsAsFactors = FALSE)
#lets look at structure of data sets
str(train)
str(test)
#Let us have a look at the label variable in training set
table(train$label)
#Create a label variable in test set and assign it to 0
#test$label<-0
#Convert it to factor
#test$label<-as.factor(test$label)
#Conver label variable in train data set to factor
train$label<-as.factor(train$label)
#Remove foreign language characters
train$tweet<-iconv(train$tweet, to = "ASCII//TRANSLIT")
test$tweet<-iconv(test$tweet, to = "ASCII//TRANSLIT")
#Pre-processing data
#install tm and SnowballC packages and load them
#install.packages("tm")
#install.packages("NLP")
#install.packages("SnowballC")
library(NLP)
library(tm)
library(SnowballC)
#Creating corpus-collection of documents
corpus<-VCorpus(VectorSource(c(train$tweet,test$tweet)))
corpus[[1]]$content
# Convert to lower-case
corpus <- tm_map(corpus, content_transformer(tolower))
#Remove punctuation
corpus<-tm_map(corpus, removePunctuation)
#Remove stopwords
corpus<-tm_map(corpus,removeWords, (stopwords("english")))
#Remove white spaces
corpus <- tm_map(corpus, stripWhitespace)
#Stemming
corpus<-tm_map(corpus, stemDocument)
#Create a matrix to of words frequencies against each document or tweet. tm package provides DocumentTermMatrix
frequencies<-DocumentTermMatrix(corpus)
#Remove sparse terms
sparse <- removeSparseTerms(frequencies, 0.999)
#Convert into dataframe
tweetsSparse <- as.data.frame(as.matrix(sparse))
# Make all variable names R-friendly
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
# Split the data
trainSparse<-head(tweetsSparse,nrow(train))
testSparse<-tail(tweetsSparse,nrow(test))

#Adding these to original dataframes
train_tweet_new<-cbind(train,trainSparse)
test_tweet_new <- cbind(test,testSparse)

#drop original tweet column from data sets
train_tweet_new$tweet<-NULL
test_tweet_new$tweet<-NULL

#let's split train set
library(caTools)
set.seed(123)
split_1<-sample.split(train_tweet_new$label, 0.90)
train_new1<-train_tweet_new[split_1==T,]
test_new1<-train_tweet_new[split_1==F,]

#Build decision tree model
library(rpart)
fit <- rpart(label ~ ., data=train_new1, method="class")

#Build model using Random Forest
#library(randomForest)
#fit <- randomForest(label ~ ., data=train_new1, importance=TRUE, ntree=2000)
Prediction<- predict(fit,test_new1,type="class")

#Calculate accuracy
table(test_new1$label, Prediction)

Prediction_test<-predict(fit,test_tweet_new,type="class")

#Create a dataframe using the prediction we just obtained and combine with the original data set
submit<-data.frame(id=test$id, tweet=test$tweet, label=Prediction_test)
write.csv(submit, file="test_predictions.csv", row.names=FALSE)











