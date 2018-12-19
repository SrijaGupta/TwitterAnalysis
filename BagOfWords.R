#------------------------***** Bag of words classification *****------------------------#

setwd("/Users/srijagupta/Desktop/Twitterchats")
data.tm<-read.csv('dataset.csv',header = T)
colnames(data.tm)

text.data<-data.tm[27000:36000,17]

library(topicmodels)
library(tm)

Corpus <-VCorpus(VectorSource(text.data))
Corpus <-tm_map(Corpus,content_transformer(tolower))
Corpus <-tm_map(Corpus,removeNumbers)
Corpus <-tm_map(Corpus,removePunctuation)

library(tidytext)
sentiments
get_sentiments('bing')

#install.packages('qdap')
library(qdap)

# bracketX(): Remove all text within brackets (e.g. “It’s (so) cool” becomes “It’s cool”)
# replace_number(): Replace numbers with their word equivalents (e.g. “2” becomes “two”)
# replace_abbreviation(): Replace abbreviations with their full text equivalents (e.g. “Sr” becomes “Senior”)
# replace_contraction(): Convert contractions back to their base words (e.g. “shouldn’t” becomes “should not”)
# replace_symbol() Replace common symbols with their word equivalents (e.g. “$” becomes “dollar”)

#------------------------***** Categorise tweet *****------------------------#

# reading bag of list for each category 
alert.words<-tolower(scan('alert.txt', what = 'character', comment.char = '',sep=','))
awareness.words<-tolower(scan('awareness.txt', what = 'character', comment.char = '',sep=','))
greetings.words<-tolower(scan('greetings.txt', what = 'character', comment.char = '',sep=','))
healthEvent.words<-tolower(scan('healthEvent.txt', what = 'character', comment.char = '',sep=','))
healthEducation.words<-tolower(scan('healthEducation.txt', what = 'character', comment.char = '',sep=','))
tips.words<-tolower(scan('tips.txt', what = 'character', comment.char = '',sep=','))
news.words<-tolower(scan('news.txt', what = 'character', comment.char = '',sep=','))
organizationalUpdate.words<-tolower(scan('organizationalUpdate.txt', what = 'character', comment.char = '', sep=','))
others.words<-tolower(scan('others.txt', what = 'character', comment.char = '', sep=','))

#read tweet
library(readxl)
data.classify<-read_excel('twitter_all_cities.xlsx')
data.classify<-as.data.frame(data.classify)
data.classify$Rcategory<-NA # adding new column
colnames(data.classify)

tweet.data<-data.classify[data.classify$State=='MI',]
done.tweet<-data.classify[32451:32720,15]

library(topicmodels)
library(tm)

# News Category tweet
categorise.word.news<- function(sentences,pos.words){
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentence,pos.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    sentence<- tolower(sentence)
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,pos.words)
    pos.matches<-!is.na(pos.matches)
    score<-sum(pos.matches)
    return(score)
  },pos.words)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}
news<-categorise.word.healthEvent(tweet.data$Tweet,news.words)
tweet.data$Rcategory[news$score>0]<-'News'

# Tips Category tweet
categorise.word.tips<- function(sentences,pos.words){
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentence,pos.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    sentence<- tolower(sentence)
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,pos.words)
    pos.matches<-!is.na(pos.matches)
    score<-sum(pos.matches)
    return(score)
  },pos.words)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}
tips<-categorise.word.healthEvent(tweet.data$Tweet,tips.words)
tweet.data$Rcategory[tips$score>0]<-'Tips'

# Greetings Category tweet
categorise.word.greetings<- function(sentences,pos.words){
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentence,pos.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    sentence<- tolower(sentence)
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,pos.words)
    pos.matches<-!is.na(pos.matches)
    score<-sum(pos.matches)
    return(score)
  },pos.words)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}
greetings<-categorise.word.healthEvent(tweet.data$Tweet,greetings.words)
tweet.data$Rcategory[greetings$score>0]<-'Greetings'

# Awareness Category tweet
categorise.word.awareness<- function(sentences,pos.words){
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentence,pos.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    sentence<- tolower(sentence)
    
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,pos.words)
    pos.matches<-!is.na(pos.matches)
    score<-sum(pos.matches)
    return(score)
  },pos.words)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}

awareness<-categorise.word.awareness(tweet.data$Tweet,awareness.words)
tweet.data$Rcategory[awareness$score>0]<-'Awareness'
