#------------------------***** Sentiment Analysis *****------------------------#

# load in the libraries we'll need
library(tidyverse)
install.packages('tokenizers')
install.packages('tidytext')
library(tidytext)
library(glue)
library(stringr)
library('ROAuth')
library('RCurl')
library(twitteR)
library(purrr)
library(dplyr)
library(plyr)


# sentiment score function
score.sentiment<- function(sentences,pos.words,neg.words){
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentence,pos.words,neg.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,pos.words)
    neg.matches<-match(words,neg.words)
    pos.matches<-!is.na(pos.matches)
    neg.matches<-!is.na(neg.matches)
    score<-sum(pos.matches)-sum(neg.matches)
    return(score)
  },pos.words,neg.words,.progress = .progress)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}

# reading positive and negative words
pos.words<-scan('positive.txt', what = 'character', comment.char = '')
neg.words<-scan('negative.txt', what = 'character', comment.char = '')

test.score<-score.sentiment(data$Tweet,pos.words,neg.words)

# adding score column to the data
data2<-cbind(dat,score=test.score$score)
write.csv(data2,file='senti.csv')

#------------------------***** Topic Modeling *****------------------------#

getwd()
setwd("/Users/srijagupta/Desktop/IDS515 InfoSysStrategy&Mgmnt/Project")
data.tm<-read.csv('dataset.csv',header = T)
colnames(data.tm)
text.data<-data.tm[27000:36000,17]

install.packages('topicmodels')
library(topicmodels)
library(tm)

topm<- function(sentences,.progress='none'){
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentences){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    sentence<-tolower(sentence)
    sentence <- tm_map(sentence,stemDocument)
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
  })}

myStopwords <- c("can", "say","one","way","use","also","howev","tell","will",
                 "much","need","take","tend","even", "like","particular","rather","said",
                 "get","well","make","ask","come","end","first","two","help","often","may",
                 "might","see","someth","thing","point","post","look","right","now","think","‘ve ",
                 "‘re ","anoth","put","set","new","good","want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar","littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw","last","never","brief","bit","entir","brief","great","lot")

stopList <- read.csv( 'stopWord.csv',header = T ) 


Corpus <-VCorpus(VectorSource(text.data))
Corpus <-tm_map(Corpus,content_transformer(tolower))
Corpus <-tm_map(Corpus,removeNumbers)
Corpus <-tm_map(Corpus,removePunctuation)
Corpus <-tm_map(Corpus,removeWords,stopList$List)
#Corpus <-tm_map(Corpus,removeWords,stopwords())
Corpus <-tm_map(Corpus,stemDocument)
Corpus <-tm_map(Corpus,stripWhitespace)
dtm<- DocumentTermMatrix(Corpus)
dtm<-removeSparseTerms(dtm,0.99)
ds<-as.data.frame(as.matrix(dtm))

frequent_terms <- freq_terms(ds, 30)
plot(frequent_terms)

tt<-ds[!(rowSums(ds)==0),]
tt
#collapse matrix by summing over columns
freq <- colSums(ds)
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
fr<-freq[ord]
fr
library(topicmodels)
ap_lda <- LDA(tt, k = 10, control = list(seed = 1234))
ap_lda

install.packages('tidytext')
library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Enhance
install.packages('wordcloud')
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(321)
#limit words by specifying min frequency
wordcloud(names(fr),fr, min.freq=95,colors=brewer.pal(5,"Dark2"))