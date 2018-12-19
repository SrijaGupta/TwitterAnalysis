# Health Event Category tweet
categorise.word.healthEvent<- function(sentences,pos.words){
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

healthEvent<-categorise.word.healthEvent(tweet.data$Tweet,healthEvent.words)
tweet.data$Rcategory[healthEvent$score>0]<-'Health Event'

# health Education Category tweet
categorise.word.healthEducation<- function(sentences,pos.words){
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

healthEducation<-categorise.word.healthEvent(tweet.data$Tweet,healthEducation.words)
tweet.data$Rcategory[healthEducation$score>0]<-'Health Education'

# Organizational Update Category tweet
categorise.word.organizationalUpdate<- function(sentences,pos.words){
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
organizationalUpdate<-categorise.word.healthEvent(tweet.data$Tweet,organizationalUpdate.words)
tweet.data$Rcategory[organizationalUpdate$score>0]<-'Organizational Update'

# Alert Category tweet
categorise.word.alert<- function(sentences,pos.words){
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

alert<-categorise.word.alert(tweet.data$Tweet,alert.words)
tweet.data$Rcategory[alert$score>0]<-'Alert'

table(tweet.data$Rcategory)
sum(is.na(tweet.data$Rcategory))

colnames(tweet.data)
# exporting relevant column to a file
mydata<-tweet.data[,c(2,14,18,15)]
write.csv(mydata, "Categorised Tweet.csv")

#------------------------*****  *****  *****------------------------#