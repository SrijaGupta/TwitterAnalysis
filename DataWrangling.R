setwd("/Users/srijagupta/Desktop/Twitterchats")

data<-read.csv('Twitter_Data.csv',header = T)
str(data)

#------------------------***** Data Wrangling *****------------------------#

data<-g.data
# We need to check for missing values.
apply(data,2,function(x) sum(is.na(x)))

# replacing blank spaces by 0
data$Favorites[data$Favorites=='']<-0

# replacing NA  by 0
data$Likes[is.na(data$Likes)]<-0
data$Retweet.Count[is.na(data$Retweet.Count)]<-0

# function to count words in a string based on any parametere
countWords <- function( s) {
  s2 <- gsub(',',"",s)
  return ((nchar(s) - nchar(s2))+1)
}

# Updating mention count
data$Mentions....<-as.character(data$Mentions....)
data$Mention.Count<- ifelse(data$Mentions....=='',0 ,countWords(data$Mentions....))
table(data$Mention.Count)

# handling tag count column
data$Tag<-as.character(data$Tag)
data$Tag.Count<- ifelse(data$Tag=='',0 ,countWords(data$Tag))
table(data$Tag.Count)

# splitting date and time into seperate columns
data$Time <- ifelse(data$State=='IL',(format(as.POSIXct(strptime(data$Date[data$State=='IL'],
                                                                 "%m/%d/%y %H:%M",tz="")) ,format = "%H.%M")), 00.00)

data$Date <- ifelse(data$State=='IL',(format(as.POSIXct(strptime(data$Date[data$State=='IL'],
                                                                 "%m/%d/%y %H:%M",tz="")) ,format = "%m/%d/%Y")),
                    format(as.POSIXct(strptime(data$Date[data$State!='IL'],"%m/%d/%y",tz="")) ,format = "%m/%d/%Y"))

# time of the day
data$Time<-as.numeric(data$Time)
data$Time.of.Day <-ifelse(data$Time>00.01 & data$Time <=04.59 , 'LateNight',
                          ifelse(data$Time >04.59 & data$Time <=11.59, 'Morning',
                                 ifelse(data$Time>11.59 & data$Time <=16.00,'Afternoon', 
                                        ifelse(data$Time>16.00 & data$Time <=18.59 , 'Evening',
                                               ifelse(data$Time>18.59 & data$Time <=23.59 , 'Night' ,'Unknown')))))


table(data$Time.of.Day)

data$Time<-as.character(data$Time)
data$Time<-ifelse(data$Time != 0, gsub('[.]',':',data$Time), 0:00)

data$Time[18000]
# output in 
write.csv(data,file='data.csv')

setwd("/Users/srijagupta/Desktop/Twitterchats")
data_2<-read.csv('data.csv',header = T)