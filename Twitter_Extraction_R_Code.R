install.packages("twitteR") #installs TwitteR library (twitteR) #loads TwitteR
library(twitteR)
library(rtweet)
library(tidyverse)
install.packages('tidyverse')
install.packages("httk")
install.packages("httr")
install.packages("httpuv")
install.packages("rtweet")

consumer_key = 'QjA2bFOSHnFeQ4B4pNKWOkBRA'
consumer_secret = 'Dx4wxueiWbn8TlErF5yYAjFrNViZ3mJZNt4H6DrFSmAg4P7o55'
access_token = '138107497-NO02BdvIl9Ym3D6teY4svo8uaQ2v0yfWiDUM5dfA'
access_token_secret = '1n5EIF0CXJOIfJO5T4KRJYTRWhe4eQv8p32FoLJAgpKNU'


appname <- "Twitter_ADBMS_TS"
token <- create_token(app =appname, consumer_key, consumer_secret,set_renv = TRUE)


lrt <- lapply(
  c("Juniper Network automation", "#Juniper Network automation"),
  search_tweets,
  n = 7000,lang = "en"
)

rt9 <- do_call_rbind(lrt)
View(rt9)
rt9 <- as.data.frame(rt9)
str(rt9)
rt9 <- rt9[!duplicated(rt9$text),]
write_as_csv(rt9,'/Users/srijagupta/Desktop/Juni_Network_Automation.csv')

