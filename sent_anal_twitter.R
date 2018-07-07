library(twitteR)

api_key<-'xxx'
api_secret<-'xxx'
access_token<-'xxx'
access_token_secret<-'xxx'

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
tweets<-searchTwitter('@brazil',n=1000,lang = 'en')
tweets
tweetsdf<-twListToDF(tweets)
tweetsdf
write.csv(tweetsdf,file='C:/Users/user/Desktop/sentiment/wc.csv',row.names = F)
getwd()
setwd("C:/Users/user/Desktop/sentiment")
wc<-read.csv("wc.csv")

#clean tweets

library(tm)
mycorpus<-Corpus(VectorSource(tweetsdf$text))
mycorpus<-tm_map(mycorpus,removeWords,stopwords())
remove_url<-function(x)gsub("http[^[:space:]]*","",x)
mycorpus<-tm_map(mycorpus,content_transformer(remove_url))
remove_punct<-function(x)gsub("[^[:alpha:][:space:]]*","",x)
mycorpus<-tm_map(mycorpus,content_transformer(remove_punct))
mycorpus<-tm_map(mycorpus,content_transformer(tolower))
mycorpus<-tm_map(mycorpus,stripWhitespace)
mycorpus<-tm_map(mycorpus,stemDocument)

#wordcloud

library(wordcloud)
wordcloud(mycorpus,min.freq = 5,random.order = F)


#setiment

#library(syuzhet)
library(sentimentr)
tweetsdf$text<-gsub("[^0-9A-Za-z///']","",tweetsdf$text)
tweetsdf$text<-gsub("http\\w+","",tweetsdf$text)
tweetsdf$text<-gsub("rt","",tweetsdf$text)
tweetsdf$text<-gsub("@\\w+","",tweetsdf$text)
tweetsdf$text<-tolower(tweetsdf$text)

brazil_tweets<-iconv(tweetsdf$text)

s<-sentiment(brazil_tweets)
s
plot(s)
extract_sentiment_terms(brazil_tweets)
