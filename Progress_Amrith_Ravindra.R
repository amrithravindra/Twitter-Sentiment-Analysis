####### Installing Packages and general setup #######
install.packages("ROAuth")
install.packages("tm")
install.packages("tau")
install.packages("twitteR")
install.packages("RCurl")
install.packages("dplyr")
library(ROAuth)
library(tm)
library(tau)
library(twitteR)
library(RCurl)
library(dplyr)

####### Code to make API calls to Twitter from R #######
apiKey <- "T8MeyvVlScOC561yhfmmFWLF6"
apiSecret <- "98gWcdrfYc8DgcUSPlnW5WQmXnzJ5EBDjIvnALgKVnFpt5jRiY"
actoken <- "233154170-BrNDcKzIoafa8yFDrbekMFTGTAdW4zuCQaJ0dASs"
acsecret <- "DsaIFD91FRaCvVYHNF0D2pyNQusJ9eeWmLm5c1qY3qwRe"

setup_twitter_oauth(apiKey, apiSecret, actoken, acsecret)

####### Text mining of tweets #######
BBC <- userTimeline('BBCWorld',n=3000, maxID = NULL, sinceID = NULL, includeRts = FALSE, excludeReplies = TRUE)
BBC_df <- do.call("rbind", lapply(BBC, as.data.frame))

CNN <- userTimeline('CNN',n=3000, maxID = NULL, sinceID = NULL, includeRts = FALSE, excludeReplies = TRUE)
CNN_df <- do.call("rbind", lapply(CNN, as.data.frame))

USEmbassy <- userTimeline('USEmbassySyria',n=3000, maxID = NULL, sinceID = NULL, includeRts = FALSE, excludeReplies = TRUE)
USEmbassy_df <- do.call("rbind", lapply(USEmbassy, as.data.frame))

Trump <- userTimeline('realDonaldTrump',n=3000, maxID = NULL, sinceID = NULL, includeRts = FALSE, excludeReplies = TRUE)
Trump_df <- do.call("rbind", lapply(Trump, as.data.frame))

tweets <- searchTwitter('ISIS', n = 3000)
tweets_df <- do.call("rbind", lapply(tweets, as.data.frame))

tweets_AlAssad <- searchTwitter('BasharAlAssad', n = 3000)
tweets_AlAssad_df <- do.call("rbind", lapply(tweets_AlAssad, as.data.frame))

tweets_Putin <- searchTwitter('Putin', n = 3000)
tweets_Putin_df <- do.call("rbind", lapply(tweets_Putin, as.data.frame))

tweets_Syria <- searchTwitter('Syria', n = 3000)
tweets_Syria_df <- do.call("rbind", lapply(tweets_Syria, as.data.frame))

tweets_Iraq <- searchTwitter('Iraq', n = 3000)
tweets_Iraq_df <- do.call("rbind", lapply(tweets_Iraq, as.data.frame))

####### Text Preprocessing #######
getTransformations()

BBCTweets = select(BBC_df, text)
BBCcorp <- Corpus(DataframeSource(BBCTweets))
BBCcorp_nonums <- tm_map(BBCcorp, removeNumbers)
BBCcorp_nopunc <- tm_map(BBCcorp_nonums, removePunctuation)
BBCcorp_nostopwords <- tm_map(BBCcorp_nopunc, removeWords, stopwords("english"))
BBCcorp_strpwsp <- tm_map(BBCcorp_nostopwords, stripWhitespace)

CNNTweets = select(CNN_df, text)
CNNcorp <- Corpus(DataframeSource(CNNTweets))
CNNcorp_nonums <- tm_map(CNNcorp, removeNumbers)
CNNcorp_nopunc <- tm_map(CNNcorp_nonums, removePunctuation)
CNNcorp_nostopwords <- tm_map(CNNcorp_nopunc, removeWords, stopwords("english"))
CNNcorp_strpwsp <- tm_map(CNNcorp_nostopwords, stripWhitespace)

USEmbassyTweets = select(USEmbassy_df, text)
USEmbassycorp <- Corpus(DataframeSource(USEmbassyTweets))
USEmbassycorp_nonums <- tm_map(USEmbassycorp, removeNumbers)
USEmbassycorp_nopunc <- tm_map(USEmbassycorp_nonums, removePunctuation)
USEmbassycorp_nostopwords <- tm_map(USEmbassycorp_nopunc, removeWords, stopwords("english"))
USEmbassycorp_strpwsp <- tm_map(USEmbassycorp_nostopwords, stripWhitespace)

TrumpTweets = select(Trump_df, text)
Trumpcorp <- Corpus(DataframeSource(TrumpTweets))
Trumpcorp_nonums <- tm_map(Trumpcorp, removeNumbers)
Trumpcorp_nopunc <- tm_map(Trumpcorp_nonums, removePunctuation)
Trumpcorp_nostopwords <- tm_map(Trumpcorp_nopunc, removeWords, stopwords("english"))
Trumpcorp_strpwsp <- tm_map(Trumpcorp_nostopwords, stripWhitespace)

AlAssadTweets = select(tweets_AlAssad_df, text)
AlAssadcorp <- Corpus(DataframeSource(AlAssadTweets))
AlAssadcorp_nonums <- tm_map(AlAssadcorp, removeNumbers)
AlAssadcorp_nopunc <- tm_map(AlAssadcorp_nonums, removePunctuation)
AlAssadcorp_nostopwords <- tm_map(AlAssadcorp_nopunc, removeWords, stopwords("english"))
AlAssadcorp_strpwsp <- tm_map(AlAssadcorp_nostopwords, stripWhitespace)

PutinTweets = select(tweets_Putin_df, text)
Putincorp <- Corpus(DataframeSource(PutinTweets))
Putincorp_nonums <- tm_map(Putincorp, removeNumbers)
Putincorp_nopunc <- tm_map(Putincorp_nonums, removePunctuation)
Putincorp_nostopwords <- tm_map(Putincorp_nopunc, removeWords, stopwords("english"))
Putincorp_strpwsp <- tm_map(Putincorp_nostopwords, stripWhitespace)

SyriaTweets = select(tweets_Syria_df, text)
Syriacorp <- Corpus(DataframeSource(SyriaTweets))
Syriacorp_nonums <- tm_map(Syriacorp, removeNumbers)
Syriacorp_nopunc <- tm_map(Syriacorp_nonums, removePunctuation)
Syriacorp_nostopwords <- tm_map(Syriacorp_nopunc, removeWords, stopwords("english"))
Syriacorp_strpwsp <- tm_map(Syriacorp_nostopwords, stripWhitespace)

IraqTweets = select(tweets_Iraq_df, text)
Iraqcorp <- Corpus(DataframeSource(IraqTweets))
Iraqcorp_nonums <- tm_map(Iraqcorp, removeNumbers)
Iraqcorp_nopunc <- tm_map(Iraqcorp_nonums, removePunctuation)
Iraqcorp_nostopwords <- tm_map(Iraqcorp_nopunc, removeWords, stopwords("english"))
Iraqcorp_strpwsp <- tm_map(Iraqcorp_nostopwords, stripWhitespace)

ISISTweets = select(tweets_df, text)
ISIScorp <- Corpus(DataframeSource(ISISTweets))
ISIScorp_nonums <- tm_map(ISIScorp, removeNumbers)
ISIScorp_nopunc <- tm_map(ISIScorp_nonums, removePunctuation)
ISIScorp_nostopwords <- tm_map(ISIScorp_nopunc, removeWords, stopwords("english"))
ISIScorp_strpwsp <- tm_map(ISIScorp_nostopwords, stripWhitespace)

####### Sentiment Analysis #######

####### Word Cloud Generation #######

####### Clustering #######



