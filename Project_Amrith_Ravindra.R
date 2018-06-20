####### Installing Packages and general setup #######

install.packages("ROAuth")
install.packages("tm")
install.packages("tau")
install.packages("twitteR")
install.packages("RCurl")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("SnowballC")

library(ROAuth)
library(tm)
library(tau)
library(twitteR)
library(RCurl)
library(dplyr)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(SnowballC)

####### Code to make API calls to Twitter from R #######

apiKey <- "T8MeyvVlScOC561yhfmmFWLF6"
apiSecret <- "98gWcdrfYc8DgcUSPlnW5WQmXnzJ5EBDjIvnALgKVnFpt5jRiY"
actoken <- "233154170-BrNDcKzIoafa8yFDrbekMFTGTAdW4zuCQaJ0dASs"
acsecret <- "DsaIFD91FRaCvVYHNF0D2pyNQusJ9eeWmLm5c1qY3qwRe"
setup_twitter_oauth(apiKey, apiSecret, actoken, acsecret)

####### Text mining of tweets #######

CNN <- userTimeline('CNN',n=3000, maxID = NULL, sinceID = NULL, includeRts = FALSE, excludeReplies = TRUE)
CNN_df <- do.call("rbind", lapply(CNN, as.data.frame))
CNN_df$text <- sapply(CNN_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

USEmbassy <- userTimeline('USEmbassySyria',n=3000, maxID = NULL, sinceID = NULL, includeRts = FALSE, excludeReplies = TRUE)
USEmbassy_df <- do.call("rbind", lapply(USEmbassy, as.data.frame))
USEmbassy_df$text <- sapply(USEmbassy_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

tweets_ISIS <- searchTwitter('ISIS', n = 3000)
tweets_ISIS_df <- do.call("rbind", lapply(tweets_ISIS, as.data.frame))
tweets_ISIS_df$text <- sapply(tweets_ISIS_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

tweets_Putin <- searchTwitter('Putin', n = 3000)
tweets_Putin_df <- do.call("rbind", lapply(tweets_Putin, as.data.frame))
tweets_Putin_df$text <- sapply(tweets_Putin_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

tweets_Syria <- searchTwitter('Syria', n = 3000)
tweets_Syria_df <- do.call("rbind", lapply(tweets_Syria, as.data.frame))
tweets_Syria_df$text <- sapply(tweets_Syria_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

tweets_Iraq <- searchTwitter('Iraq', n = 3000)
tweets_Iraq_df <- do.call("rbind", lapply(tweets_Iraq, as.data.frame))
tweets_Iraq_df$text <- sapply(tweets_Iraq_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

####### Text Preprocessing #######
getTransformations()

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

ISISTweets = select(tweets_ISIS_df, text)
ISIScorp <- Corpus(DataframeSource(ISISTweets))
ISIScorp_nonums <- tm_map(ISIScorp, removeNumbers)
ISIScorp_nopunc <- tm_map(ISIScorp_nonums, removePunctuation)
ISIScorp_nostopwords <- tm_map(ISIScorp_nopunc, removeWords, stopwords("english"))
ISIScorp_strpwsp <- tm_map(ISIScorp_nostopwords, stripWhitespace)

####### Removal of website links and other symbols #######

tempCNN = data.frame(text = sapply(CNNcorp_strpwsp, as.character), stringsAsFactors = FALSE)
tempCNN$text = gsub("http\\w+ *", "", tempCNN$text)
tempCNN$text = gsub("&amp", "", tempCNN$text)
tempCNN$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tempCNN$text)
tempCNN$text = gsub("[ \t]{2,}", "", tempCNN$text)
tempCNN$text = gsub("^\\s+|\\s+$", "", tempCNN$text)

tempUSEmbassy = data.frame(text = sapply(USEmbassycorp_strpwsp, as.character), stringsAsFactors = FALSE)
tempUSEmbassy$text = gsub("http\\w+ *", "", tempUSEmbassy$text)
tempUSEmbassy$text = gsub("&amp", "", tempUSEmbassy$text)
tempUSEmbassy$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tempUSEmbassy$text)
tempUSEmbassy$text = gsub("[ \t]{2,}", "", tempUSEmbassy$text)
tempUSEmbassy$text = gsub("^\\s+|\\s+$", "", tempUSEmbassy$text)

tempPutin = data.frame(text = sapply(Putincorp_strpwsp, as.character), stringsAsFactors = FALSE)
tempPutin$text = gsub("http\\w+ *", "", tempPutin$text)
tempPutin$text = gsub("&amp", "", tempPutin$text)
tempPutin$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tempPutin$text)
tempPutin$text = gsub("[ \t]{2,}", "", tempPutin$text)
tempPutin$text = gsub("^\\s+|\\s+$", "", tempPutin$text)

tempSyria = data.frame(text = sapply(Syriacorp_strpwsp, as.character), stringsAsFactors = FALSE)
tempSyria$text = gsub("http\\w+ *", "", tempSyria$text)
tempSyria$text = gsub("&amp", "", tempSyria$text)
tempSyria$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tempSyria$text)
tempSyria$text = gsub("[ \t]{2,}", "", tempSyria$text)
tempSyria$text = gsub("^\\s+|\\s+$", "", tempSyria$text)

tempIraq = data.frame(text = sapply(Iraqcorp_strpwsp, as.character), stringsAsFactors = FALSE)
tempIraq$text = gsub("http\\w+ *", "", tempIraq$text)
tempIraq$text = gsub("&amp", "", tempIraq$text)
tempIraq$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tempIraq$text)
tempIraq$text = gsub("[ \t]{2,}", "", tempIraq$text)
tempIraq$text = gsub("^\\s+|\\s+$", "", tempIraq$text)

tempISIS = data.frame(text = sapply(ISIScorp_strpwsp, as.character), stringsAsFactors = FALSE)
tempISIS$text = gsub("http\\w+ *", "", tempISIS$text)
tempISIS$text = gsub("&amp", "", tempISIS$text)
tempISIS$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tempISIS$text)
tempISIS$text = gsub("[ \t]{2,}", "", tempISIS$text)
tempISIS$text = gsub("^\\s+|\\s+$", "", tempISIS$text)

####### Sentiment Analysis #######

install.packages("devtools")
require(devtools)
install_url("http://www.omegahat.net/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(sentiment)
class_emo = classify_emotion(tempCNN, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(tempCNN, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=tempCNN, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
ggplot(sent_df, aes(x=emotion)) +geom_bar(aes(y=..count.., fill=emotion)) + scale_fill_brewer(palette="Dark2") +labs(x="emotion categories", y="")
ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="")

class_emo2 = classify_emotion(tempIraq, algorithm = "bayes", prior = 1.0)
emotion2 = class_emo2[,7]
emotion[is.na(emotion)] = "unknown"
class_pol2 = classify_polarity(tempIraq, algorithm="bayes")
polarity2 = class_pol2[,4]
sent_df2 = data.frame(text=tempIraq, emotion=emotion2, polarity=polarity2, stringsAsFactors=FALSE)
sent_df2 = within(sent_df2,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
ggplot(sent_df2, aes(x=emotion)) +geom_bar(aes(y=..count.., fill=emotion)) + scale_fill_brewer(palette="Dark2") +labs(x="emotion categories", y="")
ggplot(sent_df2, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="")

class_emo3 = classify_emotion(tempSyria, algorithm = "bayes", prior = 1.0)
emotion3 = class_emo3[,7]
emotion3[is.na(emotion3)] = "unknown"
class_pol3 = classify_polarity(tempSyria, algorithm="bayes")
polarity3 = class_pol3[,4]
sent_df3 = data.frame(text=tempSyria, emotion3=emotion3, polarity3=polarity3, stringsAsFactors=FALSE)
sent_df3 = within(sent_df3,emotion3 <- factor(emotion3, levels=names(sort(table(emotion3), decreasing=TRUE))))
ggplot(sent_df3, aes(x=emotion3)) +geom_bar(aes(y=..count.., fill=emotion3)) + scale_fill_brewer(palette="Dark2") +labs(x="emotion categories", y="")
ggplot(sent_df3, aes(x=polarity3)) + geom_bar(aes(y=..count.., fill=polarity3)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="")

class_emo4 = classify_emotion(tempPutin, algorithm = "bayes", prior = 1.0)
emotion4 = class_emo4[,7]
emotion4[is.na(emotion4)] = "unknown"
class_pol4 = classify_polarity(tempPutin, algorithm="bayes")
polarity4 = class_pol4[,4]
sent_df4 = data.frame(text=tempPutin, emotion4=emotion4, polarit4y=polarity4, stringsAsFactors=FALSE)
sent_df4 = within(sent_df4,emotion4 <- factor(emotion4, levels=names(sort(table(emotion4), decreasing=TRUE))))
ggplot(sent_df4, aes(x=emotion4)) +geom_bar(aes(y=..count.., fill=emotion4)) + scale_fill_brewer(palette="Dark2") +labs(x="emotion categories", y="")
ggplot(sent_df4, aes(x=polarity4)) + geom_bar(aes(y=..count.., fill=polarity4)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="")

class_emo5 = classify_emotion(tempISIS, algorithm = "bayes", prior = 1.0)
emotion5 = class_emo5[,7]
emotion5[is.na(emotion5)] = "unknown"
class_pol5 = classify_polarity(tempISIS, algorithm="bayes")
polarity5 = class_pol5[,4]
sent_df5 = data.frame(text=tempISIS, emotion5=emotion5, polarity5=polarity5, stringsAsFactors=FALSE)
sent_df5 = within(sent_df5,emotion5 <- factor(emotion5, levels=names(sort(table(emotion5), decreasing=TRUE))))
ggplot(sent_df5, aes(x=emotion5)) +geom_bar(aes(y=..count.., fill=emotion5)) + scale_fill_brewer(palette="Dark2") +labs(x="emotion categories", y="")
ggplot(sent_df5, aes(x=polarity5)) + geom_bar(aes(y=..count.., fill=polarity5)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="")

class_emo6 = classify_emotion(tempUSEmbassy, algorithm = "bayes", prior = 1.0)
emotion6 = class_emo6[,7]
emotion6[is.na(emotion6)] = "unknown"
class_pol6 = classify_polarity(tempUSEmbassy, algorithm="bayes")
polarity6 = class_pol6[,4]
sent_df6 = data.frame(text=tempUSEmbassy, emotion6=emotion6, polarity6=polarity6, stringsAsFactors=FALSE)
sent_df6 = within(sent_df6,emotion5 <- factor(emotion6, levels=names(sort(table(emotion6), decreasing=TRUE))))
ggplot(sent_df6, aes(x=emotion6)) +geom_bar(aes(y=..count.., fill=emotion6)) + scale_fill_brewer(palette="Dark2") +labs(x="emotion categories", y="")
ggplot(sent_df6, aes(x=polarity6)) + geom_bar(aes(y=..count.., fill=polarity6)) + scale_fill_brewer(palette="RdGy") + labs(x="polarity categories", y="")

####### Word Cloud Generation #######

CNNcorp2 <- Corpus(DataframeSource(tempCNN))
CNNdtm <- TermDocumentMatrix(CNNcorp2)
m <- as.matrix(CNNdtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

Iraqcorp2 <- Corpus(DataframeSource(tempIraq))
Iraqdtm <- TermDocumentMatrix(Iraqcorp2)
m <- as.matrix(Iraqdtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

Syriacorp2 <- Corpus(DataframeSource(tempSyria))
Syriadtm <- TermDocumentMatrix(Syriacorp2)
m <- as.matrix(Syriadtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

Putincorp2 <- Corpus(DataframeSource(tempPutin))
Putindtm <- TermDocumentMatrix(Putincorp2)
m <- as.matrix(Putindtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

ISIScorp2 <- Corpus(DataframeSource(tempISIS))
ISISdtm <- TermDocumentMatrix(ISIScorp2)
m <- as.matrix(ISISdtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

USEmbassycorp2 <- Corpus(DataframeSource(tempUSEmbassy))
USEmbassydtm <- TermDocumentMatrix(USEmbassycorp2)
m <- as.matrix(USEmbassydtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

####### Clustering #######

tdm <- removeSparseTerms(CNNdtm, sparse = 0.97)
m2 <- as.matrix(tdm)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 clusters 

tdm2 <- removeSparseTerms(Iraqdtm, sparse = 0.97)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6) 

tdm3 <- removeSparseTerms(Syriadtm, sparse = 0.95)
m2 <- as.matrix(tdm3)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6) 

tdm4 <- removeSparseTerms(Putindtm, sparse = 0.95)
m2 <- as.matrix(tdm4)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6)  

tdm5 <- removeSparseTerms(USEmbassydtm, sparse = 0.97)
m2 <- as.matrix(tdm5)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6) 

#######

