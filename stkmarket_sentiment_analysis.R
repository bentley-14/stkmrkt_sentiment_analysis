stkmarket_sentiment_analysis <- function(
        CEO_Handle = "ElonMusk",
        timeline_tweet_num = 5000,
        user_tweet_num = 20000,
        market_name = "TSLA",
        appname = "bentley2210",
        consumer_key = "RfHPDshi6db5QvuLs9i3j07WP",
        consumer_secret = "WhOvXFxtxxBjCNHEctD7AVMFVeo42rPpwzd3T68B9nCKCqMIvN",
        access_token = "1294892567209762816-j13RwWCBhXspMUTHixj6652hkuE8yb",
        access_secret = "elly0RBhnCUvdGWXnJ5WTbqFmPhWyL50sfAl9spYpamUk") {

library(httpuv)
library(wordcloud)
library(httr)
library(rtweet)
library(twitteR)
library(plyr)
library(ggplot2)
library(devtools)
library(tm)
library(dplyr)
library(stringr)
library(tidytext)
library(lubridate)
library(tidyquant)
library(textdata)
library(syuzhet)
library(data.table)
library(tidyr)
library(gridExtra)
library(plotly)




#Create Twitter token allowing access into the Twitter API
twitter_token <- create_token(
        app = appname,
        consumer_key = consumer_key,
        consumer_secret = consumer_secret,
        access_token = access_token,
        access_secret = access_secret)


#setup OAuth authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Pull last ~5000 tweets from Elon Musk's timeline and extract and reformat 
#the date variable into df
ceo_tweets<-get_timeline(CEO_Handle,n = timeline_tweet_num)
uniquedates <- as.Date(format(ceo_tweets$created_at, '%Y-%m-%d'))
datecount <- as.data.frame(table(unlist(uniquedates)))
names(datecount) <- c("Date", "Count")
datecount$Date <- as.Date(datecount$Date)

ggplot(data = datecount, aes(x=Date, y = Count, group=factor(year(Date)),
                             color=factor(year(Date)))) + 
        geom_line() + 
        geom_point() + 
        labs(x="Date", colour="Year", y="Number of tweets", 
             title = "CEO Tweeting Frequency") + 
        theme_classic() +
        geom_smooth(method="auto", span = 0.3, se=TRUE, fullrange=FALSE, level=0.95)


#Histogram displaying Tweet Frequency by Month
month_lookup<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
ceo_tweets$month<-factor(month_lookup[month(ceo_tweets$created_at)],month_lookup)

ggplot(data = ceo_tweets, aes(x = `month`)) + 
        geom_bar(aes(fill = ..count..)) + xlab("Month") + 
        ylab("Number of tweets") + 
        labs(title = "Number of Tweets by Month") +
        theme_classic()


#Sentiment Analysis Preperations
primary <- ceo_tweets$text
primary <- iconv(primary, from="UTF-8", to="ASCII", sub="")
primary <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",primary)
primary <- gsub("@\\w+","",primary)
primary <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+?(/)[[:alnum:]]*", "", primary)
primary <- gsub('[[:digit:]]+', '', primary)
primary <- gsub('[[:punct:]]+', '', primary)
primary <- gsub("([[:alpha:]])\1+", "", primary)
primary <- gsub("\\d","",primary)

ew_primary<-get_nrc_sentiment(primary)
primaryscores<-data.frame(colSums(ew_primary))
names(primaryscores) <- "Score"
primaryscores <- cbind("sentiment"=rownames(primaryscores),primaryscores)
rownames(primaryscores) <- NULL


# Plot Primary Scores
ggplot(data=primaryscores,aes(x=sentiment,y=Score))+
        geom_bar(aes(fill=sentiment),stat = "identity")+
        theme(legend.position="none")+
        xlab("Sentiments")+ylab("Scores")+
        ggtitle("Total Sentiment of CEO")+
        theme_minimal()


##emotions by the month(normalized)
ceo_12<-ceo_tweets%>%filter(year(created_at)==2020)%>%group_by(month(created_at))
uniqueday <- unique(as.Date(format(ceo_tweets$created_at[1:nrow(ceo_12)], '%Y-%m-%d')))
char_uniqueday <- as.character(uniqueday)

primary2<- ceo_12$text
primary2<- iconv(primary2, from="UTF-8", to="ASCII", sub="")
primary2<- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",primary2)
primary2<- gsub("@\\w+","",primary2)
primary2<- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+?(/)[[:alnum:]]*", "", primary2)
primary2<- gsub('[[:digit:]]+', '', primary2)
primary2<- gsub('[[:punct:]]+', '', primary2)
primary2<- gsub("([[:alpha:]])\1+", "", primary2)
primary2<- gsub("\\d","",primary2)

##Month List
monthly_sentiment <- list()
for (i in 1:length(uniqueday)) {
        day_sentiment <- data.frame(colSums(get_nrc_sentiment(primary2[uniqueday==uniqueday[i]])))
        monthly_sentiment[[char_uniqueday[i]]] <- day_sentiment
}

monthly_sentiment <- as.data.frame(monthly_sentiment)

#create X test set for machine learning
X_ml <- t(monthly_sentiment)
rownames(X_ml) <- char_uniqueday; X_ml <- as.data.frame(X_ml)
X_ml$date <- as.Date(char_uniqueday)
X_ml <- arrange(X_ml, date)

colnames(monthly_sentiment)<-char_uniqueday
monthly_sentiment<-setDT(monthly_sentiment,keep.rownames=TRUE)[]
colnames(monthly_sentiment)[1]<-"sentiment"
monthly_sentiment<-gather(monthly_sentiment,"day","value",2:length(monthly_sentiment))
monthly_sentiment$day<-as.Date(monthly_sentiment$day)

total_sentiment <- aggregate(cbind(value)~day,  data=monthly_sentiment,FUN=sum)
y <- rev(total_sentiment$value)

for (i in 1:length(char_uniqueday)) {
        monthly_sentiment$newval[monthly_sentiment$day==char_uniqueday[i]] <- 
                (monthly_sentiment$value[(10*i-9):(10*i)]/y[i])
}

ggplot(data=monthly_sentiment,aes(x=day,y=value,group=sentiment))+
        geom_line(aes(color=factor(sentiment)))+
        geom_point(aes(color=factor(sentiment)))+
        labs(x="Day", colour="Sentiment",y="Value",title="Monthly Sentiment")+
        theme_minimal()

ggplot(data=monthly_sentiment,aes(x=day,y=newval,group=sentiment))+
        geom_line(aes(color=factor(sentiment)))+
        geom_point(aes(color=factor(sentiment)))+
        labs(x="Day", colour="Sentiment",y="Value",
             title="Monthly Sentiment Normalized")+
        theme_minimal()

pos_neg <- subset(monthly_sentiment, 
                  subset = (`sentiment` = positive) | (`sentiment` = negative))

ggplot(data=pos_neg, aes(x=day,y=value,group=sentiment))+
        geom_line(aes(color=factor(sentiment)))+
        geom_point(aes(color=factor(sentiment)))+
        labs(x="Day", colour="Sentiment",y="Value",
             title="Positive vs. Negative Normalized")+
        theme_minimal()


#User Sentiment Analysis
user_raw <-search_tweets(market_name,lang="en", n = user_tweet_num, include_rts = FALSE, 
                         retryonratelimit = TRUE)
user.df <- user_raw[rev(order(user_raw$created_at)),]
duplicate <- duplicated(user.df$text)
duplicate <- !duplicate
user.uniqe <- user.df[duplicate,]
userstext <- user.uniqe$text

#clean userstext up
userstext<- iconv(userstext, from="UTF-8", to="ASCII", sub="")
userstext<- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",userstext)
userstext<- gsub("@\\w+","",userstext)
userstext<- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+?(/)[[:alnum:]]*", "", userstext)
userstext<- gsub('[[:digit:]]+', '', userstext)
userstext<- gsub('[[:punct:]]+', '', userstext)
userstext<- gsub("([[:alpha:]])\1+", "", userstext)
userstext<- gsub("RT", "", userstext)
userstext<- gsub("\\d","",userstext)

#create userfinal
ss_userstext <-get_nrc_sentiment(userstext)
usersfinal<-data.frame(colSums(ss_userstext))
names(usersfinal) <- "Score"
usersfinal <- cbind("sentiment"=rownames(usersfinal),usersfinal)
rownames(usersfinal) <- NULL

#Plot User Sentiment
ggplot(data=usersfinal,aes(x=sentiment,y=Score)) +
        geom_bar(aes(fill=sentiment),stat = "identity") +
        theme(legend.position="none") +
        xlab("Sentiments")+ylab("Scores") +
        ggtitle("Total sentiment of users") + 
        theme_minimal()

#Tesla Financial Date
company <- tq_get(market_name, get = "stock.prices", from = " 1990-01-01")

#subsetting rows withing tweeting timeframe
df <- rev(uniquedates)
company_f <- subset(company, company$date >= first(df) & company$date <= last(df))
company_f$difference<- company_f$close - company_f$open
company_f$differencenorm <- (company_f$close - company_f$open)/company_f$open
company_f$categorical <- as.factor(as.numeric(company_f$differencenorm > 0))

#obtaining X and y training set for machine learning
company_ml <- company_f[year(company_f$date) == 2020, ]
X_ml <- subset(X_ml, date %in% (company_ml$date-1))
company_ml <- subset(company_ml, date %in% (X_ml$date+1))

write.csv(X_ml, "training_X.csv")
write.csv(company_ml, "training_y.csv", row.names = F)

#plot financial difference (normalized)
ggplot(data = company_f, aes(x=date, y = difference, color=factor(year(date)))) + 
        geom_line() + 
        labs(x="Date", colour="Year", y="Difference", title = "Company Difference") + 
        theme_classic() +
        geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
        geom_smooth(method="auto", span = 0.3, se=TRUE, fullrange=FALSE, level=0.95)


#plot financial difference (normalized with trendline)
ggplot(data = company_f, aes(x=date, y = differencenorm, color=factor(year(date)))) + 
        geom_line() + 
        labs(x="Date", colour="Year", y="Difference Normalized", title = "Company Difference Normalized") + 
        theme_classic() +
        geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
        geom_smooth(method="auto", span = 0.3, se=TRUE, fullrange=FALSE, level=0.95)


#plot financial difference (normalized & interactive)
plot_ly(company_f, x = ~company_f$date, y = company_f$differencenorm, type = 'scatter', mode = 'lines') %>% 
        layout(title = "Company Difference Normalized", 
               xaxis = list(title = "Date"), 
               yaxis = list (title = "Difference Normalized"))

}
        