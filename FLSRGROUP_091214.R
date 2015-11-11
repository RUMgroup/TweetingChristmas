#Script for R Christmas Special

#Aim: to demonstrate the use of the twitter API and text mining of the resulting data

#install.packages(c("devtools", "rjson", "bit64", "httr"))

########  RESTART R !!!!!!!!  ##############


# library(devtools)
# install_github("twitteR", username="geoffjentry")

library("twitteR")
library("ROAuth")
library("plyr")


#need a dev twitter account - can make your own easily
API_Key <- 
API_Secret <- 
Access_Token <- 
Access_Secret <-  


setup_twitter_oauth(API_Key, API_Secret,Access_Token,Access_Secret)



#Let's search past tweets
#has a tendency to be rate limited - wait 15 mins between each big search - takes a couple of mins to complete
test=searchTwitter(searchString="#Christmas",n=10,lang="en")




#Let's listen instead - need to authenticate using ROAUTH
library(streamR)


requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

my_oauth <- OAuthFactory$new(consumerKey=API_Key,
                             consumerSecret=API_Secret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

#should open up browser and give you a pin to type into R
my_oauth$handshake()

#Listen to all english language christmas tweets and store as json
filterStream(file.name="tweets_keyword.json_christmas", track=c("Christmas","Xmas"),tweets=500000,oauth=my_oauth,language="en")

# parsing tweets into dataframe
tweets.Christmas.500000.df <- parseTweets("tweets_keyword.json", verbose = TRUE)

#remove duplicated text
tweets.Christmas.filtered <- tweets.Chrimstas.500000.df [!duplicated(tweets.Chrimstas.500000.df$text),]

#randomly sample to reduce RAM usage
tweets.Christmas.sampled <- tweets.Chrimstas.filtered[ sample(nrow(tweets.Chrimstas.filtered),50000),]




#Here's one I made earlier
load("tweets.Christmas.sampled.RData")

#get rid of non utf-8 characters as it breaks tm
library(stringr)
usableText=str_replace_all(tweets.Chrimstas.sampled$text,"[^[:graph:]]", " ") 


#do some text mining
require("tm")
myCorpus <- Corpus(VectorSource(usableText))

#get rid of the white space
myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove punctuation
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))

# remove numbers
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))

#stem words
myCorpus <- tm_map(myCorpus, content_transformer(stemDocument))

#  remove stopwords
some_stopwords <- c(stopwords('english'),"rt","http","follow","via")
myCorpus <- tm_map(myCorpus, content_transformer(removeWords),some_stopwords)
data.corpus <- TermDocumentMatrix(myCorpus)

#get rid of infrequent terms to avoid integer overflow
data.corpus <- removeSparseTerms(data.corpus,sparse=0.999)



#look at the resulting corpus
findFreqTerms(data.corpus, lowfreq=1000)

#look for associations between words - slow!
findAssocs(data.corpus, 'gift', 0.30)

#make a word cloud

library(wordcloud)
m <- as.matrix(data.corpus)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=300)


#Sentiment Analysis

#Could use machine learning if you had a training dataset. Instead we'll use a list of postive and negative words to score tweets.

#score on the basis of pos or neg words

neg.words = readLines("negative_words.txt")
pos.words = readLines("positive_words.txt")


sentences=lapply(myCorpus,"[[",1)
scores = laply(sentences,
               function(sentence, pos.words, neg.words)
               {
                 # split sentence into words with str_split (stringr package)
                 word.list = str_split(sentence, "\\s+")
                 words = unlist(word.list)
                 
                 # compare words to the dictionaries of positive & negative terms
                 pos.matches = match(words, pos.words)
                 neg.matches = match(words, neg.words)
                 
                 # get the position of the matched term or NA
                 # we just want a TRUE/FALSE
                 pos.matches = !is.na(pos.matches)
                 neg.matches = !is.na(neg.matches)
                 
                 # final score
                 score = sum(pos.matches) - sum(neg.matches)
                 return(score)
               }, pos.words, neg.words)


#Add back to the main data.frame
tweets.Chrimstas.sampled$Sentiment=scores


#unhappy tweets
tweets.Chrimstas.sampled [ tweets.Chrimstas.sampled$Sentiment==-5,"text"]



#get longitude and lattidue for tweets from the location data
library(maps)
data(world.cities)

#modified function from - http://biostat.jhsph.edu/~jleek/code/twitterMap.R
findLatLon <- function(loc){
  latlon = NA
  cont = NA
  
  # Asia = 1, Africa = 2, North America = 3, South America = 4, Australia/New Zealand = 5, Europe = 6
  continents = matrix(NA,nrow=length(unique(world.cities[,2])),ncol=2)
  continents[,1] = unique(world.cities[,2])
  continents[1:10,2] = c(1,1,1,2,1,1,1,1,1,1)
  continents[11:20,2]= c(1,1,2,1,1,2,1,2,2,2)
  continents[21:30,2] = c(2,1,6,6,6,6,6,6,6,6)
  continents[31:40,2] = c(6,6,6,6,2,4,4,1,2,1)
  continents[41:50,2] = c(4,6,1,4,6,1,3,1,6,6)
  continents[51:60,2] = c(3,2,4,2,6,1,6,1,3,2)
  continents[61:70,2] = c(1,2,2,2,3,6,3,3,6,6)
  continents[71:80,2] = c(1,1,2,6,3,4,3,4,6,1)
  continents[81:90,2] = c(3,3,3,2,2,6,6,6,6,4)
  continents[91:100,2] = c(2,5,2,2,3,1,1,1,1,1)
  continents[101:110,2] = c(1,2,1,1,1,3,2,5,1,6)
  continents[111:120,2] = c(1,6,1,1,2,6,1,1,6,2)
  continents[121:130,2] = c(6,6,6,1,1,3,4,3,4,2)
  continents[131:140,2] = c(6,6,2,2,1,1,1,4,1,1)
  continents[141:150,2] = c(1,2,2,1,1,1,4,6,6,2)
  continents[151:160,2] = c(4,1,1,1,1,2,4,6,2,2)
  continents[161:170,2] = c(1,2,2,1,6,2,1,1,6,1)
  continents[171:180,2] = c(1,1,1,2,6,2,2,6,1,1)
  continents[181:190,2] = c(2,6,2,1,6,6,3,3,3,3)
  continents[191:200,2] = c(2,2,2,2,3,2,3,2,3,1)
  continents[201:210,2] = c(3,2,2,2,2,2,2,1,6,2)
  continents[211:220,2] = c(1,3,1,6,2,4,3,6,3,4)
  continents[221:230,2] = c(1,1,1,3,2,3,3,6,1,6)
  continents[231:232,2] = c(2,1)
  
  
  # Get the first element of the location
  # firstElement = strsplit(loc,"[^[:alnum:]]")[[1]][1]
  firstElement = strsplit(loc,",")[[1]][1]
  if(is.na(firstElement)){firstElement="zzzzzzzzz"}
  
  # See if it is a city
  tmp = grep(firstElement,world.cities[,1],fixed=TRUE)
  tmp2 = grep(firstElement,state.name,fixed=TRUE)
  tmp3 = grep(firstElement,world.cities[,2],fixed=TRUE)
  
  if(length(tmp) == 1){
    latlon = world.cities[tmp,c(5,4)]
    cont = continents[which(world.cities[tmp,2]==continents[,1]),2]
  }else if(length(tmp) > 1){
    tmpCities = world.cities[tmp,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }else if(length(tmp2) == 1){
    latlon = c(state.center$x[tmp2],state.center$y[tmp2])
    cont = 3
  }else if(length(tmp3) > 0){
    tmpCities = world.cities[tmp3,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }
  
  #return(list(latlon=latlon,cont=as.numeric(cont)))
  return(latlon)
}


locs<-as.data.frame(tweets.Chrimstas.sampled$location)
locs_lat=apply(locs,1,findLatLon)


load("locs_lat.RData")

longitude=unlist(lapply(locs_lat,"[",1))
lattidue=unlist(lapply(locs_lat,"[",2))


#let's plot on the world map
library("ggmap")
library("maptools")
library("maps")

mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() + mapWorld 
mp <- mp+ geom_point(aes(x=longitude, y=lattidue) ,color="blue", size=3,alpha=0.1)
mp
