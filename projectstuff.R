library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(ggmap)
library(maps)

consumer_key <- "rXe2OXtCADNyIFzaNGxGhexnM"
consumer_secret <- "zl2R9x42I2QVcS7OJ67zqcDKqyCoBHpjybaJjuLVDC2R3TYspo"
access_token <- "2985033126-vj0tyMKeZWS4zfMlj7ClwJEjvlcBDjhXBD4xqry"
access_secret <- "lO6GshHPc5uIFe1FJjnJtiWhsphBPRySAV8fOCCCZw5EZ"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

correct <- c("their", "a lot", "received", "separate")
misspelled <- c("thier", "alot", "recieved", "seperate")

cities <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(cities) <- c("name", "geocode")
cities <- rbind(cities, data.frame(name="Boston", geocode="42.3601,-71.0589,30mi"))
cities <- rbind(cities, data.frame(name="Los Angeles", geocode="34.0522,-118.2437,30mi"))
cities <- rbind(cities, data.frame(name="Chicago", geocode="41.8781,-87.6298,30mi"))
cities <- rbind(cities, data.frame(name="Houston", geocode="29.7604,-95.3698,30mi"))
cities <- rbind(cities, data.frame(name="Atlanta", geocode="33.7490,-84.3880,30mi"))

count <- 100
data_txt <- list()
results <- list()

apply(cities, 1, function(city) {
  for(wordIndex in 1:length(correct)) {
    searchTerm <- paste(paste(correct[wordIndex], "OR", misspelled[wordIndex], sep=' '), '-filter:retweets', sep=' ')
    tweets <- searchTwitter(searchTerm, n=count, geocode=as.character(city['geocode']), resultType="recent")
    sapply(tweets, function(x) {
      index <- length(data_txt) + 1
      data_txt[[index]] <<- x$text
    })
    incorrectPercentage <- 100 - length(Filter(function(x) grepl(correct[wordIndex], tolower(iconv(x$text, to="utf-8-mac")), fixed=TRUE), tweets))/count*100
    results[[length(results) + 1]] <<- c(city = city['name'], percent = incorrectPercentage)
    print(paste(city['name'], "misspells", correct[wordIndex], "with", misspelled[wordIndex], incorrectPercentage, "percent of the time"))
  }
})
data_txt <- unlist(data_txt)


##Wordcloud 
#wordcloud style chosen to emulate a feeling of emelentarty spelling in a colorful readable colorscheme 
data_txt <- iconv(data_txt,to="utf-8-mac")
data_corpus <- Corpus(VectorSource(data_txt))
data_clean <- tm_map(data_corpus, removePunctuation, lazy=TRUE)
data_clean <- tm_map(data_clean, content_transformer(tolower), lazy=TRUE)
data_clean <- tm_map(data_clean, removeWords, stopwords("english"), lazy=TRUE)
data_clean <- tm_map(data_clean, removeNumbers, lazy=TRUE)
data_clean <- tm_map(data_clean, stripWhitespace, lazy=TRUE)
wordcloud(data_clean, scale = c(4,1), max.words = 200, random.order = F, colors=brewer.pal(10,"Paired"), vfont=c("script", "plain"))




#Here is a map of the cities I looked at
#larger dots represent larger percent of the twitter population misspells common words
boston <- 0
houston <- 0
los_angeles <- 0
chicago <- 0
atlanta <- 0

sapply(results, function(element) {
  if(as.character(element['city.name']) == "Boston") {
    boston <<- boston + as.numeric(element['percent'])
  }
  if(as.character(element['city.name']) == "Houston") {
    houston <<- houston + as.numeric(element['percent'])
  }
  if(as.character(element['city.name']) == "Los Angeles") {
    los_angeles <<- los_angeles + as.numeric(element['percent'])
  }
  if(as.character(element['city.name']) == "Chicago") {
    chicago <<- chicago + as.numeric(element['percent'])
  }
  if(as.character(element['city.name']) == "Atlanta") {
    atlanta <<- atlanta + as.numeric(element['percent'])
  }
})
results
percents <- c(boston, los_angeles, chicago, houston, atlanta)
max_percents <- max(percents)
percents <- sapply(percents, function(x) x/max_percents*6)

map <- cbind(geocode(as.character(cities$name)), cities)
ggmap(get_map(location = 'usa', zoom = 3)) + geom_point(data=map, aes(x=lon, y=lat, size=percents),show.legend=F, color="orange")

