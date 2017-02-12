library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(stringr)

# I will use a samlple data of some tweets
sample_data = read.csv("sample_tweet_data.csv", stringsAsFactors = F)
names(sample_data)
#[1] "text"          "favorited"     "favoriteCount" "replyToSN"     "created"      
#[6] "truncated"     "replyToSID"    "id"            "replyToUID"    "statusSource" 
#[11] "screenName"    "retweetCount"  "isRetweet"     "retweeted"     "longitude"    
#[16] "latitude"    

# Load the data as a corpus
docs <- Corpus(VectorSource(sample_data$text))

# Text Transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# used str_replace_all() from 'stringr' package
toSpace2 = content_transformer(function (x, pattern) str_replace_all(x, pattern, " "))

# remove trailing spaces
cut_trail = content_transformer(function(x, pattern) gsub(pattern, "", x))

# remove leading spaces
cut_leading = content_transformer(function(x, pattern) gsub(pattern, "", x))

docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "\n")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, "\\|")
docs = tm_map(docs, toSpace2, "[[:punct:]]")
docs = tm_map(docs, cut_trail, "[[:space:]]*$")  # remove trailing spaces
docs = tm_map(docs, cut_leading, "^[[:space:]]")  # remove leading spaces
  
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("hmm", "oh", "blabla", "rt", "https")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Document matrix is a table containing the frequency of the words
dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate the word cloud
jpeg("word_cloud.jpeg")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 30,
          max.words=200, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(12, "Paired"))

dev.off()
