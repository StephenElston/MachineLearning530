##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Test normaization ----
##

## Read the tweet data set
tweets = read.csv('Binary Classification_ Twitter sentiment analysis.csv', 
                   header = TRUE, stringsAsFactors = FALSE)
colnames(tweets) <- c("sentiment", "tweets") # Set the column names
tweets[, 'sentiment'] = ifelse(tweets$sentiment == 4, 1, 0)  # set sentiment to {0,1}
head(tweets) # Have a look at the data frame

## Create a tm text corpus from the tweets
library(tm)  ## tm package for text mining
tweet.corpus <- Corpus(VectorSource(tweets['tweets']))
class(tweet.corpus) # What is the class of the corpus

## Normalize tweets text
tweet.corpus <- tm_map(tweet.corpus, content_transformer(removeNumbers))
tweet.corpus <- tm_map(tweet.corpus, content_transformer(removePunctuation))
tweet.corpus <- tm_map(tweet.corpus, content_transformer(stripWhitespace))
tweet.corpus <- tm_map(tweet.corpus, content_transformer(tolower))

## -----------------------------------------------
## ----- Convert the corpus to a term document matrix
to.tdm = function(corpus, sparse = 0.998){
  require(tm)
  ## Compute a term-document matrix and then 
  require(slam) # Sparse matrix package
  tdm <- TermDocumentMatrix(corpus, control = list(stopwords = FALSE))
  tdm <- removeSparseTerms(tdm, sparse)
  tdm
}
tdm = to.tdm(tweet.corpus) # Create a term document matrix
str(tdm) # Look at sparse tdm
findFreqTerms(tdm, 2000) # Words that occur at least 2000 times


## Compute the word fequency from the tdm
to.wf = function(tdm){
  ## compute the word frequencies.
  require(slam)
  freq <- row_sums(tdm, na.rm = T)   
  ## Sort the word frequency and build a dataframe
  ## including the cumulative frequecy of the words.
  freq <- sort(freq, decreasing = TRUE)
  word.freq <- data.frame(word = factor(names(freq), levels = names(freq)), 
                          frequency = freq)
  word.freq['Cumulative'] <- cumsum(word.freq['frequency'])/sum(word.freq$frequency)
  word.freq
}
wf = to.wf(tdm)
head(wf, n = 10)


## Make a bar chart of the word frequency
word.bar = function(wf, num = 50){
  require(ggplot2)
  ggplot(wf[1:num,], aes(word, frequency)) +
    geom_bar(stat = 'identity') +
    ggtitle('Frequency of common words') +
    ylab('Frequency') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
word.bar(wf)


## Make cumulative distribution plots of the most frequent words
word.cdf = function(wf, num = 50){
  require(ggplot2)
  ggplot(wf[1:num,], aes(word, Cumulative)) +
    geom_bar(stat = 'identity') +
    ggtitle('Cumulative fraction of common words') +
    ylab('Cumulative frequency') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
word.cdf(wf)


## ----------------------------------------------------------
## -----------Stop words -------------------------------------
##
## Load stop words from a file and ensure they are 
stopWords = read.csv('stopwords.csv', header = TRUE, stringsAsFactors = FALSE)
stopWords = unique(stopWords) # Ensure the list is unique
stopWords[1:100,] # Look at the first 100 stop words

## Remove the stop words from the corpus
tweet.corpus <- tm_map(tweet.corpus, removeWords, stopWords[, 'words'])

## View the results
tdm = to.tdm(tweet.corpus) # Create a term document matrix
findFreqTerms(tdm, 2000) # Words that occur at least 2000 times
wf = to.wf(tdm)  # Compute word fequency
head(wf, n = 10)  # Look at the most common words
word.bar(wf) # Plot word frequency
word.cdf(wf) # Plot cdf


## --------------------------------------------------
## ------------ Stem the words ----------------------
##
## Use the porter stemmer in Snowball package
##
require(SnowballC) ## For Porter stemming words
tweet.corpus <- tm_map(tweet.corpus, stemDocument)

## View the results
tdm = to.tdm(tweet.corpus, sparse = 0.99) # Create a term document matrix
findFreqTerms(tdm, 2000) # Words that occur at least 2000 times
wf = to.wf(tdm)  # Compute word fequency
head(wf, n = 10)  # Look at the most common words
word.bar(wf) # Plot word frequency
word.cdf(wf) # Plot cdf






