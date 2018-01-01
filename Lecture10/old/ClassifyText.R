##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Text Classification ----
##

require(RTextTools)
## Create a data frame of the tweets with the normalized text
tweet.frame = data.frame( 
           tweets = enc2utf8(unlist(sapply(tweet.corpus, `[`, "content"))), 
           stringsAsFactors=F)
head(tweet.frame)

## Compute a tdm
tdm.tools = create_matrix(tweet.frame$tweets, language="english", removeNumbers=FALSE,
                          stemWords=FALSE, removeSparseTerms=.998, 
                          removeStopwords = FALSE, stripWhitespace = FALSE,
                          toLower = FALSE)

## Create the a container for the tdm and label
tweet.cont = create_container(tdm.tools,tweets$sentiment, trainSize = 1:120000, virgin=TRUE)

## Compute a logistic regresson model for sentiment classification
tweet.glmnet <- train_model(tweet.cont, "GLMNET")

## Test classification
tweet.class = classify_model(tweet.cont, tweet.glmnet)
tweet.metrics = create_analytics(tweet.cont, tweet.class)

## Examine some raw metrics
tweet.metrics@label_summary
cbind(head(tweet.metrics.TfIdf@document_summary, n = 10), head(tweets$sentiment, n = 10))
cbind(head(tweet.metrics@document_summary, n = 10), head(tweets$sentiment, n = 10))


## 
create_precisionRecallSummary(tweet.cont, tweet.class)


##----------------------------------------------
## Compute TFIdf weighted tdm
## Compute a tdm
tdm.tools2 = create_matrix(tweet.frame$tweets, language = "english", removeNumbers = FALSE,
                          stemWords = FALSE, removeSparseTerms = .998, 
                          removeStopwords = FALSE, stripWhitespace = FALSE,
                          toLower = FALSE, weighting = tm::weightTfIdf)

## Create the a container for the TfIdf weighted tdm and label
tweet.cont = create_container(tdm.tools2,tweets$sentiment, trainSize = 1:120000, virgin=TRUE)


## Compute a logistic regresson model for sentiment classification
tweet.glmnet.TfIdf <- train_model(tweet.cont,"GLMNET")

## Test classification
tweet.class.TfIdf = classify_model(tweet.cont, tweet.glmnet.TfIdf)
tweet.metrics.TfIdf = create_analytics(tweet.cont, tweet.class.TfIdf)

## Examine some raw metrics
tweet.metrics.TfIdf@label_summary
results = head(tweet.metrics.TfIdf@document_summary, n = 20)
results


## Look at the confusion matrix and compare to the unweighte tdf model
create_precisionRecallSummary(tweet.cont, tweet.class.TfIdf)
create_precisionRecallSummary(tweet.cont, tweet.class)
