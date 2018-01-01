##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Term Document Matix and Latent Dirichlet Allocation ----
##
## Load the data

## ---------------------------------------------------
## ---------- Exploring Term Document Matrix
##
## Load the data set as a vector corpus of 20 documents
library(tm)
data(crude)
writeLines(as.character(crude[[1]]))

## Compute the term document matrix
crude.tdm = TermDocumentMatrix(crude, control = list(removePunctuation = TRUE,
                                                     tolower = TRUE,
                                                     removePunctuation = TRUE,
                                                     removeNumbers = TRUE,
                                                     stopwords = TRUE,
                                                     stemming = TRUE))

## Have a look at the tdm 
inspect(crude.tdm[202:205, 1:5])

## Which terms occur 10 times or more?
crudeTDMHighFreq <- findFreqTerms(crude.tdm, 10, Inf)
crudeTDMHighFreq

# Do these terms show up in the first 5 documents?
inspect(crude.tdm[crudeTDMHighFreq, 1:5]) 


## -------------------------------------------------------
## Apply a topic model to the news articles\
##
##load topic models library
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5

## Compute the LDA model
crude.dtm = DocumentTermMatrix(crude, control = list(removePunctuation = TRUE,
                                                     stopwords = TRUE))
crude.dtm  ## Check the drm
ldaOut = LDA(crude.dtm, k, method= "Gibbs", control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin=thin))

## Examine the topics
ldaOut.topics <- as.matrix(topics(ldaOut))
head(ldaOut.topics)

## And the terms
ldaOut.terms <- as.matrix(terms(ldaOut,6))
head(ldaOut.terms)

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
head(topicProbabilities)

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(crude.dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
unlist(topic1ToTopic2)

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(crude.dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
topic2ToTopic3



