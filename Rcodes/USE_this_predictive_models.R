# Eye2data _PRediction model
## OBjective 2: Consolidate the datasets for modeling 

library(car)
library(tm)
library(DataCombine)
library(reshape)
library(xts)
library(lubridate)
library(forecast)
library(tseries)
library(astsa)
library(igraph)
library(gtools)
library(xts)
library(zoo)
library(hydroTSM)
library(tseries)
library(Hmisc)

library(caret)
library(klaR)
library(tm)
library(topicmodels)
library(XML)

setwd("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Progress Report - 4\\predictive_model")

central_data <- read.csv('For_centrality.csv')

## Clean the data

c_data <- central_data[!central_data$WeekId=="",]

weekly_data <- read.csv('Consolidated_dataset.csv')

tw_year <- read.csv('Data Science.csv')
#tw_year <- read.csv('tweets_DS_2015.csv')
#tw_year$Time <- as.POSIXct(as.numeric(as.character(tw_year$timestamp/1000)), origin='1970-01-01', tz='GMT')
tw_year$Timestamp <- as.POSIXct(tw_year$Time,format=c("%m/%d/%Y %H:%M"))
tw_year$Date <- as.Date(tw_year$Timestamp)
tw_year$week <- paste(week(tw_year$Date),"_",year(tw_year$Date),sep="")
tw_year$week <- as.factor(tw_year$week)
length(levels(tw_year$week))
## Create DTM for twitter:

corpus = Corpus(VectorSource(tw_year$text))

# Preprocess the tweets - convert to lower, remove punctuation, remove stop words, stemming
#When applying the Script to a NounsMerged file, skip these preprocess steps, and continue with Document Term Matrix
processedCorpus = corpus
processedCorpus = tm_map(processedCorpus, tolower)
processedCorpus = tm_map(processedCorpus, PlainTextDocument)
#Remove stop words and add terms to the predetermined english stepwords
#my_stopwords = c(stopwords("english"), 'be',  'can',	'could',	'dare',	'do',	'have',	'may',	'might',	'must',	'need',	'ought',	'shall',	'should',	'will',	'would' )
my_stopwords = c(stopwords("english"), 'new','job','via','amp'
,'using','now','2015','great','via','free','webinar','year','one','get','talk','first','boston',
'list', 'insights','learning','google','resources','tomorrow','team','looking','nice','building',
'summit','going','come','interview','course','expert','software','see','jobs','top','got','better',
'people','2014','twitter','tweet','tech','things','slides','read','getting','world','everyone','part',
'going' ,'be',  'can',  'could',	'dare',	'do',	'have',	'may',	'might',	'must',	'need',	'ought',	'shall',	'should',	'will',	'would' )

processedCorpus = tm_map(processedCorpus, removeWords, my_stopwords)
processedCorpus = tm_map(processedCorpus, removePunctuation)
#processedCorpus = tm_map(processedCorpus, stemDocument)

# Document Term Matrix from the Original Corpus
dtmOriginal = DocumentTermMatrix(corpus)
ncol(dtmOriginal)

# Document Term Matrix from the Processed Corpus
dtmProcessed = DocumentTermMatrix(processedCorpus)
ncol(dtmProcessed)

# Remove terms that do not occur frequently.
# 0.995 is the Sparcity Threshold. This indicates terms that occur less that occur in less than (1 - 0.999)* 100 = 0.1% of the tweets will be removed.
dtmSparseRemoved = removeSparseTerms(dtmProcessed, 0.999)
dim(dtmSparseRemoved)

tw_all_rows <- cbind(tw_year,as.data.frame(as.matrix(dtmSparseRemoved)))

rowTotals = apply(dtmSparseRemoved , 1, sum)
dtmSparseRemoved.new = dtmSparseRemoved[rowTotals> 0, ] 

k = 5

# CTM_out = CTM(dtmSparseRemoved.new, k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3)))
LDA_out = LDA(dtmSparseRemoved.new, k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3)))

Terms <- terms(LDA_out, 100)
Terms[,1:5]

top_terms <- as.vector(Terms[1:100,1:5])  

pr_temp <- as.data.frame(as.matrix(dtmSparseRemoved.new))

pr1 <- tw_all_rows[,colnames(tw_all_rows) %in% colnames(tw_year)]  ## Separate other variables 

pr2 <- tw_all_rows[,colnames(tw_all_rows) %in% top_terms]## 

pr <- cbind(pr1,pr2)
pr[,'r'] <-pr$rstats
pr[,'Python'] <- pr$python
tweet_f <- pr[,!names(pr)%in% c('rstats','python')]
## Closeness and Eigen vector centrality for all the hashtags, one by one: SO and Twitter

# d_data <- merge(c_data,tw_year, by.x=c("WeekId"),by.y=c("week"))

per_week_s <- split(c_data,c_data$WeekId)
per_week_t <- split(tweet_f,tweet_f$week)

## Trial

datasub <- per_week_t[[1]]

hashm <- as.data.frame(datasub[,17:ncol(datasub)]) 
thash <- t(hashm) 
tot <- as.matrix(thash) %*% as.matrix(hashm)  
g  <- graph.adjacency(tot,weighted=TRUE)

 
transitivity(g, type=c("local"),vids=c('r','Python'))
#d <- power_centrality(g, loops = FALSE, exponent = 1, rescale = FALSE, tol = 1e-07, sparse = TRUE)[218]
#power_centrality(g, loops = FALSE, exponent = 1, rescale = FALSE, tol = 1e-07, sparse = TRUE)[219]

subgraph.centrality (g, diag=FALSE)[218]

#get.shortest.paths(graph, from, to=V(graph), mode = c("all"), weights = NULL, output=c("vpath"))

## 36 column onwards are keywords

centrality_func_SO <- function(datasub)
{
  hashm <- as.data.frame(datasub[,36:ncol(datasub)]) 
  thash <- t(hashm) 
  tot <- as.matrix(thash) %*% as.matrix(hashm)  
  g  <- graph.adjacency(tot,weighted=TRUE)
  gr <- simplify(g,remove.loops=TRUE)
  #df <- get.data.frame(g)
  close <- closeness(gr, vids=c('r','python','java'), mode = c("all"),normalized = TRUE)
  eigen <- c(centr_eigen(gr, directed = FALSE, scale = TRUE,  options = arpack_defaults, normalized = TRUE)$vector[314],
             centr_eigen(gr, directed = FALSE, scale = TRUE,  options = arpack_defaults, normalized = TRUE)$vector[315],
             centr_eigen(gr, directed = FALSE, scale = TRUE,  options = arpack_defaults, normalized = TRUE)$vector[316])
  clus_coeff <- transitivity(gr, type=c("local"),vids=c('r','python','java'))
  subgraph_cen <- c(subgraph.centrality (g, diag=FALSE)[314],
                    subgraph.centrality (g, diag=FALSE)[315],
                    subgraph.centrality (g, diag=FALSE)[316])
  
    l <- list(gr,close, eigen,clus_coeff,subgraph_cen)
  return(l)
}

#ld <- centrality_func(per_week[[4]])

store_graph_s <- list() ## Store all the graphs in a list to visualize it later!
data_holder_s <- data.frame(R_close= numeric(0),Python_close = numeric(0),Java_close = numeric(0),
                          R_eigen= numeric(0),Python_eigen = numeric(0),Java_eigen = numeric(0),
                          R_clus = numeric(0),Python_clus = numeric(0),Java_clus = numeric(0),
                          R_subgraph= numeric(0),Python_subgraph = numeric(0),Java_subgraph = numeric(0))

for(l in 1:length(per_week_s))
{
  temp_list_holder <- centrality_func_SO(per_week_s[[l]])  
  store_graph_s[[l]] <- temp_list_holder[[1]]
  closeness <- temp_list_holder[[2]]
  eigen <- temp_list_holder[[3]]
  clus <- temp_list_holder[[4]]
  subgraph <- temp_list_holder[[5]]
  temp_vect <- c(closeness,eigen,clus,subgraph)
  data_holder_s[l,] <- temp_vect
}

data_holder_s$WeekID <- levels(c_data$WeekId)

## Merge with other data and convert to time series
all_in_s <- merge(data_holder_s,weekly_data, by.x=c("WeekID"),by.y=c("WeekId"))

## Now for the twitter part ##
#####################################
####################################

centrality_func_t <- function(datasub)
{
  hashm <- as.data.frame(datasub[,17:ncol(datasub)]) 
  thash <- t(hashm) 
  tot <- as.matrix(thash) %*% as.matrix(hashm)  
  g  <- graph.adjacency(tot,weighted=TRUE)
  gr <- simplify(g,remove.loops=TRUE)
  close <- closeness(gr, vids=c('r','Python'), mode = c("all"),normalized = TRUE)
  eigen <- c(centr_eigen(gr, directed = FALSE, scale = TRUE,  options = arpack_defaults, normalized = TRUE)$vector[218],
             centr_eigen(gr, directed = FALSE, scale = TRUE,  options = arpack_defaults, normalized = TRUE)$vector[219])
  clus_coeff <- transitivity(gr, type=c("local"),vids=c('r','Python'))
  subgraph_cen <- c(subgraph.centrality (gr, diag=FALSE)[218],
                    subgraph.centrality (gr, diag=FALSE)[219])
  
  l <- list(gr,close, eigen,clus_coeff,subgraph_cen)
  return(l)
}

#ld <- centrality_func(per_week[[4]])

store_graph_t <- list() ## Store all the graphs in a list to visualize it later!
data_h <- data.frame(R_close= numeric(0),Python_close = numeric(0),
                          R_eigen= numeric(0),Python_eigen = numeric(0),
                     R_clus= numeric(0),Python_clus = numeric(0),
                     R_subgraph= numeric(0),Python_subgraph = numeric(0))

for(l in 1:length(per_week_t))
{
  temp_list_holder <- centrality_func_t(per_week_t[[l]])  
  store_graph_t[[l]] <- temp_list_holder[[1]]
  closeness <- temp_list_holder[[2]]
  eigen <- temp_list_holder[[3]]
  clus <- temp_list_holder[[4]]
  subgraph <- temp_list_holder[[5]]
  temp_vect <- c(closeness,eigen,clus,subgraph)
  data_h[l,] <- temp_vect
}

data_h$WeekId <- levels(tweet_f$week)

## Merge with other data and convert to time series
all_in <- merge(data_h,all_in_s, by.x=c("WeekId"),by.y=c("WeekID"))

write.csv(all_in,"Finally_final_dataset.csv")

########################################
#######################################
#### PREDICTION ##########################
#########################################

# Eye2data _PRediction model
## OBjective 2: Consolidate the datasets for modeling 

library(car)
library(frbs)
library(caret)
library(klaR)
library(party)

## Add variables to X in three steps:
# 1)Non-network metric
# 2) Network metric (One by one)
# 3) Twitter data (ALL)

data_r <- read.csv("Finally_r.csv")
data_python <- read.csv("Finally_python.csv")
data_java <- read.csv("Finally_java.csv")


train_control <- trainControl(method="cv", number=10)  # 10 fold CV

model_pred <- function(datain,method)
{
  model <- train(Y~., data=datain, trControl=train_control, method=as.character(substitute(method)))
  return (model)
}

print_RMSE <- function(method)
{
  predictions <- predict(method, X)
  print ((mean((predictions - Y)^2))^0.5 )
}

print_MAPE <- function(method)
{
  predictions <- predict(method, X)
  print (mean(abs(predictions - Y)/Y*100)) 
}


# Part 1: 

#data_in <- data_r
#data_in <- data_python
data_in <- data_java
data_in[,2:ncol(data_in)] <- as.data.frame(scale(as.matrix(data_in[,2:ncol(data_in)]),scale=T,center=T))

X <- data_in[,2:9]
#Y <- data_in$r
#Y <- data_in$python
Y <- data_in$java

# Part 2:
X <- data_in[,c(2:9,14:15)]
# X <- data_in[,c(2:11)]  ## For Java

# Part 3:
X <- data_in[,c(2:9,16:17)]
# X <- data_in[,c(2:9,11:13)]  ## For Java

# Part 3.5:
X <- data_in[,c(2:9,14:17)]
# X <- data_in[,c(2:13)]  ## For Java

# Part 4:
X <- data_in[,c(2:17)]
## No metrics for Java 

datam <- data.frame(X,Y)

set.seed(23422)

## lasso

system.time(
  model.lasso <- model_pred(datam,lasso)
)

## Conditional inference random forest

system.time(
  model.cforest <- model_pred(datam,cforest)
)

# Stochastic Gradient Boosting

system.time(
  model.gbm <- model_pred(datam,gbm)
)

# Multivariate Adaptive Regression Splines

system.time(
  model.gcvEarth <- model_pred(datam,gcvEarth)
)

## Conditional inference regression tree ##

system.time(
  model.ctree <- model_pred(datam,ctree)
)

# Random forest

system.time(
  model.rf <- model_pred(datam,rf)
)

c(
  print_RMSE(model.lasso),
  print_RMSE(model.cforest),
  print_RMSE(model.gbm),
  print_RMSE(model.gcvEarth),
  print_RMSE(model.ctree),
  print_RMSE(model.rf)
)

c(
  print_MAPE(model.lasso),
  print_MAPE(model.cforest),
  print_MAPE(model.gbm),
  print_MAPE(model.gcvEarth),
  print_MAPE(model.ctree),
  print_MAPE(model.rf)
)

rf <- model.rf$finalModel
importance(rf)



