# Eye2data _PRediction model
## OBjective 1: Create dataset weekly R, Java and Python

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

setwd("C:/Users/karthik/Google Drive/Course_projects/Sem_3/MIS586 Big Data/Progress Report - 4/predictive_model")

## Datasets

answers <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\StackExchange_Answers_Data.csv")
users <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\StackExchange_Users_Data.csv")
userslat <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Progress Report - 2\\Stack Exchange\\SO_Posts__Users_lat.csv")
posts2 <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\StackExchange_Posts_Data_Subset.csv")
posts <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Progress Report - 2\\Stack Exchange\\SO_Posts_with_brackets.csv")
ans_users <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\Answer_Users.csv")
keywords <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Twitter\\Keywords.csv",header=F)

## Pre-processing
#users$Id <- as.factor(users$Id)
#ans_users$Id <- as.factor(ans_users$Id)
#answers$OwnerUserId <- as.factor(answers$OwnerUserId)  
#posts$OwnerUserId <- as.factor(posts$OwnerUserId)

post <- merge(posts,posts2,by.x='PostId',by.y='Id')
post$PostDate <- as.POSIXct(post$CreationDate,format="%m-%d-%Y %H:%M")

answers$AnswersDate <- as.POSIXct(answers$CreationDate,format="%Y-%m-%d %H:%M:%S")
ques_ans <- merge(post_mat,answers,by.x=c("PostId"),by.y=c("ParentId"),all.x=TRUE) 


# Create corpus of tweets
processedCorpus = Corpus(VectorSource(post$Tags))
processedCorpus = tm_map(processedCorpus, tolower)
processedCorpus = tm_map(processedCorpus, PlainTextDocument)

# Document Term Matrix from the Processed Corpus
dtmProcessed = DocumentTermMatrix(processedCorpus)

dtmSparseRemoved = removeSparseTerms(dtmProcessed, 0.999)

prog <- as.data.frame(as.matrix(dtmProcessed))
prog_concise <- as.data.frame(as.matrix(dtmSparseRemoved))

## Combine keywords related to R, Java, Python into three categories 
post_mat <- cbind(posts,prog_concise)
post_mat$OwnerUserId <- as.factor(post_mat$OwnerUserId)
post_mat$python <- bitwOr(post_mat[,'<python-2.7>'],post_mat[,'<python-3.x>'])
post_mat$python <- bitwOr(post_mat$python,post_mat[,'<python>'])
post_mat$r <- bitwOr(post_mat[,'<r-caret>'],post_mat[,'<r>'])
post_mat[,'java'] <- post_mat[,'<java>']

post_mat$dates <- as.POSIXct(posts2$CreationDate, format="%m-%d-%Y %H:%M")


## Create a single table (question_user-posts-answer-answer_user) ## Single flatfile
## Left outer join to get flatfile 
#ques_users <- merge(ques_ans, users,by.x=c("OwnerUserId.x"),by.y=c("Id"),all.x=TRUE) 
#ques_ans_users <- merge(ques_users, ans_users,by.x=c("OwnerUserId.y"),by.y=c("Id"),all.x=TRUE) 

# write.csv(ques_ans_users,"Flatfile.csv")
dat <- ques_ans

write.csv(dat,"Flatfile_8Nov2015.csv")

## Check first day of the data:
dat$date <- as.Date(dat$dates)
weekdays(dat$date[1]) 

dat1 <- dat[order(dat$dates),]

write.csv(dat1,"Ordered_user_ans.csv")

#d <- tapply(dat$PostId, week(dat$date), FUN = function(x){NROW(x)})  ## More than a year's data, so dont use this

## MoDel 1: ## Start with just the count of posts per week and create an ARIMA model

## TIme-series pre-processing 
e <- apply.weekly(zoo(dat1$PostId, dat1$date), FUN = function(x){NROW(x)}) ## This one is pretty accurate 
g <- na.approx(e,na.rm=TRUE)
h <- izoo2rzoo(g,from="2014-09-01", to="2015-11-12",tstep="weeks")
i <- apply.weekly(h,FUN = function(x){mean(x,na.rm=T)})
j <- coredata(i)

'''
## Approach for ARIMA modeling
https://www.otexts.org/fpp/8/7
1) Plot the data. Identify any unusual observations.
2) If necessary, transform the data (using a Box-Cox transformation) to stabilize the variance.
3) If the data are non-stationary: take first differences of the data until the data are stationary.
4) Examine the ACF/PACF: Is an AR(p) or MA(q) model appropriate?
5) Try your chosen model(s), and use the AICc to search for a better model.
6) Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals. If they do not look like white noise, try a modified model.
7) Once the residuals look like white noise, calculate forecasts.
'''

## Note: Implicit periodicity (as we aggregate to week). Hence decompose or periodicity or stl will not work
##
plot.ts(j)
## THe plot shows an increase in trend + genlte drift. 
## We verify the stationarity using ADF:
adf.test(j)  ## p-value is significant, so differencing not really necessary, as it is stochastic trend
acf(j)  ## This clearly shows the need to difference   
pacf(j)   ## 

k <- diff(j)
plot.ts(k)
adf.test(k)
acf(k)
pacf(k)

fit <- auto.arima(j)
## WE get same result ARIMA(0,1,1) or MA(1) for j as well as k time-series

plot(forecast(fit,h=20))
Acf(residuals(fit))
Box.test(residuals(fit), type="Ljung") ## NOrmal residuals

## Time series for each subset: R, Java, Python

## Model 2: ARIMAX. Add other variables into model as covariates
javaposts <- subset(dat,dat$java==1)
Rposts <- subset(dat,dat$r==1)
Pythonposts <- subset(dat,dat$python==1)
  
regular_ts_posts <- function(data_in)
  {
    e <- apply.weekly(zoo(data_in$PostId, data_in$date), FUN = function(x){NROW(x)}) ## This one is pretty accurate 
    g <- na.approx(e,na.rm=TRUE)
    h <- izoo2rzoo(g,from="2008-08-01", to="2015-09-12",tstep="weeks")
    i <- apply.weekly(h,FUN = function(x){mean(x,na.rm=T)})
    j <- coredata(na.approx(i))
    return(j)
  }

java_t <- regular_ts_posts(javaposts)
r_t <- regular_ts_posts(Rposts)
python_t <- regular_ts_posts(Pythonposts)

## MOdel fit is:
adjreg = sarima (y, 0,0,1, xreg=cbind(trend,x)) 

## The covariates are:

## average view count, count of posts with answers, no of hashtags co-occuring in the post, isclosed, isanswered

## Function to find the sum of metrics such as: count of posts with answers, no o  
regular_ts_sum <- function(data_in)
{
  e <- apply.weekly(zoo(data_in$PostId, data_in$date), sum) ## This one is pretty accurate 
  g <- na.approx(e,na.rm=TRUE)
  h <- izoo2rzoo(g,from="2008-08-01", to="2015-09-12",tstep="weeks")
  i <- apply.weekly(h,FUN = function(x){mean(x,na.rm=T)})
  j <- coredata(na.approx(i))
  return(j)
}

## Average value computations for a week such as average view count, no of hashtags co-occuring in the post 
regular_ts_mean <- function(data_in)
{
  e <- apply.weekly(zoo(data_in$PostId, data_in$date), FUN = function(x){mean(x,na.rm=T)}) ## This one is pretty accurate 
  g <- na.approx(e,na.rm=TRUE)
  h <- izoo2rzoo(g,from="2008-08-01", to="2015-09-12",tstep="weeks")
  i <- apply.weekly(h,FUN = function(x){mean(x,na.rm=T)})
  j <- coredata(na.approx(i))
  return(j)
}

## Function to find # Between of an edgelist 
## Matrix multiplication of hashmatrix of weekly subsets. Then compute  network metrics for R, Java, Python subset outside 

## Closeness and Eigen vector centrality for all the hashtags

centrality_func <- function(data_sub)
{
  hashm <- datasub 
  thash <- t(hashm) 
  tot <- as.matrix(thash) %*% as.matrix(hashm)  
  g  <- graph.adjacency(tot,weighted=TRUE)
  df <- get.data.frame(g)
  
}

week_dat <- dat

week_dat$week <- as.factor(paste(week(dat$date),"_",year(dat$date),sep=""))

per_week <- split(week_dat,week_dat$week)

d <- per_week[[10]]

# http://kateto.net/network-visualization

