
library(car)
library(tm)
library(DataCombine)

answers <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\StackExchange_Answers_Data.csv")

users <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\StackExchange_Users_Data.csv")
userslat <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Progress Report - 2\\Stack Exchange\\SO_Posts__Users_lat.csv")

#posts <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\StackExchange_Posts_Data.csv")
posts2 <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Stack Exchange\\StackExchange_Posts_Data_Subset.csv")
posts <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Progress Report - 2\\Stack Exchange\\SO_Posts_with_brackets.csv")

keywords <- read.csv("C:\\Users\\karthik\\Google Drive\\Course_projects\\Sem_3\\MIS586 Big Data\\Twitter\\Keywords.csv",header=F)

users$Id <- as.factor(users$Id)
length(levels(users$Id))
answers$OwnerUserId <- as.factor(answers$OwnerUserId)  
length(levels(answers$OwnerUserId))

posts$OwnerUserId <- as.factor(posts$OwnerUserId)
length(levels(posts$OwnerUserId))
## Monthly user of keyword count: 
## aggregate of all data



# Create corpus of tweets
'''
Replaces <- data.frame(from = c("<", ">"), to = c(" ", " "))
d <- FindReplace(data = posts,Var="Tags", replaceData = Replaces,
                 exact = FALSE, from = "from",to = "to")
'''
#processedCorpus = Corpus(VectorSource(d$Tags))
processedCorpus = Corpus(VectorSource(posts$Tags))

# Preprocess the tweets - convert to lower, remove punctuation, remove stop words, stemming
processedCorpus = tm_map(processedCorpus, tolower)
processedCorpus = tm_map(processedCorpus, PlainTextDocument)
#processedCorpus = tm_map(processedCorpus, removePunctuation)
#processedCorpus = tm_map(processedCorpus, removeWords, stopwords("english"))
#processedCorpus = tm_map(processedCorpus, stemDocument)

# Document Term Matrix from the Processed Corpus
dtmProcessed = DocumentTermMatrix(processedCorpus)
#ncol(dtmProcessed)

dtmSparseRemoved = removeSparseTerms(dtmProcessed, 0.999)
#nrow(as.data.frame(as.matrix(removeSparseTerms(dtmProcessed, 0.99))))

#ncol(as.data.frame(as.matrix(removeSparseTerms(dtmProcessed, 0.9))))

d <- dtmSparseRemoved[, intersect(colnames(dtmSparseRemoved), keywords$V1)]

programs <- dtmProcessed[, intersect(colnames(dtmProcessed), c("r","python","java"))]

prog <- as.data.frame(as.matrix(dtmProcessed))
prog_concise <- as.data.frame(as.matrix(dtmSparseRemoved))

setwd("C:/Users/karthik/Google Drive/Course_projects/Sem_3/MIS586 Big Data/Progress Report-3/SO")

write.csv(prog,"termdoc_all.csv")
write.csv(prog_concise,"termdoc_315.csv")

#rprog <- prog[prog$r==1,]

#h <- as.data.frame(d,row.names=F)

e <- as.POSIXct(posts$CreationDate, format="%m/%d/%Y %H:%M",tz="GMT")

f = strftime(e, "%Y-%m")

agg <- cbind(f,h)

m <- list()
for (i in 2:ncol(agg))
{
m[[i]] <- aggregate(agg[,i] ~ agg[,1],data=agg,sum)
}

o <- list.data.frame(m)

p <- m[2:length(m)]

q <- do.call(cbind.data.frame, p)
data.frame(Reduce(cbind, p))

write.csv(q,"monthly.csv")

data("crude")
ee <- DocumentTermMatrix(crude)
#nrow(as.data.frame(as.matrix(removeSparseTerms(ee, 0.9999))))
d <-as.data.frame(as.matrix(removeSparseTerms))

#fix(prog)

## We need to get users favoritism to java, R and python

ques_ans <- merge(posts,answers,by.x=c("Id"),by.y=c("ParentId")) 
ques_users <- merge(posts, users,by.x=c("OwnerUserId"),by.y=c("Id")) 

post_mat <- cbind(posts,prog_concise)

ques_users_prog <- merge(post_mat, users,by.x=c("OwnerUserId"),by.y=c("Id"),all.x=TRUE) 

ques_users_prog$python <- bitwOr(ques_users_prog[,234],ques_users_prog[,235])
ques_users_prog$python <- bitwOr(ques_users_prog$python,ques_users_prog[,236])

ques_users_prog$r <- bitwOr(ques_users_prog[,239],ques_users_prog[,238])

dates <- as.POSIXct(posts2$CreationDate, format="%m-%d-%Y %H:%M")
months = strftime(dates, "%Y-%m")
agg <- cbind(months,ques_users_prog)

agg$OwnerUserId <- as.factor(agg$OwnerUserId)

r_count <- aggregate(r ~ OwnerUserId + months ,agg,sum)

java_count <- aggregate(agg[,141] ~ OwnerUserId + months ,agg,sum)
python_count <- aggregate(python ~ OwnerUserId + months ,agg,sum)

colnames(java_count)[3] <- "java"

monthly_c <- data.frame(R = r_count$r,Java = java_count$java,Python = python_count$python)

a <- bitwOr(monthly_c[,1],monthly_c[,2])
b <- bitwOr(a,monthly_c[,3])

monthly_favorite <- colnames(monthly_c)[apply(monthly_c,1,which.max)]
monthly_favorite[b==0] <- "None"

Monthly_user_data <- cbind(java_count[,1:2], monthly_favorite,monthly_c)

answer_month <- Monthly_user_data[!monthly_favorite=="None",]

answer_month$rr <- answer_month$R/(answer_month$R+answer_month$Python+answer_month$Java)
answer_month$rp <- answer_month$Python/(answer_month$R+answer_month$Python+answer_month$Java)
answer_month$rj <- answer_month$Java/(answer_month$R+answer_month$Python+answer_month$Java)

index <- match(answer_month$OwnerUserId, as.factor(user_list))

sub <- answer_month[Monthly_user_data$OwnerUserId %in% as.factor(user_list),] 

answer_m <- merge(answer_month,user_list,by.x="OwnerUserId",by.y=)

for (i in 1:length(answer_m))
{
  for (j in 1: nrow(answer_mp[[i]]))
 { answer_mp[[i]]$  

write.csv(Monthly_user_data),"Monthly_user_favorite.csv")

## Commonly updating users who change from one to another skill questions



t_r <- aggregate(r ~ OwnerUserId  ,agg,sum)
t_java <- aggregate(agg[,141] ~ OwnerUserId  ,agg,sum)
t_python <- aggregate(python ~ OwnerUserId ,agg,sum)

java_o <- t_java[order(-t_java$java),] 

r_o <- t_r[order(-t_r$r),] 
python_o <- t_python[order(-t_python$python),] 

user_list <- rbind(python_o[1:10,1],r_o[1:10,1],java_o[1:10,1])

colnames(t_java)[2] <- "java"

t <- data.frame(R = t_r$r,Java = t_java$java,Python = t_python$python)

a <- bitwOr(t[,1],t[,2])
b <- bitwOr(a,t[,3])

t_favorite <- colnames(t)[apply(t,1,which.max)]
t_favorite[b==0] <- "None"

users_fav <- cbind(users,t_favorite)
users_f <- cbind(users_fav,userslat[,c('Latitude','Longitude')]) 

users_f$Latitude[users_f$Latitude==""] <- NA
users_f$Longitude[users_f$Longitude==""] <- NA

users_f$Latitude <- as.numeric(as.character(users_f$Latitude))
users_f$Longitude <- as.numeric(as.character(users_f$Longitude))

users_fav_languages <- users_f[!t_favorite=="None",]

#users_fav_languages <- users_fav[!t_favorite=="None",]

write.csv(users_f,"Users_language.csv")

#r_count <- aggregate(r ~ OwnerUserId,agg,sum) 

# bitwOr(c(1,1,0,0),c(1,0,1,0))

library(ggmap)
library(ggplot2)

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
kk <- mp+ geom_point(aes(x=Latitude, y=Longitude) ,color="blue", size=3,data=users_f) 

points <- data.frame(lon = users_f$Longitude,lat = users_f$Lat)

library(sp)
library(rworldmap)

coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$ADMIN  
}

coords2country(points)
