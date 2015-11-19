### MIS586 - eye2data Fall 2015 team
### Tweepy package streams Job users historical tweets and dumps into mongoDB database.

import sys
import tweepy
import json
import os
import csv
import time
import pymysql
import codecs
import sys
import pandas as pd
from pymongo import MongoClient
import re

os.chdir('C:/eye2data/Job_tweets')
client = MongoClient('localhost', 27017)
db = client['eye2data']
collection = db['datascience_Jobs11022015']

#Twitter API credentials
consumer_key = 
consumer_secret =
access_key = 
access_secret  = 


def get_all_tweets(screen_name):
	#Twitter only allows access to a users most recent 3240 tweets with this method
	## Even Twitter package gives something similar to this
    ## Twython gives upto 10 days historical
	#authorize twitter, initialize tweepy
	auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
	auth.set_access_token(access_key, access_secret)
	api = tweepy.API(auth)
	
	#initialize a list to hold all the tweepy Tweets
	alltweets = []	
	
	#make initial request for most recent tweets (200 is the maximum allowed count)
	new_tweets = api.user_timeline(screen_name = screen_name,count=200)
	
	#save most recent tweets
	alltweets.extend(new_tweets)
	
	#save the id of the oldest tweet less one
	oldest = alltweets[-1].id - 1
	
	#keep grabbing tweets until there are no tweets left to grab
	while len(new_tweets) > 0:
		print "getting tweets before %s" % (oldest)
		
		#all subsiquent requests use the max_id param to prevent duplicates
		new_tweets = api.user_timeline(screen_name = screen_name,count=100,max_id=oldest)
		
		#save most recent tweets
		alltweets.extend(new_tweets)
		
		#update the id of the oldest tweet less one
		oldest = alltweets[-1].id - 1
		counter = counter + 1
		#print "%s tweets downloaded so far" % (len(alltweets))
		#print alltweets[1]
	with open('temp.json', 'a') as f:
	    for etweet in range(0,len(alltweets)):
             json.dump(alltweets[etweet]._json,f,indent=1) 
	#for etweet in range(0,len(alltweets)):
	#    tw = alltweets[etweet]
	#    twe = tw._json
	#    into = json.loads(twe)
	#    collection.insert(into)
		#collection.insert(alltweets)
   
	
if __name__ == '__main__':
	#pass in the username of the account you want to download
    counter = 0
    users =  ["karthikarizona","KaggleCareers","BigDataJobsCom",
                     "PalantirTech","analyticbridge","AnalyticCareers","pyjobo",
                     "Python_jobs","RStatsJobs","StatsJobsite",
                     "TechyRecruiting","lennysjobblog","nfpdata"]  
    for entry in users:
        get_all_tweets (entry)
