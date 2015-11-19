### MIS586 - eye2data Fall 2015 team
### Tweepy package streams twitter data and pushes tweet by tweet into mongoDB database.

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

log = open("Error.txt", "w")

os.chdir('C:/eye2data/Twitter')

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)
#save_file = open("Data_science_keywords.json", 'a')

client = MongoClient('localhost', 27017)
db = client['eye2data']
collection = db['datascience_101415']
#mongo_tweet = json.loads(data)
keywords = open("Key_words.txt").read()
keywords = keywords.split("\n")

class CustomStreamListener(tweepy.StreamListener):
    def __init__(self, api):
        self.api = api
        super(tweepy.StreamListener, self).__init__()

    def on_data(self, tweet):
        #print tweet.lower()
        #print tweet
        #time.sleep(1)
        #save_file.write(tweet)
        collection.insert(json.loads(tweet))

    def on_error(self, status_code):
        print >> sys.stderr, 'Encountered error with status code:', status_code
        return True # Don't kill the stream
        time.sleep(1)
        print "Stream restarted"

    def on_timeout(self):
        print >> sys.stderr, 'Timeout...'
        time.sleep(10)
        return True # Don't kill the stream
        print "Stream restarted"

def start_stream():
    while True:
        try:
            sapi = tweepy.streaming.Stream(auth, CustomStreamListener(api))
            sapi.filter(track=keywords) ## Can also pass a file directly
            #sapi.filter(track=["eye2data"])
            #time.sleep(10)
        except Exception as err:
            print "Problem while streaming as:" ,  str(err)
            time.sleep(10)
            continue

start_stream()