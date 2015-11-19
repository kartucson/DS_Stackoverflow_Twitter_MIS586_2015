import requests
import csv
import BeautifulSoup
Soup = BeautifulSoup.BeautifulSoup
import argparse

brandCount = []

with open("master_brands3.csv", 'rb') as csvfile:
    brandsData = csv.reader(csvfile, delimiter=',', quotechar='|')

    for row in brandsData:
        r = requests.get('http://www.google.com/search',
                         params={'q':row,
                                 "tbs":"li:1"}
                        )
        soup = Soup(r.text)

        print str(row) + soup.find('div',{'id':'resultStats'}).text

resultFile = open("output.csv",'wb')
wr = csv.writer(resultFile, dialect='excel')
wr.writerows(brandCount)