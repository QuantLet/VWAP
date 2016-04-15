# -*- coding: utf-8 -*-
"""
Created on Tue Mar 29 22:58:43 2016

@author: Sergey
"""
import csv
import urllib2
import StringIO
import numpy as np
import pylab as pl

tickers = open('tickers100.txt')

tckrs = []
for line in tickers:
    tckrs.append(line[:-1])

nmbrs = '0123456789'

response = urllib2.urlopen('http://www.google.com/finance/getprices?i=1200&p=5d&f=d,o,h,l,c,v&df=cpct&q=IBM')
html = response.read()
cr = csv.reader(response)

for row in cr:
    print row
    
f = StringIO.StringIO(html)
reader = csv.reader(f, delimiter=',')

rows = []
for row in reader:
#    print '\t'.join(row)
    if sum([x in nmbrs for x in row[0]]) == len(row[0]):
        rows.append(row)
        print row[1]


"""Yahoo source: only 
   1-minute intervals and
   about 1100 datapoints"""
        
Table = []
for tkr in tckrs:
    prt1     = 'http://chartapi.finance.yahoo.com/instrument/20.0/'
    prt2     = '/chartdata;type=quote;range=50d/csv'
    adr      = prt1 + tkr + prt2
    response = urllib2.urlopen(adr)
    html     = response.read()
    f        = StringIO.StringIO(html)
    reader   = csv.reader(f, delimiter=',')
    
#    Volume = []
#    Date   = []
    VolDict = {}
    for row in reader:
        print '\t'.join(row)
        if sum([x in nmbrs for x in row[0]]) == len(row[0]):
            VolDict[row[0]] = row[-1]
#            Volume.append(row[-1])
#            Date.append(row[-1])
            
    Table.append(VolDict)
    

"""Google source: more data,
   as it seems"""

Table = []
for tkr in tckrs:
    #'600' in the url is 600 seconds, that is, 10 minutes
    prt1     = 'http://www.google.com/finance/getprices?i=600&p=100d&f=d,o,h,l,c,v&df=cpct&q='
    adr      = prt1 + tkr
    response = urllib2.urlopen(adr)
    html     = response.read()
    f        = StringIO.StringIO(html)
    reader   = csv.reader(f, delimiter=',')
    
#    Volume = []
    Close = []
#    Date   = []
#    VolDict = {}
    for row in reader:
        print '\t'.join(row)
        if sum([x in nmbrs for x in row[0]]) == len(row[0]):
#            VolDict[row[0]] = row[-1]
#            Volume.append(int(row[-1]))
            Close.append(float(row[1]))
#            Date.append(row[-1])
            
#    Table.append(Volume)  
    Table.append(Close)


for vld in Table:
    print len(vld) 
    
"""Save the downloaded data into a csv file
"""
with open("nasdaq_prices.csv", "w") as output: #or nasdaq_vols.csv
    writer = csv.writer(output, lineterminator='\n')
    writer.writerows(Table) 
    
    
    