# -*- coding: utf-8 -*-
"""
Created on Tue Apr 28 14:21:23 2020

@author: 41799
"""

#Module Import
import requests
import json
import pandas as pd
import numpy as np
import seaborn as sns
import datetime
import matplotlib.pyplot as plt
import quandl
from datetime import date
from datetime import datetime
from dateutil.relativedelta import relativedelta
import time

start_time = time.clock()

ticker = pd.read_excel('C:\\Users\\41799\\OneDrive - ZHAW\\TFE\\nasdaq100.xlsx', 
                       sheet_name = 'Ticker')
#JD is invalid ticker
ticker = ticker[~ticker.Symbol.str.contains("JD")]

ticker = list(ticker['Symbol'])
x = ticker[0]#First ticker to generate pd.DataFrame
#Quandl
quandl.ApiConfig.api_key = "1XyrZp9f9vsytsAEwyfs"
c = pd.DataFrame(quandl.get("WIKI/"+x+"")['Adj. Close'].tail(252))#252 = trading days / year
c['Ticker'] = np.repeat(x, c.shape[0])

#for i in ticker[1:len(ticker)]:
#    print(i)
#    a = pd.DataFrame(quandl.get("WIKI/"+i+"")['Adj. Close'].tail(252))#252 = trading days / year
#    a['Ticker'] = np.repeat(i, a.shape[0])
#    c = c.append(a)
#    
#print(time.clock() - start_time, "seconds")

for i in ticker[1:len(ticker)]:
    print(i)
    try:
        a = pd.DataFrame(quandl.get("WIKI/"+i+"")['Adj. Close'].tail(252))#252 = trading days / year
        a['Ticker'] = np.repeat(i, a.shape[0])
        c = c.append(a)
    except:
        pass
    
print(time.clock() - start_time, "seconds for data download after module import")
n = len(np.unique(c['Ticker']))#n = number of stocks
print('#Stocks:', n)
n = len(np.unique(c['Ticker'])#n = number of stocks

import sqlite3
from sqlalchemy import create_engine

#Write Data to database as backup:
#conn = sqlite3.connect('DB_NASDAQ100.db')
#cur = conn.cursor()
#engine = create_engine("sqlite:///DB_NASDAQ100.db")
#c.to_sql('Stocks', con = engine)
#df = pd.DataFrame(engine.execute("SELECT * FROM Stocks").fetchall())
#Split df to two semesters


def split(df, ticker):
    x = df[df['Ticker'] == ticker]
    return x

#there are companies where theres no data for 252 trading days
l = pd.DataFrame([split(c, h).shape[0] for h in np.unique(c['Ticker'])])
idx = l[l[0] < 252].index
reduce = np.unique(c['Ticker'])[idx][0]
c = c[~c.Ticker.str.contains(reduce)]

#sanity check:
k = pd.DataFrame([split(c, h).shape[0] for h in np.unique(c['Ticker'])])
check = k[k[0] < 252].index
print('Companies less than 252 rows:', len(check))

def pct_change(df, ticker):
    u = split(df, ticker)
    tick = u['Ticker']
    pct = u['Adj. Close'].pct_change(1).fillna(0)
    pct *= 100#Adjust for % i.e. *100
    data = pd.concat([tick, pct], axis = 1)
    data = data.rename(columns = {"Adj. Close": "Daily Return(%)"})
    return data

def mean_market(df, ticker):
    mean_return = np.mean(pct_change(df, ticker)['Daily Return(%)'])
    return mean_return

def covar():
    #blabla

mean_nasdaq_return_stock = [mean_market(c, stock) for stock in np.unique(c['Ticker'])]
mean_market = np.mean(mean_nasdaq_return_stock)












