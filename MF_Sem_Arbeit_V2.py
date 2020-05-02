# -*- coding: utf-8 -*-
"""
Created on Wed Apr 29 13:16:00 2020

@author: SIMON.BLACK
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
import fix_yahoo_finance as yf



############### Data Import from Yahoo Finance ###############
start_time = time.clock()
#source copy paste to excel: https://www.slickcharts.com/nasdaq100
ticker_yahoo = pd.read_excel('C:\\Users\\SIMON.BLACK\\Documents\\GitHub\\MF_1_Sem\\nasdaq100.xlsx', 
                       sheet_name = 'Ticker')     

ticker_yahoo = list(ticker_yahoo['Symbol'])

#Yahoo as data provider

end_date =  date.today() 
start_date = end_date - relativedelta(years = 1)

#Initialize Portfolio Data Frame
df_raw = pd.DataFrame(yf.download(ticker_yahoo[0], 
                      start = start_date, 
                      end = end_date, 
                      progress=False)['Adj Close'])
df_raw['Ticker'] = ticker_yahoo[0]#set stock TICKER

for i in ticker_yahoo[1:]:
    try:
        
        price = pd.DataFrame(yf.download(i, 
                          start = start_date, 
                          end = end_date, 
                          progress=False)['Adj Close'])
        
        price['Ticker'] = i
        df_raw = df_raw.append(price)
        print('Completed:', i)
    except:
        print('Failed', i)
        pass

############### Data Import from Yahoo Finance ###############
        
############### Data Quality Checks ###############   
    
    
def split(df, ticker):
    x = df[df['Ticker'] == ticker]
    return x

l = pd.DataFrame([split(df_raw, h).shape[0] for h in np.unique(df_raw['Ticker'])])
idx = l[l[0] < 252].index
if len(idx) < 1:
    print('good to continue')
else:
    reduce = np.unique(df_raw['Ticker'])[idx][0]
    df_raw = df_raw[~df_raw.Ticker.str.contains(reduce)]
    stockz = np.unique(df_raw['Ticker'])

#sanity check:
k = pd.DataFrame([split(df_raw, h).shape[0] for h in np.unique(df_raw['Ticker'])])
check = k[k[0] < 252].index
print('Companies less than 252 rows:', len(check))
if len(check) > 0:
    print('more need to be deleted')
else:
    print('good to continue') 
    
############### Data Quality Checks ###############   
    
    
############### Calculate Statistics such as Cov, Corr, Beta, Means, Var etc..... ###############       
def pct_change(df, ticker):
    u = split(df, ticker)
    tick = u['Ticker']
    pct = u['Adj. Close'].pct_change(1).fillna(0)
    #pct *= 100#Adjust for % i.e. *100
    data = pd.concat([tick, pct], axis = 1)
    data = data.rename(columns = {"Adj. Close": "Daily Return(%)"})
    return data

def mean_market(df, ticker):
    mean_return = np.mean(pct_change(df, ticker)['Daily Return(%)'])
    return mean_return

def equal_index(df, ticker, n_stocks = n):
    s = np.array(pct_change(df, ticker)['Daily Return(%)'])
    weights = np.repeat(1/n_stocks, s.shape[0])#create equali weighted market index
    weighted_return = s * weights
    return np.array(weighted_return)


nasdaq = equal_index(c, stockz[0])

for i in stockz[1:len(stockz)]:
    print(i)
    next_stock = equal_index(c, i)
    nasdaq += next_stock
    
mean_nasdaq_equal_w = np.mean(nasdaq)   

def covar(df, index, ticker):
    s = np.array(pct_change(df, ticker)['Daily Return(%)'])
    x = pd.DataFrame(np.array([s, index]).T)
    x.columns = [ticker, 'NASDAQ']
    #x1 = np.mean(x[ticker])
    #x2 = np.mean(x['NASDAQ'])
    x1 = np.sum(x[ticker]) / (x[ticker].shape[0] - 1)#Sample adjusted N-1
    x2 = np.sum(x['NASDAQ']) / (x['NASDAQ'].shape[0] - 1)
    a = x[ticker].apply(lambda i: i - x1)
    b = x['NASDAQ'].apply(lambda i: i - x2)
    cov = np.sum(a*b)/len(a - 1)#Sample Cov: N-1
    corr = cov/(x[ticker].std()*x['NASDAQ'].std())
    beta = cov / x['NASDAQ'].var()  
    return ticker, cov, corr, beta
    
#cov, corr, beta, ticker = covar(c, nasdaq, stockz[0])
    
##check against pandas built in without sample adjustment N-1:
#r = np.array(pct_change(df = c, ticker = 'AAPL')['Daily Return(%)'])
#x = pd.DataFrame(np.array([r, nasdaq]).T)
#x.columns = ['AAPL', 'NASDAQ']
#x.cov()
#x.corr()

beta = {covar(c, nasdaq, stockz[0])[0]: covar(c, nasdaq, stockz[0])[3]} 
for i in range(1,len(stockz)):
    beta.update({covar(c, nasdaq, stockz[i])[0]: covar(c, nasdaq, stockz[i])[3]})
for i in np.unique(c['Ticker']):
    
    print(beta[i])
beta_arr = np.array([beta[i] for i in np.unique(c['Ticker'])]) 
#mean_nasdaq_return_stock = np.array([mean_market(c, stock) for stock in np.unique(c['Ticker'])])

#Split data set in two half years:
np.mean(nasdaq)
rf_6m
    
    
    
    