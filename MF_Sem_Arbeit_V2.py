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
#import fix_yahoo_finance as yf
import yfinance as yf
from fredapi import Fred



############### Data Import from Yahoo Finance ###############
start_time = time.clock()
#source copy paste to excel: https://www.slickcharts.com/nasdaq100
ticker_yahoo = pd.read_excel('C:\\Users\\41799\\Documents\\Python Scripts\\MF_1_Sem\\nasdaq100.xlsx', 
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
    
############### Data Import from Yahoo Finance as Columns ###############  
        
def yahoo_stock_as_column(stocks, end_date, start_date):
    start_time = time.clock()
    df_folio = pd.DataFrame(yf.download(stocks[0], 
                      start = start_date, 
                      end = end_date, 
                      progress=False)['Adj Close'])
    df_folio.columns = [stocks[0]]#set stock TICKER
    


    for i in stocks[1:]:
        price = pd.DataFrame(yf.download(i, 
                          start = start_date, 
                          end = end_date, 
                          progress=False)['Adj Close'])
    
        df_folio[i] = price
        print('Completed:', i)
    print('Stocks Downloaded:', df_folio.shape[1])
    print(time.clock() - start_time, "seconds for data download after module import")
    return df_folio
        
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
n = len(np.unique(df_raw['Ticker'])) 
stockz = np.unique(df_raw['Ticker'])
print('Download took (seconds):', time.clock() - start_time)
############### Data Quality Checks ###############   
    
    
############### Calculate Statistics such as Cov, Corr, Beta, Means, Var etc..... ###############       
def log_returns(data):
    
	log_returns = np.log(data/data.shift(1))
	return log_returns


def pct_change(df, ticker):
    u = split(df, ticker)
    tick = u['Ticker']
    pct = u['Adj Close'].pct_change(1).fillna(0)
    #pct *= 100#Adjust for % i.e. *100
    data = pd.concat([tick, pct], axis = 1)
    data = data.rename(columns = {"Adj Close": "Daily Return(%)"})
    return data

def mean_market(df, ticker):
    mean_return = np.mean(pct_change(df, ticker)['Daily Return(%)'])
    return mean_return

def equal_index(df, ticker, n_stocks = n):
    s = np.array(pct_change(df, ticker)['Daily Return(%)'])
    weights = np.repeat(1/n_stocks, s.shape[0])#create equali weighted market index
    weighted_return = s * weights
    return np.array(weighted_return)


nasdaq = equal_index(df_raw, stockz[0])

for i in stockz[1:len(stockz)]:
    print(i)
    next_stock = equal_index(df_raw, i)
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

beta = {covar(df_raw, nasdaq, stockz[0])[0]: covar(df_raw, nasdaq, stockz[0])[3]} 
for i in range(1,len(stockz)):
    beta.update({covar(df_raw, nasdaq, stockz[i])[0]: covar(df_raw, nasdaq, stockz[i])[3]})

beta_arr = np.array([beta[i] for i in np.unique(df_raw['Ticker'])]) 

#Get rates from Fred API:
rates = ['DGS1MO', 'DGS3MO', 'DGS6MO', 'DGS1', 'DGS2', 'DGS3', 'DGS5',
         'DGS7', 'DGS10', 'DGS20', 'DGS30']#Treasury 1m, 3m, 6m, 1y ......30y
#Set api_key
fred = Fred(api_key='6be21606d68265a883ef83d0fd9b93a4')
rf_6m = fred.get_series(rates[2])# Time series from 1982 - 2020

#Split data set in to two half years:   
x = yahoo_stock_as_column(stockz, end_date, start_date)
x1 = x.head(int(x.shape[0]/2))
x2 = x.tail(int(x.shape[0]/2))   
#Quality Check: pd.concat([x1,x2]) - x -> zeros
rf_6m_sub = rf_6m.tail(x.shape[0])
rf_6m_2 = rf_6m_sub.tail(x2.shape[0])#there are NAs
rf_6m_1 = rf_6m_sub.head(x2.shape[0])#there are NAs
  
x1_log_return = log_returns(x1).dropna()
x2_log_return = log_returns(x2).dropna()

#Mean Return of NASDAQ 103 equally weighted
c_sum_1 = x1_log_return.cumsum()
c_sum_2 = x2_log_return.cumsum()
market_equal_w_1_mean = np.sum(c_sum_1.iloc[-1] * 1/x1_log_return.shape[1])#NASDAQ non-equal-w and non log return: 8276.85/8036.77 = 1.029873
market_equal_w_2_mean = np.sum(c_sum_2.iloc[-1] * 1/x2_log_return.shape[1])
#Time Series of Index equally weighted
market_equal_w_1 = np.matrix(x1_log_return)* 1/x1_log_return.shape[1]
market_equal_w_2 = np.matrix(x2_log_return)* 1/x2_log_return.shape[1]
market_equal_w_1_stock_index = np.sum(market_equal_w_1, axis = 1)#row_sum
market_equal_w_2_stock_index = np.sum(market_equal_w_2, axis = 1)#row_sum

#index = pd.DataFrame(market_equal_w_1_stock_index, columns = ['NASDAQ'])
#t_cov = pd.concat([pd.DataFrame(x1_log_return['AAPL'].reset_index())['AAPL'], 
#               index], axis = 1).cov()
#df = pd.concat([pd.DataFrame(x1_log_return['AAPL'].reset_index())['AAPL'], 
#               index], axis = 1)


def sample_cov(df, ticker, idx = 'NASDAQ'):
    
    x_samp_mean = np.sum(df[ticker]) / (df[ticker].shape[0] - 1)# sum(x)/N-1
    y_samp_mean = np.sum(df[idx]) / (df[idx].shape[0] - 1)
    a = df[ticker].apply(lambda i: i - x_samp_mean)
    b = df['NASDAQ'].apply(lambda i: i - y_samp_mean)
    cov = np.sum(a*b)/len(a - 1)#Sample Cov: N-1
    return cov

def beta(df, ticker, idx = 'NASDAQ'):
    cov = sample_cov(df, ticker)
    return cov / df[idx].var()

#Initiate Beta    
M = np.zeros((x1_log_return.shape[1],1))
index = pd.DataFrame(market_equal_w_1_stock_index, columns = ['NASDAQ'])
row = 0
for i in stockz[0:len(stockz)]:
    print(i)
    df = pd.concat([pd.DataFrame(x1_log_return[i].reset_index())[i], 
               index], axis = 1)
    b = beta(df, i)
    M[row] = b
    row += 1
    
df_beta = pd.DataFrame(M, columns = ['Beta']); df_beta['Ticker'] = stockz

#CAPM: ER(stock) = RiskFree + Beta(ER(market) - RiskFree))
t = 1/2#half year time period for calculation
rf = np.mean(rf_6m_1)/100#Conversion to percentage -> decimal
rf = np.exp(rf*t) - 1#Conversion of one year rate to half year with cont compounding

df_beta['CAPM Exp.Ret'] = df_beta['Beta'].apply(lambda b: rf + (b*(market_equal_w_1_mean - rf)))
emp = pd.DataFrame(x2_log_return.cumsum().iloc[-1].reset_index())
emp.columns = ['Ticker', 'Return_Empirical']
df = df_beta.merge(emp, left_on = 'Ticker', right_on = 'Ticker') 

#Calculation Check:
x2_log_return.cumsum().iloc[-1] 
start = x['AAPL'].iloc[127] 
end = x['AAPL'].iloc[-1]
np.log(end) - np.log(start)

sns.scatterplot(x = 'Beta', y = 'Return_Empirical', data = df)
sns.scatterplot(x = 'Beta', y = 'CAPM Exp.Ret', data = df)


    