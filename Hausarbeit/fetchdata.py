# imports
from fredapi import Fred

# key: b2589170969f918aeeca118f82e296e9
import yfinance as yf

import pandas as pd
import numpy as np

from datetime import date
from datetime import datetime
from dateutil.relativedelta import relativedelta
import time

path = "C:/Users/phili/OneDrive - ZHAW/ZHAW/6.Sem/MF1/Hausarbeit/Finalissima/data/"

# NDX ticker for yf download
ticker_ndx = "MSFT AAPL AMZN FB GOOGL GOOG INTC NFLX PEP CSCO NVDA CMCSA ADBE PYPL AMGN COST TSLA CHTR AVGO TXN GILD SBUX QCOM TMUS MDLZ FISV INTU VRTX ADP BKNG AMD ISRG REGN BIIB CSX MU ATVI ILMN AMAT ADSK JD ADI WBA KHC EXC LRCX EA XEL ROST MNST DXCM EBAY CTSH MELI ORLY MAR LULU NXPI BIDU SIRI SGEN VRSK WDAY PAYX VRSN NTES PCAR KLAC IDXX ALXN CSGP SNPS CTAS ANSS CDNS INCY SPLK CERN XLNX ASML FAST MCHP CPRT DLTR CTXS SWKS ALGN BMRN ZM CDW CHKP MXIM TTWO TCOM ULTA WDC LBTYK NTAP EXPE FOXA FOX UAL LBTYA"

# All NASDAQ 100 Stocks
data = yf.download(
    ticker_ndx, start=date.today() - relativedelta(years=2), end=date.today()
)

data["Adj Close"].to_csv(path + "NDX100.csv")

# NASDAQ 100 Index
data_index = yf.download(
    "^NDX", start=date.today() - relativedelta(years=2), end=date.today()
)

data_index.to_csv(path + "NDX100_index.csv")

# Get rates from Fred API:
rates = [
    "DGS1MO",
    "DGS3MO",
    "DGS6MO",
    "DGS1",
    "DGS2",
    "DGS3",
    "DGS5",
    "DGS7",
    "DGS10",
    "DGS20",
    "DGS30",
]  # Treasury 1m, 3m, 6m, 1y ......30y
# Set api_key
fred = Fred(api_key="b2589170969f918aeeca118f82e296e9")
rf_6m = fred.get_series(rates[2])  # Time series from 1982 - 2020

rf_6m.to_csv(path + "fred.csv")

print("DONE")
