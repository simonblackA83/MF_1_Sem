## imports
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import datetime

sns.set_style("darkgrid")

####################################
## Data
####################################
path = "C:\\Users\\SIMON.BLACK\\OneDrive - Zurich Insurance\\ZHAW\Hausarbeit\\data\\"
df_ndx = pd.read_csv(path + "NDX100.csv")
df_ndx_index_full = pd.read_csv(path + "NDX100_index.csv")
df_fred = pd.read_csv(path + "fred.csv")

####################################
## NASDAQ 100 Aktien
####################################
df_ndx.set_index(df_ndx["Date"], inplace=True)
df_ndx.drop("Date", axis=1, inplace=True)

# log(return) bilden
log_return = df_ndx.apply(lambda x: np.log(x / x.shift(1)))
# erste Zeile = Na, entfernen
log_return = log_return.drop(df_ndx.index[0])
# Aufteilen in 2 hälften
log_return_1 = log_return.head(int(len(log_return) / 2))
log_return_2 = log_return.tail(int(len(log_return) / 2))

####################################
## NASDAQ 100 Index
####################################
df_ndx_index = df_ndx_index_full[["Date", "Adj Close"]]
df_ndx_index.set_index(df_ndx_index["Date"], inplace=True)
df_ndx_index.drop("Date", axis=1, inplace=True)
df_ndx_index.rename(columns={"Adj Close": "NDX Index"}, inplace=True)

# log(return) bilden
index_log_return = df_ndx_index.apply(lambda x: np.log(x / x.shift(1)))
# erste Zeile = Na, entfernen
index_log_return = index_log_return.drop(df_ndx_index.index[0])
# Aufteilen in 2 hälften
index_log_return_1 = index_log_return.head(int(len(index_log_return) / 2))
index_log_return_2 = index_log_return.tail(int(len(index_log_return) / 2))

####################################
## Treasury 6m
####################################
df_fred.set_index(df_fred.iloc[:, 0], inplace=True)
df_fred.drop(df_fred.columns[0], axis=1, inplace=True)

df_fred.index.name = "Date"
df_fred.rename(columns={df_fred.columns[0]: "Rate"}, inplace=True)

fred = df_fred.tail(int(len(df_ndx)))
# NA's entfernen
fred.dropna(inplace=True)
# Aufteilen in 2 hälften
fred_1 = fred.head(int(len(fred) / 2))
fred_2 = fred.tail(int(len(fred) / 2))

####################################
## Plot für NASDAQ 100 Index time series
####################################
sub = df_ndx_index_full[["Date", "Adj Close"]]
sub.set_index(sub["Date"], inplace=True)
sub.drop("Date", axis=1, inplace=True)
sub.index = pd.to_datetime(sub.index)
sns.set_style("darkgrid")
fig, ax = plt.subplots(figsize=(15, 7))

sub.plot(ax=ax, legend=None)
ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y %b"))
ax.set_title("NASDAQ-100 Index Adj Close")
plt.show()

####################################
## Plot für Treasury
####################################
fredplot = fred
fredplot.index = pd.to_datetime(fredplot.index)

fredplot.plot(ax=ax, legend=None)
ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y %b"))
ax.set_title("Treasury Rates 6m")
plt.show()

####################################
## Mittlere risikofreier Zins
####################################
# Mittlere risikofreier Zins über die ganze Reihe
ir_rf = fred.mean() / 100
# Mittlere risikofreie Zinsen der aufgeteilten Zeitreihen
ir_rf_1 = fred_1.mean() / 100
ir_rf_2 = fred_2.mean() / 100

####################################
## Mittlere Renditen
####################################
# Mittlere Rendite über die ganze Zeitreihe
r_i = log_return.mean() * len(log_return)
# Mittlere Renditen der aufgeteilten Zeitreihen
r_i_1 = log_return_1.mean() * len(log_return_1)
r_i_2 = log_return_2.mean() * len(log_return_2)

df = pd.DataFrame({"Exp Ret": r_i,}, index=r_i.index)
df1 = pd.DataFrame({"Exp Ret": r_i_1,}, index=r_i_1.index)
df2 = pd.DataFrame({"Exp Ret": r_i_2,}, index=r_i_2.index)

# Marktportfolio Rendite über die ganze Reihe
r_M = index_log_return.mean() * len(index_log_return)
# Marktportfolio Renditen der aufgeteilten Reihen
r_M_1 = index_log_return_1.mean() * len(index_log_return_1)
r_M_2 = index_log_return_2.mean() * len(index_log_return_2)

####################################
## Vola / sd
####################################
# Vola der Aktien über die ganze Reihe
df["Vola"] = log_return.std() * np.sqrt(len(log_return))
# Volas der Aktien aufgeteilten Reihen
df1["Vola"] = log_return_1.std() * np.sqrt(len(log_return_1))
df2["Vola"] = log_return_2.std() * np.sqrt(len(log_return_2))

# Vola des Marktportfolios über die ganze Reihe
sd_M = index_log_return.std() * np.sqrt(len(index_log_return))
# Vola des Marktportfolios der aufgeteilten Reihen
sd_M_1 = index_log_return_1.std() * np.sqrt(len(index_log_return_1))
sd_M_2 = index_log_return_2.std() * np.sqrt(len(index_log_return_2))


####################################
## Kovarianzen
####################################
df["Cov"] = np.repeat(0, len(df["Vola"]))
df1["Cov"] = np.repeat(0, len(df1["Vola"]))
df2["Cov"] = np.repeat(0, len(df2["Vola"]))

for i in df["Vola"].index:
    # Kovarianz ganze Reihe
    df.loc[i, "Cov"] = np.cov(log_return[i], index_log_return.squeeze())[0][1]

for i in df1["Vola"].index:
    # Kovarianz erste hälfte
    df1.loc[i, "Cov"] = np.cov(log_return_1[i], index_log_return_1.squeeze())[0][1]

for i in df2["Vola"].index:
    # Kovarianz zweite hälfte
    df2.loc[i, "Cov"] = np.cov(log_return_2[i], index_log_return_2.squeeze())[0][1]

####################################
## Beta
####################################
# Beta ganze Reihe
df["Beta"] = df["Cov"] / index_log_return.var()[0]
# Beta's der aufgeteilten Reihen
df1["Beta"] = df1["Cov"] / index_log_return_1.var()[0]
df2["Beta"] = df2["Cov"] / index_log_return_2.var()[0]

####################################
## Jensen-Index, brauchts evtl gar nicht
####################################
df["Jensen"] = r_i - ir_rf[0] - df["Beta"] * (r_M[0] - ir_rf[0])
df1["Jensen"] = r_i_1 - ir_rf_1[0] - df1["Beta"] * (r_M_1[0] - ir_rf_2[0])
df2["Jensen"] = r_i_2 - ir_rf_2[0] - df2["Beta"] * (r_M_1[0] - ir_rf_2[0])

####################################
## Sharpe-Ratio, brauchts evtl gar nicht
####################################
df["Sharpe"] = (df["Exp Ret"] - ir_rf[0]) / df["Vola"]
df1["Sharpe"] = (df1["Exp Ret"] - ir_rf_1[0]) / df1["Vola"]
df2["Sharpe"] = (df2["Exp Ret"] - ir_rf_2[0]) / df2["Vola"]

####################################
## Systematisches Risiko, brauchts evtl gar nicht
####################################
df["Srisk"] = df["Beta"] ** 2 * sd_M[0] ** 2
df1["Srisk"] = df1["Beta"] ** 2 * sd_M_1[0] ** 2
df2["Srisk"] = df2["Beta"] ** 2 * sd_M_2[0] ** 2

####################################
## Idiosynkratisches Risiko, brauchts evtl gar nicht
####################################
df["Irisk"] = df["Vola"] ** 2 - df["Srisk"]
df1["Irisk"] = df1["Vola"] ** 2 - df1["Srisk"]
df2["Irisk"] = df2["Vola"] ** 2 - df2["Srisk"]

print(df.head())
print(df1.head())
print(df2.head())

####################################
## Kapitalmarktlinie (cml)
####################################
# Funktion für die Geraden der sml/cml
def lin_fun(x, a, b):
    return a + x * b


# Punkte der cml
x = np.linspace(0, max(df1["Vola"]), len(df1["Vola"]))
y = lin_fun(x=x, a=ir_rf_1[0], b=(r_M_1[0] - ir_rf_1[0]) / sd_M_1[0])

# cml-Plot
sns.scatterplot(x="Vola", y="Exp Ret", data=df1, color="grey")
sns.lineplot(x=x, y=y, color="grey")

plt.scatter(
    x=df1.loc[["AAPL"]]["Vola"][0],
    y=df1.loc[["AAPL"]]["Exp Ret"][0],
    color="red",
    label="Apple",
)
plt.scatter(
    x=df1.loc[["GOOG"]]["Vola"][0],
    y=df1.loc[["GOOG"]]["Exp Ret"][0],
    color="green",
    label="Google A",
)
plt.scatter(
    x=df1.loc[["GOOGL"]]["Vola"][0],
    y=df1.loc[["GOOGL"]]["Exp Ret"][0],
    color="darkgreen",
    label="Google B",
)
plt.scatter(
    x=df1.loc[["MSFT"]]["Vola"][0],
    y=df1.loc[["MSFT"]]["Exp Ret"][0],
    color="blue",
    label="Microsoft",
)
plt.scatter(
    x=df1.loc[["AMZN"]]["Vola"][0],
    y=df1.loc[["AMZN"]]["Exp Ret"][0],
    color="purple",
    label="Amazon",
)
plt.scatter(
    x=df1.loc[["FB"]]["Vola"][0],
    y=df1.loc[["FB"]]["Exp Ret"][0],
    color="yellow",
    label="Facebook",
)
plt.scatter(x=sd_M_1, y=r_M_1, label="Market", color="black")
plt.scatter(x=0, y=ir_rf_1, label=r"$r_{rf}$", color="lightblue")
plt.xlabel(r"$\sigma$")
plt.ylabel(r"$\bar{r}$")
plt.title("Kapitalmarktlinie")
plt.legend()
plt.show()

####################################
## Wertpapierlinie (sml)
####################################
# Punkte der sml
x = np.linspace(0, max(df1["Beta"]), len(df1["Beta"]))
y = lin_fun(x=x, a=ir_rf_1[0], b=r_M_1[0] - ir_rf_1[0])

# sml-Plot
sns.scatterplot(x="Beta", y="Exp Ret", data=df1, color="grey")
sns.lineplot(x=x, y=y, color="grey")
plt.scatter(
    x=df1.loc[["AAPL"]]["Beta"][0],
    y=df1.loc[["AAPL"]]["Exp Ret"][0],
    color="red",
    label="Apple",
)
plt.scatter(
    x=df1.loc[["GOOG"]]["Beta"][0],
    y=df1.loc[["GOOG"]]["Exp Ret"][0],
    color="green",
    label="Google A",
)
plt.scatter(
    x=df1.loc[["GOOGL"]]["Beta"][0],
    y=df1.loc[["GOOGL"]]["Exp Ret"][0],
    color="darkgreen",
    label="Google B",
)
plt.scatter(
    x=df1.loc[["MSFT"]]["Beta"][0],
    y=df1.loc[["MSFT"]]["Exp Ret"][0],
    color="blue",
    label="Microsoft",
)
plt.scatter(
    x=df1.loc[["AMZN"]]["Beta"][0],
    y=df1.loc[["AMZN"]]["Exp Ret"][0],
    color="purple",
    label="Amazon",
)
plt.scatter(
    x=df1.loc[["FB"]]["Beta"][0],
    y=df1.loc[["FB"]]["Exp Ret"][0],
    color="yellow",
    label="Facebook",
)

plt.scatter(x=1, y=r_M_1, label="Market", color="black")
plt.scatter(x=0, y=ir_rf_1, label=r"$r_{rf}$", color="lightblue")

plt.xlabel(r"$\beta$")
plt.ylabel(r"$\bar{r}$")
plt.title("Wertpapierlinie")
plt.legend()
plt.show()



















