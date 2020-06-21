#1. Wie sieht der Zahlungsstrom einer Anleihe mit Nennwert 100, 10-jähriger Restlaufzeit und
#jährlicher Zahlung einer Couponrate C = 5% aus?
c = .05; mat = 10; notional = 100
CF = rep(c*notional, mat)
CF[mat] = CF[mat]+notional
#2. Bestimmen Sie den Preis der Anleihe (= Barwert des Zahlungsstroms) bei einer mittleren
#Umlaufrendite ?? = 6%

PV = function(CF, r){
  n = seq(1, mat)
  p = CF / (1 + r)**n
  return(p)
}
Bar_Wert = sum(PV(CF, r = .06))
  
#3. Schreiben Sie ein S-Plus/R-Skript zur Berechnung des Preises einer Anleihe für beliebige
#Restlaufzeiten, Couponraten und Umlaufrenditen.
PV = function(Notional, Coupon, Maturity, Frequency, r){
  CF = rep((Coupon/Frequency)*Notional, Maturity*Frequency)
  k = seq(1, Maturity*Frequency, by = 1)
  discounted_CF = CF / (1 + (r/Frequency))**k
  discounted_Notional = Notional / (1 + r/Frequency)**(Maturity*Frequency)
  discounted_CF[length(discounted_CF)] = discounted_CF[length(discounted_CF)] + discounted_Notional
  return(discounted_CF)
}

Bar_Wert = sum(PV(Notional = 100, Coupon = .05, Maturity = 10, Frequency = 4, r = .06))

(2*10**6)/(1.03)**5

y = 9*93.20
x = 7*101

yield = .03
coupon = .065
notional = 100
maturity = 10
freq = 0.5

duration_cont_disc = function(yield, coupon, notional, maturity, freq){
  t = seq(freq,maturity,freq)
  CF = rep(freq*coupon*notional,length(t))
  CF[length(t)] = CF[length(t)] + notional
  DF = exp(-t * yield)
  PV_CF = DF*CF
  price = sum(PV_CF)
  w = PV_CF / price
  D = sum(t * w)
  values = list(D, price)
  return(values)
}

A = duration_cont_disc(yield = .03, coupon = .065, notional = 100,
                   maturity = 10,freq = 0.5)
B = duration_cont_disc(yield = .03,coupon = .03, notional = 100,
                       maturity = 5,freq = 0.5)
C = duration_cont_disc(yield = .03,coupon = .08, notional = 100,
                       maturity = 20,freq = 0.5)

Liab_duration = duration_cont_disc(yield = .03,coupon = 0, notional = (2*10**6),
                                   maturity = 5,freq = 0.5)

duration_cont_disc(yield = .04, coupon = .065, notional = 100,
                   maturity = 10,freq = 0.5)

####Forward Rates##########

#1a)
s1 = 0.063; s2 = 0.069
f = ((1 + s2)**2) / (1 + s1) - 1

#1b)
spot = c(5.0, 5.3, 5.6, 5.8, 6.0, 6.1)/100

M = matrix(NA, nrow = length(spot) - 1, ncol = 1)
for (j in 1:length(spot)){
  M[j] = ((((1 + spot[j + 1])**(j+1)) / (1 + spot[1]))**(1/j)) - 1
}

#1c)
CF = c(-40, 10, 10, 10, 10, 10, 10)#Annual CFs
yield_curve = spot#data from prev. exercise
t = seq(from = 1, to = length(yield_curve), by = 1)
DF = 1/(1 + yield_curve[t])**t
DF = c(1, DF)
CF * DF



