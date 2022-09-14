rm(list = ls())
#i have manually imported the csv data from my computer since the 
#U.S. Bureau of Labor Statistics does not provide data in csv format 
#i copied the table and converted into csv file called CIP.all.item
#I also manually imported Gold and Bitcoin prices 
install.packages("quantmod")
install.packages("ggfortify")
getwd()
dir("Downloads")
Goldprice <- read.csv("Historical Gold Data .csv")
Bitcoinprice <- read.csv("Historical Bitcoin Data.csv")
CPI <- read.csv("CPI all item1.csv")
DJIA <- read.csv("DJIA.csv")




library(quantmod)
library(ggplot2)
library(wooldridge)
library(ggfortify)
library(plm);library(lmtest); library(stargazer)
cpi <- CPI$CPI

#Gold niminal Monthly Return
mydata_gold <- Goldprice[order(Goldprice$Date),]
GoldPrice <- c(mydata_gold$Close.Last)
dates_gold <- as.Date(c(mydata_gold$Date))
ts_gold <- xts(GoldPrice,dates_gold)
monthlyReturn(ts_gold)
#Gold Real Monthly Return
nomial_return_gold <- fortify(monthlyReturn(ts_gold))
nomial_return_gold1 <- nomial_return_gold$monthly.returns
real_return_gold <- ((1+nomial_return_gold1)/(1+cpi))-1
real_return_gold
#plot real-return vs CPI 
new_col_gold = real_return_gold
CPI$gold <- new_col_gold

ggplot(CPI, aes(CPI, gold)) +  # "color=class, shape=class" are now used in the two geoms. 
  geom_point() +
  geom_smooth() +   
  scale_color_grey() +
  scale_shape_manual(values=1:7)

Gold1 <- lm(real_return_gold ~ cpi)
Gold3 <- lm(real_return_gold ~ poly(cpi,3))
summary(Gold)
confint(Gold, level = 0.95)
Gold
#Assumptions
autoplot(Gold1)
stargazer(Gold1, type="text")
autoplot(Gold3)
stargazer(Gold3, type="text")
coeftest(Gold3, vcovHC)

#Bitcoin Monthly Return
mydata_bitcoin <- Bitcoinprice[order(Bitcoinprice$Date),]
BitcoinPrice <- c(mydata_bitcoin$Close.Last)
dates_bitcoin <- as.Date(c(mydata_bitcoin$Date))
ts_bitcoin <- xts(BitcoinPrice,dates_bitcoin)
monthlyReturn(ts_bitcoin)
#Bitcoin Real Monthly Return 
nomial_return_bitcoin <- fortify(monthlyReturn(ts_bitcoin))
nomial_return_bitcoin1 <- nomial_return_bitcoin$monthly.returns
real_return_bitcoin <- ((1+nomial_return_bitcoin1)/(1+cpi))-1
real_return_bitcoin
#Regress Real-Return With CPI
new_col_bitcoin = real_return_bitcoin
CPI$bitcoin <- new_col_bitcoin

Bitcoin <- lm(real_return_bitcoin ~ poly(cpi,3))
summary(Bitcoin)
confint(Bitcoin, level = 0.95)
Bitcoin
#Assumption fitted values and residuals
autoplot(Bitcoin)
stargazer(Bitcoin, type="text")
coeftest(Bitcoin, vcovHC)


#DJIA Monthly Return
mydata_DJIA <- DJIA[order(DJIA$X),]
mydata_DJIA$X.1 <- as.numeric(gsub(",","",mydata_DJIA$X.1))
DJIA1 <- c(mydata_DJIA$X.1)
dates_DJIA <- as.Date(c(mydata_DJIA$X))
ts_DJIA <- xts(DJIA1,dates_DJIA)
monthlyReturn(ts_DJIA)
#Bitcoin Real Monthly Return 
nomial_return_DJIA <- fortify(monthlyReturn(ts_DJIA))
nomial_return_DJIA <- nomial_return_DJIA$monthly.returns
real_return_DJIA <- ((1+nomial_return_DJIA)/(1+cpi))-1
real_return_DJIA
#Regress Real-Return with CPI
new_col_DJIA = real_return_DJIA
CPI$DJIA <- new_col_DJIA

ggplot(CPI, aes(CPI, DJIA)) +  # "color=class, shape=class" are now used in the two geoms. 
  geom_point() +
  geom_smooth() +   
  scale_color_grey() +
  scale_shape_manual(values=1:7)

DJIA2 <- lm(real_return_DJIA ~  cpi)
summary(DJIA2)
confint(DJIA2, level = 0.95)
#Assumption 
autoplot(DJIA2)
DJIA3 <- lm(nomial_return_DJIA ~ cpi)
autoplot(DJIA3)
stargazer(DJIA2, type="text")
coeftest(DJIA2, vcovHC)

#Bitcoin vs Gold

xd <- lm(real_return_bitcoin ~ real_return_gold)
ggplot(CPI, aes(gold, bitcoin)) +  # "color=class, shape=class" are now used in the two geoms. 
  geom_point() +
  geom_smooth() +   
  scale_color_grey() +
  scale_shape_manual(values=1:7)

autoplot(xd)
stargazer(xd, type="text")
coeftest(xd, vcovHC)