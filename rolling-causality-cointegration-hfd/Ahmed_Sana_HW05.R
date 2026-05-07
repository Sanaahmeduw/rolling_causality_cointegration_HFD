###########################################################################
#    		Quantitative strategies on High Frequency Data
#					          2024/2025
#				      Sana Ahmed ID:XXXXXX
#		University of Warsaw, Faculty of Economic Sciences
###########################################################################
# 5. Cointegration, Granger causality - rolling analyses
###########################################################################

library(xts)
library(quantmod)
library(zoo)
library(urca)

# setting working directory

setwd("...")

#############################################################
# Loading Data

load("US.data.RData")

# Checking data structure of the data
head(US.data)
tail(US.data)

# open, high, low, close prices and volume for:
# PEP: Pepsi Co.
# PG: Procter & Gamble

# Creating a dataset with just close prices

US.data.close <- US.data[,grep("close", names(US.data), 
                               fixed = TRUE)]

head(US.data.close)

# Calculating log-returns
# in terms of basis points (bps) 1bps = 0.01% = 0.0001

US.data.r <- 10000*diff.xts(log(US.data.close))

head(US.data.r)
tail(US.data.r)

# Object with selected columns 

US.data2 <- merge(US.data.close[, c("PEP.close", "PG.close")],
                  US.data.r[, c("PEP.close", "PG.close")])

head(US.data2)

names(US.data2)[3:4] <- c("PEP.return", "PG.return")

# lets see the plot

layout(matrix(1:4, 2, 2))
chart_Series(US.data2$PEP.close)
chart_Series(US.data2$PG.close)
chart_Series(US.data2$PEP.return)
chart_Series(US.data2$PG.return)
layout(matrix(1))


#############################################################
# testing for cointegration 

# first step of Engle-Granger procedure - regression between variables

model.coint <- lm(PEP.close ~ PG.close - 1, 
                  # -1 means a model without a constant term
                  data = US.data2)

summary(model.coint)

# Checking a list of elements of model results
names(model.coint)

# we are interested in residuals

head(model.coint$residuals)

# Lets see the plot of residuals

plot(model.coint$residuals, type = "l")

# second step of Engle-Granger procedure 
# - testing for stationarity of model residuals

# ADF test

adf.test <- ur.df(model.coint$residuals,
                  type = c("drift"), 
                  lags = 1)   

summary(adf.test)

# Value of test-statistic is: -2.5344 3.2118 which is higher than the 5% critical value (-2.86)
# therefore we cannot reject the null about non-stationarity. 
# It shows NO COINTEGRATION


# PP Test 

pp.test <- ur.pp(model.coint$residuals,
                 type = c("Z-tau"), 
                 model = c("constant")) 
summary(pp.test)

# Value of test-statistic, type: Z-tau  is: -2.4464, which higher than the 5% critical value (-2.862219)
# so we fail to reject the null about non-stationarity.


# KPSS

kpss.test <- ur.kpss(model.coint$residuals,
                     type = c("mu")) # constant deterministic component

summary(kpss.test)

# Value of test-statistic is: 21.4438 
# is higher than the 5% critical value (0.463)
# so we reject the null about STATIONARITY

# In KPSS if the test statistic is higher than the critical value, 
# we reject the null hypothesis and when test statistic is lower 
# than the critical value, we cannot reject the null hypothesis.


# conclusion: we conclude that cointegration between PEP.close and PG.close 
# IS NOT present in the whole sample


# Rolling Window Based Analysis 

names(US.data2)

rollcoint240.adf <- rollapply(US.data2[,1:2],
                              width = 240, # window width = 4 hours
                              FUN = function(x) 
                                summary(
                                  ur.df(lm(PEP.close ~ PG.close - 1, 
                                           data = x 
                                  )$residuals, 
                                  type = c("drift"), # constant deterministic component
                                  lags = 1 # lags for augmentation of ADF
                                  )
                                )@teststat[1]
                              ,  
                              by.column = FALSE,  
                              by = 60, # for 60 mins 
                              align = "right")

# Checking results

tail(rollcoint240.adf, 61)
head(rollcoint240.adf, 10)

# the results are calculated every 60 minutes, as starting time is 9:31 so ADF value calculated for 15:30 (in tail) shows correctly set by value. 
# for the remaining rows there are missing values

# lets impute missing observations
# with the last non-missing

# na.locf() - last observations carried forward
# replaces missing value with the last non-missing

rollcoint240.adf <- na.locf(rollcoint240.adf,
                            na.rm = FALSE) # We won't remove NAs from the beginning 
# of resulting series

tail(rollcoint240.adf, 61)

# now the most recent value is repeated for
# next 62 minutes, before the next test result is available

# lets store the critical value - here once every 4 hours
# (by = 240)

rollcoint240.adf.cval <- rollapply(US.data2[,1:2],
                                   width = 240, 
                                   FUN = function(x) 
                                     summary(
                                       ur.df(lm(PEP.close ~ PG.close - 1, 
                                                data = x
                                       )$residuals, 
                                       type = c("drift"),
                                       lags = 1   
                                       )
                                     )@cval[1,2]
                                   ,  
                                   by.column = FALSE,  
                                   by = 60, 
                                   align = "right")

# Checking its unique values
tail(rollcoint240.adf.cval, 100)
summary(adf.test)
table(rollcoint240.adf.cval)

# the same for the whole period = -2.88
# (in fact it depends on the number of observations,
# which is the same for every subperiod)

# Saving object

adf.cval <- -2.88

# Checking ADF test statistic 

chart_Series(rollcoint240.adf)

# we add a reference line at the critical value level
abline(h = adf.cval, lty = 2, lwd = 2, col = "red")

# there are some subperiods when cointegration is found !!!!
# (ADF test statistic < critical value)

# Rolling PP Test on Residuals
rollcoint240.pp <- rollapply(US.data2[,1:2],
                             width = 240, # window width = 4 hours
                             FUN = function(x) 
                               summary(
                                 ur.pp(lm(PEP.close ~ PG.close - 1, 
                                          data = x
                                 )$residuals,
                                 type = "Z-tau", # PP test statistic
                                 model = "constant" 
                                 )
                               )@teststat[1]
                             ,  
                             by.column = FALSE,  
                             by = 60, # calculation every 60 minutes
                             align = "right")

# Handling missing values by carrying forward the last observation
rollcoint240.pp <- na.locf(rollcoint240.pp, na.rm = FALSE)

# Defining and plotting PP critical value
pp.cval <- -2.88  
chart_Series(rollcoint240.pp)
abline(h = pp.cval, lty = 2, lwd = 2, col = "blue")

# Rolling KPSS Test on Residuals
rollcoint240.kpss <- rollapply(US.data2[,1:2],
                               width = 240, # window width = 4 hours
                               FUN = function(x) 
                                 summary(
                                   ur.kpss(lm(PEP.close ~ PG.close - 1, 
                                              data = x
                                   )$residuals,
                                   type = "mu" # "mu" for level stationarity
                                   )
                                 )@teststat[1]
                               ,  
                               by.column = FALSE,  
                               by = 60, # calculation every 60 minutes
                               align = "right")

# Handling missing values by carrying forward the last observation
rollcoint240.kpss <- na.locf(rollcoint240.kpss, na.rm = FALSE)
tail(rollcoint240.kpss, 61)
# Defining and plotting KPSS critical value
kpss.cval <- 0.463  
chart_Series(rollcoint240.kpss)
abline(h = kpss.cval, lty = 2, lwd = 2, col = "green")



#############################################################
# Granger causality between returns


# X -> Y
# null hypothesis:
# X does NOT Granger cause Y
# low p-value means rejection of the hypothesis

source("grangerTest.R")

# lets test it on a rolling basis

granger.results <- grangerTest(US.data2[,3:4], 15)

str(granger.results)

# this is just a data.frame

# we need in fact column no 3 with p-values

granger.results[, 3]

# Storing results on rolling basis

rollgranger <- rollapply(US.data2[,3:4],
                         width = 390, # window width
                         FUN = function(x) 
                           grangerTest(x, 15)[, 3],
                         by.column = FALSE,  
                         by = 3*390,  
                         align = "right") 

tail(rollgranger, 100)

# We will keep only nonmissing rows

rollgranger <- na.omit(rollgranger)


head(rollgranger, 10)

# what is the order of columns?
# lets see on the example of first result 
# (based on observations 1:390)

# and rerun the first test manually 
grangerTest(US.data2[1:390, 3:4], 15)

# by comparison of the values we can learn
# what is stored in the rolling results

names(rollgranger) <- c("NtoA.p", "AtoN.p")

# lets plot both p-values
layout(matrix(1:2, 2, 1))
chart_Series(rollgranger$NtoA.p)
abline(h = 0.05, col = "red", lty = 2, lwd = 2)
chart_Series(rollgranger$AtoN.p)
abline(h = 0.05, col = "red", lty = 2, lwd = 2)
layout(matrix(1))

# causality is not so obvious in many subperiods.



