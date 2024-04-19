#Removing all objects from the work space
rm(list=ls())

#Importing my dependencies
library(zoo)
library(astsa)
library(urca)
library(vars)
library(forecast)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(hrbrthemes)
library(readxl)
library(ggfortify)
library(dynlm)
library(mFilter)
library(tsModel)
library(ggsci)
library(gridExtra)
library(tseries)
library(tsm)
library(tsDyn)
library(xts)
library(xlsx)
library(tidyverse)
library(tidyquant)

#Getting rid of scientific notation
options(scipen=999)

#Setting the working directory
setwd("C:\\Users\\u1429811\\Documents\\ECON 7801\\Research_Project")

#Reading in the data
VAR <- read.xlsx("VAR.xlsx", 1)

#Changing the variable order to Lead_PPI->CB_PPI->monthly_avg_real_price for Cholesky ordering
VAR <- VAR[, c(1,3,4,2)]

#Converting to a time series
Ammo_PPI <- as.xts(VAR, date_col= 'Date')

#VAR length selection criteria
VARselect(Ammo_PPI, lag.max=12, type='const')
#VAR(3) model by AIC 

# Testing for unit roots
# ADF tests
summary(ur.df(Ammo_PPI$Lead_PPI, type='none', lags=10, selectlags='AIC'))
summary(ur.df(Ammo_PPI$CB_PPI, type='none', lags=10, selectlags='AIC'))
summary(ur.df(Ammo_PPI$monthly_real_price_per_round, type='none', lags=10, selectlags='AIC'))
# KPSS tests
summary(ur.kpss(Ammo_PPI$Lead_PPI, type='mu', lags=c('long')))
summary(ur.kpss(Ammo_PPI$CB_PPI, type='mu', lags=c('long')))
summary(ur.kpss(Ammo_PPI$monthly_real_price_per_round, type='mu', lags=c('long')))
# CONCLUSION: We can treat all of the pertinent variables integrated of order 1!

#I had to install the package here to have it work properly
install.packages('vars')
library(vars)
# Estimation
Ammo_PPI_1 <- VAR(y=Ammo_PPI, p=3, type='const')
summary(Ammo_PPI_1)
# lowest AIC at 3 lags

# Correct specification tests
# Serial correlation: Breusch-Godfrey LM test
# Null hypothesis: error variances are all equal
serial.test(Ammo_PPI_1, type='BG', lags.bg=1)
serial.test(Ammo_PPI_1, type='BG', lags.bg=2)
serial.test(Ammo_PPI_1, type='BG', lags.bg=3)
serial.test(Ammo_PPI_1, type='BG', lags.bg=4)
# There is autocorrelation in the 4th lag

# Serial correlation: Portmanteau test 
serial.test(Ammo_PPI_1, type='PT.adjusted')
# Fail to reject null of no serial correlation

# Normality: Jarque-Bera test
normality.test(Ammo_PPI_1) 
# Reject H0 of normality. The residuals have skewness and kurtosis that are 
# different than a normal distribution.

# Heteroskedasticity: ARCH test
arch.test(Ammo_PPI_1)
# Reject H0 of no ARCH effects. The residuals reflect heteroskedacisty.

# Stability of parameters in VAR model: CUSUM
win.graph()
plot(stability(Ammo_PPI_1, type='OLS-CUSUM'))
# Mean of parameters in the reduced-form equations appear constant over time

# Innovation accounting results 
# Impulse-response functions (IRFs)
# For Cholesky ordering Lead_PPI -> CB_PPI -> monthly_real_price_per_round
irf.leadshock <- irf(Ammo_PPI_1, impulse='Lead_PPI', response=c('monthly_real_price_per_round'),
                     n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)

irf.leadshock <- irf(Ammo_PPI_1, impulse='Lead_PPI', response=c('CB_PPI','Lead_PPI'),
                     n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)
win.graph()
plot(irf.leadshock)
irf.leadshock

irf.CBshock <- irf(Ammo_PPI_1, impulse='CB_PPI', response=c('monthly_real_price_per_round'), n.ahead=10, ortho=T, 
                   cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)

irf.CBshock <- irf(Ammo_PPI_1, impulse='CB_PPI', response=c('CB_PPI','Lead_PPI'),
                   n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)
win.graph()
plot(irf.CBshock)
irf.CBshock

irf.Priceshock <- irf(Ammo_PPI_1, impulse='monthly_real_price_per_round', response=c('monthly_real_price_per_round'),
                      n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)

irf.Priceshock <- irf(Ammo_PPI_1, impulse='monthly_real_price_per_round', response=c('CB_PPI','Lead_PPI'),
                      n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)
win.graph()
plot(irf.Priceshock)
irf.Priceshock

# Granger causality
causality(Ammo_PPI_1, cause='Lead_PPI') 
# H0: Lead_PPI does not Granger-cause CB_PPI or monthly_real_price_per_round. We reject H0
causality(Ammo_PPI_1, cause='CB_PPI') 
# H0: CB_PPI does not Granger-cause Lead_PPI or monthly_real_price_per_round. We reject H0
causality(Ammo_PPI_1, cause='monthly_real_price_per_round')
# H0: monthly_real_price_per_round does not Granger-cause Lead_PPI or CB_PPI. We reject H0

# Forecast Error Variance Decompositions (FEVDs) for Cholesky ordering Lead_PPI -> CB_PPI -> 
# monthly_real_price_per_round
fevd.Ammo_PPI <- fevd(Ammo_PPI_1, n.ahead=11)
# FEVDs for PPI of Lead
# A shock to the lead PPI accounts for 84% of its variation
round(fevd.Ammo_PPI$Lead_PPI, 3)
# FEVDs for PPI of Copper and Brass
# A shock to the CB PPI accounts for 42% of its variation
round(fevd.Ammo_PPI$CB_PPI, 3)
# FEVDS for monthly real price per round
#A shock to the monthly real price per round accounts for 81% of its variation
round(fevd.Ammo_PPI$monthly_real_price_per_round, 3)

#Changing the Cholesky ordering to CB_PPI -> Lead_PPI -> monthly_real_price_per_round
#Copying the original data frame
VAR_2 <- VAR
#Changing the column order
VAR_2 <- VAR_2[, c(1,3,2,4)]
#Converting to a time series
Ammo_PPI_2 <- as.xts(VAR_2, date_col= 'Date')
# Estimating the VAR model
Ammo_PPI_3 <- VAR(y=Ammo_PPI_2, p=3, type='const')
summary(Ammo_PPI_3)
# For Cholesky ordering CB_PPI -> Lead_PPI -> monthly_real_price_per_round
irf.leadshock <- irf(Ammo_PPI_3, impulse='Lead_PPI', response=c('monthly_real_price_per_round'),
                     n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)

irf.leadshock <- irf(Ammo_PPI_3, impulse='Lead_PPI', response=c('CB_PPI','Lead_PPI'),
                     n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)
win.graph()
plot(irf.leadshock)
irf.leadshock

irf.CBshock <- irf(Ammo_PPI_3, impulse='CB_PPI', response=c('monthly_real_price_per_round'), n.ahead=10, ortho=T, 
                   cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)

irf.CBshock <- irf(Ammo_PPI_3, impulse='CB_PPI', response=c('CB_PPI','Lead_PPI'),
                   n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)
win.graph()
plot(irf.CBshock)
irf.CBshock

irf.Priceshock <- irf(Ammo_PPI_3, impulse='monthly_real_price_per_round', response=c('monthly_real_price_per_round'),
                      n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)

irf.Priceshock <- irf(Ammo_PPI_3, impulse='monthly_real_price_per_round', response=c('CB_PPI','Lead_PPI'),
                      n.ahead=10, ortho=T, cumulative=F, boot=T, ci=.95, runs=2000, seed=2022)
win.graph()
plot(irf.Priceshock)
irf.Priceshock

# Forecast Error Variance Decompositions (FEVDs) for Cholesky ordering CB_PPI -> Lead_PPI -> 
# monthly_real_price_per_round
fevd.Ammo_PPI_3 <- fevd(Ammo_PPI_3, n.ahead=11)
# FEVDs for PPI of Lead
# A shock to the lead PPI accounts for 52% of its variation
round(fevd.Ammo_PPI_3$Lead_PPI, 3)  
# FEVDs for PPI of Copper and Brass
# A shock to the CB PPI accounts for 68% of its variation
round(fevd.Ammo_PPI_3$CB_PPI, 3)
# FEVDS for monthly real price per round
#A shock to the monthly real price per round accounts for 81% of its variation
round(fevd.Ammo_PPI_3$monthly_real_price_per_round, 3)





