## Import data
spy = read.table("~/Rdir/fin_ek_handin1/spy.txt", header = TRUE, sep = "")


spy$logr<-log(spy$rtn+1)
spy$logr_sq <- spy$logr^2
spy$logr_ab <- abs(spy$logr)

library(zoo)
library(xts)
library('fGarch')
library('fArma')
library('forecast')



par(mfrow=c(1,2))
logr_dif <- diff(spy$logr)
plot(logr_dif)

plot(spy$logr)
plot(spy$logr_sq)
plot(spy$logr_ab)

Box.test(spy$logr, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

Box.test(spy$logr, lag = 8, type =  "Ljung")

par(mfrow=c(2,2))

acf(spy$logr, main='a')
pacf(spy$logr, main='b')

acf(spy$logr_sq, main='c')
pacf(spy$logr_sq, main='d')

acf(spy$logr_ab)
pacf(spy$logr_ab)

par(mfrow=c(1,1))

### ARMA ###

arma_11 <- armaFit(logr~arma(2,0),data = spy$logr,method = "mle", include.mean = TRUE)
summary(arma_11)
predict(arma_11)
res_arma11 <- residuals(arma_11, standardize=TRUE)
acf(res_arma11, type=c('correlation'), plot=TRUE)
Box.test(res_arma11, lag = 5, type =  "Ljung") # p-value = 0.07 so we can not reject H0. 
plot(res_arma11)

res_arma11_sq <- res_arma11^2
acf(res_arma11_sq)
pacf(res_arma11_sq, main=NON)
Box.test(res_arma11_sq, lag = 8, type =  "Ljung") # p-value = 0.07 so we can not reject H0. 
plot(res_arma11_sq)


### GARCH ###
garch_21 <- garchFit(formula = ~ arma(2,0)+garch(2, 1), data = spy$logr, include.mean = TRUE)
summary(garch_21)

res_ga21 <- residuals(garch_21, standardize= TRUE)
plot(res_ga21, type='l')
sigma.t=volatility(garch_21) # estimated volatility process sigma_t for the log returns. 
plot(sigma.t, type='l')
qqnorm(res_ga21)

summary(garch_21_fixed)
plot(garch_21)
### student t ###

garch_21_st <- garchFit(formula = ~ arma(2,0)+garch(2, 1), data = spy$logr, include.mean = TRUE,cond.dist="std")
summary(garch_21_st)
res_ga21_st <- residuals(garch_21_st, standardize= TRUE)
plot(res_ga21_st, type='l')
qqnorm(res_ga21_st)

plot(garch_21_st)
### APARCH ###
aparch_11 <- garchFit(formula = ~ arma(1,0)+aparch(1, 1), data = spy$logr, include.mean = TRUE,cond.dist = 'std')
summary(aparch_11)
plot(aparch_11)

predict(aparch_11, n.ahead = 5)


res_ga21 <- residuals(garch_21, standardize= TRUE)
plot(res_ga21, type='l')

summary(spy$logr)
