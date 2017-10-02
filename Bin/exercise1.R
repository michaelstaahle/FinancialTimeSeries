library('Hmisc')
library('fGarch')
library('fArma')
library('forecast')

## Importing data
gbp_se = read.csv("~/Rdir/fin_ek_handin1/gbp_se.csv", header = TRUE, sep = ";")


## gbp_se
lagr <- Lag(gbp_se$rate)
logr <- log(gbp_se$rate/lagr)
logr <- logr[!is.na(logr)]
logrsq <- logr^2

## us_se
lagr <- Lag(us_se$rate)
logr <- log(us_se$rate/lagr)
logr <- logr[-1]
logrsq <- logr^2

plot(logr, type='l')


Box.test(logr, lag = 7, type =  "Ljung")

par(mfrow=c(1,1))
acf(logr)
pacf(logr)

Box.test(logrsq, lag = 7, type =  "Ljung")
acf(logrsq)
pacf(logrsq)

arma_11 <- armaFit(logr~arma(0,0),data=logr,method = "mle", include.mean = TRUE)
summary(arma_11)
res11 <- residuals(arma_11)
sqres11 <- res11^2
Box.test(res11,7)

Box.test(sqres11,7)
acf(sqres11)
pacf(sqres11)

garch_11 <- garchFit(formula = ~ garch(1, 1), data = logr, include.mean = TRUE)
summary(garch_11)

res_ga11 <- residuals(garch_11, standardize= TRUE)
plot(res_ga11, type='l')
sigma.t=volatility(garch_11) # estimated volatility process sigma_t for the log returns. 
plot(sigma.t, type='l')
qqnorm(res_ga11)

plot(garch_11)

## t inovation 
garch_21_st <- garchFit(formula = ~ garch(1, 1), data = logr, include.mean = TRUE,cond.dist="std")
summary(garch_21_st)
res_ga21_st <- residuals(garch_21_st, standardize= TRUE)
plot(res_ga21_st, type='l')
qqnorm(res_ga21_st)

plot(garch_21_st)


##  "norm", "snorm", "ged", "sged", "std", "sstd", "snig", "QMLE"

## 1,0 : -7.490943 -7.478644