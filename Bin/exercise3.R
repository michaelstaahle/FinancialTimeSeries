library('fGarch')
library('fArma')
library('forecast')
library('dplyr')
library('xtable')
library('stargazer')
# Import data
ba1 = read.table("~/Rdir/fin_ek_handin1/ba.txt", header = TRUE, sep = "")

logr <- log(ba1$ba+1)

# Hetroscedasticity
plot(logr,type='l')

# Investigating serial correlation
acf(logr)
pacf(logr)

log(636)
Box.test(logr,7)

#  arma(1,1) model 
arma_11 <- armaFit(logr~arma(1,1), data = logr ,method = "mle", include.mean = TRUE)
par(mfrow=c(2,2))
summary(arma_11)
arm_res <- residuals(arma_11)
pow_arm_res <- arm_res^2
acf(pow_arm_res)
pacf(pow_arm_res)
Box.test(pow_arm_res,7) # Significant autocorrelation in squared residuals -> GARCH effects  is pressent

# Conditional hetroscedasticity
par(mfrow=c(1,1))
powlogr <- (logr-mean(logr))^2
plot(powlogr,type='l')
acf(powlogr)
pacf(powlogr)
Box.test(powlogr,7)

# GARCH(1,1)
garch_11 <- garchFit(formula = ~ garch(1, 1), data = logr, include.mean = TRUE)
summary(garch_11) # Low AIC and BIC, all coefficients significant on 5% level, all ljung-box tests are insignificant
plot(garch_11) # Bad qq-plot in both tails, not normal. 

# GARCH(1,1), student t
tgarch_11 <- garchFit(formula = ~garch(1, 1), data = logr, include.mean = TRUE, cond.dist = 'std')
summary(tgarch_11) # Low AIC and BIC value, all coefficients significant, all ljung-box tests are insignificant.  
plot(tgarch_11) 

#### TEST för qq-plot från plot() funktionen. qq-plotten är mot normal-kvantiler oberoende av cond.dist #####
st_res_tg <- residuals(tgarch_11)/volatility(tgarch_11)
qqnorm(st_res_tg)
qqline(st_res_tg)
#############################################################################################################



