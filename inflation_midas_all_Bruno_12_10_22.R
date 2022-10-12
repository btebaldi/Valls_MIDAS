rm(list=ls())
gc()

country = "BRAZIL"
step <- 1

# l <- 6
#
# Valls: need a loop for l = 6, 13 and 27
#
# Tebaldi: Fazendo uma lista que vai conter os valores de "l" a serem utilizados
# no processo de looping
loop_list <- c(6, 13, 27)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------- LOADING PACKAGES --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
  }
  require(x,character.only=TRUE)
}

load_package('tseries')
# load_package('xlsx') # tebaldi: Biblioteca desnecessaria (faz uso do java que
# muitas vezes precisa atualizar)
load_package('QRM')
load_package('openxlsx')
load_package('xts')
load_package('class')
load_package('zoo')
load_package('fBasics')
load_package('qrmtools')
# load_package('fDMA') # tebaldi: NAO TEM MAIS!!! (porem nao é necessaria no
# codigo)
load_package('TSA')
load_package('roll')
load_package('MTS')
load_package('forecast')
load_package('fGarch')
load_package('rugarch')
load_package('rmgarch')
load_package('imputeTS')
load_package('ggplot2')
load_package('reshape2') 
load_package('quadprog')
load_package('qrmdata')
load_package('mvtnorm')
load_package('graphics')
load_package('dplyr')
load_package('midasr')
load_package('TSP')
load_package('imputeTS')
load_package("MTS")
load_package("quantmod")
load_package("vars")
load_package("stats")
load_package("stargazer")
load_package("corrplot")
load_package("Metrics")
load_package("MLmetrics")
load_package("MCS")
load_package("tsDyn")
load_package("matrixStats")
load_package("car")
load_package("strucchange")
load_package("hrbrthemes")
load_package("tibble")
load_package("MASS")

load_package("lubridate")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#-------- IMPORTING DATA ---------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
# setwd("~/Dropbox/dissertacao_hully/artigo/IJF2022/github_hully/inflation-midas-master")
df <- read.csv("daily_data_all.csv", sep = ",")

#  Analisando os dados esse comando seria desnecessario
df[df==0] <- NA

# tebaldi: converte o campo "date para "Data"
df$date <- as.Date(df$date)

# tebaldi: Filtra a amostra para conter apenas os dados do pais selecionado
df_country = na.omit(df[df$country==country,])

# tebaldi: Funcao para fazer a adicao de meses
add.months <- function(date, n){
  ret <- seq(date, by = paste(n, "months"), length = 2)[2]
  return(ret)
}

#  leitura dos dados de mercado
market <- read.csv("market.csv", sep = ";", na.strings = "#N/A")
colnames(market) <- c("date", "BRAZIL", "ARGENTINA", "CHINA_S", "JAPAN", "UK", "GERMANY", "SOUTHAFRICA", "USA")

# tebaldi: Completa datos do mercado locf: Last Observation Carried Forward.
# 
# tebaldi: na.locf will be replaced by na_locf. (package imputeTS)
#
# market <- na.locf(market)
market <- imputeTS::na_locf(market)

# tebaldi: converte o campo "date para "Data"
market$date <- as.Date(market$date, "%d/%m/%Y")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----------- VARIABLES -----------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# tebaldi: determica series de inflacao PS e infalcao CPI
inflation_ps <- na.omit(xts(df_country$annualPS, order.by = df_country$date))
inflation_cpi <- na.omit(xts(df_country$annualCPI, order.by = df_country$date))
market_m_annual <- na.omit(xts(market[country], order.by = market$date) %>% diff.xts(lag = 12, log = TRUE)*100)

# tebaldi: Determina janela dos dados
min <- max(c(min(time(inflation_ps)), min(time(inflation_cpi))))
max <- min(c(max(time(inflation_ps)), max(time(inflation_cpi))))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------- ADJUSTING SAMPLES -------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sample_period <- paste(min+1, max-1, sep = "/")


inflation_cpi_full_d <- inflation_cpi[sample_period]
inflation_ps_full <- inflation_ps[sample_period]
market_m_annual_full <- market_m_annual[sample_period]

date_m <- time(market_m_annual_full)

if (country == "GERMANY"){
  date_m <- date_m[date_m !="2015-03-31"]
  market_m_annual_full[time(market_m_annual_full) != "2015-03-02",]
}

if (country == "UK"){
  date_m <- date_m[date_m !="2015-03-31"]
  market_m_annual_full[time(market_m_annual_full) != "2015-03-02",]
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---- HARMONIZING DAILY DATA -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# tebaldi: Funcao de ajusta a serie temporal para 28 dias. (exlui dias
# superiores a 28 dias)
adjust_to_28 <- function(series){
  tt <- time(series)
  last.date.of.month <- as.Date(as.yearmon(tt), frac = 1)
  series[ last.date.of.month - tt < 28 ]
}

inflation_ps_full <- adjust_to_28(inflation_ps_full)
inflation_cpi_full_d <- adjust_to_28(inflation_cpi_full_d)

# tebaldi: tira as medias mensais dos dados.
inflation_ps_agg_full <- matrix(inflation_ps_full, nrow=28) %>%
  colMeans(na.rm=TRUE) %>%
  cbind %>%
  xts(order.by = date_m)
inflation_cpi_full <- matrix(inflation_cpi_full_d, nrow=28) %>%
  colMeans(na.rm=TRUE) %>%
  cbind %>%
  xts(order.by = date_m)

date_d <- time(inflation_ps_full) %>% as.Date
aux <- xts(date_d, order.by = date_d)

#inflation_cpi_full_d <- na_locf(inflation_cpi_full_d)
#
# tebaldi: na.locf will be replaced by na_locf. (package imputeTS)
# 
# inflation_cpi_full_d <- na.locf(inflation_cpi_full_d)
inflation_cpi_full_d <- imputeTS::na_locf(inflation_cpi_full_d)
aux <-xts(time(inflation_ps_full), order.by = time(inflation_ps_full))
inflation_cpi_full_d <-merge(aux, inflation_cpi_full_d, all = FALSE)$inflation_cpi_full


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------- SOME PLOTS -----------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

par(mfrow=c(1,1))

# cpi vs PS - scatter
par(mfrow=c(1,1))
plot.default(coredata(inflation_ps_agg_full),coredata(inflation_cpi_full),
             xlab = 'PS Inflation (%)',
             ylab = 'IPCA Inflation (%)',
             cex.lab=1)
eq = lm(inflation_cpi_full ~ + inflation_ps_agg_full)
abline(coef(eq), col='red')


# cpi vs PS - plot
par(mfrow=c(1,1))
data <- cbind(inflation_ps_full, inflation_cpi_full_d)
plot.zoo(data, 
         plot.type = 'single',
         xlab = "Period",
         ylab = "Annual Inflation Rate (%)",
         lwd = 1,
         col=c(2,1),
         cex.lab=1,
)
legend("top", c('PS Inflation', 'IPCA Inflation'), lty = 1, col=c(2,1), nc=2, cex = 1, bty = "n")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Error in factor(year(date))
# Included by Pedro Valls
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(ggplot2)
# Define the plot

# tebaldi: alterado dados de data frame para utilizar as_tibble.
inflation_ps_full_df = as_tibble(inflation_ps_full)
inflation_ps_full_df$date = time(inflation_ps_full)
p <- inflation_ps_full_df %>% 
  mutate(
    year = factor(year(date)),     # use year to define separate curves
    date = update(date, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(x=date, y=V1, colour = year)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(n.breaks = 10) +
  labs(y='PS Inflation (%)',x='Period', colour = "Year")
# Raw daily data
p + geom_line(lwd=0.4)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------ CORRELATION MATRIX -------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aux <- cbind(inflation_cpi_full, inflation_ps_agg_full, market_m_annual_full)
colnames(aux) <- c("CPI Inflation", "PS Inflation", "Market")
cor(na.omit(aux))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------- PARAMETERS ----------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n = length(inflation_ps_agg_full)
n_test <- round(0.25*n)
# n_test = 10
# n_test = 30

# removing the last 10 observations
# inflation_cpi_full <- inflation_cpi_full[1:72]
# inflation_ps_full <- inflation_ps_full[1:2016]
# market_m_annual_full <- market_m_annual_full[1:72]
#  

n_max <- nrow(inflation_cpi_full)
n_train <- n_max - n_test

inflation_cpi_test <- inflation_cpi_full[(n_max - n_test + step):n_max]

date_m_train <- date_m[1:(n_max - n_test)]
date_m_test <- date_m[(n_max - n_test + 1):(n_max)]
date_d_train <- date_d[1:(n_max*28 - n_test*28)]
date_d_test <-date_d[((n_max*28 - n_test*28) + 1):(n_max*28 - (step - 1)*28)]

# tebaldi: Constroi uma lista com dados de inflacao 
data_train <- list(y = as.numeric(inflation_cpi_full[date_m_train]),
                   x = as.numeric(inflation_ps_full[date_d_train]),
                   z = as.numeric(market_m_annual_full[date_m_train]),
                   x_agg = as.numeric(inflation_ps_agg_full[date_m_train]),
                   trend = seq(1:nrow(inflation_cpi_full[date_m_train])))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------- IN-SAMPLE ------------

#--------------AR(1) ---
ar1 <- Arima(data_train$y, c(1, 0, 0), include.constant = TRUE)
summary(ar1)
aux_ar1_f <- fitted(ar1)

checkresiduals(ar1)
adf.test(residuals(ar1))
accuracy_ar1_in <- rmse(ts(inflation_cpi_full[1:n_train]), aux_ar1_f)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Since the best AR(p) is with p=2 
# Estimation of AR(2) was included
# Included by Pedro Valls
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#---------------AR(2)---
ar2 <- Arima(data_train$y, c(2, 0, 0), include.constant = TRUE)
summary(ar2)
aux_ar2_f <- fitted(ar2)

checkresiduals(ar2)
adf.test(residuals(ar2))
accuracy_ar2_in <- rmse(ts(inflation_cpi_full[1:n_train]), aux_ar2_f)



#-------------- ARIMA ---
arima <- Arima(data_train$y, c(1, 1, 0), include.constant = TRUE)
summary(arima)
aux_arima_f <- fitted(arima)

checkresiduals(arima)
adf.test(residuals(arima))
accuracy_arima_in <- rmse(ts(inflation_cpi_full[1:n_train]), aux_arima_f)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------------ARIMA(0,1,1) - local level-------
# Estimate a local level
# Included by Pedro Valls
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arima_nl <- Arima(data_train$y, c(0, 1, 1), include.constant = TRUE)
summary(arima_nl)
aux_arima_nl_f <- fitted(arima_nl)

checkresiduals(arima_nl)
adf.test(residuals(arima_nl))
accuracy_arima_nl_in <- rmse(ts(inflation_cpi_full[1:n_train]), aux_arima_nl_f)


#-------------- VAR 1 ---

aux <- cbind(na.omit(data_train$y),
             na.omit(data_train$x_agg),
             na.omit(data_train$z),
             na.omit(data_train$trend))
colnames(aux) <- c("y", "x", "z", "trend")
VARselect(aux)


aux_vec <- ts(cbind(na.omit(data_train$y), na.omit(data_train$x_agg)))
vec <- ca.jo(aux_vec, spec = "transitory")
summary(alrtest(vec, c(1,0), 1))
summary(vec)

var_1 <- VAR(aux, 1)
summary(var_1$varresult$y)
aux_var_f <- fitted(var_1)[,1]

par(mfrow=c(1,1))
plot(residuals(var_1)[,1], type = "l")
par(mfrow=c(1,2))
acf(residuals(var_1)[,1]); pacf(residuals(var_1)[,1])
par(mfrow=c(1,1))
adf.test(residuals(var_1)[,1])

accuracy_var_1_in <- rmse(ts(inflation_cpi_full[2:n_train]), aux_var_f)

#-------- BRIDGE EQUATION ---

# testing exogeneity
eq_exo <- lm(y ~ x_agg, data_train)
durbinWatsonTest(eq_exo)

# testing granger causality (H0: no Granger causality)
grangertest(y ~ x_agg, order = 1,  data = data_train)
grangertest(x_agg ~ y, order = 1,  data = data_train)

eqb_1 <- lm(y ~ trend + mls(y, 1, 1) + 
              mls(z, 1, 1) + 
              mls(x_agg, 0, 1), 
            data = data_train)

summary(eqb_1)
aux_eqb_1_f <- fitted(eqb_1)

checkresiduals(eqb_1)
adf.test(residuals(eqb_1))
accuracy_eqb_1_in <- rmse(inflation_cpi_full[2:n_train], aux_eqb_1_f)

# testing long-term relationships 
# linearHypothesis(eqb_1, c("mls(y, 1, 1)=1", "mls(z, 1, 1)=0", "mls(x_agg, 0, 1)=0"))

#------------------------------
# long run elasticity for ibov
#------------------------------
mRestriction_ibov <- matrix(0, ncol = 5, nrow = 2)
colnames(mRestriction_ibov) <- c("cons", "trend", "y", "z", "x")
mRestriction_ibov[1,3] <- 1
mRestriction_ibov[2,4] <- 1
#mRestriction[3,5] <- 1
linearHypothesis(eqb_1, hypothesis.matrix = mRestriction_ibov, rhs = c(1,0))


#------------------------------
# long run elasticity for PS
#------------------------------
mRestriction_ps <- matrix(0, ncol = 5, nrow = 2)
colnames(mRestriction_ps) <- c("cons", "trend", "y", "z", "x")
mRestriction_ps[1,3] <- 1
mRestriction_ps[2,5] <- 1
#mRestriction[3,5] <- 1
linearHypothesis(eqb_1, hypothesis.matrix = mRestriction_ps, rhs = c(1,0))


#------------------------------
# long run elasticity for PS and IBOV
#------------------------------
mRestriction_ibov_ps <- matrix(0, ncol = 5, nrow = 3)
colnames(mRestriction_ibov_ps) <- c("cons", "trend", "y", "z", "x")
mRestriction_ibov_ps[1,3] <- 1
mRestriction_ibov_ps[2,4] <- 1
mRestriction_ibov_ps[3,5] <- 1
linearHypothesis(eqb_1, hypothesis.matrix = mRestriction_ibov_ps, rhs = c(1,0,0))

linearHypothesis(eqb_1, c("mls(y, 1, 1)=1", "mls(z, 1, 1)=0"))


#-------- DUPLA DIFERENÇA

eqdd_1 <- lm(diff(y) ~ diff(mls(y, 1, 1)) + 
               diff(mls(z, 1, 1)) + 
               diff(mls(x_agg, 0, 1)), 
             data = data_train)

summary(eqdd_1)
# aux_eqdd_1_f <- diffinv(fitted(eqdd_1), xi=data_train$y[1])
aux_eqdd_1_f1 <- c(0,fitted(eqdd_1)) + data_train$y[1:(n_train-1)]

checkresiduals(eqdd_1)
adf.test(residuals(eqdd_1))
accuracy_eqdd_1_in <- rmse(inflation_cpi_full[2:n_train], aux_eqdd_1_f1)

# testing long-term relationships IBOV and PS
linearHypothesis(eqdd_1, c("diff(mls(y, 1, 1))=1", "diff(mls(z, 1, 1))=0", "diff(mls(x_agg, 0, 1))=0"))

#-----------------------
# long run elasticity for IBOV
#-----------------------

linearHypothesis(eqdd_1, c("diff(mls(y, 1, 1))=1", "diff(mls(z, 1, 1))=0"))

mRestriction <- matrix(0, ncol = 4, nrow = 2)
colnames(mRestriction) <- c("cons", "y", "z", "x")
mRestriction[1,2] <- 1
mRestriction[2,3] <- 1
#mRestriction[3,4] <- 1
linearHypothesis(eqdd_1, mRestriction, c(1, 0))

#-----------------------
# long run elasticity for PS
#-----------------------
mRestriction <- matrix(0, ncol = 4, nrow = 2)
colnames(mRestriction) <- c("cons", "y", "z", "x")
mRestriction[1,2] <- 1
#mRestriction[2,3] <- 1
mRestriction[2,4] <- 1
linearHypothesis(eqdd_1, mRestriction, c(1, 0))


# ---- l = 6, 13, 27
#-------- MIDAS-DL ---

# tebaldi: MIDAS lag structure - Create a matrix of selected MIDAS lags
# 
# l=27 # este valor estava fixo no codigo. comentado para trabalharmos com l = 6, 13, 27
# 
# mls(data_train$x, 0:l, 28) # Comando pode ser desligado
# data_train2 <- data_train # data_train2 nunca é utilizado

# tebaldi: para evitar impactos muito grande no codigo vamos fazer um looping
# para cada modelo. Acrescentados pontos de impressao cat(...) que informam o
# que esta sendo executado no looping.




for(l in loop_list){
  
  cat(sprintf("Executando MIDAS-DL, com l=%d\n", l))
  
  cat(sprintf("\nExecutando y ~ trend + mls(z, 1, 1) + mls(x, 0:%d, 28)\n", l))
  eqm_u <- lm(y ~ trend +
                mls(z, 1, 1) +
                mls(x, 0:l, 28),
              data = data_train)
  
  summary(eqm_u)
  aux_eqm_u_f <- fitted(eqm_u)
  
  # tebaldi: se l=27 precisamos reduzir o lag maximo para analise para 30.
  # (amostra nao é grande o suficiente para o lag padrao)
  if(l == 27){
    checkresiduals(object = eqm_u, lag = 30)
  } else {
    checkresiduals(eqm_u)
  }
  
  adf.test(residuals(eqm_u))
  accuracy_eqm_u_in <- rmse(inflation_cpi_full[2:n_train], aux_eqm_u_f)
  
  #---------
  # long run for IBOV and PS
  #---------
  cat(sprintf("long run for IBOV and PS\n"))
  mRestriction <- matrix(0, ncol = (l+4), nrow = 2)
  colnames(mRestriction) <- c("cons", "trend", "z", paste("x", 0:l, sep = "_"))
  mRestriction[1,3] <- 1
  mRestriction[2,4:(l+4)] <- 1
  
  #linearHypothesis(eqm_u, c("z=0", "x1 + x2 + x3 + x4 + x5 + x6 + x7 = 0"))
  linearHypothesis(eqm_u, mRestriction, c(0,0))
  
  
  #---------
  # long run for IBOV 
  #---------
  cat(sprintf("long run for IBOV\n"))
  mRestriction <- matrix(0, ncol = (l+4), nrow = 1)
  colnames(mRestriction) <- c("cons", "trend", "z", paste("x", 0:l, sep = "_"))
  mRestriction[1,3] <- 1
  #mRestriction[2,4:(l+4)] <- 1
  linearHypothesis(eqm_u, mRestriction, c(0))
  
  
  #---------
  # long run for PS 
  #---------
  cat(sprintf("long run for PS\n"))
  mRestriction <- matrix(0, ncol = (l+4), nrow = 1)
  colnames(mRestriction) <- c("cons", "trend", "z", paste("x", 0:l, sep = "_"))
  #mRestriction[1,3] <- 1
  mRestriction[1,4:(l+4)] <- 1
  linearHypothesis(eqm_u, mRestriction, c(0))
  
  
  
  #-------- MIDAS-AR(1) ---
  
  cat(sprintf("\nExecutando y ~ trend + mls(y, 1, 1) + mls(z, 1, 1) + mls(x, 0:%d, 28)\n", l))
  eqm_ar1 <- lm(y ~ trend +
                  mls(y, 1, 1) +
                  mls(z, 1, 1) +
                  mls(x, 0:l, 28),
                data = data_train)
  
  summary(eqm_ar1)
  aux_eqm_ar1_f <- fitted(eqm_ar1)
  
  # tebaldi: se l=27 precisamos reduzir o lag maximo para analise para 29.
  # (amostra nao é grande o suficiente para o lag padrao)
  # checkresiduals(eqm_ar1)
  if(l == 27){
    checkresiduals(object = eqm_ar1, lag = 29)
  } else {
    checkresiduals(eqm_ar1)
  }
  adf.test(residuals(eqm_ar1))
  accuracy_eqm_ar1_in <- rmse(inflation_cpi_full[2:n_train], aux_eqm_ar1_f)
  
  #---------
  # long run for IBOV and PS
  #---------
  cat(sprintf("long run for IBOV and PS\n"))
  mRestriction <- matrix(0, ncol = (l+5), nrow = 3)
  mRestriction
  colnames(mRestriction) <- c("cons", "trend", "y", "z", paste("x", 0:l, sep = "_"))
  mRestriction[1,3] <- 1
  mRestriction[2,4] <- 1
  mRestriction[3, 5:(l+5)] <- 1
  mRestriction
  #linearHypothesis(eqm_ar1, c("mls(y,1,1)=1", "mls(z,1,1)=0", "mls(x,0,28)+mls(x,1,28)+mls(x,2,28)+mls(x,3,28) +mls(x,4,28)+ +mls(x,5,28)+mls(x,6,28)= 0"))
  linearHypothesis(eqm_ar1, mRestriction, c(1, 0, 0))
  
  #---------
  # long run for IBOV 
  #---------
  cat(sprintf("long run for IBOV\n"))
  mRestriction <- matrix(0, ncol = (l+5), nrow = 2)
  
  colnames(mRestriction) <- c("cons", "trend", "y", "z", paste("x", 0:l, sep = "_"))
  mRestriction[1,3] <- 1
  mRestriction[2,4] <- 1
  #mRestriction[3, 5:(l+5)] <- 1
  
  # linearHypothesis(eqm_ar1, c("y=1", "z=0", "x1 + x2 + x3 + x4 + x5 + x6 + x7 = 0"))
  linearHypothesis(eqm_ar1, mRestriction, c(1, 0))
  
  
  
  #---------
  # long run for  PS
  #---------
  cat(sprintf("long run for PS\n"))
  mRestriction <- matrix(0, ncol = (l+5), nrow = 2)
  
  colnames(mRestriction) <- c("cons", "trend", "y", "z", paste("x", 0:l, sep = "_"))
  mRestriction[1,3] <- 1
  #mRestriction[2,4] <- 1
  mRestriction[2, 5:(l+5)] <- 1
  
  # linearHypothesis(eqm_ar1, c("y=1", "z=0", "x1 + x2 + x3 + x4 + x5 + x6 + x7 = 0"))
  linearHypothesis(eqm_ar1, mRestriction, c(1, 0))
  
  #-------- MIDAS-AR(1)-R ---
  
  cat(sprintf("\nExecutando y ~ trend + mls(z, 1, 1) + mls(z, 1, 1) + mls(x, 0:%d, 28, nealmon)\n", l))
  eqm_ar1r <- midas_r(y ~ trend +
                        mls(y, 1, 1) +
                        mls(z, 1, 1) +
                        mls(x, 0:l, 28, nealmon),
                      data = data_train, start = list(x = c(-0.1,0.1)))
  
  summary(eqm_ar1r)
  aux_eqm_ar1r_f <- fitted(eqm_ar1r)
  
  
  # tebaldi: Warning message:  In modeldf.default(object) : Could not find
  # appropriate degrees of freedom for this model
  #
  # Razao: o modelo nao é linear (classe "lm"), é um modelo da classe "midas_r".
  # A solução do modelo é feita por otimizacao numerica.
  #
  # Solucao: fazer o teste de Breusch-Godfrey manualmente
  
  checkresiduals(eqm_ar1r)
  
  # tebaldi: Processo de calculo do lag retirado da funcao forecast::checkresiduals
  lag <- max(length(eqm_ar1$coefficients)+3, min(10, round(length(eqm_ar1$residuals)/5)))
  if(l == 27){
    lag <- min(lag, 29)
  } 
  
  
  # tebaldi: Esse processo é bem ineficiente para varios lags, mas para os lags
  # 6, 13 e 27 funciona. Devido ao tempo seguiremos com essa solucao.
  aux_tbl <- as_tibble(eqm_ar1r$model)
  if(l == 6){
    mcols <- c("Intercept", "trend", "y_1", "z_1", "X0", "X1", "X2", "X3", "X4", "X5", "X6")
    colnames(aux_tbl) <- c("y", mcols)
  } else if(l == 13) {
    mcols <- c("Intercept", "trend", "y_1", "z_1", "X0", "X1", "X2", "X3", "X4", "X5", "X6",
               "X7", "X8", "X9", "X10", "X11", "X12", "X13")
    colnames(aux_tbl) <- c("y", mcols)
  } else if(l == 27)  {
    mcols <- c("Intercept", "trend", "y_1", "z_1", "X0", "X1", "X2", "X3", "X4", "X5", "X6",
               "X7", "X8", "X9", "X10", "X11", "X12", "X13",
               "X14", "X15", "X16", "X17", "X18", "X19", "X20",
               "X21", "X22", "X23", "X24", "X25", "X26", "X27")
    colnames(aux_tbl) <- c("y", mcols)
  } else {
    stop("NAO PREVISTO")
  }
  
  # tebaldi: se l=27 precisamos reduzir o lag maximo para analise para 30.
  # (amostra nao é grande o suficiente para o lag padrao)
  lmtest::bgtest(formula = formula(paste(c("y ~ -1", mcols), collapse = " + ")),
                 order = lag, data = aux_tbl)
  
  
  # tebaldi: remove as variaveis criadas
  rm(list = c("lag", "aux_tbl", "mcols"))
  
  adf.test(residuals(eqm_ar1r)) 
  #accuracy_eqm_ar1r_in <- cbind(rmse(inflation_cpi_full[2:n_train], aux_eqm_ar1r_f))
  accuracy_eqm_ar1r_in <- rmse(inflation_cpi_full[2:n_train], aux_eqm_ar1r_f)
  
  #---------
  # long run for IBOV and PS
  #---------
  cat(sprintf("long run for IBOV and PS\n"))
  linearHypothesis(eqm_ar1r, c("y=1", "z=0", "x1 + x2 = 0"))
  
  
  #---------
  # long run for IBOV
  #---------
  cat(sprintf("long run for IBOV\n"))
  linearHypothesis(eqm_ar1r, c("y=1", "z=0"))
  
  #---------
  # long run for  PS
  #---------
  cat(sprintf("long run for PS\n"))
  linearHypothesis(eqm_ar1r, c("y=1", "x1 + x2 = 0"))
  
  
  
  
  
  #-------- MIDAS-DL non-parametric---
  
  cat(sprintf("\nExecutando MIDAS-DL non-parametric com l=%d\n", l))
  
  eqm_np <- midas_r_np(y ~ trend +
                         mls(x, 0:l, 28),
                       data = data_train, lambda = NULL)
  
  summary(eqm_np)
  aux_eqm_np_f <- fitted(eqm_np)
  aux_eqm_np_f
  
  
  # tebaldi: Warning message:  In modeldf.default(object) : Could not find
  # appropriate degrees of freedom for this model
  #
  # Razao: o modelo nao é linear (classe "lm"), é um modelo da classe "midas_r".
  # A solução do modelo é feita por otimizacao numerica.
  #
  # Solucao: fazer o teste de Breusch-Godfrey manualmente, porem nao sei se é um
  # processo valido para um modelo nao parametrico.
  
  checkresiduals(eqm_np)
  
  adf.test(residuals(eqm_np)) 
  accuracy_eqm_np_in <- rmse(inflation_cpi_full[1:n_train], aux_eqm_np_f)
  
  
  coefs = eqm_np$coefficients
  coefs
  
  r = sum(coefs[3:(3+l)])/(l*sd(data_train$x)/length(data_train$x))
  r
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # need to include AR(2) and ARIMA(0,1,1)
  # Pedro Valls
  # STOP HERE
  # Tebaldi: modelos já incluidos
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # tebaldi: Cria um vetor com o RMSE de cada modelo
  accuracy_in <- rbind(accuracy_ar1_in,
                       accuracy_ar2_in,
                       accuracy_arima_in,
                       accuracy_arima_nl_in,
                       accuracy_var_1_in,
                       accuracy_eqb_1_in,
                       accuracy_eqdd_1_in,
                       accuracy_eqm_u_in,
                       accuracy_eqm_ar1_in,
                       accuracy_eqm_ar1r_in,
                       accuracy_eqm_np_in)
  
  # tebldi: Coloca nome nas colunas e nas linhas
  colnames(accuracy_in) <- c("RMSE")
  rownames(accuracy_in) <- c("AR(1)", "AR(2)", "ARIMA(1,1,0)",
                             "ARIMA(0,1,1)-LocalLevel", "VAR(1)", "Bridge-Equation",
                             "Double-Difference", "MIDAS-DL", "MIDAS-ADL",
                             "MIDAS-ADLr", "MIDAS-DLnp")
  
  # Tebaldi: Grava na memoria os resultados
  nome_da_var <- paste("accuracy_in", l, sep = "_")
  cat(sprintf("\n\nGravando variavel (%s) com resultados do lag: %d\n", nome_da_var, l))
  assign(x = nome_da_var, value = accuracy_in )
  
  cat(sprintf("Fim do processo para o lag: %d\n", l))
  print(accuracy_in)

} #tebaldi:  END: for(l in loop_list)

# stargazer(eqm_u, eqm_ar1, eqm_ar1, eqm_ar1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#--- FORECASTING NAIVE MODELS ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------------------------------
# need a loop for l = 6, 13 and 27
#---------------------------------

for(step in 3:1){
  
  cat("Current Step ahead:", step, "\n")
  
  ar1_f <- c()
  ar2_f <- c()
  arima_f <- c()
  arima_nl_f <- c()
  var_1_f <- c()
  eqb_1_f <- c()
  eqdd_1_f <- c()
  eqm_u_f <- c()
  eqm_ar1r_f <- c()
  eqm_np_f <- c()
  
  
  aux_var_1_rmse <- c()
  resids_pvalues_var_1 <- c()
  
  options(warn = -1)
  
  for (h in 0:(n_test - step)) {
    for (i in 1:28) {
      
      # tebaldi: TT=Index de inicio do periodo de previsao?
      # cat("TT:", TT, "i:", i, "\n")
      TT = n_max - n_test + h
      
      # aux_4= medias mensal de inflacao_PS / aux_44 : versao ts
      aux_4 <- inflation_ps_full[1:(TT * 28)] %>% coredata %>% cbind %>%
        matrix(nrow = 28) %>%
        colMeans(na.rm = TRUE)
      aux_44 <- xts(aux_4, order.by = date_m[1:TT])
      
      
      data_train <- list(
        y = as.numeric(inflation_cpi_full[1:TT]),
        x = as.numeric(aux_44),
        z = as.numeric(market_m_annual_full[1:TT]),
        trend = seq(1:TT)
      )
      
      data_train_diff <- list(
        y = as.numeric(diff(inflation_cpi_full[1:TT])),
        x = as.numeric(diff(aux_44)),
        z = as.numeric(diff(market_m_annual_full[1:TT])),
        trend = seq(1:TT)
      )
      
      # tebaldi: estima um ARIMA(1, 1, 0) na amostra util (note que temos
      # rolling window)
      aux_1 <- Arima(inflation_ps_full[1:(TT * 28 + i)], order = c(1, 1, 0))
      
      if (28 - i + 28 * (step - 1) != 0) {
        aux_2 <- predict(aux_1, (28 - i + 28 * (step - 1)))$pred %>%
          xts(order.by = date_d[(TT * 28 + i + 1):((TT + 1) * 28 + 28 * (step - 1))])
      } else {
        aux_2 <- c()
      }
      
      aux_3 <- rbind(inflation_ps_full[(TT * 28 + 1):(TT * 28 + i)], aux_2)
      aux_3_diff <- diff(aux_3)
      aux_4 <- aux_3 %>% coredata %>% cbind %>% matrix(nrow = 28) %>% colMeans(na.rm = TRUE)
      aux_4_diff <- aux_3_diff %>% coredata %>% cbind %>% matrix(nrow = 28) %>% colMeans(na.rm = TRUE)
      
      
      data_test <- list(
        y = as.numeric(rep(NA, step)),
        x = as.numeric(aux_4),
        z = as.numeric(rep(NA, step)),
        trend = seq(1:step)
      )
      
      data_test_diff <- list(
        y = as.numeric(rep(NA, step)),
        x = as.numeric(aux_4_diff),
        z = as.numeric(rep(NA, step)),
        trend = seq(1:step)
      )
      
      
      #------------- AR(1) -------------
      
      ar1 <- Arima(data_train$y, c(1, 0, 0), include.constant = TRUE)
      aux_ar1_f <- forecast(ar1, h = step)$mean[step]
      ar1_f <- rbind(ar1_f, aux_ar1_f)
      
      # tebaldi: guarda dados do AR(1) para o step atual
      assign(sprintf("ar1_f_s%d", step), ar1_f)
      

      #------------- AR(2) -------------
      
      ar2 <- Arima(data_train$y, c(2, 0, 0), include.constant = TRUE)
      aux_ar2_f <- forecast(ar2, h = step)$mean[step]
      ar2_f <- rbind(ar2_f, aux_ar2_f)
      
      # tebaldi: guarda dados do AR(2) para o step atual
      assign(sprintf("ar2_f_s%d", step), ar2_f)
      
      
      #------------- ARIMA(1,1,0) -------------
      
      arima <- Arima(data_train$y, c(1, 1, 0), include.constant = TRUE)
      aux_arima_f <- forecast(arima, h = step)$mean[step]
      arima_f <- rbind(arima_f, aux_arima_f)
      
      # tebaldi: guarda dados do ARIMA(1,1,0) para o step atual
      assign(sprintf("arima_f_s%d", step), arima_f)
      
      #------------- ARIMA(0,1,1) -------------
      
      arima_nl <- Arima(data_train$y, c(0, 1, 1), include.constant = TRUE)
      aux_arima_nl_f <- forecast(arima_nl, h = step)$mean[step]
      arima_nl_f <- rbind(arima_nl_f, aux_arima_nl_f)
      
      # tebaldi: guarda dados do ARIMA(1,1,0) para o step atual
      assign(sprintf("arima_nl_f_s%d", step), arima_nl_f)
      
      #-------------- VAR(1) --------------
      
      aux <- cbind(
        na.omit(data_train$y),
        na.omit(data_train$x),
        na.omit(data_train$z),
        na.omit(data_train$trend)
      )
      
      colnames(aux) <- c("y", "x", "z", "trend")
      
      # tebaldi: ATENCAO O VAR CONTEM UMA VARIAVEL ENDOGENA QUE É A "TREND"
      var_1 <- VAR(aux, p = 1)
      aux_var_f <- predict(var_1, n.ahead = step)$fcst$y[step, 1]
      var_1_f <- rbind(var_1_f, aux_var_f)
      
      # tebaldi: guarda dados do VAR para o step atual
      assign(sprintf("var_1_f_s%d", step), var_1_f)
      
      
      aux_var_1_rmse <- rbind(aux_var_1_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], var_1_f))
      assign(paste("aux_var_1_rmse", step, sep = "_"), aux_var_1_rmse)
      
      residuals = xts(residuals(var_1), order.by=date_d_train[2:TT])$y
      aux <- cbind(adf.test(residuals)$p.value, jarque.bera.test(residuals)$p.value, durbinWatsonTest(c(coredata(residuals))))
      resids_pvalues_var_1 <- rbind(resids_pvalues_var_1, aux)
      assign(paste("resids_pvalues_var_1", step, sep = "_"), resids_pvalues_var_1)
      
      #-------- BRIDGE EQUATION --------
      eqb_1 <- midas_r(y ~ trend + 
                         mls(y, step, 1) +
                         mls(z, step, 1) +
                         mls(x, step-1, 1),
                       data = data_train,
                       start = NULL
      )
      
      aux_eqb_1_f <- forecast(eqb_1, newdata = data_test)$mean[step]
      eqb_1_f <- rbind(eqb_1_f, aux_eqb_1_f)
      
      # tebaldi: guarda dados da BRIDGE EQUATION para o step atual
      assign(sprintf("eqb_1_f_s%d", step), eqb_1_f)
      
      
      #--------- DOUBLE DIFFERENCE --------
      eqdd_1 <- midas_r(y ~  
                          mls(y, step, 1) +
                          mls(z, step:(step+1), 1) +
                          mls(x, step-1, 1),
                        data = data_train_diff,
                        start = NULL
      )
      
      aux_eqdd_1_f <- forecast(eqdd_1, newdata=data_test_diff)$mean
      aux_eqdd_1_f1 <- sum(aux_eqdd_1_f) + data_train$y[TT]
      eqdd_1_f <- rbind(eqdd_1_f, aux_eqdd_1_f1)
      
      # tebaldi: guarda dados da DOUBLE EQUATION para o step atual
      assign(sprintf("eqdd_1_f_s%d", step), eqdd_1_f)
    }
  }

  
} # fim for step in 3:1

aux_var_1_rmse_1 = xts(aux_var_1_rmse_1, order.by = date_d_test)
aux_var_1_rmse_2 = xts(aux_var_1_rmse_2, order.by = date_d_test[-c(1:28)])
aux_var_1_rmse_3 = xts(aux_var_1_rmse_3, order.by = date_d_test[-c(1:56)])


options(warn = 1)

# AVERAGE FORECAST

# tebaldi: AVERAGE FORECAST para cada STEP (a principio os forecast NAIVE nao
# dependem de l)
ar1_f_avg_s1 <- colMeans(matrix(ar1_f_s1, 28), na.rm = TRUE)
ar2_f_avg_s1 <- colMeans(matrix(ar2_f_s1, 28), na.rm = TRUE)
arima_f_avg_s1 <- colMeans(matrix(arima_f_s1, 28), na.rm = TRUE)
arima_nl_f_avg_s1 <- colMeans(matrix(arima_nl_f_s1, 28), na.rm = TRUE)
var_1_f_avg_s1 <- colMeans(matrix(var_1_f_s1, 28), na.rm = TRUE)
eqb_1_f_avg_s1 <- colMeans(matrix(eqb_1_f_s1, 28), na.rm = TRUE)
eqdd_1_f_avg_s1 <- colMeans(matrix(eqdd_1_f_s1, 28), na.rm = TRUE)
# eqdd_1_f_avg = diffinv(colMeans(matrix(eqdd_1_f, 28), na.rm = TRUE), xi=data_train$y[n-n_test])[-1]


ar1_f_avg_s2 <- colMeans(matrix(ar1_f_s2, 28), na.rm = TRUE)
ar2_f_avg_s2 <- colMeans(matrix(ar2_f_s2, 28), na.rm = TRUE)
arima_f_avg_s2 <- colMeans(matrix(arima_f_s2, 28), na.rm = TRUE)
arima_nl_f_avg_s2 <- colMeans(matrix(arima_nl_f_s2, 28), na.rm = TRUE)
var_1_f_avg_s2 <- colMeans(matrix(var_1_f_s2, 28), na.rm = TRUE)
eqb_1_f_avg_s2 <- colMeans(matrix(eqb_1_f_s2, 28), na.rm = TRUE)
eqdd_1_f_avg_s2 <- colMeans(matrix(eqdd_1_f_s2, 28), na.rm = TRUE)


ar1_f_avg_s3 <- colMeans(matrix(ar1_f_s3, 28), na.rm = TRUE)
ar2_f_avg_s3 <- colMeans(matrix(ar2_f_s3, 28), na.rm = TRUE)
arima_f_avg_s3 <- colMeans(matrix(arima_f_s3, 28), na.rm = TRUE)
arima_nl_f_avg_s3 <- colMeans(matrix(arima_nl_f_s3, 28), na.rm = TRUE)
var_1_f_avg_s3 <- colMeans(matrix(var_1_f_s3, 28), na.rm = TRUE)
eqb_1_f_avg_s3 <- colMeans(matrix(eqb_1_f_s3, 28), na.rm = TRUE)
eqdd_1_f_avg_s3 <- colMeans(matrix(eqdd_1_f_s3, 28), na.rm = TRUE)




accuracy_ar1 <- c(rmse(inflation_cpi_test, ar1_f_avg))
loss_ar1 <- LossLevel(inflation_cpi_test, ar1_f_avg)

accuracy_ar2 <- c(rmse(inflation_cpi_test, ar2_f_avg))
loss_ar2 <- LossLevel(inflation_cpi_test, ar2_f_avg)

accuracy_arima <- c(rmse(inflation_cpi_test, arima_f_avg))
loss_arima <- LossLevel(inflation_cpi_test, arima_f_avg)

#--------------
# using only the 20 obs 
# dont know why is generating 21
#---------------------

# tebaldi:  No meu ensaio veio com 20 obs normal.
accuracy_arima_nl <- c(rmse(inflation_cpi_test, arima_nl_f_avg[1:20]))
loss_arima_nl <- LossLevel(inflation_cpi_test, arima_nl_f_avg[1:20])


accuracy_var_1 <- c(rmse(inflation_cpi_test, var_1_f_avg))
loss_var_1 <- LossLevel(inflation_cpi_test, var_1_f_avg)

accuracy_eqb_1 <- c(rmse(inflation_cpi_test, eqb_1_f_avg))
loss_eqb_1 <- LossLevel(inflation_cpi_test, eqb_1_f_avg)

accuracy_eqdd_1 <- c(rmse(inflation_cpi_test, eqdd_1_f_avg))
loss_eqdd_1 <- LossLevel(inflation_cpi_test, eqdd_1_f_avg)

#---------------------------------------------------------------
# accuracy_naive_step_1l6
# step=1 and l=6
# but step =3 and 2 are not saved
# I try to include lines 804 up to 856 inside the loop for step
# but it did not work
#--------------------------------------------------------------

# tebaldi: Cria um vetor com o RMSFE de cada modelo
accuracy_in <- rbind(accuracy_ar1,
                     accuracy_ar2,
                     accuracy_arima, 
                     accuracy_arima_nl,
                     accuracy_var_1, 
                     accuracy_eqb_1,
                     accuracy_eqdd_1)

# tebldi: Coloca nome nas colunas e nas linhas
colnames(accuracy_in) <- c("RMSFE")

rownames(accuracy_in) <- c("AR(1)",
                           "AR(2)",
                           "ARIMA(1,1,0)",
                           "ARIMA(0,1,1)-local_level",
                           "VAR(1)",
                           "Bridge Equation", 
                           "Double Difference")

# Tebaldi: Grava na memoria os resultados
nome_da_var <- paste("accuracy_naive", paste(step, l, sep="l"), sep = "_")
cat(sprintf("\n\nGravando variavel (%s) com resultados do lag: %d\n", nome_da_var, l))
assign(x = nome_da_var, value = accuracy_in )

print(accuracy_in)


# MOST RECENT FORECAST
ar1_f_last <- matrix(ar1_f, 28)[28, ]
arima_f_last <- matrix(arima_f, 28)[28, ]
var_1_f_last <- matrix(var_1_f, 28)[28, ]
eqb_1_f_last <- matrix(eqb_1_f, 28)[28, ]

accuracy_ar1 <- rmse(inflation_cpi_test, ar1_f_last)
accuracy_arima <- rmse(inflation_cpi_test, arima_f_last)
accuracy_var_1 <-rmse(inflation_cpi_test, var_1_f_last)
accuracy_eqb_1 <- rmse(inflation_cpi_test, eqb_1_f_last)

assign(paste("accuracy_naive_point", step, sep = "_"), 
       matrix(rbind(accuracy_ar1, accuracy_arima, accuracy_var_1, accuracy_eqb_1), ncol = 1, nrow = 4,
              dimnames = list(c("AR(1)", "ARIMA", "VAR(1)", "Bridge Equation"), c("RMSFE"))
       )
)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------ FORECASTING MIDAS --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#-------------------------------------------
# need top include a loop for l =6, 13 ,27
#-------------------------------------------

for(step in 3:1){
  
  cat("Current Step", step, "\n")
  
  eqm_u_f <- c()
  eqm_ar1_f <- c()
  eqm_ar1r_f <- c()
  eqm_np_f <- c()
  
  aux_eqm_u_rmse <- c()
  aux_eqm_ar1_rmse <- c()
  aux_eqm_ar1r_rmse <- c()
  aux_eqm_np_rmse <- c()
  
  resids_pvalues_ar1 <- c()
  resids_pvalues_ar1r <-c()
  
  options(warn = -1)
  
  for (h in 0:(n_test - step)) {
    for (i in 1:28) {
      
      cat(sprintf(">h:%d\ti:%d\n", h,i))
      
      TT = n_max - n_test + h
      
      data_train <- list(
        y = as.numeric(inflation_cpi_full[1:TT]),
        x = as.numeric(inflation_ps_full[1:(TT * 28)]),
        z = as.numeric(market_m_annual_full[1:TT]),
        trend = seq(1:TT)
      )
      
      
      aux_1 <- Arima(inflation_ps_full[1:(TT * 28 + i)], order = c(1, 1, 0), include.constant = TRUE)
      
      if (28 - i + 28 * (step-1) != 0) { 
        aux_2 <- xts(forecast(aux_1, h = (28-i+28*(step-1)))$mean,
                     order.by = date_d[(TT*28+i+1):((TT+1)*28+28*(step-1))])
      } else {
        aux_2 <- c()
      }
      
      aux_3 <- rbind(inflation_ps_full[(TT*28+1):(TT*28+i)], aux_2)
      
      data_test <- list(
        y = as.numeric(rep(NA, step)),
        x = as.numeric(aux_3),
        z = as.numeric(rep(NA, step)),
        trend = seq(1:step)
      )
      
      
      
      #-------- MIDAS-DL --------
      
      eqm_u <- midas_r(y ~ trend +
                         mls(z, step, 1) +
                         mls(x, 0:l, 28),
                       data = data_train,
                       start = NULL)
      
      aux_eqm_u_f <- forecast(eqm_u, newdata = data_test)$mean[step]
      eqm_u_f <- rbind(eqm_u_f, aux_eqm_u_f)
      
      aux_eqm_u_rmse <- rbind(aux_eqm_u_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], eqm_u_f))
      assign(paste("aux_eqm_u_rmse", step, sep = "_"), aux_eqm_ar1_rmse)
      
      
      
      #-------- MIDAS-AR(1) --------
      #l=13
      eqm_ar1 <- midas_r(y ~ trend +
                           mls(y, step, 1) +
                           mls(z, step, 1) +
                           mls(x, 0:l, 28),
                         data = data_train,
                         start = NULL)
      
      aux_eqm_ar1_f <- forecast(eqm_ar1, newdata = data_test)$mean[step]
      eqm_ar1_f <- rbind(eqm_ar1_f, aux_eqm_ar1_f)
      
      aux_eqm_ar1_rmse <- rbind(aux_eqm_ar1_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], eqm_ar1_f))
      assign(paste("aux_eqm_ar1_rmse", step, sep = "_"), aux_eqm_ar1_rmse)
      
      aux <- cbind(adf.test(eqm_ar1$residuals)$p.value, jarque.bera.test(eqm_ar1$residuals)$p.value, durbinWatsonTest(eqm_ar1$residuals))
      resids_pvalues_ar1 <- rbind(resids_pvalues_ar1, aux)
      assign(paste("resids_pvalues_ar1", step, sep = "_"), resids_pvalues_ar1)
      
      #-------- MIDAS-AR(1)-R --------
      #l=6
      eqm_ar1r <- midas_r(
        y ~ trend +
          mls(y, step, 1) +
          mls(z, step, 1) +
          mls(x, 0:l, 28, nealmon),
        data = data_train,
        start = list(x = c(0, 0))
      )
      
      aux_eqm_ar1r_f <- forecast(eqm_ar1r, newdata = data_test)$mean[step]
      eqm_ar1r_f <- rbind(eqm_ar1r_f, aux_eqm_ar1r_f)
      
      aux_eqm_ar1r_rmse <- rbind(aux_eqm_ar1r_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], eqm_ar1r_f))
      assign(paste("aux_eqm_ar1r_rmse", step, sep = "_"), aux_eqm_ar1r_rmse)
      
      aux <- cbind(adf.test(eqm_ar1r$residuals)$p.value, jarque.bera.test(eqm_ar1r$residuals)$p.value, durbinWatsonTest(eqm_ar1r$residuals))
      resids_pvalues_ar1r <- rbind(resids_pvalues_ar1r, aux)
      assign(paste("resids_pvalues_ar1r", step, sep = "_"), resids_pvalues_ar1r)
      
      
      #-------- MIDAS-DL non-parametric--------
      
      eqm_np <- midas_r_np(y ~ trend +
                             mls(x, 0:l, 28),
                           data = data_train,
                           lambda = NULL)
      
      aux_eqm_np_f <- forecast(eqm_np, newdata = data_test)$mean[step]
      eqm_np_f <- rbind(eqm_np_f, aux_eqm_np_f)
      
      aux_eqm_np_rmse <- rbind(aux_eqm_np_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], aux_eqm_np_f))
      assign(paste("aux_eqm_np_rmse", step, sep = "_"), aux_eqm_ar1r_rmse)
      
      
    }
  }
  
} # fim de step in 3:1

aux_eqm_ar1_rmse_1 = xts(aux_eqm_ar1_rmse_1, order.by = date_d_test)
aux_eqm_ar1r_rmse_1 = xts(aux_eqm_ar1r_rmse_1, order.by = date_d_test)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# aux_eqm_ar1_rmse_2 not found
# included by Pedro Valls
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aux_eqm_ar1_rmse_2 = xts(aux_eqm_ar1_rmse_2, order.by = date_d_test[-c(1:28)])
aux_eqm_ar1r_rmse_2 = xts(aux_eqm_ar1r_rmse_2, order.by = date_d_test[-c(1:28)])

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# aux_eqm_ar1_rmse_3 not found
# included by Pedro Valls
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


aux_eqm_ar1_rmse_3 = xts(aux_eqm_ar1_rmse_3, order.by = date_d_test[-c(1:56)])
aux_eqm_ar1r_rmse_3 = xts(aux_eqm_ar1r_rmse_3, order.by = date_d_test[-c(1:56)])



options(warn = 1)

# testing for heteroskedasticity
# ar1_resdid_eqm_ar1 <- lm(resids_eqm_ar1[[2]]~ c(NA,resids_eqm_ar1[[2]][-1]))
# bptest(ar1_resdid_eqm_ar1)

#---------------------------------------------------------------------------------
# lines 1052 to 1080 save the results of the MIDAS but only for the last step
# need to include inside the loop for step =3,2,1 and also anotger loop = 6,13,27
#---------------------------------------------------------------------------------


# AVERAGE FORECAST

eqm_u_f_avg <- colMeans(matrix(as.numeric(eqm_u_f), 28), na.rm = TRUE)
eqm_ar1_f_avg <- colMeans(matrix(as.numeric(eqm_ar1_f), 28), na.rm = TRUE)
eqm_ar1r_f_avg <- colMeans(matrix(as.numeric(eqm_ar1r_f), 28), na.rm = TRUE)
eqm_np_f_avg <- colMeans(matrix(as.numeric(eqm_np_f), 28), na.rm = TRUE)


accuracy_eqm_u <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_u_f_avg)
loss_eqm_u <- LossLevel(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_u_f_avg)
assign(paste("loss_eqm_u", l, sep = "_"), loss_eqm_u)

accuracy_eqm_ar1 <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1_f_avg)
loss_eqm_ar1 <- LossLevel(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1_f_avg)
assign(paste("loss_eqm_ar1", l, sep = "_"), loss_eqm_ar1)

accuracy_eqm_ar1r <- rmse(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_ar1r_f_avg)
loss_eqm_ar1r <- LossLevel(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_ar1r_f_avg)
assign(paste("loss_eqm_ar1r", l, sep = "_"), loss_eqm_ar1r)

accuracy_eqm_np <- rmse(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_np_f_avg)
loss_eqm_np <- LossLevel(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_np_f_avg)
assign(paste("loss_eqm_np", l, sep = "_"), loss_eqm_np)


assign(paste("accuracy_midas", l, sep = "_"),
       matrix(rbind(accuracy_eqm_u, accuracy_eqm_ar1, accuracy_eqm_ar1r, accuracy_eqm_np), ncol = 1, nrow = 4,
              dimnames = list(c("MIDAS", "MIDAS-AR(1)", "MIDAS-AR(1)R", "MIDAS-NP"), c("RMSFE")
              )
       )
)


# MOST RECENT FORECAST
eqm_u_f_last <- matrix(as.numeric(eqm_u_f), 28)[28, ]
eqm_ar1_f_last <- matrix(as.numeric(eqm_ar1_f), 28)[28, ]
eqm_ar1r_f_last <- matrix(as.numeric(eqm_ar1r_f), 28)[28, ]
eqm_np_f_last <- matrix(as.numeric(eqm_np_f), 28)[28, ][1:(n_test - step + 1)]

accuracy_eqm_u <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_u_f_last)
accuracy_eqm_ar1 <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1_f_last)
accuracy_eqm_ar1r <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1r_f_last)
accuracy_eqm_np <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_np_f_last)

assign(paste("accuracy_midas_point", l, sep = "_"),
       matrix(rbind(accuracy_eqm_u, accuracy_eqm_ar1, accuracy_eqm_ar1r, accuracy_eqm_np), ncol = 1, nrow = 4, 
              dimnames = list(c("MIDAS", "MIDAS-AR(1)", "MIDAS-AR(1)R", "MIDAS-NP"),c("RMSFE")
              )
       )
)


# intra-period forecasts
eqm_u_f <- xts(eqm_u_f, order.by = date_d_test)
eqm_ar1_f <- xts(eqm_ar1_f, order.by = date_d_test)
eqm_ar1r_f <- xts(eqm_ar1r_f, order.by = date_d_test)
eqm_np_f <- xts(eqm_np_f, order.by = date_d_test)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#--------- PLOT RMSFE ------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
par(mfrow=c(1,3))
data = c()
data = cbind(aux_var_1_rmse_1, aux_eqm_ar1_rmse_1, aux_eqm_ar1r_rmse_1)
plot.zoo(data, plot.type = 'single', col = c(1,2,4), lwd=1, xlab='Period', ylab='RSMFE')
legend('bottomright', c('VAR (1)', 'MIDAS-ADL (l=13)', 'MIDAS-ADLr (l=6)'), lty = 1, col=c(1,2,4), nc=1, cex = 1, bty = "n")
title('h=1')

data = cbind(aux_var_1_rmse_2, aux_eqm_ar1_rmse_2, aux_eqm_ar1r_rmse_2)
plot.zoo(data, plot.type = 'single', col = c(1,2,4), lwd=1, xlab='Period', ylab='RSMFE')
legend('bottomright', c('VAR (1)', 'MIDAS-ADL (l=13)', 'MIDAS-ADLr (l=6)'), lty = 1, col=c(1,2,4), nc=1, cex = 1, bty = "n")
title('h=2')

data = cbind(aux_var_1_rmse_3, aux_eqm_ar1_rmse_3, aux_eqm_ar1r_rmse_3)
plot.zoo(data, plot.type = 'single', col = c(1,2,4), lwd=1, xlab='Period', ylab='RSMFE')
legend('topleft', c('VAR (1)', 'MIDAS-ADL (l=13)', 'MIDAS-ADLr (l=6)'), lty = 1, col=c(1,2,4), nc=1, cex = 1, bty = "n")
title('h=3')


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------- COMPARING MODELS --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--------------------------------------------------------------------------------
# 1144, 1145 and 1147 and 1148 are comments because the results are not saved
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# inlcude ar2 and arima_m=nl in MCS
#--------------------------------------------------------------------------------
# MODEL CONFIDENCE SET
aux <- cbind(loss_ar1, loss_ar2, loss_arima, loss_arima_nl, loss_var_1, loss_eqb_1, loss_eqm_u_6, loss_eqm_ar1_6, loss_eqm_ar1r_6, loss_eqm_np_6)
# loss_eqm_u_13, loss_eqm_ar1_13, loss_eqm_ar1r_13, loss_eqm_np_13,  loss_eqm_u_27, loss_eqm_ar1_27, 
# loss_eqm_ar1r_27, loss_eqm_np_27)
colnames(aux) <- cbind('loss_ar1', 'loss_ar2', 'loss_arima',  'loss_arima_nl','loss_var_1', 'loss_eqb_1', 'loss_eqm_u_6', 'loss_eqm_ar1_6', 'loss_eqm_ar1r_6', 'loss_eqm_np_6')
#  'loss_eqm_u_13', 'loss_eqm_ar1_13', 'loss_eqm_ar1r_13', 'loss_eqm_np_13',  'loss_eqm_u_27', 'loss_eqm_ar1_27', 
# 'loss_eqm_ar1r_27', 'loss_eqm_np_27')
MCSprocedure(aux)

# DIEBOLD E MARIANO
error_ar1 <- ar1_f_avg - inflation_cpi_test
error_ar2 <- ar2_f_avg - inflation_cpi_test
error_arima <- arima_f_avg - inflation_cpi_test
error_arima_nl <- arima_nl_f_avg - inflation_cpi_test
error_var_1 <- var_1_f_avg - inflation_cpi_test
error_eqb_1 <- eqb_1_f_avg - inflation_cpi_test
error_eqm_u <- eqm_u_f_avg - inflation_cpi_test
error_eqm_ar1 <- eqm_ar1_f_avg - inflation_cpi_test
error_eqm_ar1r <- eqm_ar1r_f_avg - inflation_cpi_test
error_eqm_np <- eqm_np_f_avg - inflation_cpi_test

dm.test(error_var_1, error_eqm_ar1r)


# intra period forecasts
color = c("orange", "blue", "violetred2", "green", "black")
plot.zoo(cbind(eqm_u_f, eqm_ar1_f, eqm_ar1r_f, eqm_np_f,
               inflation_cpi_full_d[date_d_test]),
         plot.type = "single",
         col = color, lwd = c(1,1,1),
         ylab = "Annual Inflation (%)", xlab = "Period")
legend("topleft", inset=c(0,0), y.intersp = 1,
       legend = c("MIDAS", "MIDAS-AR(1)", "MIDAS-AR(1)R", "MIDAS-NP", "Observed"),
       lty = 1, bty = "n", col = color, cex = 0.8)
title("intra-month forecasts")


par(mfrow=c(1,3))

plot.zoo(cbind(aux_var_1_rmse_1, aux_eqm_ar1_rmse_1), main="1-step ahead", plot.type = "single",
         col =c("dodgerblue", "violetred2"), ylab = "", xlab = "",  cex.axis=1.5, cex.main=1.5)

plot.zoo(cbind(aux_var_1_rmse_2, aux_eqm_ar1_rmse_2), main="2-step ahead", plot.type = "single",
         col =c("dodgerblue", "violetred2"), ylab = "", xlab = "",  cex.axis=1.5, cex.main=1.5)

plot.zoo(cbind(aux_var_1_rmse_3, aux_eqm_ar1_rmse_3), main="3-step ahead", plot.type = "single",
         col =c("dodgerblue", "violetred2"), ylab = "", xlab = "",  cex.axis=1.5, cex.main=1.5)



# ERICSSON AND MARTINEZ

# unrestricted regression
eq_em_u <- lm(inflation_cpi_test ~
                ar1_f_avg +
                arima_f_avg + 
                var_1_f_avg +
                eqb_1_f_avg +
                eqm_u_f_avg +
                eqm_ar1_f_avg +
                eqm_ar1r_f_avg +
                eqm_np_f_avg
)

linearHypothesis(eq_em_u, c("ar1_f_avg=1", "arima_f_avg=0", "var_1_f_avg=0", "eqb_1_f_avg=0",
                            "eqm_u_f_avg=0", "eqm_ar1_f_avg=0", "eqm_ar1r_f_avg=0", "eqm_np_f_avg=0"))

linearHypothesis(eq_em_u, c("ar1_f_avg=0", "arima_f_avg=1", "var_1_f_avg=0", "eqb_1_f_avg=0",
                            "eqm_u_f_avg=0", "eqm_ar1_f_avg=0", "eqm_ar1r_f_avg=0", "eqm_np_f_avg=0"))

linearHypothesis(eq_em_u, c("ar1_f_avg=0", "arima_f_avg=0", "var_1_f_avg=1", "eqb_1_f_avg=0",
                            "eqm_u_f_avg=0", "eqm_ar1_f_avg=0", "eqm_ar1r_f_avg=0", "eqm_np_f_avg=0"))

linearHypothesis(eq_em_u, c("ar1_f_avg=0", "arima_f_avg=0", "var_1_f_avg=0", "eqb_1_f_avg=1",
                            "eqm_u_f_avg=0", "eqm_ar1_f_avg=0", "eqm_ar1r_f_avg=0", "eqm_np_f_avg=0"))

linearHypothesis(eq_em_u, c("ar1_f_avg=0", "arima_f_avg=0", "var_1_f_avg=0", "eqb_1_f_avg=0",
                            "eqm_u_f_avg=1", "eqm_ar1_f_avg=0", "eqm_ar1r_f_avg=0", "eqm_np_f_avg=0"))

linearHypothesis(eq_em_u, c("ar1_f_avg=0", "arima_f_avg=0", "var_1_f_avg=0", "eqb_1_f_avg=0",
                            "eqm_u_f_avg=0", "eqm_ar1_f_avg=1", "eqm_ar1r_f_avg=0", "eqm_np_f_avg=0"))

linearHypothesis(eq_em_u, c("ar1_f_avg=0", "arima_f_avg=0", "var_1_f_avg=0", "eqb_1_f_avg=0",
                            "eqm_u_f_avg=0", "eqm_ar1_f_avg=0", "eqm_ar1r_f_avg=1", "eqm_np_f_avg=0"))

linearHypothesis(eq_em_u, c("ar1_f_avg=0", "arima_f_avg=0", "var_1_f_avg=0", "eqb_1_f_avg=0",
                            "eqm_u_f_avg=0", "eqm_ar1_f_avg=0", "eqm_ar1r_f_avg=0", "eqm_np_f_avg=1"))

# residual diagnostic
eq_em_ar1 <- lm(error_ar1 ~
                  arima_f_avg +
                  var_1_f_avg +
                  eqb_1_f_avg +
                  eqm_u_f_avg +
                  eqm_ar1_f_avg +
                  eqm_ar1r_f_avg +
                  eqm_np_f_avg)

summary(eq_em_ar1)

eq_em_arima <- lm(error_arima ~
                    ar1_f_avg +
                    var_1_f_avg +
                    eqb_1_f_avg +
                    eqm_u_f_avg +
                    eqm_ar1_f_avg +
                    eqm_ar1r_f_avg +
                    eqm_np_f_avg)

summary(eq_em_arima)

eq_em_var <- lm(error_var_1 ~
                  ar1_f_avg +
                  arima_f_avg +
                  eqb_1_f_avg +
                  eqm_u_f_avg +
                  eqm_ar1_f_avg +
                  eqm_ar1r_f_avg +
                  eqm_np_f_avg)
summary(eq_em_var)

eq_em_eqb <- lm(error_eqb_1 ~
                  ar1_f_avg +
                  arima_f_avg + 
                  var_1_f_avg +
                  eqm_u_f_avg +
                  eqm_ar1_f_avg +
                  eqm_ar1r_f_avg +
                  eqm_np_f_avg)
summary(eq_em_eqb)


eq_em_eqm_u <- lm(error_eqm_u ~
                    ar1_f_avg +
                    arima_f_avg + 
                    var_1_f_avg +
                    eqb_1_f_avg +
                    eqm_ar1_f_avg +
                    eqm_ar1r_f_avg +
                    eqm_np_f_avg)
summary(eq_em_eqm_u)

eq_em_eqm_ar1 <- lm(error_eqm_ar1 ~
                      ar1_f_avg +
                      arima_f_avg + 
                      var_1_f_avg +
                      eqb_1_f_avg +
                      eqm_u_f_avg +
                      eqm_ar1r_f_avg +
                      eqm_np_f_avg)
summary(eq_em_eqm_ar1)

eq_em_eqm_ar1r <- lm(error_eqm_ar1r ~
                       ar1_f_avg +
                       arima_f_avg + 
                       var_1_f_avg +
                       eqb_1_f_avg +
                       eqm_u_f_avg +
                       eqm_ar1_f_avg +
                       eqm_np_f_avg)
summary(eq_em_eqm_ar1r)

eq_em_eqm_np <- lm(error_eqm_np ~
                     ar1_f_avg +
                     arima_f_avg + 
                     var_1_f_avg +
                     eqb_1_f_avg +
                     eqm_u_f_avg +
                     eqm_ar1_f_avg +
                     eqm_ar1r_f_avg)
summary(eq_em_eqm_np)


# forecast differential
data = cbind(ar1_f_avg, arima_f_avg, var_1_f_avg, eqb_1_f_avg, eqm_u_f_avg, eqm_ar1_f_avg, eqm_ar1r_f_avg, eqm_np_f_avg)

data_minus = data.frame(data - ar1_f_avg)
eq_em_ar1 <- lm(error_ar1 ~
                  data_minus$arima_f_avg +
                  data_minus$var_1_f_avg +
                  data_minus$eqb_1_f_avg +
                  data_minus$eqm_u_f_avg +
                  data_minus$eqm_ar1_f_avg +
                  data_minus$eqm_ar1r_f_avg +
                  data_minus$eqm_np_f_avg)

summary(eq_em_ar1)

data_minus = data.frame(data - arima_f_avg)
eq_em_arima <- lm(error_arima ~
                    data_minus$ar1_f_avg +
                    data_minus$var_1_f_avg +
                    data_minus$eqb_1_f_avg +
                    data_minus$eqm_u_f_avg +
                    data_minus$eqm_ar1_f_avg +
                    data_minus$eqm_ar1r_f_avg +
                    data_minus$eqm_np_f_avg)

summary(eq_em_arima)

data_minus = data.frame(data - var_1_f_avg)
eq_em_var <- lm(error_var_1 ~
                  data_minus$ar1_f_avg +
                  data_minus$arima_f_avg +
                  data_minus$eqb_1_f_avg +
                  data_minus$eqm_u_f_avg +
                  data_minus$eqm_ar1_f_avg +
                  data_minus$eqm_ar1r_f_avg +
                  data_minus$eqm_np_f_avg)
summary(eq_em_var)

data_minus = data.frame(data - eqb_1_f_avg)
eq_em_eqb <- lm(error_eqb_1 ~
                  data_minus$ar1_f_avg +
                  data_minus$arima_f_avg + 
                  data_minus$var_1_f_avg +
                  data_minus$eqm_u_f_avg +
                  data_minus$eqm_ar1_f_avg +
                  data_minus$eqm_ar1r_f_avg +
                  data_minus$eqm_np_f_avg)
summary(eq_em_eqb)

data_minus = data.frame(data - eqm_u_f_avg)
eq_em_eqm_u <- lm(error_eqm_u ~
                    data_minus$ar1_f_avg +
                    data_minus$arima_f_avg + 
                    data_minus$var_1_f_avg +
                    data_minus$eqb_1_f_avg +
                    data_minus$eqm_ar1_f_avg +
                    data_minus$eqm_ar1r_f_avg +
                    data_minus$eqm_np_f_avg)
summary(eq_em_eqm_u)

data_minus = data.frame(data - eqm_ar1_f_avg)
eq_em_eqm_ar1 <- lm(error_eqm_ar1 ~
                      data_minus$arima_f_avg + 
                      data_minus$var_1_f_avg +
                      data_minus$eqb_1_f_avg +
                      data_minus$eqm_u_f_avg +
                      data_minus$eqm_ar1r_f_avg +
                      data_minus$eqm_np_f_avg)
summary(eq_em_eqm_ar1)

data_minus = data.frame(data - eqm_ar1r_f_avg)
eq_em_eqm_ar1r <- lm(error_eqm_ar1r ~
                       data_minus$ar1_f_avg +
                       data_minus$arima_f_avg + 
                       data_minus$var_1_f_avg +
                       data_minus$eqb_1_f_avg +
                       data_minus$eqm_u_f_avg +
                       data_minus$eqm_ar1_f_avg +
                       data_minus$eqm_np_f_avg)
summary(eq_em_eqm_ar1r)

data_minus = data.frame(data - eqm_np_f_avg)
eq_em_eqm_np <- lm(error_eqm_np ~
                     data_minus$ar1_f_avg +
                     data_minus$arima_f_avg + 
                     data_minus$var_1_f_avg +
                     data_minus$eqb_1_f_avg +
                     data_minus$eqm_u_f_avg +
                     data_minus$eqm_ar1_f_avg +
                     data_minus$eqm_ar1r_f_avg)

summary(eq_em_eqm_np)

