#'
#' Tebaldi: Date: 2022-12-06
#'
#' The script was changed and split into 3 parts: Chap14_v2_part1.R;
#' Chap14_v2_part2.R; Chap14_v2_part3.R
#'
#' The 1st part is responsible for: (1) Loading the data; (2) Generating a plot
#' of the series; (3) Initial model of the series in GARCH / EGARCH / GJR-GARCH;
#'
#' The 2nd part is responsible for: (1) Forecast exercise with moving window.
#' This script takes a long time to execute, the results are saved in a file
#' called "Chap14_v2_part2.RData"
#'
#' The 3rd part is responsible for: (1) Forecast comparison using the
#' Diebold-Mariano test.
#'
#' The scripts must be run in sequence, however, the 3rd part can be run
#' separately if the file "Chap14_v2_part2.RData" has already been generated.
#' 
#' THIS IS THE 3RD SCRIPT

# Tebaldi: check if any package has to be installed
if(!any(.packages(all.available = TRUE) == "ggplot2")){  install.packages("ggplot2")  }
if(!any(.packages(all.available = TRUE) == "midasr")){   install.packages("midasr")   }
if(!any(.packages(all.available = TRUE) == "rugarch")){ install.packages("rugarch") }
if(!any(.packages(all.available = TRUE) == "forecast")){ install.packages('forecast', dependencies = TRUE) }
if(!any(.packages(all.available = TRUE) == "zoo")){ install.packages("zoo") }

# Tebaldi: Load used packages
library(data.table)
library(forecast)
library(ggplot2)
library(midasr)
library(rugarch)

load(file = "Chap14_v2_part2.RData")

sigma.pred <- rbindlist(list(garch.pred$Day, garch.pred$Week,
                             garch.pred$Month, garch.pred$SixMonth))
sigma.pred[, Frequency := rep(freq, each = t * 3)]
for (g in model) {
  for (h in freq) {
    sigma.mse <- (sigma.pred[GARCH == g & Frequency == h, sigma.hat]^2 -
                    daily.sp$data[(tt + 1):T, Return]^2)^2
    sigma.qlike <- log(sigma.pred[GARCH == g & Frequency == h, sigma.hat]^2) +
      daily.sp$data[(tt + 1):T, Return]^2 /
      sigma.pred[GARCH == g & Frequency == h, sigma.hat]^2
    sigma.pred[GARCH == g & Frequency == h,
               c('MSE', 'QLIKE') := list(sigma.mse, sigma.qlike)]
  }
}
write.csv(sigma.pred, 'sigma_hat_garch.csv', row.names = F)

# DM test
for (h in freq) {
  sigma.freq <- sigma.pred[Frequency == h]
  for (loss in c('MSE', 'QLIKE')) {
    print(dm.test(sigma.freq[GARCH == 'sGARCH'][[loss]],
                  sigma.freq[GARCH == 'eGARCH'][[loss]]))
    print(dm.test(sigma.freq[GARCH == 'sGARCH'][[loss]],
                  sigma.freq[GARCH == 'gjrGARCH'][[loss]]))
    print(dm.test(sigma.freq[GARCH == 'eGARCH'][[loss]],
                  sigma.freq[GARCH == 'gjrGARCH'][[loss]]))
  }
}

### 14.6.2 Realized Volatility ###
daily.rv <- list()
daily.rv$data <- fread('OxfordManRealizedVolatility.csv',
                       select = c('Date', 'RV'))
daily.rv$data[, c('Date', 'RV.D', 'RV.W', 'RV.M', 'RV.S') :=
                list(as.Date(as.character(Date), '%Y%m%d'),
                     c(0, RV[1:(T - 1)]),
                     c(rep(0, 5), zoo::rollapply(RV[1:(T - 1)], 5, sum)),
                     c(rep(0, 22), zoo::rollapply(RV[1:(T - 1)], 22, sum)),
                     c(rep(0, 125), zoo::rollapply(RV[1:(T - 1)], 125, sum)))]

# Pre-crisis sample
tt <- dim(daily.rv$data[Date <= '2006-12-31'])[1]

# Crisis sample
tt <- dim(daily.rv$data[Date <= '2008-10-31'])[1]

# Post-crisis sample
tt <- dim(daily.rv$data[Date <= '2009-12-31'])[1]


# MIDAS-RV
daily.rv$midas <- midas_r(RV ~ fmls(RV.D, 3, 1, nealmon),
                          start = list(RV.D = c(0, 0, 0)),
                          data = daily.rv$data[2:tt])

# HAR
daily.rv$har <- lm(RV ~ RV.D + RV.W + RV.M, data = daily.rv$data[22:tt])

summary(daily.rv$midas)
summary(daily.rv$har)

## Multiple horizons
daily.rv$MIDAS <- daily.rv$HAR <- list()

# One-day-ahead
daily.rv$MIDAS$Day <- forecast(daily.rv$midas,
                               newdata = daily.rv$data[tt:(T - 1)])$mean
daily.rv$HAR$Day <- as.numeric(predict(daily.rv$har,
                                       newdata = daily.rv$data[tt:(T - 1)]))

# One-week-ahead
daily.rv$midas <- midas_r(RV.W ~ fmls(RV.D, 3, 1, nealmon),
                          start = list(RV.D = rep(0, 3)),
                          data = daily.rv$data[6:tt])
daily.rv$MIDAS$Week <-
  forecast(daily.rv$midas, newdata = daily.rv$data[(tt - 4):(T - 5)])$mean
daily.rv$HAR$Week <-
  as.numeric(predict(daily.rv$har, newdata = daily.rv$data[(tt - 4):(T - 5)]))

# One-month-ahead
daily.rv$midas <- midas_r(RV.M ~ fmls(RV.D, 3, 1, nealmon),
                          start = list(RV.D = rep(0, 3)),
                          data = daily.rv$data[23:tt])
daily.rv$MIDAS$Month <-
  forecast(daily.rv$midas, newdata = daily.rv$data[(tt - 21):(T - 22)])$mean
daily.rv$HAR$Month <-
  as.numeric(predict(daily.rv$har,
                     newdata = daily.rv$data[(tt - 21):(T - 22)]))

# Six-month-ahead
daily.rv$midas <- midas_r(RV.S ~ fmls(RV.D, 3, 1, nealmon),
                          start = list(RV.D = rep(0, 3)),
                          data = daily.rv$data[126:tt])
daily.rv$MIDAS$SixMonth <-
  forecast(daily.rv$midas, newdata = daily.rv$data[(tt - 124):(T - 125)])$mean
daily.rv$HAR$SixMonth <-
  as.numeric(predict(daily.rv$har,
                     newdata = daily.rv$data[(tt - 124):(T - 125)]))

## DM test
daily.rv$oos <- daily.rv$data[(tt + 1):T, RV]
for (h in freq) {
  daily.rv$MSE[[h]] <- data.table('MIDAS' = (daily.rv$MIDAS[[h]] -
                                               daily.rv$oos)^2,
                                  'HAR' = (daily.rv$HAR[[h]] -
                                             daily.rv$oos)^2)
  daily.rv$QLIKE[[h]] <- data.table('MIDAS' = log(daily.rv$MIDAS[[h]]) +
                                      daily.rv$oos / daily.rv$MIDAS[[h]],
                                    'HAR' = log(daily.rv$HAR[[h]]) +
                                      daily.rv$oos / daily.rv$HAR[[h]])
  print(dm.test(daily.rv$MSE[[h]]$MIDAS, daily.rv$MSE[[h]]$HAR))
  print(dm.test(daily.rv$QLIKE[[h]]$MIDAS, daily.rv$QLIKE[[h]]$HAR))
}

