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
#' THIS IS THE 1ST SCRIPT


rm(list = ls())

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

daily.sp <- list()

### 14.6.1 ARCH-type Models ###
# S&P 500 Index
daily.sp$data <- fread('OxfordManRealizedVolatility.csv',
                       select = c('Date', 'RV', 'Return'))
daily.sp$data[, Date := as.Date(as.character(Date), '%Y%m%d')]
ggplot(daily.sp$data, aes(Date)) + geom_line(aes(y = Return)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  xlab('') + ylab('') + ggtitle('S&P 500 Daily Returns')
ggplot(daily.sp$data, aes(Date)) + geom_line(aes(y = RV)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  xlab('') + ylab('') + ggtitle('S&P 500 Index Realized Volatility (5-min)')

T <- dim(daily.sp$data)[1]
tt <- dim(daily.sp$data[Date <= '2006-12-31'])[1]
model <- c('sGARCH', 'eGARCH', 'gjrGARCH')
freq <- c('Day', 'Week', 'Month', 'SixMonth')

## GARCH / EGARCH / GJR-GARCH
# Jan. 2000 to Dec. 2006
for (g in model) {
  garch.spec <- ugarchspec(variance.model = list(model = g,
                                                 garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0),
                                             include.mean = F))
  daily.sp[[g]] <- ugarchfit(data = daily.sp$data[Date <= '2006-12-31',
                                                  Return], garch.spec)
}

# Jan. 2000 to Oct. 2008
for (g in model) {
  garch.spec <- ugarchspec(variance.model = list(model = g,
                                                 garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0),
                                             include.mean = F))
  daily.sp[[g]] <- ugarchfit(data = daily.sp$data[Date <= '2008-10-31',
                                                  Return], garch.spec)
}

# Jan. 2000 to Dec. 2009
for (g in model) {
  garch.spec <- ugarchspec(variance.model = list(model = g,
                                                 garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0),
                                             include.mean = F))
  daily.sp[[g]] <- ugarchfit(data = daily.sp$data[Date <= '2009-12-31',
                                                  Return], garch.spec)
}

