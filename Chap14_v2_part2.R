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
#' THIS IS THE 2ND SCRIPT

# Forecast evaluation
garch.pred <- list('Day' = data.table(), 'Week' = data.table(),
                   'Month' = data.table(), 'SixMonth' = data.table())

ptm <- proc.time()
for (g in model) {
  g.spec <- ugarchspec(variance.model = list(model = g, garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0),
                                         include.mean = F))
  
  # daily rolling window
  tt=1737
  g.pred <- list('Day' = rep(0, T - tt), 'Week' = rep(0, T - tt),
                 'Month' = rep(0, T - tt), 'SixMonth' = rep(0, T - tt))
  
  for (t in 1:(T - tt)) {
    cat(sprintf("%s - %7.4f%%", g, 100*t/(T - tt)), "\n") # Tebaldi: Apenas para saber o status do processo
    g.fit <- ugarchfit(data = daily.sp$data[t:(t + tt - 1), Return],
                       g.spec)
    g.pred$Day[t] <-
      as.numeric(ugarchforecast(g.fit, n.ahead = 1)@forecast$sigmaFor)
    
    g.fit <- ugarchfit(data = daily.sp$data[t:(t + tt - 5), Return],
                       g.spec)
    g.pred$Week[t] <-
      as.numeric(ugarchforecast(g.fit, n.ahead = 5)@forecast$sigmaFor)[5]
    
    garch.fit <- ugarchfit(data = daily.sp$data[t:(t + tt - 20), Return],
                           g.spec)
    g.pred$Month[t] <-
      as.numeric(ugarchforecast(garch.fit,
                                n.ahead = 20)@forecast$sigmaFor)[20]
    
    garch.fit <- ugarchfit(data = daily.sp$data[t:(t + tt - 120), Return],
                           g.spec)
    g.pred$SixMonth[t] <-
      as.numeric(ugarchforecast(garch.fit,
                                n.ahead = 120)@forecast$sigmaFor)[120]
  }
  
  for (h in freq) {
    g.pred[[h]] <- data.table('sigma.hat' = g.pred[[h]], 'GARCH' = g)
    garch.pred[[h]] <- rbindlist(list(garch.pred[[h]], g.pred[[h]]))
  }
}
# Tebaldi: Guarda o tempo total de execucao
TotalTime <- proc.time() - ptm
print(TotalTime)

# Tebaldi: Salva o resultado da execucao.
save.image("Chap14_v2_part2.RData")
