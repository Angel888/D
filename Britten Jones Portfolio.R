library(tidyquant)
library(tidyverse)
library(reshape2)
library(quadprog)

data <- tq_index("sp500") # or whatever indices included in the package

x1 <- grep("BRK.B", data$symbol)
data$symbol[x1] <- "BRK-B"

x2 <- grep("BF.B", data$symbol)
data$symbol[x2] <- "BF-B"

data <- data%>% 
  tq_get(get = "stock.prices",complete_cases = TRUE) %>%
  select(symbol, date, adjusted,close) %>%
  group_by(year(date), month(date), symbol) %>% 
  summarize( adjusted = tail(adjusted, 1),
             close = tail(close, 1),
             date = tail(date,1))

prices <- acast(data, date ~ symbol, value.var = "adjusted")
return <- diff(log(prices))

# Eliminate NA terms

drop <- which(is.na(colSums(return)))
return <- return[,-drop]

# Random Choice

# Note that the number of stocks chosen must be smaller than the minimum window chosen
sample_size <- 20

random <- sample(1:ncol(return), sample_size, replace = FALSE)

# Set replace = false to avoide repetition

sample_return <- return[,random]

# A matrix store returns chosen


rfr <- 0.03/12

return_minus_rfr <- sample_return - rfr
ones <- rep(1, nrow(sample_return))
ret_TP <- matrix(NA, nrow = nexpwin, ncol = 1)

for (i in 1:nexpwin) {
  ones <- rep(1,(i + minwin))
  solR <- lm(ones ~ -1 + head(return_minus_rfr, i+minwin))
  w2 <- coef(solR)
  w2 <- w2/sum(w2)
  ri <- w2 * return_minus_rfr[(i+minwin+1),]
  ret_TP[i] <- sum(ri)
}

# Below: turn ret_TP into a xts form
ret_TP <- as.matrix(ret_TP)
rownames(ret_TP) <- rownames(tail(sample_return,nexpwin))
colnames(ret_TP) <- "TP Return"
ret_TP <- as.xts(ret_TP)

# Calculate the Return Vector
ret_TP_acc <- cumsum(ret_TP)
ret_TP_acc <- as.matrix(ret_TP_acc)
colnames(ret_TP_acc) <- "TP Cumulative Return"
rownames(ret_TP_acc) <- rownames(tail(sample_return,nexpwin))
ret_TP_acc <- as.xts(ret_TP_acc)
# Calculate the accumulative return vector

TP_mean <- mean(ret_TP)
TP_sd <- sd(ret_TP)
TP_median <- median(ret_TP)
TP_maxdrawdonw <- maxDrawdown(ret_TP)
TP_sharpe <- as.sr(ret_TP, c0 = rfr, ope = 12,epoch = "yr")
TP_sharpe_test_0 <- sr_test(x = ret_TP, alternative = "two.sided", zeta = 0, ope = 12)
TP_sharpe_test_1 <- sr_test(x = ret_TP, alternative = "two.sided", zeta = 1, ope = 12)