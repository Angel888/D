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


# Now perform back-test on portfolio
amat <- array(1, dim = c(1,ncol(sample_return)))
bvec <- 1
zeros <- array(0, dim = c(ncol(sample_return),1))
ret_MVP <- as.matrix(NA, nrow = nexpwin, ncol = 1)
for (i in 1:nexpwin) {
  x <- head(sample_return,i+minwin)
  solQP <- solve.QP(cov(x), zeros, t(amat),bvec,meq = 1)
  wi <- as.vector(solQP$solution)
  ri <- wi * sample_return[(1+minwin+i),]
  ret_MVP[i] <- sum(ri)
}
ret_MVP <- as.matrix(ret_MVP)
rownames(ret_MVP) <- rownames(tail(sample_return,nexpwin))
colnames(ret_MVP) <- "MVP Return"
ret_MVP <- as.xts(ret_MVP)

# Above is the return of MVP
ret_acc_MVP <- cumsum(ret_MVP)
ret_acc_MVP <- as.matrix(ret_acc_MVP)
rownames(ret_acc_MVP) <- rownames(tail(sample_return,nexpwin))
colnames(ret_acc_MVP) <- "MVP Return"
ret_acc_MVP <- as.xts(ret_acc_MVP)

colnames(ret_acc_MVP) <- "MVP Cumulative Return"
# Above is the accumulative return of MVP

MVP_mean <- mean(ret_MVP)
MVP_sd <- sd(ret_MVP)
MVP_median <- median(ret_MVP)
MVP_maxdrawdonw <- maxDrawdown(ret_MVP)
MVP_sharpe <- as.sr(ret_MVP, c0 = rfr, ope = 12,epoch = "yr")

# Above is regular calculations

MVP_TP_sharpe_test <- sr_test(ret_MVP, ret_TP,ope = 12,paired = TRUE)