library(tidyquant)
library(reshape2)
library(ggplot2)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(highcharter)
library(SharpeR)
library(timetk)

SP <- tq_index("sp500") 

# R cannot correctly read the name of two stocks: BRK.B and BF.B

# Substitute the correct name into the symbol lists

x1 <- grep("BRK.B", SP$symbol)
SP$symbol[x1] <- "BRK-B"

x2 <- grep("BF.B", SP$symbol)
SP$symbol[x2] <- "BF-B"

# Get the prices of all stocks in SP500 index

SP <- SP%>% 
  tq_get(get = "stock.prices",
         complete_cases = TRUE, 
         to = "2018-12-31")

SP <- SP %>%
  select(symbol, date, adjusted,close, shares_held) %>%
  group_by(year(date), month(date), symbol) %>% 
  summarize( adjusted = tail(adjusted, 1),
             close = tail(close, 1),
             # compute market cap
             mktcap = tail(close * shares_held, 1),
             date = tail(date,1))

# Create matrix for variables
# Matrices are much easier to manipulate than data frames
# I hate data frames, they are like sins to coding

sp500_mktcap <- SP %>%
  acast(date ~ symbol,value.var = "mktcap")


sp500_price <- SP %>% 
  acast(date~symbol,value.var = "adjusted")


nrow_sp500 <- nrow(sp500_price)

sp500_intermediate_1 <- sp500_price[-1,]
sp500_intermediate_2 <- sp500_price[-nrow_sp500,]

sp500_simple_return <- (sp500_intermediate_1/sp500_intermediate_2 - 1)

# drop NA terms

drop <- which(is.na(colSums(sp500_simple_return)))


sp500_simple_return <- sp500_simple_return[,-drop]
sp500_mktcap <- sp500_mktcap[,-drop]

# Calculate individual stock returns in S&P500

sp500_continuous_return <- diff(log(sp500_price))
sp500_continuous_return <- sp500_continuous_return[,-drop]

# calculate the Equally Weighted Portfolio for S&P500

sp500_EW <- Return.portfolio(sp500_continuous_return,
                             rebalance_on = "months")

# calculate the cumulative return for S&P500 EW portfolio

sp500_EW_acc <- cumsum(sp500_EW)

# calculate a series of requried stuff

sp500_EW_mean <- mean(sp500_EW)
sp500_EW_sd <- sd(sp500_EW)
sp500_EW_median <- median(sp500_EW)
sp500_EW_skewness <- skewness(sp500_EW)
sp500_EW_kurtosis <- kurtosis(sp500_EW)

# test the sharpe ratio of Equally Weighted Portfolio
rfr <- 0.03/12
sp500_EW_sharpe <- as.sr(sp500_EW, c0 = rfr, ope = 12)
sp500_EW_sharpe_test <- sr_test(sp500_EW,alternative = "two.sided",ope = 12)

