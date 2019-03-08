SP <- tq_index("sp500") %>% 
  tq_get(get = "stock.prices", complete_cases = TRUE)

BRK.B <- tq_get("BRK-B", get = "stock.prices", complete_cases = TRUE)
BF.B <- tq_get("BF-B", get = "stock.prices", complete_cases = TRUE)

SP <- SP %>%
  select(symbol, date, adjusted) %>%
  group_by(year(date),month(date),symbol) %>%
  summarise(date = tail(date,1),
            adjusted = tail(adjusted,1))

sp500 <- SP %>% 
  acast(date~symbol,value.var = "adjusted")

tail(rownames(sp500),10)
nrow(sp500)

sp500return <- diff(log(sp500))
# Calculate the Return of sp500
drop <- which(is.na(colSums(sp500return)))
sp500return <- sp500return[,-drop]

library(PerformanceAnalytics)
library(PortfolioAnalytics)
EW <- Return.portfolio(sp500return,rebalance_on = "months")
# Create Equally Weighted Portfolio Return, a "xts" object