SP2 <- tq_index("sp500") %>% 
  tq_get(get = "stock.prices", complete_cases = TRUE) %>%
  filter(symbol %in% choicenames) %>%
  select(symbol, date, adjusted, close ,shares_held) %>%
  group_by(year(date), month(date), symbol) %>% 
  summarize( adjusted = tail(adjusted, 1),
             close = tail(close, 1),
             # compute market cap
             mktcap = tail(close * shares_held, 1),
             date = tail(date,1))
# Above gives everything we need for next step
mktcap <- acast(SP2,date ~ symbol,value.var = "mktcap")[-1,]
prices <- acast(SP2,date~symbol,value.var = "adjusted")
ret <- diff(log(prices))
# There is no need to drop "NA" terms because I have done it in Q-a
momentum <- matrix(NA, nrow =  nrow(ret), ncol =  ncol(ret))
for(i in 13:nrow(momentum)){momentum[i, ] <- 
  apply(ret[(i-12):(i-1),], 2, sum)}
momentum <- momentum[-(1:12),]
momentum <- momentum[-(108:110),]
ret <- ret[-(1:12),]
ret <- ret[-(108:110),]
mktcap <- mktcap[-(1:12),]
mktcap <- mktcap[-(108:110),]
# Create the momentum returns and align the return matrix with it
momentum <- t(apply(momentum,1,FUN = scale))
mktcap <- t(apply(mktcap,1,FUN = scale))
weight_benchmark <- 1/ncol(ret)
nt <- 1/ncol(ret)
riskaversion <- 5
# Above is taken from the handouts, easy and trivial, do not worry
pps = function(x, wb, nt, ret, momentum, mktcap, riskaversion){ 
  # weights = benchmark weight
  # + 1/N*characteristics*char. loadings
  wi = wb + nt * (x[1]*momentum + x[2]*mktcap)
  # weighted returns = sum weight_i*ret_i # at each t
  wret = rowSums(wi*ret)
  # vNM utility function
  ut = ((1 + wret)^(1 - riskaversion))/(1 - riskaversion) 
  u = -mean(ut)
  ## Below is the penalty function
  lv = abs(apply(wi, 1, function(x) sum(x[x<0])))
  pen=abs(sum((lv>2)*lv))
  ## Above is the penalty function
  return(u+pen) }
# Basically copied the data of handouts, but changed certain expressions
# To avoid over-simplification and confusion
minwin_ppp <- nrow(ret) - (nexpwin + 1)
# This is to align historical data with earlier results, keep nexpwin constant
for(i in 1:nexpwin){
  opt = optim(c(0, 0), pps, 
              wb = weight_benchmark, 
              nt = nt, 
              ret = ret[1:(minwin_ppp + i), ], 
              momentum = momentum[1:(minwin_ppp + i), ],
              mktcap = mktcap[1:(minwin_ppp + i), ],
              riskaversion = riskaversion, 
              method = "BFGS")
  res = opt$par
  w = weight_benchmark + 
    nt*(res[1]*momentum[i + minwin_ppp+1, ] + res[2]*mktcap[i + minwin_ppp+1, ])
  weight_ppp[i, ] = w
  res_save[i, ] = res
}
# Above produce the weights of PPP portfolio at each period