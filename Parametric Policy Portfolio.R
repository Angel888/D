PPP_mktcap <- random_stock_mktcap[-1,]
PPP_return <- random_stock_return

# There is no need to drop "NA" terms because I have done it in Q-a
momentum <- matrix(NA, nrow =  nrow(PPP_return), ncol =  ncol(PPP_return))

for(i in 13:nrow(momentum)){
  momentum[i, ] <- apply(PPP_return[(i-12):(i-1),], 2, sum)
}


# remove NA terms
momentum <- momentum[-(1:12),]
PPP_return <- PPP_return[-(1:12),]
PPP_mktcap <- PPP_mktcap[-(1:11),]

# Create lag in mktcap
PPP_mktcap <- PPP_mktcap[-(nrow(PPP_mktcap)),]


# Create the momentum returns and align the return matrix with it
momentum <- t(apply(momentum,1,FUN = scale))
PPP_mktcap <- t(apply(PPP_mktcap,1,FUN = scale))
wb <- 1/ncol(random_stock_return)
nt <- 1/ncol(random_stock_return)
riskaversion <- 5
# Above is taken from the handouts, easy and trivial, do not worry
pps = function(x, wb, nt, ret, momentum, PPP_mktcap, riskaversion){ 
  # weights = benchmark weight
  # + 1/N*characteristics*char. loadings
  wi = wb + nt * (x[1]*momentum + x[2]*PPP_mktcap)
  # weighted returns = sum weight_i*ret_i # at each t
  wret = rowSums(wi*ret)
  # vNM utility function
  ut = ((1 + wret)^(1 - riskaversion))/(1 - riskaversion) 
  u = -mean(ut)
  ## Below is the penalty function
  lv = abs(apply(wi, 1, function(x) sum(x[x<0])))
  pen=abs(sum((lv>1)*lv))
  ## Above is the penalty function
  return(u+pen) }
# Basically copied the data of handouts, but changed certain expressions
# To avoid over-simplification and confusion
minwin_ppp <- nrow(PPP_return) - (nexpwin + 1)
weight_PPP <- matrix(NA, ncol = ncol(PPP_return), nrow = nexpwin)
res_save <- matrix(NA, ncol = ncol(PPP_return), nrow = nexpwin)

# This is to align historical data with earlier results, keep nexpwin constant
for(i in 1:nexpwin){
  opt = optim(c(0, 0), pps, 
              wb = wb, 
              nt = nt, 
              ret = PPP_return[1:(minwin_ppp + i), ], 
              momentum = momentum[1:(minwin_ppp + i), ],
              PPP_mktcap = PPP_mktcap[1:(minwin_ppp + i), ],
              riskaversion = riskaversion, 
              method = "BFGS")
  res = opt$par
  w = wb + 
    nt*(res[1]*momentum[i + minwin_ppp+1, ] + res[2]*PPP_mktcap[i + minwin_ppp+1, ])
  weight_PPP[i, ] = w
  res_save[i, ] = res
}


# Now calculate the returns

ret_PPP <- matrix(NA, nrow = nexpwin, ncol = 1)
for (i in 1:nexpwin){
  wi <- weight_PPP[i,] * tail( PPP_return, nexpwin )[i,]
  ret_PPP[i] <- sum(wi)
}

# Make the return maktrix in a better form
rownames(ret_PPP) <- rownames(tail( PPP_return, nexpwin ))
colnames(ret_PPP) <- "PPP Return"
ret_PPP <- as.xts(ret_PPP)
ret_PPP_acc <- cumsum(ret_PPP)
colnames(ret_PPP_acc) <- "PPP cumulative return"