choice <- sample(1:ncol(sp500return), 30, replace=FALSE)
# Create a random choice of 30 stocks... Thinking About a solution to specific charateristics later
choiceret <- sp500return[,choice]
# Create the return matrix for randomly chosen stocks
choicenames <- colnames(sp500return)[choice]
# Store all the names of randomly chosen stocks

library(elasticnet)
# Reference: Class 3 Handouts P39 
ones <- rep(1, nrow(sp500return))
# A vector of ones to perform the regression based approach
choice_enet = enet(choiceret,ones,
                   lambda=1, normalize=FALSE,
                   intercept=FALSE)
nasset = order(choice_enet$Cp)[1]
choice_w <- choice_enet$beta.pure[nasset,]/sum(choice_enet$beta.pure[nasset,])
choice_p_ret <- rowSums( choice_w * choiceret )
choice_mean <- mean(choice_p_ret)
choice_median <- median(choice_p_ret)
choice_sd <- sd(choice_p_ret)
# mean, median and standard deviation of choice portfolio
choice_maxdd <- maxDrawdown(choice_p_ret)
# Calculation of the max Drawdown
choice_sharpe <- as.sr(choice_p_ret, c0 = rfr, ope = 12)
choice_sharpe_test <- sr_test(choice_p_ret, c0 = rfr, ope = 12, zeta = 1)
choice_sharpe_test <- sr_test(choice_p_ret, c0 = rfr, ope = 12, zeta = 0)
# Normal Sharpe Ratios.. Need to further discuss on how why the difference exist

minwin <- 59
nexpwin <- nexpwin = nrow(choiceret)-(minwin+1)
for(i in 1:nexpwin){
  retloop = choiceret[1:(minwin+i),]
  oneloop = rep(1,nrow(retloop))
  choice_enet = enet(retloop,oneloop,lambda=0,
                     normalize=FALSE,intercept=FALSE) 
  nasset = order(choice_enet$Cp)[1]
  # if all get zero coeff, use equal weights
  if(nasset==1) beta = rep(1,ncol(choiceret)) 
  else beta = choice_enet$beta.pure[nasset,]
  w = beta/sum(beta)
  choice_w[i,] = w
}
choice_bt_ret <- rowSums(choice_w * tail(choiceret, nexpwin))
# back-testing return for choice portfolio
choice_bt_accret <- cumsum(choice_bt_ret)
# back-testing accumulative return for choice portfolio
choice_bt_mean <- mean(choice_bt_ret)
choice_bt_median <- median(choice_bt_ret)
choice_bt_sd <- sd(choice_bt_ret)
# mean, median and standard deviation of back test choice portfolio
choice_maxd <- maxDrawdown(choice_bt_ret)
# Calculation of the max Drawdown of back test choice portfolio
choice_bt_sharpe <- as.sr(choice_bt_ret, c0 = rfr, ope = 12)
choice_bt_sharpe_test1 <- sr_test(choice_bt_ret, c0 = rfr2014, ope = 12, zeta = 1,
                                  alternative = "two.sided")
choice_bt_sharpe_test0 <- sr_test(choice_bt_ret, c0 = rfr2014, ope = 12, zeta = 0, 
                                  alternative = "two.sided")
# This is to calculate the sharpe ratio of choice back test portfolio
# As well as test the significance
choice_bt_accret <- as.xts(choice_bt_accret)
colnames(choice_bt_accret) <- "return"
# Change accumulative return of back-test portfolio into "xts" object and assign a name
highchart(type = "stock") %>%
  hc_title(text = "Cumulative Returns of Bakc-testing Portfolio") %>%
  hc_add_series(choice_bt_accret$return, name = "Choice Back Test")
# Plot the cumulative return with Highcharter package
# Above is the back - test code ---------------------------------------------------
