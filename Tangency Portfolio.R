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