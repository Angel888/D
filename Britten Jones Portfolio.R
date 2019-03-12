# Correct Version of Regression Calculated Tangency Portfolio
rfr <- 0.03/12
library(quadprog)
return_minus_rfr <- choiceret - rfr
ones <- rep(1, nrow(choiceret))
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
rownames(ret_TP) <- rownames(tail(choiceret,nexpwin))
colnames(ret_TP) <- "TP Return"
ret_TP <- as.xts(ret_TP)

# Calculate the Return Vector
ret_TP_acc <- cumsum(ret_TP)
ret_TP_acc <- as.matrix(ret_TP_acc)
colnames(ret_TP_acc) <- "TP Cumulative Return"
rownames(ret_TP_acc) <- rownames(tail(choiceret,nexpwin))
ret_TP_acc <- as.xts(ret_TP_acc)
# Calculate the accumulative return vector

TP_mean <- mean(ret_TP)
TP_sd <- sd(ret_TP)
TP_median <- median(ret_TP)
TP_maxdrawdonw <- maxDrawdown(ret_TP)
TP_sharpe <- as.sr(ret_TP, c0 = rfr, ope = 12,epoch = "yr")
TP_sharpe_test_0 <- sr_test(x = ret_TP, alternative = "two.sided", zeta = 0, ope = 12)
TP_sharpe_test_1 <- sr_test(x = ret_TP, alternative = "two.sided", zeta = 1, ope = 12)