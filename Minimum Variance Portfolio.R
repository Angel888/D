amat <- array(1, dim = c(1,ncol(choiceret)))
bvec <- 1
zeros <- array(0, dim = c(ncol(choiceret),1))
ret_MVP <- as.matrix(NA, nrow = nexpwin, ncol = 1)
for (i in 1:nexpwin) {
  x <- head(choiceret,i+minwin)
  solQP <- solve.QP(cov(x), zeros, t(amat),bvec,meq = 1)
  wi <- as.vector(solQP$solution)
  ri <- wi * choiceret[(1+minwin+i),]
  ret_MVP[i] <- sum(ri)
}
ret_MVP <- as.matrix(ret_MVP)
rownames(ret_MVP) <- rownames(tail(choiceret,nexpwin))
colnames(ret_MVP) <- "MVP Return"
ret_MVP <- as.xts(ret_MVP)

# Above is the return of MVP
ret_acc_MVP <- cumsum(ret_MVP)
ret_acc_MVP <- as.matrix(ret_acc_MVP)
rownames(ret_acc_MVP) <- rownames(tail(choiceret,nexpwin))
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