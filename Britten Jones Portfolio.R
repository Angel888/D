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