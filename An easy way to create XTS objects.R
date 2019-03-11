# an easy way to turn return series into XTS object

# The key is to import dates into rownames of matrix

# Give that you are 100% sure about the date

# Same methods can be duplicated in different portfolios

ret_TP <- as.matrix(ret_TP)
rownames(ret_TP) <- rownames(tail(random_stock_return,nexpwin))
colnames(ret_TP) <- "TP Return"
ret_TP <- as.xts(ret_TP)