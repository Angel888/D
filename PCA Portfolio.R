ret_pca <- diff(log(prices))[-(120:122),]
# earlier prices can still be used, but we need a new return object
pca <- princomp(ret_pca) 
pca2 <- princomp(ret_pca, covmat = MASS::cov.rob(stackloss))# Robust PCA, consider later
pca_acc_sdev <- cumsum(pca$sdev)/sum(pca$sdev)
# Basically we can find that top 15 factors explain 71% of total variance
# So I decide to use 15 factors
f1 <- colSums(pca$loadings[,1]*t(ret_pca)) # Just a test data
f1 <- as.matrix(f1) # Just a test data
factor_matrix <- matrix(NA,nrow = nrow(ret_pca), ncol = 15)
for (i in 1:15) {
  x <- colSums(pca$loadings[,i]*t(ret_pca))
  factor_matrix[,i] <- x
}
colnames(factor_matrix) <- c("f1","f2","f3","f4","f5",
                             "f6","f7","f8","f9","f10",
                             "f11","f12","f13","f14","f15")
rownames(factor_matrix) <- rownames(f1)
# Above is the factor returns of all 15 factors & adding data to factor_matrix
factor_matrix <- tk_tbl(factor_matrix,preserve_index = TRUE, rename_index = "date")
# Change factor_matrix into a tibble data.frame
pca_loading <- as.matrix(unclass(pca$loadings))
# import loadings (eigenvectors) of pca into a matrix form
weight_pca <- matrix(NA, nrow = nrow(pca_loading), ncol = 15)
rownames(weight_pca) <- rownames(pca_loading)
colnames(weight_pca) <- colnames(factor_matrix)[2:16]
# Create a weight matrix for the factor mimicking portfolios
for (i in 1:15) {
  wi <- (1 / (sum(pca_loading[,i])) ) * pca_loading[,i]
  weight_pca[,i] <- wi
}
# Create the weight matrix for pca portfolio, full sample, think about back test later
f1_return <- Return.portfolio(
  ret_pca,
  weights = weight_pca[,1],
  rebalance_on = "months"
)
f1_acc_return <- cumsum(f1_return)
colnames(f1_acc_return) <- colnames(f1_return)
highchart(type = "stock") %>%
  hc_add_series(x_acc$portfolio.returns, name = "Factor No.1 Return") %>%
  hc_title("cumulative return for first factor")
# ------------------Above is the return for full-sample calculation----------------------
# Need to work on back-test data later .. So tired

# Below is to define a function to mimick a factor

factor_return <- function(data, minwin, nexpwin, factor.no){
  weight_factor <<- matrix(NA, nrow = 59, ncol = 30)
  ret_factor <<- rep(NA, 59)
  # Below is the back-test function
  for(i in 1:nexpwin){
    ret_pca_loop <- head(data,i + minwin)
    pca_loop <- princomp(ret_pca_loop)
    loading_loop <- as.matrix(pca_loop$loading)[,factor.no]
    wf <- loading_loop * (1/sum(loading_loop))
    weight_factor[i,] <<- wf
    ret_factor[i] <<- sum(wf * ret_pca[i+minwin+1,])
  }
  # Above is the back-test function
  ret_factor <<- as.matrix(ret_factor)
  rownames(ret_factor) <<- rownames(tail(data,nexpwin))
  ret_factor <<- as.xts(ret_factor)
  colnames(ret_factor) <<- "return"
  ret_factor_acc <<- cumsum(ret_factor)
  highchart(type = "stock") %>%
    hc_add_series(ret_factor_acc$return, name = "Factor")
}

# Above is to define a function to mimick a factor

factor_return(ret_pca,minwin = minwin,nexpwin = nexpwin,factor.no = 1)
# This means mimick the first factor (who explained the most variance) 
