# Step One - Load the package

library(elasticnet)

# Create a matrix of Elasticnet Return's Weights
weight_elastic <- matrix(NA, nrow = nexpwin, ncol = ncol(random_stock_return))

# Create a weight matrix for elastic net portfolio
for(i in 1:nexpwin){
  elastic_retloop <- random_stock_return[1:(minwin+i),]
  elastic_oneloop <- rep(1,nrow(elastic_retloop))
  random_stock_elastic = elastic(elastic_retloop,elastic_oneloop,lambda=1,
                           normalize=FALSE,intercept=FALSE) 
  nasset = order(random_stock_elastic$Cp)[1]
  # if all get zero coeff, use equal weights
  if(nasset==1) beta = rep(1,ncol(random_stock_return)) 
  else beta = random_stock_elastic$beta.pure[nasset,]
  w = beta/sum(beta)
  weight_elastic[i,] = w
}

# Now calculate the return of Elastic Net Portfolio

ret_elastic <- matrix(NA, nrow = nexpwin, ncol = 1)
minwin_elastic <- nrow(random_stock_return) - (nexpwin+1)

for(i in 1: nexpwin){
  loop_elastic <- weight_elastic[i,] * random_stock_return[(i + 1 + minwin_elastic),]
  ret_elastic[i] <- sum(loop_elastic)
}

# Turn ret_elastic into a better format
colnames(ret_elastic) <- "elastic Return"
rownames(ret_elastic) <- rownames(tail(random_stock_return,nexpwin))
ret_elastic <- as.xts(ret_elastic)

# And calculate the cumulative return

ret_elastic_acc <- cumsum(ret_elastic)
colnames(ret_elastic_acc) <- "elastic Cumulative Return"