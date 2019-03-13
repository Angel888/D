# Step One - Load the package
library(elasticnet)
library(tidyquant)
library(tidyverse)
library(reshape2)

library(tidyquant)
library(tidyverse)
library(reshape2)
library(quadprog)

data <- tq_index("sp500") # or whatever indices included in the package

x1 <- grep("BRK.B", data$symbol)
data$symbol[x1] <- "BRK-B"

x2 <- grep("BF.B", data$symbol)
data$symbol[x2] <- "BF-B"

data <- data%>% 
  tq_get(get = "stock.prices",complete_cases = TRUE) %>%
  select(symbol, date, adjusted,close) %>%
  group_by(year(date), month(date), symbol) %>% 
  summarize( adjusted = tail(adjusted, 1),
             close = tail(close, 1),
             date = tail(date,1))

prices <- acast(data, date ~ symbol, value.var = "adjusted")
return <- diff(log(prices))

# Eliminate NA terms

drop <- which(is.na(colSums(return)))
return <- return[,-drop]

# Random Choice

# Note that the number of stocks chosen must be smaller than the minimum window chosen
sample_size <- 20

random <- sample(1:ncol(return), sample_size, replace = FALSE)

# Set replace = false to avoide repetition

sample_return <- return[,random]

# A matrix store returns chosen

# Set the window for Portfolios

minwin_elastic <- 59 # or whatever you like
nexpwin <- nrow(sample_return) - (minwin_elastic+1)


# Create a matrix of Elasticnet Return's Weights
weight_elastic <- matrix(NA, nrow = nexpwin, ncol = ncol(sample_return))

# Create a weight matrix for elastic net portfolio
for(i in 1:nexpwin){
  elastic_retloop <- sample_return[1:(minwin+i),]
  elastic_oneloop <- rep(1,nrow(elastic_retloop))
  random_stock_elastic = elastic(elastic_retloop,elastic_oneloop,lambda=1,
                           normalize=FALSE,intercept=FALSE) 
  nasset = order(random_stock_elastic$Cp)[1]
  # if all get zero coeff, use equal weights
  if(nasset==1) beta = rep(1,ncol(sample_return)) 
  else beta = random_stock_elastic$beta.pure[nasset,]
  w = beta/sum(beta)
  weight_elastic[i,] = w
}

# Now calculate the return of Elastic Net Portfolio

ret_elastic <- matrix(NA, nrow = nexpwin, ncol = 1)
minwin_elastic <- nrow(sample_return) - (nexpwin+1)

for(i in 1: nexpwin){
  loop_elastic <- weight_elastic[i,] * sample_return[(i + 1 + minwin_elastic),]
  ret_elastic[i] <- sum(loop_elastic)
}

# Turn ret_elastic into a better format
colnames(ret_elastic) <- "elastic Return"
rownames(ret_elastic) <- rownames(tail(sample_return,nexpwin))
ret_elastic <- as.xts(ret_elastic)

# And calculate the cumulative return

ret_elastic_acc <- cumsum(ret_elastic)
colnames(ret_elastic_acc) <- "elastic Cumulative Return"