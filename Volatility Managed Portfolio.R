exposure <- 0.05 
# This is just a guess
trial_init_vol <- matrix(NA, ncol = ncol(choiceret), nrow = 1)
trial_init_mean <- matrix(NA, ncol = ncol(choiceret), nrow = 1)
colnames(trial_init_vol) <- colnames(choiceret)
colnames(trial_init_mean) <- colnames(choiceret)
# prepare two empty matrix for the trial

for (i in 1:ncol(choiceret)){
  trial_init_vol[1,i] <- var(choiceret[1:(minwin+1),i])
  trial_init_mean[1,i] <- mean(choiceret[1:(minwin+1),i])
}
# run a loop to calculate the mean & volatility separately for each stock
trial_init_weight <- exposure *(trial_init_mean/trial_init_vol)
ret_bh <- matrix(NA, ncol = 1, nrow = nexpwin)
rownames(ret_bh) <- rownames(tail(choiceret,nexpwin))
colnames(ret_bh) <- "return"
# Trial - Step - One: create a return matrix for buy & hold portfolio
for(i in 1:nexpwin){
  bhr <- trial_init_weight * choiceret[i+(minwin+1),]
  ret_bh[i] <- sum(bhr)
}
bh_vol <- var(ret_bh)
# Above is the calculation of buy and hold portfolio volatility


# Try to build a function to calculate individual mean & return

VolatilityManagement_vm <- function(data,minwin,step.no){
  function_v <<- matrix(NA, ncol = ncol(data), nrow = 1)
  function_m <<- matrix(NA, ncol = ncol(data), nrow = 1)
  for(i in 1: ncol(data)){
    function_v[1,i] <<- var(data[1:(minwin+step.no),i])
    function_m[1,i] <<- mean(data[1:(minwin+step.no),i])
  }
}

# Try to build a function to calculate individual mean & return

weight_trial <- matrix(NA, ncol = ncol(choiceret), nrow = nexpwin)
ret_trial <- matrix(NA, ncol = 1, nrow = nexpwin)


for(i in 1:nexpwin){
  VolatilityManagement_vm(data = choiceret, minwin = minwin, step.no = i)
  weight_loop <- exposure * (function_m/function_v)
  weight_trial[i,] <- exposure * (function_m/function_v)
  ret_trial[i] <-  sum(weight_loop * choiceret[i+(minwin+1),])
}