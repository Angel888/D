vm_index <-  tq_index("sp500")

x1 <- grep("BRK.B", vm_index$symbol)
vm_index$symbol[x1] <- "BRK-B"
x2 <- grep("BF.B", vm_index$symbol)
vm_index$symbol[x2] <- "BF-B"

vm_index <- vm_index %>%
  tq_get(get = "stock.prices",
         complete_cases = TRUE,
         from = "2009-01-01",
         to = "2018-12-31") %>%
  filter(symbol %in% random_stock_names) %>%
  select(symbol, date, adjusted,close, shares_held) %>%
  mutate(mktcap = close * shares_held)

# Step 1: Construct the matrix for daily return

vm_daily_price <- vm_index %>%
  acast(date ~ symbol, value.var = "adjusted")

vm_daily_return_log <- diff(log(vm_daily_price))

# Step 2: Construct Market Capital Factor in Daily frequency
# The following code gives market capital of each stock at beginning of each month
# Therefore, the market-capital factor rebalances at beginning of each month


vm_mktcap_daily <- vm_index %>%
  group_by(symbol, year(date), month(date)) %>%
  summarise(mktcap = head(mktcap,1),
            date = head(date,1))

vm_mktcap_daily <- vm_mktcap_daily %>%
  acast(date ~ symbol, value.var = "mktcap")

# Step 3: Sacle the Market Capital factor Weights for each month
# 
vm_mktcap_scale_d <- matrix(NA, nrow = nrow(vm_mktcap_daily), ncol = ncol(vm_mktcap_daily))
rownames(vm_mktcap_scale_d) <- rownames(vm_mktcap_daily)
colnames(vm_mktcap_scale_d) <- colnames(vm_mktcap_daily)

for (i in 1:nrow(vm_mktcap_daily)) {
  vm_mktcap_scale_d[i,] <- vm_mktcap_daily[i,] /sum(vm_mktcap_daily[i,])
}

# Step 4: Calculate Daily Return for market

mktcap_d_r <- matrix(NA, nrow = nrow(vm_daily_return_log), ncol = 1)
rownames(mktcap_d_r) <- rownames(vm_daily_return_log)
colnames(mktcap_d_r) <- "mktcap_daily_return"

# idea behind this loop:
# vm_mktcap_scale_d stores the weight of each stock at the beginning of each month
# for every date, the for-loop looks for the same year and same month
# Then calculate the daily return through multiplying daiy return and the weight of that month


for (i in 1:nrow(vm_daily_return_log)) {
  for (j in 1:nrow(vm_mktcap_scale_d)) {
    if(year(rownames(vm_daily_return_log)[i]) == year(rownames(vm_mktcap_scale_d)[j]) && 
       month(rownames(vm_daily_return_log)[i]) == month(rownames(vm_mktcap_scale_d)[j]))
      r <- vm_mktcap_scale_d[j,] * vm_daily_return_log[i,]
  }
  mktcap_d_r[i] <- sum(r)
}

# Step 5: turn daily_return series into tibble data frame
# This makes it easier for calculation of monthly variance

mktcap_d_r_tb <- tk_tbl(mktcap_d_r)



# Step 6: calculate the variance for each month
mktcap_var <- mktcap_d_r_tb %>%
  group_by(year(index), month(index)) %>%
  summarise(variance = var(mktcap_daily_return),
            date = tail(index,1)) %>% 
  # Save date at end of each month for consistency with other calculations
  ungroup() %>%
  select(date, variance)

# Then reduce it to 119 obervations and make it a matrix for later use

mktcap_var <- as.matrix(mktcap_var$variance[-1])
colnames(mktcap_var) <- "monthly_variance"
rownames(mktcap_var) <- rownames(random_stock_return)




# Step 7: calculate the monthly return for market capital factor

# First calculate the monthly market capital
# We save the market capital data at the end of each month 
# Such that the data would be lagged for monthly return


vm_mktcap_monthly <- vm_index %>%
  group_by(symbol, year(date), month(date)) %>%
  summarise(mktcap = tail(mktcap,1),
            date = tail(date,1))

vm_mktcap_monthly <- vm_mktcap_monthly %>%
  acast(date ~ symbol, value.var = "mktcap")

# Then monthly market capital weights

vm_mktcap_scale_m <- matrix(NA, nrow = nrow(vm_mktcap_monthly), ncol = ncol(vm_mktcap_monthly))
rownames(vm_mktcap_scale_m) <- rownames(vm_mktcap_monthly)
colnames(vm_mktcap_scale_m) <- colnames(vm_mktcap_monthly)

# Scaled Monthly Market Capital Weight

for (i in 1:nrow(vm_mktcap_monthly)) {
  vm_mktcap_scale_m[i,] <- vm_mktcap_monthly[i,] /sum(vm_mktcap_monthly[i,])
}

# Now monthly Price

vm_monthly_price <- vm_index %>%
  group_by(symbol,year(date),month(date))%>%
  summarise(date = tail(date,1),
            adjusted = tail(adjusted,1)) %>%
  acast( date ~ symbol, value.var = "adjusted" )

vm_monthly_return_log <- diff(log(vm_monthly_price))

# Step 8: Calculate the monthly return for market capital factor

# This means at the beginning of each month, market capital portfolio rebalanced
# And calculate the return at end of each month
# There might be minor errors, but that should be acceptable
mktcap_m_r <- matrix(NA, nrow = nrow(random_stock_return), ncol = 1)

for (i in 1:nrow(mktcap_var)){
  r_loop <- vm_monthly_return_log[i,] * vm_mktcap_scale_m[i,]
  sr_loop <- sum(r_loop)
  mktcap_m_r[i] <- sr_loop
}

rownames(mktcap_m_r) <- rownames(random_stock_return)
colnames(mktcap_m_r) <- "mktcap_monthly_return"

# In the paper of Moreira and Muir, they adjusted factor return for risk-free rate
# However, this exercise would not do so
# Such that the return would be consistent with other portfolios

# Step 9: perform a trial to align the variance of 
# volatility managed portfolio and buy-and-hold portfolio

# Specify a trial-exposure level

trial_exposure <- 0.05

trial_weight <- matrix(NA, nrow = nexpwin, ncol = 1)
trial_return <- matrix(NA, nrow = nexpwin, ncol = 1)

# Use a loop to do the calculation

for (i in 1:nexpwin){
  mean_loop <- mean( head(mktcap_m_r , (minwin + i))  )
  # Calculate conditional expectation of market-capital factor return
  variance_loop <- mktcap_var[(minwin + i)]
  # extract the 1-period lagged realized variance of market-capital factor
  weight_loop <- trial_exposure * ( mean_loop / variance_loop )
  trial_weight[i] <- weight_loop
  trial_return[i] <- weight_loop * mktcap_m_r[(1 + minwin + i)]
  # calculate one-period ahead return
}

# now calculate the exposure level

v1 <- var(tail(mktcap_m_r, nexpwin))
# Calculate the unconditional variance for buy-and-hold portfolio
v2 <- var(trial_return)
# calculate the unconditional variance for trial volatility managed portfolio
exposure <- as.numeric(sqrt(v1/v2) * trial_exposure)

# Step 10: calculate the return of Volatility Managed Portfolio

weight_VM <- matrix(NA, nrow = nexpwin, ncol = 1)
rownames(weight_VM) <- rownames(tail(random_stock_return , nexpwin)  )
colnames(weight_VM) <- "VM_weight"

ret_VM <- matrix(NA, nrow = nexpwin, ncol = 1)

rownames(ret_VM) <- rownames(tail(random_stock_return , nexpwin)  )
colnames(ret_VM) <- "VM_return"


for (i in 1:nexpwin){
  mean_loop <- mean( head(mktcap_m_r , (minwin + i))  )
  # Calculate conditional expectation of market-capital factor return
  variance_loop <- mktcap_var[(minwin + i)]
  # extract the 1-period lagged realized variance of market-capital factor
  weight_loop <-  exposure * ( mean_loop / variance_loop )
  weight_VM[i] <- weight_loop
  ret_VM[i] <- weight_loop * mktcap_m_r[(1 + minwin + i)]
  # calculate one-period ahead return
}

# Now turn return matrix of Volatility Managed Portfolio into better format

ret_VM <- as.xts(ret_VM)
ret_VM_acc <- cumsum(ret_VM)
colnames(ret_VM_acc) <- "VM_acc_return"

# Construct the return series for market-capital factor

mktcap_m_r <- as.xts(mktcap_m_r)
ret_MKT <- tail(mktcap_m_r, nexpwin)
ret_MKT_acc <- cumsum( tail(mktcap_m_r, nexpwin) )

# Calculate some results of Volatility Managed Portfolio

VM_mean <- mean(ret_VM)
VM_sd <- sd(ret_VM)
VM_median <- median(ret_VM)

MKT_mean <- mean(ret_MKT)
MKT_sd <- sd(ret_MKT)
MKT_median <- median(ret_MKT)

vm_p1 <- 
  highchart(type = "stock") %>%
  hc_add_series(ret_VM_acc, name = "Volatility Managed Portfolio") %>%
  hc_add_series(ret_MKT_acc, name = "Market-Capital Weighted Portfolio") %>%
  hc_title(text = "Volatility Managed Portfolio")%>%
  hc_legend(enabled = TRUE)

# Plot the weight of Volatility Managed Portfolio

weight_VM <- as.xts(weight_VM)

vm_weight_p1 <- 
  highchart(type = "stock") %>%
  hc_add_series(weight_VM) %>%
  hc_title(text = "Weight of Volatility Managed Portfolio")