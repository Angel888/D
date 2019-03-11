library(quadprog)

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


#---------------------------Compare two methods----------------------


library(PortfolioAnalytics)
library(foreach)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
# A freakingly amount of libraries to load
mvp <- portfolio.spec(choicenames)
fi_constr <- weight_sum_constraint(type = "full_investment")
mvp_constr <- list(fi_constr)
# full investment constraint and the function only read "list objects"
var_obj <- return_objective(name="var")
mvp_obj <- list(var_obj)
# varian objective and the function only read "list objects"
mvp_bt <- optimize.portfolio.rebalancing( R=choiceret,
                                          portfolio=mvp,
                                          constraints=mvp_constr,
                                          objectives=mvp_obj,
                                          optimize_method="ROI",
                                          rebalance_on="months",
                                          training_period=60)
# code from class3 handouts and class four
mvp_weight <- extractWeights(mvp_bt)
mvp_weight_matrix <- as.matrix(mvp_weight)
choiceret_tail <- tail(choiceret,60)
mvp_ret <- rowSums(mvp_weight_matrix * choiceret_tail)
mvp_acc_ret <- as.xts(mvp_acc_ret)
mvp_acc_ret <- cumsum(mvp_ret)
colnames(mvp_acc_ret) <- "return"
# plot the return series
highchart(type = "stock") %>%
  hc_title(text = "Cumulative Returns of MVP") %>%
  hc_add_series(mvp_acc_ret$return, name = "MVP Back Test") %>%
  hc_add_series(choice_bt_accret$return, names = "Tangency Portfolio")
# Above function would plot the return series in Highcharter Environment
mvp_mean <- mean(mvp_ret)
mvp_median <- median(mvp_ret)
mvp_sd <- sd(mvp_ret)
mvp_drawdonw <- maxDrawdown(mvp_ret)
# Above is the simple calculation of some statistics
mvp_ret_reduced <- mvp_ret[-1]

#-------------------------------------------------------------------
library(highcharter)
highchart(type = "stock") %>%
  hc_add_series(mvp_acc_ret, name = "method2") %>%
  hc_add_series(ret_acc_MVP, name = "method1")

