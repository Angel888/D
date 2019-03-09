library(PortfolioAnalytics)
library(foreach)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

mvp <- portfolio.spec(choicenames)
fi_constr <- weight_sum_constraint(type = "full_investment")
mvp_constr <- list(fi_constr)

var_obj <- return_objective(name="var")
mvp_obj <- list(var_obj)

mvp_bt <- optimize.portfolio.rebalancing( R=choiceret,
                                          portfolio=mvp,
                                          constraints=mvp_constr,
                                          objectives=mvp_obj,
                                          optimize_method="ROI",
                                          rebalance_on="months",
                                          training_period=60)

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
# Note that this mvp_ret started at 2014-1-13, a month earlier than choice_bt_ret
mvp_sharpe <- as.sr(mvp_ret_reduced,ope = 12,c0 = rfr2014)
# Above is to calculate the Sharpe Ratio of mvp_ret
tp_mvp_test <- sr_test(mvp_ret_reduced,
                       choice_bt_ret,
                       ope = 12,
                       paired = TRUE)
# Above is to test the difference between Tangency Portfolio and MVP