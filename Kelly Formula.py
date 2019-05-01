import math
import numpy as np
import pandas as pd
import talib
# 在这个方法中编写任何的初始化逻辑。context对象将会在你的算法策略的任何方法之间做传递。
def init(context):
    context.benchmark='中信证券'
    context.s1 = "中信证券"
    context.days = 250
    context.period = 3
    update_universe(context.s1)
    scheduler.run_weekly(get_historyret,weekday=1)
    scheduler.run_weekly(trading,weekday=1)
# 你选择的证券的数据更新将会触发此段逻辑，例如日或分钟历史数据切片或者是实时数据切片更新
def get_historyret(context,bar_dict):
    #获取基本面数据
    prices = history(context.days*context.period,'1d','close')[context.s1].values
    context.max_price = np.max(prices[-250:-1])
    context.min_price = np.min(prices[-250:-1])
    context.current_price = prices[-1]
def trading(context, bar_dict):  
    b1 = context.max_price/bar_dict[context.s1].close - 1
    c1 = 1 - context.min_price/bar_dict[context.s1].close
    kelly = (0.5*b1-0.5*c1)/(b1*c1)
    if kelly > 1:
        kelly = 1
    if kelly < 0:
        kelly = 0
    if bar_dict[context.s1].close > context.max_price:
        kelly = 1
    if bar_dict[context.s1].close < context.min_price:
        kelly = 0
    #logger.info(kelly)
    if bar_dict[context.s1].is_trading:
        order_target_percent(context.s1,kelly*0.98)