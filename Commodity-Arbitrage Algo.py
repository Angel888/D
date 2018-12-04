
import numpy as np


# to define a new and adaptive function
def init(context):
    context.s1 = 'AG1612' # just an example
    context.s2 = 'AU1612' # and nother example commodity

    # 
    context.counter = 0

    # rolling window
    context.window = 60

    # set the posiiton of hedging
    context.ratio = 15

    context.up_cross_up_limit = False
    context.down_cross_down_limit = False

    # criterial for buying
    context.entry_score = 2

    # initialize marekt information, would refresh continuously
    subscribe([context.s1, context.s2])


# before_trading
def before_trading(context):

    context.counter = 0


# Update data of chosen commodity
def handle_bar(context, bar_dict):

    # get position data
    position_a = context.portfolio.positions[context.s1]
    position_b = context.portfolio.positions[context.s2]

    context.counter += 1
    # logic for trading
    if context.counter > context.window:

        # get price lists
        price_array_a = history_bars(context.s1, context.window, '1m', 'close')
        price_array_b = history_bars(context.s2, context.window, '1m', 'close')

        # calculate the statistics for price lists
        spread_array = price_array_a - context.ratio * price_array_b
        std = np.std(spread_array)
        mean = np.mean(spread_array)
        up_limit = mean + context.entry_score * std
        down_limit = mean - context.entry_score * std

        # calculate difference in prices
        price_a = bar_dict[context.s1].close
        price_b = bar_dict[context.s2].close
        spread = price_a - context.ratio * price_b

        # buy-signal
        if spread <= down_limit and not context.down_cross_down_limit:
    
            logger.info('spread: {}, mean: {}, down_limit: {}'.format(spread, mean, down_limit))
   


            qty_a = 1 - position_a.buy_quantity
            qty_b = context.ratio - position_b.sell_quantity

       
            if qty_a > 0:
                buy_open(context.s1, qty_a)
            if qty_b > 0:
                sell_open(context.s2, qty_b)
            if qty_a == 0 and qty_b == 0:
 
                context.down_cross_down_limit = True
     


        if spread >= mean and context.down_cross_down_limit:
            logger.info('spread: {}, mean: {}, down_limit: {}'.format(spread, mean, down_limit))


     
            qty_a = position_a.buy_quantity
            qty_b = position_b.sell_quantity
            if qty_a > 0:
                sell_close(context.s1, qty_a)
            if qty_b > 0:
                buy_close(context.s2, qty_b)
            if qty_a == 0 and qty_b == 0:
                context.down_cross_down_limit = False
  

        # sell-operation
        if spread >= up_limit and not context.up_cross_up_limit:
            logger.info('spread: {}, mean: {}, up_limit: {}'.format(spread, mean, up_limit))
 
            qty_a = 1 - position_a.sell_quantity
            qty_b = context.ratio - position_b.buy_quantity
            if qty_a > 0:
                sell_open(context.s1, qty_a)
            if qty_b > 0:
                buy_open(context.s2, qty_b)
            if qty_a == 0 and qty_b == 0:
                context.up_cross_up_limit = True


        # clear position-signla
        if spread < mean and context.up_cross_up_limit:
            logger.info('spread: {}, mean: {}, up_limit: {}'.format(spread, mean, up_limit))
       
            qty_a = position_a.sell_quantity
            qty_b = position_b.buy_quantity
            if qty_a > 0:
                buy_close(context.s1, qty_a)
            if qty_b > 0:
                sell_close(context.s2, qty_b)
            if qty_a == 0 and qty_b == 0:
                context.up_cross_up_limit = False

