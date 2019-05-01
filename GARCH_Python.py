import cvxopt
from functools import partial
import math
import numpy as np
import scipy
from scipy import stats
import statsmodels as sm
from statsmodels import regression
from statsmodels.stats.stattools import jarque_bera

import matplotlib.pyplot as plt

# 参数初始化
a0 = 1.0
a1 = 0.1
b1 = 0.8
sigma1 = math.sqrt(a0 / (1 - a1 - b1))

def simulate_GARCH(T, a0, a1, b1, sigma1):
    
    # 初始化
    X = np.ndarray(T)
    sigma = np.ndarray(T)
    sigma[0] = sigma1
    
    for t in range(1, T):
        # 产生下一个Xt
        X[t - 1] = sigma[t - 1] * np.random.normal(0, 1)
        # 产生下一个sigma_t
        sigma[t] = math.sqrt(a0 + b1 * sigma[t - 1]**2 + a1 * X[t - 1]**2)
        
    X[T - 1] = sigma[T - 1] * np.random.normal(0, 1)    
    
    return X, sigma

X, _ = simulate_GARCH(10000, a0, a1, b1, sigma1) #生成模拟序列
X = X[1000:] # Drop burn in
X = X / np.std(X) # 标准化X

def compare_tails_to_normal(X):
    # Define matrix to store comparisons
    A = np.zeros((2,4))
    for k in range(4):
        A[0, k] = len(X[X > (k + 1)]) / float(len(X)) # 估计X的尾部
        A[1, k] = 1 - stats.norm.cdf(k + 1) # 正态分布的尾部
    return A

compare_tails_to_normal(X)

plt.figure(figsize=(16, 9)) 
plt.hist(X, bins=50)
plt.xlabel('sigma')
plt.ylabel('observations');

# 一个正态分布的样本
X2 = np.random.normal(0, 1, 9000)
# 将两者存到一个矩阵中
both = np.matrix([X, X2])

# 绘制GARCH模型和正态分布的分布情况，注意蓝线是GARCH
plt.figure(figsize=(16, 9)) 
plt.plot(both.T, alpha=.7);
plt.axhline(X2.std(), color='yellow', linestyle='--')
plt.axhline(-X2.std(), color='yellow', linestyle='--')
plt.axhline(3*X2.std(), color='red', linestyle='--')
plt.axhline(-3*X2.std(), color='red', linestyle='--')
plt.xlabel('time')
plt.ylabel('sigma')
plt.legend(["GARCH","Normal"]);

