# Option pricing with Python

# Basically get this from google hahahahahaha!
from math import exp, log, pi

def norm_pdf(x):
    """
    Standard normal probability density function
    """
    return (1.0/((2*pi)**0.5))*exp(-0.5*x*x)

def norm_cdf(x):
    """
    An approximation to the cumulative distribution
    function for the standard normal distribution:
    N(x) = \frac{1}{sqrt(2*\pi)} \int^x_{-\infty} e^{-\frac{1}{2}s^2} ds
    """
    k = 1.0/(1.0+0.2316419*x)
    k_sum = k * (0.319381530 + k * (-0.356563782 + \
        k * (1.781477937 + k * (-1.821255978 + 1.330274429 * k))))

    if x >= 0.0:
        return (1.0 - (1.0 / ((2 * pi)**0.5)) * exp(-0.5 * x * x) * k_sum)
    else:
        return 1.0 - norm_cdf(-x)
    
# Above is merely a series of ... Preparations I think
# In particlular, preparations of statistical tools
        
# Below is the calculation function of European Call & Put prices
        
from math import exp, log, sqrt
from statistics import norm_pdf, norm_cfd

def d_j(j, S, K, r, v, T):
    """
    d_j = \frac{log(\frac{S}{K})+(r+(-1)^{j-1} \frac{1}{2}v^2)T}{v sqrt(T)}
    """
    return (log(S/K) + (r + ((-1)**(j-1))*0.5*v*v)*T)/(v*(T**0.5))

def vanilla_call_price(S, K, r, v, T):
    """
    Price of a European call option struck at K, with
    spot S, constant rate r, constant vol v (over the
    life of the option) and time to maturity T
    """
    return S * norm_cdf(d_j(1, S, K, r, v, T)) - \
        K*exp(-r*T) * norm_cdf(d_j(2, S, K, r, v, T))
        
def vanilla_put_price(S, K, r, v, T):
    """
    Price of a European put option struck at K, with
    spot S, constant rate r, constant vol v (over the
    life of the option) and time to maturity T
    """
    return -S * norm_cdf(-d_j(1, S, K, r, v, T)) + \
        K*exp(-r*T) * norm_cdf(-d_j(2, S, K, r, v, T))

