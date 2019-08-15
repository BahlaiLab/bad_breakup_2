""" This is a program that tests how likely a short-term study is to miss the long-term trend.

We are testing for the likelihood of type 2 errors produced by studies of various lengths.
"""
import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats

test = pd.read_csv('github/test.csv') #Need to find correct file-path

def standardize(data):
    pd.data.columns("year", "response")
    response = pd.data['response']
    meanResp = pd.response.mean()
    stdResp = pd.response.std()
    respList = pd.response.tolist()
    standRespList = [((x - meanResp)/stdResp) for x in respList]
    pd.data.insert(2, "stand.response", standRespList)
    return(data)

test1 = standardize(test)
print(test1)

def linefit(data):
    slope, intercept, r_value, p_value, std_err = stats.linregress(pd.data["year"], pd.data["stand.response"])
    output = np.array([min(pd.data['year']), len(pd.data.index(), slope, std_err, p_value, (r_value**2)]) #don't know how to access adjusted r^2
    ser = pd.Series(output)
    return output

print(linefit(test1))
