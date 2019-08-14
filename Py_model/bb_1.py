""" This is a program that tests how likely a short-term study is to miss the long-term trend.

We are testing for the likelihood of type 2 errors produced by studies of various lengths.
"""
import pandas as pd

test = pd.read_csv('test', header = None) "!Doesn't see file!"

def standardize(data):
    pd.data.columns("year", "response")
    response = pd.data['response']
    meanResp = pd.response.mean()
    stdResp = pd.response.std()
    standResp = ((response - meanResp)/stdResp)
    standRespList = pd.standResp.tolist()
    pd.data.insert(2, "stand.response", standRespList, True)
    return(data)

test1 = standardize(test)
print(test1)
