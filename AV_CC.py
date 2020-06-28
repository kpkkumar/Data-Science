# -*- coding: utf-8 -*-
"""
Created on Sun Mar 22 10:35:19 2020

@author: inspiron
"""

import numpy as np
import pandas as pd
import matplotlib as plt
import seaborn as sns

data = pd.read_csv("C:\\Users\\inspiron\\Downloads\\train_20D8GL3.csv")
traindata = pd.DataFrame(data)
traindata.dtypes
traindata= traindata.astype({'SEX' : 'category'})


traindata['EDUCATION'] = traindata['EDUCATION'].replace({6 : 5})

traindata['EDUCATION'].describe()

traindata['EDUCATION'] = traindata['EDUCATION'].replace({0 : 2})
traindata = traindata.astype({'EDUCATION' : 'category'})
traindata["EDUCATION"].dtypes
traindata['EDUCATION'].describe()

traindata = traindata.astype({'MARRIAGE' : 'category'})
traindata = traindata.astype({'PAY_0' : 'category'})
traindata = traindata.astype({'PAY_2' : 'category'})
traindata = traindata.astype({'PAY_3' : 'category'})
traindata = traindata.astype({'PAY_4' : 'category'})
traindata = traindata.astype({'PAY_5' : 'category'})
traindata = traindata.astype({'PAY_6' : 'category'})
traindata = traindata.astype({'default_payment_next_month' : 'category'})



traindata.dtypes

traindata.head(6)
traindata.isnull().sum()

traindata = traindata.drop('ID',axis=1)

corr = traindata.corr()

traindata = traindata.drop(['BILL_AMT2'], axis=1)
corr = traindata.corr()
traindata = traindata.drop(['BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'], axis=1)
corr = traindata.corr()

traindata = traindata.drop(['PAY_4'],axis=1)
corr = traindata.corr()
traindata = traindata.drop(['PAY_5'],axis=1)
corr = traindata.corr()
traindata = traindata.drop(['PAY_3'],axis=1)
corr = traindata.corr()

traindata.describe()
sns.pairplot(traindata)

sns.boxplot(x=traindata['LIMIT_BAL'])
traindata= traindata[(traindata.LIMIT_BAL <= 520000)]

sns.boxplot(x=traindata['AGE'])
traindata= traindata[(traindata.AGE <= 60)]

sns.countplot(x='default_payment_next_month', data = traindata)
traindata.default_payment_next_month.value_counts()

traindata.dtypes


sns.pairplot(traindata)


xnum = traindata.select_dtypes(include=['int64'])

from sklearn.preprocessing import StandardScaler
scal=StandardScaler()
xnum = pd.DataFrame(scal.fit_transform(xnum),columns = xnum.columns)
xnum.head(5)
xncol = xnum.columns
xcat=traindata.drop(xncol,axis=1)
xccol = xcat.columns
xccol
xncol

numdata = xnum
catdata = xcat

train = pd.concat([catdata,numdata],axis=1, join='inner')
train.default_payment_next_month.value_counts()

from sklearn.utils import resample

df_majority = train[train.default_payment_next_month==0]
df_minority = train[train.default_payment_next_month==1]


df_minority_upsampled = resample(df_minority, replace=True, n_samples=15885, random_state=123)
df_upsampled = pd.concat([df_majority, df_minority_upsampled])
df_upsampled.default_payment_next_month.value_counts()



ytrain=df_upsampled.default_payment_next_month
ytrain=pd.DataFrame(ytrain).astype("category")

train=df_upsampled.drop('default_payment_next_month', axis =1)

from sklearn.linear_model import LogisticRegression
model = LogisticRegression()
model.fit(train,ytrain)

testdata = pd.read_csv("C:\\Users\\inspiron\\Downloads\\test_O6kKpvt.csv")
testdata = testdata.drop(['ID','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6', 'PAY_4','PAY_5','PAY_3'], axis = 1)

predictions = model.predict(testdata)
predictions = pd.DataFrame(predictions)
predictions.to_csv(r'C:\\Users\\inspiron\\Downloads\\submission.csv')

