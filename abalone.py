'''
Created on Feb 11, 2016

@author: Thilini
Description : Analysis of abalone dataset, predict rings based on the characteristics and 
                measurements given. Used different modeling techniques.
                

'''
##Load libraries

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.linear_model import LinearRegression
from sklearn import metrics, linear_model
from sklearn.feature_selection import RFE

##Load the data set

abalone = pd.read_csv('C:/Users/Toshiba/Google Drive/python/datasets/abalone.csv')

##Data manipulation
#print(abalone.shape)
##remove missing values for heights (denoted by 0 s)
abalone = abalone[abalone['Height'] > 0]
#print(abalone.shape)

##Dummies for Sex
abalone['male'] = np.where(abalone['Sex']=='M',1,0)
abalone['female'] = np.where(abalone['Sex']=='F',1,0)

#print(abalone.head())

abalone_new = abalone[['Rings', 'Length' , 'Diameter' , 'Height', 'Whole weight', 'Shucked weight', 'Viscera weight', 'Shell weight', 'male', 'female']]

#abalone_new = abalone.drop('Sex', 1)
#print(abalone_new.head())
#print(abalone_new.shape)

i_range = range(1,21)

##Store all the MSPEs
summary = pd.DataFrame(index=range(1,21), columns=['OLS','OLS-RFE','LASSO','Ridge'])
summary = summary.fillna(0) #
    

##generate uniform random numbers


for i in i_range:
    random_unif = np.random.uniform(size=abalone_new.shape[0]) 
    abalone_new['Set'] = np.where(random_unif <= 0.75, 1, 2)
    training = abalone_new[abalone_new['Set'] == 1]
    test = abalone_new[abalone_new['Set'] == 2]
    
    X_train = training.drop(['Rings','Set'],1)
    X_test = test.drop(['Rings','Set'],1)


    y_train = training['Rings']
    y_test = test['Rings']
 

    ##1. Linear regression- full X
  
    linreg = LinearRegression()
    linreg.fit(X_train, y_train)
    y_pred = linreg.predict(X_test)
    summary.ix[i,0]= metrics.mean_squared_error(y_test,y_pred)
   
    ##2. Linear regression - rfe
    
    select_x = RFE(linreg,6,step=1)
    subset_reg = select_x.fit(X_train, y_train)
    y_pred = select_x.predict(X_test)
    summary.ix[i,1]= metrics.mean_squared_error(y_test,y_pred)
    
    ###3. LASSO
    
    las = linear_model.Lasso(alpha=0.005, normalize=True)
    las.fit(X_train,y_train)
    y_pred = las.predict(X_test)
    summary.ix[i,2]= metrics.mean_squared_error(y_test,y_pred)
    
    
    ##4. ridge
    
    ridge = linear_model.Ridge(alpha=0.01,normalize=True)
    ridge.fit(X_train,y_train)
    y_pred = ridge.predict(X_test)
    summary.ix[i,3]= metrics.mean_squared_error(y_test,y_pred)
   

#print(summary.tail())
rmspe = np.sqrt(summary)


##DRaw boxplots
sns.boxplot(rmspe[['OLS','OLS-RFE','LASSO','Ridge']])
plt.show()
