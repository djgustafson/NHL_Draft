#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 29 11:13:00 2019

@author: danielgustafson
"""

#%% Packages
import pandas as pd
import numpy as np
from sklearn import preprocessing

from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor

from sklearn.model_selection import GridSearchCV

from sklearn.metrics import mean_squared_error as mse

#%% Data
d = pd.read_csv("/Volumes/Samsung USB/NHL/NHL Draft Prediction/NHL_Draft_05_19.csv")
dat_players = d

#%%
dat_players = d.loc[d['ss'] == 2014, :]

players = dat_players.iloc[:,0]

#%% Remove names
d = d.iloc[:,1:]

#%% Convert categorical to dummies
d = pd.get_dummies(d)

#%% Drop some vars
d = d.drop(['draft_year', 'id'], axis = 1)

#%% Separate into train and test
train = d.loc[d['ss'] < 2014, :]

test = d.loc[d['ss'] == 2014, :]

#%% Separate into X and y
X_train = train.drop('war', axis = 1)
X_test = test.drop('war', axis = 1)

y_train = train.loc[:,'war']
y_test = test.loc[:,'war']


#%% Scale data
# Get column names first
names = list(X_train)

# Create the Scaler object
scaler = preprocessing.StandardScaler()

# Fit your data on the scaler object
X_train_scaled = scaler.fit_transform(X_train)
X_train_scaled = pd.DataFrame(X_train, columns=names)

X_test_scaled = scaler.fit_transform(X_test)
X_test_scaled = pd.DataFrame(X_test, columns=names)

#%% RF
rf = RandomForestRegressor(n_estimators=500, max_depth = 8,
                           min_samples_leaf = .005,
                           random_state=2309, n_jobs=-1)

rf.fit(X_train, y_train)

y_pred_rf = rf.predict(X_test)

rf_mse = mse(y_test, y_pred_rf)

# Predictions

players = players.reset_index(drop = True)
preds = pd.concat([players, pd.Series(y_pred_rf)], axis=1)
preds.columns = ['name','pred']

#%% GBM
gbm = GradientBoostingRegressor(max_depth=10, n_estimators=1000, random_state=2309)

gbm.fit(X_train, y_train)

y_pred_gbm = gbm.predict(X_test)

gbm_mse = mse(y_test, y_pred_gbm)

# Predictions

players = players.reset_index(drop = True)
preds = pd.concat([players, pd.Series(y_pred_gbm)], axis=1)
preds.columns = ['name','pred']

#%%
params_cv = {'n_estimators': [1000,1500,500],
             'max_depth': [4,8,12],
             'min_samples_leaf': [.005, .0075, .01]
             }

grid_cv = GridSearchCV(estimator=rf,
                       param_grid=params_cv,
                       scoring='neg_mean_squared_error',
                       cv=5,
                       verbose=2,
                       n_jobs=-1)

grid_cv.fit(X_train, y_train)


best_hyperparams = grid_cv.best_params_

print('Best hyerparameters:\n', best_hyperparams)

 #%% Look at preds
players = players.reset_index(drop = True)
preds = pd.concat([players, pd.Series(y_pred_rf)], axis=1)
preds.columns = ['name','pred']

#%%
var_imp=pd.DataFrame([np.array(names), rf.feature_importances_]).T

var_imp.columns = ['feature', 'importance']

var_imp = var_imp.sort_values('importance', ascending = False)

var_imp[:20]

#%%

