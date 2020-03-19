
RANDOM_STATE = 547

import numpy as np
np.random.seed(RANDOM_STATE)

import os
cwd = os.getcwd()

import sys
sys.path.insert(0, cwd + "/Lasso/")
from lasso import Lasso

import pickle

# Assigns each of 'n' objects to one of 'k' folds
def create_k_folds(n, k):
    assignments = np.zeros((n))
    i = 0
    for i in range(n):
        assignments[i] = i % k
    assignments = assignments[np.random.permutation(n)]
    return assignments

# X_test/y_test should be the elements of X/y for which 'assignments' is equal to 'k'
# X_train/ytrain should be the remaining elements of X/y
# Return X_train, X_test, y_train, y_test (in that order)
def get_fold(X, y, assignments, k):
    X_train = []
    X_test = []
    y_train = []
    y_test = []
    # Implement your method here
    for i in range(len(X)):
        if assignments[i] == k:
            X_test+=[np.array(X[i])]
            y_test+=[np.array(y[i])]
        else:
            X_train+=[np.array(X[i])]
            y_train+=[np.array(y[i])]
    return np.array(X_train),np.array(X_test),np.array(y_train),np.array(y_test)

# Fit a Lasso() model on the training data using regularization strength 'alpha'
# Return that model's mean squared error (MSE) on the testing data
def evaluate(X_train, X_test, y_train, y_test, alpha):
    # Implement your method here
    model = Lasso(alpha=alpha)
    model.fit(X_train,y_train)
    MSEsum = 0
    y_pred = model.predict(X_test)
    for i in range(len(y_test)):
        MSEsum+=(y_pred[i]-y_test[i])**2
    return MSEsum/len(y_test)

# Return the cross validation (CV) estimate of the MSE of a Lasso() model with regularization strength 'alpha'
# Note:  most of the work should be done by 'get_fold()' and 'evaluate()'
def evaluate_alpha_by_cv(X, y, assignments, alpha):
    k = np.int(np.max(assignments) + 1)
    # Implement your method here
    MSEsum = 0
    for i in range(k):
        X_train, X_test, y_train, y_test = get_fold(X,y,assignments,i)
        MSEsum = MSEsum+evaluate(X_train,X_test,y_train,y_test,alpha)
    CVest = MSEsum/k
    return CVest

# Return in this order:
# -  an array containing the CV estimate of the MSE of a Lasso() model for each value in 'alphas'
# -  the alpha value chosen by CV
# Note:  most of the work should be done by 'evaluate_alpha_by_cv()'
def evaluate_alphas_by_cv(X, y, k, alphas):
    assignments = create_k_folds(X.shape[0], k)
    minerror = evaluate_alpha_by_cv(X,y,assignments,alphas[0])
    minalpha = alphas[0]
    errors = [minerror]
    for alpha in alphas[1:]:
        error = evaluate_alpha_by_cv(X,y,assignments,alpha)
        if(error<minerror):
            minerror = error
            minalpha = alpha
        errors = errors + [error]
    return np.array(errors),minalpha
    # Implement your method here

# Return in this order:
# -  an array containing the CV estimate of the MSE of a Lasso() model for each value in 'alphas'
# -  the alpha value chosen by CV
# -  the testing MSE of the Lasso() model with the chosen value of alpha
# Note:  most of the work should be done by 'evaluate_alphas_by_cv()' and 'evaluate()'
def evaluate_final_model(X, y, k, alphas):
    X_train = X[:180, ]
    y_train = y[:180]
    X_test = X[180:, ]
    y_test = y[180:]
    
    # Implement your method here
    # Step 1:  Choose Alpha
    # Step 2:  Evaluate Chosen Alpha
    cvest, alphaalpha = evaluate_alphas_by_cv(X_train,y_train,k,alphas)
    mse = evaluate(X_train,X_test,y_train,y_test,alphaalpha)
    return cvest,alphaalpha,mse
    pass

if __name__ == "__main__":
    with open("data.pkl", "rb") as f:
        data = pickle.load(f)
    X = data["X"]
    y = data["y"]
    k = 5
    alphas = [0.00001, 0.0001, 0.001, 0.01, 0.1, 1.0]
    print(evaluate_final_model(X, y, k, alphas))
