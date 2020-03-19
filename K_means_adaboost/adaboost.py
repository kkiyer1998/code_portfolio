from scipy.io import loadmat
import numpy as np
import math
# comment the import when you submit to autolab
# import matplotlib.pyplot as plt

def plot_func(X, y):
  import matplotlib.pyplot as plt
  for y_value in [-1, 1]:
    x_list = []
    for i in range(X.shape[0]):
      if y[i] == y_value:
        x_list += [X[i].tolist()]
    x_list = np.array(x_list)
    plt.plot(x_list[:, 0], x_list[:, 1], ["r.", "", "b."][y_value + 1])
  plt.show()

def load_data(filename):
  X_train = np.load("../Data/{:s}_X_train.npy".format(filename))
  y_train = np.load("../Data/{:s}_y_train.npy".format(filename))
  X_test = np.load("../Data/{:s}_X_test.npy".format(filename))
  y_test = np.load("../Data/{:s}_y_test.npy".format(filename))
  return X_train, y_train, X_test, y_test

def one(y, yprime):
  if y != yprime:
    return 1
  return 0

def find_hyp(X_train, y_train, Dt):
  feature = 0 # 0 if x_1 and 1 if x_2
  pospos = 0 # position of stump for +1 coeff
  posneg = 0 # position of stump for -1coeff
  coeff = 1 # Coefficient of output
  minerror = 1
  minpos = 0
  for i in range(len(X_train)):
    pos1 = X_train[i][0]
    pos2 = X_train[i][1]
    x = X_train.transpose()
    x1 = np.where(x[0]>=pos1, 1, -1)
    x2 = np.where(x[0]>=pos1, -1, 1)
    x3 = np.where(x[1]>=pos2, 1, -1)
    x4 = np.where(x[1]>=pos2, -1, 1)
    error01 = np.sum(np.multiply(Dt,np.where(y_train!=x1,1,0)))
    error02 = np.sum(np.multiply(Dt,np.where(y_train!=x1,1,0)))
    error11 = np.sum(np.multiply(Dt,np.where(y_train!=x1,1,0)))
    error12 = np.sum(np.multiply(Dt,np.where(y_train!=x1,1,0)))
    
    if error01 < minerror:
      feature = 0
      coeff = 1
      minerror = error01
      minpos = pos1
    if error02 < minerror:
      feature = 0
      coeff = -1
      minerror = error02
      minpos = pos1
    if error11 < minerror:
      feature = 1
      coeff = 1
      minerror = error11
      minpos = pos2
    if error12 < minerror:
      feature = 1
      coeff = -1
      minerror = error12
      minpos = pos2


  return minpos, coeff, feature, minerror


  # start by finding min pos over x_1 and both -1 and +1 coeff
  # minpos = [0,0]
  # minposneg1 = [0,0]
  # minerror1 = [0,0]
  # minerrorneg1 = [0,0]
  # sortedx1 = np.array(sorted([(X_train[i][0],y_train[i],Dt[i]) for i in range(len(X_train))],key = lambda x : x[0]))
  # sortedx2 = np.array(sorted([(X_train[i][1],y_train[i],Dt[i]) for i in range(len(X_train))],key = lambda x : x[0]))
  # X_trains = np.dstack((sortedx1,sortedx2))
  # for j in range(2):
  #   minpos[j] = X_trains[0][0][j] - 1
  #   for i in range(len(X_trains)):
  #     minerror1[j] += X_trains[i][2][j]*one(X_trains[i][1][j], 1 if X_trains[i][0][j] >= minpos[j] else -1)
  #     minerrorneg1[j] += X_trains[i][2][j]*one(X_trains[i][1][j], -1 if X_trains[i][0][j] >= minpos[j] else 1)
  #   k = 0
  #   while k < len(X_trains) - 1:
  #     pos = (X_trains[k][0][j] + X_trains[k + 1][0][j]) / 2
  #     error1 = 0
  #     errorneg1 = 0
  #     for i in range(len(X_trains)):
  #       error1 += X_trains[i][2][j]*one(X_trains[i][1][j], 1 if X_trains[i][0][j] >= pos else -1)
  #       errorneg1 += X_trains[i][2][j]*one(X_trains[i][1][j], -1 if X_trains[i][0][j] >= pos else 1)
  #     if (error1 < minerror1[j]):
  #       minerror1[j] = error1
  #       minpos[j] = pos
  #     elif  (errorneg1 < minerrorneg1[j]):
  #       minerrorneg1[j] = errorneg1
  #       minposneg1[j] = pos
  #     k+=1
  #   pos = X_trains[-1][0][j]+1
  #   error1 = 0
  #   errorneg1 = 0
  #   for i in range(len(X_trains)):
  #     error1 += X_trains[i][2][j]*one(X_trains[i][1][j], 1 if X_trains[i][0][j] >= pos else -1)
  #     errorneg1 += X_trains[i][2][j]*one(X_trains[i][1][j], -1 if X_trains[i][0][j] >= pos else 1)
  #   if (error1 < minerror1[j]):
  #     minerror1[j] = error1
  #     minpos[j] = pos
  #   elif (errorneg1 < minerrorneg1[j]):
  #     minerrorneg1[j] = errorneg1
  #     minposneg1[j] = pos
  # smallest = min(minerror1+minerrorneg1)
  # if smallest in minerror1:
  #   pos = minpos[minerror1.index(smallest)]
  #   coeff = 1
  #   feature = minerror1.index(smallest)
  # else:
  #   pos = minposneg1[minerrorneg1.index(smallest)]
  #   coeff = -1
  #   feature = minerrorneg1.index(smallest)
  # return pos, coeff, feature, smallest

def newD(Dt, alpha_t, pred, X_train, y_train):
  Z = sum([Dt[i] * np.exp(-alpha_t * pred(X_train[i]) * y_train[i]) for i in range(len(Dt))])
  Dtplus1 = np.zeros_like(Dt)
  for j in range(len(Dt)):
    Dtplus1[j] = Dt[j]*np.exp(-alpha_t * y_train[j] * pred(X_train[j])) / Z
  return Dtplus1


def adaboost(X_train, y_train, X_test, num_iter):
  m = len(X_train)
  Dt = np.ones(m)*1/m
  h = [None]*num_iter
  alpha = np.ones(num_iter)
  for i in range(num_iter):
    #pick best hypothesis
    pos, coeff, feature, error = find_hyp(X_train, y_train, Dt)
    h[i] = lambda x : coeff if x[feature] >= pos else -coeff
    alpha[i] = 0.5*np.log((1-error)/error)
    Dt = newD(Dt, alpha[i], h[i], X_train, y_train)
  H = lambda x : 1 if (sum([alpha[i]*h[i](x) for i in range(num_iter)]) >= 0) else -1
  pred = np.zeros(len(X_test))
  for i in range(len(X_test)):
    pred[i] = H(X_test[i])
  return pred

def eval_acc(X_train, y_train, X_test, y_test, num_iter):
  """
    Arguments:
      X_train: (m, 2) numpy array
      Y_train: (m,) numpy array
      X_test: (n, 2) numpy array
      Y_test: (n,) numpy array
      num_iter: an integer
    Returns:
      pred: (n,) numpy array
  """
  y_pred = adaboost(X_train, y_train, X_test, num_iter)
  acc = np.equal(y_pred, y_test).astype("float32").sum() / y_pred.shape[0]
  return acc

if __name__ == "__main__":
  num_iter = 200
  X_train, y_train, X_test, y_test = load_data("adaboost_dataset_1")
  acc = eval_acc(X_train, y_train, X_test, y_test, num_iter)
  print("test acc for dataset 1 {:.3f}".format(acc))

  X_train, y_train, X_test, y_test = load_data("adaboost_dataset_2")
  acc = eval_acc(X_train, y_train, X_test, y_test, num_iter)
  print("test acc for dataset 2 {:.3f}".format(acc))
