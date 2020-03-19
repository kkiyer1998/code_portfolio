import numpy as np
import collections
import math
from scipy.io import loadmat
#import matplotlib.pyplot as plt


def sq_distance(X,Y):
  Z = X - Y
  Z = [i**2 for i in Z]
  return sum(Z)

def update_assignments(X, C):
  """
    Arguments:
      X: (n, d) numpy array
      C: (k, d) numpy array
    Returns:
      assignments: (n,) numpy array
  """
  n = len(X)
  k = len(C)
  assignments = np.zeros((n,), dtype = int)
  for i in range(n):
    min_dis = sq_distance(X[i], C[0])
    min_index = 0
    for j in range(1,k):
      dis = sq_distance(X[i], C[j])
      if(dis<=min_dis):
        min_dis = dis
        min_index = j
    assignments[i] = min_index
  return assignments

def update_centers(X, prev_C, assignments):
  """
    Arguments:
      X: (n, d) numpy array
      prev_C: (k, d) numpy array
      assignments: (n,) numpy array
    Returns:
      C: (k, d) numpy array
  """
  n = len(X)
  k = len(prev_C)
  C = np.zeros_like(prev_C)
  num_points = np.zeros(k, dtype = int)
  for i in range(n):
    j = assignments[i]
    C[j] += X[i]
    num_points[j] += 1
  for i in range(k):
    if num_points[i] == 0:
      C[i] = prev_C[i]
    else:
      C[i] = C[i] / num_points[i]
  return C

def lloyd_iteration(X, C0):
  """
    Arguments:
      X: (n, d) numpy array
      C0: (k, d) numpy array
    Returns:
      C: (k, d) numpy array
      assignments: (n,) numpy array
  """
  converged = False
  k = len(C0)
  n = len(X)
  C = C0
  prev_C = C0
  while(not converged):
    assignments = update_assignments(X, C)
    C = update_centers(X, C, assignments)
    if(np.array_equal(prev_C, C)):
      converged = True
    else:
      prev_C = C
  return C, assignments

def kmeans_obj(X, C, assignments):
  """
    Arguments:
      X: (n, d) numpy array
      C: (k, d) numpy array
      assignments: (n,) numpy array
    Returns:
      obj: a float
  """
  n = len(assignments)
  k = len(C)
  obj = 0.0
  for i in range(n):
    cur_center = assignments[i]
    obj += (sq_distance(X[i], C[cur_center]))
  return obj

def discrete_sample(weights):
  weights = weights / weights.sum()
  return np.random.choice(weights.shape[0], 1, p=weights)[0]

def kmeanspp_init(X, k):
  n = X.shape[0]
  sq_min_dist = np.ones((n,), dtype="float32") * 10000
  C = np.zeros((k, X.shape[1]), dtype="float32")
  for i in range(k):
    idx = discrete_sample(sq_min_dist)
    C[i] = X[idx]
    sq_dist = np.power(X - X[idx: idx + 1], 2).sum(axis=1)
    sq_min_dist = np.minimum(sq_min_dist, sq_dist)
  return C

def kmeans_cluster(X, k, init, num_restarts):
  best_obj = float("inf")
  best_C = None
  best_assignments = None
  for i in range(num_restarts):
    if init == "random":
      perm = np.random.permutation(X.shape[0])
      C = X[perm[:k]]
    elif init == "kmeans++":
      C = kmeanspp_init(X, k)
    else:
      assert False
    C, assignments = lloyd_iteration(X, C)
    obj = kmeans_obj(X, C, assignments)
    if obj < best_obj:
      best_C = C.copy()
      best_assignments = assignments.copy()
      best_obj = obj
  return best_C, best_assignments, best_obj

def load_data():
  data = loadmat("../Data/kmeans_data.mat")
  return data["X"]

if __name__ == "__main__":
  X = load_data()
  kmeans_cluster(X, 8, 'random', 10)

  

