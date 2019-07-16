# -*- coding: utf-8 -*-
"""
Created on Fri Mar  2 21:33:09 2018

@author: ss
"""

import pandas as pd
#import numpy as np

milk = pd.read_csv("F:/Python Material/Python Course/Datasets/milk.csv",index_col=0)

# Import KMeans
from sklearn.cluster import KMeans

# Create a KMeans instance with 3 clusters: model
model = KMeans(n_clusters=3)

# Fit model to points
model.fit(milk)

# Determine the cluster labels of new_points: labels
labels = model.predict(milk)

# Print cluster labels of new_points
print(labels)

clustNos = [2,3,4,5,6,7,8,9,10]
Inertia = []

for i in clustNos :
    model = KMeans(n_clusters=i)
    model.fit(milk)
    Inertia.append(model.inertia_)
    
# Import pyplot
import matplotlib.pyplot as plt


plt.plot(clustNos, Inertia, '-o')
plt.xlabel('Number of clusters, k')
plt.ylabel('Inertia')
plt.xticks(clustNos)
plt.show()

