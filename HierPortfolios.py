# translation of the R code from Carlos Trucios Maza

import numpy as np
import pandas as pd
from scipy.spatial.distance import pdist, squareform
from scipy.cluster.hierarchy import linkage, leaves_list


def HRP_Portfolio(df, linkage_method="single", graph=False):
    if linkage_method not in ["single", "complete", "average", "ward"]:
        return "ERROR: linkage_method argument only supports 'single', 'complete', 'average' or 'ward' options"
    if linkage_method == "ward":
        linkage_method = "ward.D2"
    
    # Stage 1: Tree clustering
    corr = df.corr()
    distance = np.sqrt(0.5 * (1 - corr))
    euclidean_distance = squareform(pdist(distance, metric="euclidean"))
    
    # Stage 2: Quasi-Diagonalisation
    clustering = linkage(euclidean_distance, method=linkage_method)
    clusters_order = leaves_list(clustering)
    
    # Stage 3: Recursive Bisection
    weights = np.ones(df.shape[1])
    index = [clusters_order.tolist()]
    while len(index) > 0:
        new_index = []
        for i in index:
            middle = len(i) // 2
            indexa = i[:middle]
            indexb = i[middle:]
            covar_clustera = df.iloc[:, indexa].iloc[indexa, :]
            covar_clusterb = df.iloc[:, indexb].iloc[indexb, :]
            weightsa = 1 / np.diag(covar_clustera)
            weightsa /= np.sum(weightsa)
            weightsb = 1 / np.diag(covar_clusterb)
            weightsb /= np.sum(weightsb)
            variance_clustera = np.dot(weightsa, np.dot(covar_clustera, weightsa))
            variance_clusterb = np.dot(weightsb, np.dot(covar_clusterb, weightsb))
            alpha = 1 - variance_clustera / (variance_clustera + variance_clusterb)
            weights[indexa] *= alpha
            weights[indexb] *= (1 - alpha)
            if len(indexa) > 1:
                new_index.append(indexa)
            if len(indexb) > 1:
                new_index.append(indexb)
        index = new_index
    
    if graph:
        import matplotlib.pyplot as plt
        from scipy.cluster.hierarchy import dendrogram
        
        plt.figure(figsize=(10, 7))
        plt.xlabel("Stocks")
        plt.ylabel("Distance")
        plt.title("Cluster Dendrogram - HRP")
        column_names = list(df.columns)
        dendrogram(clustering, leaf_rotation=90, leaf_font_size=8, labels=column_names)
        plt.show()


    
    return weights
