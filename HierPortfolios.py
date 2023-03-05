import numpy as np
from scipy.spatial.distance import pdist, squareform
from scipy.cluster.hierarchy import linkage, dendrogram, fcluster

def HRP_Portfolio(covar, linkage="single", graph=False):
    linkage_methods = ["single", "complete", "average", "ward"]
    if linkage not in linkage_methods:
        return("ERROR: linkage argument only supports {} options".format(linkage_methods))
    if linkage == "ward":
        linkage = "ward"
    # Stage 1: Tree clustering
    corre = covar / np.sqrt(np.diag(covar))
    distance = np.sqrt(0.5 * (1 - corre))
    euclidean_distance = pdist(distance, metric='euclidean')
    euclidean_distance = squareform(euclidean_distance)
    # Stage 2: Quasi-Diagonalisation
    clustering = linkage(euclidean_distance, method=linkage)
    clusters_order = fcluster(clustering, t=len(covar), criterion='maxclust')
    # Stage 3: Recursive Bisection
    weights = np.ones(covar.shape[0])
    index = list(clusters_order)
    while len(index) > 0:
        new_index = []
        for i in index:
            middle = len(i)//2
            indexa = i[:middle]
            indexb = i[middle:]
            covar_clustera = covar[np.ix_(indexa, indexa)]
            covar_clusterb = covar[np.ix_(indexb, indexb)]
            weightsa = 1/np.diag(covar_clustera) / sum(1/np.diag(covar_clustera))
            weightsb = 1/np.diag(covar_clusterb) / sum(1/np.diag(covar_clusterb))
            variance_clustera = weightsa @ covar_clustera @ weightsa
            variance_clusterb = weightsb @ covar_clusterb @ weightsb
            alpha = 1 - variance_clustera/(variance_clustera + variance_clusterb)
            weights[indexa] = weights[indexa] * alpha
            weights[indexb] = weights[indexb] * (1 - alpha)
            if len(indexa) > 1: new_index.append(indexa)
            if len(indexb) > 1: new_index.append(indexb)
        index = new_index
    if graph:
        dendrogram(clustering, labels=covar.columns)
    return weights
