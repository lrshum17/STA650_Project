#This function return Spectral Clustering results for a specified number of blocks
clusterSpec = function(graphK, blocksK){
  
  adjK = getAdjacency(graphK)
  graphNodesK = getNodes(adjK)
  
  # Spectral Clustering #
  SCVecK = specClust(adjMat = adjK, nBlocks = blocksK, nIter = 1000)
  graphNodesK$group = SCVecK
  # Checking the clusters #
  clusterID = SCVecK
  nodesID = graphNodesK$id
  clusterList = list()
  
  for(i in 1:length(unique(clusterID))){
    clusterList[[i]] = graphNodesK$id[which(graphNodesK$group == unique(clusterID)[i])]
  }
  
  circlesKEstSC = getCirclesEst(clusterList)
  #clustKSC = as_membership(SCVecK)
  #communitiesSC = make_clusters(graphK, membership = clustKSC)
  #plot(communitiesSC, graphK, main ="Spectral Clustering") 
  return(circlesKEstSC)
}


#This function returns Hierarchical Agglomorative Clustering using fast and greedy method
fastGreedyClust = function(graphK){
  
  adjK = getAdjacency(graphK)
  graphNodesK = getNodes(adjK)
  
  kc = fastgreedy.community(graphK)
  
  kcLabels = kc$membership
  # Checking the clusters #
  clusterID = kcLabels
  nodesID = graphNodesK$id
  graphNodesK$group = kcLabels
  clusterList = list()
  
  for(i in 1:length(unique(clusterID))){
    clusterList[[i]] = graphNodesK$id[which(graphNodesK$group == unique(clusterID)[i])]
  }
  
  circlesFGEst = getCirclesEst(clusterList)
  #plot(kc, graphK, main ="Fast & Greedy")
  return(circlesFGEst)
}


#This function return leading eigenvalue clustering results
levClust = function(graphK){
  
  adjK = getAdjacency(graphK)
  graphNodesK = getNodes(adjK)
  
  lev = leading.eigenvector.community(graphK)
  #lev = make_clusters(graphK, membership = lev$membership)
  kcLabels = lev$membership
  # Checking the clusters #
  clusterID = kcLabels
  nodesID = graphNodesK$id
  graphNodesK$group = kcLabels
  
  clusterList = list()
  
  for(i in 1:length(unique(clusterID))){
    clusterList[[i]] = graphNodesK$id[which(graphNodesK$group == unique(clusterID)[i])]
  }
  
  circlesLevEst = getCirclesEst(clusterList)
  #plot(lev, graphK, main ="LEV")
  return(circlesLevEst)  
}


#This function return mcl clustering results
clusterMCL = function(graphK){
  
  adjK = getAdjacency(graphK)
  nodesK= getNodes(adjK)
  
  cluster <- mcl( x = adjK, addLoops=TRUE, ESM = TRUE)
  
  nodesK$group = cluster$Cluster
  clusterID1 = as.integer(nodesK$group)
  clusterList1 = list()
  for(i in 1:length(unique(clusterID1))){
    clusterList1[[i]] = nodesK$id[which(nodesK$group == unique(clusterID1)[i])]
  }
  
  circlesKEstMCL = getCirclesEst(clusterList1)
  #clustKMCL = as_membership(cluster$Cluster)
  #communitiesKMCL = make_clusters(graphK, membership = clustKMCL)
  #plot(communitiesKMCL, graphK, main ="Markov Clustering")
  return(circlesKEstMCL)
}


