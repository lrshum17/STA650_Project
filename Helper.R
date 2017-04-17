#Function return the number of nodes in the adjancency matrix
getNodes = function(adjK){
  graphNodesK <- data.frame(id=as.integer(rownames(adjK)))
  graphNodesK$group <- 1
  return(graphNodesK)
}

#Function return adjacency matrix of a graph in full form
getAdjacency = function(graphK){
  adjK = as.matrix(get.adjacency(graphK))
  return(adjK)
}

#This function formats result from Clustering Techniques
getCirclesEst = function(circlesEst){
  
  y = circlesEst
  circles0 = y
  for(i in 1:length(y)){
    circles0[[i]] = t(as.matrix(y[[i]]))
  }
  circles0 = rbind.fill.matrix(circles0)
  return(circles0)
}


#This function saves results in format readable by python loss calc
ResultSaver = function(graphname, circles, FastGreedyRes, levClustRes, SpecClustRes, mclClustRes){
  sink(paste(graphname,"output.txt", sep = "_"))
  cat('Groundtruth \n')
  cat("[")
  for(i in 1:dim(circles)[1]){
    cat("[")
    b = circles[i,]
    b = b[!is.na(b)]
    cat(paste (b,sep="", collapse=","))
    cat("]")
    if(i != dim(circles)[1]){
      cat(",")
    }
  }
  cat("]")
  cat('\n FastGreedy \n')
  cat("[")
  for(i in 1:dim(FastGreedyRes)[1]){
    cat("[")
    b = FastGreedyRes[i,]
    b = b[!is.na(b)]
    cat(paste (b,sep="", collapse=","))
    cat("]")
    if(i != dim(FastGreedyRes)[1]){
      cat(",")
    }
  }
  cat("]")
  cat('\n FirstEigen \n')
  cat("[")
  for(i in 1:dim(levClustRes)[1]){
    cat("[")
    b = levClustRes[i,]
    b = b[!is.na(b)]
    cat(paste (b,sep="", collapse=","))
    cat("]")
    if(i != dim(levClustRes)[1]){
      cat(",")
    }
  }
  cat("]")
  cat('\n Spectral \n')
  cat("[")
  for(i in 1:dim(SpecClustRes)[1]){
    cat("[")
    b = SpecClustRes[i,]
    b = b[!is.na(b)]
    cat(paste (b,sep="", collapse=","))
    cat("]")
    if(i != dim(SpecClustRes)[1]){
      cat(",")
    }
  }
  cat("]")
  cat('\n MCL \n')
  cat("[")
  for(i in 1:dim(mclClustRes)[1]){
    cat("[")
    b = mclClustRes[i,]
    b = b[!is.na(b)]
    cat(paste (b,sep="", collapse=","))
    cat("]")
    if(i != dim(mclClustRes)[1]){
      cat(",")
    }
  }
  cat("]")
  sink()
}
  