#This function reads 'xx.egonet' and put the element into a matrix format
getCircles = function(circlesFile){
  
  # Read in the data
  x = scan(circlesFile, what="", sep="\n")
  # Separate elements by one or more whitepace
  y = strsplit(x, "[[:space:]]+")
  # Extract the first vector element and set it as the list element name
  names(y) = sapply(y, `[[`, 1)
  # Remove the first vector element from each list element
  y = lapply(y, `[`, -1)
  # Change character to numbers
  y = sapply(y, as.numeric)
  
  circles0Names = names(y)
  circles0 = y
  for(i in 1:length(y)){
    circles0[[i]] = t(as.matrix(y[[i]]))
  }
  circles0 = rbind.fill.matrix(circles0)
  rownames(circles0) = circles0Names
  
  return(circles0)
}


#This function returns an edgelist for graph 'xx.egonet'
getEdgesK = function(edgeFile){
  graphKEdges = getCircles(edgeFile)
  nodeN = as.numeric(str_replace(rownames(graphKEdges), pattern =  ":", replacement = ""))
  edgeListK = NA
  for(i in 1:length(nodeN)){
    #get first row of matrix
    mat_row = graphKEdges[i,]
    #remove NAs
    mat_row = mat_row[!is.na(mat_row)]
    #create edge pairing for nodeN
    newEdge = cbind(rep(nodeN[i], length(mat_row)), mat_row)
    #add to edgelist
    edgeListK = rbind(edgeListK, newEdge)
  }
  edgeListK = edgeListK[-1,]
  return(edgeListK)
} 

#This function returns a graph object from a edgelist
getGraph = function(edjK){
  temp = apply(edjK,1,as.character)
  temp = t(temp)
  graphK = graph_from_edgelist(temp, directed = FALSE)
  adj = as.matrix(get.adjacency(graphK)/2)
  graphK = as.undirected(graph_from_adjacency_matrix(adj))
  return(graphK)
}


#This function returns number of nodes 
getNodes = function(adjK){
  graphNodesK <- data.frame(id=as.integer(rownames(adjK)))
  graphNodesK$group <- 1
  return(graphNodesK)
}