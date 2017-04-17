##Parse filetype 'xx.egonet' and save adjanceny matrix of the network

##Sret working directory and Load libraries
setwd('~/Desktop/Spring_2017/Sta650/Project/Scripts/')
library(plyr)
library(stringr)
library(igraph)
library(SpecClustPack)
library(MCL)
source("Read_Data.R")
source("Cluster_Methods.R")
source("Helper.R")

##list networks 'xx.egonets' to process
#18005,
network_list = c( 24758, 239, 26321, 8100, 5881, 9846, 16203, 19129, 1357)

for(i in network_list){
  edgeFile = paste(c('../Data/Kaggle_data/egonets/',i,'.egonet'), collapse = "" )
  circleFile = paste(c('../Data/Kaggle_data/Training/',i,'.circles'), collapse = "" )
  edge = getEdgesK(edgeFile)
  graph = getGraph(edge)
  
  FastGreedyRes = fastGreedyClust(graph)
  levClustRes = levClust(graph)
  SpecClustRes = clusterSpec(graph, dim(FastGreedyRes)[1])
  mclClustRes = clusterMCL(graph)
  
  circles = getCircles(circleFile) 
  ResultSaver(i, circles, FastGreedyRes, levClustRes, SpecClustRes, mclClustRes)
  print(i)
}



