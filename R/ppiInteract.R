## ===========================================================================
##  Test the association between AP-MS data and phenotype
## ---------------------------------------------------------------------------
##  via a graph  and permutation model
## ---------------------------------------------------------------------------
ppiInteract <- function(genename, expGraph, bait, prey, perm=10){
  ##we need to work with the underlying
  ##undirected graph
  graphData <- new("graphNEL", nodes=nodes(expGraph), edgeL=edges(expGraph), edgemode="directed")
  graphData <- ugraph(graphData)
  nodeNames <- nodes(graphData)
  canUse <- intersect(genename, nodeNames)
  
  bp <- intersect(bait, prey)
  bonly <- setdiff(bait, prey)
  ponly <- setdiff(prey, bait)
  
  communBP <-intersect(canUse, bp)
  sharedB <-intersect(canUse, bonly)
  sharedP <-intersect(canUse, ponly)
  
  nBP <- length(communBP)
  nB <- length(sharedB)
  nP <- length(sharedP)
  stopifnot(nBP + nB + nP == length(canUse))
  
  ##create a cluster graph - check to make sure we have everything here
  others = as.list(setdiff(nodeNames, canUse))
  others$cU = canUse
  cG = new("clusterGraph", clusters = others)
  obsInt = numEdges(intersection(cG, graphData))
  
  rval = rep(NA, perm)
  
  mysample = function(x, size, replace = FALSE, prob = NULL) {
    if(size == 0 && length(x)==0) 
      return(x)
    else return(sample(x, size, replace, prob))
  }
  genSample = function() 
    c( mysample(bp, nBP), mysample(bonly, nB), mysample(ponly, nP))
  
  for(i in 1:perm) {
    newS = genSample()
    others = as.list(setdiff(nodeNames, newS))
    others$cU = newS 
    cG = new("clusterGraph", clusters = others)
    rval[i] = numEdges(intersection(cG, graphData))
  }
  
  return(list(obVal = obsInt, permVals = rval))
}

