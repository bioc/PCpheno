## ===========================================================================
##  search overlapping definition for the component of biological organizational units
## interactome must be matrix
## ---------------------------------------------------------------------------
## biology (cellular, tissular) Organizational Units  methods
## ---------------------------------------------------------------------------

overlap <-  function(interactome){

  if(!is.matrix(interactome))
    stop("the interactome must be a matrix")
  
    overlapM <- crossprod(interactome, interactome)
    overlapM[upper.tri(overlapM, diag=TRUE)] <- NA
    overlap <- which(overlapM>0, TRUE)
    nbSharedProt <- cbind(rownames(overlapM)[overlap[, 1]],
                          colnames(overlapM)[overlap[, 2]], overlapM[overlap])
    row.names(nbSharedProt) <- c(paste(rownames(overlapM)[overlap[, 1]], "-",
                                       colnames(overlapM)[overlap[, 2]], sep=""))
    colnames(nbSharedProt) <- c("C1", "C2", "nbSharedProt")
    return(nbSharedProt)
}
