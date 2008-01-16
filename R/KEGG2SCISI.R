## ===========================================================================
## Searching for overlap between interactomes ex KEGG and ScISI
## ---------------------------------------------------------------------------
## 
## ---------------------------------------------------------------------------
KEGG2SCISI <-function(pw, pc, pcMat, pwMat){
    
    ##pw list of pathway names; pwMat pathway incidence matrix
    ##pc list of complex names; pcMat complex incidence matrix
    stopifnot(all(is.character(pw)))
    stopifnot(all(is.character(pc)))

    ## resize matrix to the interesting columns 
    keggMatint <-  pwMat[,pw]
    scISIMatint <- pcMat[,pc]

    ## resize matrix to the commun rows
    cr <- intersect(rownames(keggMatint),rownames(scISIMatint))
    keggMatint <- keggMatint[cr,]
    scISIMatint <- scISIMatint[cr,]
    
    ## count nb genes overlaping
    mapp <-t(keggMatint) %*% scISIMatint
    return(mapp)
}


summaryMap <- function(mapp, pcMat, pwMat, phenotype){

    ## mapp incidence matrix of pw x pc
    ## pwMat pathway incidence matrix
    ## pcMat complex incidence matrix
    ## phenotype e.g. essential

    ## resize matrix
    nr <- rowSums(mapp)
    nc <-  colSums(mapp)
    pcacross <- mapp[nr>0, , drop=FALSE]
    pcacross <- pcacross[, nc>0, drop=FALSE]
   
    ## count genes overlaping; return a vector or list
    pcacross2 <- apply(pcacross, 1, function(x){ g <- which(x>0)
                                             return(x[g])})
    
    if(is.list(pcacross2)){    
        ##call annotatePC and format results
        comp2Kegg <- lapply(pcacross2, function(x) annotatePC(x, pcMat, phenotype))
        comp2Kegg <-data.frame(format(comp2Kegg))
    }
    
    if(!is.list(pcacross2)){
        y <- pcacross[,colSums(pcacross)>0]
        comp2Kegg <- annotatePC(y, pcMat, phenotype)
        comp2Kegg <- matrix(format(comp2Kegg))
        rownames(comp2Kegg) <- names(pcacross2)
    }
    return(comp2Kegg)
}

annotatePC <- function(x, pcMat, phenotype){
    
    ## complex names 
    cn = names(x)
    if(is.null(cn))
      cn = colnames(x)

    ## size of the pathways and number of gene in the category/phenotype
    if(length(cn) == 1){
        sizePC <- sum(pcMat[, cn])
        nomen <- names(which(pcMat[, cn]>0))
        inC <- length(intersect(nomen, phenotype))
        
    } else {
        sizePC <- colSums(pcMat[, cn])
        inC <- apply(pcMat[, cn], 2, function(y) length(intersect(names(y[y>0]), phenotype)))
    }
    ans <- paste(cn, " (", sizePC, "," , inC, ")", " [", x[x>0], "]", sep="")
    return(ans)
}
