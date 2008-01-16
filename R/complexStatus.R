## ===========================================================================
## Score (classify) complexes according pvalue and the distribution of the
## genes associated to the observed phenotype
## ---------------------------------------------------------------------------
complexStatus <- function(data, phenotype, interactome, threshold=0.05){

    interactomeP <- interactome[intersect(phenotype,rownames(interactome)),]
    
    significant <- names(data@pvalues[pvalues(data) <= threshold])
    non <-  names(data@pvalues[pvalues(data) > threshold])
    noPheno <- names(data@geneCounts[geneCounts(data)==0])

    d1 <- interactomeP[,significant]
    d2 <- interactomeP[,non[!non%in%noPheno]]
   
    res <- vector(mode="list")
    res$A <- significant
    res$B <- colnames(d2)
    res$C <- setdiff(colnames(interactome), c(res$A,res$B))

    return(res)
}
