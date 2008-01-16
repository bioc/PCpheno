## ===========================================================================
## Compute density estimate to test whether  genes (genename) inducing a phenotype
## are randomly distributed in the interactome
## ---------------------------------------------------------------------------
##  Observed ratio vs Expected ratios
## ---------------------------------------------------------------------------
densityEstimate <- function(genename, interactome, perm){

    if(length(genename) == 0 && !is.character(genename))
      stop("genename must be a character vector")
    if(length(genename) == 0 && !is.matrix(interactome))
      stop("interactome must be a binary matrix")  
    if(perm < 0 && perm == 0)
      stop("The number of permutation must be positive and different from zero")
    
    geneInteractome <- rownames(interactome)
    phenoInteractome <- interactome[geneInteractome%in%genename, ]
    nrpheno <- nrow(phenoInteractome)

    if(nrpheno == 0)
      stop("No overlap between the genenames and the interactome")

    nbPhenoGene <- colSums(phenoInteractome)
    sizeC <- colSums(interactome)
    ratio <- nbPhenoGene / sizeC

    ratioX <- matrix(0, nrow=ncol(interactome), ncol=perm)
    for(i in 1:perm) {
        u <-  sample(geneInteractome, nrpheno)
        nbPhenoGeneX <- colSums(interactome[u, ])
        ratioX[, i] <- nbPhenoGeneX / sizeC
    }
    return(new("deResult", "Observed"=ratio, "Expected"=ratioX, "Size"=sizeC))
}

## ===========================================================================
## Graph theory approach to test whether  genes (genename) inducing a phenotype
## are randomly distributed in the interactome
## ---------------------------------------------------------------------------
##  Observed edges vs Expected edges
## ---------------------------------------------------------------------------
graphTheory <-function(genename,interactome,perm){
    ginteractome <- row.names(interactome)
    
    ##restrict the genename list to those for which we have complex co-membership data
    interest <-  intersect(genename, ginteractome)
    
    PPIg <- interactome %*% t(interactome)
    
    v1 <- rep(0, nrow(interactome))
    names(v1) <-  row.names(interactome)
    v1[interest] <- 1
    
    ##compute the graph where all interesting genes have edges to each other
    interestG <-  outer(v1, v1)        
    ##drop the self-loops
    diag(interestG) <-  0
    g <-  PPIg * interestG
    edgeCount <- sum(g)

    ##now for the simulation
    ans <- rep(NA, perm)
    for(i in 1:perm) {
        vx <-  sample(v1, nrow(interactome))
        Gx <-  outer(vx, vx)
        diag(Gx) <- 0
        ans[i] <-  sum(PPIg*Gx)
    }
    up <- length(which(ans>edgeCount))
    pval <- up/perm

    return(new("gtResult", "Observed"=edgeCount, "Expected"=sort(ans),"Pvalue"=pval))
}
