## ===========================================================================
## Work in progress - function not exported
## 
## ---------------------------------------------------------------------------
## 
## ---------------------------------------------------------------------------
logistPheno <- function(genename,interactome,pval,iter=FALSE){
    gI <- row.names(interactome)
    EGs <- intersect(genename, gI)
    v1 <-  rep(0, nrow(interactome))
    names(v1) <-  gI
    v1[EGs] <-  1
    
    reg1 <-  glm(v1 ~ interactome, family = binomial)

    sum1 <-  summary(reg1)
    ScISIs <- interactome[, !sum1$aliased[-1]]
    d2 <- sum1$coef[,4] < 0.001
    comp <-  ScISIs[,d2[-1]]

    if(iter==TRUE){
        if (ncol(comp)!=0){
            reg <-  glm(v1 ~ comp, family= binomial)
            sum1 <-  summary(reg)
        }
    }
    res <- row.names(sum1$coef)[sum1$coef[,4] < pval]

    if(length(res)!=0){
        result <- sum1$coef[res,4]
        names(result) <-  gsub("interactome","",res)
        
    }
    else{
         result <- "Null"
    }
    return(result)
}
