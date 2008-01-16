## ===========================================================================
## Get Fitness Defect gene list based on a cutoff over the fitness defect score
## can be applied by conditions (media) or generation time
## ---------------------------------------------------------------------------

getFDgene <- function(data, condition, cutoff=c(20,100,100), mode="generation", subset=c(5,15,20)){
    if((mode!="condition")&(mode!="generation"))
      stop("mode wrongly defined or unspecified, mode must be either 'condition' or 'generation'.")
    
    fitnessG <- vector(mode="list")
    if (mode=="generation"){
        if(length(data)!=nrow(condition)) stop ("length of data must equal the number of condition")
        
        for(j in 1:length(subset)){
            idx <- which(condition[,2]== subset[j])
            dat <- data[names(data)%in%condition[idx,3]]
            temp <- lapply(dat,function(val) res <- val[which(val> cutoff[j])])
            fitnessG <- c(fitnessG,temp)
        }
    }
    if (mode=="condition"){
        for(i in 1:length(subset)){
            dat <- data[unlist(subset[i])]
            temp <- lapply(dat,function(val) res <- val[which(val> cutoff[i])])
            fitnessG <- c(fitnessG,temp)
        }
    }
    return(fitnessG)
}

## ---------------------------------------------------------------------------
## Build Fitness Defect Matrix from the Giaever dataset
## ---------------------------------------------------------------------------

buildFDMat <- function(data, genenames, condition){
  GiaeverPhenoM <- matrix(0, nrow = length(genenames), ncol = length(condition),
                          dimnames = list(genenames, condition))
  
  genefitness <- lapply(data, names)
  for(i in 1:length(data)){
    id <- which(colnames(GiaeverPhenoM) == names(genefitness)[i])
    GiaeverPhenoM[genefitness[[i]], id] <- 1
  }
  return(GiaeverPhenoM)
}
