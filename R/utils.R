## Truncate character strings
truncName <- function(x,n){
    if (nchar(x)> n) {
        ans <- paste(substr(x,0,n),"...",sep="")
    } 
    else{ 
        ans <- x
    } 
    return(ans)
}


## Reduce a binary matrix to the number of commun rows with a vector 
## Reduce Interactome
reduceM <- function(x, mat, threshold=0){

    if(!is.null(x) && !is.character(x) && !is.numeric(x))
      stop("x must be a vector")

    rN <- rownames(mat)
    common <- intersect(x, rN)

    if(length(common) == 0)
      stop("no intersection between x vector and the matrix rownames")

    res <- mat[common, ]
    res[ , colSums(res)> threshold]
}

##Same function that in Rintact: list2Matrix, therefore not exported
list2matrix <- function(x){
    allrows <- unlist(x)
    rows <- unique(allrows)
    cols <- names(x)
    mat <- matrix(0,nrow=length(rows),ncol=length(cols),dimnames=list(rows,cols))

    for(i in 1:length(cols)){
        idx <- rows%in%x[[i]]
        mat[idx, cols[i]] <- 1
    }
    return(mat)
}
