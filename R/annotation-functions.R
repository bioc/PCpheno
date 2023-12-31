## ===========================================================================
## Annotation function
## ---------------------------------------------------------------------------
## Functions to complete cellular organizational units annotation  
## ---------------------------------------------------------------------------

getDescr <- function(x, database="GO.db"){
    
    Descr <- vector(length=length(x))
    names(Descr) <- x
    if(any(database == "KEGG.db")){
        if(length(x)>0){
            if(require("KEGG.db", character.only=TRUE,quietly=TRUE))
            kegg <- mget(x, KEGGPATHID2NAME, ifnotfound=NA)            
            Descr[x] <- unlist(kegg)
        }
    }
    
    if(any(database == "GO.db")){
        GOtermX <- grep("GO", x)
        if(length(GOtermX)>0){
            if(require("GO.db"))    
            gocomplex = x[GOtermX]
            xx = as.list(GOTERM)
            annot =  xx[gocomplex]
            termsGO = sapply(annot, function(x) if(!is.null(x)){Term(x)}else{NA})
            Descr[GOtermX] <- termsGO
        }
    }
    
    if(any(database =="MIPS")){
        MIPStermX <- grep("MIPS", x)
        if(length(MIPStermX)>0){
            MIPSterm <- x[MIPStermX]
            mips <- getMipsInfo()
            termsMIPS <- mips[MIPSterm]
            termsMIPS <- sapply(termsMIPS, function(x) attr(x, "desc"))
            Descr[MIPStermX] <- termsMIPS
        }
        
    }
   
    return(Descr)
}


