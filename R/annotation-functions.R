## ===========================================================================
## Annotation function
## ---------------------------------------------------------------------------
## Functions to complete cellular organizational units annotation  
## ---------------------------------------------------------------------------

getDescr <- function(x, database="GO"){
    
    Descr <- vector(length=length(x))
    names(Descr) <- x
    if(any(database == "KEGG")){
        if(length(x)>0){
            if(require("KEGG", character.only=TRUE,quietly=TRUE))
            kegg <- mget(x, env=KEGGPATHID2NAME, ifnotfound=NA)            
            Descr[x] <- unlist(kegg)
        }
    }
    
    if(any(database == "GO")){
        GOtermX <- grep("GO", x)
        if(length(GOtermX)>0){
            termsGO <-unlist(getGOTerm(x[GOtermX]))
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


