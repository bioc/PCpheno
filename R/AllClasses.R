## ===========================================================================
## testResult
## ---------------------------------------------------------------------------
## virtual class 
## ---------------------------------------------------------------------------
setClass("testResult",
         representation("VIRTUAL", Observed="numeric", Expected="ANY"),
         prototype=list(Observed=numeric(0), Expected=matrix(0)))
         
## ===========================================================================
## deResult
## ---------------------------------------------------------------------------
## A container for the results after applying a density Estimate test 
## ---------------------------------------------------------------------------
setClass("deResult",
         contains="testResult",
         representation(Size="numeric"),       
         prototype=list(Size=numeric(0)))

## ===========================================================================
## gtResult
## ---------------------------------------------------------------------------
## A container for the results after applying a density Estimate test 
## ---------------------------------------------------------------------------
setClass("gtResult",
         contains="testResult",
         representation(Pvalue="numeric"),
         prototype=list(Pvalue=numeric(0)))


## ===========================================================================
## HyperG parameters 
## ---------------------------------------------------------------------------
## A container for the parameters to use for a hyperGtest 
## ---------------------------------------------------------------------------

setClass("CoHyperGParams",
         contains="HyperGParams",
         prototype=prototype(categoryName="character"))

## ===========================================================================
## HyperG parameters 
## ---------------------------------------------------------------------------
## A container for the results after applying a HyperGTest 
## ---------------------------------------------------------------------------

setClass("CoHyperGResult",
         contains="HyperGResultBase",
         representation=representation(
           pvalues="numeric",
           oddsRatios="numeric",
           expectedCounts="numeric",
           geneCounts="numeric",
           universeCounts="numeric",
           catToGeneId="list"))
