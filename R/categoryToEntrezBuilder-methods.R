## ===========================================================================
## universeBuilder method
## ---------------------------------------------------------------------------
## Methods for HyperG test for cellular organizational units
## ---------------------------------------------------------------------------
setMethod("universeBuilder", signature(p="CoHyperGParams"),
          function(p) {
              p@universeGeneIds
          })

## ===========================================================================
## categoryToEntrezBuilder method
## ---------------------------------------------------------------------------
## Methods for HyperG test for cellular organizational units
## ---------------------------------------------------------------------------
setMethod("categoryToEntrezBuilder",
          signature(p="CoHyperGParams"),
          function(p) {
              getCompToEntrezMap(p)
          })



getCompToEntrezMap <- function(p) {
    
    category <- get(p@categoryName)
    apply( category, 2, function(x)  names(which(x>0)))
}
