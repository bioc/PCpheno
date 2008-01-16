## ===========================================================================
## Methods for the CoHyperGResult object
## 
## ---------------------------------------------------------------------------
setMethod("pvalues", signature(r="CoHyperGResult"),
          function(r) r@pvalues)

setMethod("oddsRatios", signature(r="CoHyperGResult"),
          function(r) r@oddsRatios)

setMethod("expectedCounts", signature(r="CoHyperGResult"),
          function(r) r@expectedCounts)

setMethod("geneCounts", signature(r="CoHyperGResult"),
          function(r) {
              sapply(r@catToGeneId, function(x) {
                  sum(geneIds(r) %in% x)
              })
          })

setMethod("universeCounts", signature(r="CoHyperGResult"),
          function(r) {
              ans <- listLen(r@catToGeneId)
              names(ans) <- names(r@catToGeneId)
              ans
          })


