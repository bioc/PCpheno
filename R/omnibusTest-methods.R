## ===========================================================================
## plot
## ---------------------------------------------------------------------------
## Plot function for the densityEstimate results 
## ---------------------------------------------------------------------------
setMethod("plot",
          signature=signature(x="deResult", y="missing"),
          function(x, exp.col="grey", obs.col="black", main="", ylim=NULL, ...){
            perm <- ncol(x@Expected)
            a <- x@Observed 
            b <- x@Expected
            
            ## ylim calculation
            if(is.null(ylim)){
                d <- density(b)
                maxb <- max(d$y)
                ylim = c(0, maxb)
            }
            
            
            plot(density(a[!is.na(a)]), col=obs.col, ylim=ylim, main=main, ...)
            for(i in 1:perm){
              lines(density(b[,i]), col= exp.col, type="l", pch=20)
            }      
          }
          )

## ===========================================================================
## plot
## ---------------------------------------------------------------------------
## Plot function for the graphTheory results  
## ---------------------------------------------------------------------------

setMethod("plot",
          signature=signature(x="gtResult", y="missing"),
          function(x, exp.col="grey", obs.col="red", main="",...){
            a <- x@Observed 
            b <- x@Expected
            maxb <- max(b)
            hist(b, xlim= c(0, a+a*.25),  axes=TRUE, col=exp.col, main=main, xlab="Edges",...)
            lines(c(a,  a), c(0, 10*maxb), lty=2, lwd=3, col=obs.col,...)
          }
          )      
