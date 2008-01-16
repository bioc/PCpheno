setMethod("hyperGTest",
          signature(p="CoHyperGParams"), 
          function(p) {
              origGeneIds <- geneIds(p)
              p@geneIds <- p@geneIds[p@geneIds %in% p@universeGeneIds]
              cat2Entrez <- categoryToEntrezBuilder(p)
              stats <- .doHyperGTest(p, cat2Entrez, list(),
                                     p@geneIds)
              ord <- order(stats$p)
              new("CoHyperGResult",
                  pvalues=stats$p[ord],
                  oddsRatios=stats$odds[ord],
                  expectedCounts=stats$expected[ord],
                  catToGeneId = cat2Entrez[ord],
                  annotation=annotation(p),
                  geneIds= p@geneIds,
                  testName=categoryName(p),
                  pvalueCutoff=pvalueCutoff(p),
                  testDirection=testDirection(p))
          })


setMethod("summary", signature(object="CoHyperGResult"),
          function(object, pvalue, htmlLinks=TRUE) {
              
              AMIGO_URL <- "http://www.godatabase.org/cgi-bin/amigo/go.cgi?view=details&search_constraint=terms&depth=0&query=%s"
              MIPS_URL <-"http://mips.gsf.de/genre/proj/yeast/searchCatalogAction.do?style=catalog.xslt&table=CELLULAR_COMPLEXES&num=%s&db=CYGD"
              
              
              if (missing(pvalue))
                pvalue <- pvalueCutoff(object)
              
              ## Filter Complex based on p-value and size
              pvals <- pvalues(object)
              ucounts <- universeCounts(object)
              wanted <- pvals < pvalue
              
              pvals <- pvals[wanted]
              ucounts <- ucounts[wanted]
              
              complexIds <- names(pvals)
              goID <- grep("GO",complexIds)
              mipsID <- grep("MIPS",complexIds)
              
              goIdUrls <- sapply(complexIds[goID], function(x) sprintf(AMIGO_URL, x))
              mipsIdUrls <- sapply(complexIds[mipsID], function(x) sprintf(MIPS_URL, x))
              
              odds <- oddsRatios(object)[wanted]
              ecounts <- expectedCounts(object)[wanted]
              counts <- geneCounts(object)[wanted]
              if (htmlLinks) {
                  categoryTerm <- complexIds
                  categoryTerm[goID] <- paste('<a href="', goIdUrls, '">', complexIds[goID],
                                   '</a>', sep="")
                  categoryTerm[mipsID] <- paste('<a href="', mipsIdUrls, '">', complexIds[mipsID],
                                   '</a>', sep="")
              }
          
              df <- data.frame(ID=complexIds, Pvalue=pvals, OddsRatio=odds,
                               ExpCount=ecounts, Count=counts,
                               Size=ucounts, Term=categoryTerm,
                               stringsAsFactors=FALSE,
                               row.names=NULL)
              df
          })


