CreateInitialDNA <- function(neurons){
  dna <- list()
  for(i in 1:(length(neurons) - 1)){
    nRows <- length(neurons[[i + 1]])
    nCols <- length(neurons[[i]])
    weightsMat <- matrix(runif(nRows * nCols, min = -5, max = 5), nrow = nRows, ncol = nCols)
    dna[[i]] <- weightsMat
  }
  return(dna)
}