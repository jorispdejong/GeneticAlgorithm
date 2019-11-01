########################
### GLOBAL VARIABLES ###
########################
min_mutation <- -1
max_mutation <- 1

# Create different functions for the rally and the serve in case the need to be different
#################
### DNA SERVE ###
#################
# Create random dna
CreateInitialDNAS <- function(neurons, no_bias = TRUE){
  dna <- list()
  for(i in 1:(length(neurons) - 1)){
    nRows <- length(neurons[[i + 1]])
    if(no_bias){
      nCols <- length(neurons[[i]])
    }else{
      nCols <- length(neurons[[i]]) + 1
    }
    weightsMat <- matrix(runif(nRows * nCols, min = min_mutation, max = max_mutation), nrow = nRows, ncol = nCols)
    dna[[i]] <- weightsMat
  }
  return(dna)
}
# Crossover of genetic algorithm
CrossOverS <- function(selection_dna, amount=1){
  n_selection <- length(selection_dna)
  if(amount==1){
    selection_prob_1 <- 1/n_selection
    selection_prob_rest <- 1/n_selection
  }else{
    selection_prob_1 <- 0.9
    selection_prob_rest <- 0.1/(n_selection-1)
  }
  
  dna_length <- length(selection_dna[[1]])
  dna_child <- list()
  for(k in 1:dna_length){
    n_rows <- dim(selection_dna[[1]][[k]])[1]
    n_cols <- dim(selection_dna[[1]][[k]])[2]
    weights_matrix <- matrix(rep(NA, n_rows*n_cols), nrow = n_rows, ncol = n_cols)
    for(j in 1:n_rows){
      for(i in 1:n_cols){
        rand <- runif(1)
        if(rand < selection_prob_1){
          weights_matrix[j,i] <- selection_dna[[1]][[k]][j,i]
        }
        for(sel in 2:n_selection){
          if(rand >= selection_prob_1 + (sel-2)*selection_prob_rest && rand < selection_prob_1 + (sel-1)*selection_prob_rest){
            weights_matrix[j,i] <- selection_dna[[sel]][[k]][j,i]
          }
        }
      }
    }
    dna_child[[k]] <- weights_matrix
  }
  return(dna_child)
}
# Mutation of genetic algorithm
MutationS <- function(dna){
  new_dna <- list()
  for(k in 1:length(dna)){
    weights_matrix <- dna[[k]]
    for(j in 1:dim(weights_matrix)[1]){
      for(i in 1:dim(weights_matrix)[2]){
        if(runif(1) < (1/dim(weights_matrix)[1])){
          weights_matrix[j,i] <- runif(1, min_mutation, max_mutation)
        }
      }
    }
    new_dna[[k]] <- weights_matrix
  }
  return(new_dna)
}

#################
### DNA RALLY ###
#################
# Create random dna
CreateInitialDNAR <- function(neurons, no_bias = TRUE){
  dna <- list()
  for(i in 1:(length(neurons) - 1)){
    nRows <- length(neurons[[i + 1]])
    if(no_bias){
      nCols <- length(neurons[[i]])
    }else{
      nCols <- length(neurons[[i]]) + 1
    }
    weightsMat <- matrix(runif(nRows * nCols, min = min_mutation, max = max_mutation), nrow = nRows, ncol = nCols)
    dna[[i]] <- weightsMat
  }
  return(dna)
}
# Crossover of genetic algorithm
CrossOverR <- function(selection_dna, amount=1){
  n_selection <- length(selection_dna)
  selection_prob <- 1/n_selection
  dna_length <- length(selection_dna[[1]])
  dna_child <- list()
  for(k in 1:dna_length){
    n_rows <- dim(selection_dna[[1]][[k]])[1]
    n_cols <- dim(selection_dna[[1]][[k]])[2]
    weights_matrix <- matrix(rep(NA, n_rows*n_cols), nrow = n_rows, ncol = n_cols)
    for(j in 1:n_rows){
      for(i in 1:n_cols){
        rand <- runif(1)
        if(rand < selection_prob){
          weights_matrix[j,i] <- selection_dna[[1]][[k]][j,i]
        }
        for(sel in 2:n_selection){
          if(rand >= (sel-1)*selection_prob && rand < sel*selection_prob){
            weights_matrix[j,i] <- selection_dna[[sel]][[k]][j,i]
          }
        }
      }
    }
    dna_child[[k]] <- weights_matrix
  }
  return(dna_child)
}
# Mutation of genetic algorithm
MutationR <- function(dna){
  new_dna <- list()
  for(k in 1:length(dna)){
    weights_matrix <- dna[[k]]
    for(j in 1:dim(weights_matrix)[1]){
      for(i in 1:dim(weights_matrix)[2]){
        if(runif(1) < (1/dim(weights_matrix)[1])){
          weights_matrix[j,i] <- weights_matrix[j,i] * sign(runif(1, min_mutation, max_mutation))*runif(1, 0.9, 1.1)
        }
      }
    }
    new_dna[[k]] <- weights_matrix
  }
  return(new_dna)
}
