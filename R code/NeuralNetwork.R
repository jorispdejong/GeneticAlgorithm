#################
### FUNCTIONS ###
#################
# Activation function
ActivationFunction <- function(x){
  return(1/(1+exp(-x)))
}
# Feedforward function
FeedForward <- function(inputs, dna, no_bias=TRUE){
  if(no_bias){
    if(length(inputs) != dim(dna[[1]])[2]) stop("Number of inputs does not match the neural network")
    n_hidden_layers <- length(dna) - 1
    neuronsTemp <- list(inputs)
    for(i in 1:(n_hidden_layers+1)){
      unNormalizedNeurons <- dna[[i]] %*% neuronsTemp[[i]]
      neuronsTemp[[i + 1]] <- ActivationFunction(unNormalizedNeurons)
    }
  }else{
    if(length(inputs) != (dim(dna[[1]])[2]-1)) stop("Number of inputs does not match the neural network")
    n_hidden_layers <- length(dna) - 1
    neuronsTemp <- list(inputs)
    for(i in 1:(n_hidden_layers+1)){
      unNormalizedNeurons <- dna[[i]][,1:(dim(dna[[i]])[2]-1)]%*% neuronsTemp[[i]] + dna[[i]][,dim(dna[[i]])[2]]
      neuronsTemp[[i + 1]] <- ActivationFunction(unNormalizedNeurons)
    }
  }
  
  outputs <- neuronsTemp[[length(dna) + 1]]
  return(outputs)
}
# Create the initial dna
CreateEmptyNeurons <- function(n_inputs, n_outputs, n_hidden_layers, n_hidden){
  if(length(n_hidden) != n_hidden_layers) stop("Number of hidden layers does not match the vector with hidden layer sizes")
  inputLayer <- rep(NA, n_inputs)
  outputLayer <- rep(NA, n_outputs)
  if(n_hidden_layers==0){
    neuralNetwork <- list(inputLayer, outputLayer) 
  }else{
    neuralNetwork <- list(inputLayer)
    for(i in 1:n_hidden_layers){
      hiddenLayer <- rep(NA, n_hidden[i])
      neuralNetwork[[i + 1]] <- hiddenLayer
    }
    neuralNetwork[[n_hidden_layers + 2]] <- outputLayer 
  }
  return(neuralNetwork)
}

# Create different functions for the serve and the rally in case they need to be different
############################
### NEURAL NETWORK SERVE ###
############################
# Set variables
nInputsS <- 2 #1=z-position of player (height), 2=alpha
nOutputsS <- 3 # 1=v0 of ball, 2=theta, 3=angular velocity of ball
nHiddenLayersS <- 0 # Must equal the number of elements in nHidden
nHiddenS <- c() # Must have the same number of elements as nHiddenLayer
# Create a list of empty neurons as initialization for the neural network
empty_neuronsS <- CreateEmptyNeurons(nInputsS, nOutputsS, nHiddenLayersS, nHiddenS)

############################
### NEURAL NETWORK RALLY ###
############################
# Set variables
nInputsR <- 3 #1=x-position of player, 2=z-position of player, 3=v_incoming
nOutputsR <- 3 # 1=theta, 2=w ball, 3=w_racket
nHiddenLayersR <- 0 # Must equal the number of elements in nHidden
nHiddenR <- c() # Must have the same number of elements as nHiddenLayer
# Create a list of empty neurons as initialization for the neural network
empty_neuronsR <- CreateEmptyNeurons(nInputsR, nOutputsR, nHiddenLayersR, nHiddenR)


