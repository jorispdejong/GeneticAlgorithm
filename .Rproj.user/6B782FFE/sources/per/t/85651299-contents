# Set variables
population <- 20
nInputs <- 3 # 1=Height of player, 2=x-position of player, 3=y-position of player
nOutputs <- 4 # 1=v0 of ball, 2=theta, 3=alpha, 4=angular velocity of spin
nHiddenLayers <- 3 # Must equal the number of elements in nHidden
nHidden <- c(10, 10, 10) # Must have the same number of elements as nHiddenLayer

# Create the initial dna's
CreateEmptyNeurons <- function(n_inputs, n_outputs, n_hidden_layers, n_hidden){
  if(length(nHidden) != nHiddenLayers) stop("Number of hidden layers does not match the vector with hidden layer sizes")
  inputLayer <- rep(NA, n_inputs)
  outputLayer <- rep(NA, n_outputs)
  neuralNetwork <- list(inputLayer)
  for(i in 1:n_hidden_layers){
    hiddenLayer <- rep(NA, n_hidden[i])
    neuralNetwork[[i + 1]] <- hiddenLayer
  }
  neuralNetwork[[n_hidden_layers + 2]] <- outputLayer 
  return(neuralNetwork)
}
Sigmoid <- function(x){
  return(1/(1+exp(-x)))
}
FeedForward <- function(inputs, dna){
  if(length(inputs) != dim(dna[[1]])[2]) stop("Number of inputs does not match the neural network")
  neuronsTemp <- list(inputs)
  for(i in 1:(nHiddenLayers+1)){
    unNormalizedNeurons <- dna[[i]] %*% neuronsTemp[[i]]
    neuronsTemp[[i + 1]] <- Sigmoid(unNormalizedNeurons)
  }
  outputs <- neuronsTemp[[length(dna) + 1]]
  return(outputs)
}

# Create a list of empty neurons as initialization for the neural network
empty_neurons <- CreateEmptyNeurons(nInputs, nOutputs, nHiddenLayers, nHidden)
