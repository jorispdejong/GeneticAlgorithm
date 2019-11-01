rm(list = ls(all.names = TRUE)) # Clear global environment (including hidden objects)

# Source
source("Equations of Motion.R")  # Numerically solves the equations of motion
source("Functions.R")            # Contains usefull functions, e.g. plotting the tennis field, computing the height of the tennis net
source("NeuralNetwork.R")        # Defines the Neural Network we will be using
source("DNA.R")             # Contains the Genetic Algorithm functions and the function to create the initial DNA 
source("Global_parameters.R")

########################
### DEFINE FUNCTIONS ###
########################
# Determine whether to play to ball on the left or right of the opponent
HitBallToLeftOfOpponentRally <- function(pos_opponent_y, a_1=0.8){
  # Determine p_l and p_r
  res_1 <- matrix(rep(NA, 2*length(pos_opponent_y)), nrow=length(pos_opponent_y), ncol = 2)
  p_l <- 1/(exp(a_1*pos_opponent_y)+1)
  res_1[,1] <- p_l
  res_1[,2] <- 1-p_l
  
  # Determine left or right
  rand_1 <- runif(1)
  left <- rand_1 < p_l
  # right <- !left
  return(left)
}
# Determine alpha based on the position of the player and its opponent, using a skew probability distribution
DetermineAlphaRally <- function(pos_player_a, pos_opponent_a){
  left <- HitBallToLeftOfOpponentRally(pos_opponent_a[2])
  
  angle_1 <- atan((field_width-pos_player_a[2])/(field_baseline_length + abs(pos_player_a[1])))*180/pi
  angle_2 <- atan((-field_width-pos_player_a[2])/(field_baseline_length + abs(pos_player_a[1])))*180/pi
  
  rand_skew <- rbeta(1, skew_parameter_1, skew_parameter_2)
  
  if(pos_player_a[1]<0){
    angle_left <- angle_1*rand_skew
    angle_right <- angle_2*rand_skew
  }else{
    angle_left <- angle_2*rand_skew
    angle_right <- angle_1*rand_skew
  }
  
  if(left){
    angle_new <- angle_left
  }else{
    angle_new <- angle_right
  }
  
  return(angle_new)
}
# Function that checks the dna we found
CheckRallyDna <- function(dna, n_balls){
  ##############################################
  ### SET THE PLAYER'S POSITIONS FOR EACH HIT###
  ##############################################
  
  pos_players_rally <- matrix(rep(NA, 3*n_balls), nrow = n_balls, ncol = 3)
  pos_opponents_rally <- matrix(rep(NA, 3*n_balls), nrow = n_balls, ncol = 3)
  alpha_players_rally <- matrix(rep(NA, n_balls), nrow = n_balls, ncol = 1)
  for(i in 1:n_balls){
    pos_players_rally[i,] <- c(runif(1,min_x0_rally, max_x0_rally),runif(1,min_y0_rally, max_y0_rally),runif(1,min_z0_rally, max_z0_rally))
    pos_opponents_rally[i,] <- c(runif(1, -12, -1), runif(1, min_y0_rally, max_y0_rally), runif(1, 2.7, 3.2))
    alpha_players_rally[i] <- DetermineAlphaRally(pos_players_rally[i,1:2], pos_opponents_rally[i,1:2])
  }
  
  ########################################
  ### GENERATE INCOMING BALLS AND SPIN ###
  ########################################
  v_incoming_training <- runif(n_balls, min_v0_incoming_rally, max_v0_incoming_rally)
  
  ##############################
  ### FEEDFORWARD AND OUTPUTS###
  ##############################
  outputs_players_rally <- matrix(rep(NA, n_balls*nOutputsR), ncol = nOutputsR)
  for(i in 1:n_balls){
    unnormalized_input_rally <- c(pos_players_rally[i,c(1,3)], v_incoming_training[i])
    if(pos_players_rally[i,1] < 0){
      temp_min_x0_rally <- -max_x0_rally
      temp_max_x0_rally <- min_x0_rally
    }else{
      temp_min_x0_rally <- min_x0_rally
      temp_max_x0_rally <- max_x0_rally
    }
    normalized_input_rally <- c(Normalize(temp_min_x0_rally, temp_max_x0_rally, unnormalized_input_rally[1]), 
                                Normalize(min_z0_rally, max_z0_rally, unnormalized_input_rally[2]),
                                Normalize(min_v0_incoming_rally, max_v0_incoming_rally, unnormalized_input_rally[3]))
    temp_feedforward_rally <- FeedForward(normalized_input_rally, dna, no_bias = no_bias_rally)*output_scalars_rally + c(min_theta_rally, min_w_ball_rally, min_w_racket_rally)
    outputs_players_rally[i,] <- temp_feedforward_rally
  }
  
  #######################
  ### BALL TRAJECTORY ###
  #######################
  balls_in_rally_check <- c()
  # png(paste0("Check_Rally/rplot_check.png"), width = 600, height = 450)
  # PlotTennisField(c(-25, 25), c(-10,10))
  for(i in 1:n_balls){
    # rand_col <- rgb(runif(1, 0, 255),runif(1, 0, 255),runif(1, 0, 255), max=255)
    # PlotOpponent(pos_opponents_rally[i,], rand_col)
    # PlotPlayer(pos_players_rally[i,], rand_col)
    spin <- 1
    v_spin <- R*outputs_players_rally[i,2]
    v0_rally <- max(VelocityBall(seq(0, 0.8, 0.05), v_incoming_training[i], outputs_players_rally[i,3]))
    initial_state_rally <- c(0, pos_players_rally[i,], v0_rally)
    alpha_rally <- alpha_players_rally[i,]
    theta_rally <- outputs_players_rally[i,1]
    sols_rally <- SolveEquationsOfMotionWithBounce(initial_state_rally, alpha_rally, theta_rally, v_spin, spin)
    # lines(sols_rally[[1]][,2], sols_rally[[1]][,3], col = rand_col)
    if(length(sols_rally)>2){
      # points(sols_rally[[3]][1], sols_rally[[3]][2], col = "green")
      # points(sols_rally[[5]][1], sols_rally[[5]][2], col = "red") 
      check_ball_field <- CheckBallInField(pos_players_rally[i,], sols_rally[[3]])
      balls_in_rally_check <- c(balls_in_rally_check, as.integer(check_ball_field[[1]]))
    }else{
      balls_in_rally_check <- c(balls_in_rally_check, 0)
    }
  }
  # dev.off()
  
  return(balls_in_rally_check)
}

###############################
### DEFINE GLOBAL VARIABLES ###
###############################
# Starting parameter of the serve training algorithm (n_serves=15 yields quick results)
generation_rally <- 1                  # Current generation
population_rally <- 10                 # Number of players is each generation
n_balls_x <- 5
n_balls_y <- 1
n_balls_z <- 5
n_balls <- n_balls_x*n_balls_y*n_balls_z    # Number of balls per player 

# track what the maximum number of balls in the field was per generation and save the values
max_balls_in_vec <- c()
max_balls_in <- 0

# Boolean to determine whether start the training and whether or not to save/check the training results
neural_network_is_training_rally <- TRUE
check_rally_dna <- TRUE

# Bias parameters or not
no_bias_rally <- FALSE

##########################################
### INITIALIZE SOME LISTS AND MATRICES ###
##########################################
output_scalars_rally <- c(max_theta_rally - min_theta_rally, max_w_ball_rally - min_w_ball_rally, max_w_racket_rally-min_w_racket_rally)
outputs_players_rally <- matrix(rep(NA, nOutputsR*n_balls),nrow = n_balls, ncol = nOutputsR)
pos_players_rally <- matrix(rep(NA, 3*n_balls), nrow = n_balls, ncol = 3)
pos_opponents_rally <- matrix(rep(NA, 3*n_balls), nrow = n_balls, ncol = 3)
alpha_players_rally <- matrix(rep(NA, n_balls), nrow = n_balls, ncol = 1)

######################################
### SET INITIAL DNA OF EACH PLAYER ###
###################################### 
population_dna_rally <- list()
for(k in 1:population_rally){
  population_dna_rally[[k]] <- CreateInitialDNAR(neurons = empty_neuronsR, no_bias = no_bias_rally) 
}

######################
### START TRAINING ###
######################
while(neural_network_is_training_rally){
  print(paste0("Generation: ", generation_rally))
  
  ##############################################
  ### SET THE PLAYER'S POSITIONS FOR EACH HIT###
  ##############################################
  x_pos_rally <- seq(min_x0_rally+0.5*(max_x0_rally-min_x0_rally)/n_balls_x, max_x0_rally-+0.5*(max_x0_rally-min_x0_rally)/n_balls_x, length.out = n_balls_x)
  y_pos_rally <- seq(min_y0_rally+0.5*(max_y0_rally-min_y0_rally)/n_balls_y, max_y0_rally-+0.5*(max_y0_rally-min_y0_rally)/n_balls_y, length.out = n_balls_y)
  z_pos_rally <- seq(0.5, 0.8, length.out = n_balls_z)
  combs_pos_rally <- as.matrix(expand.grid(x_pos_rally, y_pos_rally, z_pos_rally))

  data_population_rally <- list()
  for(k in 1:population_rally){
    height_opponent <- runif(1, 2.8, 3.1) # not really important, just for completeness
    for(i in 1:n_balls){
      y_opponent <- runif(1, -4, 4)
      pos_players_rally[i,] <- combs_pos_rally[i,]
      pos_opponents_rally[i,] <- c(runif(1, -12, -1), y_opponent, height_opponent)
      alpha_players_rally[i] <- DetermineAlphaRally(pos_players_rally[i,1:2], pos_opponents_rally[i,1:2])
    }
    data_population_rally[[k]] <- list(pos_players_rally, pos_opponents_rally, alpha_players_rally)
  }

  ########################################
  ### GENERATE INCOMING BALLS AND SPIN ###
  ########################################
  v_incoming_training <- matrix(runif(n_balls*population_rally, min_v0_incoming_rally, max_v0_incoming_rally), nrow = population_rally, ncol = n_balls)

  ##############################
  ### FEEDFORWARD AND OUTPUTS###
  ##############################
  outputs_population_rally <- list()
  for(k in 1:population_rally){
    for(i in 1:n_balls){
      unnormalized_input_rally <- c(data_population_rally[[k]][[1]][i,c(1,3)], v_incoming_training[k,i])
      normalized_input_rally <- c(Normalize(min_x0_rally, max_x0_rally, abs(unnormalized_input_rally[1])), 
                                  Normalize(min_z0_rally, max_z0_rally, unnormalized_input_rally[2]),
                                  Normalize(min_v0_incoming_rally, max_v0_incoming_rally, unnormalized_input_rally[3]))
      temp_feedforward_rally <- FeedForward(normalized_input_rally, population_dna_rally[[k]], no_bias = no_bias_rally)*output_scalars_rally + c(min_theta_rally, min_w_ball_rally, min_w_racket_rally)
      outputs_players_rally[i,] <- temp_feedforward_rally
    }
    outputs_population_rally[[k]] <- outputs_players_rally
  }
  
  #######################
  ### BALL TRAJECTORY ###
  #######################
  results_serves_rally <- list()
  results_population_rally <- list()
  for(k in 1:population_rally){
    # png(paste0("Generations_Rally/Generation (",generation_rally,")/rplot", k, ".png"), width = 600, height = 450)
    # PlotTennisField(c(-25, 25), c(-10,10))
    for(i in 1:n_balls){
      # rand_col <- rgb(runif(1, 0, 255),runif(1, 0, 255),runif(1, 0, 255), max=255)
      # PlotOpponent(data_population_rally[[k]][[2]][i,], rand_col)
      # PlotPlayer(data_population_rally[[k]][[1]][i,], rand_col)
      spin <- 1
      v_spin <- R*outputs_population_rally[[k]][i,2]
      v0_rally <- max(VelocityBall(seq(0, 0.8, 0.05), v_incoming_training[k,i], outputs_population_rally[[k]][i,3]))
      initial_state_rally <- c(0, data_population_rally[[k]][[1]][i,], v0_rally)
      alpha_rally <- data_population_rally[[k]][[3]][i]
      theta_rally <- outputs_population_rally[[k]][i,1]
      sols_rally <- SolveEquationsOfMotionWithBounce(initial_state_rally, alpha_rally, theta_rally, v_spin, spin)
      # lines(sols_rally[[1]][,2], sols_rally[[1]][,3], col = rand_col)
      # if(length(sols_rally)>2){
      #   points(sols_rally[[3]][1], sols_rally[[3]][2], col = "green")
      #   points(sols_rally[[5]][1], sols_rally[[5]][2], col = "red")
      # }
      results_serves_rally[[i]] <- sols_rally
    }
    # dev.off()
    results_population_rally[[k]] <- results_serves_rally
  }
  
  ############################################################
  ### FUNCTION THAT COMPUTES THE FITNESS OF THE POPULATION ###
  ############################################################
  fitness_player_rally <- matrix(rep(NA, 2*n_balls), nrow = n_balls, ncol = 2)
  fitness_population_rally <- list()
  for(k in 1:population_rally){
    for(i in 1:n_balls){
      size_list_rally <- length(results_population_rally[[k]][[i]])
      pos_player_rally <- data_population_rally[[k]][[1]][i,]
      length_sols_rally <- length(results_population_rally[[k]][[i]][[1]][,1])
      v_ground_rally <- sqrt(sum(results_population_rally[[k]][[i]][[1]][length_sols_rally,5:7]^2))
      if(size_list_rally == 2){
        pos_ball_ground_rally <- c(0,0,0)
        fitness_rally <- FitnessFunctionRally(pos_player_rally, pos_ball_ground_rally)
      }else{
        pos_ball_ground_rally <- results_population_rally[[k]][[i]][[3]]
        fitness_rally <- FitnessFunctionRally(pos_player_rally, pos_ball_ground_rally)
      }
      fitness_player_rally[i,] <- fitness_rally
    }
    fitness_population_rally[[k]] <- fitness_player_rally
  }
  
  #########################
  ### GENETIC ALGORITHM ###
  #########################
  # Determine which player is the fittest
  fitness_sums_rally <- matrix(rep(NA, population_rally), nrow = population_rally, ncol = 1)
  balls_in_rally <- matrix(rep(NA, population_rally), nrow = population_rally, ncol = 1)
  for(k in 1:population_rally){
    fitness_sums_rally[k] <-   colSums(fitness_population_rally[[k]])[1]
    balls_in_rally[k] <- colSums(fitness_population_rally[[k]])[2]
  }
  
  # Save the max number of balls in field
  max_balls_in_vec <- c(max_balls_in_vec, max(balls_in_rally))
  print(paste0(max(balls_in_rally), "/", n_balls))
  
  # Determine the two fittest players and get their DNA
  n_parents_rally <- 2
  parents_rally <- rep(NA, n_parents_rally)
  dna_parents_rally <- list()
  for(i in 1:n_parents_rally){
    parents_rally[i] <- which(fitness_sums_rally==sort(fitness_sums_rally)[i])[1]
    dna_parents_rally[[i]] <- population_dna_rally[[parents_rally[i]]]
  }
  
  # if a better player than the previous best player has been found then save the dna
  if(max(balls_in_rally)>max_balls_in){
    saveRDS(dna_parents_rally[[1]], "best_rally_topspin_dna.rds")
    max_balls_in <- max(balls_in_rally)
  }
  
  # Create the dna of the new generation using crossover between parent1 and parent2 and then mutate the dna
  new_population_dna_rally <- list()
  for(i in 1:population_rally){
    temp_dna <- CrossOverS(dna_parents_rally, amount = 2)
    new_population_dna_rally[[i]] <- MutationR(temp_dna)
  }
  population_dna_rally <- new_population_dna_rally
  generation_rally <- generation_rally + 1
  
  # Stop the training if at least one player hit all the balls in the field
  if(sum(as.integer(balls_in_rally==(n_balls))) > 0){
    neural_network_is_training_rally <- FALSE
    print("A good player has been found")
  } 
}

######################################
### CHECK IF THE FOUND DNA IS GOOD ###
######################################

if(check_rally_dna){
  dna <- readRDS("best_rally_topspin_dna.rds")
  check <- CheckRallyDna(dna, 1000)
  sum(check)
}

plot(1:length(max_balls_in_vec), max_balls_in_vec, type = "l", main = "Less vigorous mutation and less crossover", ylim = c(0,25), ylab = "max balls in field", xlab = "generation")
