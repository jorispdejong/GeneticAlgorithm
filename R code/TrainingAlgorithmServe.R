# Clear global environment (including hidden objects)
rm(list = ls(all.names = TRUE))

# Source
source("Equations of Motion.R")  # Numerically solves the equations of motion
source("Functions.R")            # Contains usefull functions, e.g. plotting the tennis field, computing the height of the tennis net
source("NeuralNetwork.R")   # Defines the Neural Network we will be using
source("DNA.R")             # Contains the Genetic Algorithm functions and the function to create the initial DNA 
source("Global_parameters.R")

########################
### DEFINE FUNCTIONS ###
########################
# Determine whether to play to ball on the left or right of the opponent during serve
HitBallToLeftOfOpponentServe <- function(pos_player_h, pos_opponent_h){
  p_left <- 1/(1+exp(-1.5*(abs(pos_player_h[2])-0.5*field_width)))
  
  # Determine left or right
  rand_1 <- runif(1)
  left <- rand_1 > p_left
  return(left)
}
# Determine alpha based on the position of the player and its opponent, using a skew probability distribution
DetermineAlphaServe <- function(pos_player_a, pos_opponent_a){
  max_angle_wide <- atan((0.2+field_width+abs(pos_player_a[[2]]))/(field_serve_length+field_baseline_length))*180/pi
  max_angle_mid <- atan((0.2+abs(pos_player_a[[2]]))/(field_serve_length+field_baseline_length))*180/pi
  angle_between_players_a <- atan((pos_opponent_a[2]-pos_player_a[2])/(abs(pos_opponent_a[1]-pos_player_a[1])))*180/pi
  
  if(angle_between_players_a > 0){
    alpha_bias_2_a <- max_angle_wide - angle_between_players_a
    alpha_bias_1_a <- angle_between_players_a - max_angle_mid
  }else{
    alpha_bias_2_a <- -max_angle_wide - angle_between_players_a
    alpha_bias_1_a <- angle_between_players_a + max_angle_mid
  }
  
  left <- HitBallToLeftOfOpponentServe(pos_player_a, pos_opponent_a)
  if(pos_player_a[2] < 0){
    if(left){
      alpha_bias_a <- -rbeta(1, skew_parameter_1, skew_parameter_2)*alpha_bias_1_a
    }else{
      alpha_bias_a <- rbeta(1, skew_parameter_1, skew_parameter_2)*alpha_bias_2_a
    }
  }else{
    if(left){
      alpha_bias_a <- -rbeta(1, skew_parameter_1, skew_parameter_2)*alpha_bias_1_a
    }else{
      alpha_bias_a <- rbeta(1, skew_parameter_1, skew_parameter_2)*alpha_bias_2_a
    }
  }
  
  return(angle_between_players_a + alpha_bias_a)
}
# Check dna for serve by performing n_rand_serves serves from random positions 
CheckServeResults <- function(dna_test, n_rand_serves){
  # Set the player's and opponent's random positions
  pos_players_c <- matrix(rep(NA, n_rand_serves * 3), nrow = n_rand_serves, ncol = 3)
  pos_opponents_c <- matrix(rep(NA, n_rand_serves * 3), nrow = n_rand_serves, ncol = 3)
  alpha_players_c <- matrix(rep(NA, n_rand_serves), nrow = n_rand_serves, ncol = 1)
  
  height_player_c <- runif(1, min_z0_serve, max_z0_serve)
  height_opponent_c <- runif(1, min_z0_serve, max_z0_serve)
  for(i in 1:n_rand_serves){
    if(runif(1)>0.5){
      y_player_c <-  runif(1,0.5,4)
    }else{
      y_player_c <- runif(1,-4,-0.5)
    }
    if(y_player_c < 0){
      y_opponent_c <- runif(1,3,4)
    }else{
      y_opponent_c <- runif(1,-4,-3)
    }
    pos_players_c[i,] <- c(field_baseline_length, y_player_c, height_player_c)
    pos_opponents_c[i,] <- c(-field_baseline_length, y_opponent_c, height_opponent_c)
    alpha_players_c[i] <- DetermineAlphaServe(pos_players_c[i,1:2], pos_opponents_c[i,1:2])
  }
  
  outputs_players_c <- matrix(rep(NA, n_rand_serves*nOutputsS), nrow = n_rand_serves, ncol = nOutputsS)
  for(i in 1:n_rand_serves){
    unnormalized_input_c <- c(pos_players_c[i,3], alpha_players_c[i])
    normalized_input_c <- c(Normalize(min_z0_serve, max_z0_serve, unnormalized_input_c[1]), 
                            Normalize(min_alpha_serve, max_alpha_serve, abs(unnormalized_input_c[2])))
    temp_feedforward_c <- FeedForward(normalized_input_c, dna_test, no_bias = no_bias_serve)*output_scalars + c(min_v0_serve, min_theta_serve, 0)
    outputs_players_c[i,] <- temp_feedforward_c  
  }
  
  results_player_c <- list()
  in_or_out <- rep(NA, n_rand_serves)
  # initial_state <- c(t0, x0, y0, z0, v0)
  for(i in 1:n_rand_serves){
    # png(paste0("Check_Serve/rplot", i, ".png"), width = 800, height = 600)
    # PlotTennisField(c(-25, 25), c(-10,10))
    # rand_col_c <- rgb(runif(1, 0, 255),runif(1, 0, 255),runif(1, 0, 255), max=255)
    # PlotOpponent(pos_opponents_c[i,], rand_col_c)
    # PlotPlayer(pos_players_c[i,], rand_col_c)
    spin <- 1
    v_spin_c <- R*outputs_players_c[i,3]
    initial_state_c <- c(0, pos_players_c[i,], outputs_players_c[i,1])
    alpha_c <- alpha_players_c[i]
    theta_c <- outputs_players_c[i,2]
    sols_c <- SolveEquationsOfMotionServe(initial_state_c, alpha_c, theta_c, v_spin_c, spin)
    if(length(sols_c)==2){
      in_or_out[[i]] <- FALSE  
    }else{
      check_serve <- CheckServeInField(pos_players_c[i,], sols_c[[3]])
      in_or_out[[i]] <- check_serve[[1]]
    }
    # lines(sols_c[[1]][,2], sols_c[[1]][,3], col = rand_col_c)
    # dev.off()
  }
  
  return(in_or_out)
}

###############################
### DEFINE GLOBAL VARIABLES ###
###############################
# Starting parameter of the serve training algorithm (n_serves=15 yields quick results)
generation <- 1                   # Current generation
population <- 10                  # Number of players is each generation
n_serves <- 30                    # Number of serves per player 

# Boolean to determine whether start the training and whether or not to save/check the training results
neural_network_is_training <- TRUE
save_dna_serve <- FALSE
check_serve_dna <- FALSE
save_json <- FALSE

# Bias parameters or not
no_bias_serve <- TRUE

######################################
### SET INITIAL DNA OF EACH PLAYER ###
###################################### 
population_dna <- list()
for(i in 1:population){
  population_dna[[i]] <- CreateInitialDNAS(neurons = empty_neuronsS, no_bias = no_bias_serve) 
}

##########################################
### INITIALIZE SOME LISTS AND MATRICES ###
##########################################
output_scalars <- c(max_v0_serve - min_v0_serve, max_theta_serve - min_theta_serve, max_w_ball_serve)
outputs_players <- matrix(rep(NA, nOutputsS*n_serves),nrow = n_serves, ncol = nOutputsS)
pos_players <- matrix(rep(NA, 3*n_serves), nrow = n_serves, ncol = 3)
pos_opponents <- matrix(rep(NA, 3*n_serves), nrow = n_serves, ncol = 3)
alpha_players <- rep(NA, n_serves)

# List for the JSON file
coords_time <- list()

# balls in trackers per player
balls_in_mat <- matrix(ncol = population, nrow = 0)

########################################
### SEARCH FOR A GOOD STARTING SERVE ###
########################################
while(neural_network_is_training){
  print(paste0("Generation: ", generation))
  
  ##########################################################################
  ### SET THE PLAYER'S POSITIONS FOR EACH SERVE AND  SET PLAYER'S HEIGHT ###
  ##########################################################################
  data_population <- list()
  for(k in 1:population){
    height_opponent <- runif(1, 1.6, 2) # Not necessary, merely for completeness 
    for(i in 1:n_serves){
      height_player <- runif(1, min_z0_serve, max_z0_serve)
      y_player <- -max_y0_serve + (2*i-1)*max_y0_serve/n_serves
      if(y_player < 0){
        y_opponent <- runif(1,min_y0_serve_receive,max_y0_serve_receive)
      }else{
        y_opponent <- runif(1,-max_y0_serve_receive,-min_y0_serve_receive)
      }
      pos_players[i,] <- c(field_baseline_length, y_player, height_player)
      pos_opponents[i,] <- c(-field_baseline_length, y_opponent, height_opponent)
      alpha_players[i] <- DetermineAlphaServe(pos_players[i,1:2], pos_opponents[i,1:2])
    }
    data_population[[k]] <- list(pos_players, pos_opponents, alpha_players)
  }
  
  ##############################
  ### FEEDFORWARD AND OUTPUTS###
  ##############################
  outputs_population <- list()
  for(k in 1:population){
    for(i in 1:n_serves){
      unnormalized_input <- c(data_population[[k]][[1]][i,3], data_population[[k]][[3]][i])
      normalized_input <- c(Normalize(min_z0_serve, max_z0_serve, unnormalized_input[1]), 
                            Normalize(min_alpha_serve, max_alpha_serve, abs(unnormalized_input[2])))
      temp_feedforward <- FeedForward(normalized_input, population_dna[[k]], no_bias = no_bias_serve)*output_scalars + c(min_v0_serve, min_theta_serve, 0)
      outputs_players[i,] <- temp_feedforward  
    }
    outputs_population[[k]] <- outputs_players
  }

  #######################
  ### BALL TRAJECTORY ###
  #######################
  print("Calculate and save the ball trajectories")
  results_serves <- list()
  results_population <- list()
  # initial_state <- c(t0, x0, y0, z0, v0)
  for(k in 1:population){
    # png(paste0("Generations_Serve/Generation (",generation,")/rplot", k, ".png"), width = 600, height = 450)
    # PlotTennisField(c(-17, 17), c(-7,7))
    for(i in 1:n_serves){
      # rand_col <- rgb(runif(1, 0, 255),runif(1, 0, 255),runif(1, 0, 255), max=255)
      # PlotOpponent(data_population[[k]][[2]][i,], rand_col)
      # PlotPlayer(data_population[[k]][[1]][i,], rand_col)
      v_spin <- R*outputs_population[[k]][i,3]
      initial_state <- c(0, data_population[[k]][[1]][i,], outputs_population[[k]][i,1])
      alpha <- data_population[[k]][[3]][i]
      theta <- outputs_population[[k]][i,2]
      sols <- SolveEquationsOfMotionServe(initial_state_e = initial_state, alpha_e = alpha, theta_e = theta, v_spin = v_spin, spin = 1)
      # lines(sols[[1]][,2], sols[[1]][,3], col = rand_col)
      results_serves[[i]] <- sols
    }
    # dev.off()
    results_population[[k]] <- results_serves
  }

  #######################
  ### CONVERT TO JSON ###
  #######################
  if(save_json){
    i_s <- c()
    for(i in 1:population){
      for(j in 1:n_serves){
        i_s <- c(i_s, length(results_population[[i]][[j]][[1]][,1]))
      }
    }
    largest_i <- max(i_s)
    for(i in 1:population){
      for(j in 1:n_serves){
        len <- length(results_population[[i]][[j]][[1]][,1])
        add_matrix <- matrix(rep(results_population[[i]][[j]][[1]][len,], largest_i-len), byrow = TRUE, ncol = 7)
        
        results_population[[i]][[j]][[1]] <- rbind(results_population[[i]][[j]][[1]], add_matrix)
        results_population[[i]][[j]][[1]][,1] <- seq(0,(length(results_population[[i]][[j]][[1]][,1])-1)*dt, dt)
      }
    }
    for(k in 1:length(results_population)){
      for(i in 1:largest_i){
        if(i%%15==0){
          xyz <- list()
          for(j in 1:length(results_population[[k]])){
            xyz <- append(xyz, list(list("x" = results_population[[k]][[j]][[1]][i,2],
                                         "y" = results_population[[k]][[j]][[1]][i,3],
                                         "z" = results_population[[k]][[j]][[1]][i,4])))
          }
          coords_time <- append(coords_time,
                                list(
                                  list("time"=results_population[[k]][[j]][[1]][i,1],
                                       "generation"=generation,
                                       "player"=k,
                                       "balls"=xyz))) 
        }
      }
    }
  }
  
  
  ############################################################
  ### FUNCTION THAT COMPUTES THE FITNESS OF THE POPULATION ###
  ############################################################
  fitness_player <- matrix(rep(NA, 2*n_serves), nrow = n_serves, ncol = 2)
  fitness_population <- list()
  for(k in 1:population){
    for(i in 1:n_serves){
      size_list <- length(results_population[[k]][[i]])
      ball_over_net <- results_population[[k]][[i]][[2]]
      pos_player <- data_population[[k]][[1]][i,]
      length_sols <- length(results_population[[k]][[i]][[1]][,1])
      v_ground <- sqrt(sum(results_population[[k]][[i]][[1]][length_sols,5:7]^2))
      if(size_list == 2){
        pos_ball_ground <- c(0,0,0)
        fitness <- FitnessFunctionServe(ball_over_net, pos_player, pos_ball_ground, v_ground)
      }else{
        pos_ball_ground <- results_population[[k]][[i]][[3]]
        fitness <- FitnessFunctionServe(ball_over_net, pos_player, pos_ball_ground, v_ground)
      }
      fitness_player[i,] <- fitness
    }
    fitness_population[[k]] <- fitness_player
  }
  
  #########################
  ### GENETIC ALGORITHM ###
  #########################
  # Determine which player is the fittest
  fitness_sums <- matrix(rep(NA, population), nrow = population, ncol = 1)
  serves_in <- matrix(rep(NA, population), nrow = population, ncol = 1)
  for(k in 1:population){
    fitness_sums[k] <-   colSums(fitness_population[[k]])[1]
    serves_in[k] <- colSums(fitness_population[[k]])[2]
  }
  
  balls_in_mat <- rbind(balls_in_mat, t(serves_in))

  for(i in 0:n_serves){
    print(paste0(length(serves_in[serves_in==i]), " players: ", i," serves in"))
  }
  
  # Determine the two fittest players and get their DNA
  n_parents <- 2
  parents <- rep(NA, n_parents)
  dna_parents <- list()
  for(i in 1:n_parents){
    parents[i] <- which(fitness_sums==sort(fitness_sums)[i])[1]
    dna_parents[[i]] <- population_dna[[parents[i]]]
  }

  # Genetic algorithm
  new_population_dna <- list()
  for(i in 1:population){
    if(generation<5){
      temp_dna <- CrossOverS(dna_parents, amount = 2)
    }else{
      if(generation%%10==0){
        temp_dna <- CrossOverS(dna_parents, amount = 1)
      }else{
        temp_dna <- CrossOverS(dna_parents, amount = 2)
      }
    }
    new_population_dna[[i]] <- MutationS(temp_dna)
  }
  population_dna <- new_population_dna
  generation <- generation + 1
  
  # Stop the training if at least one player hit all the balls in the field
  if(length((serves_in > (n_serves-1))[(serves_in > (n_serves-1))==TRUE]) > 0){
    neural_network_is_training <- FALSE
    print("A good starting player has been found")
  } 
}

# save data as a json file
if(save_json){
  # prepare scores
  scores <- list()
  for(j in 1:dim(balls_in_mat)[1]){
    scores_temp <- list()
    for(i in 1:dim(balls_in_mat)[2]){
      scores_temp <- append(scores_temp, balls_in_mat[j,i])
    }
    scores[[j]] <- list("score_per_player"=scores_temp)
  }
  coords_time_data <- list("n_players"=population, "balls_played_per_players"=n_serves, "scores"=scores, "coords_time"=coords_time)
  coords_time_data_json <- toJSON(coords_time_data)
  write(coords_time_data_json, file="Training_serve_data.json")
}

# save dna
if(save_dna_serve){
  saveRDS(dna_parents[[1]], "DNA/dna_serve.rds")
}

# check the dna for 1000 random balls
if(check_serve_dna){
  nn_serve <- readRDS("DNA/dna_serve.rds")
  serves_check <- CheckServeResults(nn_serve, 1000)
  sum(as.integer(serves_check))
}


