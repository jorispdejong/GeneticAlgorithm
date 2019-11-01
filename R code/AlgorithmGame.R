# Clear global environment (including hidden objects)
rm(list = ls(all.names = TRUE))

# Source R Files
source("Functions.R")
source("NeuralNetwork.R")
source("Equations of Motion.R")
source("Global_parameters.R")
source("Functions.R")

# Load neural networks
dna_serve <- readRDS("DNA/dna_serve.rds")  # Load dna of serve
dna_rally_topspin <- readRDS("DNA/dna_rally_topspin.rds")  # Load dna of rally

#################
### FUNCTIONS ###
#################
# Parameters for the distribution of determining left or right
skew_parameter_1_max <- 6            # Shape of the skew distribution
skew_parameter_1_min <- 2            # Shape of the skew distribution
skew_parameter_2_max <- 6            # Shape of the skew distribution
skew_parameter_2_min <- 2            # Shape of the skew distribution

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
  max_angle_wide <- atan((field_width+abs(pos_player_a[[2]]))/(field_serve_length+field_baseline_length))*180/pi
  max_angle_mid <- atan((abs(pos_player_a[[2]]))/(field_serve_length+field_baseline_length))*180/pi
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
      alpha_bias_a <- rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*alpha_bias_1_a
    }else{
      alpha_bias_a <- -rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*alpha_bias_1_a
    }
  }else{
    if(left){
      alpha_bias_a <- -rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*alpha_bias_1_a
    }else{
      alpha_bias_a <- rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*alpha_bias_2_a
    }
  }
  
  return(angle_between_players_a + alpha_bias_a)
}
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
DetermineAlphaRallyGame <- function(pos_player_a, pos_opponent_a){
  left <- HitBallToLeftOfOpponentRally(pos_opponent_a[2])

  angle_between_players_a <- atan((pos_opponent_a[2]-pos_player_a[2])/(abs(pos_opponent_a[1]-pos_player_a[1])))*180/pi
  
  angle_min <- atan((-field_width-pos_player_a[2])/(field_baseline_length+abs(pos_player_a[1])))*180/pi
  angle_max <- atan((field_width-pos_player_a[2])/(field_baseline_length+abs(pos_player_a[1])))*180/pi
  
  if(pos_player_a[1] > 0){
    if(left){
      if(pos_player_a[2] > 0){
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_max - angle_between_players_a)
      }else{
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_min - angle_between_players_a)
      }
    }else{
      if(pos_player_a[2] > 0){
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_min - angle_between_players_a)
      }else{
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_max - angle_between_players_a)
      }
    }
  }else{
    if(left){
      if(pos_player_a[2] > 0){
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_min - angle_between_players_a)
      }else{
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_max - angle_between_players_a)
      }
    }else{
      if(pos_player_a[2] > 0){
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_max - angle_between_players_a)
      }else{
        angle_new <- angle_between_players_a + rbeta(1, skew_parameter_1_max, skew_parameter_1_min)*(angle_min - angle_between_players_a)
      }
    }
  }
  
  return(angle_new)
}

####################################
### GLOBAL VARIABLES AND VECTORS ###
####################################
# Reaction time
r_time <- 0.1
# Playing time; how long it takes for the player to be able to run again, after hitting the ball
p_time <- 0.1
# Arm length + racket length
arm_racket_length <- 1

# Scalars
min_y0_receive <- 3
max_y0_receive <- field_width

# Vectors
output_scalars_serve <- c(max_v0_serve - min_v0_serve, max_theta_serve - min_theta_serve, max_w_ball_serve)
output_scalars_rally <- c(max_theta_rally - min_theta_rally, max_w_ball_rally - min_w_ball_rally, max_w_racket_rally-min_w_racket_rally)

# Convert to json or not
convert_json <- FALSE

#################
### FUNCTIONS ###
#################
PlayerMovementToBall <- function(pos_player, ball_traj){
  # Get bounce
  t_ground <- which.min(abs(ball_traj[,4]))
  ball_traj_after_bounce <- ball_traj[t_ground:length(ball_traj[,1]),]
  ball_traj_after_bounce <- ball_traj_after_bounce[1:which.max(ball_traj_after_bounce[,4]),]

  t_hit <- which.min(abs(ball_traj_after_bounce[,4]-0.6))
  
  ball_pos_hit <- ball_traj_after_bounce[t_hit, 2:4]
  distance_to_ball <- sqrt(sum((ball_pos_hit[1:2]-pos_player[1:2])^(2)))
  distance_needed <- min(distance_to_ball)-arm_racket_length
  t_needed <- TimePlayerNeedsToReachBall(abs(distance_needed),1)+r_time
  
  # How long it takes for the ball to be at the position where the player will hit the ball 
  t_ball <- t_hit+t_ground
  
  t_player_movement <- seq(0,t_needed, dt)
  dis_player_movement <- x_movement_player(t_player_movement)
  
  x_dif <- abs(pos_player[1]-ball_pos_hit[1])
  y_dif <- abs(pos_player[2]-ball_pos_hit[2])
  
  if(pos_player[2] > ball_pos_hit[2]){
    if(pos_player[1] > ball_pos_hit[1]){
      dis_player_movement_x <- pos_player[1] - cos(atan(y_dif/x_dif))*dis_player_movement
      dis_player_movement_y <- pos_player[2] - sin(atan(y_dif/x_dif))*dis_player_movement
    }else{
      dis_player_movement_x <- pos_player[1] + cos(atan(y_dif/x_dif))*dis_player_movement
      dis_player_movement_y <- pos_player[2] - sin(atan(y_dif/x_dif))*dis_player_movement
    }
  }else{
    if(pos_player[1] > ball_pos_hit[1]){
      dis_player_movement_x <- pos_player[1] - cos(atan(y_dif/x_dif))*dis_player_movement
      dis_player_movement_y <- pos_player[2] + sin(atan(y_dif/x_dif))*dis_player_movement
    }else{
      dis_player_movement_x <- pos_player[1] + cos(atan(y_dif/x_dif))*dis_player_movement
      dis_player_movement_y <- pos_player[2] + sin(atan(y_dif/x_dif))*dis_player_movement
    }
  }
  
  player_traj <- cbind(t_player_movement, dis_player_movement_x, dis_player_movement_y)
  new_ball_traj <- ball_traj[1:t_ball,]
  
  return(list("player_traj" = player_traj, "new_ball_traj"=new_ball_traj))
}
PlayerMovementToMid <- function(pos_player, time_get){
  if(pos_player[1]<0){
    mid_point <- c(-(field_baseline_length+3),0,0)
  }else{
    mid_point <- c(field_baseline_length+3,0,0)
  }
  
  distance_to_mid <- sqrt(sum((mid_point[1:2]-pos_player[1:2])^(2)))
  
  angle_1 <- acos(sum(pos_player[1:2]*mid_point[1:2])/(sqrt(sum(pos_player[1:2]*pos_player[1:2]))*sqrt(sum(mid_point[1:2]*mid_point[1:2]))))*180/pi
  angle_2 <- acos(((field_baseline_length+3)^(2)-distance_to_mid^(2)-sum(pos_player[1:2]^(2)))/(-2*distance_to_mid*sqrt(sum(pos_player[1:2]^(2)))))*180/pi
  
  t_needed <- TimePlayerNeedsToReachBall(distance_to_mid, 1)
  
  if(t_needed < (time_get-p_time)){
    if(t_needed < p_time){
      t_seq <- seq(0,t_needed, dt)
    }else{
      t_seq <- seq(0,t_needed - p_time, dt)
    }
  }else{
    t_seq <- seq(0,time_get - p_time, dt)
  }
  
  dis_player_movement_x <- x_movement_player(t_seq)*cos((angle_1+angle_2)*pi/180)
  dis_player_movement_y <- x_movement_player(t_seq)*sin((angle_1+angle_2)*pi/180)
  
  if(pos_player[1] > 0){
    dis_player_movement_x <- pos_player[1] - dis_player_movement_x 
  }else{
    dis_player_movement_x <- pos_player[1] + dis_player_movement_x 
  }
  if(pos_player[2]<mid_point[2]){
    dis_player_movement_y <- pos_player[2] + dis_player_movement_y
  }else{
    dis_player_movement_y <- pos_player[2] - dis_player_movement_y 
  }
  
  player_mov_mid <- cbind(t_seq, dis_player_movement_x, dis_player_movement_y)
  
  return(player_mov_mid)
}
PlayServe <- function(serve_side){
  double_fault_serve <- FALSE
  continue_point <- FALSE
  first_serve_in <- FALSE  # Determines if the first serve was in
  serve <- TRUE            # Determines if one of the players is still serving
  n_hits_serve <- 1        # Track how many hits the serve takes
  serve_traj <- list()
  
  while(serve){
    # Determine the starting positions of player 1 en player 2
    # Note that player 1 always begin on the right side of the baseline
    if(n_hits_serve==1){
      pos_player_1 <- matrix(c(field_baseline_length, serve_side*runif(1, min_y0_serve, max_y0_serve), runif(1, min_z0_serve, max_z0_serve)), nrow = 1, ncol = 3)
      pos_player_2 <- matrix(c(-field_baseline_length, serve_side*(-min_y0_receive + (pos_player_1[n_hits_serve,2]/(-max_y0_serve+min_y0_serve))*(abs(-max_y0_receive + min_y0_receive))), runif(1, min_z0_serve, max_z0_serve)), nrow = 1, ncol = 3)
    }else{
      pos_player_1 <- rbind(pos_player_1, c(field_baseline_length, serve_side*runif(1, min_y0_serve, max_y0_serve), runif(1, min_z0_serve, max_z0_serve)))
      pos_player_2 <- rbind(pos_player_2, c(-field_baseline_length, serve_side*(-min_y0_receive + (pos_player_1[n_hits_serve,2]/(-max_y0_serve+min_y0_serve))*(abs(-max_y0_receive + min_y0_receive))), runif(1, min_z0_serve, max_z0_serve)))
    }
    
    # Determine alpha
    alpha <- DetermineAlphaServe(pos_player_1[n_hits_serve,], pos_player_2[n_hits_serve,])
    
    # Normalize input variables
    unnormalized_input <- c(pos_player_1[n_hits_serve, 3], alpha)
    normalized_input <- c(Normalize(min_z0_serve, max_z0_serve, unnormalized_input[1]), 
                          Normalize(min_alpha_serve, max_alpha_serve, abs(unnormalized_input[2])))
    
    # Feed input to neural network and rescale
    output_serve <- FeedForward(normalized_input, dna_serve, no_bias = TRUE)*output_scalars_serve + c(min_v0_serve, min_theta_serve, 0)
    
    # Create the ball trajectory
    spin <- 1
    v_spin <- R*output_serve[3]
    initial_state <- c(0, pos_player_1[n_hits_serve,], output_serve[1])
    alpha <- alpha
    theta <- output_serve[2]
    sols <- SolveEquationsOfMotionWithBounce(initial_state_e = initial_state, alpha_e = alpha, theta_e = theta, v_spin = v_spin, spin)
    
    # Check if ball was in
    if(length(sols)==2){
      first_serve_in <- FALSE
    }else{
      check_serve <- CheckServeInField(pos_player_1[n_hits_serve,], sols[[3]])
      if(check_serve[[1]]){
        first_serve_in <- TRUE
      }
    }
    
    # Save the ball trajectory, regardless of whether the ball was in or not
    serve_traj[[n_hits_serve]] <- sols[[1]]
    
    # If the first serve was in then continue the point, otherwise, play a second serve
    if(!first_serve_in){
      if(n_hits_serve==2){
        serve <- FALSE
        double_fault_serve <- TRUE
      }else{
        n_hits_serve <- 2
      }
    }else{
      serve <- FALSE
      continue_point <- TRUE
    }
  }
  return(list("serve_traj"=serve_traj, "pos_player1"=pos_player_1, "pos_player2"=pos_player_2, "n_hits_serve"=n_hits_serve))
}
PlayRally <- function(ball_traj_temp, pos_player_off, pos_player_def, after_serve){
  # Make the player move
  player_mov_ball <- PlayerMovementToBall(pos_player_def, ball_traj_temp)
  # Actual ball trajectory
  ball_traj <- player_mov_ball[[2]] 
  # Set the new player coordinates
  prep_data_def <- cbind(player_mov_ball[[1]][,2:3],rep(pos_player_def[3],length(player_mov_ball[[1]][,2])))
  pos_player_def <- rbind(pos_player_def, prep_data_def)
  # Let other player walk back to the baseline middle
  player_mov_mid <- PlayerMovementToMid(pos_player_off, length(player_mov_ball[[2]][,1])*dt)
  prep_data_off <- cbind(player_mov_mid[,2:3],rep(pos_player_off[3],length(player_mov_mid[,2])))
  pos_player_off <- rbind(pos_player_off, prep_data_off)
  # Hit ball back
  pos_player_current <- ball_traj[length(ball_traj[,1]),2:4]
  pos_opponent_current <- pos_player_off[length(pos_player_off[,1]),]
  alpha_current <- DetermineAlphaRallyGame(pos_player_current, pos_opponent_current)
  v_incoming <- sqrt(sum(ball_traj[length(ball_traj[,1]),5:7]^2))
  # Feedforward 
  unnormalized_input_game <- c(ball_traj[length(ball_traj[,1]),c(2,4)], v_incoming)
  normalized_input_game <- c(Normalize(min_x0_rally, max_x0_rally, abs(unnormalized_input_game[1])), 
                             Normalize(min_z0_rally, max_z0_rally, unnormalized_input_game[2]),
                             Normalize(min_v0_incoming_rally, max_v0_incoming_rally, unnormalized_input_game[3]))
  feedforward_game <- FeedForward(normalized_input_game, dna_rally_topspin, no_bias = FALSE)*output_scalars_rally + c(min_theta_rally, min_w_ball_rally, min_w_racket_rally)
  # Create new ball trajectory
  spin <- 1
  v_spin <- R*feedforward_game[2]
  if(after_serve){
    v0_rally <- max(VelocityBall(seq(0, 0.8, 0.05), v_incoming, 10))
  }else{
    v0_rally <- max(VelocityBall(seq(0, 0.8, 0.05), v_incoming, feedforward_game[3]))
  }
  initial_state_rally <- c(0, ball_traj[length(ball_traj[,1]),2:4], v0_rally)
  theta_rally <- feedforward_game[1]
  sols_rally <- SolveEquationsOfMotionWithBounce(initial_state_rally, alpha_current, theta_rally, v_spin, spin)
  
  return(list(ball_traj, sols_rally, pos_player_off, pos_player_def))  
}
PlayPoint <- function(serve_side, visualize = FALSE){
  if(serve_side!=1 && serve_side!=-1) stop("serve_side must be either 1 (right from point of view of serving player) or -1 (left ...)")
  
  # Create empty lists, which will store the trajectories during the point
  point_ball_traj <- list() # List with trajectories of the ball, per hit
  point_player1_traj <- list() # List with trajectories of the ball, per hit
  point_player2_traj <- list() # List with trajectories of the ball, per hit
  hit_i <- 1 
  
  two_serves_needed <- FALSE
  
  # Serve the ball
  serve <- PlayServe(serve_side)
  # Get the last ball trajectory
  ball_traj_temp <- serve[[1]][[length(serve[[1]])]]
  # Get the last player positions
  pos_player_1 <- serve[[2]]
  pos_player_2 <- serve[[3]]
  if(length(serve[[1]])==2){
    point_ball_traj[[hit_i]] <- serve[[1]][[1]]
    point_player1_traj[[hit_i]] <- pos_player_1[1,]
    point_player2_traj[[hit_i]] <- pos_player_2[1,]
    hit_i <- hit_i + 1
    two_serves_needed <- TRUE
  }
  
  # Start the rally
  ball_in <- TRUE
  player_1_is_offensive <- TRUE
  while(ball_in){
    if(player_1_is_offensive){
      pos_player_offensive <- pos_player_1[length(pos_player_1[,1]),]
      pos_player_defensive <- pos_player_2[length(pos_player_2[,1]),]
    }else{
      pos_player_offensive <- pos_player_2[length(pos_player_2[,1]),]
      pos_player_defensive <- pos_player_1[length(pos_player_1[,1]),]
    }
    if(hit_i==1){
      rally_1 <- PlayRally(ball_traj_temp, pos_player_offensive, pos_player_defensive, after_serve = TRUE)
    }else{
      rally_1 <- PlayRally(ball_traj_temp, pos_player_offensive, pos_player_defensive, after_serve = FALSE)
    }
    # Save the ball and players trajectories
    point_ball_traj[[hit_i]] <- rally_1[[1]]
    new_pos_player_offensive <- rally_1[[3]]
    new_pos_player_defensive <- rally_1[[4]]
    if(player_1_is_offensive){
      point_player1_traj[[hit_i]] <- new_pos_player_offensive
      point_player2_traj[[hit_i]] <- new_pos_player_defensive
    }else{
      point_player1_traj[[hit_i]] <- new_pos_player_defensive
      point_player2_traj[[hit_i]] <- new_pos_player_offensive
    }
    
    hit_i <- hit_i + 1
    # new trajectory of the ball
    new_ball_traj_temp <- rally_1[[2]]
    if(length(new_ball_traj_temp)==2){
      ball_in <- FALSE
    }else{
      check_in <- CheckBallInField(pos_player_defensive, new_ball_traj_temp[[3]])
      if(check_in[[1]]==FALSE){
        ball_in <- FALSE
      }else{
        if(player_1_is_offensive){
          pos_player_1 <- new_pos_player_offensive
          pos_player_2 <- new_pos_player_defensive
          player_1_is_offensive <- FALSE
        }else{
          pos_player_1 <- new_pos_player_defensive
          pos_player_2 <- new_pos_player_offensive
          player_1_is_offensive <- TRUE
        }
      }
      ball_traj_temp <- new_ball_traj_temp[[1]]
    }
    
    if(!ball_in){
      point_ball_traj[[hit_i]] <- new_ball_traj_temp[[1]]
    }
    
    if(hit_i > 100){
      ball_in <- FALSE
    }
  }
  
  # Determine who won the point
  if(two_serves_needed){
    if(length(point_ball_traj)%%2==0){
      player_who_won_point <- 2
    }else{
      player_who_won_point <- 1
    }
  }else{
    if(length(point_ball_traj)%%2==0){
      player_who_won_point <- 1
    }else{
      player_who_won_point <- 2
    }
  }
  
  # Visualize the ball trajectories and the player movements
  if(visualize){
    PlotTennisField(c(-20,20), c(-7,7))
    for(i in 1:length(point_ball_traj)){
      lines(point_ball_traj[[i]][,2], point_ball_traj[[i]][,3])
    }
    
    for(i in 1:length(point_player1_traj)){
      lines(point_player1_traj[[i]][,1], point_player1_traj[[i]][,2], lwd=3)
    }
    
    for(i in 1:length(point_player2_traj)){
      lines(point_player2_traj[[i]][,1], point_player2_traj[[i]][,2], lwd=3, col="red")
    }
  }
  
  return(list("point_ball_traj"=point_ball_traj, "point_player1_traj"=point_player1_traj, "point_player2_traj"=point_player2_traj, "player_who_won_point"=player_who_won_point))
}
PlayGame <- function(){
  # Game algorithm
  game_over <- FALSE
  # Game always starts on the right serve side (serve_side=1)
  serve_side <- 1
  game_score <- matrix(c(0,0), nrow=1, ncol=2)
  hit_point <- 1 # track number of points played
  game_traj <- list() # List with the trajectories of each point
  player_who_won_game <- c() 
  while(!game_over){
    print(hit_point)
    play_point <- PlayPoint(serve_side)
    game_traj[[hit_point]] <- list("ball_traj"=play_point[[1]], "player1_traj"=play_point[[2]], "player2_traj"=play_point[[3]])
    hit_point <- hit_point + 1
    serve_side <- -1*serve_side
    if(play_point[[4]]==1){
      if(game_score[length(game_score[,1]),1]==45 && game_score[length(game_score[,2]),2]<45){
        game_score <- rbind(game_score, c(100, game_score[length(game_score[,2]),2]))
        player_who_won_game <- 1
        game_over <- TRUE
      }
      else if(game_score[length(game_score[,1]),1]==60 && game_score[length(game_score[,2]),2]==45){
        game_score <- rbind(game_score, c(100, game_score[length(game_score[,2]),2]))
        player_who_won_game <- 1
        game_over <- TRUE
      }
      else if(game_score[length(game_score[,1]),1]==45 && game_score[length(game_score[,2]),2]==60){
        game_score <- rbind(game_score, c(game_score[length(game_score[,1]),1], game_score[length(game_score[,2]),2]-15))
      }else{
        game_score <- rbind(game_score, c(game_score[length(game_score[,1]),1]+15, game_score[length(game_score[,2]),2]))
      }
    }else{
      if(game_score[length(game_score[,2]),2]==45 && game_score[length(game_score[,1]),1]<45){
        game_score <- rbind(game_score, c(game_score[length(game_score[,1]),1], 100))
        player_who_won_game <- 2
        game_over <- TRUE
      }
      else if(game_score[length(game_score[,2]),2]==60 && game_score[length(game_score[,1]),1]==45){
        game_score <- rbind(game_score, c(game_score[length(game_score[,1]),1], 100))
        player_who_won_game <- 2
        game_over <- TRUE
      }
      else if(game_score[length(game_score[,2]),2]==45 && game_score[length(game_score[,1]),1]==60){
        game_score <- rbind(game_score, c(game_score[length(game_score[,1]),1]-15, game_score[length(game_score[,2]),2]))
      }else{
        game_score <- rbind(game_score, c(game_score[length(game_score[,1]),1], game_score[length(game_score[,2]),2]+15))
      }  
    }
  }
  game_score <- game_score[2:length(game_score[,1]),]
  
  return(list("game_score"=game_score, "game_traj"=game_traj))
}

###################
### PLAY A GAME ###
###################
# play the game
play_game <- PlayGame()
# see (save) the score
(game_score <- play_game[[1]])
# Plot the game
for(j in 1:length(play_game[[2]])){
  PlotTennisField(c(-20,20), c(-7,7))
  for(i in 1:length(play_game[[2]][[j]][[1]])){
    lines(play_game[[2]][[j]][[1]][[i]][,2], play_game[[2]][[j]][[1]][[i]][,3])
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<0.5){}
  }
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<4){}
}

#######################
### CONVERT TO JSON ###
#######################
if(convert_json){
  # flatten the ball_traj
  ball_traj_total <- list()
  for(j in 1:length(play_game[[2]])){
    ball_traj_total[[j]] <- play_game[[2]][[j]][[1]][[1]]
    for(i in 2:length(play_game[[2]][[j]][[1]])){
      ball_traj_total[[j]] <- rbind(ball_traj_total[[j]], play_game[[2]][[j]][[1]][[i]])
    }
  }
  
  # Adjust time
  for(i in 1:length(ball_traj_total)){
    ball_traj_total[[i]][,1] <- seq(0,(length(ball_traj_total[[i]][,1])-1)*dt, dt)
  }
  
  # Create json structure
  coords_time <- list()
  for(k in 1:length(ball_traj_total)){
    for(i in 1:length(ball_traj_total[[k]][,1])){
      if((i-1)%%15==0){
        coords_time <- append(coords_time,
                              list(
                                list("time"=ball_traj_total[[k]][i,1],
                                     "point"=k,
                                     "balls"=list(list(
                                       "x" = ball_traj_total[[k]][i,2],
                                       "y" = ball_traj_total[[k]][i,3],
                                       "z" = ball_traj_total[[k]][i,4]
                                     ))
                                )
                              )
        ) 
      }
    }
  }
  
  coords_time_game <- list("coords_time"=coords_time)
  coords_time_game_json <- toJSON(coords_time_game)
  write(coords_time_game_json, file="Game.json")
}
