# Clear global environment (including hidden objects)
rm(list = ls(all.names = TRUE))

# libraries
library("neuralnet")
library("rjson")

# Sourcing
source("Functions.R")            # Contains usefull functions, e.g. plotting the tennis field, computing the height of the tennis net
source("Equations of Motion.R")  # Numerically solves the equations of motion
source("TennisStats.R")

# Functions
sigmoid <- function(x) 1/(1+exp(-x))
NormalizeData <- function(df, lim_mat){
  new_df <- matrix(rep(NA, dim(df)[1] * dim(df)[2]), nrow = dim(df)[1], ncol = dim(df)[2])
  size <- dim(df)[2]
  for(i in 1:size){
    new_df[,i] <- apply(t(df[,i]), 1, function(x)(x-lim_mat[i,1])/(lim_mat[i,2]-lim_mat[i,1]))
  }
  return(new_df)
}

# We're going to create multiple datasets
# 1) Normal rally (spin=1); player wants to hit the ball as fast as possible near the baseline (topspin)
# 2) Normal rally (spin=-1); player wants to hit the ball as fast as possible near the baseline (backspin)
# 3) Rally short; (spin=-1); player hits the ball just over the net (backspin)

create_normal_rally_1 <- FALSE
create_normal_rally_2 <- FALSE
create_rally_short <- FALSE

write_table_normal_rally_1 <- FALSE
write_table_normal_rally_2 <- FALSE
write_table_rally_short <- FALSE

train_rally_short_bool <- FALSE
train_rally_normal_topspin_bool <- FALSE
train_rally_normal_backspin_bool <- FALSE

#############################
### NORMAL RALLY (spin=1) ###
#############################
system.time(if(create_normal_rally_1){
  print("Creating a training data set for normal rally topspin")
  
  training_data_rally_normal_1 <- matrix(nrow = 0, ncol = 11)
  colnames(training_data_rally_normal_1) <- c("x", "y", "z", "alpha", "spin", "theta", "v_incoming", "w_ball", "w_racket", "x_ground", "y_ground")
  
  # Define parameters
  n_xT <- 7
  n_yT <- 7
  n_zT <- 7
  n_vT <- 30
  n_alphaT <- 7
  n_wT <- 30
  n_v0T <- n_wT*n_vT
  n_v0_w <- 10
  
  min_velo_1 <- 25
  max_velo_1 <- max_v0_rally
  
  min_v_inc <- 15
  max_v_inc <- 40
  
  min_w <- 10
  max_w <- 100
  
  min_zT <- 0.5
  max_zT <- 1.5
  # Create positions all over the field with corresponding alphas
  xT <- seq(0, field_baseline_length, field_baseline_length/(n_xT-1))+2
  yT <- seq(-field_width, field_width, 2*field_width/(n_yT-1))
  zT <- seq(min_zT, max_zT, (max_zT-min_zT)/(n_zT-1))
  pos_playersT <- as.matrix(expand.grid(xT, yT))
  angle_1T <- as.matrix(atan((field_width-pos_playersT[,2])/(field_baseline_length + abs(pos_playersT[,1])))*180/pi)
  angle_2T <- as.matrix(atan((-field_width-pos_playersT[,2])/(field_baseline_length + abs(pos_playersT[,1])))*180/pi)
  alphaT <- matrix(rep(NA, (n_alphaT+1)*length(angle_1T)), nrow = length(angle_1T), ncol = n_alphaT+1)
  for(k in 1:length(angle_1T)){
    step <- (angle_1T[k]-angle_2T[k])/n_alphaT
    for(i in 1:n_alphaT){
      alphaT[k,i] <- angle_1T[k] - (i-1)*step
    }
    alphaT[k,n_alphaT+1] <- angle_2T[k,]
  }
  
  posxyz_alpha <- matrix(nrow = 0, ncol = 4)
  colnames(posxyz_alpha) <- c("x", "y", "z", "alpha")
  for(i in 1:dim(pos_playersT)[1]){
    temp <- as.matrix(expand.grid(pos_playersT[i,1], pos_playersT[i,2], zT, alphaT[i,]))
    posxyz_alpha <- rbind(posxyz_alpha, temp)
  }
  
  # Determine a set of initial velocities and angular velocities racket
  v_incomingT <- seq(min_v_inc, max_v_inc, (max_v_inc-min_v_inc)/(n_vT-1))
  wT <- seq(min_w, max_w, (max_w-min_w)/(n_wT-1))
  v0T <- matrix(rep(NA, 3*n_wT*n_vT), nrow = n_wT*n_vT, ncol = 3)
  colnames(v0T) <- c("v0", "v_incoming", "w_racket")
  for(j in 1:n_vT){
    for(i in 1:n_wT){
      v0T[i + n_wT*(j-1),] <- c(max(VelocityBall(seq(0, 0.8, 0.005), v_incomingT[j], wT[i])), v_incomingT[j], wT[i])
    }
  }
  v0T <- v0T[(v0T[,1]>min_velo_1),]
  v0T <- v0T[(v0T[,1]<max_v0_rally),]
  # Take a random sample of n_v0_w combinations of v0 and w
  v0T <- v0T[sample(1:dim(v0T)[1], n_v0_w),]
  
  # Loop over values of theta and v0 to find a good trajectory
  # When a good trajectory has been found the data is stored in the matrix "training_data_rally_normal_1"
  found_training_ball <- FALSE
  pos_alpha_training <- FALSE
  pos_i <- 1
  thetas <- seq(-5, 15, 1)
  thetas_high <- seq(5, 15, 1)
  
  while(!pos_alpha_training){
    print(paste0("pos: ",pos_i))
    velo_i <- 1
    training_data_rally_normal_1_temp <- matrix(nrow = 0, ncol = 10)
    if(posxyz_alpha[pos_i,3] < 0.9){
      while(!found_training_ball){
        thetas_high <- sample(thetas_high)
        for(i in 1:length(thetas_high)){
          # Per position
          initial_state <- c(0, posxyz_alpha[pos_i,1:3], v0T[velo_i,1])
          alpha <- posxyz_alpha[pos_i,4]
          theta <- thetas[i]
          w_ball <- runif(1, 100, 500)
          v_spin <- R*w_ball
          spin <- 1
          eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)
          if(length(eq_mo_1)!=2){
            check_eq <- CheckBallInField(posxyz_alpha[pos_i,1:3], eq_mo_1[[3]])
            if(check_eq[[1]] && (eq_mo_1[[3]][1]+field_baseline_length) < 4){
              training_data_rally_normal_1 <- rbind(training_data_rally_normal_1, c(posxyz_alpha[pos_i,], spin, theta, v0T[velo_i,2], w_ball, v0T[velo_i,3], eq_mo_1[[3]]))
              found_training_ball <- TRUE
              break
            }
          }
        }
        if(velo_i == n_v0_w){
          found_training_ball <- TRUE
        }else{
          velo_i <- velo_i + 1
        }
      }
    }else{
      while(!found_training_ball){
        thetas <- sample(thetas)
        for(i in 1:length(thetas)){
          # Per position
          initial_state <- c(0, posxyz_alpha[pos_i,1:3], v0T[velo_i,1])
          alpha <- posxyz_alpha[pos_i,4]
          theta <- thetas[i]
          w_ball <- runif(1, 100, 500)
          v_spin <- R*w_ball
          spin <- 1
          eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)
          if(length(eq_mo_1)!=2){
            check_eq <- CheckBallInField(posxyz_alpha[pos_i,1:3], eq_mo_1[[3]])
            if(check_eq[[1]] && (eq_mo_1[[3]][1]+field_baseline_length) < 4){
              training_data_rally_normal_1 <- rbind(training_data_rally_normal_1, c(posxyz_alpha[pos_i,], spin, theta, v0T[velo_i,2], w_ball, v0T[velo_i,3], eq_mo_1[[3]]))
              found_training_ball <- TRUE
              break
            }
          }
        }
        if(velo_i == n_v0_w){
          found_training_ball <- TRUE
        }else{
          velo_i <- velo_i + 1
        }
      }
    }
    
    if(pos_i == dim(posxyz_alpha)[1]){
      pos_alpha_training <- TRUE
    }else{
      pos_i <- pos_i + 1
      found_training_ball <- FALSE
    }
  }
  
  if(write_table_normal_rally_1){
    write.table(training_data_rally_normal_1, "Training_Data_Normal_Rally_Topspin.txt", sep = "\t")
  }
})

#############################
### NORMAL RALLY (spin=-1) ###
#############################
system.time(if(create_normal_rally_2){
  print("Creating a training data set for normal rally backspin")
  
  training_data_rally_normal_2 <- matrix(nrow = 0, ncol = 11)
  colnames(training_data_rally_normal_2) <- c("x", "y", "z", "alpha", "spin", "theta", "v_incoming", "w_ball", "w_racket", "x_ground", "y_ground")
  
  # Define parameters
  n_xT <- 7
  n_yT <- 7
  n_zT <- 7
  n_vT <- 30
  n_alphaT <- 7
  n_wT <- 30
  n_v0T <- n_wT*n_vT
  n_v0_w <- 10
  
  min_velo_1 <- 25
  max_velo_1 <- max_v0_rally
  
  min_zT <- 0.75
  max_zT <- 1.5
  # Create positions all over the field with corresponding alphas
  xT <- seq(0, field_baseline_length, field_baseline_length/(n_xT-1))+2
  yT <- seq(-field_width, field_width, 2*field_width/(n_yT-1))
  zT <- seq(min_zT, max_zT, (max_zT-min_zT)/(n_zT-1))
  pos_playersT <- as.matrix(expand.grid(xT, yT))
  angle_1T <- as.matrix(atan((field_width-pos_playersT[,2])/(field_baseline_length + abs(pos_playersT[,1])))*180/pi)
  angle_2T <- as.matrix(atan((-field_width-pos_playersT[,2])/(field_baseline_length + abs(pos_playersT[,1])))*180/pi)
  alphaT <- matrix(rep(NA, (n_alphaT+1)*length(angle_1T)), nrow = length(angle_1T), ncol = n_alphaT+1)
  for(k in 1:length(angle_1T)){
    step <- (angle_1T[k]-angle_2T[k])/n_alphaT
    for(i in 1:n_alphaT){
      alphaT[k,i] <- angle_1T[k] - (i-1)*step
    }
    alphaT[k,n_alphaT+1] <- angle_2T[k,]
  }
  
  posxyz_alpha <- matrix(nrow = 0, ncol = 4)
  colnames(posxyz_alpha) <- c("x", "y", "z", "alpha")
  for(i in 1:dim(pos_playersT)[1]){
    temp <- as.matrix(expand.grid(pos_playersT[i,1], pos_playersT[i,2], zT, alphaT[i,]))
    posxyz_alpha <- rbind(posxyz_alpha, temp)
  }
  
  # Determine a set of initial velocities and angular velocities racket
  v_incomingT <- seq(10, 50, 40/(n_vT-1))
  wT <- seq(10, 100, 90/(n_wT-1))
  v0T <- matrix(rep(NA, 3*n_wT*n_vT), nrow = n_wT*n_vT, ncol = 3)
  colnames(v0T) <- c("v0", "v_incoming", "w_racket")
  for(j in 1:n_vT){
    for(i in 1:n_wT){
      v0T[i + n_wT*(j-1),] <- c(max(VelocityBall(seq(0, 0.8, 0.005), v_incomingT[j], wT[i])), v_incomingT[j], wT[i])
    }
  }
  v0T <- v0T[(v0T[,1]>min_velo_1),]
  v0T <- v0T[(v0T[,1]<max_v0_rally),]
  # Take a random sample of n_v0_w combinations of v0 and w
  v0T <- v0T[sample(1:dim(v0T)[1], n_v0_w),]
  
  # Loop over values of theta and v0 to find a good trajectory
  # When a good trajectory has been found the data is stored in the matrix "training_data_rally_normal_1"
  found_training_ball <- FALSE
  pos_alpha_training <- FALSE
  pos_i <- 1
  thetas <- seq(-5, 15, 1)
  thetas_high <- seq(5, 15, 1)
  
  while(!pos_alpha_training){
    print(paste0("pos: ",pos_i))
    velo_i <- 1
    if(posxyz_alpha[pos_i,3] < 0.9){
      while(!found_training_ball){
        thetas_high <- sample(thetas_high)
        for(i in 1:length(thetas_high)){
          # Per position
          initial_state <- c(0, posxyz_alpha[pos_i,1:3], v0T[velo_i,1])
          alpha <- posxyz_alpha[pos_i,4]
          theta <- thetas[i]
          w_ball <- runif(1, 100, 500)
          v_spin <- R*w_ball
          spin <- -1
          eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)
          if(length(eq_mo_1)!=2){
            check_eq <- CheckBallInField(posxyz_alpha[pos_i,1:3], eq_mo_1[[3]])
            if(check_eq[[1]] && (eq_mo_1[[3]][1]+field_baseline_length) < 4){
              training_data_rally_normal_2 <- rbind(training_data_rally_normal_2, c(posxyz_alpha[pos_i,], spin, theta, v0T[velo_i,2], w_ball, v0T[velo_i,3], eq_mo_1[[3]]))
              found_training_ball <- TRUE
              break
            }
          }
        }
        if(velo_i == n_v0_w){
          found_training_ball <- TRUE
        }else{
          velo_i <- velo_i + 1
        }
      }
    }else{
      while(!found_training_ball){
        thetas <- sample(thetas)
        for(i in 1:length(thetas)){
          # Per position
          initial_state <- c(0, posxyz_alpha[pos_i,1:3], v0T[velo_i,1])
          alpha <- posxyz_alpha[pos_i,4]
          theta <- thetas[i]
          w_ball <- runif(1, 100, 500)
          v_spin <- R*w_ball
          spin <- -1
          eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)
          if(length(eq_mo_1)!=2){
            check_eq <- CheckBallInField(posxyz_alpha[pos_i,1:3], eq_mo_1[[3]])
            if(check_eq[[1]] && (eq_mo_1[[3]][1]+field_baseline_length) < 4){
              training_data_rally_normal_2 <- rbind(training_data_rally_normal_2, c(posxyz_alpha[pos_i,], spin, theta, v0T[velo_i,2], w_ball, v0T[velo_i,3], eq_mo_1[[3]]))
              found_training_ball <- TRUE
              break
            }
          }
        }
        if(velo_i == n_v0_w){
          found_training_ball <- TRUE
        }else{
          velo_i <- velo_i + 1
        }
      }
    }
    
    if(pos_i == dim(posxyz_alpha)[1]){
      pos_alpha_training <- TRUE
    }else{
      pos_i <- pos_i + 1
      found_training_ball <- FALSE
    }
  }
  
  if(write_table_normal_rally_2){
    write.table(training_data_rally_normal_2, "Training_Data_Normal_Rally_Backspin.txt", sep = "\t")
  }
})


#############################
### RALLY SHORT (spin=-1) ###
#############################
system.time(if(create_rally_short){
  print("Creating a training data set for rally short")
  
  training_data_rally_short <- matrix(nrow = 0, ncol = 11)
  colnames(training_data_rally_short) <- c("x", "y", "z", "alpha", "spin", "theta", "v_incoming", "w_ball", "w_racket", "x_ground", "y_ground")
  
  # Define parameters
  n_xT <- 7
  n_yT <- 7
  n_zT <- 7
  n_vT <- 30
  n_alphaT <- 7
  n_wT <- 30
  n_v0T <- n_wT*n_vT
  n_v0_w <- 40
  
  min_velo_1 <- 5
  max_velo_1 <- 25
  
  min_zT <- 0.5
  max_zT <- 1.5
  
  min_x <- 0
  
  # Create positions all over the field with corresponding alphas
  xT <- seq(min_x, field_baseline_length, (field_baseline_length-min_x)/(n_xT-1))+2
  yT <- seq(-field_width, field_width, 2*field_width/(n_yT-1))
  zT <- seq(min_zT, max_zT, (max_zT-min_zT)/(n_zT-1))
  pos_playersT <- as.matrix(expand.grid(xT, yT))
  angle_1T <- as.matrix(atan((field_width-pos_playersT[,2])/(field_baseline_length + abs(pos_playersT[,1])))*180/pi)
  angle_2T <- as.matrix(atan((-field_width-pos_playersT[,2])/(field_baseline_length + abs(pos_playersT[,1])))*180/pi)
  alphaT <- matrix(rep(NA, (n_alphaT+1)*length(angle_1T)), nrow = length(angle_1T), ncol = n_alphaT+1)
  for(k in 1:length(angle_1T)){
    step <- (angle_1T[k]-angle_2T[k])/n_alphaT
    for(i in 1:n_alphaT){
      alphaT[k,i] <- angle_1T[k] - (i-1)*step
    }
    alphaT[k,n_alphaT+1] <- angle_2T[k,]
  }
  
  posxyz_alpha <- matrix(nrow = 0, ncol = 4)
  colnames(posxyz_alpha) <- c("x", "y", "z", "alpha")
  for(i in 1:dim(pos_playersT)[1]){
    temp <- as.matrix(expand.grid(pos_playersT[i,1], pos_playersT[i,2], zT, alphaT[i,]))
    posxyz_alpha <- rbind(posxyz_alpha, temp)
  }
  
  # Determine a set of initial velocities and angular velocities racket
  v_incomingT <- seq(0, max_v0_rally, (max_v0_rally-10)/(n_vT-1))
  wT <- seq(0, 30, 20/(n_wT-1))
  v0T <- matrix(rep(NA, 3*n_wT*n_vT), nrow = n_wT*n_vT, ncol = 3)
  colnames(v0T) <- c("v0", "v_incoming", "w_racket")
  for(j in 1:n_vT){
    for(i in 1:n_wT){
      v0T[i + n_wT*(j-1),] <- c(max(VelocityBall(seq(0, 0.8, 0.005), v_incomingT[j], wT[i])), v_incomingT[j], wT[i])
    }
  }
  v0T <- v0T[(v0T[,1]>min_velo_1),]
  v0T <- v0T[(v0T[,1]<max_velo_1),]
  v0T <- v0T[order(v0T[,2]),]
  # Take a random sample of n_v0_w combinations of v0 and w
  v0T <- v0T[seq(dim(v0T)[1]/n_v0_w, dim(v0T)[1], (dim(v0T)[1]-(dim(v0T)[1]/n_v0_w))/n_v0_w),]
  
  # Loop over values of theta and v0 to find a good trajectory
  # When a good trajectory has been found the data is stored in the matrix "training_data_rally_normal_1"
  found_training_ball <- FALSE
  pos_alpha_training <- FALSE
  pos_i <- 1
  thetas_low <- seq(0, 7.5, 0.5)
  thetas_mid <- seq(7.5, 15, 0.5)
  thetas_high <- seq(15, 25, 0.5)
  v0_low <- v0T[order(v0T[,1]),][1:ceiling(dim(v0T)[1]/3),]
  v0_high <- v0T[order(v0T[,1]),][ceiling(dim(v0T)[1]/3+1):dim(v0T)[1],]
  
  while(!pos_alpha_training){
    print(paste0("pos: ",pos_i))
    velo_i <- 1
    if(posxyz_alpha[pos_i, 1] > 6){
      while(!found_training_ball){
        for(i in 1:length(thetas_high)){
          # Per position
          initial_state <- c(0, posxyz_alpha[pos_i,1:3], v0_high[velo_i,1])
          alpha <- posxyz_alpha[pos_i,4]
          theta <- thetas_high[i]
          w_ball <- 500 # Assume that when a player plays a backspin the maximum angular velocity of the tennis ball is obtained
          v_spin <- R*w_ball 
          spin <- -1
          eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)
          if(length(eq_mo_1)!=2){
            check_eq <- CheckBallInField(posxyz_alpha[pos_i,1:3], eq_mo_1[[3]])
            if(check_eq[[1]] && abs(eq_mo_1[[3]][1]) < 3.5){
              training_data_rally_short <- rbind(training_data_rally_short, c(posxyz_alpha[pos_i,], spin, theta, v0_high[velo_i,2], w_ball, v0_high[velo_i,3], eq_mo_1[[3]]))
              found_training_ball <- TRUE
              break
            }
          }
        }
        if(velo_i == dim(v0_high)[1]){
          found_training_ball <- TRUE
        }else{
          velo_i <- velo_i + 1
        }
      }
    }else{
      while(!found_training_ball){
        for(i in 1:length(thetas_high)){
          # Per position
          initial_state <- c(0, posxyz_alpha[pos_i,1:3], v0_low[velo_i,1])
          alpha <- posxyz_alpha[pos_i,4]
          theta <- thetas_high[i]
          w_ball <- 500
          v_spin <- R*w_ball
          spin <- -1
          eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)
          if(length(eq_mo_1)!=2){
            check_eq <- CheckBallInField(posxyz_alpha[pos_i,1:3], eq_mo_1[[3]])
            if(check_eq[[1]] && abs(eq_mo_1[[3]][1]) < 3.5){
              training_data_rally_short <- rbind(training_data_rally_short, c(posxyz_alpha[pos_i,], spin, theta, v0_low[velo_i,2], w_ball, v0_low[velo_i,3], eq_mo_1[[3]]))
              found_training_ball <- TRUE
              break
            }
          }
        }
        if(velo_i == dim(v0_low)[1]){
          found_training_ball <- TRUE
        }else{
          velo_i <- velo_i + 1
        }
      }
    }
    
    if(pos_i == dim(posxyz_alpha)[1]){
      pos_alpha_training <- TRUE
    }else{
      pos_i <- pos_i + 1
      found_training_ball <- FALSE
    }
  }
  
  if(write_table_rally_short){
    write.table(training_data_rally_short, "Training_Data_Rally_Short.txt", sep = "\t")
  }
})

PlotTennisField()
points(training_data_rally_short[,1], training_data_rally_short[,2])
points(training_data_rally_short[,10], training_data_rally_short[,11])
# initial_s <- c(0, 2, -4, 0.5, 8)
# theta <- 25
# alpha <- 25
# spin <- -1
# v_spin <- R*500
# eq_mo <- SolveEquationsOfMotionWithBounce(initial_s, alpha, theta, v_spin, spin)
# plot(eq_mo[[1]][,2], eq_mo[[1]][,4], type = "l", xlim = c(-15, 15), ylim = c(0,2))
# lines(rep(initial_s[2],2), c(0,2), col="green")
# lines(c(0,0), c(0,1), col="red")





############################
### TRAINING RALLY SHORT ###
############################
if(train_rally_short_bool){
  training_data_rally_short_raw <- as.data.frame(read.table("TrainingData/Training_Data_Rally_Short.txt"))
  training_data_rally_short <- training_data_rally_short_raw[,c(1, 3:4, 6:7, 9)]
  colnames(training_data_rally_short) <- c("X", "Z", "ALPHA", "THETA", "V_INCOMING", "W_RACKET")
  training_data_rally_short <- training_data_rally_short[,c("X", "Z", "ALPHA", "V_INCOMING", "THETA", "W_RACKET")]
  
  lim_mat <- rbind(c(0, 15), c(0, 2), c(-45, 45), c(0, 40), c(0,30), c(0, 30))
  new_df <- NormalizeData(training_data_rally_short, lim_mat)
  colnames(new_df) <- c("X", "Z", "ALPHA", "V_INCOMING", "THETA", "W_RACKET")
  
  nn_rally_short <- neuralnet(W_RACKET + THETA ~ X + Z + ALPHA + V_INCOMING, 
                              new_df,
                              linear.output = FALSE, 
                              hidden = c(15), 
                              act.fct = sigmoid,
                              threshold = 0.01,
                              stepmax = 1e+06,
                              lifesign = "full")
  dna_rally_short <- (nn_rally_short$weights)[[1]]
  SaveDna(dna_rally_short, "dna_rally_short.RData")
  
  # Functions that help check the found data
  generate_random_inputs <- function(){
    pos <- c(runif(1, 2, 12), runif(1, 0.5, 1.5))
    alpha <- 15
    v_inc <- runif(1, 10, 20)
    mat <- matrix(c(pos, alpha, v_inc), nrow = 1, byrow = TRUE)
    return(mat)
  }
  check_nn_random <- function(len=1, plot_ball=FALSE){
    if(plot_ball) plot(c(0,0), c(0,1), xlim = c(-15, 15), ylim = c(0,2), col = "red", type = "l")
    balls_in <- rep(NA, len)
    for(i in 1:len){
      if(i%%10==0) print(i)
      rand_inp <- generate_random_inputs()
      rand_inp_norm <- c(Normalize(lim_mat[1,1], lim_mat[1,2], rand_inp[1]), Normalize(lim_mat[2,1], lim_mat[2,2], rand_inp[2]), Normalize(lim_mat[3,1], lim_mat[3,2], rand_inp[3]), Normalize(lim_mat[4,1], lim_mat[4,2], rand_inp[4]))
      pred_out <- predict(nn_rally_short, rand_inp)
      scaled_out <- pred_out * c(30, 30)
      v0 <- max(VelocityBall(seq(0,0.8,0.005), rand_inp[1,4], scaled_out[1,1]))
      # Per position
      initial_state <- c(0, rand_inp[1], runif(1, -4, 4), rand_inp[2], v0)
      alpha <- rand_inp[3]
      theta <- scaled_out[1,2]
      w_ball <- 500
      v_spin <- R*w_ball
      spin <- -1
      eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)                 
      if(plot_ball) lines(eq_mo_1[[1]][,2], eq_mo_1[[1]][,4])
      balls_in[i] <- eq_mo_1[[2]]
    }
    return(balls_in)
  }
  
  check_1 <- as.integer(check_nn_random(len = 500, plot_ball = FALSE))
  sum(check_1)
}


#####################################
### TRAINING RALLY NORMAL TOPSPIN ###
#####################################
if(train_rally_normal_topspin_bool){
  training_data_rally_topspin_raw <- as.data.frame(read.table("TrainingData/Training_Data_Normal_Rally_Topspin.txt"))
  training_data_rally_topspin <- training_data_rally_topspin_raw[,c(1, 3:4, 6:9)]
  colnames(training_data_rally_topspin) <- c("X", "Z", "ALPHA", "THETA", "V_INCOMING", "W_BALL", "W_RACKET")
  training_data_rally_topspin <- training_data_rally_topspin[,c("X", "Z", "ALPHA", "V_INCOMING", "THETA", "W_BALL", "W_RACKET")]
  
  lim_mat <- rbind(c(0, 15), c(0, 2), c(-35, 35), c(10, 40), c(-3,10), c(100,500), c(0, 70))
  new_df <- NormalizeData(training_data_rally_topspin, lim_mat)
  colnames(new_df) <- c("X", "Z", "ALPHA", "V_INCOMING", "THETA", "W_BALL", "W_RACKET")

  # Take random sample of data set
  n_data <- 500
  random_data_topspin <- new_df[seq(1, dim(new_df)[1], dim(new_df)[1]/n_data), ]
  
  nn_rally_topspin <- neuralnet(THETA + W_BALL + W_RACKET ~ X + Z + ALPHA + V_INCOMING, 
                                random_data_topspin,
                                linear.output = FALSE, 
                                act.fct = sigmoid,
                                hidden = c(10,10),
                                algorithm = "rprop+",
                                threshold = 0.02,
                                stepmax = 1e+06,
                                lifesign = "full")
  saveRDS(nn_rally_topspin, "dna_rally_topspin.rds")
  
  # Functions that help check the found data
  generate_random_inputs <- function(){
    pos <- c(runif(1, 2, 12), runif(1, 0.5, 1.5))
    alpha <- runif(1, -10, 10)
    v_inc <- runif(1, lim_mat[4,1], lim_mat[4,2])
    mat <- matrix(c(pos, alpha, v_inc), nrow = 1, byrow = TRUE)
    return(mat)
  }
  check_nn_random <- function(len=1, plot_ball=FALSE){
    if(plot_ball) plot(c(0,0), c(0,1), xlim = c(-15, 15), ylim = c(0,3), col = "red", type = "l")
    pos_ground <- matrix(nrow = 0, ncol = 2)
    balls_in <- rep(NA, len)
    returnlist <- list()
    for(i in 1:len){
      if(i%%10==0) print(i)
      rand_inp <- generate_random_inputs()
      rand_inp_norm <- c(Normalize(lim_mat[1,1], lim_mat[1,2], rand_inp[1,1]), Normalize(lim_mat[2,1], lim_mat[2,2], rand_inp[1,2]), Normalize(lim_mat[3,1], lim_mat[3,2], rand_inp[1,3]), Normalize(lim_mat[4,1], lim_mat[4,2], rand_inp[1,4]))
      pred_out <- predict(nn_rally_topspin, rand_inp)
      scaled_out <- pred_out * c((lim_mat[5,2]-lim_mat[5,1]),(lim_mat[6,2]-lim_mat[6,1]), (lim_mat[7,2]-lim_mat[7,1])) + c(lim_mat[5,1], lim_mat[6,1], lim_mat[7,1])
      v0 <- max(VelocityBall(seq(0,0.8,0.005), rand_inp[1,4], scaled_out[1,3]))
      # Per position
      initial_state <- c(0, rand_inp[1,1], runif(1, -4, 4), rand_inp[1,2], v0)
      alpha <- rand_inp[1,3]
      theta <- scaled_out[1,1]
      w_ball <- scaled_out[1,2]
      v_spin <- R*w_ball
      spin <- 1
      eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)                 
      if(plot_ball) lines(eq_mo_1[[1]][,2], eq_mo_1[[1]][,4])
      if(length(eq_mo_1)!=2){
        balls_in[i] <- CheckBallInField(initial_state[2:4], eq_mo_1[[3]])[[1]]
      }else{
        balls_in[i] <- FALSE
      } 
    }
    res <- matrix(as.integer(balls_in))
    return(res)
  }
  
  check_1 <- check_nn_random(len = 10, plot_ball = TRUE)
  sum(check_1)
}

#####################################
### TRAINING RALLY NORMAL BACKSPIN ###
#####################################
if(train_rally_normal_backspin_bool){
  training_data_rally_backspin_raw <- as.data.frame(read.table("Training_Data_Normal_Rally_Backspin.txt"))
  training_data_rally_backspin <- training_data_rally_backspin_raw[,c(1, 3:4, 6:7, 9)]
  colnames(training_data_rally_backspin) <- c("X", "Z", "ALPHA", "THETA", "V_INCOMING", "W_RACKET")
  training_data_rally_backspin <- training_data_rally_backspin[,c("X", "Z", "ALPHA", "V_INCOMING", "THETA", "W_RACKET")]
  
  lim_mat <- rbind(c(0, 15), c(0, 2), c(-45, 45), c(0, 40), c(0,30), c(0, 30))
  new_df <- NormalizeData(training_data_rally_backspin, lim_mat)
  colnames(new_df) <- c("X", "Z", "ALPHA", "V_INCOMING", "THETA", "W_RACKET")
  
  nn_rally_backspin <- neuralnet(W_RACKET + THETA ~ X + Z + ALPHA + V_INCOMING, 
                                 new_df,
                                 linear.output = FALSE, 
                                 hidden = c(15), 
                                 act.fct = sigmoid,
                                 threshold = 0.01,
                                 stepmax = 1e+06,
                                 lifesign = "full")
  dna_rally_backspin <- (nn_rally_backspin$weights)[[1]]
  SaveDna(dna_rally_backspin, "dna_rally_backspin.RData")
  
  # Functions that help check the found data
  generate_random_inputs <- function(){
    pos <- c(runif(1, 2, 12), runif(1, 0.5, 1.5))
    alpha <- 15
    v_inc <- runif(1, 10, 20)
    mat <- matrix(c(pos, alpha, v_inc), nrow = 1, byrow = TRUE)
    return(mat)
  }
  check_nn_random <- function(len=1, plot_ball=FALSE){
    if(plot_ball) plot(c(0,0), c(0,1), xlim = c(-15, 15), ylim = c(0,2), col = "red", type = "l")
    balls_in <- rep(NA, len)
    for(i in 1:len){
      if(i%%10==0) print(i)
      rand_inp <- generate_random_inputs()
      rand_inp_norm <- c(Normalize(lim_mat[1,1], lim_mat[1,2], rand_inp[1]), Normalize(lim_mat[2,1], lim_mat[2,2], rand_inp[2]), Normalize(lim_mat[3,1], lim_mat[3,2], rand_inp[3]), Normalize(lim_mat[4,1], lim_mat[4,2], rand_inp[4]))
      pred_out <- predict(nn_rally_backspin, rand_inp)
      scaled_out <- pred_out * c(30, 30)
      v0 <- max(VelocityBall(seq(0,0.8,0.005), rand_inp[1,4], scaled_out[1,1]))
      # Per position
      initial_state <- c(0, rand_inp[1], runif(1, -4, 4), rand_inp[2], v0)
      alpha <- rand_inp[3]
      theta <- scaled_out[1,2]
      w_ball <- 500
      v_spin <- R*w_ball
      spin <- -1
      eq_mo_1 <- SolveEquationsOfMotionWithBounce(initial_state, alpha, theta, v_spin, spin)                 
      if(plot_ball) lines(eq_mo_1[[1]][,2], eq_mo_1[[1]][,4])
      balls_in[i] <- eq_mo_1[[2]]
    }
    return(balls_in)
  }
  
  check_1 <- as.integer(check_nn_random(len = 500, plot_ball = FALSE))
  sum(check_1)
}
