library("deSolve") # Library to solve the equations of motion

source("Functions.R") # Source the "Functions" scripts

# Define constant variables
m <- 0.058 # Mass of tennis ball in kg
d <- 1.21 # Density of air in kg/m^3
R <- 0.033 # Radius of a tennis ball in m
g <- 9.81 # Gravity acceleration
k <- d*pi*R^2/(2*m) # Constant variable used in the equations of motion

# Coefficient of Restitution (COR) and coefficient of sliding friction (COF)
# COR and COF: http://www.physics.usyd.edu.au/~cross/PUBLICATIONS/52.%20SpeedAndBounce.pdf
cor <- 0.8  # Coefficient of Restitution
mu <- 0.65 # Coefficient of sliding friction
a <- 0.55 # http://www.physics.usyd.edu.au/~cross/PUBLICATIONS/31.%20Spin.pdf
I_b <- a * m * R^2 # Moment of inertia of a regular tennis ball in kg*m^2

# Equations of motion with air resistense and spin included can be written as follows:
# dv_x/dt = -k*v*(C_d*v_x - spin * C_l*v_y)
# dv_y/dt = k*v*(-spin * C_l*v_x - C_d*v_y) - g
# where v = sqrt(v_x^2+v_y^2) and spin = 1 (topspin) or spin = -1 (backspin)
# Topspin: spin=1; Backspin: spin=-1

time_step <- 0.001 # Timestep of numerical solution to equations of motion

########################
### START SIMULATION ###
########################

# Set the initial values of the player and the ball
theta <- -6 # Angle in the horizontal direction (serve: theta<0, rally (usually): theta >= 0)
alpha <- 3 # Angle in the vertizal direction (left or right on the field)
height <- 2.8 # Height at which the ball is hit (serve: 2.5-3, rally: 0.5-1.5)
pos_player <- c(-11.89, 0) # Initial position of player 1; (-11.89,0) is in the middle of the base line
v0 <- 58 # Average serve of Federer is ~208km/h
w_ball <- runif(1, 100, 200) # Topspin/backspin is typically 100-500 radians/s

TennisBallSimulation <- function(pos_player, height, v0, w_ball, spin, theta, alpha){
  v_spin <- R * w_ball
  v0_x <- v0 * cos(theta * pi / 180)
  v0_y <- v0 * sin(theta * pi / 180)
  # Set the initial state of the system 
  parameters <- c(v0 = v0, k = k, C_d = C_d(v0, v_spin), C_l = C_l(v0, v_spin), spin = spin, g = g) # Set parameters
  state <- c(X1 = 0, X2 = v0_x, Y1 = height, Y2 = v0_y) # Define the state the system is in
  times <- seq(0, 2, time_step) # Set time range
  sols <- ode(y = state, times = times, func = EquationsOfMotion, parms = parameters) # Solve the equations of motion
  
  # Determine whether the ball makes it over the net
  distance_to_net <- abs(pos_player[1])/cos(abs(alpha) * pi / 180) # Distance from the player's position to the net
  if(sols[length(sols[,1]),2] < distance_to_net){
    ball_over_net <- FALSE
    ball_reach_net <- "ball didn't reach the net"
    ball_didnt_reach_net <- TRUE
  }else{
    ball_didnt_reach_net <- FALSE
    t_net <- approx(sols[,2], sols[,1], distance_to_net)$y # Time at which the ball reaches the net
    
    height_net <- HeightTennisNet(distance_to_net*sin(alpha * pi /180)) # Determine the height of the net
    height_ball <- approx(sols[,1], sols[,4], t_net)$y # Determine the height of the ball
    if(height_ball > height_net) ball_over_net <- TRUE else ball_over_net <- FALSE # Set boolean to save observation
  }
  if(ball_didnt_reach_net == FALSE){
    # Get time, velocity, position, angle when the ball hits the ground
    out_temp <- sols[sols[,4]>0,]
    out1 <- out_temp[order(abs(out_temp[,4])),]
    out2 <- sols[sols[,4]<0,]
    trajectory <- matrix(c(sols[1:length(out1[,1]),1], sols[1:length(out1[,1]),4], sols[1:length(out1[,1]),2]), ncol = 3)
    x_temp <- c(out1[1,1], out2[1,1])
    y_temp <- c(out1[1,4], out2[1,4])
    t_ground <- approx(y_temp, x_temp, 0)$y # Time after which the ball hits the ground
    dis_ground <- approx(sols[,1], sols[,2], t_ground)$y # Horizontal distance travelled by the ball
    pos_ground <- c(pos_player[1] + dis_ground*cos(alpha * pi / 180), pos_player[2] + dis_ground*sin(alpha * pi / 180)) # Position of ball
    v1_y <- approx(sols[,1], sols[,5], t_ground)$y # Vertical velocity of ball when it hits the ground
    v1_x <- approx(sols[,1], sols[,3], t_ground)$y # Horizontal velocity of ball when it hits the ground
    angle_before_impact_ground <- abs(atan(v1_y/v1_x) * 180 / pi) # Angle of impact
    
    # Calculate the new trajectory after the ball hits the ground
    v2_x <- v1_x - spin * mu * (1 + cor) * v1_y # New horizontal velocity
    v2_y <- cor * -v1_y # New vertical velocity
    v2 <- sqrt(v2_x^2+v2_y^2) # New total velocity (v0)
    angle_after_impact_ground <- atan(v2_y/v2_x) * 180 / pi # New angle theta
    w2 <- w_ball + mu * (1 + cor) * -v1_y * R * m / I_b # New angular velocity
    v2_spin <- R * w2 
    
    # Solve the equations of motion with the new values
    parameters_1 <- c(v0 = v2, k = k, C_d = C_d(v2, v2_spin), C_l = C_l(v2, v2_spin), spin = spin, g = g) # Set parameters
    state_1 <- c(X1 = 0, X2 = v2_x, Y1 = 0, Y2 = v2_y) # Set state of the system
    sols_1 <- ode(y = state_1, times = times, func = EquationsOfMotion, parms = parameters_1) # Solve the equations of motion
    
    # Determine when the opponent will return the ball
    t_return <- round(runif(1, 200, 300)) # Time after which the opponent returns the ball (0.2~0.3 sec)
    trajectory <- rbind(trajectory, matrix(c(trajectory[length(trajectory[,1]),1] + sols_1[1:t_return, 1], sols_1[1:t_return, 4], pos_ground[1] - pos_player[1] + sols_1[1:t_return, 2]), ncol = 3))
    pos_ball_end <- pos_ground + c(sols_1[t_return, 2]*cos(alpha*pi/180), sols_1[t_return, 2]*sin(alpha*pi/180)) # End position of ball 
    vel_ball_end <- c(sols_1[t_return, 3] * cos(alpha * pi / 180), sols_1[t_return, 3] * sin(alpha * pi / 180), sols_1[t_return, 5])
    
    trajectory[,3] <- trajectory[,3] + pos_player[1]
    results <- list("trajectory"=trajectory, "height"=sols_1[t_return,4], "position_ground" = pos_ground, "position_end"=pos_ball_end,  "velocity_end"=vel_ball_end, "ball_over_net"=ball_over_net)
    
    return(results)
  }else{
    return(list(ball_reach_net))
  }
}

# list <- TennisRallySimulation(height, pos_player, v0, w1, spin, theta, alpha)
# list_1 <- TennisRallySimulation(list[[2]], list[[3]], 35, w_ball = 400, spin, theta = 178, alpha = -5)
# list_2 <- TennisRallySimulation(list_1[[2]], list_1[[3]], 40, w_ball = 400, spin, theta = 10, alpha = 5)
# list_3 <- TennisRallySimulation(list_2[[2]], list_2[[3]], 37, w_ball = 300, spin, theta = 177, alpha = 5)
# list_4 <- TennisRallySimulation(list_3[[2]], list_3[[3]], 40, w_ball = 400, spin, theta = 10, alpha = 5)
# 
# plot(c(0,0), c(0, list[[2]]), type ="l", col = "red", lwd = 3 , xlim = c(-15, 15), ylim = c(0,3))
# lines(list[[1]][,3], list[[1]][,2], col = "orange", lwd=3)
# lines(list_1[[1]][,3], list_1[[1]][,2], col = "green", lwd=3)
# lines(list_2[[1]][,3], list_2[[1]][,2], col = "orange", lwd=3)
# lines(list_3[[1]][,3], list_3[[1]][,2], col = "green", lwd=3)
# lines(list_4[[1]][,3], list_4[[1]][,2], col = "orange", lwd=3)
