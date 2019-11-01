# Source 
source("Functions.R")

# Define constant (global) variables
m <- 0.057                  # Mass of tennis ball in kg
d <- 1.21                   # Density of air in kg/m^3
R <- 0.033                  # Radius of a tennis ball in m
g <- 9.81                   # Gravity acceleration
k_e <- 0.0356865967561098   # d*pi*R^2/(2*m) Constant variable used in the equations of motion

# Coefficient of Restitution (COR) and coefficient of sliding friction (COF)
# COR and COF: http://www.physics.usyd.edu.au/~cross/PUBLICATIONS/52.%20SpeedAndBounce.pdf
cor <- 0.9                  # Coefficient of Restitution (vertical energy loss coefficient)
cor_x <- 0.1                # Tangent coefficient of restituation (horizontal energy loss coefficient)
mu <- 0.65                  # Coefficient of sliding friction; 0.6 is a fast court, 0.8 is a slow court
a <- 0.55                   # http://www.physics.usyd.edu.au/~cross/PUBLICATIONS/31.%20Spin.pdf
I_b <- a * m * R^2          # Moment of inertia of a regular tennis ball in kg*m^2
I_t <- 0.03                 # Article: Measuring the inertial properties of a tennis racket

# Equations of motion with air resistense and spin included can be written as follows:
# dv_x/dt = -k*v*(C_d*v_x - spin * C_l*v_y)
# dv_y/dt = k*v*(-spin * C_l*v_x - C_d*v_y) - g
# where v = sqrt(v_x^2+v_y^2) and spin = 1 (topspin) or spin = -1 (backspin)
# Topspin: spin=1; Backspin: spin=-1

dt <- 0.001                 # Timestep in discretization

#######################
### HANDY FUNCTIONS ###
#######################
cd <- function(v, v_spin){
  cd <- 0.55+1/((22.5+4.2*(v/v_spin)^2.5)^0.4)
  return(cd)
}
cl <- function(v, v_spin){
  cl <- 1/(2+(v/v_spin))
  return(cl)
}
dvx <- function(v, vx, vy, vz, alpha, dt, v_spin, spin){
  dvx <- (-k_e*v*(cd(v, v_spin) * sqrt(vx^2+vy^2) - spin * cl(v, v_spin) * vz)) * cos(alpha * pi / 180)
  return(dvx)
}
dvy <- function(v, vx, vy, vz, alpha, dt, v_spin, spin){
  dvy <- (-k_e*v*(cd(v, v_spin) * sqrt(vx^2+vy^2) - spin * cl(v, v_spin) * vz)) * sin(alpha * pi / 180)
  return(dvy)
}
dvz <- function(v, vx, vy, vz, dt, v_spin, spin){
  dvz <- (k_e*v*(-spin * cl(v, v_spin) * sqrt(vx^2+vy^2) - cd(v, v_spin) * vz) - g)
  return(dvz)
}

# D is the distance from the point of impact and the point of release after the bounce
# D is 4 mm for a low speed bounce and can be up to 11mm for a high speed bounce
# Make a linear approximation
ComputeD <- function(v){
  D <- 0.004 + 0.007*(v/50) # Assume the maximum speed of a ball is 70m/s
}
# The condition for the ball to slide throughout the bounce is given by the following function (article:Measurements of the horizontal and vertical speeds of tennis courts)
SlideThroughoutBounce <- function(vx_1, w1, theta_1, D, spin){
  mu_condition <- (1-(R*spin*w1/vx_1))/((1+1/a)*(1+cor)*tan(theta_1*pi/180))+D/((1+a)*R)
  return(mu_condition)
}
# vx velocity after the bounce if it (mostly) slides
vx2Slide <- function(vx_1, theta_1,spin){
  return(vx_1 * (1 - spin*mu * (1 + cor) * tan(theta_1*pi/180)))
}
# vx velocity after the bounce if it (mostly) bites
vx2Bite <- function(vx_1, w1, theta_1, D, spin){
  vx_2 <- vx_1 * (1 - ((1+cor_x)*(1-R*spin*w1/vx_1))/(1+1/a) - (D*(1+cor)*tan(theta_1*pi/180))/(R*(1+a)))
  return(vx_2)
}
# horizontal velocity of ball after bounce
vy2 <- function(vy_1){
  return(-cor*vy_1)
}
# angular velocity of the ball if it slides
w2Slide <- function(vx_2){
  return(vx_2/R)
}
# angular velocity of the ball if it bites
w2Bite <- function(vx_1, vx_2, vy_1, w1, D, spin){
  w2 <- spin*w1 + (vx_1-vx_2)/(a*R) - D*(1+cor)*vy_1/(a*R*R)
  return(w2)
}
# Determine (vx, vy, vz, angular velocity, angle after impact) for 
Bounce <- function(vx_1, vy_1, vz_1, w1, theta_1, alpha, spin){
  # rewrite to two dimension
  vx_1_temp <- sqrt(vx_1^2+vy_1^2)
  
  # Compute D and determine whether the ball slides or bites
  D <- ComputeD(sqrt(vx_1^2+vy_1^2+vz_1^2))
  mu_cond <- abs(SlideThroughoutBounce(vx_1_temp, w1, theta_1, D, spin))
  if(mu_cond < mu){
    vx_2 <- vx2Slide(vx_1_temp, theta_1, spin)*cos(alpha*pi/180)
    vy_2 <- vx2Slide(vx_1_temp, theta_1, spin)*sin(alpha*pi/180)
    vz_2 <- vy2(vz_1)
    w2 <- w2Slide(vx_1_temp)
  }else{
    vx_2 <- vx2Bite(vx_1_temp, w1, theta_1, D, spin)*cos(alpha*pi/180)
    vy_2 <- vx2Bite(vx_1_temp, w1, theta_1, D, spin)*sin(alpha*pi/180)
    vz_2 <- vy2(vz_1)
    w2 <- w2Bite(vx_1_temp, sqrt(vx_2^2+vy_2^2), vz_1, w1, D, spin)
  }
  theta_2 <- atan(vz_2/sqrt(vx_2^2+vy_2^2))*180/pi
  
  return(c(vx_2, vy_2, vz_2, w2, theta_2))
}

#####################################################
### FUNCTION FOR SOLVING THE EQUATIONS OF MOTION ###
#####################################################
# Inputs are: initial_state(t0, x0, y0, z0, v0), alpha, theta
SolveEquationsOfMotionServe <- function(initial_state_e, alpha_e, theta_e, v_spin, spin){

  # Set the initial values
  t0_e <- initial_state_e[1]
  x0_e <- initial_state_e[2]
  y0_e <- initial_state_e[3]
  z0_e <- initial_state_e[4]
  v0_e <- initial_state_e[5]
  
  if(x0_e < 0){
    vx_direction_e <- 1
  }else{
    vx_direction_e <- -1
  }

  vx0_e <- v0_e * cos(theta_e * pi / 180) * cos(vx_direction_e * alpha_e * pi / 180)
  vy0_e <- v0_e * cos(theta_e * pi / 180) * sin(alpha_e * pi / 180)
  vz0_e <- v0_e * sin(theta_e * pi / 180)
  initial_state_new_e <- c(t0_e, x0_e, y0_e, z0_e, vx0_e, vy0_e, vz0_e)

  # Solve the equations of motion
  period_e <- 1.5
  times_e <- seq(t0_e, period_e, dt)
  n_e <- length(times_e)-1
  results_e <- matrix(rep(NA, length(times_e) * length(initial_state_new_e)), ncol = length(initial_state_new_e))
  results_e[1,] <- initial_state_new_e
  for(i in 1:n_e){
    if(abs(x0_e-results_e[i,2]) > abs(x0_e) || results_e[i,4] < 0){
      break
    }
    x_e <- results_e[i,2]
    y_e <- results_e[i,3]
    z_e <- results_e[i,4]
    vx_e <- results_e[i,5]
    vy_e <- results_e[i,6]
    vz_e <- results_e[i,7]
    v_e <- sqrt(vx_e*vx_e+vy_e*vy_e+vz_e*vz_e)
    delta_vx_e <- dvx(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
    delta_vy_e <- dvy(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
    delta_vz_e <- dvz(v_e, vx_e, vy_e, vz_e, dt, v_spin, spin)
    results_e[i+1,1] <- i * dt
    results_e[i+1,2] <- x_e + vx_direction_e * (dt * vx_e + dt^2 * delta_vx_e)
    results_e[i+1,3] <- y_e + dt * vy_e + dt^2 * delta_vy_e
    results_e[i+1,4] <- z_e + dt * vz_e + dt^2 * delta_vz_e
    results_e[i+1,5] <- vx_e + dt * delta_vx_e 
    results_e[i+1,6] <- vy_e + dt * delta_vy_e 
    results_e[i+1,7] <- vz_e + dt * delta_vz_e 
  }
  results_temp_e <- na.omit(results_e)
  if(abs(x0_e-results_temp_e[length(results_temp_e[,2]),2]) < abs(x0_e)){
    return(list("Trajectory"= results_temp_e, "Ball_over_net"=FALSE))
  }else if(results_temp_e[length(results_temp_e[,1]),4] < 0){
    return(list("Trajectory"= results_temp_e, "Ball_over_net"=FALSE)) 
  }else{
    # Get the values from BEFORE and AFTER the ball passes the net
    t_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 1], results_temp_e[length(results_temp_e[,1]), 1]))
    x_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 2], results_temp_e[length(results_temp_e[,1]), 2]))
    y_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 3], results_temp_e[length(results_temp_e[,1]), 3]))
    z_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 4], results_temp_e[length(results_temp_e[,1]), 4]))
    # Interpolate (linear) to get the approximate time the ball passes the net
    t_net_e <- approx(x_temp_e, t_temp_e, 0)$y
    # Get the values from the ball when it passes the net
    height_ball_at_net_e <- approx(t_temp_e, z_temp_e, t_net_e)$y
    height_tennis_net_e <- HeightTennisNet(approx(t_temp_e, y_temp_e, t_net_e)$y)
    if(height_ball_at_net_e < height_tennis_net_e){
      ball_pass_net_e <- FALSE
      return(list("Trajectory"= results_temp_e, "Height dif ball net"=height_ball_at_net_e - height_tennis_net_e))
    }else{
      ball_pass_net_e <- TRUE
    }
    # If the ball passes the net then continue with the trajectory
    if(!ball_pass_net_e){
      lines(results_temp_e[,2], results_temp_e[,3], col="red")
    }else{
      start_i <- length(results_temp_e[,1])
      for(i in start_i:n_e){
        if(results_e[i,4] < 0){
          break
        }
        x_e <- results_e[i,2]
        y_e <- results_e[i,3]
        z_e <- results_e[i,4]
        vx_e <- results_e[i,5]
        vy_e <- results_e[i,6]
        vz_e <- results_e[i,7]
        v_e <- sqrt(vx_e*vx_e+vy_e*vy_e+vz_e*vz_e)
        delta_vx_e <- dvx(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
        delta_vy_e <- dvy(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
        delta_vz_e <- dvz(v_e, vx_e, vy_e, vz_e, dt, v_spin, spin)
        results_e[i+1,1] <- i * dt
        results_e[i+1,2] <- x_e + vx_direction_e * (dt * vx_e + dt^2 * delta_vx_e)
        results_e[i+1,3] <- y_e + dt * vy_e + dt^2 * delta_vy_e
        results_e[i+1,4] <- z_e + dt * vz_e + dt^2 * delta_vz_e
        results_e[i+1,5] <- vx_e + dt * delta_vx_e 
        results_e[i+1,6] <- vy_e + dt * delta_vy_e 
        results_e[i+1,7] <- vz_e + dt * delta_vz_e
      }
      results_temp_e <- na.omit(results_e)
      # Get the values from BEFORE and AFTER the ball hits the ground
      t_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 1], results_temp_e[length(results_temp_e[,1]), 1]))
      x_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 2], results_temp_e[length(results_temp_e[,1]), 2]))
      y_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 3], results_temp_e[length(results_temp_e[,1]), 3]))
      z_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 4], results_temp_e[length(results_temp_e[,1]), 4]))
      # Interpolate (linear) to get the approximate time the ball hits the ground
      t_ground_e <- approx(z_temp_e, t_temp_e, 0)$y
      # Get the values from when the ball hits the ground
      x_ground_e <- approx(t_temp_e, x_temp_e, t_ground_e)$y
      y_ground_e <- approx(t_temp_e, y_temp_e, t_ground_e)$y
      
      return(list("Trajectory"=results_temp_e, 
                  "ball over net"=TRUE, 
                  "Position ball at ground"=c(x_ground_e, y_ground_e), 
                  "t at ground"=t_ground_e))
    }
  }
}
SolveEquationsOfMotionWithBounce <- function(initial_state_e, alpha_e, theta_e, v_spin, spin){
  
  # Set the initial values
  t0_e <- initial_state_e[1]
  x0_e <- initial_state_e[2]
  y0_e <- initial_state_e[3]
  z0_e <- initial_state_e[4]
  v0_e <- initial_state_e[5]
  
  if(x0_e < 0){
    vx_direction_e <- 1
  }else{
    vx_direction_e <- -1
  }
  
  vx0_e <- v0_e * cos(theta_e * pi / 180) * cos(vx_direction_e * alpha_e * pi / 180)
  vy0_e <- v0_e * cos(theta_e * pi / 180) * sin(alpha_e * pi / 180)
  vz0_e <- v0_e * sin(theta_e * pi / 180)
  initial_state_new_e <- c(t0_e, x0_e, y0_e, z0_e, vx0_e, vy0_e, vz0_e)
  
  # Solve the equations of motion
  period_e <- 10
  times_e <- seq(t0_e, period_e, dt)
  n_e <- length(times_e)-1
  results_e <- matrix(rep(NA, length(times_e) * length(initial_state_new_e)), ncol = length(initial_state_new_e))
  results_e[1,] <- initial_state_new_e
  for(i in 1:n_e){
    if(abs(x0_e-results_e[i,2]) > abs(x0_e) || results_e[i,4] < 0){
      break
    }
    x_e <- results_e[i,2]
    y_e <- results_e[i,3]
    z_e <- results_e[i,4]
    vx_e <- results_e[i,5]
    vy_e <- results_e[i,6]
    vz_e <- results_e[i,7]
    v_e <- sqrt(vx_e*vx_e+vy_e*vy_e+vz_e*vz_e)
    delta_vx_e <- dvx(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
    delta_vy_e <- dvy(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
    delta_vz_e <- dvz(v_e, vx_e, vy_e, vz_e, dt, v_spin, spin)
    results_e[i+1,1] <- i * dt
    results_e[i+1,2] <- x_e + vx_direction_e * (dt * vx_e + dt^2 * delta_vx_e)
    results_e[i+1,3] <- y_e + dt * vy_e + dt^2 * delta_vy_e
    results_e[i+1,4] <- z_e + dt * vz_e + dt^2 * delta_vz_e
    results_e[i+1,5] <- vx_e + dt * delta_vx_e 
    results_e[i+1,6] <- vy_e + dt * delta_vy_e 
    results_e[i+1,7] <- vz_e + dt * delta_vz_e 
  }
  results_temp_e <- na.omit(results_e)
  if(abs(x0_e-results_temp_e[length(results_temp_e[,2]),2]) < abs(x0_e) || results_e[i,4] < 0){
    return(list("Trajectory"= results_temp_e, "Ball_over_net"=FALSE))
  }else{
    # Get the values from BEFORE and AFTER the ball passes the net
    t_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 1], results_temp_e[length(results_temp_e[,1]), 1]))
    x_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 2], results_temp_e[length(results_temp_e[,1]), 2]))
    y_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 3], results_temp_e[length(results_temp_e[,1]), 3]))
    z_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 4], results_temp_e[length(results_temp_e[,1]), 4]))
    # Interpolate (linear) to get the approximate time the ball passes the net
    t_net_e <- approx(x_temp_e, t_temp_e, 0)$y
    # Get the values from the ball when it passes the net
    height_ball_at_net_e <- approx(t_temp_e, z_temp_e, t_net_e)$y
    height_tennis_net_e <- HeightTennisNet(approx(t_temp_e, y_temp_e, t_net_e)$y)
    if(height_ball_at_net_e < height_tennis_net_e){
      ball_pass_net_e <- FALSE
      return(list("Trajectory"=results_temp_e, "Ball over net"=FALSE))
    }else{
      ball_pass_net_e <- TRUE
    }
    # If the ball passes the net then continue with the trajectory
    start_i <- length(results_temp_e[,1])
    for(i in start_i:n_e){
      if(results_e[i,4] < 0){
        break
      }
      x_e <- results_e[i,2]
      y_e <- results_e[i,3]
      z_e <- results_e[i,4]
      vx_e <- results_e[i,5]
      vy_e <- results_e[i,6]
      vz_e <- results_e[i,7]
      v_e <- sqrt(vx_e*vx_e+vy_e*vy_e+vz_e*vz_e)
      delta_vx_e <- dvx(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
      delta_vy_e <- dvy(v_e, vx_e, vy_e, vz_e, alpha_e, dt, v_spin, spin)
      delta_vz_e <- dvz(v_e, vx_e, vy_e, vz_e, dt, v_spin, spin)
      results_e[i+1,1] <- i * dt
      results_e[i+1,2] <- x_e + vx_direction_e * (dt * vx_e + dt^2 * delta_vx_e)
      results_e[i+1,3] <- y_e + dt * vy_e + dt^2 * delta_vy_e
      results_e[i+1,4] <- z_e + dt * vz_e + dt^2 * delta_vz_e
      results_e[i+1,5] <- vx_e + dt * delta_vx_e 
      results_e[i+1,6] <- vy_e + dt * delta_vy_e 
      results_e[i+1,7] <- vz_e + dt * delta_vz_e
    }
    results_temp_e <- na.omit(results_e)
    # Get the values from BEFORE and AFTER the ball hits the ground
    t_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 1], results_temp_e[length(results_temp_e[,1]), 1]))
    x_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 2], results_temp_e[length(results_temp_e[,1]), 2]))
    y_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 3], results_temp_e[length(results_temp_e[,1]), 3]))
    z_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 4], results_temp_e[length(results_temp_e[,1]), 4]))
    vx_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 5], results_temp_e[length(results_temp_e[,1]), 5]))
    vy_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 6], results_temp_e[length(results_temp_e[,1]), 6]))
    vz_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 7], results_temp_e[length(results_temp_e[,1]), 7]))
    
    # Interpolate (linear) to get the approximate time the ball hits the ground
    t_ground_e <- approx(z_temp_e, t_temp_e, 0)$y
    # Get the values from when the ball hits the ground
    x_ground_e <- approx(t_temp_e, x_temp_e, t_ground_e)$y
    y_ground_e <- approx(t_temp_e, y_temp_e, t_ground_e)$y
    vx_ground_e <- approx(t_temp_e, vx_temp_e, t_ground_e)$y
    vy_ground_e <- approx(t_temp_e, vy_temp_e, t_ground_e)$y
    vz_ground_e <- approx(t_temp_e, vz_temp_e, t_ground_e)$y
    angle_of_impact <- atan(vz_ground_e/(sqrt(vx_ground_e^2+vy_ground_e^2)))*180/pi
    
    # Create the bounce of the ball
    bounce_res <- Bounce(vx_ground_e, vy_ground_e, vz_ground_e, v_spin/R, angle_of_impact, alpha_e, spin)
    # Create the trajectory starting from the ground with the new parameters
    start_i <- length(results_temp_e[,1])
    new_initial_values <- c(t_ground_e, x_ground_e, y_ground_e, 0, bounce_res[1], bounce_res[2], bounce_res[3])
    results_e[start_i,] <- new_initial_values
    new_theta_e <- bounce_res[5]
    new_v_spin <- bounce_res[4]*R
    for(i in start_i:n_e){
      if(results_e[i,4] < 0){
        break
      }
      x_e <- results_e[i,2]
      y_e <- results_e[i,3]
      z_e <- results_e[i,4]
      vx_e <- results_e[i,5]
      vy_e <- results_e[i,6]
      vz_e <- results_e[i,7]
      v_e <- sqrt(vx_e*vx_e+vy_e*vy_e+vz_e*vz_e)
      delta_vx_e <- dvx(v_e, vx_e, vy_e, vz_e, alpha_e, dt, new_v_spin, spin)
      delta_vy_e <- dvy(v_e, vx_e, vy_e, vz_e, alpha_e, dt, new_v_spin, spin)
      delta_vz_e <- dvz(v_e, vx_e, vy_e, vz_e, dt, new_v_spin, spin)
      results_e[i+1,1] <- i * dt
      results_e[i+1,2] <- x_e + vx_direction_e * (dt * vx_e + dt^2 * delta_vx_e)
      results_e[i+1,3] <- y_e + dt * vy_e + dt^2 * delta_vy_e
      results_e[i+1,4] <- z_e + dt * vz_e + dt^2 * delta_vz_e
      results_e[i+1,5] <- vx_e + dt * delta_vx_e 
      results_e[i+1,6] <- vy_e + dt * delta_vy_e 
      results_e[i+1,7] <- vz_e + dt * delta_vz_e
    }
    results_temp_e <- na.omit(results_e)
    # Get the values from BEFORE and AFTER the ball hits the ground
    t_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 1], results_temp_e[length(results_temp_e[,1]), 1]))
    x_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 2], results_temp_e[length(results_temp_e[,1]), 2]))
    y_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 3], results_temp_e[length(results_temp_e[,1]), 3]))
    z_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 4], results_temp_e[length(results_temp_e[,1]), 4]))
    vx_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 5], results_temp_e[length(results_temp_e[,1]), 5]))
    vy_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 6], results_temp_e[length(results_temp_e[,1]), 6]))
    vz_temp_e <- sort(c(results_temp_e[length(results_temp_e[,1])-1, 7], results_temp_e[length(results_temp_e[,1]), 7]))
    
    # Interpolate (linear) to get the approximate time the ball hits the ground
    t_ground_e_1 <- approx(z_temp_e, t_temp_e, 0)$y
    # Get the values from when the ball hits the ground
    x_ground_e_1 <- approx(t_temp_e, x_temp_e, t_ground_e)$y
    y_ground_e_1 <- approx(t_temp_e, y_temp_e, t_ground_e)$y
    
    height_ball <- results_temp_e[which.min(abs(results_temp_e[,4] - 0.8)),2:4]
    t_height_ball <- results_temp_e[which.min(abs(results_temp_e[,4] - 0.8)),1] + t_ground_e
    
    return(list("Trajectory"=results_temp_e, 
                "ball over net"=TRUE, 
                "Pos ball ground 1"=c(x_ground_e, y_ground_e), 
                "t ground 1"=t_ground_e, 
                "Pos ball ground 2"=c(x_ground_e_1, y_ground_e_1), 
                "t ground 2"=t_ground_e_1, 
                "height_ball_hit"=height_ball, 
                "t_height_ball_hit"=t_height_ball))     
  }
}

###############
### EXAMPLE ###
###############
show_example <- FALSE
if(show_example){
  initial_state_1 <- c(0, -12, 1, 1, 30)
  alpha_1 <- 0
  theta_1 <- 6
  v_spin_1 <- R*100
  spin_1 <- 1
  traj_1 <- SolveEquationsOfMotionWithBounce(initial_state_1, alpha_1, theta_1, v_spin_1, spin_1)
  
  initial_state_2 <- c(0, -12, 1, 1, 30)
  alpha_2 <- 0
  theta_2 <- 6
  v_spin_2 <- R*100
  spin_2 <- -1
  traj_2 <- SolveEquationsOfMotionWithBounce(initial_state_2, alpha_2, theta_2, v_spin_2, spin_2)
  
  initial_state_3 <- c(0, -12, 1, 1, 30)
  alpha_3 <- 0
  theta_3 <- 6
  v_spin_3 <- 0
  spin_3 <- 1
  traj_3 <- SolveEquationsOfMotionWithBounce(initial_state_3, alpha_3, theta_3, v_spin_3, spin_3)
  
  
  plot(traj_1[[1]][,2], traj_1[[1]][,4], type = "l", xlab = "x", ylab = "z", xlim=c(-13,20), ylim = c(0,3), lwd=2, col="red")
  lines(traj_2[[1]][,2], traj_2[[1]][,4], col="yellow", lwd=2)
  lines(traj_3[[1]][,2], traj_3[[1]][,4], col="orange", lwd=2)
  legend(10, 2.5, legend=c("Topspin", "No spin", "Backspin"),
         col=c("red", "orange", "yellow"), lty=c(1,1,1), lwd=3, cex=1.6)
  
}



