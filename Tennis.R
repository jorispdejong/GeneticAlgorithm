library("rootSolve")
VelocityBall <- function(distance, current_velocity, mass_ball, inertia_racket, angular_velocity_racket, coef_restitution){
  a1 <- (current_velocity * mass_ball * distance - inertia_racket * angular_velocity_racket) * distance
  a2 <- coef_restitution * (angular_velocity_racket * distance + current_velocity) * inertia_racket
  a3 <- inertia_racket + mass_ball * distance * distance
  return((a2-a1)/a3)
}
MomentOfInertia <- function(a, m, R){
  return(a*m*R*R)
}

a <- 0.55 # http://www.physics.usyd.edu.au/~cross/PUBLICATIONS/31.%20Spin.pdf
R <- 0.0033 # Radius of a tennis ball
tennisBallMass <- 0.058 # kg
inertiaRacket <- 0.04 # kg*m^2
angularVelocityRacket <- 300 # rad/s
cor <- 0.5 #Coefficient of Restitution
g <- 9.81



terminalVelocityTennisBall <- 100 #m/s
ini_vel <- 58
height <- 2.8
anglex <- 5
anglez <- -7.5
ini_pos_p1 <- c(-11.89, 0)

DistanceZ <- function(t, v0, vt, theta, height){
  return(height + (vt/g)*(v0*sin(theta*pi/180)+vt)*(1-exp(-g*t/vt))-vt*t)
}
DistanceX <- function(t, v0, vt, theta, height){
  dis <- (v0 * vt * cos(theta * pi / 180) / g) * (1 - exp(-g*t/vt))
  return(dis)
}
VelocityX <- function(t, v0, vt, theta){
  return(v0*cos(theta*pi/180)*exp(-g*t/vt))
}
VelocityZ <- function(t, v0, vt, theta){
  return(v0*sin(theta*pi/180)*exp(-g*t/vt)-vt*(1-exp(-g*t/vt)))
}
BallOverNet <- function(pos_player, v0, vt, theta, alpha, height){
  distance_to_net <- abs(pos_player[1])
  fun <- function(x) DistanceX(x, v0, vt, theta, height) * cos(alpha * pi / 180) - distance_to_net
  tSolved <- uniroot.all(fun, c(0,2))[1]
  height_ball_at_net <- DistanceZ(tSolved, v0, vt, theta, height)
  distance_from_net_middle <- DistanceX(tSolved, v0, vt, theta, height) * sin(alpha * pi / 180)
  height_net <- HeightTennisNet(distance_from_net_middle)
  if(height_ball_at_net > height_net){
    b <- TRUE
  }
  else{
    b <- FALSE
  }
  return(list(b, height_ball_at_net-height_net))
}
PositionVelocityTimeGround <- function(pos_player, v0, vt, theta, alpha, height){
  fun <- function(x) DistanceZ(x, v0, vt, theta, height)
  t <- uniroot.all(fun, c(0,2))[1]
  dis <- DistanceX(t, v0, vt, theta, height)
  pos <- c(dis * cos(alpha * pi / 180), dis * sin(alpha * pi / 180), 0)
  velx <- VelocityX(t, v0, vt, theta)
  velz <- VelocityZ(t, v0, vt, theta)
  vel <- c(velx * cos(alpha * pi / 180), velx * sin(alpha * pi / 180), velz)
  df <- list(pos + c(pos_player,0), vel, t)
  curve(DistanceZ(x, v0, vt, theta, height), 0, 1)
  return(df)
}
PositionVelocityTimePlayer <- function(pos_ground, vel_ground, vt, angle_of_impact_ground, alpha){
  height <- 0
  v0 <- sqrt(sum(vel_ground^2))
  theta <- -angle_of_impact_ground
  fun <- function(x) DistanceZ(x, v0, vt, theta, height)-1
  t <- uniroot.all(fun, c(0,2))[1]
  dis <- DistanceX(t, v0, vt, theta, height)
  pos <- c(dis * cos(alpha * pi / 180), dis * sin(alpha * pi / 180), DistanceZ(t, v0, vt, theta, height = 0))
  velx <- VelocityX(t, v0, vt, theta)
  velz <- VelocityZ(t, v0, vt, theta)
  vel <- c(velx * cos(alpha * pi / 180), velx * sin(alpha * pi / 180), velz)
  df <- list(pos + pos_ground, vel, t)
  curve(DistanceZ(x, v0, vt, theta, height), 0, 2)
  return(df)
}
BallOverNet(ini_pos_p1, ini_vel, terminalVelocityTennisBall, anglez, anglex, height)
pos_vel_t_ground <- PositionVelocityTimeGround(ini_pos_p1, ini_vel, terminalVelocityTennisBall, anglez, anglex, height)
pos_vel_t_player <- PositionVelocityTimePlayer(pos_vel_t_ground[[1]], pos_vel_t_ground[[2]], terminalVelocityTennisBall, atan(pos_vel_t_ground[[2]][3]/initialVelocity)*180/pi, anglex)


# Set Initial values
m_b <- 0.058 # Mass tennis ball
I_b <- 0.04 # Inertia of tennis racket 
g <- 9.81
serve <- TRUE
vt <- 100 # Max velocity due to air resistance 
if(serve){
  v0 <- 58 # Federer serve ~ 208km/h
  height <- 2.8 # 1.80m (height tennis player) + 1m (arm + racket)
  theta <- -7.5 # Angle with respect to the height (around z-axis)
  alpha <- 5 # Angle with respect to the width of the field (around x-axis)
  pos_player <- c(-11.89, 1) # On the right base line (where you serve from)
}



