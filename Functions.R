# Velocity of the ball directly after impact
VelocityBall <- function(distance_on_racket, current_velocity, mass_ball, inertia_racket, angular_velocity_racket, coef_restitution){
  a1 <- (current_velocity * mass_ball * distance_on_racket - inertia_racket * angular_velocity_racket) * distance_on_racket
  a2 <- coef_restitution * (angular_velocity_racket * distance_on_racket + current_velocity) * inertia_racket
  a3 <- inertia_racket + mass_ball * distance_on_racket * distance_on_racket
  return((a2-a1)/a3)
}

# Drag coefficient (Stepanek, 1985) (Force due to air resistence)
C_d <- function(v, v_spin){
  return(0.55 + 1/(22.5 + 4.2 * (v / v_spin)^2.5)^0.4)
}

# Lift coefficient (Force due to spin)
C_l <- function(v, v_spin){
  return(1/(2 + (v/v_spin)))
}

# Prepare to solve the equations of motion with drag forces numerically
EquationsOfMotion <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    dX1 <- X2
    dX2 <- -k * v0 * (C_d * X2 - spin * C_l * Y2)
    dY1 <- Y2
    dY2 <- k * v0 * (- spin * C_l * X2 - C_d * Y2) - g
    list(c(dX1, dX2, dY1, dY2))
  })
}

HeightTennisNet <- function(x){
  aSolved <- 94.043
  h <- 0.5*aSolved*(exp(x/aSolved)+exp(-x/aSolved)) + 0.91 - aSolved
  return(h)
}

CheckServeInField <- function(pos_ball){
  
}

CheckBallInField <- function(pos_ball){
  
}

