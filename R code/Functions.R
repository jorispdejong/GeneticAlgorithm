# libraries
suppressPackageStartupMessages(library("rjson"))
suppressPackageStartupMessages(library("scatterplot3d"))


########################
### GLOBAL VARIABLES ###
########################
# Parameters for the distribution of determining left or right
skew_parameter_1R <- 5            # Shape of the skew distribution
skew_parameter_2R <- 2            # Shape of the skew distribution
# Parameters for the randomness of backspin / topspin
skew_parameter_spin_1 <- 4            # Shape of the skew distribution
skew_parameter_spin_2 <- 2            # Shape of the skew distribution

#######################
### PLAYER MOVEMENT ###
#######################
# Global variables (article: Physics of sprinting)
sigma <- 0.667  
f <- 8.13
x_movement_player <- function(t, v0=0){
  x_t <- f*t/sigma + ((f/(sigma*sigma))-v0/sigma)*(exp(-sigma*t)-1)
  return(x_t)
}                  # Function that determines the distance a player can travel in t sec
v_movement_player <- function(t, v0){
  v_t <- v0*exp(-sigma*t)+(f/sigma)*(1-exp(-sigma*t))
  return(v_t)
}                    # Function that determines the speed of a player after t sec of accelerating
TimePlayerNeedsToReachBall <- function(dis_to_ball,v0){
  x_movement_player_f <- function(t){
    x_t <- f*t/sigma + ((f/(sigma*sigma))-v0/sigma)*(exp(-sigma*t)-1)
    return(x_t-dis_to_ball)
  }
  t_needed <- uniroot(x_movement_player_f, lower = 0, upper = 10)$root
  return(t_needed)
}  # Determine the time a player needs to reach a ball at dis_to_ball away

#########################
### FITNESS FUNCTIONS ###
#########################
# Fitness function of serve training algorithm
FitnessFunctionServe <- function(ball_over_net, pos_player, pos_ball_ground, v_ground){
  h <- 0
  check <- CheckServeInField(pos_player, pos_ball_ground)
  if(ball_over_net){
    h <- h - 50
    if(!check[[1]]){
      h <- h + (1+check[[2]])^5
    }else{
      h <- h - v_ground
    }
  }else{
    h <- h + 50
  }
  
  return(c(h, check[[1]]))
}
# Fitness function of rally training algorithm
FitnessFunctionRally <- function(pos_player, pos_ball_ground){
  h <- 0
  check_rally <- CheckBallInField(pos_player, pos_ball_ground)
  if(check_rally[[2]]!=0){
    h <- h + check_rally[[2]]
  }else{
    h <- h + (abs(field_baseline_length) - abs(pos_ball_ground[1]))
  }
  
  return(c(h, check_rally[[1]]))
}

##########################
### PLOTTING FUNCTIONS ###
##########################
# Plot the above view of a tennis field
PlotTennisField <- function(xlim = c(-13, 13), ylim = c(-7, 7), xlab="", ylab="", main="", col="black", lwd=1){
  plot(c(-11.89, 11.89), c(-4.12, -4.12), type = "l", 
       xlim = xlim, ylim = ylim, 
       xlab = xlab, ylab = ylab, 
       main=main, 
       col=col, 
       lwd=lwd)
  lines(c(-11.89, 11.89), c(4.12, 4.12), col=col, lwd=lwd)
  lines(c(-11.89, 11.89), c(5.49, 5.49), col=col, lwd=lwd)
  lines(c(-11.89, 11.89), c(-5.49, -5.49), col=col, lwd=lwd)
  lines(c(-6.4, 6.4), c(0, 0), col=col, lwd=lwd)
  lines(c(-11.89, -11.89), c(-5.49, 5.49), col=col, lwd=lwd)
  lines(c(11.89, 11.89), c(-5.49, 5.49), col=col, lwd=lwd)
  lines(c(0, 0), c(-5.49, 5.49), col=col, lwd=lwd)
  lines(c(-6.4, -6.4), c(-4.12, 4.12), col=col, lwd=lwd)
  lines(c(6.4, 6.4), c(-4.12, 4.12), col=col, lwd=lwd)
}
# Plot the above view of a tennis field
PlotTennisField3d <- function(xlim = c(-15, 15), ylim = c(-7, 7), zlim = c(0, 3)){
  n_points <- 100
  net_points <- 20
  # Create data points
  line1 <- matrix(c(seq(-11.89, 11.89, length.out = n_points), rep(-4.12, n_points), rep(0,n_points)), ncol = 3)
  line2 <- matrix(c(rep(11.89,n_points), seq(-4.12, 4.12, length.out = n_points), rep(0,n_points)), ncol = 3)
  line3 <- matrix(c(seq(11.89, -11.89, length.out = n_points), rep(4.12, n_points), rep(0,n_points)), ncol = 3)
  line4 <- matrix(c(rep(-11.89,n_points), seq(-4.12, 5.49, length.out = n_points), rep(0,n_points)), ncol = 3)
  line5 <- matrix(c(seq(-11.89, 11.89, length.out = n_points), rep(5.49, n_points), rep(0,n_points)), ncol = 3)
  line6 <- matrix(c(rep(11.89,n_points), seq(5.49, -5.49, length.out = n_points), rep(0,n_points)), ncol = 3)
  line7 <- matrix(c(seq(11.89, 0, length.out = n_points), rep(-5.49, n_points), rep(0,n_points)), ncol = 3)
  line8 <- matrix(c(rep(0,n_points), seq(-5.49, 5.49, length.out = n_points), rep(0,n_points)), ncol = 3)
  line9 <- matrix(c(rep(0,n_points), seq(5.49, 4.12, length.out = n_points), rep(0,n_points)), ncol = 3)
  line10 <- matrix(c(seq(0, -6.4, length.out = n_points), rep(4.12, n_points), rep(0,n_points)), ncol = 3)
  line11 <- matrix(c(rep(-6.4,n_points), seq(4.12, -4.12, length.out = n_points), rep(0,n_points)), ncol = 3)
  line12 <- matrix(c(rep(-6.4,n_points), seq(-4.12, 0, length.out = n_points), rep(0,n_points)), ncol = 3)
  line13 <- matrix(c(seq(-6.4, 6.4, length.out = n_points), rep(0, n_points), rep(0,n_points)), ncol = 3)
  line14 <- matrix(c(rep(6.4,n_points), seq(0, 4.12, length.out = n_points), rep(0,n_points)), ncol = 3)
  line15 <- matrix(c(rep(6.4,n_points), seq(4.12, -4.12, length.out = n_points), rep(0,n_points)), ncol = 3)
  line16 <- matrix(c(seq(6.4, 0, length.out = n_points), rep(4.12, n_points), rep(0,n_points)), ncol = 3)
  line17 <- matrix(c(rep(0,n_points), seq(-4.12, -5.49, length.out = n_points), rep(0,n_points)), ncol = 3)
  line18 <- matrix(c(seq(0, -11.89, length.out = n_points), rep(-5.49, n_points), rep(0,n_points)), ncol = 3)
  line19 <- matrix(c(rep(-11.89,n_points), seq(-5.49, -4.12, length.out = n_points), rep(0,n_points)), ncol = 3)
  line20 <- matrix(c(rep(-11.89,n_points), seq(-4.12, -5.49, length.out = n_points), rep(0,n_points)), ncol = 3)
  line21 <- matrix(c(seq(-11.89, 0, length.out = n_points), rep(-5.49, n_points), rep(0,n_points)), ncol = 3)
  line22 <- matrix(c(rep(0,n_points), rep(-5.49,n_points), seq(0,1.07, length.out = n_points)), ncol = 3)
  line23 <- matrix(c(rep(0,2*net_points), sort(seq(-5.49,5.49, length.out = 2*net_points)), rbind(HeightTennisNet(seq(-5.49,5.49, length.out = net_points)), rep(0,net_points))), ncol = 3)
  line24 <- matrix(c(rep(0,n_points), seq(5.49,-5.49, length.out = n_points), HeightTennisNet(seq(5.49,-5.49, length.out = n_points))), ncol = 3)
  
  datatotal <- rbind(line1, line2, line3, line4, line5, line6, line7, line8, line9, line10, 
                     line11, line12, line13, line14, line15, line16, line17, line18, line19,
                     line20, line21, line22, line23, line24)
  
  scatterplot3d(datatotal[,1], datatotal[,2], datatotal[,3], 
                xlim = xlim, ylim = ylim, zlim = zlim, 
                type = "l", 
                xlab = "x", ylab = "y", zlab = "z",
                lwd = 2,
                angle=60,
                grid = FALSE,
                box = FALSE, 
                axis = FALSE)
}
# Plot opponent
PlotOpponent <- function(pos_opponent_1, rgb_col = rgb(55,0,77, max=255)){
  points(pos_opponent_1[1], pos_opponent_1[2], col = rgb_col, lwd = 5)
}
# Plot player
PlotPlayer <- function(pos_player_1, rgb_col = rgb(55,0,77, max=255)){
  points(pos_player_1[1], pos_player_1[2], col = rgb_col, lwd = 5)
}
# Plot the line along the determined alpha
PlotAlphaLine <- function(pos_player, pos_opponent, alpha_players){
  PlotTennisField()
  PlotPlayer(pos_player)
  PlotOpponent(pos_opponent)
  if(pos_player[1]<0){
    lines(c(pos_player[1], field_baseline_length), c(pos_player[2], pos_player[2] + (field_baseline_length + abs(pos_player[1]))*tan(alpha_players*pi/180)))
  }else{
    lines(c(pos_player[1], -field_baseline_length), c(pos_player[2], pos_player[2] + (field_baseline_length + abs(pos_player[1]))*tan(alpha_players*pi/180)))
  }
}

#########################
### GENERAL FUNCTIONS ###
#########################
# Velocity of the ball directly after impact
VelocityBall <- function(d, v_incoming, w_racket){
  a1 <- (v_incoming * m * d - I_t * w_racket) * d
  a2 <- cor * (w_racket * d + v_incoming) * I_t
  a3 <- I_t + m * d * d
  return((a2-a1)/a3)
}
# Normalize function
Normalize <- function(min_1, max_1, value_1){
  if(value_1 < min_1 || value_1 > max_1) stop("Value to be normalized must be inbetween min and max")
  return((value_1 - min_1) / (max_1 - min_1))
}
# Feedforward activation function
ActivationFunction <- function(x){
  return(1/(1+exp(-x)))
}

###########################
### CHECK BALL IN FIELD ###
###########################
# Check if the serve was in the field, on the correct side of the field, and the difference wrt the correct serve field
CheckServeInField <- function(pos_player_2, pos_ball_2){
  serve_in <- FALSE
  serve_side <- FALSE
  if(pos_player_2[1]< 0 && pos_player_2[2] <= 0){
    field <- 1
  }
  else if(pos_player_2[1]<0 && pos_player_2[2] > 0){
    field <- 2
  }
  else if(pos_player_2[1]>0 && pos_player_2[2] <= 0){
    field <- 3
  } 
  else if(pos_player_2[1]>0 && pos_player_2[2] > 0){
    field <- 4
  }else{
    print()
  }
  
  # Player serves from bottom right
  if(field == 1){
    if(pos_ball_2[1] > 0 && pos_ball_2[1] < 6.4){
      if(pos_ball_2[2] > 0 && pos_ball_2[2] < 4.12){
        serve_side <- TRUE
        serve_in <- TRUE
      }else{
        serve_side <- TRUE
        serve_in < FALSE
      }
    }else{
      if(pos_ball_2[2] > 0 && pos_ball_2[2] < 4.12){
        serve_side <- TRUE
        serve_in <- FALSE
      }else{
        serve_side <- FALSE
        serve_in < FALSE
      }
    }
    if(!serve_in){
      dif <- sqrt(abs(6.4-pos_ball_2[1])^2 + abs(4.12-pos_ball_2[2])^2)
    }else{
      dif <- 0
    }
  }
  # player serve from bottom left
  else if(field == 2){
    if(pos_ball_2[1] > 0 && pos_ball_2[1] < 6.4){
      if(pos_ball_2[2] > -4.12 && pos_ball_2[2] < 0){
        serve_side <- TRUE
        serve_in <- TRUE
      }else{
        serve_side <- TRUE
        serve_in < FALSE
      }
    }else{
      if(pos_ball_2[2] > -4.12 && pos_ball_2[2] < 0){
        serve_side <- TRUE
        serve_in <- FALSE
      }else{
        serve_side <- FALSE
        serve_in < FALSE
      }
    }
    if(!serve_in){
      dif <- sqrt(abs(6.4-pos_ball_2[1])^2 + abs(-4.12-pos_ball_2[2])^2)
    }else{
      dif <- 0
    }
  }
  # player serves from top right
  else if(field == 3){
    if(pos_ball_2[1] > -6.4 && pos_ball_2[1] < 0){
      if(pos_ball_2[2] > 0 && pos_ball_2[2] < 4.12){
        serve_side <- TRUE
        serve_in <- TRUE
      }else{
        serve_side <- TRUE
        serve_in < FALSE
      }
    }else{
      if(pos_ball_2[2] > 0 && pos_ball_2[2] < 4.12){
        serve_side <- TRUE
        serve_in <- FALSE
      }else{
        serve_side <- FALSE
        serve_in < FALSE
      }
    }
    if(!serve_in){
      dif <- sqrt(abs(-6.4-pos_ball_2[1])^2 + abs(4.12-pos_ball_2[2])^2)
    }else{
      dif <- 0
    }
  }
  # player serves from top left
  else if(field == 4){
    if(pos_ball_2[1] > -6.4 && pos_ball_2[1] < 0){
      if(pos_ball_2[2] > -4.12 && pos_ball_2[2] < 0){
        serve_side <- TRUE
        serve_in <- TRUE
      }else{
        serve_side <- TRUE
        serve_in < FALSE
      }
    }else{
      if(pos_ball_2[2] > -4.12 && pos_ball_2[2] < 0){
        serve_side <- TRUE
        serve_in <- FALSE
      }else{
        serve_side <- FALSE
        serve_in < FALSE
      }
    }
    if(!serve_in){
      dif <- sqrt(abs(-6.4-pos_ball_2[1])^2 + abs(-4.12-pos_ball_2[2])^2)
    }else{
      dif <- 0
    }
  }else{
    stop("field must be 1, 2, 3, or 4.")
  }
  return(list("Serve in field"=serve_in, "Difference wrt serving field"=dif)) 
}
# Check if the ball was in the field and the difference wrt the field
CheckBallInField <- function(pos_player, pos_ball){
  ball_in <- FALSE
  
  # Player serves from bottom right
  if(pos_player[1] < 0){
    if(pos_ball[1] > 0 && pos_ball[1] < field_baseline_length){
      dif_1 <- 0
      ball_length_in <- TRUE
    }else{
      dif_1 <- pos_ball[1] - field_baseline_length
      ball_length_in <- FALSE
    }
    if(pos_ball[2] > -field_width && pos_ball[2] < field_width){
      dif_2 <- 0
      ball_width_in <- TRUE
    }else{
      if(pos_ball[2] < 0){
        dif_2 <- pos_ball[2] + field_width
      }else{
        dif_2 <- pos_ball[2] - field_width
      }
      ball_width_in <- FALSE
    }
    if(ball_length_in && ball_width_in){
      ball_in <- TRUE
      dif <- 0
    }else{
      ball_in <- FALSE
      dif <- sqrt(dif_1^2+dif_2^2)
    }
  }else{
    if(pos_ball[1] > -field_baseline_length && pos_ball[1] < 0){
      dif_1 <- 0
      ball_length_in <- TRUE
    }else{
      dif_1 <- pos_ball[1] + field_baseline_length
      ball_length_in <- FALSE
    }
    if(pos_ball[2] > -field_width && pos_ball[2] < field_width){
      dif_2 <- 0
      ball_width_in <- TRUE
    }else{
      if(pos_ball[2] < 0){
        dif_2 <- pos_ball[2] + field_width
      }else{
        dif_2 <- pos_ball[2] - field_width
      }
      ball_width_in <- FALSE
    }
    if(ball_length_in && ball_width_in){
      ball_in <- TRUE
      dif <- 0
    }else{
      ball_in <- FALSE
      dif <- sqrt(dif_1^2+dif_2^2)
    }
  }
  
  return(list("Ball in field"=ball_in, "Difference wrt serving field"=dif)) 
}

###########################
### SOLVE HEIGHT OF NET ###
###########################
# shape of the tennis net; needs to be scaled first
Catenary <- function(x, a){
  return(0.5*a*(exp(x/a)+exp(-x/a)))
}
# function we wish to solve; height at end of net (5.49m) is 1.07m, in the middle (0m) is 0.91m
# a = 94.21445 gives the desired result
CatenarySolver <- function(a){
  return(0.5*a*(exp(5.49/a)+exp(-5.49/a)-2))
} 
# Function to compute the height of the tennis net
HeightTennisNet <- function(x){
  aSolved <- 94.21445
  h_1 <- 0.5*aSolved*(exp(x/aSolved)+exp(-x/aSolved)) + 0.91 - aSolved
  return(h_1)
}