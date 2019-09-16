rm(list = ls()) # Clear global environment 

#####################
### SPAWN PLAYERS ###
#####################
pos_player_1 <- c()
pos_player_2 <- c()
serve_player_1 <- FALSE
serve_player_2 <- FALSE
serve_left <- FALSE
serve_right <- FALSE
height_player_1 <- 1.85
height_player_2 <- 1.85

######################################
### CHOOSE SERVING PLAYER AND SIDE ###
######################################
# Set the serving player
if(runif(1) > 0.5){
  serve_player_1 <- TRUE
  serve_player_2 <- FALSE
}else{
  serve_player_1 <- FALSE
  serve_player_2 <- TRUE
}
# Set the serving side
if(runif(1) > 0.5){
  serve_left <- TRUE
  serve_right <- FALSE
}else{
  serve_left <- FALSE
  serve_right <- TRUE
}

#############################
### SET PLAYERS POSITIONS ###
#############################
if(serve_player_1){
  if(serve_left){
    pos_player_1 <- c(-12, runif(1, 0, 2), height_player_1)
    pos_player_2 <- c(12, runif(1, -3.5, -2), height_player_2)
  }else{
    pos_player_1 <- c(-12, runif(1, -2, 0), height_player_1)
    pos_player_2 <- c(12, runif(1, 2, 3.5), height_player_2)
  }
}else{
  if(serve_left){
    pos_player_1 <- c(-12, runif(1, 2, 3.5), height_player_1)
    pos_player_2 <- c(12, runif(1, -2, 0), height_player_2)
  }else{
    pos_player_1 <- c(-12, runif(1, -3.5, -2), height_player_1)
    pos_player_2 <- c(12, runif(1, 0, 2), height_player_2)
  }
}
