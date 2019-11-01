# Tennis field parameters
field_serve_length <- 6.4
field_width <- 4.12
field_baseline_length <- 11.89

# Outputs min's and max's
min_theta_rally <- 0
max_theta_rally <- 10
min_w_ball_rally <- 0
max_w_ball_rally <- 300
min_w_racket_rally <- 10
max_w_racket_rally <- 80

# Inputs min's and max's
min_x0_rally <- 6
max_x0_rally <- 20
min_z0_rally <- 0
max_z0_rally <- 1.2
min_v0_incoming_rally <- 0
max_v0_incoming_rally <- 50

# Parameters used in the algorithm
# rally
min_y0_rally <- -field_width    # Minimum y-position of player
max_y0_rally <- field_width     # Maximum y-position of player
# serve
min_y0_serve <- 0                # Minimum y-position of serving player
max_y0_serve <- field_width      # Maximum y-position of serving player
min_y0_serve_receive <- 3        # Minimum y-position of serving player
max_y0_serve_receive <- 4        # Maximum y-position of serving player
min_z0_serve <- 2.7              # Minimum z-position of serving player
max_z0_serve <- 3.2              # Maximum z-position of serving player
min_v0_serve <- 40               # Minimal serve speed of tennis ball
max_v0_serve <- 70               # Maximal serve speed of tennis ball
min_alpha_serve <- 0             # Minimum angle (left/right)
max_alpha_serve <- atan((2*field_width+0.5)/(field_baseline_length+field_serve_length))*180/pi            # Maximum angle (left/right)
min_theta_serve <- -10           # Minimal angle (up/down)
max_theta_serve <- 0             # Maximal angle (up/down)
max_w_ball_serve <- 300          # Maximal angular velocity of tennis ball (how much spin it has)

# Parameters for the distribution of determining left or right
skew_parameter_1 <- 6            # Shape of the skew distribution
skew_parameter_2 <- 2            # Shape of the skew distribution