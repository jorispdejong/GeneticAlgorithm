# shape of the tennis net; needs to be scaled first
Catenary <- function(x, a){
  return(0.5*a*(exp(x/a)+exp(-x/a)))
}
# function we wish to solve; height at end of net (5.485m) is 1.07m, in the middle (0m) is 0.91m
CatenarySolver <- function(a){
  return(0.5*a*(exp(5.485/a)+exp(-5.485/a)-2))
}
# a = 94.043 gives the desired result
CatenarySolver(94.043)