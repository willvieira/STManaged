# Function to initiate landscape
## input
 # - Climate range
 # - Cell size
 # - Initial state of landsape
## Ouput
 # - raster with first step landscape

create_landscape <- function(climRange = c(-1.9, 0.6),
                             cellSize = 5) # in Km
{

  # probabilty occupancy of each state based on temperature (Env1: -2 to 1.2)
  envProb <- read.table('data/envProb.txt', h = T)

  # get grid size
  landDist = 1200 # TODO: rethink about it
  nrow = landDist/cellSize #TODO: define the distance between climRange and real distance
  ncol = nrow/12
  
  # temperature gradient over the grid
  env1 <- seq(climRange[1], climRange[2], length.out = nrow)
  envList <- rep(list(env1), ncol) # list for each col of the matrix

  # initial state over the temperature gradient
  getState <- function(env) {
    p <- envProb[which.min(abs(envProb$temp-env)),c(2:5)] # probability for a given temperature
    state <- names(envProb[,-1])[which(rmultinom(n = 1, size = 1, prob = p) == 1)] # get a state depending on the probability `p`
    return(state)
  }

  land <- lapply(envList, function(x) sapply(x, getState))

  return(list(land = land, env1 = env1))
}
