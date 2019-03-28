# Function to initiate landscape
## input
 # - Climate range
 # - Cell size
 # - Initial state of landsape
## Ouput
 # - raster with first step landscape

 # probabilty occupancy of each state based on temperature (Env1: -2 to 1.2)
 envProb <- read.table('data/envProb.txt', h = T)

# initial state over the temperature gradient
getState <- function(env) {
 p <- envProb[which.min(abs(envProb$temp - env)),c(2:5)] # probability for a given temperature
 state <- which(rmultinom(n = 1, size = 1, prob = p) == 1) # get a state depending on the probability `p`
 return(state)
}

# Function to get a vector of cell position (ignoring border first and last line and column)
getPosition <- function(nRow, nCol) {
  nLines = nRow - 2
  pos = numeric((nCol - 2) * (nRow - 2))
  i = 1
  j = nCol - 2

  for(line in 1:nLines) {
    pos[i:j] = (nCol * line + 2):((nCol * line) + nCol - 1)
    i = j + 1; j = j + nCol - 2
  }

  return(pos)
}

# Function to get a list of neighborhood index for each cell position
getNeighbor <- function(nRow, nCol) {
  jump = nCol - 3
  cellPerRow = nCol - 2
  lineNeighbor <- c(1:3, (4:6 + jump), (7:9 + (jump * 2)))
  totalCells = (nCol - 2) * (nRow - 2)

  neighList = rep(list(NA), totalCells)
  count = 0; count2 = 0
  for(i in 1:totalCells) {
    neighList[[i]] = lineNeighbor + count + (nCol * count2)
    count = count + 1
    if(count == cellPerRow) {
      count = 0
      count2 = count2 + 1
    }
  }
  return(neighList)
}

create_landscape <- function(climRange = c(-2.1, 0.55),
                             cellSize = 1.2) # in Km
{

  # get grid size
  landDist = 800 # TODO: rethink about it
  nCol = round(landDist/cellSize, 0) #TODO: define the distance between climRange and real distance
  nRow = round(nCol/10, 0)

  # temperature gradient over the grid
  env1 <- seq(climRange[1], climRange[2], length.out = nCol)
  landscape <- setNames(rep(env1, nRow), 1:(nRow * nCol)) # list for each col of the matrix

  land <- sapply(landscape, getState)

  # get position to be calculate (i.e. ignore border)
  position = getPosition(nRow = nRow, nCol = nCol)

  # get neighbors for each position cell
  neighbor = getNeighbor(nRow = nRow, nCol = nCol)

  return(list(land = land, env1 = env1, nCol = nCol, nRow = nRow, position = position, neighbor = neighbor))
}
