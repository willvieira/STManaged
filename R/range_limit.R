# Function to get range limit
  ## Input:
   # - land (one step landscape)
   # - occup (degree of occupancy at the row level to be considered a limit)
  ## Output:
   # - nb of the row (nb for both Boreal and Temperate states)

range_limit <- function(land, occup)
{
  nc <- length(land[[1]])
  nr <- length(land[[1]][[1]])

  # list to matrix
  landM <- matrix(unlist(land[[1]]), ncol = nc)

  # summary for each row (get proportion for each row of the landscape)
  getProp <- function(x) {
    B <- sum(landM[x,] == 1)/nc
    T <- sum(landM[x,] == 2)/nc
    M <- sum(landM[x,] == 3)/nc
    R <- 1 - sum(B, T, M)
    return(setNames(c(B, T, M, R), c('B', 'T', 'M', 'R')))
  }

  prop <- sapply(1:nr, getProp)

  # limit Boreal
  limB <- as.numeric(max(which(prop['B',] > occup)))
  # limit Temperate
  limT <- as.numeric(min(which(prop['T',] > occup)))

  rtrn <- setNames(c(limB, limT), c('limitB', 'limitT'))

  return(rtrn)
}
