# Function to get range limit
  ## Input:
   # - land (one step landscape)
   # - occup (degree of occupancy at the row level to be considered a limit)
  ## Output:
   # - nb of the row (nb for both Boreal and Temperate states)

range_limit <- function(land, nRow, nCol, occup)
{
  # list to matrix
  landM <- matrix(land, ncol = nRow)

  prop <- apply(landM, 1, getProp, nRow = nRow)

  # limit Boreal
  limB <- as.numeric(max(which(prop['B',] > occup)))
  # limit Temperate
  limT <- as.numeric(min(which(prop['T',] > occup)))

  rtrn <- setNames(c(limB, limT), c('limitB', 'limitT'))

  return(rtrn)
}
