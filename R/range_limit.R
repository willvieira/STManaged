# Function to get range limit
  ## Input:
   # - land (one step landscape)
   # - occup (degree of occupancy at the row level to be considered a limit)
  ## Output:
   # - nb of the row (nb for both Boreal and Temperate states)

# summary for each row (get proportion for each row of the landscape)
getProp <- function(x, nRow) {
 B <- sum(x == 1)/nRow
 T <- sum(x == 2)/nRow
 M <- sum(x == 3)/nRow
 R <- 1 - sum(B, T, M)
 return(setNames(c(B, T, M, R), c('B', 'T', 'M', 'R')))
}

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
