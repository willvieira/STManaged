#' Calculate state range limit
#'
#' This function calculates the south range limit of boreal and the north range limit of temperate states in a specific landscape configuration
#' @param land vector, one element of the \code{\link{run_model}} output
#' @param nRow numeric, number of rows of the landscape. Value is found in the output list from the \code{\link{run_model}} function
#' @param nCol numeric, number of columns of the landscape. Value is found in the output list from the \code{\link{run_model}} function
#' @param occup numeric between 0 and 1. The value determines the minimum occupancy a row of the landscape must be occupied by a specific forest state to be considered part of the state range
#' @export
#' @examples
#' \dontrun{
#' lands <- run_model(steps = 10, initLand)
#' range_limit(lands[['land_TX']], lands[['nRow']], lands[['nCol']], occup = .7)
#' }

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
