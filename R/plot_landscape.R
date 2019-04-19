#' Plot landscape
#'
#' This function plots a specific time step landscape
#' @param land a vector with the distribution of the four forest states in the landscape. This is found in the output list from the \code{\link{run_model}} function named 'land_TX', where X is the time step
#' @param nRow numeric, number of rows of the landscape. Value is found in the output list from the \code{\link{run_model}} function
#' @param nCow numeric, number of columns of the landscape. Value is found in the output list from the \code{\link{run_model}} function
#' @param Title character, title of the landscape plot
#' @param rmBorder logical, if \code{TRUE} the four side borders will be removed of the plot. This option is available because the model do not calculate state prevalence in the borders.
#' @param rangeLimit, vector, the latitudinal position of the boreal and temperate range limit in the landscape configuration. This value is obtained from the function \code{\link{range_limit}}
#' @export
#' @examples
#' \dontrun{
#' plot_landscape(land = output[[100]], nRow = output[['nRow']],
#'                nCol = output[['nCol']], Title = 'land at step 100')
#' }

plot_landscape <- function(land, nRow, nCol, Title = NULL, rmBorder = TRUE, rangeLimit = NULL)
{
  # define coordinates
  coordx <- seq(0, nCol)
  coordy <- seq(0, nRow)

  # Transform land from list to matrix
  landM <- matrix(land, nrow = nCol)

  # remove border of landscape that are not updated
  if(rmBorder == TRUE) {
    landM <- landM[c(-1, -nCol), c(-1, -nRow)]
    coordx <- seq(1, dim(landM)[1])
    coordy <- seq(1, dim(landM)[2])
  }

  # plot
  col <- c("darkcyan", "orange", "palegreen3", "black")

  par(mar = c(.5,0.5,3.5,0.5), cex.main = 1, xpd = ifelse(!is.null(rangeLimit), T, F))
  image(x = coordx, y = coordy, xaxt='n', yaxt = 'n', z = landM, xlab = "", ylab = "", col = col, main = Title, breaks = c(0, 1, 2, 3, 4))

  # add rangeLimit line
  if(!is.null(rangeLimit)) {
    lines(c(rangeLimit[1], rangeLimit[1]), c(nCol, -2), lwd = 2)
    lines(c(rangeLimit[2], rangeLimit[2]), c(nCol, -2), lwd = 2)
  }

  # add north arrow
  north.arrow = function(x, y, h) {
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y + h/2.4, y), col = "black", border = NA)
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y - h/2.4, y))
    #text(x, y, "N", adj = c(7, 0.4), cex = 2.5)
  }
  north.arrow(par("usr")[1]+0.995*diff(par("usr")[1:2]), par("usr")[3]+0.12*diff(par("usr")[3:4]), diff(par("usr")[1:2]) * 0.013)
}
