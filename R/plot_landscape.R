# FunColtion to plot the landscape grid
  ## Input:
   # - land (model output)
   # - title if needed
   # - rmBorder (remove border of plot)
   # - rangeLimit (if TRUE will add a line in the plot with the range limit of Boreal and Temperate)
  ## Output:
   # - an image()

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
