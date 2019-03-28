# function to plot the distribution of state occupancy in the landscape
  ## Input:
   # - land
   # - years (vector with time steps desired to be plotted)
  ## Output:
   # - scatterplot

# summary for each row
getProp <- function(land, nRow) {
 B <- sum(land == 1)/nRow
 T <- sum(land == 2)/nRow
 M <- sum(land == 3)/nRow
 R <- 1 - sum(B, T, M)
 return(setNames(c(B, T, M, R), c('B', 'T', 'M', 'R')))
}

plot_occupancy <- function(lands, years)
{
  # define coordinates
  nCol <- lands[['nCol']]
  nRow <- lands[['nRow']]

  # Test if years has the good size and values
  if(length(years) > lands[['steps']]) stop('Years is larger than the time steps from `lands`')
  if(max(years) > lands[['steps']]) stop(paste('year', max(year), 'is larger than the time steps from `lands`'))

  for(i in years) {

    # vector to matrix
    land <- matrix(lands[[i]], ncol = nRow)

    prop <- apply(land, 1, getProp, nRow = nRow)

    # plot
    col <- c("darkcyan","orange","palegreen3","black")

    par(mar = c(1.5,2.5,3,0.5), mgp = c(1.5, 0.3, 0), tck = -.008)
    plot(1:nCol, prop[1,], type = 'l', lwd = 1.3, xlab = "", ylim = c(0, 1), ylab = "State occupancy", col = col[1])
    invisible(sapply(2:3, function(x) points(1:nCol, prop[x,], type = 'l', lwd = 1.3, col = col[x])))
    legend(nRow/2 - nRow*.1, 1, legend = c('Boreal', 'Temperate', 'Mixed', 'Regeneration'), lty = 1, col = col, bty = 'n', cex = 0.8)
    mtext(names(lands)[i], 3, line = 0.5)

  }
}

plot_occupancy_gif <- function()
{
  # TODO
}
# sooth lines
#smoothingSpline = smooth.spline(1:nRow, prop[1,], spar=0.4)
#lines(smoothingSpline, col = 'red')
