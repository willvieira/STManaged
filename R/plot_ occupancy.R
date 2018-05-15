# function to plot the distribution of state occupancy in the landscape
  ## Input:
   # - lands
   # - years (vector with time steps desired to be plotted)
  ## Output:
   # - scatterplot

plot_occupancy <- function(lands, years)
{
  # define coordinates
  nc <- length(lands[[1]][[1]])
  nr <- length(lands[[1]][[1]][[1]])

  for(i in years) {

    # list to matrix
    land <- matrix(unlist(lands[[i]][[1]]), ncol = nc)

    # summary for each row
    getProp <- function(x) {
      B <- sum(land[x,] == "B")/nc
      T <- sum(land[x,] == "T")/nc
      M <- sum(land[x,] == "M")/nc
      R <- 1 - sum(B, T, M)
      return(setNames(c(B, T, M, R), c('B', 'T', 'M', 'R')))
    }

    prop <- sapply(1:nr, getProp)

    # plot
    col <- c("darkcyan","orange","palegreen3","black")

    par(mar = c(1.5,2.5,3,0.5), mgp = c(1.5, 0.3, 0), tck = -.008)
    plot(1:nr, prop[1,], type = 'l', lwd = 1.3, xlab = "", ylab = "State occupancy", col = col[1])
    invisible(sapply(2:3, function(x) points(1:nr, prop[x,], type = 'l', lwd = 1.3, col = col[x])))
    legend(nr/2 - nr*.1, 1, legend = c('Boreal', 'Temperate', 'Mixed', 'Regeneration'), lty = 1, col = col, bty = 'n', cex = 0.8)
    mtext(names(lands)[i], 3, line = 0.5)

  }
}

plot_occupancy_gif <- function()
{
  # TODO
}
# sooth lines
#smoothingSpline = smooth.spline(1:nr, prop[1,], spar=0.4)
#lines(smoothingSpline, col = 'red')
