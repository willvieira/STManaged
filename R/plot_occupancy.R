#' Plot landscape occupancy
#'
#' This function plots the distribution of state occupancy over the Latitudinal grandient of the landscape
#' @param lands the object output of the \code{\link{run_model}} function
#' @param years vector, time steps to be plotted. Remember that mfrow set must be setted before the function in case years has more than one step
#' @param spar numeric, value between 0 and 1 to sooth the state occupancy
#' @export
#' @examples
#' par(mfrorw = c(1, 2))
#' plot_occupancy(lands, years = c(1, 100), spar = 0.2)

plot_occupancy <- function(lands, years, spar)
{
  # define coordinates
  nCol <- lands[['nCol']]
  nRow <- lands[['nRow']]

  # checks
  if(!all(years %in% seq_len(lands[['steps']]))) stop('years vector must be included in 1:steps')

  for(i in years) {

    # vector to matrix
    land <- matrix(lands[[i]], ncol = nRow)

    prop <- apply(land, 1, getProp, nRow = nRow)

    # plot
    cols <- c("darkcyan", "orange", "palegreen3", "black")

    par(mar = c(2.5, 2.5, 3, 0.5), mgp = c(1.5, 0.3, 0), tck = -.008)
    plot(1:nCol, prop[1,], pch = '', lwd = 1.3, xlab = "Latitudinal gradient", ylim = c(0, 1), ylab = "State occupancy", col = cols[1])
    invisible(sapply(1:3, function(x) points(1:nCol, if(spar == 0) {prop[x, ]} else {smooth.spline(prop[x, ], spar = spar)$y}, type = 'l', lwd = 1.3, col = cols[x])))
    legend(nRow/2 - nRow*.1, 0.8, legend = c('Boreal', 'Temperate', 'Mixed'), lty = 1, col = cols[1:3], bty = 'n', cex = 0.8)
    mtext(names(lands)[i], 3, line = 0.5)

  }
}
