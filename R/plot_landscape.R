#' Plot landscape
#'
#' This function plots a specific time step landscape
#' @param lands, the object output from either the \code{\link{run_model}} or \code{\link{create_landscape}} functions
#' @param step numeric, time step to be ploted. If \code{NULL} it will plot the initial landscape for a \code{create_landscape} object, and the last time step for a \code{run_model} object.
#' @param Title character, title of the landscape plot
#' @param xaxis logical, if \code{TRUE} it will add the x axis with the annual mean temperate values.
#' @param rmBorder logical, if \code{TRUE} the four side borders will be removed of the plot. This option is available because the model do not calculate state prevalence in the borders.
#' @param rangeLimit, vector, the latitudinal position of the boreal and temperate range limit in the landscape configuration. This value is obtained from the function \code{\link{range_limit}}
#' @export
#' @examples
#' \dontrun{
#' plot_landscape(lands = output, step = 100,  Title = 'land at step 100')
#' }

plot_landscape <- function(lands, step = NULL, Title = NULL, xaxis = FALSE, rmBorder = TRUE, rangeLimit = NULL)
{
  # Get info
  nCol = lands[['nCol']]
  nRow = lands[['nRow']]
  env1 = lands[['env1']]
  if(!is.null(lands[['land']])) {
    land = lands[['land']]
  }else{
    availableSteps <- gsub('land_T', '', names(lands)[grep('land_', names(lands))])
    if(is.null(step)) {
      land = lands[[paste0('land_T', max(as.numeric(availableSteps)))]]
    }else{
      # check if step is present in the object
      if(!(step %in% as.numeric(availableSteps))) stop(paste('`step` value must be one of the available land_T from the lands object. These are the following steps available:', paste(availableSteps, collapse = ', ')))
      land = lands[[paste0('land_T', step)]]
    }
  }

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
    env1 <- env1[c(-1, -length(env1))]
  }

  # plot
  col <- c("darkcyan", "orange", "palegreen3", "black")

  par(mar = c(ifelse(xaxis, 2, 0.5), 0.5, ifelse(is.null(Title), 0.5, 3), 0.5), cex.main = 1, xpd = ifelse(!is.null(rangeLimit), T, F), mgp = c(1, 0.2, 0), tck = -.01)
  image(x = coordx, y = coordy, xaxt='n', yaxt = 'n', z = landM, xlab = "", ylab = "", col = col, main = Title, breaks = c(0, 1, 2, 3, 4))
  if(!is.null(env1)) {
    par(new = T)
    plot(env1 * vars.sd['annual_mean_temp'] + vars.means['annual_mean_temp'], 1:length(env1), pch = '', yaxt = 'n', xlab = 'Latitude (annual mean temperature)')
  }

  # add rangeLimit line
  if(!is.null(rangeLimit)) {
    lines(c(rangeLimit[1], rangeLimit[1]), c(nCol, -2), lwd = 2)
    lines(c(rangeLimit[2], rangeLimit[2]), c(nCol, -2), lwd = 2)
  }

  # add north arrow
  north.arrow <- function(x, y, h) {
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y + h/2.4, y), col = "black", border = NA)
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y - h/2.4, y))
    #text(x, y, "N", adj = c(7, 0.4), cex = 2.5)
  }

  if(is.null(env1)) {
    north.arrow(par("usr")[1]+0.995*diff(par("usr")[1:2]), par("usr")[3]+0.12*diff(par("usr")[3:4]), diff(par("usr")[1:2]) * 0.013)
  }
}
