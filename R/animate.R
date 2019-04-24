#' Make an animated gif of landscape over time
#'
#' This function creates an animated gif with every landscape year configuration
#' @param lands the object output of the \code{\link{run_model}} function
#' @param stepsBy integer number, increment value from 1 to length of steps
#' @param steps vector, specific steps to be animated
#' @param fps frames per second
#' @param gifName string with name of the file output
#' @param rangeLimitOccup numeric between 0 and 1, if \code{rangeLimitOccup} is not \code{NULL}, the animation will include two lines to define the south range limit of boreal and the north range limit of temperate. \code{rangeLimitOccup} define the amount of occupancy a forest state must not occupy to determine the limit of its range distribution
#' @return an animated gif file
#' @importFrom magick image_graph
#' @importFrom magick image_join
#' @importFrom magick image_animate
#' @importFrom magick image_write
#' @export
#' @examples
#' \dontrun{
#' animate(lands, stepsBy = 1, fps = 5, gifName = 'myGif', rangeLimitOccup = 0.75)
#' }

animate <- function(lands, stepsBy = 1, steps = NULL, fps = 5, gifName = NULL, rangeLimitOccup = 0.75)
{
  # define land to be ploted
  if(!is.null(steps)) {
    # check if steps exists
    if(all(steps %in% 1:100) == FALSE) stop("'steps' values do not match with available steps from 'lands'")
    lds <- steps
  }else {
    lds <- seq(1, lands[['steps']], by = stepsBy)
  }

  # loop to save each plot in an obj
  for(i in lds) {
    figName <- paste0('landPlot', i)
    assign(figName, magick::image_graph(width = 800, height = 230, res = 60, pointsize = 20, clip = TRUE)) # create obj to save `plot_landscape`; clip = FALSE speeds up a lot the process
    if(!is.null(rangeLimitOccup)) {
      rangeLim <- range_limit(lands[[i]], nRow = lands[['nRow']], nCol = lands[['nCol']], rangeLimitOccup = rangeLimitOccup)
    }else rangeLim <- NULL
    plot_landscape(lands[[i]], nRow = lands[['nRow']], nCol = lands[['nCol']], Title = names(lands)[i], rangeLimit = rangeLim)
    dev.off()

    # print progress
    cat("==>", format(100 * seq_len(length(lds))[lds %in% i]/length(lds), digits = 2), "%", "\r")
  }

  # obj with plot names
  plots <- paste0('landPlot', lds)

  # merge plots
  img <- mget(plots)
  img <- magick::image_join(img)

  # create and save gif
  gif <- magick::image_animate(img, fps = fps, dispose = "previous")
  if(is.null(gifName)) gifName <- paste0('RCP', lands[['RCP']])
  magick::image_write(gif, paste0(gifName, '.gif'))

  # clean memory
  rm(list = ls()[ls() %in% paste0('landPlot', lds)])
  graphics.off()
}
