# function to make a gif of the outputs
  ## input:
   # - lands
   # - time interval (steps * 5 = year)
  ## output:
   # - gif

make_gif <- function(lands, steps = NULL, years = NULL, fps = 6, gifName = NULL, rangeLimit = FALSE, occup = 0.5)
{
  library(magick)

  # define land to be ploted
  if(!is.null(years)) {
    lds <- years
  }else {
    lds <- seq(1, lands[['steps']], by = steps)
  }

  # loop to save each plot in an obj
  for(i in lds) {
    figName <- paste0('landPlot', i)
    assign(figName, image_graph(width = 800, height = 230, res = 60, pointsize = 20, clip = TRUE)) # create obj to save `plot_landscape`; clip = FALSE speeds up a lot the process
    if(rangeLimit == TRUE) {
      rangeLim <- range_limit(lands[[i]], nRow = lands[['nRow']], nCol = lands[['nCol']], occup = occup)
    }else rangeLim <- NULL
    plot_landscape(lands[[i]], nRow = lands[['nRow']], nCol = lands[['nCol']], Title = names(lands)[i], rangeLimit = rangeLim)
    dev.off()

    # print progress
    cat("==>", format(100*i/length(lds), digits = 4), "%", "\r")
  }

  # obj with plot names
  plots <- paste0('landPlot', lds)

  # merge plots
  img <- mget(plots)
  img <- image_join(img)

  # create and save gif (magick must be installed)
  gif <- image_animate(img, fps = fps, dispose = "previous")
  if(is.null(gifName)) gifName <- past0('RCP', lands[['RCP']])
  image_write(gif, paste0(gifName, '.gif'))

  # clean memory
  rm(list = ls()[ls() %in% paste0('landPlot', lds)])

}
