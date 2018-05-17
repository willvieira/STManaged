# function to make a gif of the outputs
  ## input:
   # - lands
   # - time interval (steps * 5 = year)
  ## output:
   # - gif

make_gif <- function(lands, steps = NULL, years = NULL, fps = 6, gifName = NULL)
{
  library(magick)

  # define land to be ploted
  if(!is.null(years)) {
    lds <- years
  }else {
    lds <- seq(1, land[['steps']], by = steps)
  }

  # loop to save each plot in an obj
  for(i in lds) {
    figName <- paste0('landPlot', i)
    assign(figName, image_graph(res = 60, pointsize = 20, clip = TRUE)) # create obj to save `plot_landscape`; clip = FALSE speeds up a lot the process
    plot_landscape(lands[[i]], Title = names(lands)[i])
    dev.off()
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
