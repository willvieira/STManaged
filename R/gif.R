# function to make a gif of the outputs
  ## input:
   # - lands
   # - time interval (steps * 5 = year)
  ## output:
   # - gif

make_gif <- function(lands, steps = NULL, years = NULL, fps = 6)
{
  library(magick)

  # define lands to be ploted
  if(!is.null(years)) {
    lds <- years
  }else {
    lds <- seq(1, length(lands), by = steps)
  }

  # loop to save each plot in an obj
  for(i in lds) {
    figName <- paste0('landPlot', i)
    assign(figName, image_graph(res = 70)) # create obj to save `plot_landscape`
    plot_landscape(lands[[i]], title = names(lands)[i])
    dev.off()
  }

  # obj with plot names
  plots <- paste0('landPlot', lds)

  # merge plots
  img <- mget(plots)
  img <- image_join(img)

  # create and save gif (magick must be installed)
  gif <- image_animate(img, fps = fps, dispose = "previous")
  image_write(gif, "gif.gif")

  # clean memory
  rm(list = ls()[ls() %in% paste0('landPlot', lds)])

}
