## get functions
file.sources <- dir('R/')
invisible(sapply(paste0('R/', file.sources), source))


## get initial lanscape
initLand <- create_landscape(climRange = c(-1.9, 0.6), cellSize = 5)
plot_landscape(initLand, title = 'initial_landscape')

# load pars
params = read.table("data/pars.txt", row.names = 1)

# run the model (1000 years = 7MB of data; 42 seconds)
land <- run_model(steps = 150, initLand, params,
                   plantInt = 0, # for all management practice: [0-1]
                   harvInt = 0,
                   thinInt = 0,
                   enrichInt = 0,
                   RCP = 4.5, # either 0, 2.6, 4.5, 6.0 and 8.5
                   stoch = T)

# outputs
  ## plot some landscapes
  par(mfrow = c(3, 3))
  invisible(sapply(1:9, function(x) plot_landscape(land[[x]], title = names(land)[x], rmBorder = T)))

  ## create a gif (11 seconds for 21 images; 8Kbs) (http://imagemagick.org must be installed)
  make_gif(land, steps = 4, years = NULL, fps = 5, gifName = 'RCP4.5')
  make_gif(land, years = 1:100, fps = 5, gifName = 'RCP4.5')

  ## plot
  par(mfrow = c(1, 2))
  plot_occupancy(land, years = c(1, 2))

  ## Summary
  plot_occ_time(land)
