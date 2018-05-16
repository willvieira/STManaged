## get functions
file.sources <- dir('R/')
invisible(sapply(paste0('R/', file.sources), source))


## get initial lanscape
initLand <- create_landscape(climRange = c(-1.9, 0.6), cellSize = 20)

# load pars
params = read.table("data/pars.txt", row.names = 1)

# run the model (1000 years = 7MB of data; 42 seconds)
lands <- run_model(steps = 250, initLand, params,
                   plantInt = 0, # for all management practice: [0-1]
                   harvInt = 0,
                   thinInt = 0,
                   enrichInt = 0,
                   RCP = 4.5) # either 0, 2.6, 4.5, 6.0 and 8.5

# outputs
  ## plot some landscapes
  par(mfrow = c(3, 3))
  invisible(sapply(1:9, function(x) plot_landscape(lands[[x]], title = names(lands)[x])))

  ## create a gif (11 seconds for 21 images; 8Kbs)
  make_gif(lands, steps = 10, fps = 5, gifName = 'RCP2.6')

  ## plot
  par(mfrow = c(1, 2))
  plot_occupancy(lands, years = c(1, 2))
