## get functions
file.sources <- dir('R/')
invisible(sapply(paste0('R/', file.sources), source))


## get initial lanscape
initLand <- create_landscape(climRange = c(-1.9, 0.6), cellSize = 20)

# load pars
params = read.table("data/pars.txt", row.names = 1)

# run the model
lands <- run_model(steps = 200, initLand, params) # 1000 years = 7MB of data; 42 seconds

# outputs
  ## plot some landscapes
  par(mfrow = c(3, 3))
  invisible(sapply(1:9, function(x) plot_landscape(lands[[x]], title = names(lands)[x])))

  ## create a gif (11 seconds for 21 images; 8Kbs)
  make_gif(lands, steps = 10, fps = 5)

  #
